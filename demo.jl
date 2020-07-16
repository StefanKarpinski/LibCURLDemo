using LibCURL

curl_global_init(CURL_GLOBAL_ALL)

const curl = curl_multi_init()

# some libuv wrappers

const UV_READABLE = 1
const UV_WRITABLE = 2

uv_poll_alloc() =
    ccall(:jl_malloc, Ptr{Cvoid}, (Csize_t,), Base._sizeof_uv_poll)

uv_poll_free(p::Ptr{Cvoid}) =
    ccall(:jl_free, Cvoid, (Ptr{Cvoid},), p)

function uv_poll_init_socket(uv_poll_p::Ptr{Cvoid}, sock::curl_socket_t)
    ccall(:uv_poll_init_socket, Cint, (Ptr{CVoid}, Ptr{Cvoid}, curl_socket_t),
          Base.eventloop(), uv_poll_p, sock)
    # NOTE: if assertion fails need to store indirectly
    @assert sizeof(curl_socket_t) <= sizeof(Ptr{Cvoid})
    unsafe_store!(convert(Ptr{curl_socket_t}, sock))
end

uv_poll_start(uv_poll_p::Ptr{Cvoid}, events::Integer, cb::Ptr{Cvoid}) =
    ccall(:uv_poll_start, Cint, (Ptr{Cvoid}, Cint, Ptr{Cvoid}), uv_poll_p, events, cb)

uv_poll_stop(uv_poll_p::Ptr{Cvoid}) =
    ccall(:uv_poll_start, Cint, (Ptr{Cvoid},), uv_poll_p)

uv_close(uv_handle_p::Ptr{Cvoid}, uv_close_cb::Ptr{Cvoid}) =
    ccall(:uv_close, Cvoid, (Ptr{Cvoid}, Ptr{Cvoid}), uv_handle_p, uv_close_cb)

# callbacks used by the curl multisocket API

function write_callback(
    ptr   :: Ptr{Cchar},
    size  :: Csize_t,
    count :: Csize_t,
    data  :: Ptr{Cvoid},
)::Csize_t
    n = size*count
    buffer = Array{UInt8}(undef, n)
    ccall(:memcpy, Ptr{Cvoid}, (Ptr{Cvoid}, Ptr{Cvoid}, UInt64), buffer, ptr, n)
    io = unsafe_load(convert(Ptr{Any}, data))::IO
    write(io, data)
end

function socket_callback(
    handle    :: Ptr{Cvoid},
    sock      :: curl_socket_t,
    action    :: Cint,
    userp     :: Ptr{Cvoid},
    uv_poll_p :: Ptr{Cvoid},
)::Cint
    if action in (CURL_POLL_IN, CURL_POLL_OUT, CURL_POLL_INOUT)
        if uv_poll_p == C_NULL
            uv_poll_p = uv_poll_alloc()
            uv_poll_init_socket(uv_poll_p, sock)
            curl_multi_assign(handle, sock, uv_poll_p)
        end
        events = 0
        action == CURL_POLL_IN  || (events |= UV_WRITABLE)
        action == CURL_POLL_OUT || (events |= UV_READABLE)
        uv_poll_start(up_poll_p, events, curl_event_cb)
    elseif action == CURL_POLL_REMOVE
        if uv_poll_p != C_NULL
            uv_poll_stop(uv_poll_p)
            uv_close(uv_poll_p, uv_poll_free)
            curl_multi_assign(handle, sock, C_NULL)
        end
    else
        error("socket_callback: unexpected action — $action")
    end
end

function curl_event_callback(
    
)

# ccallable callback pointers

const write_cb = @cfunction(write_callback, Csize_t,
    (Ptr{Cvoid}, Csize_t, Csize_t, Ptr{Cvoid}))

const socket_cb = @cfunction(socket_callback, Csize_t,
    (Ptr{Cvoid}, curl_socket_t, Cint, Ptr{Cvoid}, Ptr{Cvoid}))

function add_download(url::AbstractString, io::IO)
    # init a single curl handle
    handle = curl_easy_init()

    # set the URL and request to follow redirects
    curl_easy_setopt(handle, CURLOPT_URL, url)
    curl_easy_setopt(handle, CURLOPT_FOLLOWLOCATION, 1)

    # associate IO object with handle
    curl_easy_setopt(handle, CURLOPT_PRIVATE, io)
    curl_easy_setopt(handle, CURLOPT_WRITEDATA, io)

    # set various callbacks
    curl_easy_setopt(handle, CURLOPT_WRITEFUNCTION, write_cb)
    curl_easy_setopt(handle, CURLOPT_SOCKETFUNCTION, socket_cb)

    # add curl handle to be multiplexed
    curl_multi_add_handle(curl, handle)

    return handle
end

