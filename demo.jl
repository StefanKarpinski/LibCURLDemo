using LibCURL

curl_global_init(CURL_GLOBAL_ALL)

const curl = curl_multi_init()
const timer = ccall(:jl_malloc, Ptr{Cvoid}, (Csize_t,), Base._sizeof_uv_timer)

# some libuv wrappers

const UV_READABLE = 1
const UV_WRITABLE = 2

uv_poll_alloc() =
    ccall(:jl_malloc, Ptr{Cvoid}, (Csize_t,), Base._sizeof_uv_poll)

uv_poll_free(p::Ptr{Cvoid}) =
    ccall(:jl_free, Cvoid, (Ptr{Cvoid},), p)

function uv_poll_init_socket(p::Ptr{Cvoid}, sock::curl_socket_t)
    ccall(:uv_poll_init_socket, Cint, (Ptr{Cvoid}, Ptr{Cvoid}, curl_socket_t),
          Base.eventloop(), p, sock)
    # NOTE: if assertion fails need to store indirectly
    @assert sizeof(curl_socket_t) <= sizeof(Ptr{Cvoid})
    unsafe_store!(convert(Ptr{curl_socket_t}, p), sock)
end

uv_poll_start(p::Ptr{Cvoid}, events::Integer, cb::Ptr{Cvoid}) =
    ccall(:uv_poll_start, Cint, (Ptr{Cvoid}, Cint, Ptr{Cvoid}), p, events, cb)

uv_poll_stop(p::Ptr{Cvoid}) =
    ccall(:uv_poll_start, Cint, (Ptr{Cvoid},), p)

uv_close(p::Ptr{Cvoid}, cb::Ptr{Cvoid}) =
    ccall(:uv_close, Cvoid, (Ptr{Cvoid}, Ptr{Cvoid}), p, cb)

uv_timer_start(p::Ptr{Cvoid}, cb::Ptr{Cvoid}, t::Integer, r::Integer) =
    ccall(:uv_timer_start, Cint, (Ptr{Cvoid}, Ptr{Cvoid}, UInt64, UInt64), p, cb, t, r)

uv_timer_stop(p::Ptr{Cvoid}) = ccall(:uv_timer_stop, Cint, (Ptr{Cvoid},), p)

# additional libcurl methods

curl_multi_socket_action(multi_handle, s, ev_bitmask) =
    curl_multi_socket_action(multi_handle, s, ev_bitmask, Ref{Cint}())

# curl callbacks

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
        uv_poll_start(up_poll_p, events, event_cb)
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

function timer_callback(
    multi::Ptr{Cvoid},
    timeout_ms::Clong,
    userp::Ptr{Cvoid},
)::Cint
    if timeout_ms ≥ 0
        uv_timer_start(timer, timeout_cb, max(1, timeout_ms), 0)
    else
        uv_timer_stop(timer)
    end
    return 0
end

const write_cb = @cfunction(write_callback,
    Csize_t, (Ptr{Cvoid}, Csize_t, Csize_t, Ptr{Cvoid}))
const socket_cb = @cfunction(socket_callback,
    Cint, (Ptr{Cvoid}, curl_socket_t, Cint, Ptr{Cvoid}, Ptr{Cvoid}))
const timer_cb = @cfunction(timer_callback,
    Cint, (Ptr{Cvoid}, Clong, Ptr{Cvoid}))

# libuv callbacks

function event_callback(
    uv_poll_p::Ptr{Cvoid},
    status::Cint,
    events::Cint,
)::Cvoid
    flags = 0
    events & UV_READABLE != 0 && (flags |= CURL_CSELECT_IN)
    events & UV_WRITABLE != 0 && (flags |= CURL_CSELECT_OUT)
    sock = unsafe_load(convert(Ptr{curl_socket_t}, uv_poll_p))
    curl_multi_socket_action(curl, sock, flags)
    # check_multi_info()
end

function timeout_callback(::Ptr{Cvoid})::Cvoid
    curl_multi_socket_action(curl, CURL_SOCKET_TIMEOUT, 0)
    # check_multi_info()
end

const event_cb = @cfunction(event_callback, Cvoid, (Ptr{Cvoid}, Cint, Cint))
const timeout_cb = @cfunction(timeout_callback, Cvoid, (Ptr{Cvoid},))

# adding a URL to download

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
    curl_easy_setopt(handle, CURLMOPT_TIMERFUNCTION, timer_cb)
    curl_easy_setopt(handle, CURLMOPT_SOCKETFUNCTION, socket_cb)

    # add curl handle to be multiplexed
    curl_multi_add_handle(curl, handle)

    return handle
end
