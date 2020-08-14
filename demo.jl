using LibCURL

puts(s::Union{String,SubString{String}}) = ccall(:puts, Cint, (Ptr{Cchar},), s)

macro check(ex::Expr)
    ex.head == :call ||
        error("@check: not a call: $ex")
    if ex.args[1] == :ccall
        ex.args[2] isa QuoteNode ||
            error("@check: ccallee must be a symbol")
        f = ex.args[2].value :: Symbol
    else
        f = ex.args[1] :: Symbol
    end
    prefix = "$f: "
    quote
        r = $(esc(ex))
        iszero(r) || @async @error($prefix * string(r))
        nothing
    end
end

# some libuv wrappers

const UV_READABLE = 1
const UV_WRITABLE = 2

uv_poll_alloc() = ccall(:jl_malloc, Ptr{Cvoid}, (Csize_t,), Base._sizeof_uv_poll)

function uv_poll_init(p::Ptr{Cvoid}, sock::curl_socket_t)
    @check ccall(:uv_poll_init, Cint,
        (Ptr{Cvoid}, Ptr{Cvoid}, curl_socket_t), Base.eventloop(), p, sock)
end

function uv_poll_start(p::Ptr{Cvoid}, events::Integer, cb::Ptr{Cvoid})
    @check ccall(:uv_poll_start, Cint, (Ptr{Cvoid}, Cint, Ptr{Cvoid}), p, events, cb)
end

function uv_poll_stop(p::Ptr{Cvoid})
    @check ccall(:uv_poll_stop, Cint, (Ptr{Cvoid},), p)
end

function uv_close(p::Ptr{Cvoid}, cb::Ptr{Cvoid})
    ccall(:uv_close, Cvoid, (Ptr{Cvoid}, Ptr{Cvoid}), p, cb)
end

function uv_timer_init(p::Ptr{Cvoid})
    @check ccall(:uv_timer_init, Cint, (Ptr{Cvoid}, Ptr{Cvoid}), Base.eventloop(), p)
end

function uv_timer_start(p::Ptr{Cvoid}, cb::Ptr{Cvoid}, t::Integer, r::Integer)
    @check ccall(:uv_timer_start, Cint,
        (Ptr{Cvoid}, Ptr{Cvoid}, UInt64, UInt64), p, cb, t, r)
end

function uv_timer_stop(p::Ptr{Cvoid})
    @check ccall(:uv_timer_stop, Cint, (Ptr{Cvoid},), p)
end

# additional libcurl methods

import LibCURL: curl_multi_socket_action

function curl_multi_socket_action(multi_handle, s, ev_bitmask)
    curl_multi_socket_action(multi_handle, s, ev_bitmask, Ref{Cint}())
end

# curl callbacks

function write_callback(
    ptr   :: Ptr{Cchar},
    size  :: Csize_t,
    count :: Csize_t,
    userp :: Ptr{Cvoid},
)::Csize_t
    n = size * count
    # ccall(:write, Csize_t, (Cint, Ptr{Cvoid}, Csize_t), 1, ptr, n)
    buffer = Array{UInt8}(undef, n)
    ccall(:memcpy, Ptr{Cvoid}, (Ptr{Cvoid}, Ptr{Cvoid}, Csize_t), buffer, ptr, n)
    io = unsafe_pointer_to_objref(userp)::IO
    @async write(io, buffer)
    return n
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
            uv_poll_init(uv_poll_p, sock)
            # NOTE: if assertion fails need to store indirectly
            @assert sizeof(curl_socket_t) <= sizeof(Ptr{Cvoid})
            unsafe_store!(convert(Ptr{curl_socket_t}, uv_poll_p), sock)
            @check curl_multi_assign(curl, sock, uv_poll_p)
        end
        events = 0
        action != CURL_POLL_IN  && (events |= UV_WRITABLE)
        action != CURL_POLL_OUT && (events |= UV_READABLE)
        uv_poll_start(uv_poll_p, events, event_cb)
    elseif action == CURL_POLL_REMOVE
        if uv_poll_p != C_NULL
            uv_poll_stop(uv_poll_p)
            uv_close(uv_poll_p, cglobal(:jl_free))
            @check curl_multi_assign(curl, sock, C_NULL)
        end
    else
        @async @error("socket_callback: unexpected action", action)
    end
    return 0
end

function timer_callback(
    multi      :: Ptr{Cvoid},
    timeout_ms :: Clong,
    userp      :: Ptr{Cvoid},
)::Cint
    if timeout_ms ≥ 0
        uv_timer_start(timer, timeout_cb, max(1, timeout_ms), 0)
    else
        uv_timer_stop(timer)
    end
    return 0
end

const write_cb = @cfunction(write_callback,
    Csize_t, (Ptr{Cchar}, Csize_t, Csize_t, Ptr{Cvoid}))
const socket_cb = @cfunction(socket_callback,
    Cint, (Ptr{Cvoid}, curl_socket_t, Cint, Ptr{Cvoid}, Ptr{Cvoid}))
const timer_cb = @cfunction(timer_callback,
    Cint, (Ptr{Cvoid}, Clong, Ptr{Cvoid}))

# libuv callbacks

struct CURLMsg
   msg    :: CURLMSG
   handle :: Ptr{Cvoid}
   code   :: CURLcode
end

function check_multi_info()
    while true
        p = curl_multi_info_read(curl, Ref{Cint}())
        p == C_NULL && return
        message = unsafe_load(convert(Ptr{CURLMsg}, p))
        if message.msg == CURLMSG_DONE
            handle = message.handle
            url_ref = Ref{Ptr{Cchar}}()
            @check curl_easy_getinfo(handle, CURLINFO_EFFECTIVE_URL, url_ref)
            url = unsafe_string(url_ref[])
            msg = unsafe_string(curl_easy_strerror(message.code))
            @async @info("request done", url, msg, code = Int(message.code))
            @check curl_multi_remove_handle(curl, handle)
            curl_easy_cleanup(handle)
        else
            @async @info("unknown CURL message type", msg = message.msg)
        end
    end
end

function event_callback(
    uv_poll_p :: Ptr{Cvoid},
    status    :: Cint,
    events    :: Cint,
)::Cvoid
    flags = 0
    events & UV_READABLE != 0 && (flags |= CURL_CSELECT_IN)
    events & UV_WRITABLE != 0 && (flags |= CURL_CSELECT_OUT)
    sock = unsafe_load(convert(Ptr{curl_socket_t}, uv_poll_p))
    @check curl_multi_socket_action(curl, sock, flags)
    check_multi_info()
end

function timeout_callback(p::Ptr{Cvoid})::Cvoid
    @check curl_multi_socket_action(curl, CURL_SOCKET_TIMEOUT, 0)
    check_multi_info()
end

const event_cb = @cfunction(event_callback, Cvoid, (Ptr{Cvoid}, Cint, Cint))
const timeout_cb = @cfunction(timeout_callback, Cvoid, (Ptr{Cvoid},))

# adding a URL to download

function add_download(url::AbstractString, io::IO)
    # init a single curl handle
    handle = curl_easy_init()

    # HTTP options
    @check curl_easy_setopt(curl, CURLOPT_POSTREDIR, CURL_REDIR_POST_ALL)

    # HTTPS: tell curl where to find certs
    certs_file = joinpath(Sys.BINDIR, "..", "share", "julia", "cert.pem")
    @check curl_easy_setopt(handle, CURLOPT_CAINFO, certs_file)

    # set the URL and request to follow redirects
    @check curl_easy_setopt(handle, CURLOPT_URL, url)
    @check curl_easy_setopt(handle, CURLOPT_FOLLOWLOCATION, true)

    # associate IO object with handle
    # TODO: may need to root `io` to keep it alive
    p = pointer_from_objref(io)
    @check curl_easy_setopt(handle, CURLOPT_WRITEDATA, p)
    @check curl_easy_setopt(handle, CURLOPT_WRITEFUNCTION, write_cb)

    # add curl handle to be multiplexed
    @check curl_multi_add_handle(curl, handle)

    return handle
end

## libuv setup ##

const timer = ccall(:jl_malloc, Ptr{Cvoid}, (Csize_t,), Base._sizeof_uv_timer)

uv_timer_init(timer)

## curl setup ##

@check curl_global_init(CURL_GLOBAL_ALL)

const curl = curl_multi_init()

# set various callbacks
@check curl_multi_setopt(curl, CURLMOPT_TIMERFUNCTION, timer_cb)
@check curl_multi_setopt(curl, CURLMOPT_SOCKETFUNCTION, socket_cb)

## actually use it ##

io1 = IOBuffer()
io2 = IOBuffer()
add_download("https://us-west.pkg.julialang.org/registries", io1)
add_download("https://us-west.pkg.julialang.org/registry/23338594-aafe-5451-b93e-139f81909106/35fb75d4e6db1100c206a54522092f9c836b21c0", io2)
sleep(1)
println("length 1: ", length(take!(io1)))
println("length 2: ", length(take!(io2)))

## cleanup ##

uv_close(timer, cglobal(:jl_free))
curl_multi_cleanup(curl)
