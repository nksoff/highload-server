-module(httpd).

-export([listen/0]).

listen() ->
    DefaultPort = 80,

    DefaultPacketFormat = binary,
    DefaultBackLog = 200,
    DefaultIP = {0, 0, 0, 0},
    DefaultIPVersion = inet, % inet6

    DefaultActive = false,
    DefaultBuffer = 65536,
    DefaultKeepAlive = false,
    DefaultNoDelay = true,

    DefaultReuseaddr = true,

    case gen_tcp:listen(DefaultPort, [DefaultPacketFormat,
                                       {backlog, DefaultBackLog},
                                       {ip, DefaultIP},
                                       DefaultIPVersion,
                                       {active, DefaultActive},
                                       {buffer, DefaultBuffer},
                                       {keepalive, DefaultKeepAlive},
                                       {nodelay, DefaultNoDelay},
                                       {reuseaddr, DefaultReuseaddr}
                                     ]) of
        {ok, LSocket} -> 
            httpd_accept:accept(LSocket),
            {ok, DefaultPort};
        {error, Reason} ->
            {error, Reason}
    end.
