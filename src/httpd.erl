-module(httpd).

-export([start/0]).

start() ->
    ets:new(highload_settings, [named_table, protected, set, {keypos, 1}]),
    ets:insert(highload_settings, {"document_root", "http-test-suite"}),

    [{_, DocumentRoot} | _] = ets:lookup(highload_settings, "document_root"),
    case filelib:ensure_dir(DocumentRoot) of
        ok ->
            listen();
        _ ->
            {error, directory_not_found}
    end.

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
