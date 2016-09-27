-module(httpd).

-export([start/0]).

start() ->
    case os:getenv("HTTPD_DOCUMENT_PATH") of
        false ->
            DocRoot = "./";
        "" ->
            DocRoot = "./";
        X ->
            DocRoot = X
    end,

    ets:new(highload_settings, [named_table, protected, set, {keypos, 1}]),
    ets:insert(highload_settings, {"document_root", DocRoot}),

    io:format("working directory is ~s~n", [DocRoot]),

    case filelib:is_dir(DocRoot) of
        true ->
            ok;
        _ ->
            io:format("warning: directory ~s is not found~n", [DocRoot])
    end,

    NCpu = os:getenv("HTTPD_CPU"),

    case NCpu of
        false ->
            ok;
        _ ->
            io:format("cpus: ~s~n", [NCpu]),
            erlang:system_flag(schedulers_online, list_to_integer(NCpu))
    end,

    listen().

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
