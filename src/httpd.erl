-module(httpd).
-include("httpd.hrl").

-export([start/0, get_setting/1]).

start() ->
    DocRoot = get_setting("path"),
    io:format("working directory is ~s~n", [DocRoot]),

    case filelib:is_dir(DocRoot) of
        true ->
            ok;
        _ ->
            io:format("warning: directory ~s is not found~n", [DocRoot])
    end,

    Cpu = get_setting("cpu"),
    io:format("cpus: ~B~n", [Cpu]),
    erlang:system_flag(schedulers_online, Cpu),

    listen().

listen() ->
    DefaultPort = get_setting("port"),

    DefaultPacketFormat = binary,
    DefaultBackLog = 200,
    DefaultIP = get_setting("ip"),
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

load_settings() ->
    % cpu
    EnvCpu = os:getenv(?ENV_CPU),

    Cpu = try
        list_to_integer(EnvCpu)
    catch error:badarg ->
        erlang:system_info(schedulers)
    end,

    EnvDocumentPath = os:getenv(?ENV_DOCUMENT_PATH),

    % document path
    DocumentPath =
    case EnvDocumentPath of
        X when X /= "", X /= false ->
            EnvDocumentPath;
        _ ->
            "./"
    end,

    #httpd_settings{
       document_path = DocumentPath,
       cpu = Cpu,
       ip = {127, 0, 0, 1},
       port = 80
      }.

get_setting(Key) ->
    Settings = load_settings(),

    case Key of
        "path" ->
            Settings#httpd_settings.document_path;
        "cpu" ->
            Settings#httpd_settings.cpu;
        "ip" ->
            Settings#httpd_settings.ip;
        "port" ->
            Settings#httpd_settings.port
    end.
