-module(httpd).

-behaviour(supervisor).

-include("httpd.hrl").

-export([init/1, start_link/0, get_setting/1]).

init(_Arg) ->
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

    case listen() of
        {ok, _Port, LSocket} ->
            {
             ok,
             {
              {
               one_for_all,          %% Restart strategy
               5,                    %% Max Restarts
               60                    %% Max Time
              },
              [
               {
                httpd_accept_pool_id, %% ID
                {
                 httpd_accept_pool,   %% Module Name
                 start_link,         %% Function Name
                 [ LSocket ]         %% Args
                },
                permanent,           %% Restart Type
                10000,               %% Max shutdown wait time (10 sec)
                supervisor,          %% Child Type
                [httpd_accept_pool]  %% Callback Module
               }
              ]
             }
            };
        {error, Reason} ->
            {error, Reason}
    end.

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

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
            {ok, DefaultPort, LSocket};
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
