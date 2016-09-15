-module(httpd).

-export([listen/0]).

listen() ->
    DefaultPort = 80,

    DefaultPacketFormat = binary,
    DefaultBackLog = 200,
    DefaultIP = {0, 0, 0, 0},
    DefaultIPVersion = inet, % inet6

    case gen_tcp:listen(DefaultPort, [DefaultPacketFormat,
                                       {backlog, DefaultBackLog},
                                       {ip, DefaultIP},
                                       DefaultIPVersion
                                     ]) of
        {ok, _LSocket} -> 
            {ok, DefaultPort};
        {error, Reason} ->
            {error, Reason}
    end.
