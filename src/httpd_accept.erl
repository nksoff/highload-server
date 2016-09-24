-module(httpd_accept).

-export([accept/1]).

accept(LSocket) ->
    case gen_tcp:accept(LSocket) of
        {ok, Socket} ->
            HandlerPid = spawn(httpd_handle, action, [Socket]),
            gen_tcp:controlling_process(Socket, HandlerPid),
            accept(LSocket);
        {error, Reason} ->
            {error, Reason}
    end.
