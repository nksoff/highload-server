-module(httpd_accept).

-behaviour(gen_server).

-export([start_link/1, code_change/3, handle_call/3, handle_cast/2, handle_info/2, init/1, terminate/2]).

start_link(LSocket) ->
    gen_server:start_link(?MODULE, LSocket, []).

init(LSocket) ->
    gen_server:cast(self(), accept),
    {ok, LSocket}.

handle_cast(accept, LSocket) ->
    case gen_tcp:accept(LSocket) of
        {ok, Socket} ->
            inet:setopts(Socket, [{active, false}]),
            HandlerPid = spawn(httpd_handle, action, [Socket]),
            gen_tcp:controlling_process(Socket, HandlerPid),
            gen_server:cast(self(), accept),
            {noreply, LSocket};
        {error, _} ->
            {stop, LSocket}
    end.

terminate(_Reason, _Sock) ->
    ok.

handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
