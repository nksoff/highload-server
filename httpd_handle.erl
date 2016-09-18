-module(httpd_handle).

-export([action/1]).

action(Socket) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, _Data} ->
            gen_tcp:send(Socket, response("Hello")),
            gen_tcp:close(Socket),
            ok;
        _ ->
            gen_tcp:close(Socket),
            ok

    end.

response(Str) ->
    B = iolist_to_binary(Str),
    iolist_to_binary(
      io_lib:fwrite(
        "HTTP/1.0 200 OK\nContent-Type: text/html\nContent-Length: ~p\n\n~s",
        [size(B), B])).
