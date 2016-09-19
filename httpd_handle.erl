-module(httpd_handle).
-include("httpd.hrl").

-export([action/1]).

action(Socket) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Data} ->
            Request = get_request(Data),
            Response = get_response(Request),

            gen_tcp:send(Socket, response_to_text(Response)),
            gen_tcp:close(Socket),
            ok;
        _ ->
            gen_tcp:close(Socket),
            ok

    end.

get_request(RawHttpString) ->
    [Method, RawPath | _] = binary:split(RawHttpString, ?DELIMITERS, [trim_all, global]),
    #request{method = Method, path = http_uri:decode(binary_to_list(RawPath))}.

handle_path(Path) ->
    [{_, DocumentRoot} | _] = ets:lookup(highload_settings, "document_root"),
    filename:join(DocumentRoot, Path).

is_path_valid(Path) ->
    not lists:member("..", string:tokens(Path, "/")).

get_response(#request{method = Method, path = Path}) ->
    MethodValid = Method =:= <<"GET">> orelse Method =:= <<"HEAD">>,
    HandledPath = handle_path(Path),
    PathValid = is_path_valid(HandledPath),

    case {MethodValid, PathValid} of
        {false, _} ->
            #response{code = 400, body = "Wrong method"};
        {_, false} ->
            #response{code = 400, body = "Wrong path"};
        _ ->
            #response{code = 200, body = Path}
    end.

response_to_text(#response{code = Code, body = Body}) ->
    NewBody = iolist_to_binary(Body),
    "HTTP/1.1 " ++ integer_to_list(Code) ++ " OK\r\nContent-Length: " ++ integer_to_list(size(NewBody)) ++ "\r\nConnection: close\r\n\r\n" ++ NewBody.
