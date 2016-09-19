-module(httpd_handle).
-include_lib("kernel/include/file.hrl").
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
    FlattenedPath = filename:flatten([DocumentRoot, Path]),
    case binary:last(list_to_binary(FlattenedPath)) of
        $/ ->
            filename:flatten([FlattenedPath, ?DEFAULT_INDEX]);
        _ ->
            FlattenedPath
    end.

is_path_valid(Path) ->
    not lists:member("..", string:tokens(Path, "/")).

is_file_available(Path) ->
    case file:read_file_info(Path) of
        {ok, #file_info{type = regular, access = Access}}
          when Access =:= read_write orelse Access =:= read ->
            true;
        _ ->
            false
    end.

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
            FileAvailable = is_file_available(HandledPath),

            case {FileAvailable, Method} of
                {true, <<"GET">>} ->
                    #response{code = 200, body_from_file = HandledPath};
                {true, <<"HEAD">>} ->
                    #response{code = 200, body_from_file = HandledPath, headers_only = false};
                _ ->
                    #response{code = 404}
            end
    end.

response_to_text(#response{code = Code, body = Body}) ->
    NewBody = iolist_to_binary(Body),
    "HTTP/1.1 " ++ integer_to_list(Code) ++ " OK\r\nContent-Length: " ++ integer_to_list(size(NewBody)) ++ "\r\nConnection: close\r\n\r\n" ++ NewBody.
