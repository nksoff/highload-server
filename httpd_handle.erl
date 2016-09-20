-module(httpd_handle).
-include_lib("kernel/include/file.hrl").
-include("httpd.hrl").

-export([action/1]).

action(Socket) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Data} ->
            Request = get_request(Data),
            Response = get_response(Request),
            make_response(Response, Socket),
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
            #response{code = 405};
        {_, false} ->
            #response{code = 405};
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

code_to_header(Code) ->
    CodeStr = "HTTP/1.1 " ++ integer_to_list(Code) ++ " ",
    case Code of
        200 ->
            CodeStr ++ "OK";
        404 ->
            CodeStr ++ "Not found";
        405 ->
            CodeStr ++ "Method not allowed"
    end.

date_for_header() -> 
    {Date, {Hours, Minutes, Seconds}} = calendar:universal_time(),
	DayOfWeek = element(calendar:day_of_the_week(Date), {"Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"}),
	{Year, MonthNumber, Day} = Date,
	Month = element(MonthNumber, {"Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"}),
	io_lib:format("~s, ~B ~s ~B ~2..0B:~2..0B:~2..0B GMT", [DayOfWeek, Day, Month, Year, Hours, Minutes, Seconds]).

common_headers() ->
    "Server: httpd" ++
    "\r\n" ++
    "Date: " ++
    date_for_header() ++
    "\r\n" ++
    "Connection: close" ++
    "\r\n".

mime_type(File) ->
    case filename:extension(File) of
        <<".html">> -> "text/html";
        <<".css">>  -> "text/css";
        <<".js">>   -> "application/x-javascript";
        <<".jpg">>  -> "image/jpeg";
        <<".jpeg">> -> "image/jpeg";
        <<".png">>  -> "image/png";
        <<".gif">>  -> "image/gif";
        <<".swf">>  -> "application/x-shockwave-flash";
        _ ->
            undefined
    end.

file_headers(File) ->
    MimeType = mime_type(list_to_binary(File)),
    {ok, FileInfo} = file:read_file_info(File),
    Size = FileInfo#file_info.size,

    ContentLength = "Content-Length: " ++ integer_to_list(Size) ++ "\r\n",

    case MimeType of
        undefined ->
            ContentLength;
        _ ->
            "Content-Type: " ++ MimeType ++ "\r\n" ++ ContentLength
    end.

make_response(#response{code = Code, body_from_file = BodyFromFile, headers_only = HeadersOnly}, Socket) ->
    StartHeaders = code_to_header(Code) ++
    "\r\n" ++
    common_headers(),

    case BodyFromFile of
        <<"">> -> % not a file response
            gen_tcp:send(Socket, StartHeaders),
            gen_tcp:send(Socket, "\r\n");
        _ ->
            gen_tcp:send(Socket, StartHeaders ++ file_headers(BodyFromFile) ++ "\r\n"),
            case HeadersOnly of
                false ->
                    file:sendfile(list_to_binary(BodyFromFile), Socket)
            end
    end.
