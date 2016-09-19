-type http_method() :: binary().
-type http_path() :: binary().
-type http_code() :: 100..600.

-record(request, {
          method :: http_method(),
          path :: http_path()
         }).

-record(response, {
          code = 400 :: http_code(),
          body = <<"">> :: binary(),
          body_from_file = <<"">> :: binary(),
          content_type = "text/html" :: binary()
         }).

-define(DELIMITERS, [<<" ">>, <<"?">>, <<"\r">>, <<"\n">>, <<"\r\n">>]).
