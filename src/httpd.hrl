-type http_method() :: binary().
-type http_path() :: binary().
-type http_code() :: 100..600.

-record(request, {
          method :: http_method(),
          path :: http_path()
         }).

-record(response, {
          code = 405 :: http_code(),
          body_from_file = <<"">> :: binary(),
          headers_only = false :: boolean()
         }).

-define(DELIMITERS, [<<" ">>, <<"?">>, <<"\r">>, <<"\n">>, <<"\r\n">>]).
-define(DEFAULT_INDEX, "index.html").
