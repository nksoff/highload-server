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

-record(httpd_settings, {
          document_path = "./" :: list(),
          port = 80 :: integer(),
          ip = {127, 0, 0, 1} :: inet:socket_address(),
          cpu = 1 :: integer()
         }).

-define(DELIMITERS, [<<" ">>, <<"?">>, <<"\r">>, <<"\n">>, <<"\r\n">>]).
-define(DEFAULT_INDEX, "index.html").

-define(ENV_DOCUMENT_PATH, "HTTPD_DOCUMENT_PATH").
-define(ENV_CPU, "HTTPD_CPU").
