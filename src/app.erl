-module(app).

-behaviour(application).

-export([start/2, stop/1]).

start(normal, _Args) ->
    httpd:start_link().

stop(_State) ->
    ok.
