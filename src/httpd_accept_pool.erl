-module(httpd_accept_pool).

-behaviour(supervisor).

-export([init/1, start_link/1]).

-define(NUM_ACCEPTORS, 20).

init(LSocket) ->
    spawn_link(fun start_listeners/0),
    {
     ok,
     {
      {
       simple_one_for_one, %% Restart strategy
       5,                  %% Max Restarts
       60                  %% Max Time
      },
      [
       {
        httpd_accept_id,   %% ID
        {
         httpd_accept,     %% Module Name
         start_link,       %% Function Name
         [LSocket]         %% Args
        },
        permanent,         %% Restart Type
        5000,              %% Max shutdown wait time (5 sec)
        worker,            %% Child Type
        [httpd_accept]     %% Callback Module
       }
      ]
     }
    }.

start_link(LSocket) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, LSocket).

add_acceptor() ->
    supervisor:start_child(?MODULE, []).

start_listeners() ->
    io:format("starting ~B accept workers~n", [?NUM_ACCEPTORS]),
    [add_acceptor() || _ <- lists:seq(1, ?NUM_ACCEPTORS)],
    ok.
