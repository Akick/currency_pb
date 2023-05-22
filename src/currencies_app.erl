%%%-------------------------------------------------------------------
%% @doc currencies public API
%% @end
%%%-------------------------------------------------------------------

-module(currencies_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/", default, []},
            {"/api/v1/get_currencies", api_v1_get_currencies, [get]}
        ]}
    ]),
    {ok, CowboyResult} = cowboy:start_clear(my_http_listener,
        [{port, 8082}],
        #{env => #{dispatch => Dispatch}}
    ),
    logger:info("Cowboy started: ~p", [CowboyResult]),
    cowboy_sup:start_link(),

    currencies_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
