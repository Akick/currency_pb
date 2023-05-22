%%%-------------------------------------------------------------------
%%% @author oleksiika
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 21. тра 2023 19:27
%%%-------------------------------------------------------------------
-module(default).
-author("oleksiika").

-export([init/2]).

init(Req0, State) ->
  Req = cowboy_req:reply(200,
    #{<<"content-type">> => <<"text/plain">>},
    <<"Hello!">>,
    Req0),
  {ok, Req, State}.
