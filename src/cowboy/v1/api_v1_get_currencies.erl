%%%-------------------------------------------------------------------
%%% @author oleksiika
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 21. тра 2023 18:35
%%%-------------------------------------------------------------------
-module(api_v1_get_currencies).
-author("oleksiika").
%%-behavior(cowboy_handler).

%% API
-export([init/2]).

-include_lib("include/currencies.hrl").

init(Req0, State) ->
  try
    Currencies = ets:tab2list(currencies),
    ExchangeRates = [io_lib:format("<row><exchangerate ccy=\"~s\" base_ccy=\"~s\" buy=\"~s\" sale=\"~s\"/></row>~n", [Ccy, BaseCcy, Buy, Sale]) || #currency{ccy = Ccy, base_ccy = BaseCcy, buy = Buy, sale = Sale} <- Currencies],
    logger:info("~p ~p", [?MODULE, ExchangeRates]),
    Response = list_to_binary(io_lib:format("<exchangerates>~n~s</exchangerates>", [ExchangeRates])),
    Req = cowboy_req:reply(200, #{<<"content-type">> => <<"application/xml; charset=utf-8">>}, Response, Req0),
    {ok, Req, State}
  catch
    _:_ ->
      R = cowboy_req:reply(500, #{<<"content-type">> => <<"application/text; charset=utf-8">>}, <<"Internal server error">>, Req0),
      {ok, R, State}
  end.


