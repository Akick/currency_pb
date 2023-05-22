%%%-------------------------------------------------------------------
%%% @author oleksiika
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 22. тра 2023 19:06
%%%-------------------------------------------------------------------
-author("oleksiika").

-record(currency, {
  name :: binary(),
  ccy :: binary(),
  base_ccy :: binary(),
  buy :: binary(),
  sale :: binary(),
  updated_at :: integer()
}).
