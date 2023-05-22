%%%-------------------------------------------------------------------
%%% @author Oleksii Kliuiev
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 21. тра 2023 18:09
%%%-------------------------------------------------------------------
-module(currencies).
-author("Oleksii Kliuiev").

%% API
-export([
  start/0,
  stop/0
]).

-define(APPS, [
  compiler,
  syntax_tools,
  inets,
  crypto,
  asn1,
  public_key,
  ssl,
  ranch,
  cowlib,
  cowboy,
  sync,
  jsx,
  mimetypes,
  currencies]).
%% ===================================================================
%% API functions
%% ===================================================================

start() ->
  ok = ensure_started(?APPS),
  ok = sync:go().

stop() ->
  sync:stop(),
  ok = stop_apps(lists:reverse(?APPS)).

%% ===================================================================
%% Internal functions
%% ===================================================================

ensure_started([]) -> ok;
ensure_started([App | Apps]) ->
  case application:start(App) of
    ok -> ensure_started(Apps);
    {error, {already_started, App}} ->
      logger:error("already_started: ~p", [App]),
      ensure_started(Apps);
    {error, _Reason} ->
      logger:error("error: ~p before app: ~p", [_Reason, App]),
      ensure_started(Apps)
  end.

stop_apps([]) -> ok;
stop_apps([App | Apps]) ->
  application:stop(App),
  stop_apps(Apps).

