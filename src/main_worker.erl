%%%-------------------------------------------------------------------
%%% @author oleksiika
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 21. тра 2023 19:25
%%%-------------------------------------------------------------------
-module(main_worker).
-author("oleksiika").

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).


-define(SERVER, ?MODULE).

-define(URLS, [
  "https://api.privatbank.ua/p24api/pubinfo?json&exchange&coursid=5"
  , "https://api.privatbank.ua/p24api/pubinfo?json&exchange&coursid=4"
]).

-record(main_worker_state, {}).


-include_lib("include/currencies.hrl").


%%%===================================================================
%%% API
%%%===================================================================

%% @doc Spawns the server and registers the local name (unique)
-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc Initializes the server
-spec(init(Args :: term()) ->
  {ok, State :: #main_worker_state{}} | {ok, State :: #main_worker_state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([]) ->
  ets:new(currencies, [set, named_table, {keypos, 2}]),
  ets:new(currencies_cache, [set, named_table]),
  update_currencies(),
  {ok, #main_worker_state{}}.

%% @private
%% @doc Handling call messages
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #main_worker_state{}) ->
  {reply, Reply :: term(), NewState :: #main_worker_state{}} |
  {reply, Reply :: term(), NewState :: #main_worker_state{}, timeout() | hibernate} |
  {noreply, NewState :: #main_worker_state{}} |
  {noreply, NewState :: #main_worker_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #main_worker_state{}} |
  {stop, Reason :: term(), NewState :: #main_worker_state{}}).
handle_call(_Request, _From, State = #main_worker_state{}) ->
  {reply, ok, State}.

%% @private
%% @doc Handling cast messages
-spec(handle_cast(Request :: term(), State :: #main_worker_state{}) ->
  {noreply, NewState :: #main_worker_state{}} |
  {noreply, NewState :: #main_worker_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #main_worker_state{}}).
handle_cast(_Request, State = #main_worker_state{}) ->
  {noreply, State}.

%% @private
%% @doc Handling all non call/cast messages
-spec(handle_info(Info :: timeout() | term(), State :: #main_worker_state{}) ->
  {noreply, NewState :: #main_worker_state{}} |
  {noreply, NewState :: #main_worker_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #main_worker_state{}}).
handle_info({update_currencies}, State) ->
  update_currencies(),
  {noreply, State};
handle_info(_Info, State = #main_worker_state{}) ->
  {noreply, State}.

%% @private
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #main_worker_state{}) -> term()).
terminate(_Reason, _State = #main_worker_state{}) ->
  ok.

%% @private
%% @doc Convert process state when code is changed
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #main_worker_state{},
    Extra :: term()) ->
  {ok, NewState :: #main_worker_state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State = #main_worker_state{}, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec(get_currencies_from_source(list(), list()) -> list()).
get_currencies_from_source([], Result) -> Result;
get_currencies_from_source([URL | Rest], Result) ->
  case httpc:request(URL) of
    {ok, {{_Version, 200, _ReasonPhrase}, _Headers, Body}} ->
      case jsx:is_json(list_to_binary(Body)) of
        true ->
          Json = jsx:decode(list_to_binary(Body), [return_maps]),
          get_currencies_from_source(Rest, Result ++ Json);
        false ->
          logger:error("JSON error while getting currency rates: ~p", [Body]),
          get_currencies_from_source(Rest, Result)
      end;
    {error, Reason} ->
      logger:error("Connection error while getting currency rates: ~p", [Reason]),
      get_currencies_from_source(Rest, Result)
  end.

-spec(save_currencies(list()) -> true ).
save_currencies(Currencies) ->
  ets:insert(currencies, Currencies).

-spec(prepare_for_save(list()) -> list()).
prepare_for_save(Currencies) ->
  lists:map(fun(Currency) ->
    Ccy = maps:get(<<"ccy">>, Currency),
    BaseCcy = maps:get(<<"base_ccy">>, Currency),
    #currency{
      name = <<Ccy/binary, "_", BaseCcy/binary>>,
      ccy = Ccy,
      base_ccy = BaseCcy,
      buy = maps:get(<<"buy">>, Currency),
      sale = maps:get(<<"sale">>, Currency),
      updated_at = erlang:system_time(second)
    }
  end, Currencies).

-spec(update_currencies() -> ok).
update_currencies() ->
  CurrencyRates = get_currencies_from_source(?URLS, []),
  CurrenciesForSave = prepare_for_save(CurrencyRates),
  _R = save_currencies(CurrenciesForSave),
  logger:info("Result of update: ~p", [_R]),
  erlang:send_after(55000, self(), {update_currencies}),
  ok.


