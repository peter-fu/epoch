%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc Memory pool of unconfirmed transactions.
%%%
%%% Unconfirmed transactions are transactions not included in any
%%% block in the longest chain.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(aec_tx_pool).

-behaviour(gen_server).

-include("common.hrl").
-include("txs.hrl").

-define(MEMPOOL, mempool).
-define(KEY_NONCE_PATTERN(Sender), {{'_', Sender, '$1'}, '_'}).

%% API
-export([start_link/0,
         stop/0]).
-export([push/1, push/2,
         delete/1,
         peek/1,
         fork_update/2,
         get_max_nonce/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(TAB, ?MODULE).

-record(state, {db :: pool_db()}).

-type negated_fee() :: non_pos_integer().
-type non_pos_integer() :: neg_integer() | 0.

-type pool_db_key() ::
        {negated_fee(), pubkey(), non_neg_integer()} | undefined.
-type pool_db_value() :: signed_tx().
-type pool_db() :: atom().

-type event() :: tx_created | tx_received.

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() ->
    %% implies also clearing the mempool
    gen_server:stop(?SERVER).

-define(PUSH_EVENT(E), Event =:= tx_created; Event =:= tx_received).

%% INFO: Transaction from the same sender with the same nonce and fee
%%       will be overwritten
-spec push(signed_tx()|list(signed_tx())) -> ok.
push(Tx) ->
    push(Tx, tx_created).

-spec push(signed_tx()|list(signed_tx()), event()) -> ok.
push([_|_] = Txs, Event) when ?PUSH_EVENT(Event) ->
    gen_server:call(?SERVER, {push, Txs, Event});
push([], _) -> ok;
push(Tx, Event) when ?PUSH_EVENT(Event) ->
    gen_server:call(?SERVER, {push, [Tx], Event}).

-spec delete(signed_tx()|list(signed_tx())) -> ok.
delete(Txs) when is_list(Txs) ->
    gen_server:call(?SERVER, {delete, Txs});
delete(Tx) ->
    gen_server:call(?SERVER, {delete, [Tx]}).

-spec get_max_nonce(pubkey()) -> {ok, non_neg_integer()} | undefined.
get_max_nonce(Sender) ->
    gen_server:call(?SERVER, {get_max_nonce, Sender}).

%% The specified maximum number of transactions avoids requiring
%% building in memory the complete list of all transactions in the
%% pool.
-spec peek(pos_integer() | infinity) -> {ok, [signed_tx()]}.
peek(MaxN) when is_integer(MaxN), MaxN >= 0; MaxN =:= infinity ->
    gen_server:call(?SERVER, {peek, MaxN}).

-spec fork_update(AddedToChain::[signed_tx()], RemovedFromChain::[signed_tx()]) -> ok.
fork_update(AddedToChain, RemovedFromChain) ->
    %% Add back transactions to the pool from discarded part of the chain
    %% Mind that we don't need to add those which are incoming in the fork
    %% TODO: check if local diff is indeed cheaper than hitting ETS table more times
    push(RemovedFromChain -- AddedToChain),
    %% Remove transactions added to the chain.
    %% Mind that we don't need to remove those that were included in the old chain
    delete(AddedToChain -- RemovedFromChain),
    ok.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    {ok, Db} = pool_db_open(),
    State = #state{db = Db},
    {ok, State}.

handle_call({get_max_nonce, Sender}, _From, #state{db = Mempool} = State) ->
    {reply, int_get_max_nonce(Mempool, Sender), State};
handle_call({push, Txs, Event}, _From, #state{db = Mempool} = State) ->
    [pool_db_put(Mempool, pool_db_key(Tx), Tx, Event) || Tx <- Txs],
    {reply, ok, State};
handle_call({delete, Txs}, _From, #state{db = Mempool} = State) ->
    [pool_db_delete(Mempool, pool_db_key(Tx)) || Tx <- Txs],
    {reply, ok, State};
handle_call({peek, MaxNumberOfTxs}, _From, #state{db = Mempool} = State)
  when is_integer(MaxNumberOfTxs), MaxNumberOfTxs >= 0;
       MaxNumberOfTxs =:= infinity ->
    Txs = pool_db_peek(Mempool, MaxNumberOfTxs),
    {reply, {ok, Txs}, State};
handle_call(Request, From, State) ->
    lager:warning("Ignoring unknown call request from ~p: ~p", [From, Request]),
    {noreply, State}.

handle_cast(Msg, State) ->
    lager:warning("Ignoring unknown cast message: ~p", [Msg]),
    {noreply, State}.

handle_info({'ETS-TRANSFER', _, _, _}, State) ->
    {noreply, State};
handle_info(Info, State) ->
    lager:warning("Ignoring unknown info: ~p", [Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec int_get_max_nonce(pool_db(), pubkey()) -> {ok, non_neg_integer()} | undefined.
int_get_max_nonce(Mempool, Sender) ->
    case lists:flatten(ets:match(Mempool, ?KEY_NONCE_PATTERN(Sender))) of
        [] ->
            undefined;
        Nonces ->
            MaxNonce = lists:max(Nonces),
            {ok, MaxNonce}
    end.


pool_db_key(SignedTx) ->
    Tx = aec_tx_sign:data(SignedTx),
    %% INFO: Sort by fee
    %%       TODO: sort by fee, then by origin, then by nonce

    %% INFO: * given that nonce is an index of transactions for a user,
    %%         the following key is unique for a transaction
    %%       * negative fee places high profit transactions at the beginning
    %%       * ordered_set type enables implicit overwrite of the same txs
    exclude_coinbase({-aec_tx:fee(Tx), aec_tx:origin(Tx), aec_tx:nonce(Tx)}).

exclude_coinbase({_, undefined, undefined}) ->
    undefined; %% Identify coinbase
exclude_coinbase({Fee, Origin, Nonce}) ->
    {Fee, Origin, Nonce}.

-spec pool_db_open() -> {ok, pool_db()}.
pool_db_open() ->
    {ok, ets:new(?MEMPOOL, [ordered_set, public, named_table])}.

-spec pool_db_peek(pool_db(), MaxNumber::pos_integer() | infinity) ->
                          [pool_db_value()].
pool_db_peek(_, 0) -> [];
pool_db_peek(Mempool, Max) ->
    sel_return(
      ets_select(Mempool, [{ {'_', '$1'}, [], ['$1'] }], Max)).

ets_select(T, P, infinity) ->
    ets:select(T, P);
ets_select(T, P, N) when is_integer(N), N >= 1 ->
    ets:select(T, P, N).

sel_return(L) when is_list(L) -> L;
sel_return('$end_of_table' ) -> [];
sel_return({Matches, _Cont}) -> Matches.

-spec pool_db_put(pool_db(), pool_db_key(), pool_db_value(), event()) -> true.
pool_db_put(_, undefined, _, _) ->
    false; %% Ignore coinbase
pool_db_put(Mempool, Key, Tx, Event) ->
    case ets:member(Mempool, Key) of
        false ->
            case aec_tx_sign:verify(Tx) of
                ok ->
                    ets:insert(Mempool, {Key, Tx}),
                    aec_events:publish(Event, Tx),
                    true;
                {error, Reason} ->
                    lager:error("verification error: ~p; Tx = ~p",
                                [Reason, Tx]),
                    false
            end;
        true ->
            lager:debug("Tx already in pool (~p)", [Key]),
            false
    end.

-spec pool_db_delete(pool_db(), pool_db_key()) -> true.
pool_db_delete(_, undefined) ->
    true; %% Ignore coinbase
pool_db_delete(Mempool, Key) ->
    ets:delete(Mempool, Key).
