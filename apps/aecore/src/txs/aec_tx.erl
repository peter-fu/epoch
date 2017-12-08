-module(aec_tx).

-export([apply_signed/3,
         is_coinbase/1,
         signers/1]).
-export([serialize/1,
         deserialize/1,
         serialize_to_binary/1,
         deserialize_from_binary/1]).

%% TX body API: getters, helpers...
-export([fee/1,
         nonce/1,
         origin/1]).

-include("common.hrl").
-include("trees.hrl").
-include("txs.hrl").

-export_type([tx/0,
              signed_tx/0]).

%%%=============================================================================
%%% aec_tx behavior callbacks
%%%=============================================================================

-callback new(Args :: map()) ->
    {ok, Tx :: term()} | {error, Reason :: term()}.

-callback fee(Tx :: term()) ->
    Fee :: integer().

-callback nonce(Tx :: term()) ->
    Nonce :: non_neg_integer() | undefined.

-callback origin(Tx :: term()) ->
    Origin :: pubkey() | undefined.

-callback signers(Tx :: term()) -> [pubkey()].

-callback check(Tx :: term(), Trees :: trees(), Height :: non_neg_integer()) ->
    {ok, NewTrees :: trees()} | {error, Reason :: term()}.

-callback process(Tx :: term(), Trees :: trees(), Height :: non_neg_integer()) ->
    {ok, NewTrees :: trees()}.


%% Relax type spec for now to have different spec in coinbase/spend
-callback serialize(Tx :: term()) -> term().

%% Relax type spec for now to have different spec in coinbase/spend
-callback deserialize(term()) -> Tx :: term().

-callback type() -> binary().

%%%%=============================================================================
%% API
%%%=============================================================================

fee(Tx) ->
    Mod = tx_dispatcher:handler(Tx),
    Mod:fee(Tx).

nonce(Tx) ->
    Mod = tx_dispatcher:handler(Tx),
    Mod:nonce(Tx).

origin(Tx) ->
    Mod = tx_dispatcher:handler(Tx),
    Mod:origin(Tx).


%% @doc Note that we drop invalid transactions, but we allow them in the block.
-spec apply_signed(list(signed_tx()), trees(), non_neg_integer()) ->
                          {list(signed_tx()), trees()}.
apply_signed(SignedTxs, Trees0, Height) ->
    {FilteredSignedTxs, Txs, Trees1, TotalFee} = do_apply_signed(SignedTxs, Trees0, Height),
    Trees2 = grant_fee_to_miner(Txs, Trees1, Height, TotalFee),
    {FilteredSignedTxs, Trees2}.

%% TODO: there should be an easier way to do this...
-spec is_coinbase(signed_tx()) -> boolean().
is_coinbase(Signed) ->
    Tx = aec_tx_sign:data(Signed),
    Mod = tx_dispatcher:handler(Tx),
    Type = Mod:type(),
    <<"coinbase">> =:= Type.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

-spec do_apply_signed(list(signed_tx()), trees(), height()) ->
                             {list(signed_tx()), list(tx()), trees(), non_neg_integer()}.
do_apply_signed(SignedTxs, Trees, Height) ->
    do_apply_signed(SignedTxs, [], [], Trees, 0, Height).

do_apply_signed([], FilteredSignedTxs, Txs, Trees, TotalFee, _Height) ->
    {lists:reverse(FilteredSignedTxs), lists:reverse(Txs), Trees, TotalFee};
do_apply_signed([SignedTx | Rest], FilteredSignedTxs, Txs, Trees0, TotalFee, Height) ->
    case aec_tx_sign:verify(SignedTx) of
        ok ->
            Tx = aec_tx_sign:data(SignedTx),
            case check_single(Tx, Trees0, Height) of
                {ok, Trees1} ->
                    {ok, Trees2} = process_single(Tx, Trees1, Height),
                    TxFee = fee(Tx),
                    do_apply_signed(Rest, [SignedTx | FilteredSignedTxs], [Tx | Txs],
                                    Trees2, TotalFee + TxFee, Height);
                {error, Reason} ->
                    lager:debug("Tx ~p cannot be applied due to an error ~p",
                                [Tx, Reason]),
                    do_apply_signed(Rest, FilteredSignedTxs, Txs, Trees0, TotalFee, Height)
            end;
        {error, _Reason} ->
            do_apply_signed(Rest, FilteredSignedTxs, Txs, Trees0, TotalFee, Height)
    end.

signers(Tx) ->
    Mod = tx_dispatcher:handler(Tx),
    Mod:signers(Tx).

serialize(Tx) ->
    Mod = tx_dispatcher:handler(Tx),
    Mod:serialize(Tx).

deserialize(Data) ->
    Mod = tx_dispatcher:handler_by_type(type_of(Data)),
    Mod:deserialize(Data).

serialize_to_binary(Tx) ->
    msgpack:pack(serialize(Tx)).

deserialize_from_binary(Bin) ->
    {ok, Unpacked} = msgpack:unpack(Bin),
    deserialize(Unpacked).

type_of([#{}|_] = L) ->
    [Type] = [T || #{<<"type">> := T} <- L],
    Type;
type_of([Type|_]) ->
    Type.

%%------------------------------------------------------------------------------
%% Check transaction. Prepare state tree: e.g., create newly referenced account
%%------------------------------------------------------------------------------
-spec check_single(tx(), trees(), non_neg_integer()) -> {ok, trees()} | {error, term()}.
check_single(Tx, Trees, Height) ->
    Mod = tx_dispatcher:handler(Tx),
    Mod:check(Tx, Trees, Height).

%%------------------------------------------------------------------------------
%% Process the transaction. Accounts must already be present in the state tree
%%------------------------------------------------------------------------------
-spec process_single(tx(), trees(), non_neg_integer()) -> {ok, trees()}.
process_single(Tx, Trees, Height) ->
    Mod = tx_dispatcher:handler(Tx),
    Mod:process(Tx, Trees, Height).

-spec grant_fee_to_miner(list(tx()), trees(), height(), non_neg_integer()) ->
                                trees().
grant_fee_to_miner([], Trees, _Height, 0) ->
    lager:debug("No transactions in genesis block"),
    Trees;
grant_fee_to_miner(Txs, Trees0, Height, TotalFee) ->
    %% Consider creation of aec_accounts_service,
    %% which will take state trees, height and list of (pubkey, operation) pairs
    %% (valuable also during txs processing)
    case [ AccountPubkey || #coinbase_tx{account = AccountPubkey} <- Txs] of
      [] ->
        lager:info("Invalid coinbase_tx transaction in block -- no fee"),
        Trees0;
      [MinerPubkey] ->
        AccountsTrees0 = aec_trees:accounts(Trees0),

        {ok, Account0} = aec_accounts:get(MinerPubkey, AccountsTrees0),
        {ok, Account} = aec_accounts:earn(Account0, TotalFee, Height),

        {ok, AccountsTrees} = aec_accounts:put(Account, AccountsTrees0),
        Trees = aec_trees:set_accounts(Trees0, AccountsTrees),
        Trees
    end.

