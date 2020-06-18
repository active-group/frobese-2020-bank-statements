%% This module represents the business logic layer

-module(business_logic).
-include("data.hrl").
-export([sort_tx/1]).

%% Takes a list of transactions and returns them sorted by their id (asc)

sort_tx(Txs) ->
    lists:sort(fun(Tx1, Tx2) -> Tx2#transaction.timestamp < Tx1#transaction.timestamp end, Txs).
