%% This module represents the business logic layer

-module(business_logic).
-include("data.hrl").
-export([sort_tx/1, create_account/4, create_transaction/4, calculate_balance/2]).

%% Takes a list of transactions and returns them sorted by their id (asc)

sort_tx(Txs) ->
    lists:sort(fun(Tx1, Tx2) -> Tx2#transaction.timestamp < Tx1#transaction.timestamp end, Txs).

calculate_balance(Account, Transactions) ->
    lists:foldl(fun(Transaction, Balance) ->
        case Transaction of
            #transaction{from_acc_nr = From} when From == Account#account.account_number -> 
                Balance - Transaction#transaction.amount;
             _T ->
                Balance + Transaction#transaction.amount
            end end, Account#account.amount, Transactions).

create_account(Number, Firstname, Surname, Amount) ->
    Account = #account{account_number = Number, firstname = Firstname, surname = Surname, amount = Amount},
    database:put_account(Account).

create_transaction(Timestamp, From, To, Amount) ->
    Id = database:unique_tx_id(),
    Transaction = #transaction{id = Id, timestamp = Timestamp, from_acc_nr = From, to_acc_nr = To, amount = Amount},
    database:put_transaction(Transaction).
