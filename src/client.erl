%% This module represents the frontend layer

-module(client).
-include("data.hrl").
-export([bank_statement/1]).

%% returns the name of the person associated to the account 
%% given by account number.
-spec name_by_account_nr(account_number()) -> string().
name_by_account_nr(AccountNumber) ->
    {ok, Account} = database:get_account(AccountNumber),
    binary_to_list(Account#account.firstname) ++ " " ++ binary_to_list(Account#account.surname).


%% prints the header of a bank statement, namely the full name and the
%% current balance, associated with the account number to stdout.
print_head(AccountNumber, Txs) ->
    {ok, Account} = database:get_account(AccountNumber),
    Name = name_by_account_nr(AccountNumber),
    Balance = business_logic:calculate_balance(Account, Txs),
    io:format("~nBank statement for: ~s~n", [Name]),
    io:format("---------------------------------------------------- ~n", []),
    io:format("Balance: ~p~n", [Balance]),
    io:format("---------------------------------------------------- ~n", []).


%% takes an transaction record and prints it to stdout.
print_tx(Tx) ->
    Name1 = name_by_account_nr(Tx#transaction.from_acc_nr),
    Name2 = name_by_account_nr(Tx#transaction.to_acc_nr),
    Amount = Tx#transaction.amount,
    Id = Tx#transaction.id,
    io:format("#~p\t ~p\t ~s \t -> ~s ~n", [Id, Amount, Name1, Name2]).

%% takes a list of transactions records and prints them to stdout
print_txs(Txs) ->
    lists:map(fun print_tx/1, Txs).


%% takes an account number and prints a bank statement to stdout.
%% That is a full name, the current balance, and a list of
%% transactions associated with the account.
bank_statement(AccountNumber) ->
    Txs = database:get_all_transactions(AccountNumber),
    RelevantTxs = lists:filter(fun(Tx) -> Tx#transaction.from_acc_nr == AccountNumber orelse
                                              Tx#transaction.to_acc_nr == AccountNumber
                               end, Txs),

    SortedRelevantTxs = business_logic:sort_tx(RelevantTxs),

    print_head(AccountNumber, Txs),
    print_txs(SortedRelevantTxs),

    io:format("~n~n", []).
