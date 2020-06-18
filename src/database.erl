%% This module represents the database layer

-module(database).
-include("data.hrl").
-export([init_database/0,
         put_account/1, get_account/1,
         put_transaction/1, get_all_transactions/1]).

%% destroy tables in case they already existed
destroy_tables() ->
    mnesia:del_table_copy(transaction, node()),
    mnesia:del_table_copy(account, node()).

create_tables() ->
    mnesia:create_table(transaction, [{attributes, record_info(fields, transaction)}]),
    mnesia:create_table(account, [{attributes, record_info(fields, account)}]).


init_database() ->
    mnesia:create_schema([node()]),
    mnesia:start(),
    destroy_tables(),
    create_tables(),
    ok.


write(Obj) ->
    Fun = fun() -> mnesia:write(Obj) end,
    {atomic, ok} = mnesia:transaction(Fun),
    ok.


-spec read_one(mnesia:table(), unique_id()) -> {ok, tuple()} | {error, not_found | more_than_one}.
read_one(Table, Id) ->
    Fun = fun() -> mnesia:read(Table, Id) end,
    {atomic, Res} = mnesia:transaction(Fun),
    case Res of
        [A] -> {ok, A};
        []  -> {error, not_found};
        [_ | _] -> {error, more_than_one}
    end.

% -spec read_all(mnesia:table()) -> list(tuple()).
% read_all(Table) ->
%     Fun = fun() -> mnesia:select(Table,[{'_',[],['$_']}]) end,
%     {atomic, Res} = mnesia:transaction(Fun),
%     Res.


%% ALL
-spec put_account(#account{}) -> ok.
put_account(Account) ->
    write(Account).

-spec get_account(account_number()) -> {ok, #account{}} | {error, any()}.
get_account(AccountNumber) ->
    read_one(account, AccountNumber).

-spec put_transaction(#transaction{}) -> ok.
put_transaction(Transaction) ->
    write(Transaction).

% -spec get_transaction(unique_id()) -> {ok, #transaction{}} | {error, any()}.
% get_transaction(TransactionId) ->
%     read_one(transaction, TransactionId).

% -spec get_all_transactions() -> list(#transaction{}).
% get_all_transactions() ->
%     read_all(transaction).

-spec get_all_transactions(account_number()) -> list(#transaction{}).
get_all_transactions(AccountNr) ->
    Fun = fun() ->
            mnesia:select(transaction,
                           [{'$1',
                            [{'orelse',
                                {'==', {element, #transaction.from_acc_nr, '$1'}, AccountNr},
                                {'==', {element, #transaction.to_acc_nr, '$1'}, AccountNr}}],
                            ['$_']}]) 
          end,
    {atomic, Res} = mnesia:transaction(Fun),
    Res.
