-module(subserver).

-behavior(gen_server).
-c(logger).

-include("trans_data.hrl").

-export([init/1, start/1,
         handle_cast/2, handle_call/3, handle_info/2, handle_continue/2]).


%% N: Anfangsstand des Zählers
init(_) ->
    database:init_database(),
    {ok, #{}, {continue, register}}. % Anfangszustand vom gen_server

start(_) ->
    gen_server:start({global, global_inc}, ?MODULE, [], []).
                                                  % ^ wird an init übergeben


handle_cast(_Msg, State) ->
    {noreply, State}.

handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_continue(register, State) ->
    State = account_register(State),
    State = transaction_register(State),
    {noreply, State}.

handle_info({replay, Accounts}, State) when is_list(Accounts) ->
    lists:foreach(fun (Account) -> 
        case Account of
            #{account_number := Number, amount := Amount, firstname := Firstname, surname := Surname} ->
                business_logic:create_account(Number, Firstname, Surname, Amount);
            _ -> error      
        end, 
        Accounts
    end),
    {noreply, State};

handle_info({new, Account}, State) when is_map(Account) ->
    case Account of
        #{account_number := Number, amount := Amount, firstname := Firstname, surname := Surname} ->
            business_logic:create_account(Number, Firstname, Surname, Amount);
        _ -> error      
    end,
    {noreply, State};

handle_info(#transaction{} = Transaction, State) ->
    business_logic:create_transaction(Transaction#transaction.timestamp, Transaction#transaction.sender, Transaction#transaction.receiver, Transaction#transaction.amount),
    {noreply, State}.

account_register(State) ->
    gen_server:cast({global, accounts}, {register, self()}),
    gen_server:cast({global, accounts}, {replay, self()}),

    State.

transaction_register(State) ->
    gen_server:call({global, transaction_server}, #register{since = nil}),
    State.