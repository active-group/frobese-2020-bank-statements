-module(transaction_faker).

-behavior(gen_server).
-c(logger).

-include("trans_data.hrl").

-export([init/1, start/1,
         handle_cast/2, handle_call/3, handle_info/2, handle_continue/2]).


%% N: Anfangsstand des Zählers
init(_) ->
    {ok, [], {continue, spam}}. % Anfangszustand vom gen_server

start(_) ->
    gen_server:start({global, transaction_server}, ?MODULE, [], []).
                                                  % ^ wird an init übergeben


handle_cast(_Msg, State) ->
    {noreply, State}.

handle_call(#register{}, {Pid, _Tag}, State) ->
    {reply, ok, [Pid | State]}.

handle_continue(spam, State) ->
    erlang:send_after(1000, self(), spam),
    {noreply, State}.


handle_info(spam, State) ->
    Fake_transaction = #transaction{
        timestamp = os:timestamp(),
        sender = 0,
        receiver = 1,
        amount = rand:uniform(10000)
    },
    lists:map(fun (Pid) -> Pid ! Fake_transaction end, State),
    
    {noreply, State, {continue, spam}}.
