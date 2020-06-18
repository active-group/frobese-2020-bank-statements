-module(subserver).

-behavior(gen_server).

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


handle_info(_Msg, State) ->
    %% CODE zum daten abspeichern
    {noreply, State}.

account_register(State) ->
    gen_server:cast({global, accounts}, {register, node(), self()}),
    gen_server:cast({global, accounts}, {replay, node(), self()}),
    State.

transaction_register(State) ->
    gen_server:call({global, transaction_server}, #register{since = nil}),
    State.