-module(accounts_faker).

-behavior(gen_server).
-c(logger).

-include("trans_data.hrl").

-export([init/1, start/1,
         handle_cast/2, handle_call/3, handle_info/2]).


%% N: Anfangsstand des Zählers
init(_) ->
    {ok, []}. % Anfangszustand vom gen_server

start(_) ->
    gen_server:start({global, accounts}, ?MODULE, [], []).
                                                  % ^ wird an init übergeben

handle_cast({replay, Pid}, State) ->
    Pid ! {replay, [
        #{account_number => 0, amount => 100000, firstname => <<"Alice">>, surname => <<"Bobbins">>},
        #{account_number => 1, amount => 100000, firstname => <<"Bob">>, surname => <<"Alisons">>}
    ]},
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_info(_Msg, State) ->
    {noreply, State}.

% handle_continue(spam, State) ->
%     erlang:send_after(1000, self(), spam),
%     {noreply, State}.


% handle_info(spam, State) ->
%     Fake_transaction = #transaction{
%         timestamp = os:timestamp(),
%         sender = 0,
%         receiver = 1,
%         amount = rand:uniform(10000)
%     },
%     lists:map(fun (Pid) -> Pid ! Fake_transaction end, State),
    
%     {noreply, State}.
