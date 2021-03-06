%%%-------------------------------------------------------------------
%% @doc erlbank_statements public API
%% @end
%%%-------------------------------------------------------------------

-module(erlbank_statements_app).

-behaviour(application).

-export([start/2, stop/1]).

start_cowboy() ->
    %% Cowboy test code
    Dispatch = cowboy_router:compile([{'_',
				       [{"/bank-statements", web_frontend, index},
					{"/bank-statements/request",
					 web_frontend, request}]}]),
    {ok, _} = cowboy:start_clear(my_http_listener,
				 [{port, 9001}],
				 #{env => #{dispatch => Dispatch}}).

start(_StartType, _StartArgs) ->
    % database:init_database(),
    lager:info("Starting bank-statements service: ~p, cookie: ~p~n",
	       [node(), erlang:get_cookie()]),
    start_cowboy(),
    AccountsPing = net_adm:ping('accounts@accounts-host'),
    lager:info("Pinging accounts: ~p", [AccountsPing]),
    TransactionsPing = net_adm:ping('transactions@transactions-host'),
    lager:info("Pinging transactions: ~p", [TransactionsPing]),
    timer:sleep(5000), % give the global references time to show up
    erlbank_statements_sup:start_link().

stop(_State) -> ok.
