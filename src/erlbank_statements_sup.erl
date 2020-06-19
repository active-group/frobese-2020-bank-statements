%%%-------------------------------------------------------------------
%% @doc erlbank_statements top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(erlbank_statements_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([]) ->
    SupFlags = #{strategy => one_for_all, intensity => 0,
		 period => 1},
    ChildSpecs = child_specs(),
    {ok, {SupFlags, ChildSpecs}}.

child_specs() ->
    [
        % #{id => accounts_faker, start => {accounts_faker, start, [[]]}},
        #{id => transaction_faker, start => {transaction_faker, start, [[]]}},
        #{id => subserver, start => {subserver, start, [[]]}}
    ].     
%% internal functions

