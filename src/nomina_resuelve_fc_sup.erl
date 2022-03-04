%%%-------------------------------------------------------------------
%% @doc nomina_resuelve_fc top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(nomina_resuelve_fc_sup).

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
    SupFlags = #{strategy => one_for_one,
                 intensity => 10,
                 period => 10},
    ChildSpecs = [
		  #{id => nomina_resuelve_fc_server,
		    start => {nomina_resuelve_fc_server, start_link, []},
		    restart => permanent,
		    shutdown => 10000,
		    type => worker,
		    modules => [nomina_resuelve_fc_server]}
		 ],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
