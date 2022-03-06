
%%%-------------------------------------------------------------------
%% @doc nomina_resuelve_fc public API
%% @end
%%%-------------------------------------------------------------------

-module(nomina_resuelve_fc_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
  Dispatch =
    cowboy_router:compile([
			   {'_', [
				  {"/api/nomina", toppage_h, []}
				 ]}
			  ]),
  {ok, _} = cowboy:start_clear(http,
			       [{port, 8080}],
			       #{env => #{dispatch => Dispatch}}
			      ),
  nomina_resuelve_fc_sup:start_link().

stop(_State) ->
  ok = cowboy:stop_listener(http).

%% internal functions
