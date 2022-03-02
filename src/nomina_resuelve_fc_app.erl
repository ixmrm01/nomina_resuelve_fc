%%%-------------------------------------------------------------------
%% @doc nomina_resuelve_fc public API
%% @end
%%%-------------------------------------------------------------------

-module(nomina_resuelve_fc_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    nomina_resuelve_fc_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
