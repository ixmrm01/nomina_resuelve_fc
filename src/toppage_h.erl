%% Feel free to use, reuse and abuse the code in this file.

%% @doc Nomina Resuelve FC handler.
-module(toppage_h).

-export([init/2]).
-export([allowed_methods/2]).
-export([content_types_accepted/2]).
-export([json_a_calcular_nomina/2]).

init(Req, Opts) ->
  {cowboy_rest, Req, Opts}.

allowed_methods(Req, State) ->
  {[<<"GET">>, <<"POST">>], Req, State}.

content_types_accepted(Req, State) ->
  {[{{<<"application">>, <<"json">>, '*'}, json_a_calcular_nomina}], Req, State}.

json_a_calcular_nomina(Req, State) ->
  {ok, Data, Req1} = cowboy_req:read_body(Req),
  MapaEntrada = jsx:decode(Data, []),
  MapaSalida = nomina_resuelve_fc_server:obtener_nomina(MapaEntrada),
  Body = jsx:encode(MapaSalida),
  Req2 = cowboy_req:set_resp_header(<<"content-type">>, <<"application/json">>, Req1),
  Req3 = cowboy_req:set_resp_body(Body, Req2),
  {true, Req3, State}.

