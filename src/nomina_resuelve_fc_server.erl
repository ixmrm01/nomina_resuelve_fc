%%%-------------------------------------------------------------------
%%% @author Martin Rodriguez <martin@kapps.macbookpro.dev>
%%% @copyright (C) 2022, Martin Rodriguez
%%% @doc
%%%
%%% @end
%%% Created :  3 Mar 2022 by Martin Rodriguez <martin@kapps.macbookpro.dev>
%%%-------------------------------------------------------------------
-module(nomina_resuelve_fc_server).

-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([obtener_nomina/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3, format_status/2]).

-define(SERVER, ?MODULE).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%% @end
%%--------------------------------------------------------------------
-spec start_link() -> {ok, Pid :: pid()} |
	{error, Error :: {already_started, pid()}} |
	{error, Error :: term()} |
	ignore.
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec obtener_nomina(map()) -> list(). 
obtener_nomina(Trama) ->
  gen_server:call(?SERVER, {obtener_nomina, Trama}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%% @end
%%--------------------------------------------------------------------
-spec init(Args :: term()) -> {ok, State :: term()} |
	{ok, State :: term(), Timeout :: timeout()} |
	{ok, State :: term(), hibernate} |
	{stop, Reason :: term()} |
	ignore.
init([]) ->
  process_flag(trap_exit, true),
  {ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%% @end
%%--------------------------------------------------------------------
-spec handle_call(Request :: term(), From :: {pid(), term()}, State :: term()) ->
	{reply, Reply :: term(), NewState :: term()} |
	{reply, Reply :: term(), NewState :: term(), Timeout :: timeout()} |
	{reply, Reply :: term(), NewState :: term(), hibernate} |
	{noreply, NewState :: term()} |
	{noreply, NewState :: term(), Timeout :: timeout()} |
	{noreply, NewState :: term(), hibernate} |
	{stop, Reason :: term(), Reply :: term(), NewState :: term()} |
	{stop, Reason :: term(), NewState :: term()}.
handle_call({obtener_nomina, Jugadores}, _From, State) ->
  Reply = nomina_equipo(Jugadores),
  {reply, Reply, State};
handle_call(_Request, _From, State) ->
  Reply = undefined,
  {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%% @end
%%--------------------------------------------------------------------
-spec handle_cast(Request :: term(), State :: term()) ->
	{noreply, NewState :: term()} |
	{noreply, NewState :: term(), Timeout :: timeout()} |
	{noreply, NewState :: term(), hibernate} |
	{stop, Reason :: term(), NewState :: term()}.
handle_cast(_Request, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%% @end
%%--------------------------------------------------------------------
-spec handle_info(Info :: timeout() | term(), State :: term()) ->
	{noreply, NewState :: term()} |
	{noreply, NewState :: term(), Timeout :: timeout()} |
	{noreply, NewState :: term(), hibernate} |
	{stop, Reason :: normal | term(), NewState :: term()}.
handle_info(_Info, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%% @end
%%--------------------------------------------------------------------
-spec terminate(Reason :: normal | shutdown | {shutdown, term()} | term(),
		State :: term()) -> any().
terminate(_Reason, _State) ->
  ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%% @end
%%--------------------------------------------------------------------
-spec code_change(OldVsn :: term() | {down, term()},
		  State :: term(),
		  Extra :: term()) -> {ok, NewState :: term()} |
	{error, Reason :: term()}.
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called for changing the form and appearance
%% of gen_server status when it is returned from sys:get_status/1,2
%% or when it appears in termination error logs.
%% @end
%%--------------------------------------------------------------------
-spec format_status(Opt :: normal | terminate,
		    Status :: list()) -> Status :: term().
format_status(_Opt, Status) ->
  Status.

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec nomina_equipo(map()) -> list().
nomina_equipo(#{<<"jugadores">> := Jugadores}) ->
  {GolesEquipo, MetaGolesEquipo} = acumular_goles_equipo(Jugadores, 0, 0),
  AlcanceEquipo = GolesEquipo / MetaGolesEquipo,
  calcular_sueldo(Jugadores, AlcanceEquipo, []).

-spec calcular_sueldo(list(), number(), list()) -> list().
calcular_sueldo([], _AlcanceEquipo, JugadoresSueldo) ->
  JugadoresSueldo;
calcular_sueldo([#{<<"goles">> := Goles, <<"nivel">> := Nivel, <<"bono">> := Bono, <<"sueldo">> := Sueldo} = Jugador | Jugadores], AlcanceEquipo, JugadoresSueldo) ->
  MetaGolesIndividual = meta_goles_individual(Nivel),
  AlcanceIndividual = Goles / MetaGolesIndividual,
  AlcanceTotal = ((AlcanceEquipo + AlcanceIndividual) / 2),
  BonoVariable = Bono * AlcanceTotal,
  SueldoCompleto = trunc(Sueldo + BonoVariable),
  calcular_sueldo(Jugadores, AlcanceEquipo, [maps:update(<<"sueldo_completo">>, SueldoCompleto, Jugador) | JugadoresSueldo]).

-spec acumular_goles_equipo(list(), number(), number()) -> {number(), number()}.
acumular_goles_equipo([], GolesEquipo, MetaGolesEquipo) ->
  {GolesEquipo, MetaGolesEquipo};
acumular_goles_equipo([#{<<"goles">> := Goles, <<"nivel">> := Nivel} | Jugadores], GolesEquipo, MetaGolesEquipo) ->
  MetaGolesIndividual = meta_goles_individual(Nivel),
  acumular_goles_equipo(Jugadores, GolesEquipo + Goles, MetaGolesEquipo + MetaGolesIndividual).

-spec meta_goles_individual(binary()) -> number().
meta_goles_individual(<<"A">>) -> 5;
meta_goles_individual(<<"B">>) -> 10;
meta_goles_individual(<<"C">>) -> 15;
meta_goles_individual(<<"Cuauh">>) -> 20;
meta_goles_individual(_) -> 0.

