%%% chf_app.erl
%%% vim: ts=3
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @copyright 2024 SigScale Global Inc.
%%% @end
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%     http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc This {@link //stdlib/application. application} behaviour callback
%%%   module starts and stops the {@link //chf. chf} application.
%%%
-module(chf_app).
-copyright('Copyright (c) 2024 SigScale Global Inc.').
-author('Vance Shipley <vances@sigscale.org>').

-behaviour(application).

%% callbacks needed for application behaviour
-export([start/2, stop/1, config_change/3]).
%% optional callbacks for application behaviour
-export([prep_stop/1, start_phase/3]).

-include_lib("kernel/include/logger.hrl").

-type state() :: [{NchName :: ranch:ref(), Listener :: pid()}].

%%----------------------------------------------------------------------
%%  The chf_app aplication callbacks
%%----------------------------------------------------------------------

-type start_type() :: normal | {takeover, node()} | {failover, node()}.
-spec start(StartType, StartArgs) -> Result
	when
		StartType :: start_type(),
		StartArgs :: term(),
		Result :: {ok, pid()} | {ok, pid(), State} | {error, Reason},
		State :: state(),
		Reason :: term().
%% @doc Starts the application processes.
start(normal = _StartType, _Args) ->
	{ok, Nchfs} = application:get_env(nchf),
	start1(Nchfs, []).
%% @hidden
start1([{Name, Transport, TransportOpts} | T] = _Nchfs, State) ->
	start1([{Name, Transport, TransportOpts, #{}} | T], State);
start1([{Name, Transport, TransportOpts, ProtocolOpts} | T] = _Nchfs, State) ->
	case chf:start(Name, Transport, TransportOpts, ProtocolOpts) of
		{ok, Listener} ->
			start1(T, [{Name, Listener} | State]);
		{error, Reason} ->
			{error, Reason}
	end;
start1([], State) ->
	case supervisor:start_link(chf_sup, []) of
		{ok, Sup} ->
			{ok, Sup, State};
		{error, Reason} ->
			{error, Reason}
	end.

-spec start_phase(Phase, StartType, PhaseArgs) -> Result
	when
		Phase :: atom(),
		StartType :: start_type(),
		PhaseArgs :: term(),
		Result :: ok | {error, Reason},
		Reason :: term().
%% @doc Called for each start phase in the application and included
%%   applications.
%% @see //kernel/app
%%
start_phase(_Phase, _StartType, _PhaseArgs) ->
	ok.

-spec prep_stop(State) -> State
	when
		State :: state().
%% @doc Called when the application is about to be shut down,
%%   before any processes are terminated.
%% @see //kernel/application:stop/1
%%
prep_stop([{Name, Listener} | T] = _State) ->
	case chf:stop(Name) of
		ok ->
			prep_stop(T);
		{error, Reason} ->
			?LOG_WARNING(#{module => ?MODULE, function => prep_stop,
					nchf => Name, listener => Listener, reason => Reason}),
			prep_stop(T)
	end;
prep_stop([] = State) ->
	State.

-spec stop(State) -> any()
	when
		State :: state().
%% @doc Called after the application has stopped to clean up.
%%
stop(_State) ->
	ok.

-spec config_change(Changed, New, Removed) -> ok
	when
		Changed:: [{Par, Val}],
		New :: [{Par, Val}],
		Removed :: [Par],
		Par :: atom(),
		Val :: atom().
%% @doc Called after a code  replacement, if there are any
%%   changes to the configuration  parameters.
%%
config_change(_Changed, _New, _Removed) ->
	ok.

%%----------------------------------------------------------------------
%%  Internal functions
%%----------------------------------------------------------------------

