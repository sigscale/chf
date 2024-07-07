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
	case supervisor:start_link(chf_sup, []) of
		{ok, TopSup} ->
			{ok, Nchf} = application:get_env(nchf),
			start1(TopSup, Nchf);
		{error, Reason} ->
			{error, Reason}
	end.
%% @hidden
start1(TopSup, [H | T]) ->
	case supervisor:start_child(chf_nchf_sup, [tuple_to_list(H)]) of
		{ok, _Child} ->
			start1(TopSup, T);
		{error, Reason} ->
			{error, Reason}
	end;
start1(TopSup, []) ->
	{ok, Nrf} = application:get_env(nrf),
	start2(TopSup, Nrf).
%% @hidden
start2(TopSup, [H | T]) ->
	case supervisor:start_child(chf_nrf_connection_sup, [tuple_to_list(H), []]) of
		{ok, _Child} ->
			start2(TopSup, T);
		{error, Reason} ->
			{error, Reason}
	end;
start2(TopSup, []) ->
	{ok, TopSup}.

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
prep_stop(State) ->
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

