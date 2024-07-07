%%% chf_nrf_sup.erl
%%% vim: ts=3
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @copyright 2024 SigScale Global Inc.
%%% @author Vance Shipley <vances@sigscale.org> [http://www.sigscale.org]
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
%%% @docfile "{@docsrc supervision.edoc}"
%%%
-module(chf_nrf_sup).
-copyright('Copyright (c) 2024 SigScale Global Inc.').
-author('Vance Shipley <vances@sigscale.org>').

-behaviour(supervisor).

%% export the callback needed for supervisor behaviour
-export([init/1]).

-type registered_name() :: {local, Name :: atom()}
		| {global, Name :: atom()}
		| {via, ViaModule :: atom(), Name :: any()}.

%%----------------------------------------------------------------------
%%  The supervisor callback
%%----------------------------------------------------------------------

-spec init(Args) -> Result
	when
		Args :: [],
		Result :: {ok, {SupFlags, [ChildSpec]}} | ignore,
		SupFlags :: supervisor:sup_flags(),
		ChildSpec :: supervisor:child_spec().
%% @doc Initialize the {@module} supervisor.
%% @see //stdlib/supervisor:init/1
%% @private
%%
init(_Args) ->
	ChildSpecs = [group(chf), supervisor({local, chf_nrf_connection_sup},
			chf_nrf_connection_sup, [])],
	SupFlags = #{strategy => one_for_all},
	{ok, {SupFlags, ChildSpecs}}.

%%----------------------------------------------------------------------
%%  internal functions
%%----------------------------------------------------------------------

-spec supervisor(RegName, StartMod, Args) -> Result
	when
		RegName :: registered_name(),
		StartMod :: atom(),
		Args :: [term()],
		Result :: supervisor:child_spec().
%% @doc Build a supervisor child specification for a
%%    {@link //stdlib/supervisor. supervisor} behaviour.
%% @private
%%
supervisor(RegName, StartMod, Args) ->
	StartArgs = [RegName, StartMod, Args],
	StartFunc = {supervisor, start_link, StartArgs},
	#{id => StartMod, start => StartFunc,
			type => supervisor, modules => [StartMod]}.

-spec group(Scope) -> Result
	when
		Scope :: atom(),
		Result :: supervisor:child_spec().
%% @doc Build a supervisor child specification for a
%% 	{@link pg. pg} scope.
%% @private
%%
group(Scope) ->
	StartArgs = [Scope],
	StartFunc = {pg, start_link, StartArgs},
	#{id => pg, start => StartFunc}.

