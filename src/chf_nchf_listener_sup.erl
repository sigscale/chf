%%% chf_nchf_listener_sup.erl
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
%%% @doc This {@link //stdlib/supervisor_bridge. supervisor_bridge}
%%% 	behaviour callback module implements a supervisor in the
%%% 	{@link //chf. chf} application.
%%%
-module(chf_nchf_listener_sup).
-copyright('Copyright (c) 2024 SigScale Global Inc.').
-author('Vance Shipley <vances@sigscale.org>').

-behaviour(supervisor_bridge).

%% export the callback needed for supervisor_bridge behaviour
-export([init/1, terminate/2]).

-include_lib("kernel/include/logger.hrl").

-type state() :: #{name := Name :: ranch:ref(), pid := Listener :: pid()}.

%%----------------------------------------------------------------------
%%  The supervisor_bridge callback
%%----------------------------------------------------------------------

-spec init(Args) -> Result
	when
		Args :: list(),
		Result :: {ok, Pid, State} | ignore | {error, Reason},
		Pid :: pid(),
		State :: term(),
		Reason :: term().
%% @see //stdlib/supervisor_bridge:init/1
%% @private
%%
init([Name, Transport, TransportOpts] = _Args) ->
	case start_nchf(Name, Transport, TransportOpts) of
		{ok, Listener} ->
			{ok, Listener, #{name => Name, pid => Listener}};
		{error, Reason} ->
			{error, Reason}
	end;
init([Name, Transport, TransportOpts, ProtocolOpts] = _Args) ->
	case start_nchf(Name, Transport, TransportOpts, ProtocolOpts) of
		{ok, Listener} ->
			{ok, Listener, #{name => Name, pid => Listener}};
		{error, Reason} ->
			{error, Reason}
	end.

-spec terminate(Reason, State) -> any()
	when
		Reason :: normal | shutdown | {shutdown, term()} | term(),
		State :: state().
%% @see //stdlib/gen_server_bridge:terminate/2
%% @private
terminate(_Reason, #{name := Name, pid := Listener} = _State) ->
	case cowboy:stop_listener(Name) of
		ok ->
			ok;
		{error, Reason} ->
			?LOG_WARNING(#{module => ?MODULE, function => terminate,
					nchf => Name, listener => Listener, reason => Reason})
	end.

%%----------------------------------------------------------------------
%%  internal functions
%%----------------------------------------------------------------------

-spec start_nchf(Name, Transport, TransportOpts) -> Result
	when
		Name :: ranch:ref(),
		Transport :: tcp | tls,
		TransportOpts :: ranch_tcp:opts() | ranch_ssl:opts(),
		Result :: {ok, Listener} | {error, Reason},
		Listener :: pid(),
		Reason :: eaddrinuse | term().
%% @doc Start an Nchf interface endpoint.
%% @equiv start_nchf(Name, tcp, TransportOpts, #{})
%% @hidden
start_nchf(Name, Transport, TransportOpts) ->
	start_nchf(Name, Transport, TransportOpts, #{}).

-spec start_nchf(Name, Transport, TransportOpts, ProtocolOpts) -> Result
	when
		Name :: ranch:ref(),
		Transport :: tcp | tls,
		TransportOpts :: ranch_tcp:opts() | ranch_ssl:opts(),
		ProtocolOpts :: cowboy:opts(),
		Result :: {ok, Listener} | {error, Reason},
		Listener :: pid(),
		Reason :: eaddrinuse | term().
%% @doc Start an Nchf interface endpoint.
%% @hidden
start_nchf(Name, Transport, TransportOpts, ProtocolOpts)
		when ((Transport == tcp) orelse (Transport == tls)),
		is_list(TransportOpts), is_map(ProtocolOpts) ->
	ApiName = <<"nchf-convergedcharging">>,
	ResourceName = <<"chargingdata">>,
	CollectionPath = [$/, ApiName, $/, <<":version">>, $/, ResourceName],
	ItemPath = [CollectionPath, $/, <<":ChargingDataRef">>],
	PathMatch1 = CollectionPath,
	PathMatch2 = [ItemPath, $/, <<"update">>],
	PathMatch3 = [ItemPath, $/, <<"release">>],
	Paths = [PathMatch1, PathMatch2, PathMatch3],
	Handler = chf_nchf_handler,
	InitialState = #{},
	PathList = [{P, Handler, InitialState} || P <- Paths],
	HostMatch = '_',
	Routes = [{HostMatch, PathList}],
	Dispatch = cowboy_router:compile(Routes),
	start1_nchf(Name, Transport, TransportOpts,
			ProtocolOpts#{env => #{dispatch => Dispatch}}).
%% @hidden
start1_nchf(Name, tcp, TransportOpts, ProtocolOpts) ->
	cowboy:start_clear(Name, TransportOpts, ProtocolOpts);
start1_nchf(Name, tls, TransportOpts, ProtocolOpts) ->
	cowboy:start_tls(Name, TransportOpts, ProtocolOpts).

