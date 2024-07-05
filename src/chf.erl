%%% chf.erl
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
%%% @doc This library module implements the public API for the
%%%   {@link //chf. chf} application.
%%%
-module(chf).
-copyright('Copyright (c) 2024 SigScale Global Inc.').
-author('Vance Shipley <vances@sigscale.org>').

%% export the chf public API
-export([start/2, start/3, start/4, stop/1]).

-define(CHARGINGDATA, "/nchf-convergedcharging/v3/chargingdata/").

%%----------------------------------------------------------------------
%%  The chf public API
%%----------------------------------------------------------------------

-spec start(Name, TransportOpts) -> Result
	when
		Name :: ranch:ref(),
		TransportOpts :: ranch_tcp:opts(),
		Result :: {ok, Listener} | {error, Reason},
		Listener :: pid(),
		Reason :: eaddrinuse | term().
%% @doc Start an Nchf interface endpoint.
%% @equiv start(Name, TransportOpts, #{})
start(Name, TransportOpts) ->
	start(Name, TransportOpts, #{}).

-spec start(Name, TransportOpts, ProtocolOpts) -> Result
	when
		Name :: ranch:ref(),
		TransportOpts :: ranch_tcp:opts(),
		ProtocolOpts :: cowboy:opts(),
		Result :: {ok, Listener} | {error, Reason},
		Listener :: pid(),
		Reason :: eaddrinuse | term().
%% @doc Start an Nchf interface endpoint.
%% @equiv start(Name, tcp, TransportOpts, #{})
start(Name, TransportOpts, ProtocolOpts) ->
	start(Name, tcp, TransportOpts, ProtocolOpts).

-spec start(Name, Transport, TransportOpts, ProtocolOpts) -> Result
	when
		Name :: ranch:ref(),
		Transport :: tcp | tls,
		TransportOpts :: ranch_tcp:opts() | ranch_ssl:opts(),
		ProtocolOpts :: cowboy:opts(),
		Result :: {ok, Listener} | {error, Reason},
		Listener :: pid(),
		Reason :: eaddrinuse | term().
%% @doc Start an Nchf interface endpoint.
start(Name, Transport, TransportOpts, ProtocolOpts)
		when ((Transport == tcp) orelse (Transport == tls)),
		is_list(TransportOpts), is_map(ProtocolOpts) ->
	ChargingData = [{?CHARGINGDATA, chf_nchf_handler, []}],
	Dispatch = cowboy_router:compile([{'_', ChargingData}]),
	start1(Name, Transport, TransportOpts,
			ProtocolOpts#{env => #{dispatch => Dispatch}}).
%% @hidden
start1(Name, tcp, TransportOpts, ProtocolOpts) ->
	cowboy:start_clear(Name, TransportOpts, ProtocolOpts);
start1(Name, tls, TransportOpts, ProtocolOpts) ->
	cowboy:start_tls(Name, TransportOpts, ProtocolOpts).

-spec stop(Name) -> Result
	when
		Name :: ranch:ref(),
		Result :: ok | {error, Reason},
		Reason :: not_found.
%% @doc Stop an Nchf interface endpoint.
stop(Name) ->
	cowboy:stop_listener(Name).

%%----------------------------------------------------------------------
%%  internal functions
%%----------------------------------------------------------------------

