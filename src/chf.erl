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
-export([start_nchf/2, start_nchf/3, start_nchf/4, stop_nchf/1]).
-export([start_nrf/2, start_nrf/3, stop_nrf/1]).

%%----------------------------------------------------------------------
%%  The chf public API
%%----------------------------------------------------------------------

-spec start_nchf(Name, TransportOpts) -> Result
	when
		Name :: ranch:ref(),
		TransportOpts :: ranch_tcp:opts(),
		Result :: {ok, NchfListenerSup} | {error, Reason},
		NchfListenerSup :: pid(),
		Reason :: supervisor:startchild_err().
%% @doc Start an Nchf interface endpoint.
%% @equiv start_nchf(Name, TransportOpts, #{})
start_nchf(Name, TransportOpts) ->
	start_nchf(Name, TransportOpts, #{}).

-spec start_nchf(Name, TransportOpts, ProtocolOpts) -> Result
	when
		Name :: ranch:ref(),
		TransportOpts :: ranch_tcp:opts(),
		ProtocolOpts :: cowboy:opts(),
		Result :: {ok, NchfListenerSup} | {error, Reason},
		NchfListenerSup :: pid(),
		Reason :: supervisor:startchild_err().
%% @doc Start an Nchf interface endpoint.
%% @equiv start_nchf(Name, tcp, TransportOpts, #{})
start_nchf(Name, TransportOpts, ProtocolOpts) ->
	start_nchf(Name, tcp, TransportOpts, ProtocolOpts).

-spec start_nchf(Name, Transport, TransportOpts, ProtocolOpts) -> Result
	when
		Name :: ranch:ref(),
		Transport :: tcp | tls,
		TransportOpts :: ranch_tcp:opts() | ranch_ssl:opts(),
		ProtocolOpts :: cowboy:opts(),
		Result :: {ok, NchfListenerSup} | {error, Reason},
		NchfListenerSup :: pid(),
		Reason :: supervisor:startchild_err().
%% @doc Start an Nchf interface endpoint.
start_nchf(Name, Transport, TransportOpts, ProtocolOpts)
		when ((Transport == tcp) orelse (Transport == tls)),
		is_list(TransportOpts), is_map(ProtocolOpts) ->
	supervisor:start_child(chf_nchf_sup,
			[[Name, Transport, TransportOpts, ProtocolOpts]]).

-spec stop_nchf(NchfConnectionSup) -> Result
	when
		NchfConnectionSup :: pid(),
		Result :: ok | {error, Reason},
		Reason :: not_found | simple_one_for_one.
%% @doc Stop an Nchf interface endpoint.
stop_nchf(NchfConnectionSup) ->
	supervisor:terminate_child(chf_nchf_sup, NchfConnectionSup).

-spec start_nrf(Host, Port) -> Result
	when
		Host :: inet:hostname() | inet:ip_address(),
		Port :: inet:port_number(),
		Result :: {ok, NrfConnectionSup} | {error, Reason},
		NrfConnectionSup :: pid(),
		Reason :: supervisor:startchild_err().
%% @doc Start an Nrf interface endpoint.
%% @equiv start_nrf(Host, Port, #{})
start_nrf(Host, Port) ->
	start_nrf(Host, Port, #{}).

-spec start_nrf(Host, Port, Opts) -> Result
	when
		Host :: inet:hostname() | inet:ip_address(),
		Port :: inet:port_number(),
		Opts :: gun:opts(),
		Result :: {ok, NrfConnectionSup} | {error, Reason},
		NrfConnectionSup :: pid(),
		Reason :: supervisor:startchild_err().
%% @doc Start an Nrf interface endpoint.
start_nrf(Host, Port, Opts) ->
	supervisor:start_child(chf_nrf_connection_sup, [[Host, Port, Opts], []]).

-spec stop_nrf(NrfConnectionSup) -> Result
	when
		NrfConnectionSup :: pid(),
		Result :: ok | {error, Reason},
		Reason :: not_found | simple_one_for_one.
%% @doc Stop an Nrf interface endpoint.
stop_nrf(NrfConnectionSup) ->
	supervisor:terminate_child(chf_nrf_connection_sup, NrfConnectionSup).

%%----------------------------------------------------------------------
%%  internal functions
%%----------------------------------------------------------------------

