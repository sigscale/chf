%%% chf_api_SUITE.erl
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
%%% Test suite for the public API of the {@link //chf. chf} application.
%%%
-module(chf_api_SUITE).
-copyright('Copyright (c) 2024 SigScale Global Inc.').
-author('Vance Shipley <vances@sigscale.org>').

%% common_test required callbacks
-export([suite/0, sequences/0, all/0]).
-export([init_per_suite/1, end_per_suite/1]).
-export([init_per_testcase/2, end_per_testcase/2]).

%% export test cases
-export([start_nchf/0, start_nchf/1,
		stop_nchf/0, stop_nchf/1,
		start_nrf/0, start_nrf/1,
		stop_nrf/0, stop_nrf/1]).

-include_lib("common_test/include/ct.hrl").

%%---------------------------------------------------------------------
%%  Test server callback functions
%%---------------------------------------------------------------------

-spec suite() -> DefaultData :: [tuple()].
%% Require variables and set default values for the suite.
%%
suite() ->
	[{timetrap, {minutes, 1}}].

-spec init_per_suite(Config :: [tuple()]) -> Config :: [tuple()].
%% Initiation before the whole suite.
%%
init_per_suite(Config) ->
	ok = chf_test_lib:start(),
	Config.

-spec end_per_suite(Config :: [tuple()]) -> any().
%% Cleanup after the whole suite.
%%
end_per_suite(_Config) ->
	ok = chf_test_lib:stop().

-spec init_per_testcase(TestCase :: atom(), Config :: [tuple()]) -> Config :: [tuple()].
%% Initiation before each test case.
%%
init_per_testcase(_TestCase, Config) ->
   Config.

-spec end_per_testcase(TestCase :: atom(), Config :: [tuple()]) -> any().
%% Cleanup after each test case.
%%
end_per_testcase(_TestCase, _Config) ->
	ok.

-spec sequences() -> Sequences :: [{SeqName :: atom(), Testcases :: [atom()]}].
%% Group test cases into a test sequence.
%%
sequences() ->
	[].

-spec all() -> TestCases :: [Case :: atom()].
%% Returns a list of all test cases in this test suite.
%%
all() ->
	[start_nchf, stop_nchf, start_nrf, stop_nrf].

%%---------------------------------------------------------------------
%%  Test cases
%%---------------------------------------------------------------------

start_nchf() ->
	[{userdata, [{doc, "Start an Nchf interface endpoint"}]}].

start_nchf(_Config) ->
	Name = chf_test_lib:rand_dn(),
	Port = rand:uniform(64511) + 1024,
	TransportOpts = [{port, Port}],
	{ok, NchfListenerSup} = chf:start_nchf(Name, TransportOpts),
	true = is_process_alive(NchfListenerSup).

stop_nchf() ->
	[{userdata, [{doc, "Stop an Nchf interface endpoint"}]}].

stop_nchf(_Config) ->
	Name = chf_test_lib:rand_dn(),
	Port = rand:uniform(64511) + 1024,
	TransportOpts = [{port, Port}],
	{ok, NchfListenerSup} = chf:start_nchf(Name, TransportOpts),
	ok = chf:stop_nchf(NchfListenerSup),
	false = is_process_alive(NchfListenerSup).

start_nrf() ->
	[{userdata, [{doc, "Start an Nrf interface endpoint"}]}].

start_nrf(_Config) ->
	Host = {127,0,0,1},
	Port = rand:uniform(64511) + 1024,
	{ok, NrfConnectionSup} = chf:start_nrf(Host, Port),
	true = is_process_alive(NrfConnectionSup).

stop_nrf() ->
	[{userdata, [{doc, "Stop an Nrf interface endpoint"}]}].

stop_nrf(_Config) ->
	Host = {127,0,0,1},
	Port = rand:uniform(64511) + 1024,
	{ok, NrfConnectionSup} = chf:start_nrf(Host, Port),
	ok = chf:stop_nrf(NrfConnectionSup),
	false = is_process_alive(NrfConnectionSup).

%%---------------------------------------------------------------------
%%  Internal functions
%%---------------------------------------------------------------------

