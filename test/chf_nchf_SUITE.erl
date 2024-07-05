%%% chf_nchf_SUITE.erl
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
%%% Test suite for the Nchf interface of the {@link //chf. chf} application.
%%%
-module(chf_nchf_SUITE).
-copyright('Copyright (c) 2024 SigScale Global Inc.').
-author('Vance Shipley <vances@sigscale.org>').

%% common_test required callbacks
-export([suite/0, sequences/0, all/0]).
-export([init_per_suite/1, end_per_suite/1]).
-export([init_per_testcase/2, end_per_testcase/2]).

%% export test cases
-export([not_found/0, not_found/1]).

-include_lib("common_test/include/ct.hrl").

%%---------------------------------------------------------------------
%%  Test server callback functions
%%---------------------------------------------------------------------

-spec suite() -> DefaultData :: [tuple()].
%% Require variables and set default values for the suite.
%%
suite() ->
   [{userdata, [{doc, "Test suite for Nchf Interface in CHF"}]},
	{require, nchf},
	{default_config, nchf,
			[{host, {127,0,0,1}},
			{path, "/nchf-convergedcharging/v3/chargingdata/"}]},
   {timetrap, {minutes, 1}}].

-spec init_per_suite(Config :: [tuple()]) -> Config :: [tuple()].
%% Initiation before the whole suite.
%%
init_per_suite(Config) ->
	ok = chf_test_lib:start(),
	Name = ?MODULE,
	Host = ct:get_config({nchf, host}),
	Port = rand:uniform(64511) + 1024,
	Path = ct:get_config({nchf, path}),
	TransportOpts = [{ip, Host}, {port, Port}],
	{ok, _Listener} = chf:start(Name, TransportOpts),
	[{host, Host}, {port, Port}, {path, Path} | Config].

-spec end_per_suite(Config :: [tuple()]) -> any().
%% Cleanup after the whole suite.
%%
end_per_suite(_Config) ->
	ok = chf_test_lib:stop().

-spec init_per_testcase(TestCase :: atom(), Config :: [tuple()]) -> Config :: [tuple()].
%% Initiation before each test case.
%%
init_per_testcase(TestCase, Config)
		when TestCase == not_found  ->
	Host = proplists:get_value(host, Config),
	Port = proplists:get_value(port, Config),
	Opts = #{transport => tcp, protocols => [http2]},
	{ok, ConnPid} = gun:open(Host, Port, Opts),
	[{conn_pid, ConnPid} | Config];
init_per_testcase(_TestCase, Config) ->
   Config.

-spec end_per_testcase(TestCase :: atom(), Config :: [tuple()]) -> any().
%% Cleanup after each test case.
%%
end_per_testcase(TestCase, Config)
		when TestCase == not_found  ->
	ConnPid = proplists:get_value(conn_pid, Config),
	ok = gun:shutdown(ConnPid),
	proplists:delete(conn_pid, Config);
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
	[not_found].

%%---------------------------------------------------------------------
%%  Test cases
%%---------------------------------------------------------------------

not_found() ->
	[{userdata, [{doc, "POST to nonexistent ChargingDataRef"}]}].

not_found(Config) ->
	ConnPid = proplists:get_value(conn_pid, Config),
	BasePath = proplists:get_value(path, Config),
	ChargingDataRef = chf_test_lib:rand_dn(),
	Path = [BasePath, $/, ChargingDataRef, $/, "update"],
	ContentType =  {"Content-Type", "application/json"},
	Accept = {"Accept", "application/json"},
	Headers1 =  [ContentType, Accept],
	ChargingDataRequest = update_request(),
	Body = zj:encode(ChargingDataRequest),
	StreamRef = gun:post(ConnPid, Path, Headers1, Body),
	{response, fin, 404, _Headers2} = gun:await(ConnPid, StreamRef).

%%---------------------------------------------------------------------
%%  Internal functions
%%---------------------------------------------------------------------

update_request() ->
	NFIdentification = #{"nodeFunctionality" => "SMF"},
	#{"nfConsumerIdentification" => NFIdentification,
			"invocationTimeStamp" => chf_rest:now(),
			"invocationSequenceNumber" => 1}.

