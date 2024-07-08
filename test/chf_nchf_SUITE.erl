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
-export([invalid_path/0, invalid_path/1,
		unknown_dataref/0, unknown_dataref/1,
		create_scur/0, create_scur/1,
		update_scur/0, update_scur/1,
		release_scur/0, release_scur/1]).

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
			{path, <<"/nchf-convergedcharging/v3/chargingdata">>}]},
	{require, nrf},
	{default_config, nrf,
			[{host, {127,0,0,1}},
			{path, <<"/nrf-rating/v1/ratingdata">>}]},
   {timetrap, {minutes, 1}}].

-spec init_per_suite(Config :: [tuple()]) -> Config :: [tuple()].
%% Initiation before the whole suite.
%%
init_per_suite(Config) ->
	ok = chf_ct_lib:load(chf),
	Name = ?MODULE,
	Host = ct:get_config({nchf, host}),
	Port = rand:uniform(64511) + 1024,
	Path = ct:get_config({nchf, path}),
	TransportOpts = [{ip, Host}, {port, Port}],
	ok = application:set_env(chf, nchf, [{Name, tcp, TransportOpts}]),
	Config1 = [{nchf_host, Host}, {nchf_port, Port},
			{nchf_path, Path}, {nchf_name, Name} | Config],
	init_per_suite1(Config1).
init_per_suite1(Config) ->
	case gen_server:start(chf_rf_server, [], []) of
		{ok, Pid} ->
			init_per_suite2([{ct_rf, Pid} | Config]);
		{error, Reason} ->
			ct:fail(Reason)
	end.
init_per_suite2(Config) ->
	ok = chf_ct_lib:start(cowboy),
	Rf = proplists:get_value(ct_rf, Config),
	Name = chf_ct_lib:rand_dn(),
	Host = ct:get_config({nrf, host}),
	Port = rand:uniform(64511) + 1024,
	Path = ct:get_config({nrf, path}),
	PathMatch1 = [Path],
	PathMatch2 = [Path, <<"/:RatingDataRef/update">>],
	PathMatch3 = [Path, <<"/:RatingDataRef/release">>],
	Paths = [PathMatch1, PathMatch2, PathMatch3],
	State = #{rf => Rf},
	PathList = [{P, chf_nrf_handler, State} || P <- Paths],
	HostMatch = '_',
	Routes = [{HostMatch, PathList}],
	Dispatch = cowboy_router:compile(Routes),
	TransportOpts = [{ip, Host}, {port, Port}],
	ProtocolOpts = #{env => #{dispatch => Dispatch}},
	case cowboy:start_clear(Name, TransportOpts, ProtocolOpts) of
		{ok, Listener} ->
			Config1 = [{nrf_host, Host}, {nrf_port, Port},
					{nrf_path, Path}, {nrf_name, Name},
					{nrf_pid, Listener} | Config],
			init_per_suite3(Config1);
		{error, Reason} ->
			{error, Reason}
	end.
init_per_suite3(Config) ->
	Host = proplists:get_value(nrf_host, Config),
	Port = proplists:get_value(nrf_port, Config),
	Opts = #{transport => tcp, protocols => [http2]},
	ok = application:set_env(chf, nrf, [{Host, Port, Opts}]),
	ok = chf_ct_lib:start(),
	Config.

-spec end_per_suite(Config :: [tuple()]) -> any().
%% Cleanup after the whole suite.
%%
end_per_suite(_Config) ->
	ok = chf_ct_lib:stop().

-spec init_per_testcase(TestCase :: atom(), Config :: [tuple()]) -> Config :: [tuple()].
%% Initiation before each test case.
%%
init_per_testcase(_TestCase, Config) ->
	ets:new(rf_dataref, [named_table, public]),
	Host = proplists:get_value(nchf_host, Config),
	Port = proplists:get_value(nchf_port, Config),
	Opts = #{transport => tcp, protocols => [http2]},
	{ok, ConnPid} = gun:open(Host, Port, Opts),
	[{conn_pid, ConnPid} | Config].

-spec end_per_testcase(TestCase :: atom(), Config :: [tuple()]) -> any().
%% Cleanup after each test case.
%%
end_per_testcase(_TestCase, Config) ->
	ConnPid = proplists:get_value(conn_pid, Config),
	ok = gun:shutdown(ConnPid),
	proplists:delete(conn_pid, Config).

-spec sequences() -> Sequences :: [{SeqName :: atom(), Testcases :: [atom()]}].
%% Group test cases into a test sequence.
%%
sequences() ->
	[].

-spec all() -> TestCases :: [Case :: atom()].
%% Returns a list of all test cases in this test suite.
%%
all() ->
	[invalid_path, unknown_dataref, create_scur, update_scur, release_scur].

%%---------------------------------------------------------------------
%%  Test cases
%%---------------------------------------------------------------------

invalid_path() ->
	[{userdata, [{doc, "Invalid resource path"}]}].

invalid_path(Config) ->
	IMSI = "001001" ++ chf_ct_lib:rand_dn(9),
	SI = rand:uniform(20),
	RG = rand:uniform(99) + 100,
	Volume = rand:uniform(1000000),
	ConnPid = proplists:get_value(conn_pid, Config),
	BasePath = proplists:get_value(nchf_path, Config),
	ChargingDataRef = chf_rest:id(),
	Path = [BasePath, $/, ChargingDataRef, $/, <<"bogus">>],
	ContentType =  {<<"content-type">>, <<"application/json">>},
	Accept = {<<"accept">>, <<"application/json">>},
	RequestHeaders =  [ContentType, Accept],
	ChargingDataRequest = update_request(IMSI, SI, RG, Volume),
	RequestBody = zj:encode(ChargingDataRequest),
	StreamRef = gun:post(ConnPid, Path, RequestHeaders, RequestBody),
	{response, fin, 404, _ResponseHeaders} = gun:await(ConnPid, StreamRef).

unknown_dataref() ->
	[{userdata, [{doc, "Unknown ChargingDataRef"}]}].

unknown_dataref(Config) ->
	IMSI = "001001" ++ chf_ct_lib:rand_dn(9),
	SI = rand:uniform(20),
	RG = rand:uniform(99) + 100,
	Volume = rand:uniform(1000000),
	ConnPid = proplists:get_value(conn_pid, Config),
	BasePath = proplists:get_value(nchf_path, Config),
	ChargingDataRef = chf_rest:id(),
	Path = [BasePath, $/, ChargingDataRef, $/, <<"update">>],
	ContentType =  {<<"content-type">>, <<"application/json">>},
	Accept = {<<"accept">>, <<"application/json">>},
	RequestHeaders =  [ContentType, Accept],
	ChargingDataRequest = update_request(IMSI, SI, RG, Volume),
	RequestBody = zj:encode(ChargingDataRequest),
	StreamRef = gun:post(ConnPid, Path, RequestHeaders, RequestBody),
	{response, fin, 404, _ResponseHeaders} = gun:await(ConnPid, StreamRef).

create_scur() ->
	[{userdata, [{doc, "Nchf_ConvergedCharging_Create (SCUR)"}]}].

create_scur(Config) ->
	IMSI = "001001" ++ chf_ct_lib:rand_dn(9),
	RG = rand:uniform(99) + 100,
	UnitSize = 1000000,
	Balance = rand:uniform(UnitSize) + UnitSize * 10,
	Rf = proplists:get_value(ct_rf, Config),
	{ok, {Balance, 0}} = chf_rf_server:add_subscriber(Rf, IMSI, Balance),
	ConnPid = proplists:get_value(conn_pid, Config),
	Path = proplists:get_value(nchf_path, Config),
	ContentType =  {<<"content-type">>, <<"application/json">>},
	Accept = {<<"accept">>, <<"application/json">>},
	RequestHeaders = [ContentType, Accept],
	ChargingDataRequest = create_request(IMSI, RG),
	RequestBody = zj:encode(ChargingDataRequest),
	StreamRef = gun:post(ConnPid, Path, RequestHeaders, RequestBody),
	{response, nofin, 201, ResponseHeaders} = gun:await(ConnPid, StreamRef),
	Location = proplists:get_value(<<"location">>, ResponseHeaders),
	true = is_prefix(Path, Location),
	{ok, ResponseBody} = gun:await_body(ConnPid, StreamRef),
	{ok, #{"multipleUnitInformation" := [MUI]}} = zj:decode(ResponseBody),
	#{"resultCode" := "SUCCESS", "grantedUnit" := GUI} = MUI,
	#{"totalVolume" := UnitSize} = GUI.

update_scur() ->
	[{userdata, [{doc, "Nchf_ConvergedCharging_Update (SCUR)"}]}].

update_scur(Config) ->
	IMSI = "001001" ++ chf_ct_lib:rand_dn(9),
	SI = rand:uniform(20),
	RG = rand:uniform(99) + 100,
	UnitSize = 1000000,
	Balance = rand:uniform(UnitSize) + UnitSize * 10,
	Rf = proplists:get_value(ct_rf, Config),
	{ok, {Balance, 0}} = chf_rf_server:add_subscriber(Rf, IMSI, Balance),
	ConnPid = proplists:get_value(conn_pid, Config),
	Path1 = proplists:get_value(nchf_path, Config),
	ContentType1 =  {<<"content-type">>, <<"application/json">>},
	Accept = {<<"accept">>, <<"application/json">>},
	RequestHeaders = [ContentType1, Accept],
	ChargingDataRequest1 = create_request(IMSI, RG),
	RequestBody1 = zj:encode(ChargingDataRequest1),
	StreamRef1 = gun:post(ConnPid, Path1, RequestHeaders, RequestBody1),
	{response, nofin, 201, ResponseHeaders1} = gun:await(ConnPid, StreamRef1),
	Location = proplists:get_value(<<"location">>, ResponseHeaders1),
	Volume = rand:uniform(UnitSize) + UnitSize,
	Path2 = [Location, $/, <<"update">>],
	ChargingDataRequest2 = update_request(IMSI, SI, RG, Volume),
	RequestBody2 = zj:encode(ChargingDataRequest2),
	StreamRef2 = gun:post(ConnPid, Path2, RequestHeaders, RequestBody2),
	{response, nofin, 200, ResponseHeaders2} = gun:await(ConnPid, StreamRef2),
	ContentType2 = proplists:get_value(<<"content-type">>, ResponseHeaders2),
	<<"application/json">> = ContentType2,
	{ok, ResponseBody} = gun:await_body(ConnPid, StreamRef2),
	{ok, #{"multipleUnitInformation" := [MUI]}} = zj:decode(ResponseBody),
	#{"resultCode" := "SUCCESS", "grantedUnit" := GUI} = MUI,
	#{"totalVolume" := UnitSize} = GUI.

release_scur() ->
	[{userdata, [{doc, "Nchf_ConvergedCharging_Release (SCUR)"}]}].

release_scur(Config) ->
	IMSI = "001001" ++ chf_ct_lib:rand_dn(9),
	SI = rand:uniform(20),
	RG = rand:uniform(99) + 100,
	UnitSize = 1000000,
	Balance = rand:uniform(UnitSize) + UnitSize * 10,
	Rf = proplists:get_value(ct_rf, Config),
	{ok, {Balance, 0}} = chf_rf_server:add_subscriber(Rf, IMSI, Balance),
	ConnPid = proplists:get_value(conn_pid, Config),
	Path1 = proplists:get_value(nchf_path, Config),
	ContentType1 =  {<<"content-type">>, <<"application/json">>},
	Accept = {<<"accept">>, <<"application/json">>},
	RequestHeaders = [ContentType1, Accept],
	ChargingDataRequest1 = create_request(IMSI, RG),
	RequestBody1 = zj:encode(ChargingDataRequest1),
	StreamRef1 = gun:post(ConnPid, Path1, RequestHeaders, RequestBody1),
	{response, nofin, 201, ResponseHeaders1} = gun:await(ConnPid, StreamRef1),
	Location = proplists:get_value(<<"location">>, ResponseHeaders1),
	Volume1 = rand:uniform(UnitSize) + UnitSize,
	Path2 = [Location, $/, <<"update">>],
	ChargingDataRequest2 = update_request(IMSI, SI, RG, Volume1),
	RequestBody2 = zj:encode(ChargingDataRequest2),
	StreamRef2 = gun:post(ConnPid, Path2, RequestHeaders, RequestBody2),
	{response, nofin, 200, _ResponseHeaders2} = gun:await(ConnPid, StreamRef2),
	Path3 = [Location, $/, <<"release">>],
	Volume2 = rand:uniform(UnitSize) + UnitSize,
	ChargingDataRequest3 = release_request(IMSI, SI, RG, Volume2),
	RequestBody3 = zj:encode(ChargingDataRequest3),
	StreamRef3 = gun:post(ConnPid, Path3, RequestHeaders, RequestBody3),
	{response, nofin, 200, ResponseHeaders3} = gun:await(ConnPid, StreamRef3),
	ContentType2 = proplists:get_value(<<"content-type">>, ResponseHeaders3),
	<<"application/json">> = ContentType2,
	{ok, ResponseBody} = gun:await_body(ConnPid, StreamRef3),
	{ok, #{}} = zj:decode(ResponseBody).

%%---------------------------------------------------------------------
%%  Internal functions
%%---------------------------------------------------------------------

create_request(IMSI, RG) ->
	NFIdentification = #{"nodeFunctionality" => "SMF"},
	MultipleUnitUsage = #{"ratingGroup" => RG, "requestedUnit" => #{}},
	#{"invocationSequenceNumber" => 1,
			"invocationTimeStamp" => chf_rest:now(),
			"nfConsumerIdentification" => NFIdentification,
			"subscriberIdentifier" => "imsi-" ++ IMSI,
			"serviceSpecificationInfo" => "32291@3gpp.org",
			"multipleUnitUsage" => [MultipleUnitUsage]}.

update_request(IMSI, SI, RG, Volume) ->
	NFIdentification = #{"nodeFunctionality" => "SMF"},
	UsedUnitContainer = #{"localSequenceNumber" => 1,
			"serviceId" => SI,
			"totalVolume" => Volume},
	MultipleUnitUsage = #{"ratingGroup" => RG,
			"requestedUnit" => #{},
			"usedUnitContainer" => [UsedUnitContainer]},
	#{"invocationSequenceNumber" => 2,
			"invocationTimeStamp" => chf_rest:now(),
			"nfConsumerIdentification" => NFIdentification,
			"serviceSpecificationInfo" => "32291@3gpp.org",
			"subscriberIdentifier" => "imsi-" ++ IMSI,
			"multipleUnitUsage" => [MultipleUnitUsage]}.

release_request(IMSI, SI, RG, Volume) ->
	NFIdentification = #{"nodeFunctionality" => "SMF"},
	UsedUnitContainer = #{"localSequenceNumber" => 1,
			"serviceId" => SI,
			"totalVolume" => Volume},
	MultipleUnitUsage = #{"ratingGroup" => RG,
			"usedUnitContainer" => [UsedUnitContainer]},
	#{"invocationSequenceNumber" => 3,
			"invocationTimeStamp" => chf_rest:now(),
			"nfConsumerIdentification" => NFIdentification,
			"serviceSpecificationInfo" => "32291@3gpp.org",
			"subscriberIdentifier" => "imsi-" ++ IMSI,
			"multipleUnitUsage" => [MultipleUnitUsage]}.

is_prefix(Prefix, Path)
		when is_list(Prefix) ->
	is_prefix(iolist_to_binary(Prefix), Path);
is_prefix(Prefix, Path)
		when is_list(Path) ->
	is_prefix(Prefix, iolist_to_binary(Path));
is_prefix(Prefix, Path)
		when is_binary(Prefix), is_binary(Path),
		byte_size(Prefix) =< byte_size(Path) ->
	case binary:part(Path, 0, byte_size(Prefix)) of
		Prefix ->
			true;
		_ ->
			false
	end.

