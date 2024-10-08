%%% chf_nchf_handler.erl
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
%%% @doc This {@link //cowboy/cowboy_rest. cowboy_rest}
%%% 	callback module implements an Nchf interface endpoint
%%% 	handler in the the {@link //chf. chf} application.
%%%
%%% 	An instance of this module is spawned for each HTTP(2) request
%%% 	(`Stream'). This process handles an `Nchf_ConvergedCharging' request,
%%% 	transcodes `ChargingDataRequest' to `RatingDataRequest', sends an
%%% 	`Nrf_Rating' request, awaits and handles an `Nrf_Rating' response,
%%% 	transcodes `RatingDataResponse' to `ChargingDataResponse' and sends
%%% 	an `Nchf_ConvergedCharging' response.
%%%
%%% 	The `Nrf_Rating' request MUST include `serviceContextId' in each
%%% 	`ServiceRating' object, the value of which MAY be found in the
%%% 	`serviceSpecificationInformation' attribute of `ChargingDataRequest',
%%% 	however if it is not present a default value is chosen based on the
%%% 	value of `nfConsumerIdentification.nodeFunctionality' as described
%%% 	below:
%%% 	<table id="mt">
%%% 		<tr id="mt">
%%% 			<th id="mt">NodeFunctionality (Nchf)</th>
%%% 			<th id="mt">ServiceContextId (Nrf)</th>
%%% 		</tr>
%%% 		<tr id="mt">
%%% 			<td id="mt">SMF</td>
%%% 			<td id="mt">32255@3gpp.org</td>
%%% 		</tr>
%%% 		<tr id="mt">
%%% 			<td id="mt">V_SMF</td>
%%% 			<td id="mt">32255@3gpp.org</td>
%%% 		</tr>
%%% 		<tr id="mt">
%%% 			<td id="mt">AMF</td>
%%% 			<td id="mt">32256@3gpp.org</td>
%%% 		</tr>
%%% 		<tr id="mt">
%%% 			<td id="mt">SMSF</td>
%%% 			<td id="mt">32274@3gpp.org</td>
%%% 		</tr>
%%% 		<tr id="mt">
%%% 			<td id="mt">SGW</td>
%%% 			<td id="mt">32251@3gpp.org</td>
%%% 		</tr>
%%% 		<tr id="mt">
%%% 			<td id="mt">ePDG</td>
%%% 			<td id="mt">32251@3gpp.org</td>
%%% 		</tr>
%%% 		<tr id="mt">
%%% 			<td id="mt">SGSN</td>
%%% 			<td id="mt">32251@3gpp.org</td>
%%% 		</tr>
%%% 		<tr id="mt">
%%% 			<td id="mt">IMS_Node</td>
%%% 			<td id="mt">32260@3gpp.org</td>
%%% 		</tr>
%%% 	</table>
%%%
-module(chf_nchf_handler).
-copyright('Copyright (c) 2024 SigScale Global Inc.').
-author('Vance Shipley <vances@sigscale.org>').

-behavior(cowboy_rest).

% mandatory callbacks for cowboy_handler behavior
-export([init/2]).
% optional callbacks for cowboy_rest behavior
-export([allowed_methods/2, content_types_provided/2,
		content_types_accepted/2, resource_exists/2,
		allow_missing_post/2]).
% optional callbacks for cowboy_loop behavior
-export([info/3]).
% other callbacks
-export([from_json/2]).

-type state() :: #{operation => create | update | release,
		conn_pid => pid(),
		stream_ref => gun:stream_ref(),
		version => binary(),
		chargingdataref => binary(),
		chargingdata => map(),
		data => [binary()],
		status => 100..599}.

%%----------------------------------------------------------------------
%%  The chf_nchf_handler cowboy_rest callbacks
%%----------------------------------------------------------------------

-spec init(Req, State) -> Result
	when
		Req :: cowboy_req:req(),
		State :: state(),
		Result :: {cowboy_rest, Req, State}
				| {cowboy_rest, Req, State, hibernate | timeout()}.
%% @doc Initialize the handler.
init(Req, State) ->
	{cowboy_rest, Req, State}.

-spec allowed_methods(Req, State) -> Result
	when
		Req :: cowboy_req:req(),
		State :: state(),
		Result :: {Methods, Req, State},
		Methods :: [binary()].
%% @doc Allowed HTTP methods.
allowed_methods(Req, State) ->
	{[<<"POST">>], Req, State}.

-spec content_types_provided(Req, State) -> Result
	when
		Req :: cowboy_req:req(),
		State :: state(),
		Result :: {Provided, Req, State},
		Provided :: [{binary() | ParsedMime, ProvideCallback}],
		ParsedMime :: {Type, SubType, '*' | Params},
		Type :: binary(),
		SubType :: binary(),
		Params :: [{Key, Value}],
		Key :: binary(),
		Value :: binary(),
		ProvideCallback :: atom() | undefined.
%% @doc Return the list of media types the resource provides
%% 	in order of preference.
content_types_provided(Req, State) ->
	ParsedMime = {<<"application">>, <<"json">>, '*'},
	ProvideCallback = to_json,
	{[{ParsedMime, ProvideCallback}], Req, State}.

-spec content_types_accepted(Req, State) -> Result
	when
		Req :: cowboy_req:req(),
		State :: state(),
		Result :: {Accept, Req, State},
		Accept :: [{'*' | binary() | ParsedMime, AcceptCallback }],
		ParsedMime :: {Type, SubType, '*' | Params},
		Type :: binary(),
		SubType :: binary(),
		Params :: [{Key, Value}],
		Key :: binary(),
		Value :: binary(),
		AcceptCallback :: atom() | undefined.
%% @doc Return the list of media types the resource accepts
%% 	in order of preference.
content_types_accepted(Req, State) ->
	ParsedMime = {<<"application">>, <<"json">>, '*'},
	AcceptCallback = from_json,
	{[{ParsedMime, AcceptCallback}], Req, State}.

-spec resource_exists(Req, State) -> Result
	when
		Req :: cowboy_req:req(),
		State :: state(),
		Result :: {Exists, Req, State},
		Exists :: boolean().
%% @doc Return whether the resource exists or not.
resource_exists(#{bindings := #{version := ApiVersion,
		'ChargingDataRef' := ChargingDataRef}} = Req, State) ->
	case ets:lookup(chf_dataref, ChargingDataRef) of
		[{_, ConnPid, RatingDataRef}] ->
			State1 = State#{conn_pid => ConnPid,
					version => ApiVersion,
					chargingdataref => ChargingDataRef,
					ratingdataref => RatingDataRef},
			{true, Req, State1};
		[] ->
			{false, Req, State}
	end;
resource_exists(#{bindings := #{version := ApiVersion}} = Req, State) ->
	State1 = State#{version => ApiVersion},
	{true, Req, State1}.

-spec allow_missing_post(Req, State) -> Result
	when
		Req :: cowboy_req:req(),
		State :: state(),
		Result :: {Exists, Req, State},
		Exists :: boolean().
%% @doc Return whether post allowed on non-existing resource.
allow_missing_post(Req, State) ->
	{false, Req, State}.

-spec from_json(Req, State) -> Result
	when
		Req :: cowboy_req:req(),
		State :: state(),
		Result :: {RestResult, Req, State},
		RestResult :: boolean() | {created, URI} | {see_other, URI}
				| stop | {switch_handler, cowboy_loop},
		URI :: iodata().
%% @doc Process the request body.
from_json(#{bindings := #{'ChargingDataRef' := ChargingDataRef},
		path := Path} = Req, #{conn_pid := ConnPid,
		chargingdataref := ChargingDataRef,
		ratingdataref := RatingDataRef} = State) ->
	Operation = case hd(lists:reverse(binary:split(Path, <<$/>>, [global]))) of
		<<"update">> ->
			update;
		<<"release">> ->
			release
	end,
	{ok, RequestBody, Req1} = read_body(Req, []),
	{ok, ChargingData} = zj:binary_decode(RequestBody),
	RatingData = zj:binary_encode(to_ratingdata(ChargingData)),
	Headers = #{<<"content-type">> => <<"application/json">>,
			<<"accept">> => [<<"application/json">>,
			$,, <<"application/problem+json">>]},
	ApiName = <<"nrf-rating">>,
	ApiVersion = <<"v1">>,
	Resource = <<"ratingdata">>,
	NrfPath= iolist_to_binary([$/, ApiName, $/, ApiVersion, $/, Resource,
			$/, RatingDataRef, $/, atom_to_binary(Operation)]),
	StreamRef = gun:post(ConnPid, NrfPath, Headers, RatingData),
	State1 = State#{operation => Operation, stream_ref => StreamRef},
	{{switch_handler, cowboy_loop}, Req1, State1};
from_json(Req, State) ->
	{ok, RequestBody, Req1} = read_body(Req, []),
	{ok, ChargingData} = zj:binary_decode(RequestBody),
	RatingData = zj:binary_encode(to_ratingdata(ChargingData)),
	Headers = #{<<"content-type">> => <<"application/json">>,
			<<"accept">> => [<<"application/json">>,
			$,, <<"application/problem+json">>]},
	ApiName = <<"nrf-rating">>,
	ApiVersion = <<"v1">>,
	Resource = <<"ratingdata">>,
	NrfPath = iolist_to_binary([$/, ApiName, $/, ApiVersion, $/, Resource]),
	ConnPid = nrf(),
	StreamRef = gun:post(ConnPid, NrfPath, Headers, RatingData),
	State1 = State#{operation => create,
			conn_pid => ConnPid, stream_ref => StreamRef},
	{{switch_handler, cowboy_loop}, Req1, State1}.

-spec info(Info, Req, State) -> Result
	when
		Info :: any(),
		Req :: cowboy_req:req(),
		State :: state(),
		Result :: {ok, Req, State}
				| {ok, Req, State, hibernate | timeout()}
				| {{switch_handler, cowboy_rest}, Req, State}
				| {stop, Req, State}.
%% @doc Handle received messages.
info({gun_response, ConnPid, StreamRef, fin, Status, _Headers} = _Info,
		Req, #{conn_pid := ConnPid, stream_ref := StreamRef} = State) ->
	Req1 = cowboy_req:reply(Status, #{}, Req),
	State1 = State#{status => Status},
	{stop, Req1, State1};
info({gun_response, ConnPid, StreamRef, nofin, 201, Headers} = _Info,
		Req, #{operation := create, conn_pid := ConnPid,
		stream_ref := StreamRef} = State) ->
	case lists:keyfind(<<"location">>, 1, Headers) of
		{_, Location} ->
			RatingDataRef = hd(lists:reverse(binary:split(Location, <<$/>>, [global]))),
			ChargingDataRef = chf_rest:id(),
			ets:insert(chf_dataref, {ChargingDataRef, ConnPid, RatingDataRef}),
			State1 = State#{ratingdataref => RatingDataRef,
					chargingdataref => ChargingDataRef, status => 201},
			{ok, Req, State1};
		false ->
			State1 = State#{status => 201},
			{ok, Req, State1}
	end;
info({gun_response, ConnPid, StreamRef, nofin, Status, _Headers} = _Info,
		Req, #{conn_pid := ConnPid, stream_ref := StreamRef} = State) ->
	State1 = State#{status => Status},
	{ok, Req, State1};
info({gun_data, ConnPid, StreamRef, nofin, Data} = _Info, Req,
		#{conn_pid := ConnPid, stream_ref := StreamRef,
		data := Data1} = State) ->
	State1 = State#{data => [Data | Data1]},
	{ok, Req, State1};
info({gun_data, ConnPid, StreamRef, nofin, Data} = _Info, Req,
		#{conn_pid := ConnPid, stream_ref := StreamRef } = State) ->
	State1 = State#{data => [Data]},
	{ok, Req, State1};
info({gun_data, ConnPid, StreamRef, fin, Data} = _Info, Req,
		#{operation := create, conn_pid := ConnPid, stream_ref := StreamRef,
		data := Data1, version := ApiVersion, status := 201,
		chargingdataref := ChargingDataRef} = State) ->
	{ok, RatingData} = zj:binary_decode(lists:reverse([Data | Data1])),
	ChargingData = from_ratingdata(RatingData),
	Body = zj:binary_encode(ChargingData),
	ApiName = <<"nchf-convergedcharging">>,
	Resource = <<"chargingdata">>,
	CollectionPath = [$/, ApiName, $/, ApiVersion, $/, Resource],
	Location = [CollectionPath, $/, ChargingDataRef],
	Headers = #{<<"content-type">> => <<"application/json">>,
			<<"location">> => Location},
	Req1 = cowboy_req:reply(201, Headers, Body, Req),
	{stop, Req1, State};
info({gun_data, ConnPid, StreamRef, fin, Data} = _Info, Req,
		#{operation := create, conn_pid := ConnPid, stream_ref := StreamRef,
		version := ApiVersion, status := 201,
		chargingdataref := ChargingDataRef} = State) ->
	{ok, RatingData} = zj:binary_decode(Data),
	ChargingData = from_ratingdata(RatingData),
	Body = zj:binary_encode(ChargingData),
	ApiName = <<"nchf-convergedcharging">>,
	Resource = <<"chargingdata">>,
	CollectionPath = [$/, ApiName, $/, ApiVersion, $/, Resource],
	Location = [CollectionPath, $/, ChargingDataRef],
	Headers = #{<<"content-type">> => <<"application/json">>,
			<<"location">> => Location},
	Req1 = cowboy_req:reply(201, Headers, Body, Req),
	{stop, Req1, State};
info({gun_data, ConnPid, StreamRef, fin, Data} = _Info, Req,
		#{conn_pid := ConnPid, stream_ref := StreamRef,
		data := Data1, status := 200} = State) ->
	{ok, RatingData} = zj:binary_decode(lists:reverse([Data | Data1])),
	ChargingData = from_ratingdata(RatingData),
	Body = zj:binary_encode(ChargingData),
	Headers = #{<<"content-type">> => <<"application/json">>},
	Req1 = cowboy_req:reply(200, Headers, Body, Req),
	{stop, Req1, State};
info({gun_data, ConnPid, StreamRef, fin, Data} = _Info, Req,
		#{conn_pid := ConnPid, stream_ref := StreamRef,
		status := 200} = State) ->
	{ok, RatingData} = zj:binary_decode(Data),
	ChargingData = from_ratingdata(RatingData),
	Body = zj:binary_encode(ChargingData),
	Headers = #{<<"content-type">> => <<"application/json">>},
	Req1 = cowboy_req:reply(200, Headers, Body, Req),
	{stop, Req1, State};
info({gun_data, ConnPid, StreamRef, fin, Data} = _Info, Req,
		#{conn_pid := ConnPid, stream_ref := StreamRef,
		status := Status} = State)
		when Status == 400; Status == 403; Status == 404 ->
	{ok, RatingProblem} = zj:binary_decode(Data),
	ChargingProblem = from_problemdetails(Status, RatingProblem),
	Body = zj:binary_encode(ChargingProblem),
	Headers = #{<<"content-type">> => <<"application/problem+json">>},
	Req1 = cowboy_req:reply(Status, Headers, Body, Req),
	{stop, Req1, State};
info({gun_data, ConnPid, StreamRef, fin, _Data} = _Info, Req,
		#{conn_pid := ConnPid, stream_ref := StreamRef} = State) ->
	ChargingProblem = #{<<"title">> => <<"Error">>,
			<<"detail">> => <<"An unexpected rating failure has occurred.">>,
			<<"status">> => 500, <<"cause">> => <<"SYSTEM_FAILURE">>},
	Body = zj:binary_encode(ChargingProblem),
	Headers = #{<<"content-type">> => <<"application/problem+json">>},
	Req1 = cowboy_req:reply(500, Headers, Body, Req),
	{stop, Req1, State}.

%%----------------------------------------------------------------------
%%  Internal functions
%%----------------------------------------------------------------------

%% @hidden
read_body(Req, Acc) ->
	case cowboy_req:read_body(Req) of
		{ok, Data, Req1} ->
			{ok, lists:reverse([Data | Acc]), Req1};
		{more, Data, Req1} ->
			read_body(Req1, [Data | Acc])
	end.

%% @hidden
to_ratingdata(#{<<"serviceSpecificationInfo">>
		:= ServiceSpecId} = ChargingData) ->
	to_ratingdata1(ServiceSpecId, ChargingData);
to_ratingdata(#{<<"nfConsumerIdentification">>
		:= #{<<"nodeFunctionality">> := <<"SMF">>}} = ChargingData) ->
	to_ratingdata1(<<"32255@3gpp.org">>, ChargingData);
to_ratingdata(#{<<"nfConsumerIdentification">>
		:= #{<<"nodeFunctionality">> := <<"V_SMF">>}} = ChargingData) ->
	to_ratingdata1(<<"32255@3gpp.org">>, ChargingData);
to_ratingdata(#{<<"nfConsumerIdentification">>
		:= #{<<"nodeFunctionality">> := <<"AMF">>}} = ChargingData) ->
	to_ratingdata1(<<"32256@3gpp.org">>, ChargingData);
to_ratingdata(#{<<"nfConsumerIdentification">>
		:= #{<<"nodeFunctionality">> := <<"SMSF">>}} = ChargingData) ->
	to_ratingdata1(<<"32274@3gpp.org">>, ChargingData);
to_ratingdata(#{<<"nfConsumerIdentification">>
		:= #{<<"nodeFunctionality">> := <<"SGW">>}} = ChargingData) ->
	to_ratingdata1(<<"32251@3gpp.org">>, ChargingData);
to_ratingdata(#{<<"nfConsumerIdentification">>
		:= #{<<"nodeFunctionality">> := <<"ePDG">>}} = ChargingData) ->
	to_ratingdata1(<<"32251@3gpp.org">>, ChargingData);
to_ratingdata(#{<<"nfConsumerIdentification">>
		:= #{<<"nodeFunctionality">> := <<"SGSN">>}} = ChargingData) ->
	to_ratingdata1(<<"32251@3gpp.org">>, ChargingData);
to_ratingdata(#{<<"nfConsumerIdentification">>
		:= #{<<"nodeFunctionality">> := <<"IMS_Node">>}} = ChargingData) ->
	to_ratingdata1(<<"32260@3gpp.org">>, ChargingData).
%% @hidden
to_ratingdata1(ServiceSpecId, ChargingData) ->
	F = fun(<<"invocationSequenceNumber">> = Key, N, Facc)
					when is_integer(N), N >= 0 ->
				Facc#{Key => N};
			(<<"subscriberIdentifier">>, Value, Facc)
					when is_binary(Value) ->
				Facc#{<<"subscriptionId">> => [Value]};
			(<<"oneTimeEvent">> = Key, Value, Facc)
					when is_boolean(Value) ->
				Facc#{Key => Value};
			(<<"oneTimeEventType">> = Key, Value, Facc)
					when Value == <<"IEC">>; Value == <<"PEC">> ->
				Facc#{Key => Value};
			(<<"multipleUnitUsage">>, Value, Facc)
					when is_list(Value) ->
				ServiceRating = to_servicerating(ServiceSpecId,
						ChargingData, Value, []),
				Facc#{<<"serviceRating">> => ServiceRating};
			(_, _, Facc) ->
				Facc
	end,
   CHF = #{<<"nodeFunctionality">> => <<"CHF">>},
	RatingData = #{<<"invocationTimeStamp">> => list_to_binary(chf_rest:now()),
			<<"nfConsumerIdentification">> => CHF},
	maps:fold(F, RatingData, ChargingData).

%% @hidden
to_servicerating(ServiceSpecId, ChargingData,
		[#{<<"ratingGroup">> := RatingGroup} = MultipleUnitUsage | T], Acc)
		when is_integer(RatingGroup) ->
	F = fun(<<"ratingGroup">> = Key, Value, Facc)
					when is_integer(Value) ->
				Facc#{Key => Value};
			(<<"uPFID">> = Key, Value, Facc)
					when is_binary(Value)->
				Facc#{Key => Value};
			(_, _, Facc) ->
				Facc
	end,
	ServiceRating1 = case maps:find(<<"pDUSessionChargingInformation">>,
			ChargingData) of
		{ok, PDUSessionChargingInformation}
				when is_map(PDUSessionChargingInformation) ->
			#{<<"serviceContextId">> => ServiceSpecId,
					<<"serviceInformation">> => PDUSessionChargingInformation};
		error ->
			#{<<"serviceContextId">> => ServiceSpecId}
	end,
	ServiceRating2 = maps:fold(F, ServiceRating1, MultipleUnitUsage),
	Acc1 = case maps:find(<<"requestedUnit">>, MultipleUnitUsage) of
		{ok, RequestedUnit} when is_map(RequestedUnit) ->
			[ServiceRating2#{<<"requestedUnit">> => RequestedUnit,
					<<"requestSubType">> => <<"RESERVE">>} | Acc];
		error ->
			Acc
	end,
	Acc2 = case maps:find(<<"usedUnitContainer">>, MultipleUnitUsage) of
		{ok, UsedUnitContainer} when is_list(UsedUnitContainer) ->
			ServiceRating3 = ServiceRating2#{<<"requestSubType">> => <<"DEBIT">>},
			to_consumedunit(UsedUnitContainer, ServiceRating3, Acc1);
		error ->
			Acc1
	end,
	to_servicerating(ServiceSpecId, ChargingData, T, Acc2);
to_servicerating(_, _, [], Acc) ->
	lists:reverse(Acc).

%% @hidden
to_consumedunit([UsedUnitContainer | T], ServiceRating, Acc)
		when is_map(UsedUnitContainer) ->
	F = fun(<<"time">> = Key, N, Facc)
					when is_integer(N), N > 0 ->
				Facc#{Key => N};
			(<<"totalVolume">> = Key, N, Facc)
					when is_integer(N), N > 0 ->
				Facc#{Key => N};
			(<<"uplinkVolume">> = Key, N, Facc)
					when is_integer(N), N > 0 ->
				Facc#{Key => N};
			(<<"downlinkVolume">> = Key, N, Facc)
					when is_integer(N), N > 0 ->
				Facc#{Key => N};
			(<<"serviceSpecificUnit">> = Key, N, Facc)
					when is_integer(N), N > 0 ->
				Facc#{Key => N};
			(_, _, Facc) ->
				Facc
	end,
	ConsumedUnit = maps:fold(F, #{}, UsedUnitContainer),
	Acc1 = case maps:find(<<"serviceId">>, UsedUnitContainer) of
		{ok, SI} ->
			[ServiceRating#{<<"consumedUnit">> => ConsumedUnit,
					<<"serviceId">> => SI} | Acc];
		error ->
			[ServiceRating#{<<"consumedUnit">> => ConsumedUnit} | Acc]
	end,
	to_consumedunit(T, ServiceRating, Acc1);
to_consumedunit([], _, Acc) ->
	Acc.

%% @hidden
from_ratingdata(RatingData)
		when is_map(RatingData) ->
	F = fun(<<"invocationSequenceNumber">> = Key, N, Facc)
					when is_integer(N), N >= 0 ->
				Facc#{Key => N};
			(<<"serviceRating">>, Value, Facc)
					when is_list(Value) ->
				case from_servicerating(Value, []) of
					[] ->
						Facc;
					MUI ->
						Facc#{<<"multipleUnitInformation">> => MUI}
				end;
			(_, _, Facc) ->
				Facc
	end,
	ChargingData = #{<<"invocationTimeStamp">> => list_to_binary(chf_rest:now())},
	maps:fold(F, ChargingData, RatingData).

%% @hidden
from_servicerating([#{<<"grantedUnit">> := GU} = SR| T], Acc) ->
	Fold = fun(<<"ratingGroup">> = Key, N, Facc)
					when is_integer(N) ->
				Facc#{Key => N};
			(<<"resultCode">> = Key, Value, Facc)
					when is_binary(Value) ->
				Facc#{Key => Value};
			(<<"grantedUnit">> = Key, Value, Facc)
					when is_map(Value) ->
				Facc#{Key => Value};
			(<<"validUnits">>, Value, Facc) when is_integer(Value),
					is_map_key(<<"time">>, GU) ->
				Facc#{<<"timeQuotaThreshold">> => Value};
			(<<"validUnits">>, Value, Facc) when is_integer(Value),
					is_map_key(<<"totalVolume">>, GU) ->
				Facc#{<<"volumeQuotaThreshold">> => Value};
			(<<"validUnits">>, Value, Facc) when is_integer(Value),
					is_map_key(<<"uplinkVolume">>, GU) ->
				Facc#{<<"volumeQuotaThreshold">> => Value};
			(<<"validUnits">>, Value, Facc) when is_integer(Value),
					is_map_key(<<"downlinkVolume">>, GU) ->
				Facc#{<<"volumeQuotaThreshold">> => Value};
			(<<"validUnits">>, Value, Facc) when is_integer(Value),
					is_map_key(<<"serviceSpecificUnit">>, GU) ->
				Facc#{<<"unitQuotaThreshold">> => Value};
			(<<"uPFID">> = Key, Value, Facc)
					when is_binary(Value) ->
				Facc#{Key => Value};
			(_Key, _Value, Facc) ->
				Facc
	end,
	MultipleUnitInformation = maps:fold(Fold, #{}, SR),
	from_servicerating(T, [MultipleUnitInformation | Acc]);
from_servicerating([_ | T], Acc) ->
	from_servicerating(T, Acc);
from_servicerating([], Acc) ->
	lists:reverse(Acc).

%% @hidden
from_problemdetails(400, #{<<"cause">> := <<"RATING_FAILED">>}) ->
	#{<<"title">> => <<"Failed">>,
			<<"detail">> => <<"The request failed due to incomplete or inconsistent information.">>,
			<<"status">> => 400,
			<<"cause">> => <<"CHARGING_FAILED">>};
from_problemdetails(400, #{<<"cause">> := <<"CHARGING_FAILED">>}) ->
	#{<<"title">> => <<"Failed">>,
			<<"detail">> => <<"The request failed due to incomplete or inconsistent information.">>,
			<<"status">> => 400,
			<<"cause">> => <<"CHARGING_FAILED">>};
from_problemdetails(400, #{<<"cause">> := <<"RE_AUTHORIZATION_FAILED">>}) ->
	#{<<"title">> => <<"Failed">>,
			<<"detail">> => <<"The request failed due to incomplete or inconsistent information.">>,
			<<"status">> => 400,
			<<"cause">> => <<"RE_AUTHORIZATION_FAILED">>};
from_problemdetails(400, _RatingProblem) ->
	#{<<"title">> => <<"Failed">>,
			<<"detail">> => <<"The request failed due to incomplete or inconsistent information.">>,
			<<"status">> => 400,
			<<"cause">> => <<"CHARGING_FAILED">>};
from_problemdetails(403, #{<<"cause">> := <<"CHARGING_NOT_APPLICABLE">>}) ->
	#{<<"title">> => <<"Not required">>,
			<<"detail">> => <<"It was determined that service may be provided to the subscriber without charging.">>,
			<<"status">> => 403,
			<<"cause">> => <<"CHARGING_NOT_APPLICABLE">>};
from_problemdetails(403, #{<<"cause">> := <<"END_USER_REQUEST_DENIED">>}) ->
	#{<<"title">> => <<"Denied">>,
			<<"detail">> => <<"It was determined that service may not be provided to the subscriber.">>,
			<<"status">> => 403,
			<<"cause">> => <<"END_USER_REQUEST_DENIED">>};
from_problemdetails(403, #{<<"cause">> := <<"END_USER_REQUEST_REJECTED">>}) ->
	#{<<"title">> => <<"Denied">>,
			<<"detail">> => <<"The charging service request is rejected for this end user.">>,
			<<"status">> => 403,
			<<"cause">> => <<"END_USER_REQUEST_REJECTED">>};
from_problemdetails(403, #{<<"cause">> := <<"QUOTA_LIMIT_REACHED">>}) ->
	#{<<"title">> => <<"Out of credit">>,
			<<"detail">> => <<"The subscriber's account has insufficient balance to cover the request.">>,
			<<"status">> => 403,
			<<"cause">> => <<"QUOTA_LIMIT_REACHED">>};
from_problemdetails(403, _RatingProblem) ->
	#{<<"title">> => <<"Denied">>,
			<<"detail">> => <<"It was determined that service may not be provided to the subscriber.">>,
			<<"status">> => 403,
			<<"cause">> => <<"END_USER_REQUEST_DENIED">>};
from_problemdetails(404, _RatingProblem) ->
	#{<<"title">> => <<"Uknown user">>,
			<<"detail">> => <<"The end user is not known to the charging service.">>,
			<<"status">> => 404,
			<<"cause">> => <<"USER_UNKNOWN">>};
from_problemdetails(500, #{<<"cause">> := <<"SYSTEM_FAILURE">>}) ->
	#{<<"title">> => <<"Error">>,
			<<"detail">> => <<"A system failure occurred while processing the request.">>,
			<<"status">> => 500,
			<<"cause">> => <<"SYSTEM_FAILURE">>};
from_problemdetails(500, _RatingProblem) ->
	#{<<"title">> => <<"Error">>,
			<<"detail">> => <<"An unexpected error occurred while processing the request.">>,
			<<"status">> => 500,
			<<"cause">> => <<"SYSTEM_FAILURE">>};
from_problemdetails(Status, _RatingProblem) ->
	#{<<"title">> => <<"Other">>,
			<<"status">> => Status,
			<<"cause">> => <<"CHARGING_FAILED">>}.

%% @hidden
nrf() ->
	nrf_local(pg:get_local_members(chf, nrf)).
%% @hidden
nrf_local([]) ->
	nrf_global(pg:get_members(chf, nrf));
nrf_local(Nrfs) ->
	lists:nth(rand:uniform(length(Nrfs)), Nrfs).
%% @hidden
nrf_global([]) ->
	throw(no_nrf);
nrf_global(Nrfs) ->
	lists:nth(rand:uniform(length(Nrfs)), Nrfs).

