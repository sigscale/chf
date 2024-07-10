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
		ratingdata => binary()}.

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
	Headers = [{<<"content-type">>, <<"application/json">>},
			{<<"accept">>, <<"application/json">>}],
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
	RequestHeaders = [{<<"content-type">>, <<"application/json">>},
			{<<"accept">>, <<"application/json">>}],
	ApiName = <<"nrf-rating">>,
	ApiVersion = <<"v1">>,
	Resource = <<"ratingdata">>,
	NrfPath = iolist_to_binary([$/, ApiName, $/, ApiVersion, $/, Resource]),
	ConnPid = nrf(),
	StreamRef = gun:post(ConnPid, NrfPath, RequestHeaders, RatingData),
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
	{stop, Req1, State};
info({gun_response, ConnPid, StreamRef, nofin, 201, Headers} = _Info,
		Req, #{operation := create, conn_pid := ConnPid,
		stream_ref := StreamRef} = State) ->
	case lists:keyfind(<<"location">>, 1, Headers) of
		{_, Location} ->
			RatingDataRef = hd(lists:reverse(binary:split(Location, <<$/>>, [global]))),
			ChargingDataRef = chf_rest:id(),
			ets:insert(chf_dataref, {ChargingDataRef, ConnPid, RatingDataRef}),
			State1 = State#{ratingdataref => RatingDataRef,
					chargingdataref => ChargingDataRef},
			{ok, Req, State1};
		false ->
			{ok, Req, State}
	end;
info({gun_response, ConnPid, StreamRef, nofin, _Status, _Headers} = _Info,
		Req, #{conn_pid := ConnPid, stream_ref := StreamRef} = State) ->
	{ok, Req, State};
info({gun_data, ConnPid, StreamRef, nofin, Data} = _Info, Req,
		#{conn_pid := ConnPid, stream_ref := StreamRef,
		ratingdata := Data1} = State) ->
	State1 = State#{ratingdata => [Data | Data1]},
	{ok, Req, State1};
info({gun_data, ConnPid, StreamRef, nofin, Data} = _Info, Req,
		#{conn_pid := ConnPid, stream_ref := StreamRef } = State) ->
	State1 = State#{ratingdata => Data},
	{ok, Req, State1};
info({gun_data, ConnPid, StreamRef, fin, Data} = _Info, Req,
		#{operation := create, conn_pid := ConnPid, stream_ref := StreamRef,
		ratingdata := Data1, version := ApiVersion,
		chargingdataref := ChargingDataRef} = State) ->
	{ok, RatingData} = zj:binary_decode(lists:reverse([Data | Data1])),
	ChargingData = from_ratingdata(RatingData),
	Body = zj:binary_encode(ChargingData),
	ApiName = <<"nchf-convergedcharging">>,
	Resource = <<"chargingdata">>,
	CollectionPath = [$/, ApiName, $/, ApiVersion, $/, Resource],
	Location = [CollectionPath, $/, ChargingDataRef],
	Headers = #{<<"location">> => Location},
	Req1 = cowboy_req:reply(201, Headers, Body, Req),
	{stop, Req1, State};
info({gun_data, ConnPid, StreamRef, fin, Data} = _Info, Req,
		#{conn_pid := ConnPid, stream_ref := StreamRef,
		ratingdata := Data1} = State) ->
	{ok, RatingData} = zj:binary_decode(lists:reverse([Data | Data1])),
	ChargingData = from_ratingdata(RatingData),
	Body = zj:binary_encode(ChargingData),
	Headers = #{},
	Req1 = cowboy_req:reply(200, Headers, Body, Req),
	{stop, Req1, State};
info({gun_data, ConnPid, StreamRef, fin, Data} = _Info, Req,
		#{operation := create, conn_pid := ConnPid, stream_ref := StreamRef,
		version := ApiVersion, chargingdataref := ChargingDataRef} = State) ->
	{ok, RatingData} = zj:binary_decode(Data),
	ChargingData = from_ratingdata(RatingData),
	Body = zj:binary_encode(ChargingData),
	ApiName = <<"nchf-convergedcharging">>,
	Resource = <<"chargingdata">>,
	CollectionPath = [$/, ApiName, $/, ApiVersion, $/, Resource],
	Location = [CollectionPath, $/, ChargingDataRef],
	Headers = #{<<"location">> => Location},
	Req1 = cowboy_req:reply(201, Headers, Body, Req),
	{stop, Req1, State};
info({gun_data, ConnPid, StreamRef, fin, Data} = _Info, Req,
		#{conn_pid := ConnPid, stream_ref := StreamRef} = State) ->
	{ok, RatingData} = zj:binary_decode(Data),
	ChargingData = from_ratingdata(RatingData),
	Body = zj:binary_encode(ChargingData),
	Headers = #{},
	Req1 = cowboy_req:reply(200, Headers, Body, Req),
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
to_ratingdata(ChargingData)
		when is_map(ChargingData) ->
	ServiceSpecId = maps:get(<<"serviceSpecificationInfo">>,
			ChargingData, <<"32291@3gpp.org">>),
	F = fun(<<"invocationSequenceNumber">> = Key, N, Acc)
					when is_integer(N), N > 0 ->
				Acc#{Key => N};
			(<<"subscriberIdentifier">>, Value, Acc)
					when is_binary(Value) ->
				Acc#{<<"subscriptionId">> => [Value]};
			(<<"oneTimeEvent">> = Key, Value, Acc)
					when is_boolean(Value) ->
				Acc#{Key => Value};
			(<<"oneTimeEventType">> = Key, Value, Acc)
					when Value == <<"IEC">>; Value == <<"PEC">> ->
				Acc#{Key => Value};
			(<<"multipleUnitUsage">>, Value, Acc)
					when is_list(Value) ->
				ServiceRating = to_servicerating(ServiceSpecId, Value, []),
				Acc#{<<"serviceRating">> => ServiceRating};
			(_, _, Acc) ->
				Acc
	end,
   CHF = #{<<"nodeFunctionality">> => <<"CHF">>},
	AccIn = #{<<"invocationTimeStamp">> => chf_rest:now(),
			<<"nfConsumerIdentification">> => CHF},
	maps:fold(F, AccIn, ChargingData).

%% @hidden
to_servicerating(ServiceSpecId,
		[#{<<"ratingGroup">> := RatingGroup} = MultipleUnitUsage | T], Acc)
		when is_integer(RatingGroup), RatingGroup >= 0,
		is_map(MultipleUnitUsage) ->
	ServiceRating = #{<<"serviceContextId">> => ServiceSpecId,
			<<"ratingGroup">> => RatingGroup},
	Acc1 = case maps:find(<<"requestedUnit">>, MultipleUnitUsage) of
		{ok, RequestedUnit} ->
			[ServiceRating#{<<"requestedUnit">> => RequestedUnit,
			<<"requestSubType">> => <<"RESERVE">>} | Acc];
		error ->
			Acc
	end,
	Acc2 = case maps:find(<<"usedUnitContainer">>, MultipleUnitUsage) of
		{ok, UsedUnitContainer} ->
			ServiceRating1 = ServiceRating#{<<"requestSubType">> => <<"DEBIT">>},
			to_servicerating1(UsedUnitContainer, ServiceRating1, Acc1);
		error ->
			Acc1
	end,
	to_servicerating(ServiceSpecId, T, Acc2);
to_servicerating(_, [], Acc) ->
	lists:reverse(Acc).
%% @hidden
to_servicerating1([UsedUnitContainer | T], ServiceRating, Acc)
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
	Acc1 = case maps:find(<<"serviceId">>,
			UsedUnitContainer) of
		{ok, SI} ->
			[ServiceRating#{<<"consumedUnit">> => ConsumedUnit,
					<<"serviceId">> => SI} | Acc];
		error ->
			[ServiceRating#{<<"consumedUnit">> => ConsumedUnit} | Acc]
	end,
	to_servicerating1(T, ServiceRating, Acc1);
to_servicerating1([], _, Acc) ->
	lists:reverse(Acc).

%% @hidden
from_ratingdata(RatingData)
		when is_map(RatingData) ->
	F = fun(<<"invocationSequenceNumber">> = Key, N, Acc)
					when is_integer(N), N > 0 ->
				Acc#{Key => N};
			(<<"serviceRating">>, Value, Acc)
					when is_list(Value) ->
				case from_servicerating(Value, []) of
					[] ->
						Acc;
					MUI ->
						Acc#{<<"multipleUnitInformation">> => MUI}
				end;
			(_, _, Acc) ->
				Acc
	end,
	AccIn = #{<<"invocationTimeStamp">> => chf_rest:now()},
	maps:fold(F, AccIn, RatingData).

%% @hidden
from_servicerating([#{<<"requestSubType">> := <<"RESERVE">>} = SR| T],
		Acc) ->
	Fold = fun(<<"ratingGroup">> = Key, N, Facc)
					when is_integer(N), N > 0 ->
				Facc#{Key => N};
			(<<"resultCode">> = Key, Value, Facc)
					when is_binary(Value) ->
				Facc#{Key => Value};
			(<<"grantedUnit">> = Key, Value, Facc)
					when is_map(Value) ->
				Facc#{Key => Value};
			(_Key, _Value, Facc) ->

				Facc
	end,
	MUI = maps:fold(Fold, #{}, SR),
	from_servicerating(T, [MUI | Acc]);
from_servicerating([_ | T], Acc) ->
	from_servicerating(T, Acc);
from_servicerating([], Acc) ->
	lists:reverse(Acc).

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

