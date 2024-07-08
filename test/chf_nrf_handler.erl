%%% chf_nrf_handler.erl
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
%%% @doc This {@link //cowboy/cowboy_rest. cowboy_rest} callback module
%%% 	implements an Nrf interface endpoint handler for a test suite of
%%% 	the {@link //chf. chf} application.
%%%
-module(chf_nrf_handler).
-copyright('Copyright (c) 2024 SigScale Global Inc.').
-author('Vance Shipley <vances@sigscale.org>').

-behavior(cowboy_rest).

% mandatory callbacks
-export([init/2]).
% optional callbacks
-export([allowed_methods/2, content_types_provided/2,
		content_types_accepted/2, resource_exists/2,
		allow_missing_post/2]).
% other callbacks
-export([to_json/2, from_json/2]).

-define(RATINGDATA, <<"/nrf-rating/v1/ratingdata">>).
-define(RESERVATION, 1000000).

-type state() :: #{rf := pid(),
		ratingdataref => binary(),
		ratingdata => map()}.

%%----------------------------------------------------------------------
%%  The chf_nrf_handler cowboy_rest callbacks
%%----------------------------------------------------------------------

-spec init(Req, State) -> Result
	when
		Req :: cowboy_req:req(),
		State :: state(),
		Result :: {cowboy_rest, Req, State}.
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
resource_exists(#{bindings := #{'RatingDataRef' := RatingDataRef}} = Req,
		State) ->
	case ets:lookup(rf_dataref, RatingDataRef) of
		[_] ->
			{true, Req, State};
		[] ->
			{false, Req, State}
	end;
resource_exists(Req, State) ->
	{true, Req, State}.

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
		RestResult :: boolean() | {created, URI} | {see_other, URI},
		URI :: iodata().
%% @doc Process the request body.
from_json(#{bindings := #{'RatingDataRef' := RatingDataRef},
		path := Path} = Req, #{rf := Rf} = State) ->
	Operation = hd(lists:reverse(binary:split(Path, <<$/>>, [global]))),
	{ok, RequestBody, Req1} = read_body(Req, []),
	{ok, RatingData1} = zj:binary_decode(RequestBody),
	Subscriber = get_subscriber(RatingData1),
	SN = maps:get(<<"invocationSequenceNumber">>, RatingData1),
	SR1 = maps:get(<<"serviceRating">>, RatingData1),
	try
		DebitAmount = get_debit(SR1),
		SR2 = case chf_rf_server:debit(Rf, Subscriber, DebitAmount) of
			{ok, _} ->
				set_debit(SR1);
      	{error, Reason} ->
				throw(Reason)
		end,
		case Operation of
			<<"update">> ->
				ReserveAmount = get_reserve(SR2),
				case chf_rf_server:reserve(Rf, Subscriber, ReserveAmount) of
					{ok, _} ->
						set_reserve(SR2);
      			{error, Reason1} ->
						throw(Reason1)
				end;
			<<"release">> ->
				ets:delete(rf_dataref, RatingDataRef),
				SR2
		end
	of
		SR3 ->
			RatingData2 = #{<<"invocationSequenceNumber">> => SN,
					<<"invocationTimeStamp">> => chf_rest:now(),
					<<"serviceRating">> => SR3},
			ResponseBody = zj:binary_encode(RatingData2),
			Req2 = cowboy_req:reply(200, #{}, ResponseBody, Req1),
			{stop, Req2, State}
	catch
      out_of_credit ->
			Req2 = cowboy_req:reply(403, #{}, Req1),
			{stop, Req2, State};
		not_found ->
			Req2 = cowboy_req:reply(404, #{}, Req1),
			{stop, Req2, State}
	end;
from_json(Req, #{rf := Rf} = State) ->
	{ok, RequestBody, Req1} = read_body(Req, []),
	{ok, RatingData1} = zj:binary_decode(RequestBody),
	Subscriber = get_subscriber(RatingData1),
	SN = maps:get(<<"invocationSequenceNumber">>, RatingData1),
	SR1 = maps:get(<<"serviceRating">>, RatingData1),
	ReserveAmount = get_reserve(SR1),
	case chf_rf_server:reserve(Rf, Subscriber, ReserveAmount) of
		{ok, {_Balance, ReserveAmount}} ->
			RatingDataRef = chf_rest:id(),
			ets:insert(rf_dataref, {RatingDataRef}),
			SR2 = set_reserve(SR1),
			RatingData2 = #{<<"invocationSequenceNumber">> => SN,
					<<"invocationTimeStamp">> => chf_rest:now(),
					<<"serviceRating">> => SR2},
			ResponseBody = zj:binary_encode(RatingData2),
			Req2 = cowboy_req:set_resp_body(ResponseBody, Req1),
			{{created, [?RATINGDATA, $/, RatingDataRef]}, Req2, State};
      {error, out_of_credit} ->
			Req2 = cowboy_req:reply(403, #{}, Req1),
			{stop, Req2, State};
		{error, not_found} ->
			Req2 = cowboy_req:reply(404, #{}, Req1),
			{stop, Req2, State}
	end.

-spec to_json(Req, State) -> Result
	when
		Req :: cowboy_req:req(),
		State :: state(),
		Result :: {Body, Req, State},
		Body :: iodata().
%% Produce the JSON body for a response.
to_json(Req, State) ->
	Body = <<>>,
	{Body, Req, State}.

%%----------------------------------------------------------------------
%%  Internal functions
%%----------------------------------------------------------------------

%% @hidden
read_body(Req, Acc) ->
	case cowboy_req:read_body(Req) of
		{ok, Data, NewReq} ->
			{ok, lists:reverse([Data | Acc]), NewReq};
		{more, Data, NewReq} ->
			read_body(NewReq, [Data | Acc])
	end.

%% @hidden
get_subscriber(#{<<"subscriptionId">> := [SubscriptionId | _]} = _RatingData) ->
	[_, SubscriptionId1] = binary:split(SubscriptionId, <<$->>),
	binary_to_list(SubscriptionId1).

%% @hidden
get_reserve(ServiceRating) ->
	get_reserve(ServiceRating, 0).
%% @hidden
get_reserve([#{<<"requestSubType">> := <<"RESERVE">>,
		<<"requestedUnit">> := #{<<"totalVolume">> := N}} | T], Acc) ->
	get_reserve(T, Acc + N);
get_reserve([#{<<"requestSubType">> := <<"RESERVE">>} | T], Acc) ->
	get_reserve(T, Acc + ?RESERVATION);
get_reserve([_ | T], Acc) ->
	get_reserve(T, Acc);
get_reserve([], Acc) ->
	Acc.

%% @hidden
get_debit(ServiceRating) ->
	get_debit(ServiceRating, 0).
%% @hidden
get_debit([#{<<"requestSubType">> := <<"DEBIT">>,
		<<"consumedUnit">> := #{<<"totalVolume">> := N}} | T], Acc) ->
	get_debit(T, Acc + N);
get_debit([_ | T], Acc) ->
	get_debit(T, Acc);
get_debit([], Acc) ->
	Acc.

%% @hidden
set_reserve(ServiceRating) ->
	set_reserve(ServiceRating, []).
%% @hidden
set_reserve([#{<<"requestSubType">> := <<"RESERVE">>, 
		<<"requestedUnit">> := #{<<"totalVolume">> := N}} = SR| T], Acc)
		when is_integer(N), N > 0 ->
	SR1 = maps:remove(<<"requestedUnit">>, SR),
	SR2 = SR1#{<<"grantedUnit">> => #{<<"totalVolume">> => N},
			<<"resultCode">> => <<"SUCCESS">>},
	set_reserve(T, [SR2 | Acc]);
set_reserve([#{<<"requestSubType">> := <<"RESERVE">>,
		<<"requestedUnit">> := #{}} = SR| T], Acc) ->
	SR1 = maps:remove(<<"requestedUnit">>, SR),
	SR2 = SR1#{<<"grantedUnit">> => #{<<"totalVolume">> => ?RESERVATION},
			<<"resultCode">> => <<"SUCCESS">>},
	set_reserve(T, [SR2 | Acc]);
set_reserve([H | T], Acc) ->
	set_reserve(T, [H | Acc]);
set_reserve([], Acc) ->
	lists:reverse(Acc).

%% @hidden
set_debit(ServiceRating) ->
	set_debit(ServiceRating, []).
%% @hidden
set_debit([#{<<"requestSubType">> := <<"DEBIT">>} = SR| T], Acc) ->
	SR1 = SR#{<<"resultCode">> => <<"SUCCESS">>},
	set_debit(T, [SR1 | Acc]);
set_debit([H | T], Acc) ->
	set_debit(T, [H | Acc]);
set_debit([], Acc) ->
	lists:reverse(Acc).

