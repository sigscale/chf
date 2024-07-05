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
%%% @doc This {@link //cowboy/cowboy_handler. cowboy_handler}
%%% 	callback module implements an Nchf interface endpoint
%%% 	handler in the the {@link //chf. chf} application.
%%%
-module(chf_nchf_handler).
-copyright('Copyright (c) 2024 SigScale Global Inc.').
-author('Vance Shipley <vances@sigscale.org>').

-behavior(cowboy_rest).

% mandatory callbacks
-export([init/2]).
% optional callbacks
-export([allowed_methods/2, content_types_provided/2,
		content_types_accepted/2]).
% other callbacks
-export([to_json/2, from_json/2]).

-type state() :: #{}.

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

-spec from_json(Req, State) -> Result
	when
		Req :: cowboy_req:req(),
		State :: state(),
		Result :: boolean() | {created, URI} | {see_other, URI},
		URI :: iodata().
%% @doc Process the request body.
from_json(_Req, _State) ->
	Location = <<>>,
	{created, Location}.

