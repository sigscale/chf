%%% chf_nrf_connection_fsm.erl
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
%%% @doc This {@link //stdlib/gen_statem. gen_statem} behaviour callback
%%% 	module handles {@link //diameter. diameter} service  events.
%%%
-module(chf_nrf_connection_fsm).
-copyright('Copyright (c) 2024 SigScale Global Inc.').

-behaviour(gen_statem).

%% export the callbacks needed for gen_statem behaviour
-export([init/1, handle_event/4, callback_mode/0,
			terminate/3, code_change/4]).
%% export the callbacks for gen_statem states.
-export([down/3, up/3]).
%% export the public api
-export([get_state/1]).

-include_lib("kernel/include/logger.hrl").

-type state() :: down | up.
-type statedata() :: #{
		host := Host :: inet:hostname() | inet:ip_address(),
		port := Port :: inet:port_number(),
		opts := Opts :: gun:opts(),
		pid => ConnPid :: pid(),
		protocol => Protocol :: http | http2 | raw | socks}.

%%----------------------------------------------------------------------
%%  the chf_nrf_connection_fsm public api
%%----------------------------------------------------------------------

-spec get_state(Fsm) -> Result
	when
		Fsm :: pid(),
		Result :: state().
%% @doc Get the `gen_statem' state data.
get_state(Fsm) ->
	gen_statem:call(Fsm, get_state).

%%----------------------------------------------------------------------
%%  the chf_nrf_connection_fsm gen_statem callbacks
%%----------------------------------------------------------------------

-spec callback_mode() -> Result
	when
		Result :: gen_statem:callback_mode_result().
%% @doc Set the callback mode of the callback module.
%% @see //stdlib/gen_statem:callback_mode/0
%% @private
%%
callback_mode() ->
	[state_functions, state_enter].

-spec init(Args) -> Result
	when
		Args :: [term()],
		Result :: {ok, State, Data} | {ok, State, Data, Actions}
				| ignore | {stop, Reason},
		State :: state(),
		Data :: statedata(),
		Actions :: Action | [Action],
		Action :: gen_statem:action(),
		Reason :: term().
%% @doc Initialize the {@module} finite state machine.
%%
%% 	Initialize a Diameter Service instance.
%%
%% @see //stdlib/gen_statem:init/1
%% @private
init([Host, Port] = _Args) ->
	init1(Host, Port, #{});
init([Host, Port, Opts] = _Args) ->
	init1(Host, Port, Opts).
%% @hidden
init1(Host, Port, Opts) ->
	process_flag(trap_exit, true),
	Data = #{host => Host, port => Port, opts => Opts},
	{ok, down, Data}.

-spec down(EventType, EventContent, Data) -> Result
	when
		EventType :: gen_statem:event_type(),
		EventContent :: term(),
		Data :: statedata(),
		Result :: gen_statem:event_handler_result(state()).
%% @doc Handles events received in the <em>down</em> state.
%% @private
down(enter = _EventType, down = _EventContent,
		#{host := Host, port := Port, opts := Opts} = Data) ->
	case gun:open(Host, Port, Opts) of
		{ok, ConnPid} ->
			link(ConnPid),
			{keep_state, Data#{pid => ConnPid}};
		{error, Reason} ->
			{stop, Reason}
	end;
down(enter, _EventContent, #{pid:= ConnPid} = Data) ->
	pg:leave(chf, nrf, ConnPid),
	{keep_state, maps:remove(pid, Data)};
down(info, {gun_up, ConnPid, Protocol},
		#{host := Host, port := Port} = Data) ->
	Endpoint = endpoint(Host, Port),
	?LOG_INFO(#{?MODULE => up, Protocol => Endpoint}),
	{next_state, up, Data#{pid => ConnPid, protocol => Protocol}};
down(info, {gun_error, ConnPid, Reason},
		#{pid := ConnPid, host := Host, port := Port,
		opts := Opts} = Data) ->
	Endpoint = endpoint(Host, Port),
	?LOG_ERROR(#{?MODULE => error, host => Endpoint, port => Port,
			opts => Opts, error => Reason}),
   {stop, Reason, maps:remove(ConnPid, Data)};
down(info, {gun_error, ConnPid, _StreamRef, Reason},
		#{pid := ConnPid, host := Host, port := Port,
		opts := Opts} = Data) ->
	Endpoint = endpoint(Host, Port),
	?LOG_ERROR(#{?MODULE => error, host => Endpoint, port => Port,
			opts => Opts, error => Reason}),
   {stop, Reason, maps:remove(ConnPid, Data)};
down({call, From}, get_state, Data) ->
   {keep_state_and_data, {reply, From, Data}};
down(info, {'EXIT', ConnPid, Reason}, #{pid := ConnPid} = Data) ->
   {stop, Reason, maps:remove(ConnPid, Data)}.

-spec up(EventType, EventContent, Data) -> Result
	when
		EventType :: gen_statem:event_type(),
		EventContent :: term(),
		Data :: statedata(),
		Result :: gen_statem:event_handler_result(state()).
%% @doc Handles events received in the <em>up</em> state.
%% @private
up(enter = _EventType, _EventContent, #{pid := ConnPid} = _Data) ->
	pg:join(chf, nrf, ConnPid),
	keep_state_and_data;
up(info, {gun_upgrade, ConnPid, _StreamRef, Protocols, _Headers},
		#{pid := ConnPid, host := Host, port := Port,
		protocol := Protocol} = Data) ->
	Endpoint = endpoint(Host, Port),
	?LOG_INFO(#{?MODULE => upgrade, Protocol => Endpoint,
			protocols => Protocols}),
   {keep_state, Data};
up(info, {gun_tunnel_up, ConnPid, _StreamRef, Protocol},
		#{pid := ConnPid, host := Host, port := Port} = Data) ->
	Endpoint = endpoint(Host, Port),
	?LOG_INFO(#{?MODULE => tunnel_up, Protocol => Endpoint}),
   {keep_state, Data#{protocol => Protocol}};
up(info, {gun_down, ConnPid, _Protocol, closed, _KilledStreams},
		#{pid := ConnPid, host := Host, port := Port} = Data) ->
	{next_state, down, Data};
up(info, {gun_down, ConnPid, Protocol, Reason, KilledStreams},
		#{pid := ConnPid, host := Host, port := Port} = Data) ->
	Endpoint = endpoint(Host, Port),
	?LOG_INFO(#{?MODULE => down, Protocol => Endpoint,
			error => Reason, killed_streams => length(KilledStreams)}),
	{next_state, down, Data};
up(info, {gun_error, ConnPid, Reason},
		#{pid := ConnPid, host := Host, port := Port,
		opts := Opts} = Data) ->
	Endpoint = endpoint(Host, Port),
	?LOG_ERROR(#{?MODULE => error, host => Endpoint, port => Port,
			opts => Opts, error => Reason}),
   {stop, Reason, maps:remove(ConnPid, Data)};
up(info, {gun_error, ConnPid, _StreamRef, Reason},
		#{pid := ConnPid, host := Host, port := Port,
		opts := Opts} = Data) ->
	Endpoint = endpoint(Host, Port),
	?LOG_ERROR(#{?MODULE => error, host => Endpoint, port => Port,
			opts => Opts, error => Reason}),
   {stop, Reason, maps:remove(ConnPid, Data)};
up({call, From}, get_state, Data) ->
   {keep_state_and_data, {reply, From, Data}};
up(info, {'EXIT', ConnPid, Reason}, #{pid := ConnPid} = Data) ->
   {stop, Reason, maps:remove(ConnPid, Data)}.

-spec handle_event(EventType, EventContent, State, Data) -> Result
	when
		EventType :: gen_statem:event_type(),
		EventContent :: term(),
		State :: state(),
		Data :: statedata(),
		Result :: gen_statem:event_handler_result(State).
%% @doc Handles events received in any state.
%% @private
%%
handle_event(_EventType, _EventContent, _State, _Data) ->
	keep_state_and_data.

-spec terminate(Reason, State, Data) -> any()
	when
		Reason :: normal | shutdown | {shutdown, term()} | term(),
		State :: state(),
		Data ::  statedata().
%% @doc Cleanup and exit.
%% @see //stdlib/gen_statem:terminate/3
%% @private
%%
terminate(_Reason1, _StateName, #{pid := ConnPid} = _Data) ->
	gun:close(ConnPid);
terminate(_Reason1, _StateName, _Data) ->
	ok.

-spec code_change(OldVsn, OldState, OldData, Extra) -> Result
	when
		OldVsn :: Version | {down, Version},
		Version ::  term(),
		OldState :: state(),
		OldData :: statedata(),
		Extra :: term(),
		Result :: {ok, NewState, NewData} |  Reason,
		NewState :: state(),
		NewData :: statedata(),
		Reason :: term().
%% @doc Update internal state data during a release upgrade&#047;downgrade.
%% @see //stdlib/gen_statem:code_change/3
%% @private
%%
code_change(_OldVsn, OldState, OldData, _Extra) ->
	{ok, OldState, OldData}.

%%----------------------------------------------------------------------
%%  internal functions
%%----------------------------------------------------------------------

%% @hidden
endpoint(Address, Port)
		when is_tuple(Address), is_integer(Port) ->
	lists:flatten([inet:ntoa(Address),
			$:, integer_to_list(Port)]);
endpoint(Host, Port)
		when is_atom(Host), is_integer(Port) ->
	lists:flatten([atom_to_list(Host),
			$:, integer_to_list(Port)]);
endpoint(Host, Port)
		when is_list(Host), is_integer(Port) ->
	lists:flatten([Host, $:, integer_to_list(Port)]).

