%%% chf_ct_lib.erl
%%% vim: ts=3
%%%
-module(chf_ct_lib).

-export([start/0, start/1, stop/0, stop/1]).
-export([load/1, unload/1]).
-export([rand_name/0, rand_name/1]).
-export([rand_dn/0, rand_dn/1]).

applications() ->
	[crypto, asn1, public_key, ssl, ranch, cowlib, cowboy, gun, chf].

start() ->
	start(applications()).

start(Application) when is_atom(Application) ->
	F = fun(A) when A /= Application ->
				true;
			(_) ->
				false
	end,
	start(lists:takewhile(F, applications()) ++ [Application]);
start([H | T]) ->
	case application:start(H) of
		ok  ->
			start(T);
		{error, {already_started, H}} ->
			start(T);
		{error, Reason} ->
			{error, Reason}
	end;
start([]) ->
	ok.

stop() ->
	stop(lists:reverse(applications())).

stop([H | T]) ->
	case application:stop(H) of
		ok  ->
			stop(T);
		{error, {not_started, H}} ->
			stop(T);
		{error, Reason} ->
			{error, Reason}
	end;
stop([]) ->
	ok.

load(Application) ->
	case application:load(Application) of
		ok ->
			ok;
		{error, {already_loaded, Application}} ->
			ok = unload(Application),
			load(Application);
		{error, {running, Application}} ->
			ok = application:stop(Application),
			ok = unload(Application),
			load(Application)
	end.

unload(Application) ->
	case application:unload(Application) of
		ok ->
			ok;
		{error, {running, Application}} ->
			ok = application:stop(Application),
			unload(Application);
		{error, {not_loaded, Application}} ->
			ok
	end.

%% @doc Returns 5-13 random printable characters.
rand_name() ->
	rand_name(rand:uniform(8) + 5).

%% @doc Returns N random printable characters.
rand_name(N) ->
	UpperCase = lists:seq(65, 90),
	LowerCase = lists:seq(97, 122),
	Digits = lists:seq(48, 57),
	Special = [$#, $%, $+, $-, $.],
	CharSet = lists:flatten([UpperCase, LowerCase, Digits, Special]),
	rand_name(N, CharSet, []).
rand_name(0, _CharSet, Acc) ->
	Acc;
rand_name(N, CharSet, Acc) ->
	Char = lists:nth(rand:uniform(length(CharSet)), CharSet),
	rand_name(N - 1, CharSet, [Char | Acc]).

%% @doc Returns ten random digits.
rand_dn() ->
	rand_dn(10).

%% @doc Returns N random digits.
rand_dn(N) ->
	rand_dn(N, []).
rand_dn(0, Acc) ->
	Acc;
rand_dn(N, Acc) ->
	rand_dn(N - 1, [47 + rand:uniform(10) | Acc]).

