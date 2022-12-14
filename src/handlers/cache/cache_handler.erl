% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2010 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module(cache_handler).
-include("wf.hrl").
-export([
	get_cached/3,
	set_cached/3,
	clear/1,
	clear_all/0
]).

-callback init(
	Config :: term(),
	State :: term()
) -> {ok, State :: term()}.
-callback finish(
	Config :: term(),
	State :: term()
) -> {ok, State :: term()}.
-callback get_cached(
	Key :: term(),
	Function :: fun(),
	TTL :: infinity | integer(),
	Config :: term(),
	State :: term()
) -> {ok, Value :: any(), State :: term()}.
-callback set_cached(
	Key :: term(),
	Value :: term(),
	TTL :: infinity | integer(),
	Config :: term(),
	State :: term()
) -> {ok, State :: term()}.
-callback clear(
	Key :: term(),
	Config :: term(),
	State :: term()
) -> {ok, State :: term()}.
-callback clear_all(
	Config :: term(),
	State :: term()
) -> {ok, State :: term()}.


% @doc Returns the cache value associated with the specified key.
%
% If it is not found, then run the Function, store the resulting value in cache
% under Key, and return the value.
%
-spec get_cached( Key :: any(), Function :: function(),
				  TTL :: integer() | undefined | infinity ) -> { ok, term() }.
get_cached( Key, Function, TTL ) ->

	ContextedFun = fun() ->

		% Capture existing caching state before evaluating:
		Caching = wf_context:caching(),

		% Let us know we are caching:
		wf_context:caching(true),

		% Process the caching function:
		Result = Function(),

		% Restores the caching status to its previous state (and if the state is
		% unchanged, save some cycles by skipping it:
		%
		?WF_IF(not (Caching), wf_context:caching(Caching)),

		% Returns the result:
		Result

	end,

	{ ok, _Value } = wf_handler:call( cache_handler, get_cached,
									  [ Key, ContextedFun, TTL ] ).

	%trace_utils:debug_fmt( "[~w] Getting cached value for key '~p': '~p'.",
	%   [ self(), Key, Value ] ),



% Set the cached value directly without checking if it currently exists. If a
% value already exists for that key, it is replaced.
set_cached(Key, Value, TTL) ->
	ok = wf_handler:call(cache_handler, set_cached, [Key, Value, TTL]).

% @doc Remove a value from cache.
-spec clear(Key :: any()) -> ok.
clear(Key) ->
	ok = wf_handler:call(cache_handler, clear, [Key]).

% @doc Clear all values from cache.
-spec clear_all() -> ok.
clear_all() ->
	ok = wf_handler:call(cache_handler, clear_all).
