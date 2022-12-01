% Nitrogen Web Framework for Erlang
% Copyright (c) 2015-2020 Jesse Gumm
% See MIT-LICENSE for licensing information.
%
% This cache handler relies on nitro_cache
%

-module(default_cache_handler).

-behaviour(cache_handler).

-include("wf.hrl").


-export ([
	init/2,
	finish/2,
	get_cached/5,
	set_cached/5,
	get_full_key/2,
	clear/3,
	clear_all/2
]).



% Stores the base content root of the Nitrogen-based website corresponding to
% this cache instance:
%
-type cache_state() :: file_utils:bin_directory_path().

-export_type([ cache_state/0 ]).



init( Config, InitialCacheState ) ->

	CacheName = cache_name( Config ),

	%trace_utils:debug_fmt( "Initialising default cache handler from "
	%	"configuration '~p' and initial cache state ~p (cache name is '~ts').",
	%	[ Config, InitialCacheState, CacheName ] ),

	%basic_utils:crash(),

	try

		maybe_add_cache(CacheName)

	catch error:undef ->

		wf:error("nitro_cache not found in the system. You should add it as a dependency to your rebar.config like this:~n    ~p~n", [{nitro_cache, {git, "git://github.com/nitrogen/nitro_cache", {branch, master}}}]),

		throw({nitro_cache_not_found, "See message above"})

	end,

	{ok, InitialCacheState}.



maybe_add_cache(CacheName) ->
	case application:get_env(nitro_cache, initialized_nitrogen_caches) of
		undefined ->
			add_cache([], CacheName);
		{ok, Caches} ->
			case lists:member(CacheName, Caches) of
				true -> ok;
				false -> add_cache(Caches, CacheName)
			end
	end.

add_cache(Caches, CacheName) ->
	try nitro_cache:init(CacheName)
	catch error:badarg ->
		case nitro_cache:cache_exists(CacheName) of
			true -> ok;
			false -> throw({cannot_init_cache, CacheName})
		end
	end,
	application:set_env( nitro_cache, initialized_nitrogen_caches,
						 [ CacheName | Caches ] ).


finish(_Config, State) ->
	{ok, State}.


-spec get_cached(term(), fun(), integer() | infinity, proplist(), any()) ->
						{ok, term(), any()}.
get_cached(Key, Function, TTL, Config, State)
		when is_function(Function, 0) ->

	CacheName = cache_name(Config),

	% Integrates the content root in this key:
	FullKey = get_full_key( Key, State ),

	%trace_utils:debug_fmt( "Default cache handler looking up Key=~p, "
	%	"with Function=~p, Config=~p and State=~p",
	%	[ FullKey, Function, Config, State ] ),

	Return = nitro_cache:get(CacheName, TTL, FullKey, Function),
	{ok, Return, State}.

set_cached(Key, Value, TTL, Config, State) ->
	CacheName = cache_name(Config),
	nitro_cache:set(CacheName, TTL, Key, Value),
	{ok, State}.

clear(Key, Config, State) ->
	nitro_cache:flush(cache_name(Config), Key),
	{ok, State}.

clear_all(Config, State) ->
	nitro_cache:flush(cache_name(Config)),
	{ok, State}.

cache_name(undefined) ->
	nitrogen;
cache_name(Config) ->
	proplists:get_value(cache_name, Config, nitrogen).


% Returns the full key corresponding to specified one, i.e. integrating the
% content root.
%
 get_full_key( Key, State ) ->

	% State is simply directly the content root of interest:
	BinContentRoot = State,

	{ Key, BinContentRoot }.
