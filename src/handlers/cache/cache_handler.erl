% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2010 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module(cache_handler).
-include("wf.hrl").
-export([
    init/2,
    get_cached/3,
    set_cached/3,
    clear/1,
    clear_all/0
]).

-type cache_state() :: term().

-callback init(Config :: term(), CacheState :: cache_state()) ->
    {ok, CacheState :: cache_state()}.

-callback finish(Config :: term(), CacheState :: cache_state()) ->
    {ok, CacheState :: cache_state()}.

-callback get_cached(
    Key :: term(),
    Function :: fun(),
    TTL :: infinity | integer(),
    Config :: term(),
    CacheState :: cache_state()
) ->
    {ok, Value :: any(), CacheState :: cache_state()}.

-callback set_cached(
    Key :: term(),
    Value :: term(),
    TTL :: infinity | integer(),
    Config :: term(),
    CacheState :: cache_state()
) ->
    {ok, CacheState :: cache_state()}.

-callback clear(
    Key :: term(),
    Config :: term(),
    CacheState :: cache_state()
) ->
    {ok, CacheState :: cache_state()}.

-callback clear_all(Config :: term(), CacheState :: cache_state()) ->
    {ok, CacheState :: cache_state()}.

-spec init(Config :: term(), CacheState :: cache_state()) ->
    {ok, CacheState :: cache_state()}.
init(Config, InitialCacheState) ->
    %trace_utils:debug_fmt( "Initialising cache handler from configuration ~p "
    %	"and initial cache state ~p.", [ Config, InitialCacheState ] ),

    {ok, InitialCacheState}.

% Returns the cache value associated with Key. If it is not found, then run the
% Function, store the resulting value in cache under Key, and return the value.
%
-spec get_cached(
    Key :: any(),
    Function :: function(),
    TTL :: integer() | undefined | infinity
) -> {ok, term()}.
get_cached(Key, Function, TTL) ->
    ContextedFun = fun() ->
        %% capture existing caching state before evaluating
        Caching = wf_context:caching(),

        %trace_utils:debug_fmt( "Caching: ~p", [ Caching ] ),

        % Let us know we are caching:
        wf_context:caching(true),

        %% Process the caching function
        Result = Function(),

        % Restores the caching status to its previous state (and if the state is
        % unchanged, save some cycles by skipping it):
        %
        ?WF_IF(Caching, wf_context:caching(Caching)),

        %% Return the result
        Result
    end,

    P =
        {ok, Value} = wf_handler:call(
            cache_handler,
            get_cached,
            [Key, ContextedFun, TTL]
        ),

    %trace_utils:debug_fmt( "[~w] Getting cached value for key '~p': '~p'.",
    %					   [ self(), Key, Value ] ),

    P.

% Set the cached value directly without checking if it currently exists. If a
% value already exists for that key, it is replaced.
set_cached(Key, Value, TTL) ->
    %trace_utils:debug_fmt( "Setting cached value for key '~p' to ~p",
    %					   [ Key,Value ] ),

    ok = wf_handler:call(cache_handler, set_cached, [Key, Value, TTL]).

% @doc Remove a value from cache.
-spec clear(Key :: any()) -> ok.
clear(Key) ->
    ok = wf_handler:call(cache_handler, clear, [Key]).

% @doc Clear all values from cache.
-spec clear_all() -> ok.
clear_all() ->
    ok = wf_handler:call(cache_handler, clear_all).
