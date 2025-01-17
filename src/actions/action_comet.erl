% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2010 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module(action_comet).
-include("wf.hrl").
-compile(export_all).
-define(COMET_INTERVAL, 10 * 1000).
-define(ONE_SECOND, 1000).
-define(TEN_SECONDS, 10 * 1000).
-define(TWENTY_SECONDS, 20 * 1000).

%% TODO: This whole module needs to be spliy up into several gen_servers. One
%% for Guardians, one for Accumulators, and probably one for pools, too.  There
%% are so many naked bangs, it's a little hard to follow.

% Comet and polling/continuations are now handled using Nitrogen's asynchronous
% processing scheme. This allows you to fire up an asynchronous process with the
% #comet { fun=AsyncFunction } action.
%
% TERMINOLOGY:

% AsyncFunction - An Erlang function that executes in the background. The function
% generates Actions that are then sent to the browser via the accumulator. In addition
% each AsyncFunction is part of one (and only one) pool. The pool name provides a way
% to identify previously spawned processes, much like a Pid. Messages sent to the pool
% are distributed to all AsyncFunction processes in that pool.
%
% Pool - A pool contains one or more running AsyncFunctions. Any messages sent to the pool
% are distributed to all processes within the pool. Pools can either have a local
% or global scope. Local scope means that the pool applies only to the current
% series of page requests by a user. Global means that the pool applies to
% the entire system. Global pools provide the foundation for chat applets and
% other interactive/multi-user software.
%
% Series - A series of requests to a Nitrogen resource. A series consists of
% the first request plus any postbacks by the same visitor in the same browser
% window, and any connected websockets from that initial request.
%
% Accumulator - There is one accumulator per series. The accumulator holds
% Nitrogen actions generated by AsyncFunctions, and is checked at the end
% of each Nitrogen request for anything that should be sent to the browser.
%
% AsyncGuardian - There is one AsyncGuardian for each AsyncFunction. The
% Guardian is responsible for informing the Accumulator when an AsyncFunction
% dies, and vice versa.

%%% INTERFACE %%%

%% NOTE: For comet/1, comet/2, and comet_global/2, there was a bug that would
%% cause the process to loop forever without being killed in the event that
%% wf:comet was called without javascript being loaded.  This bug is documented
%% here: https://github.com/nitrogen/nitrogen/issues/37
%%
%% The "Easy" fix would be to change those functions to start the comet call
%% inside the postback by changing function=Pid to function=F. Then we would
%% be guaranteed that the accumulator would be watching the pid and can kill it
%% on its own. But the drawback to this is that the comet function itself would
%% not start until the postback is received, meaning the function wouldn't be started
%% until javascript is loaded on the client and the postback received by the server.
%% This may be too much latency, so I've introduced a couple of hacks to make sure that
%% the pid is killed if the accumulator is never started (meaning never posted back).

%% @doc Convenience method to start a comet process.
-spec comet(comet_function()) -> {ok, pid()}.
comet(F) ->
	comet(F, default).

%% @doc Convenience method to start a comet process.
-spec comet(CometFunction :: comet_function(), Pool :: term()) -> {ok, pid()}.
comet(CometFunction, Pool) ->
	comet(CometFunction, Pool, local).

%% @doc Convenience method to start a comet process with global pool.
-spec comet_global(CometFunction :: comet_function(), Pool :: term()) -> {ok, pid()}.
comet_global(CometFunction, Pool) ->
	comet(CometFunction, Pool, global).

-spec comet(CometFunction :: comet_function(), Pool :: term(), Scope :: local | global) ->
	{ok, pid()}.
comet(CometFunction, Pool, Scope) ->
	Pid = spawn_with_context(CometFunction, undefined, page),
	wf:wire(page, page, #comet{function = Pid, pool = Pool, scope = Scope}),
	{ok, Pid}.

status(Pid) ->
	{dictionary, PD} = erlang:process_info(Pid, dictionary),
	Context = proplists:get_value(context, PD),
	PC = Context#context.page_context,
	Seriesid = PC#page_context.series_id,
	?PRINT({seriesid, Seriesid}),
	{ok, AccPid} = get_accumulator_pid_no_start(Seriesid),
	AccPid ! info.

%% @doc Gather all wired actions, and send to the accumulator.
flush() ->
	%% If we've managed to establish a websocket connection after the comet
	%% process started, this will convert the current connection to use
	%% websockets. Then instead of returning the function calls from the comet
	%% request, it'll just send the actions to the websocket and the comet
	%% request will die peacefully.
	determine_async_mode(),

	%% First, let's render actions from the action queue into a binary for easy
	%% passing around with messages. Doing this step here will ensure the
	%% integrity of the action queue processing is maintained.
	{ok, Javascript} = wf_render_actions:render_action_queue(),
	JSBin = wf:to_binary(Javascript),

	AsyncMode = wf_context:async_mode(),
	inner_flush(AsyncMode, JSBin),
	ok.

inner_flush({websocket, Pid}, JSBin) ->
	flush_websocket(Pid, JSBin);
inner_flush(_, JSBin) ->
	flush_non_websocket(JSBin).

flush_websocket(Pid, JSBin) ->
	%% If there are any latent actions from an old comet process that
	%% was upgraded to a websocket, this will clear the actions and add
	%% them to the response. It's also safe to let the websocket
	%% process render them.
	AccumulatorActions = get_actions(),

	%% And we send both the "old commands" from the accumulator, and
	%% the new ones we just processed.
	Pid ! {comet_actions, [AccumulatorActions, JSBin]}.

flush_non_websocket(JSBin) ->
	SeriesID = wf_context:series_id(),
	{ok, AccumulatorPid} = get_accumulator_pid(SeriesID),
	%% Now we send that binary to the accumulator to be pulled.
	AccumulatorPid ! {add_actions, JSBin}.

%% @doc Send a message to all processes in the specified local pool.
send(Pool, Message) ->
	inner_send(Pool, local, Message).

%% @doc Send a message to all processes in the specified global pool.
send_global(Pool, Message) ->
	inner_send(Pool, global, Message).

%%% - ACTION - %%%

render_action(Record) ->
	% If the pool is undefined, then give it a random value.
	Record1 =
		case Record#comet.pool == undefined of
			true -> Record#comet{pool = erlang:make_ref()};
			false -> Record
		end,

	% This will immediately trigger a postback to event/1 below.
	#event{
		delegate = ?MODULE,
		postback = {spawn_async_function, Record1}
	}.

% This event is called to start a Nitrogen async loop.
% In the process of starting the function, it will create
% an accumulator and a pool if they don't already exist.
event(
	{spawn_async_function, #comet{
		pool = Pool,
		scope = Scope,
		function = Function,
		dying_message = DyingMessage,
		reconnect_actions = ReconActions
	}}
) ->
	SeriesID = wf_context:series_id(),

	% Get or start the accumulator process, which is used to hold any Nitrogen
	% Actions that are generated by async processes.
	{ok, AccumulatorPid} = get_accumulator_pid(SeriesID),

	% Get or start the pool process, which is a distributor that sends Erlang
	% messages to the running async function.
	{ok, PoolPid} = get_pool_pid(SeriesID, Pool, Scope),

	% Create a process for the AsyncFunction...
	FunctionPid = create_comet_process(Function, ReconActions),

	% Create a process for the AsyncGuardian...
	GuardianFun = fun() ->
		guardian_process(FunctionPid, AccumulatorPid, PoolPid, DyingMessage)
	end,
	GuardianPid = erlang:spawn(GuardianFun),

	% Register the function with the accumulator and the pool.
	AccumulatorPid ! {add_guardian, GuardianPid},
	PoolPid ! {add_process, FunctionPid},

	% Only start the async event loop if it has not already been started...
	Actions = [
		<<"if (!document.comet_started) { document.comet_started=true; ">>,
		make_async_event(0),
		<<" }">>
	],
	wf:wire(page, page, Actions);
% This clause is the heart of async functions. It is first triggered by the
% event/1 function above, and then continues to trigger itself in a loop, but
% in different ways depending on whether the page is doing comet-based or
% polling-based background updates.
%
% To update the page, the function gathers actions in the accumulator and wires
% both the actions and the looping event.
event(start_async) ->
	SeriesID = wf_context:series_id(),
	case get_accumulator_pid_no_start(SeriesID) of
		undefined ->
			wf:wire(
				"document.comet_started=false; Nitrogen.$set_disconnected(true); Nitrogen.$reconnect_system();"
			);
		{ok, _Pid} ->
			case effective_async_mode() of
				{websocket, Pid} ->
					start_async_websocket(Pid);
				comet ->
					start_async_comet();
				{poll, Interval} ->
					start_async_polling(Interval)
			end
	end;
event({maybe_reconnect, ReconTag, ReconActions}) ->
	Pid = get_process_tag_pid(ReconTag),
	maybe_reconnect(Pid, ReconTag, ReconActions).

maybe_reconnect(Pid, ReconTag, ReconActions) ->
	case is_pid(Pid) andalso is_process_alive(Pid) of
		true ->
			%% Our comet process is still alive, so we don't even need to run the
			%% "reconnection" events, we just renew the lease, and hijack the comet
			%% process to use the new connection.
			set_lease(?COMET_INTERVAL + ?TEN_SECONDS),

			%% We also need to requeue up the reconnection postback, since each
			%% reconnection attempt is only run a single time.
			wire_reconnection(ReconTag, ReconActions);
		false ->
			%% Our comet process is dead, so let's run our reconnection event
			%% (which may restart it, or which may just say something like
			%% "Your process died due to inactivity")
			?WF_IF(ReconActions, wf:wire(ReconActions))
	end.

start_async_websocket(Pid) ->
	%% Because we're using websocket, we don't have to do as much.
	%% We can get away with calling an async_event and sending it to
	%% the browser to serve as a ping and also to make sure the
	%% connection isn't hung.
	Event = make_async_event(?TEN_SECONDS),
	Pid ! {comet_actions, Event},
	register_websocket_pid_with_accumulator(Pid),
	set_lease(?COMET_INTERVAL + ?TEN_SECONDS).

start_async_comet() ->
	% Tell the accumulator to stay alive until
	% we call back, with some padding...
	set_lease(?COMET_INTERVAL + ?TEN_SECONDS),

	% Start the polling postback...
	Actions = get_actions_blocking(?COMET_INTERVAL),
	Event = make_async_event(0),
	wf:wire(page, page, [Actions, Event]),

	% Renew the lease, because the blocking call
	% could have used up a significant amount of time.
	set_lease(?COMET_INTERVAL + ?TEN_SECONDS).

start_async_polling(Interval) ->
	% Tell the accumulator to stay alive until
	% we call back, with some padding.
	set_lease(Interval + ?TEN_SECONDS),

	% Start the polling postback...
	Actions = get_actions(),
	Event = make_async_event(Interval),
	wf:wire(page, page, [Actions, Event]).

create_comet_process(Pid, _) when is_pid(Pid) ->
	notify_accumulator_checker(Pid),
	Pid;
create_comet_process(Function, ReconActions) ->
	Pid = spawn_with_context(Function, ReconActions, postback),
	notify_accumulator_checker(Pid),
	Pid.

determine_async_mode() ->
	%% Reconnections happen, and as such, we need to make sure our reconnected
	%% websocket is properly updated.
	case get_websocket_pid_from_accumulator() of
		undefined ->
			ok;
		{ok, Pid} ->
			case is_process_alive(Pid) of
				true ->
					wf_context:async_mode({websocket, Pid});
				false ->
					wf_context:async_mode(comet)
			end
	end.

effective_async_mode() ->
	case wf_context:async_mode() of
		{websocket, Pid} ->
			case is_process_alive(Pid) of
				true -> {websocket, Pid};
				false -> comet
			end;
		Other ->
			Other
	end.

%% - POOL - %%

% Retrieve a Pid from the process_registry for the specified pool.
% A pool can either be local or global. In a local pool, messages sent
% to the pool are only sent to async processes for one browser window.
% In a global pool, messages sent to the pool are sent to all processes
% in the pool across the entire system. This is useful for multi-user applications.
get_pool_pid(SeriesID, Pool, Scope) ->
	PoolID =
		case Scope of
			local -> {Pool, SeriesID};
			global -> {Pool, global}
		end,
	PoolFun = fun() -> pool_loop([]) end,
	{ok, _Pid} = process_registry_handler:get_pid({async_pool, PoolID}, PoolFun).

% The pool loop keeps track of the AsyncFunction processes in a pool,
% and is responsible for distributing messages to all processes in the pool.
pool_loop(Processes) ->
	receive
		{add_process, JoinPid} ->
			case lists:member(JoinPid, Processes) of
				true ->
					pool_loop(Processes);
				false ->
					erlang:monitor(process, JoinPid),
					case Processes of
						[] -> JoinPid ! 'INIT';
						_ -> [Pid ! {'JOIN', JoinPid} || Pid <- Processes]
					end,
					pool_loop([JoinPid | Processes])
			end;
		{'DOWN', _, process, LeavePid, _} ->
			[Pid ! {'LEAVE', LeavePid} || Pid <- Processes],
			NewProcesses = Processes -- [LeavePid],
			case NewProcesses == [] of
				false -> pool_loop(NewProcesses);
				true -> erlang:exit({pool_loop, exiting_empty_pool})
			end;
		Message ->
			[Pid ! Message || Pid <- Processes],
			pool_loop(Processes)
	after ?TWENTY_SECONDS ->
		NewProcesses = [X || X <- Processes, is_remote_process_alive(X)],
		case NewProcesses == [] of
			true -> erlang:exit({pool_loop, exiting_empty_pool});
			false -> pool_loop(NewProcesses)
		end
	end.

%% - ACCUMULATOR - %%

% Retrieve a Pid from the process registry for the specified Series.
get_accumulator_pid(SeriesID) ->
	AccumulatorFun = fun() -> accumulator_loop([], [], none, undefined, undefined, []) end,
	Key = {async_accumulator, SeriesID},
	{ok, _Pid} = process_registry_handler:get_pid(Key, AccumulatorFun).

get_accumulator_pid_no_start(SeriesID) ->
	Key = {async_accumulator, SeriesID},
	case process_registry_handler:get_pid(Key) of
		{ok, Pid} when is_pid(Pid) -> {ok, Pid};
		undefined -> undefined
	end.

% The accumulator_loop keeps track of guardian processes within a pool,
% and gathers actions from the various AsyncFunctions in order
% to send it the page when the actions are requested.
accumulator_loop(Guardians, Actions, Waiting, TimerRef, WebsocketPid, ProcessTags) ->
	receive
		{add_guardian, Pid} ->
			erlang:monitor(process, Pid),
			accumulator_loop(
				[Pid | Guardians], Actions, Waiting, TimerRef, WebsocketPid, ProcessTags
			);
		{'DOWN', _, process, Pid, _} ->
			accumulator_loop(
				Guardians -- [Pid], Actions, Waiting, TimerRef, WebsocketPid, ProcessTags
			);
		{add_actions, NewActions} ->
			case is_remote_process_alive(Waiting) of
				true ->
					Waiting ! {actions, [NewActions | Actions]},
					accumulator_loop(Guardians, [], none, TimerRef, WebsocketPid, ProcessTags);
				false ->
					accumulator_loop(
						Guardians, [NewActions | Actions], none, TimerRef, WebsocketPid, ProcessTags
					)
			end;
		{get_actions_blocking, Pid} when Actions == [] ->
			accumulator_loop(Guardians, [], Pid, TimerRef, WebsocketPid, ProcessTags);
		{get_actions_blocking, Pid} when Actions /= [] ->
			Pid ! {actions, lists:reverse(Actions)},
			accumulator_loop(Guardians, [], none, TimerRef, WebsocketPid, ProcessTags);
		{get_actions, Pid} ->
			Pid ! {actions, lists:reverse(Actions)},
			accumulator_loop(Guardians, [], none, TimerRef, WebsocketPid, ProcessTags);
		{set_lease, LengthInMS} ->
			timer:cancel(TimerRef),
			{ok, NewTimerRef} = timer:send_after(LengthInMS, {maybe_die, LengthInMS}),
			accumulator_loop(Guardians, Actions, Waiting, NewTimerRef, WebsocketPid, ProcessTags);
		{websocket_pid, NewWebsocketPid} ->
			accumulator_loop(Guardians, Actions, Waiting, TimerRef, NewWebsocketPid, ProcessTags);
		{get_websocket_pid, Pid} ->
			Pid ! {websocket_pid, WebsocketPid},
			accumulator_loop(Guardians, Actions, Waiting, TimerRef, WebsocketPid, ProcessTags);
		{add_process_tag, ReconTag, FunPid} ->
			NewProcessTags0 = proplists:delete(ReconTag, ProcessTags),
			NewProcessTags = [{ReconTag, FunPid} | NewProcessTags0],
			accumulator_loop(Guardians, Actions, Waiting, TimerRef, WebsocketPid, NewProcessTags);
		{get_process_tag_pid, Pid, ReconTag} ->
			TaggedPid =
				case lists:keyfind(ReconTag, 1, ProcessTags) of
					false -> undefined;
					{_, P} -> P
				end,
			Pid ! {process_tag_pid, TaggedPid},
			accumulator_loop(Guardians, Actions, Waiting, TimerRef, WebsocketPid, ProcessTags);
		{maybe_die, LengthInMS} ->
			%% If we're using websockets and the websocket is alive, let's not
			%% die, instead, we just renew the lease again. This allows us to
			%% verify that the connection is still alive internally in order to
			%% kill the guardian process.
			%% If we're not using websockets, or the websocket pid is dead,
			%% then die.
			case is_pid(WebsocketPid) andalso erlang:is_process_alive(WebsocketPid) of
				true ->
					{ok, NewTimerRef} = timer:send_after(LengthInMS, {maybe_die, LengthInMS}),
					accumulator_loop(
						Guardians, Actions, Waiting, NewTimerRef, WebsocketPid, ProcessTags
					);
				false ->
					% Guardian_process will detect that we've died and update
					% the pool.
					maybe_kill_waiting(Waiting),
					erlang:exit({accumulator_loop, exiting_lease_expired})
			end;
		info ->
			Status = [
				{guardians, Guardians},
				{actions, Actions},
				{timer_ref, TimerRef},
				{websocket_pid, WebsocketPid},
				{process_tags, ProcessTags}
			],
			?PRINT({accumulator_status, Status}),
			accumulator_loop(Guardians, Actions, Waiting, TimerRef, WebsocketPid, ProcessTags);
		Other ->
			?PRINT({accumulator_loop, unhandled_event, Other}),
			accumulator_loop(Guardians, Actions, Waiting, TimerRef, WebsocketPid, ProcessTags)
	after ?TWENTY_SECONDS ->
		%% If we have no TimerRef, then the browser never performed
		%% the callback, so kill this process after 20 seconds.
		case TimerRef == undefined of
			true ->
				maybe_kill_waiting(Waiting),
				erlang:exit({accumulator_loop, timeout});
			false ->
				accumulator_loop(Guardians, Actions, Waiting, TimerRef, WebsocketPid, ProcessTags)
		end
	end.

maybe_kill_waiting(Pid) when is_pid(Pid) ->
	Pid ! killed_when_accumulator_died;
maybe_kill_waiting(_) ->
	ok.

% The guardian process monitors the running AsyncFunction and
% the running Accumulator. If either one dies, then send
% DyingMessage to the pool, and end.
guardian_process(FunctionPid, AccumulatorPid, PoolPid, DyingMessage) ->
	erlang:monitor(process, FunctionPid),
	erlang:monitor(process, AccumulatorPid),
	erlang:monitor(process, PoolPid),
	receive
		{'DOWN', _, process, FunctionPid, _} ->
			% The AsyncFunction process has died.
			% Communicate dying_message to the pool and exit.
			case DyingMessage of
				undefined -> ignore;
				_ -> PoolPid ! DyingMessage
			end,
			exit(async_function_died);
		{'DOWN', _, process, AccumulatorPid, _} ->
			% The accumulator process has died.
			% Communicate dying_message to the pool,
			% kill the AsyncFunction process, and exit.
			case DyingMessage of
				undefined -> ignore;
				_ -> PoolPid ! DyingMessage
			end,
			erlang:exit(FunctionPid, async_die),
			erlang:exit({guardian_process, exiting_accumulator_died});
		{'DOWN', _, process, PoolPid, Info} ->
			% The pool should never die on us.
			?PRINT({unexpected_pool_death, Info}),
			erlang:exit({guardian_process, exiting_pool_died});
		Other ->
			?PRINT({FunctionPid, AccumulatorPid, PoolPid}),
			?PRINT({guardian_process, unhandled_event, Other}),
			guardian_process(FunctionPid, AccumulatorPid, PoolPid, DyingMessage)
	end.

%%% PRIVATE FUNCTIONS %%%

%% @doc Mode can be either atoms 'page' or 'postback'.
%% page means it's spawned from wf:comet(), meaning that the accumulator and whatnot
%% is not started until after the comet postback. So we have to run a checker to fix it
%% postbak means it's spawned from the postback event already along with the accumulator
%% and everything. So no need to check for it.
spawn_with_context(Function, ReconActions, Mode) when is_function(Function) ->
	spawn_with_context({undefined, Function, undefined}, ReconActions, Mode);
spawn_with_context({Name, Function}, ReconActions, Mode) ->
	spawn_with_context({Name, Function, undefined}, ReconActions, Mode);
spawn_with_context({Name, Function, Msg}, ReconActions, Mode) ->
	WrappedCometFun = create_wrapped_comet_fun(Function, Mode, ReconActions),
	Pid = start_comet_process(Name, Msg, WrappedCometFun),
	start_accumulator_check_timer(Mode, Pid),
	Pid.

create_wrapped_comet_fun(Function, Mode, ReconActions) ->
	Context = wf_context:context(),
	ReconTag = "recon" ++ wf:to_list(?WF_RAND_UNIFORM(1, 9999999999999999999999999999999999)),
	fun() ->
		wf_context:context(Context),
		wf_context:type(comet),
		wf_context:clear_action_queue(),
		register_process_tag(ReconTag, self()),
		wire_reconnection(ReconTag, ReconActions),
		flush(),
		run_comet_fun_maybe_with_mode(Function, Mode),
		wire_cancel_reconnection(ReconTag),
		flush()
	end.

wire_reconnection(ReconTag, ReconActions) ->
	Postback = {maybe_reconnect, ReconTag, ReconActions},
	Actions = #event{postback = Postback, delegate = ?MODULE},
	Action = [
		<<"Nitrogen.$register_system_reconnection_event('">>,
		ReconTag,
		<<"', function() {">>,
		Actions,
		<<"}); ">>
	],
	wf:wire(page, page, Action).

wire_cancel_reconnection(undefined) ->
	ok;
wire_cancel_reconnection(ReconTag) ->
	Action = [<<"Nitrogen.$cancel_system_reconnection_event('">>, ReconTag, <<"');">>],
	wf:defer(page, page, Action).

start_comet_process(undefined, _, WrappedCometFun) ->
	erlang:spawn(WrappedCometFun);
start_comet_process(Name, Msg, WrappedCometFun) ->
	SeriesID = wf_context:series_id(),
	Key = {SeriesID, Name},
	{ok, Pid} = process_registry_handler:get_pid(Key, WrappedCometFun),
	maybe_send_reconnection_message(Pid, Msg),
	Pid.

run_comet_fun_maybe_with_mode(Function, Mode) ->
	case erlang:fun_info(Function, arity) of
		{arity, 1} -> Function(Mode);
		{arity, 0} -> Function()
	end.

%% When the accumulator
maybe_send_reconnection_message(_Pid, undefined) ->
	ok;
maybe_send_reconnection_message(Pid, Msg) ->
	Pid ! Msg.

start_accumulator_check_timer(page, Pid) ->
	%% This function gets called before we've verified that the browser
	%% is even doing JS, and without JS, this will never get assigned
	%% an accumulator.  So here's a hack to make sure that the accumulator
	%% exists after 20 seconds, and if not, it simply kills the pid
	start_accumulator_check_timer(Pid);
start_accumulator_check_timer(_, _) ->
	do_nothing.

%% If P is a pid, then it was initiated on the page
%% and that means we have to kill the checker pid that's
%% waiting to kill it for us.  Hacky, I know. Real
%% spaghetti-like. I don't particularly like it, but it
%% fixes the bug.
notify_accumulator_checker(Pid) ->
	case get_accumulator_checker_pid(Pid) of
		{ok, CheckerPid} when is_pid(CheckerPid) ->
			CheckerPid ! accumulator_started;
		_ ->
			ok
	end.

get_accumulator_checker_pid(Pid) ->
	Key = get_accumulator_checker_key(Pid),
	process_registry_handler:get_pid(Key).

get_accumulator_checker_key(Pid) ->
	_Key = {accumulator_check_timer, Pid}.

start_accumulator_check_timer(Pid) ->
	Key = get_accumulator_checker_key(Pid),
	CheckFunction = fun() ->
		receive
			accumulator_started -> ok
		after ?TWENTY_SECONDS + ?TEN_SECONDS ->
			%% The page never posted back at all (Hey, we gave it 30 seconds),
			%% and so the accumulator loop doesn't exist for this pid, and so
			%% we have nothing to check for it. So let's just kill the pid
			exit(Pid, accumulator_never_started)
		end
	end,

	{ok, _CheckPid} = process_registry_handler:get_pid(Key, CheckFunction).

inner_send(Pool, Scope, Message) ->
	% ?PRINT({Pool, Scope, Message}),
	SeriesID = wf_context:series_id(),
	{ok, PoolPid} = get_pool_pid(SeriesID, Pool, Scope),
	PoolPid ! Message,
	ok.

% Get actions from accumulator. If there are no actions currently in the
% accumulator, then [] is immediately returned.
get_actions() ->
	SeriesID = wf_context:series_id(),
	{ok, Pid} = get_accumulator_pid(SeriesID),
	get_actions_from_accumulator(Pid).

get_actions_from_accumulator(AccumulatorPid) ->
	AccumulatorPid ! {get_actions, self()},
	receive
		{actions, X} -> X
	after 100 ->
		[]
	end.

get_actions_and_register_new_websocket_pid(WebsocketPid) ->
	SeriesID = wf_context:series_id(),
	case get_accumulator_pid_no_start(SeriesID) of
		undefined ->
			[];
		{ok, Pid} ->
			Pid ! {websocket_pid, WebsocketPid},
			get_actions_from_accumulator(Pid)
	end.

register_process_tag(ReconTag, Pid) ->
	SeriesID = wf_context:series_id(),
	{ok, AccPid} = get_accumulator_pid(SeriesID),
	AccPid ! {add_process_tag, ReconTag, Pid}.

% Get actions from accumulator in a blocking fashion. If there are no actions
% currently in the accumulator, then this blocks for up to Timeout milliseconds.
% This works by telling Erlang to send a dummy 'add_actions' command to the accumulator
% that will be executed when the timeout expires.
get_actions_blocking(Timeout) ->
	SeriesID = wf_context:series_id(),
	{ok, AccumulatorPid} = get_accumulator_pid(SeriesID),
	AccumulatorNode = node(AccumulatorPid),
	TimerRef = rpc:call(AccumulatorNode, erlang, send_after, [
		Timeout, AccumulatorPid, {add_actions, []}
	]),
	HardTimeout = Timeout + 10000,

	AccumulatorPid ! {get_actions_blocking, self()},
	receive
		{actions, X} ->
			erlang:cancel_timer(TimerRef),
			X;
		Other ->
			?PRINT({unhandled_event, Other}),
			[]
	after HardTimeout ->
		%?PRINT({no_comet_actions_checking_pid, AccumulatorPid}),
		case erlang:is_process_alive(AccumulatorPid) of
			true ->
				%?PRINT({is_alive_relooping}),
				erlang:cancel_timer(TimerRef),
				get_actions_blocking(Timeout);
			false ->
				erlang:cancel_timer(TimerRef),
				?PRINT({accumulator_is_dead, AccumulatorPid}),
				[]
		end
	end.

set_lease(LengthInMS) ->
	SeriesID = wf_context:series_id(),
	{ok, AccumulatorPid} = get_accumulator_pid(SeriesID),
	AccumulatorPid ! {set_lease, LengthInMS}.

register_websocket_pid_with_accumulator(WebsocketPid) ->
	SeriesID = wf_context:series_id(),
	{ok, AccumulatorPid} = get_accumulator_pid(SeriesID),
	AccumulatorPid ! {websocket_pid, WebsocketPid}.

get_websocket_pid_from_accumulator() ->
	SeriesID = wf_context:series_id(),
	{ok, AccumulatorPid} = get_accumulator_pid(SeriesID),
	AccumulatorPid ! {get_websocket_pid, self()},
	receive
		{websocket_pid, undefined} -> undefined;
		{websocket_pid, WebsocketPid} -> {ok, WebsocketPid}
	after 10 -> undefined
	end.

get_process_tag_pid(ReconTag) ->
	SeriesID = wf_context:series_id(),
	case get_accumulator_pid_no_start(SeriesID) of
		undefined ->
			undefined;
		{ok, AccPid} ->
			AccPid ! {get_process_tag_pid, self(), ReconTag},
			receive
				{process_tag_pid, Pid} -> Pid
			after 10 ->
				undefined
			end
	end.

% Convenience function to return an #event that will call event(start_async) above.
make_async_event(Interval) ->
	#event{type = system, delay = Interval, delegate = ?MODULE, postback = start_async}.

% Return true if the process is alive, accounting for processes on other nodes.
is_remote_process_alive(Pid) ->
	is_pid(Pid) andalso
		pong == net_adm:ping(node(Pid)) andalso
		rpc:call(node(Pid), erlang, is_process_alive, [Pid]).
