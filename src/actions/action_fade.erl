% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2010 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module(action_fade).
-include("wf.hrl").
-export([render_action/1]).

render_action(Record) ->
	#jquery_effect{
		type = fade,
		speed = Record#fade.speed,
		actions = Record#fade.actions
	}.
