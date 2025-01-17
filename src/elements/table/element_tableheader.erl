% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2010 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module(element_tableheader).
-include("wf.hrl").
-export([
	reflect/0,
	render_element/1
]).

-spec reflect() -> [atom()].
reflect() -> record_info(fields, tableheader).

-spec render_element(#tableheader{}) -> body().
render_element(Record) ->
	Body = [
		wf:html_encode(Record#tableheader.text, Record#tableheader.html_encode),
		Record#tableheader.body
	],

	wf_tags:emit_tag(th, Body, [
		{id, Record#tableheader.html_id},
		{class, [tableheader, Record#tableheader.class]},
		{title, Record#tableheader.title},
		{style, Record#tableheader.style},
		{align, Record#tableheader.align},
		{valign, Record#tableheader.valign},
		{colspan, Record#tableheader.colspan},
		{rowspan, Record#tableheader.rowspan},
		{data_fields, Record#tableheader.data_fields}
	]).
