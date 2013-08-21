% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% See MIT-LICENSE for licensing information.

-module (element_time).
-include("wf.hrl").
-export([
    reflect/0,
    render_element/1
]).

-spec reflect() -> [atom()].
reflect() -> record_info(fields, time).

-spec render_element(#time{}) -> body().
render_element(Record) ->
    wf_tags:emit_tag(time, Record#time.body, [
        {id, Record#time.html_id},
        {class, ["time", Record#time.class]},
        {style, Record#time.style},
        {data_fields, Record#time.data_fields},
        ?WF_IF(Record#time.pubdate, pubdate),
        ?WF_IF(Record#time.datetime, {datetime, Record#time.datetime})
    ]).
