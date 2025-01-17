<!-- dash: Handlers - Route | Guide | ###:Section -->



## Route Handler

  The route handler determines how requests are handled, either by directing a
  request to a module, or marking a request to be handled by the static files,
  or by marking a request as 404 Not Found.

  The default route handler (`dynamic_route_handler.erl`) also attempts to load
  modules for pages if the modules need to be loaded by the Erlang Virtual
  Machine.

### Behavior Functions

##### `init(Config, State)`

  Initialize the request.  Unlike most handlers, this is basically where most
  of the work of the handler is done.  It should read the path from the request
  bridge, and eventually call `wf_context:page_module(Module)` to tell Nitrogen
  which module should be handling the request, and in most cases, it should
  also be calling `wf_context:path_info(Pathinfo)` to set the path_info so that
  pages can refer to the path.

  It's not exactly the "functional style" because it's highly dependent on side
  effects in the code (the `wf_context` calls).

 *  /Return Value/ - `{ok, NewState}`

##### `finish(Config, State)`

  Clean up the handler

 *  /Return Value/ - `{ok, NewState}`

### Example

Here is the complete text of the simple passthrough route handler
(found in handlers/passthrough_route_handler.erl)

```erlang
-module (passthrough_route_handler).
-behaviour (route_handler).
-include_lib ("wf.hrl").
-export ([
	init/2,
	finish/2
]).

init(Module, State) ->
	% Some values...
	RequestBridge = wf_context:request_bridge(),
	Path = RequestBridge:path(),

	% Update the page_context with the path and module.
	wf_context:page_module(Module),
	wf_context:path_info(Path),
	{ok, State}.

finish(_Config, State) ->
	{ok, State}.

```

### A More Complete Handler

   Here is the complete text of the default route handler for (as of Nitrogen
   2.2) (found in `handlers/dynamic_route_handler.erl`). To help direct, I've
   kept the comments in the code explaining it along the way.

```erlang

-module (dynamic_route_handler).
-behaviour (route_handler).
-include_lib ("wf.hrl").
-export ([
	init/2,
	finish/2
]).

%% @doc
%% dynamic_route_handler looks at the requested path and file extension
%% to determine how a request should be served. If the request path has no
%% extension, then it assumes this request should be handled by a Nitrogen
%% page module that matches the path with slashes converted to underscores.
%% If no module is found, then it will chop off the last part of the path
%% (storing it for later access in wf:path_info/0) and try again, repeating
%% until either a module is found, or there are no more parts to chop. If
%% a module still can't be found, then the web_404 module is used if defined
%% by the user, otherwise a 404 is generated internally.
%%
%% Requests for "/" are automatically sent to index.
%%
%% If the request path does have an extension, then it is treated like a request
%% for a static file. This is delegated back to the HTTP server.

init(_Config, State) ->
	% Get the path...
	RequestBridge = wf_context:request_bridge(),
	Path = RequestBridge:path(),

	% Convert the path to a module. If there are no routes defined, then just
	% convert everything without an extension to a module.
	% Otherwise, look through all routes for the first matching route.
	{Module, PathInfo} = route(Path),
	{Module1, PathInfo1} = check_for_404(Module, PathInfo, Path),

	wf_context:page_module(Module1),
	wf_context:path_info(PathInfo1),

	{ok, State}.

finish(_Config, State) ->
	{ok, State}.

%%% PRIVATE FUNCTIONS %%%

% First, check if this is a request for the root path. If so
% then just send it to index.
% Check if there is an extension, if so, it's static.
% Otherwise, try to load a module according to each part of the path.
% First, cycle through code:all_loaded(). If not there, then check erl_prim_loader:get_file()
% If still not there, then 404.
route("/") ->
	{list_to_atom(module_name(["index"])), []};

route(Path) ->
	IsStatic = (filename:extension(Path) /= []),
	case IsStatic of
		true ->
			% Serve this up as a static file.
			{static_file, Path};

		false ->
			Path1 = string:strip(Path, both, $/),
			Tokens = string:tokens(Path1, "/"),
			% Check for a loaded module. If not found, then try to load it.
			case try_load_module(Tokens) of
				{Module, PathInfo} ->
					{Module, PathInfo};
				undefined ->
					{web_404, Path1}
			end
	end.

module_name(Tokens) ->
	ModulePrefix = wf:config_default(module_prefix, ""),
		AllTokens = case ModulePrefix of
			"" -> Tokens;
			_ -> [ ModulePrefix | Tokens ]
		end,
		_ModuleName = string:join(AllTokens, "_").

try_load_module(Tokens) -> try_load_module(Tokens, []).
try_load_module([], _ExtraTokens) -> undefined;
try_load_module(Tokens, ExtraTokens) ->
	%% Get the module name...
	ModuleName = module_name(Tokens),
	Module = try
		list_to_existing_atom(ModuleName)
	catch _:_ ->
		case erl_prim_loader:get_file(ModuleName ++ ".beam") of
			{ok, _, _} -> list_to_atom(ModuleName);
			_ -> list_to_atom("$not_found")
		end
	end,

	%% Load the module, check if it exports the right method...
	code:ensure_loaded(Module),
	case erlang:function_exported(Module, main, 0) of
		true ->
			PathInfo = string:join(ExtraTokens, "/"),
			{Module, PathInfo};
		false ->
			next_try_load_module(Tokens, ExtraTokens)
	end.


next_try_load_module(Tokens, ExtraTokens) ->
	Tokens1 = lists:reverse(tl(lists:reverse(Tokens))),
	ExtraTokens1 = [hd(lists:reverse(Tokens))|ExtraTokens],
	try_load_module(Tokens1, ExtraTokens1).

check_for_404(static_file, _PathInfo, Path) ->
	{static_file, Path};

check_for_404(Module, PathInfo, Path) ->
	% Make sure the requested module is loaded. If it
	% is not, then try to load the web_404 page. If that
	% is not available, then default to the 'file_not_found_page' module.
	case code:ensure_loaded(Module) of
		{module, Module} -> {Module, PathInfo};
		_ ->
			case code:ensure_loaded(web_404) of
				{module, web_404} -> {web_404, Path};
				_ -> {file_not_found_page, Path}
			end
	end.


```

### Smart Extensions

   As of Nitrogen 2.3, Nitrogen's dynamic route handler supports Smart
   Extensions.  You can read all about them in the
   [Smart Extensions documentation](smart_extensions.md).

### RESTFul Handler

   As of Nitrogen 2.3, the dynamic route handler also supports the use of a
   RESTful module.  You can read about RESTFul Modules in the
   [REST API documentation](rest.md).

### See Also

 *  [Handler Overview](./handlers.md)

 *  [Web Request and Response](./api.md) - Scroll down a bit to the
	  `wf:page_info` options.
