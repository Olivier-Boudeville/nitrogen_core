<!-- dash: Handlers - Crash | Guide | ###:Section -->



## Crash Handler

  The crash handler provides a simple mechanism for dealing with page crashes
  to provide more user-friendly notifications, as well as giving the programmer
  the ability to hook whatever kind of logging they want into errors.

  The previous functionality in Nitrogen, when a page simply crashed, was to
  return "Internal Server Error" and send a 500 error to the client.  This,
  unfortunately, results in a big scary white page with a useless error message
  for the connected client, or in the case of AJAX, silently failing (buttons
  that do nothing, and the user thinks "Hey, maybe it's just running slowly",
  when in fact, it's crashed.

  The crash handler allows you to present a template to the user, wire events,
  and overall present something more useful (or at the very least, more
  friendly) to your customer (for example, presenting a formatted page saying
  "Sorry, something has gone awry, we've been notified and are looking into
  the problem").

### Behavior Functions

##### `init(Config, State)`

  Initialize the handler

 *  /Return Value/ - `{ok, NewState}`

##### `finish(Config, State)`

  Clean up the handler

 *  /Return Value/ - `{ok, NewState}`

##### `first_request(ErrorType, Error, Stacktrace, Config, State)`

  If the client's request was a "first request" (not a postback, comet,
  or websocket request), this will be called with the Error information.

 *  `ErrorType` - This is merely the error type retrieved from the class
	 portion of a `catch` statement, typically this will simply be `error`,
	 or it could be `throw` if it's a programmer-thrown error, or `exit` if the
	 process called `exit/1`

 *  `Error` - This will be the more descriptive nature of the error, for
	 example `badarg` or `{badmatch, V}`. See
	 [Errors and Error Handling Section 10.4](http://erlang.org/doc/reference_manual/errors.html)
	 for more descriptions of the error.

 *  `Stacktrace` - This will be the result of calling `erlang:stack_trace/0`
	 at the time of the error.

 *  `Config` - Any configuration associated with the crash handler.

 *  `State` - Any state information associated with the crash handler
	 (likely unused).

 *  /Return Value/ - The body to be sent to the browser.

#### `postback_request(ErrorType, Error, Stacktrace, Config, State)`

	If the client's request was a an AJAX, Comet, Websocket, or other
	non-initial request, this function will be called.

	It takes all the same arguments, but the return value is discarded, hence
	the recommendation to simply return `ok`.

	Instead, anything you wire to the browser will be sent. This could allow
	you to send a simple, useful little alert box to the user saying "We're
	sorry, there was an issue processing this request, please try again or
	reload the page". At least then the user will get **some** kind of feedback
	and not just silently failing.

### Example

  Here is the complete text of the default crash handler, which is simple, and
  for production use, is probably too simple, but it demonstrates the most basic
  form of crash handler.

```erlang
-module(default_crash_handler).
-behaviour(crash_handler).
-include("wf.hrl").
-export([
	init/2,
	finish/2,
	first_request/5,
	postback_request/5
]).

init(_Config,State) ->
	{ok, State}.

finish(_Config, State) ->
	{ok, State}.

first_request(Type, Error, Stacktrace, _Config, _State) ->
	%% Print the error message to the Erlang console
	?WF_LOG("~p~n", [{error, Type, Error, Stacktrace}]),

	%% Set the response status code to 500 (internal server error)
	wf:status_code(500),

	%% Send just the text "Internal Server Error" with no formatting or layout
	"Internal Server Error".

postback_request(Type, Error, Stacktrace, _Config, _State) ->
	%% Print the error message to the Erlang console
	?WF_LOG("~p~n", [{error, Type, Error, Stacktrace}]),

	%% Set the status code to 500 (internal server error)
	wf:status_code(500),

	%% Do nothing else
	ok.


```

  A more useful example would be a crash handler that uses a template and pops
  a message to the user when a postback is used.

```erlang
-module(simple_crash_handler).
-behaviour(crash_handler).
-include_lib("nitrogen_core/include/wf.hrl").
-export([
	init/2,
	finish/2,
	first_request/5,
	postback_request/5,
	body/1
]).

init(_Config,State) ->
	{ok, State}.

finish(_Config, State) ->
	{ok, State}.

first_request(Type, Error, Stacktrace, _Config, _State) ->
	%% Print the error message to the Erlang console
	?WF_LOG("~p~n", [{error, Type, Error, Stacktrace}]),

	%% Set the response status code to 500 (internal server error)
	wf:status_code(500),

	#template{
	  file="./site/templates/error.html",
	  bindings=[{'Stacktrace', Stacktrace}],
	  module_aliases=[{page,?MODULE}]
	}.

%% This is assuming that the error.html template includes a call to [[[page:body(Stacktrace)]]]
body(Stacktrace) ->
	[
	  #h1{text="UH OH! Something went wrong!"},
	  #panel{text=wf:f("Here's the contents of the error: ~p",[Stacktrace])}
	].


postback_request(Type, Error, Stacktrace, _Config, _State) ->
	%% Print the error message to the Erlang console
	?WF_LOG("~p~n", [{error, Type, Error, Stacktrace}]),

	%% Note, we don't set the status code to 500. If we did, the browser will
	%% simply discard any javascript. So we keep a 200 status code and print
	%% a friendlier error message.

	Msg = wf:f("I'm sorry, but there was an error. Here's the stack trace: ~p",[Stacktrace]),
	wf:wire(#alert{text=Msg}).


```

### Recommendations for first requests

   It's very likely that for handling first requests, you'll want to display a
   template to the user, rather than simple raw text or hand rolled HTML.

   In that event, it's probably very likely you'll want to redirect requests
   to the standard `page` module to the crash handler module (or a custom crash
   module). This is done with the `module_aliases` attribute on the `#template`
   element. This is especially useful if you want to use the same template for
   your errors that you might use with the rest of the page.

   If you plan on sending any of the arguments passed to `first_request/5` to
   a template, make sure you take advantage of the `bindings` attribute.

### Recommendations for postback requests

   If you want to gracefully handle crashes related to postbacks, comets, etc,
   then you need to make sure you do **not** set the status_code to a failure
   code (like 500). You might as well keep it as the default 200, and then wire
   whatever commands you wish.

   At the very least, we recommend giving the user **some** kind of feedback,
   even if it's a simple javascript alert (See Alert Action below).

### What if your handler crashes?

   If your custom crash handler crashes, then Nitrogen will fall back to its
   default behavior of throwing a 500 error and returning "Internal Server
   Error".

   It's recommended that, much like Erlang supervisors, that your crash handler
   modules be very simple, with minimal moving parts, so as to prevent this from
   happening. For example: It's probably not a great idea to hinge your crash
   handler to database availability, unless you verify with the `ErrorType` and
   `Error` arguments that the error is not related to database availability.

   Using those arguments, you can custom tailor the error messages to certain
   classes and show different templates or content depending on the message.

   But the simplest error template should just be a mostly static HTML template
   that probably doesn't even make an page calls. That will ensure that your
   crash handler doesn't crash, effectively negating its existence.

### See Also

 *  [Handler Overview](./handlers.md)

 *  [Template Element](template.md)

 *  [Alert Action](alert.md)
