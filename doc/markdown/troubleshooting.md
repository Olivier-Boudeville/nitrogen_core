<!-- dash: Troubleshooting | Guide | ##:Section -->


# Troubleshooting

## Common Questions when Troubleshooting Nitrogen Apps

 *  [What tools do I need for troubleshooting Nitrogen apps?](0)
 *  [I got an error on the Erlang shell. What does it mean?](100)
 *  [I started Nitrogen but nothing is happening. What do I do?](200)
 *  [Why aren't postbacks working?](300)
 *  [My postbacks aren't doing what I expect, how can I figure out what's happening?](400)
 *  [Why isn't upgrading working?](500)
 *  [Other questions not answered here?](10000)

## What tools do I need for troubleshooting Nitrogen apps?

  Here are a few recommendations:

 *  Have `sync` running. (run in the Erlang shell with `sync:go()`)

 *  Have a browser with decent Javsacript debugging and network inspecting
	(Chrome's inspector, Firebug for Firefox, etc). Because Nitrogen does a lot
	of AJAX requests, you'll be watching those requests happen, and when or if
	they error out, you'll see those changes.

 *  Don't be afraid to use Nitrogen's log handler with: `wf:info/1,2`,
	`wf:warning/1,2`, or `wf:error/1,2` , or send Erlang terms formatted as
	strings to the javascript console with `wf:console_log/1,2`.

 *  Finally, when you're really not sure, don't be afraid to dig into the
	nitrogen source code to see what's going on.

## I got an error on the Erlang shell. What does it mean?

  Erlang errors can be a little scary to the uninitiated, but are very helpful
  once you get the feel for them.  They include relatively complete stack
  traces, so you can see the stack of calling functions (what functions called
  which functions, which lead to this particular crash).

  Conveniently, as of R15, errors at least show line numbers, so you're no
  longer wading throught long, multi-clause functions to find the cause of the
  error. Prior to R15, tracking down the cause of an error in something like an
  `event` function or a `gen_server`'s `handle_call` function was a tedius
  endeavor.

  Anyway, here's a pretty typical-looking Nitrogen error:

```erlang
=INFO REPORT==== 28-Aug-2013::16:20:02 ===
  {error,error,function_clause,
		 [{wf_convert,html_encode,
					  [{link,is_element,element_link,undefined,undefined,
							 undefined,true,[],[],[],[],[],"My Link",[],false,
							 true,false,false,"javascript:",undefined,undefined,
							 false,undefined,undefined},
					   true],
					  [{file,"src/lib/wf_convert.erl"},{line,93}]},
		  {element_span,render_element,1,
						[{file,"src/elements/html/element_span.erl"},{line,19}]},
		  {wf_render_elements,call_element_render,3,
							  [{file,"src/lib/wf_render_elements.erl"},
							   {line,130}]},
		  {wf_render_elements,render_element,1,
							  [{file,"src/lib/wf_render_elements.erl"},
							   {line,115}]},
		  {wf_render_elements,render_elements,2,
							  [{file,"src/lib/wf_render_elements.erl"},
							   {line,37}]},
		  {lists,foldl,3,[{file,"lists.erl"},{line,1248}]},
		  {wf_render_elements,render_elements,2,
							  [{file,"src/lib/wf_render_elements.erl"},
							   {line,32}]},
		  {lists,foldl,3,[{file,"lists.erl"},{line,1248}]}]}

```

  Understanding this error takes a little digging. The top line

```erlang
  {error, error, function_clause

```

  Lets us know that a Erlang tried to call a function for which there was no
  matching clause. The next line tells us what function was attempted and what
  arguments were passed to it:

```erlang
   [{wf_convert,html_encode,
		  [{link,is_element,element_link,undefined,undefined,
				 undefined,true,[],[],[],[],[],"My Link",[],false,
				 true,false,false,"javascript:",undefined,undefined,
				 false,undefined,undefined},
		   true],
		  [{file,"src/lib/wf_convert.erl"},{line,93}]},

```

  This tells us a lot of information. The function was `wf_convert:html_encode`
  on line 93 in `src/lib/wf_convert.erl` (which is actually a Nitrogen core
  module). Generally, if the module name starts with `wf_`, it's probably part
  of Nitrogen core.

  The first argument was that huge tuple that starts with `{link`, and the
  second argument was the atom `true`.  This is already giving us a good hint
  as to the cause of the error.

  Because Nitrogen elements are simply Erlang records, and Erlang records are
  just syntactic sugar for tuples, we can conclude based on
  `{link, is_element,...` that the argument that's causing problems is a
  `#link` element somewhere in our code.

  But if the cause is still not clear, let's go deeper.  The next chunk of the error is this:

```erlang
	{element_span,render_element,1,
		[{file,"src/elements/html/element_span.erl"},{line,19}]},

```

  This tells us that the `element_span:render_element` function is calling function. We can imply from this that there is a `#span` element containing a `#link` element, and something about that is causing a crash.

  If we look at the code of our page, we'll see this line:

```erlang
   #span{text=#link{text="My Link"}},

```

  The error reveals itself: the problem is that you can't pass Nitrogen
  elements into a `text` attribute. The `text` attribute of the `#span` element
  attempts to call `html_encode` on the `#link` element, but `html_encode` only
  accepts text (strings and binaries), so it crashes.

  This has been a rather contrived example, but it's the kind of error you may
  run into. Dialyzer support has been added to help eliminate some of this class of error.

  If you're really stuck, run dialyzer:

  : make dialyzer

  Which might give a message like this:

```erlang
index.erl:30: Record construction #span{is_element::'is_element',module::'element_span',
show_if::'true',class::[],style::[],html_id::[],data_fields::[],body::[],
text::#link{is_element::'is_element',module::'element_link',show_if::'true',class::[],
style::[],html_id::[],data_fields::[],title::[],text::[32 | 76 | 77 | 105 | 107 | 110
| 121,...],body::[],new::'false',html_encode::'true',mobile_target::'false',
mobile_dialog::'false',url::[58 | 97 | 99 | 105 | 106 | 112 | 114 | 115 | 116 | 118,...],
handle_invalid::'false'},title::[],html_encode::'true'} violates the declared type of
field text::binary() | maybe_improper_list(binary() | maybe_improper_list(any(),binary()
| []) | char(),binary() | [])

```

  And while this is a hugely long error, the key informaton is the =Record
  contruction #span` is the culprit, and the reason it's failing is `violates the
  declared type of field text::...=. This is telling us that something is wrong
  with the `text` field, as it's expecting something other than what it was
  given.

## I started Nitrogen but nothing is happening. What do I do?

  This is a very general kind of issue, and can be any number of the following:

 *  If Erlang crashes right from the get-go, make sure you don't already have
	that port in use, and make sure you have access to open the specified port in
	app.config.

 *  If you know the port is available, and you get an error about "name in use"
	from epmd, that means you have a node of the same name running on the
	machine. You can search for instances of the executable `erl` or `beam`
	running.

 *  If Erlang launches without crashing, but the page http://127.0.0.1:8000
	isn't responding, then either you want to make sure that you don't have a
	firewall blocking that port from being bound.  Also make sure your config is
	binding to the right IP address. "`0.0.0.0`" or `{0,0,0,0}` (depending on
	server config) will bind to any of the machine's IPs.

 *  If neither of the above solve your problem, there might be something more
	significant. Feel free to email the
	[Mailing List](https://groups.google.com/d/forum/nitrogenweb)

## Why aren't postbacks working?

  If the rest of your page is working, but postbacks aren't working, you can
  narrow down the cause by first opening up the javascript console in your
  browser and looking for any errors. If there are javascript errors during the
  page generation, javascript will halt on the page, and very likely any
  postbacks that haven't been wired to elements won't get wired at all. By far,
  this is the most common cause - simple javascript errors.

  To figure out why the javascript is erroring out, you'll need to look at the
  line causing the error, and a good browser-based javascript console will give
  you the line in question and the cause pf the error.  In this situation, it's
  typically hand-rolled javascript sent out to the browser (e.g.
  `wf:wire("do_somthing()")`), rather than generated javascript using Nitrogen
  elements (e.g. ~wf:wire(#alert{text="Something"})~).

  The other cause of javascript errors is with validations that might have been
  removed. While efforts have been made to minimize this problem, it's still
  good practice to [clear validators](clear_validation.md) if
  a form with validation is removed from the page. For example, if a field has
  a validator attached to it, but the field is removed from the page without
  the submit button being removed, the validation system will still attempt to
  confirm that the field has a value (which it doesn't, since it's not there).
  This can throw off the validation system, and cause javascript errors when
  the submit button is pressed.

  **If there are no javascript errors in the console**, then the problem is
  either one of two things:

 *  Your template doesn't have a
	: [[[script]]]
	section like it should (in which case, initializing javascript is never
	sent to the browser), or
 *  Your postbacks are silently failing on the server.

  The most common cause of postbacks silently failing on the server (meaning
  not generating console errors) is the presence of a catcha-all event. For
  example:

  : event(_) -> ok.

  Having that clause on a page will cause unmatched postbacks to simply
  silently fail and do nothing. For debugging purposes, we recommend against
  that practice.

  To figure out if postbacks are even making it to the server, open up your
  browser's network inspector and click a button that will generate a postback.
  You should see the AJAX request pop up there and do somthhing.  If you see it
  fail for some reason, then you should usually get an error in the Erlang
  console. However, if it succeeds, you can view the contents of the response.
  You should see some javascript in there besides just the page context stuff.

  This will give you some insight into what's going on, and why your postbacks
  are failing.

## My postbacks aren't doing what I expect, how can I figure out what's happening?

  If your postbacks seem to be succeeding, but not responding as expected, then
  resorting to "printf debugging" is a good way to test things.  You can print
  text to the erlang console to inspect the status of variables with `wf:info`,
  `wf:warn`, and `wf:error`, or you can send helper commands to the browser
  with the `#alert{}` or `#console_log` actions.

## Why isn't upgrading working?

  If you run `make upgrade`, but find that it seems to go through all the steps
  of downloading the latest versions, and yet you notice nothing really
  happens, this can be evidence of a known issue with rebar. This is generally
  caused by your the dependencies in your rebar.config looking like this:

  : {nitrogen_core, ".*", {git, "git://github.com/nitrogen/nitrogen_core", "HEAD"}}

  or this:

  : {nitrogen_core, ".*", {git, "git://github.com/nitrogen/nitrogen_core", "master"}}

  The solution is to "fix" your rebar.config to have the dependency lines look like this:

  : {nitrogen_core, ".*", {git, "git://github.com/nitrogen/nitrogen_core", {branch, master}}}

  Notice that the difference is in the fact use of `{branch, master}` instead
  of the plain text `master`.

  Once you make this change, upgrading should work as intented.

## Other questions not answered here?

  Running into an error that's not covered here?  Not sure where to start? Ask
  your question in the comments below, or contact the
  [mailing list](https://groups.google.com/forum/#!forum/nitrogenweb)
