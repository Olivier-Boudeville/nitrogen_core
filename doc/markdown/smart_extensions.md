<!-- dash: Smart Extensions | Guide | ###:Section -->


# Smart Extensions

## Overview

  Smart Extensions are a feature of the Nitrogen Dynamic Route Handler, which
  gives the ability to use different entry points into a page module based on
  the extension provided to the request. Using this feature, through your
  application's configuration, you can add, for example. a `.json` extension to
  a request, and have Nitrogen call the `json_main()` function, which returns a
  simple Erlang Proplist.  The routing system would then convert that proplist
  to a properly formatted JSON string.

### Usage

  To use a smart extension, you would add the following to
  your `nitrogen` application configuration:

```erlang
{smart_extensions, [
  {EXTENSION, ENTRY_FUNCTION, PROCESSING_FUNCTION}
]}

```

 *  `EXTENSION` :: is the string extension of the file
	("json", "csv", etc).

 *  `ENTRY_FUNCTION` :: is the function in our page module that
	is called. For Nitrogen's built-in JSON smart extension, you might call
	it `json_main`). This is expected to be an atom.

 *  `PROCESSING_FUNCTION` :: identifies the module and function
	of a pre and post- processing function. It can either be the atom
	`undefined`, in which case no pre or post processing will be done.  If,
	however it's a two-tuple consisting of `{Module, Function}`, it will call
	`Module:Function(EntryFun)` passing `ENTRY_FUNCTION` above as the only
	argument.

### Example

  To use Nitrogen's built-in JSON smart extension the following would be
  used:

```erlang
{smart_extensions, [
  {"json", json_main, {nitrogen_smart_extensions, json}}
]}

```

  Then add a `json_main()` function to your page module, which
  returns an Erlang proplist. The proplist will automatically be
  converted to a JSON string and the MIME-type will be set to
  "application/json".

  Here's an example of a `json_main()` function in action.

```erlang
json_main() ->
  [
	{favorite_fruit, <<"This is a string">>},
	{a_list_of_numbers, [45, 100, 50000, 60]},
	{some_value, "This would also render as a list of numbers"}
  ].

```

   The Smart Extensions feature knows that with the JSON to convert the
   proplist into a JSON-encoded string that looks like this:

```javascript
   {
	"favorite_fruit":"This is a string",
	"a_list_of_numbers":[45,100,50000,60],
	"some_value":[84,104,105,115,32,119,111,117,108,100,32,97,
				  108,115,111,32,114,101,110,100,101,114,32,97,
				  115,32,97,32,108,105,115,116,32,111,102,32,
				  110,117,109,98,101,114,115]
   }

```

  As you can see, "This would also render as a list of numbers" rendered as a
  list of numbers. This is due to the JSON encoding which treats string-lists
  as literal lists of integers. If you need a string to be encoded and sent to
  the client, make sure to use a binary instead of a string list.

### A Simple Smart Extension

  You can add your own smart extension as well.  The simplest smart extension would be to simply provide an entry point for an extension. First adding `{"my_ext", my_entry}` to to the list of smart extensions in your config:

```erlang
{smart_extensions, [
  {"my_ext", my_entry}
]}

```

Then add the following to a page you wish to use the `my_ext` smart extension with:

```erlang
my_entry() ->
   "This is my the my_ext smart handler".

```

  This is the simplest kind of smart extension.  Simply put, if it sees a
  `.my_ext` as the extension, it would use `my_entry()` as the entry point
  in your module, and be done with it. There would be no pre- or
  post-processing performed with it - whatever is returned by the function
  would be sent to the browser.

### An Advanced Smart Extension

   Adding a more advanced smart extension allows you to do pre- and
   post-processing.  Expanding on the above, let's say we have some weird
   legacy system that expects a series of words in all upper-case, each one its
   own line, and to automatically download as a file called "legacy.txt", and
   we wanted the extension of the request to be "textlist".

   So the specs would be something like this:

 *  Request: `/download/mydata.textlist`
 *  Which downloads a file to the filesystem called "legacy.txt"
 *  And which looks like this:
```txt
WORD
APPLE
BANANA
LINUX
NINJA

```

  First, let's add smart extenion to our app.config:

```erlang
{smart_extensions, [
  {"textlist", textlist, {textlist_handler, handle}}
]}

```

   Simply stated, this means: "If the request ends with .textlist, and our page
   module has a `textlist/0` function, we're going to call
   `textlist_handler:handle`, which will pre-process and call
   `Page:textlist()`, then post-process the result.

So let's edit our page module (by the request above, it should be `download_mydata` - remember slashes become underscores in modules):

```erlang
-module(download_mydata)
-compile(export_all).

textlist() ->
  %% more likely, these words would be read from a database or something, but
  %% for the sake of our demo, we're just hardcoding a list of words
  ["word", "apple", "banana", "linux", "ninja"].

```

Now we need to create our `textlist_handler` module to do the pre and postprocessing.

```erlang
-module(textlist_handler).
-export([handle/1]).

handle(EntryFun) ->
  %% Force it to download as the filename legacy.txt
  wf:download_as("legacy.txt")

  %% Force the content type to text/plain
  wf:context_type("text/plain")

  %% Let's call the entry function, which is passed to us as a function.
  List = EntryFun(),

  %% Let's ensure all values are strings, and convert them to uppercase:
  UpperList = [string:to_upper(wf:to_list(X)) || X <- List],

  %% Finally, let's make sure each ends up on its own line and return that
  wf:join(UpperList, "\n").

```

And that's it.

### See Also

 *  [Route Handler Docs](route.md)

 *  [Smart Extension Demo](http://nitrogenproject.com/demos/smart_extension)

 *  [Nitrogen's Smart Extension code](https://github.com/nitrogen/nitrogen_core/blob/ws/src/handlers/route/nitrogen_smart_extensions.erl)

 *  [Dynamic Route Handler code](https://github.com/nitrogen/nitrogen_core/blob/ws/src/handlers/route/dynamic_route_handler.erl)
