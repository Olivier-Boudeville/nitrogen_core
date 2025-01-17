<!-- dash: Upgrading to Nitrogen 2.3 | Guide | ###:Section -->


# Upgrading to Nitrogen 2.3

## Overview

  Upgrading to Nitrogen 2.3 is simple, however, ensuring that your upgraded app
  supports Websockets will require a small amount of work (mostly copying a few
  files, and checking simple configurations).

### The Simple Method (which won't allow the use of Websockets)

   Nitrogen 2.3 is designed to be completely backwards compatible with Nitrogen
   2.2 and earlier, meaning no changes to your existing codebase should be
   required (beyond recompiling your modules to support any changes to header
   files).

   If downtime is acceptable, you can do the following after bringing down your
   Erlang VM.

   To upgrade to Nitrogen 2.3, you'll want to update your rebar.config and
   change the `nitrogen_core` appliation to use the "v2.3.0" tag, and update
   the `simple_bridge` application to use the "v2.0.0" tag:

```erlang
  {nitrogen_core, "", {git, "git://github.com/nitrogen/nitrogen_core", {tag, "v2.3.0"}}},
  {simple_bridge, "", {git, "git://github.com/nitrogen/simple_bridge", {tag, "v2.0.0"}}},

```

   Then run `make upgrade` in your app, which should fetch the latest versions
   and recompile your app accordingly.

   Your application will then be upgraded.

### Upgrading with Sync

   If you wanted, you can enable sync, either with `bin/dev compile` (at the
   \*nix terminal) or with `sync:go()` (at the Erlang Shell).

   Then run the above instructions for upgrading. Depending on the size of your
   application, you may experience a little bit of downtime while your Nitrogen
   installation is recompiling, but as soon as its finished, Sync will load the
   modules and you'll be good to go.

### The Simple Method with Sync and without any Downtime

   If you want to ensure you don't experience any downtime during this upgrade,
   you're going to want to upgrade your `sync` version first.

   The easiest way to do this is to first start `sync` in your Nitrogen
   application, either with `sync:go()` or at the Linux/Unix terminal with
   `bin/dev compile`.

   Then we're going to manually upgrade sync:

```bash
$ cd lib/sync
$ git checkout master
$ git pull origin master

```

   That second line is just to make sure we're actually on the master branch.
   Previous versions of Nitrogen had an issue that might have caused us to be
   on a detached branch, this will ensure we're good to go.

   After running that command, sync very likely would have crashed. That's
   okay: it's supervised, so it would be restarted with a fresh state.

   Now that sync is upgraded, we're going to want to pause it:

   At the Erlang console, run:

```erlang
> sync:pause().

```

   Now let's unpause sync:

```erlang
> sync:unpause().

```

   This should start a whole slew of things going on in the erlang VM,
   scrolling really fast (just notifying you of updated BEAM files), and when
   it's finished, your application will be upgraded and live with no downtime.

### The More Complete Method

   The previous instructions were okay as long as you don't care about
   Websockets.

   If you want to use websockets, however, it's going to take a little bit more
   work.

   Please note that these instructions will require a bit of downtime because
   we'll have to take the underlying server offline to be restarted by
   SimpleBridge.  In this case, we may as well stop `sync` before doing this
   process.

   Same as above, we're going to need to add the same rebar dependencies:

```erlang
  {nitrogen_core, "", {git, "git://github.com/nitrogen/nitrogen_core", {tag, "v2.3.0"}}},
  {simple_bridge, "", {git, "git://github.com/nitrogen/simple_bridge", {tag, "v2.0.0"}}},

```

   Then run `make upgrade`.

   Now we're going to need to download a few files. Feel free to copy and paste
   the following into your *nix shell:

```bash
$ curl https://raw.githubusercontent.com/nitrogen/nitrogen/master/rel/overlay/common/site/src/nitrogen_main_handler.erl -o site/src/nitrogen_main_handler.erl
$ curl https://raw.githubusercontent.com/nitrogen/nitrogen/master/rel/overlay/common/site/src/nitrogen_sup.erl -o site/src/nitrogen_sup.erl
$ curl https://raw.githubusercontent.com/nitrogen/nitrogen/master/rel/overlay/common/etc/simple_bridge.config -o etc/simple_bridge.config

```

   Notice that the second line will actually overwrite the current
   `nitrogen_sup.erl` file, so if you have anything customized there, make note
   of it or make a backup first.

   Now you'll want to copy any custom handlers and other configs you've
   manually added to `site/src/nitrogen_PLATFORM.erl` into
   `site/src/nitrogen_main_handler.erl` before into the `handlers()` function.
   This will ensure that your custom handlers get initialized with both
   standard requests and websocket requests.

   So if your `nitrogen_mochiweb.erl` file looked like this:

```erlang
-module(nitrogen_mochiweb).
-export ([loop/1]).
-include_lib("nitrogen_core/include/wf.hrl").

loop(Req) ->
	{ok, DocRoot} = application:get_env(mochiweb, document_root),
	RequestBridge = simple_bridge:make_request(mochiweb_request_bridge, {Req, DocRoot}),
	ResponseBridge = simple_bridge:make_response(mochiweb_response_bridge, {Req, DocRoot}),
	nitrogen:init_request(RequestBridge, ResponseBridge),
	wf:header('cache-control',"no-cache"),
	wf:content_type("text/html; charset=utf-8"),
	nitrogen:handler(myapp_security_handler,[]),
	nitrogen:handler(myapp_route_handler, []),
	nitrogen:run().

```

   Then we're going to want our `nitrogen_main_handler.erl` file to look like this:

```erlang
-module(nitrogen_main_handler).
   -export([run/0]).

   handlers() ->
	   nitrogen:handler(myapp_security_handler,[]),
	   nitrogen:handler(myapp_route_handler, []),

   ws_init() ->
	  handlers(),
	  ok.

   run() ->
	   wf:header('cache-control',"no-cache"),
	   wf:content_type("text/html; charset=utf-8"),
	   handlers(),
	   wf_core:run().

```

   You'll notice above that we don't set the header or content type in the
   `ws_init()` function. This is because the headers would have already been
   sent when the websocket connection is established.

   Next, we're going to edit `etc/simple_bridge.config` to make sure we're
   binding to the correct IP address and ports and that those settings are all
   correct (static paths, max post post and file sizes, etc).

   At this point, we can safely remove your `site/PLATFORM.config` file and
   your old `site/src/nitrogen_PLATFORM.erl` file (where `PLATFORM` is your
   server of choice).

   From here, you can run a simple `make upgrade`, and your application will be
   upgraded.

   Go ahead and kill the Erlang VM and restart it and it should be good to go,
   complete with websocket support.

### A Note about Nginx and Reverse Proxies

   If you are using Nginx as a reverse proxy for Nitrogen, and if you wish to
   support Websockets, you're going to need to be running at least version 1.4
   of Nginx.
