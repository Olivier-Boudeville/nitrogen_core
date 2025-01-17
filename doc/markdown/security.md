<!-- dash: Handlers - Security | Guide | ###:Section -->



## Security Handler

  The security handler provides a mechanism for determining whether or not the
  client user has access to a particular resource.  In particular, it can be
  used for injecting HTTP Authentication headers, or for being customized to
  your own app's authentication system, like checking cookies to automatically
  log in a returning user.

### Behavior Functions

##### `init(Config, State)`

  Initialize the Security Handler.  Like the [Route Handler](./route.md),
  the majority of the work done in the security handler will happen here, or
  at least will start here.

  It's typical to check for headers, cookies, or other settings here, and for
  possibly loading paths and their required settings.

 *  /Return Value/ - `{ok, NewState}`

##### `finish(Config, State)`

  Clean up the handler

 *  /Return Value/ - `{ok, NewState}`

### Simplest Example - A security handler that lets everything through

The default security handler for Nitrogen is extremely simple and allows all
requests through. The text of that handler is here:

```erlang
-module (default_security_handler).
-behaviour (security_handler).
-export ([
	init/2,
	finish/2
]).


init(_Config, State) ->
	% By default, let all requests through. If we wanted to impose
	% security, then check the page module (via wf:page_module()),
	% and if the user doesn't have access, then set a new page module and path info,
	% via wf_context:page_module(Module), wf_context:path_info(PathInfo).
	{ok, State}.

finish(_Config, State) ->
	{ok, State}.

```

### Cookie-based Authentication Example

As an example, you could automatically log in a user based on a cookie inside
the security handler. This example assumes you'll check an authentication
cookie stored with your login table within a database, providing a `db_login`
module for interacting with the user table. It assumes the existance of a
`db_login:loginid_from_cookie/1` function which takes an a `Cookie` and returns
a `Loginid` or `undefined`.

```erlang

-module (bracketpal_security_handler).
-behaviour (security_handler).
-export ([
	init/2,
	finish/2
]).


init(_Config, State) ->
	attempt_cookie_login(),
	{ok, State}.

finish(_Config, State) ->
	{ok, State}.

%% PRIVATE FUNCTIONS %%

%% First, let's check to see if we've already tried to log in this user using
%% cookies. If we have, then we don't want to keep trying, and hitting the
%% database every time we load a page when we already know that it won't
%% authenticate

attempt_cookie_login() ->
	attempt_cookie_login(session).

attempt_cookie_login(session) ->
	%% Have we already attempted a cookie login?
	%% If so, then skip it
	%% If not, let's try to authenticate with the cookie
	case wf:session_default(cookie_login_attempted,false) of
		false -> attempt_cookie_login(cookie);
		_ -> do_nothing
	end;
attempt_cookie_login(cookie) ->
	%% Let's read the authentication token from our "quicklogin" cookie
	case wf:cookie(quicklogin) of
		undefined ->
			%% The cookie is undefined, so obviously there's nothing to log in
			do_nothing;
		Cookie ->
			%% Let's check our database to see if we have a user that's
			%% associated with our authentication cookie
			case db_login:loginid_from_cookie(Cookie) of
				undefined ->
				  %% No user associated with that token, so let's just do nothing
				  do_nothing;
				Loginid ->
				  %% Yes! We have a login associated with that cookie, so let's
				  %% Set use the Nitrogen user handler to set the loginid
				  wf:user(Loginid)
			end,

			%% Finally, we've now attempted to do a cookie login, so let's set
			%% a session variable so we don't try doing the cookie login again
			%% for this session
			wf:session(cookie_login_attempted,true)
	end.

```

### An HTTP Auth Example

Nitrogen also provides a simple HTTP Auth security handler for those interested
in employing some HTTP auth-type user verification (courtesy of Torbjorn Tornkvist).

The full text of this can be viewed on [Github](https://github.com/nitrogen/nitrogen_core/blob/master/src/handlers/security/http_basic_auth_security_handler.erl)


### See Also

 *  [Handler Overview](../handlers.md)
