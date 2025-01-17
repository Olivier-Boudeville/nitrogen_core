<!-- dash: #recaptcha | Element | ###:Section -->


## Recaptcha Element - #recaptcha{}

### Overview

   The recaptcha element produces a recaptcha element.
   For more information about recaptchas you may refer to
   https://developers.google.com/recaptcha/

   The element is rendered and evaluated by an external library.

   The captcha is processed as follows:
   * The user solves the captcha.
   * If the solution is wrong the captcha gets refreshed until the user
	  gives up or the solution is correct, and a `Page:recaptcha_event` is
	  called with a `Result` of `error`.
   * If the solution is correct the `Page:recaptcha_event` callback is called
	  with a `Result` of `ok`.
   * The return value of the `Page:recaptcha_event` callback is evaluated as
	  follows:
 *  If the value is `ok` nothing is done.
 *  If the return value is `error`, the captcha gets refreshed.
 *  If the return value is `{error, Msg}`, the captcha gets
		refreshed and the Msg is shown as the error message.

### Preparations

   Before you can use the the recaptcha element, you have to register at
   https://www.google.com/recaptcha/admin/create.

   After that, you have to either specify the following values in your
   app.config or specify them as attributes to the `#recaptcha{}` element
   itself.

```erlang
[
 {nitrogen, [
			 {recaptcha,
			  [{public_key, "recaptcha_public_key"},
			   {private_key, "recaptcha_private_key"},
			   {challenge_url, "http://www.google.com/recaptcha/api/challenge"},
			   {verify_url, "http://www.google.com/recaptcha/api/verify"}]}
			   ...
 ]},
 ...
].

```

### Usage

```erlang
   #recaptcha {
	  public_key="some_public_key",
	  private_key="some_private_key"
   }

```

### Attributes

   The following attributes are required unless they are defined as nitrogen
   environment variables above.

   * `public_key` (string) - Public key provided by the Recaptcha service.
   * `private_key` (string) - Private key provided by the Recaptcha service.

#### The following attributes are optional

   * `tag` (term) - The "tag" for the recaptcha element to be passed to the
	 `recaptcha_event/2` function
   * `button_id` (atom) - The button's id
   * `button_label` (string) - The button's label
   * `button_class` (string, atom, or list of atoms) - The button's HTML
	 class.
   * `captcha_opts` (proplist) - The captcha's options. For more
	 information see the official
	 [Recaptcha Documentation](https://developers.google.com/recaptcha/docs/customization)
	 `custom_theme_widget` is not supported
 *  delegate (*Module*)  :: The module where the event callback gets called
 *  fail_body (*List*)   :: The fail message. It can be a simple string
	 or an array of Nitrogen elements. It will be displayed below the
	 recaptcha if the user fails to enter the correct recaptcha.
   * `challenge_url` (string) - The challenge URL as specified in the
	 Recaptcha Documentation (default: https://www.google.com/recaptcha/api/challenge")
   * `verify_url` (string) - The verification URL as specified in the
	 Recaptcha Documentation (default: https://www.google.com/recaptcha/api/verify")

### Postback Function

   When a recaptcha succeeds or fails, the `recaptcha_event/2` function
   is called on your page module. `Tag` will be from the `tag` attribute above,
   and `Result` will be a status atom, either `ok` if successful, or
   `{error, ErrorMessage}`  if the recaptcha fails.

   `recaptcha_event/2` can return any of the following:

 *  `ok` - Everything is okay.
 *  `error` - There was something that failed (maybe you did some data
	  validation in the `event_recaptcha/2` function and something failed so
	  let's just return an error.
 *  `{error, ErrorMessage}` - Something went wrong, and let's set the error
	  message to the contents of `ErrorMessage` (which, like `fail_body` above,
	  can be either text or Nitrogen elements).

```erlang
recaptcha_event(_Tag, ok) ->
   wf:wire(#alert{text="Congrats, you're human"}),
   ok;
recaptcha_event(_Tag, {error, ErrorReason}) ->
   wf:wire(#alert{text="WE THINK YOU'RE A ROBOT! YOU MUST PROVE OTHERWISE!"}),
   {error, "Please Try again!"}.

```

### Regarding Validation with Recaptcha

   If you wish to trigger validators before the Recaptcha gets used, you'll
   need to wire the validators to the Recaptcha's `button_id` attribute.

   You can see this in use in the example below:


### Example

Code in a page module may look like this:
```erlang
inner_body() ->
   %% Wire a validator to be triggered by `recaptcha_button`, and target `name`
   wf:wire(recaptcha_button, name, #validate{ validators=[
	  #is_required{}
   ]}),
   [
	  #label{text="Enter your name"},
	  #textbox{id=name},
	  #recaptcha{
		 button_id=recaptcha_button,
		 id=recaptcha,
		 tag=my_recaptcha,
		 captcha_opts=[{theme,white}]
	  }
   ].

event_recaptcha(my_recaptcha, ok) ->
	case check_user_input() of
		ok     -> wf:remove(recaptcha),
				  ok;
		error  -> {error, "FAIL!"}
	end.

check_user_input() ->
%% your check routine

```

### See Also

 *  [Base Element](element_base.md)

 *  [Validators](validators.md)

 *  [Recaptcha Demo](http://nitrogenproject.com/demos/recaptcha)
