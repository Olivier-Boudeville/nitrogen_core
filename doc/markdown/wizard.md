<!-- dash: #wizard | Element | ###:Section -->



## Wizard Element - #wizard{}

  The Wizard element is an element to simply present a multi-step process to
  the user throught a single unifying element.

### Usage

```erlang
   #wizard{
	  tag=the_wizard,
	  titles=[Step1Title, Step2Title, Step3Title, ...],
	  steps=[Step1Body, Step2Body, Step3Body, ...]
   }.

   ...

   wizard_event(the_wizard) ->
	  wf:wire(#alert{text="Congrats, you finished the wizard!"}).

```

### Callback

   Your page must export a `wizard_event(Tag)` function, which will be called
   when the "Finish" button is clicked on the Wizard.

### Attributes

   * `tag` (Erlang term) - The identifying tag to be sent to the wizard

   * `titles` (list of strings) - The list of title strings for each step.

   * `steps` (list of step bodies) - A list of Nitrogen "bodies". Each item
   in this list will be considered as a `body` attribute for each step.  There
   must be exactly the same number of items in the `titles` list as there are
   in the `steps` list.

   * `next` (string) - The text of the "Next" button, which takes the user
   to the next step in the wizard (default: "Next")

   * `back` (string) - The text of the "Back" button, which takes the user
   to the previous step in the wizard (default: "Back")

   * `finish` (string) - The text of the "Finish" button, which is presented
   on the last step, and triggers the `wizard_event/1` callback on your page.

   * `show_progress` (boolean) - Show the progress through the wizard.

   * `progress_text` (string) - The text progress "meter", the first `~p`
   will be replaced with the current steps, and the second `~p` will be
   replaced with the total number of steps. For example, `"Step 6 of 12"`.
   (Default: `"Step ~p of ~p"`)

### Other Important Information

   The current implementation of the `#wizard` element does not allow for
   multiple wizards on a single page.

### Wiring Validators

   Wiring validators to the Wizard element can be a little trickier than usual.
   There are "Next" and "Back" buttons on both the top and bottom of the
   wizard. You want to make sure you wire validators to **both** buttons. As
   such.

   Further, since the "Back" button could very well be executed with an empty
   form, so it's not recommended to attempt to validate on the "Back" button,
   only to validate on the next buttons.

   So with that said, to validate a field, you want to wire the validation to
   both "Next" or "Finish" buttons (if it's the last step, it'll be the
   "Finish" button). You can determine the `id`s of the next and finish buttons
   using the `element_wizard:next_button_ids(Step, NumSteps` function, where
   `Step` is the **current** step, and **NumSteps** is the total number of steps in
   the Wizard.

   For example, assuming you have a field called `pet_name` in step 2 (of 5 steps) that you wish to be a required field, you would wire the validator as follows:

```erlang
   Buttonids = element_wizard:next_button_ids(2, 5),
   Validator = #validate{validators=#is_required{text="Required"}},
   [wf:wire(Buttonid, pet_name, Validator) || Buttonid <- Buttonids].

```

### See Also

 *  [base element](element_base.md)

 *  [Wizard Demonstration](http://nitrogenproject.com/demos/wizard)
