<!-- dash: Nitrogen Paths | Guide | ##:Section -->


# Nitrogen Paths

## Overview

  Nitrogen 2.x allows you to reference element paths using jQuery
  selectors. These selectors can be used in three contexts:

 *  As the target (first parameter) of `wf:wire/2`,
	`wf:update/2`, `wf:insert_top/2`,
	`wf:insert_bottom/2`, etc.

 *  As the trigger (first parameter) or target (second parameter) of
	`wf:wire/3`.

 *  As the `trigger` or `target` of an `#event{}`
	action.

  Path matching in Nitrogen is complicated because it must provide a
  simple, straight-forward interface while still accounting for potential
  name-collisions due to the nested nature of Nitrogen elements.

  For example, you should be able to create two elements with the same
  ID, but in different areas of the element hierarchy, and be able to
  reference those elements in predictable ways.

## How Are Paths Matched?

  The `objs(Path, Anchor)` function is used to match selectors on the
  client. This is defined in /nitrogen.js/, and returns a jQuery
  object containing all of the elements that match the selector
  provided under `Path`, potentially in close proximity to the
  selector provided under `Anchor`. If `Anchor` is not provided, then
  the anchor set by `Nitrogen.$anchor(Anchor, Target)` is used.

  `Nitrogen.$anchor/2` is automatically called by Nitrogen before each
  action is rendered. The `Anchor` value is set to the uniquely
  generated temp ID for the element to which the action applies, it
  will look something like `.wfid_temp23422`.

  Unless you are doing some wacky stuff, your application should not
  need to worry about calling `objs()` or `Nitrogen.$anchor()`
  directly, but it helps to know what is happening behind the scenes
  in case you need to debug.

  If you are trying to wire an action or update a command, and the
  statement seems to affect the wrong elements, it may help to drop
  down into a Javascript console and run `objs(YourPath)` to see what
  is actually getting matched.

### Steps To Match a Path

   Here are the steps that the `objs()` function takes when matching a
   path.

   1. If the selector has multiple parts separated by commas, then the
	  selector is split, and each part is combined.

   2. If the selector is "page", then a reference to the DOM document
	  is returned.

   3. The string "##" is replaced with ".wfid_". This allows you to
	  specify Nitrogen elementIDs in a string selector. For example
	  "##myelementid > .someclass" will be rewritten to
	  ".wfid_myelementid > .someclass". This is done because Nitrogen
	  uses classes to tag your elements with their ID, so an element
	  with ID of 'myelementid' will be tagged with a class called
	  '.wfid_myelementid'.

   4. If the string "me" is found surrounded by non-word characters,
	  it is replaced with the anchor that was set. This allows
	  you to write paths such as "me > .image" which will apply to all
	  elements with class ".image" under the current element.

   5. If the path is a one word string and it is the name of an HTML
	  element such as "table", "div", "h1", etc., then try matching as
	  a Nitrogen element ID first, and if that doesn't work, then try
	  matching as an HTML element.

   6. If a string is found containing only words separated by periods
	  (for example, "element1.element2"), then it is assumed to be a
	  nested series of elements, and is converted to ".wfid_element1
	  .wfid_element2". This will match all Nitrogen elements named
	  'element2' underneath any Nitrogen elements named 'element1'.

   7. If the path begins with "body", then match using a call to
	  `jQuery(path)`. This will match any elements in the context of the
	  entire document.

   8. Otherwise, Nitrogen will try to match elements as closely as
	  possible starting at the current anchor. First, try to match the
	  path under the current anchor using
	  `jQuery(anchor).find(path)`. If results were found, then stop
	  and return.  Otherwise, call `jQuery(anchor).parents()` to get a
	  list of parents, and then for each parent, call
	  `jQuery(parent).find(path)`. If results were found, then stop
	  and return.

   9. Finally, if no elements were found, then Nitrogen resorts to
	  matching `jQuery(path)`.

## Nitrogen Specific Selectors

### Selector: me
   A keyword to specify the context of the current anchor. In other
   words, if you wire an event to an element, then 'me' will point to that element.

### Selecter: ##element
   A shortcut for referring to a Nitrogen element when you specify a
   string path. Because of the way Nitrogen uses class names to tag
   elements, this is the same as specifying ".wfid_element".

## jQuery Selectors

  Selectors and descriptions below are pulled directly from the [jQuery Selectors Documentation](http://api.jquery.com/category/selectors/).

### Selector: *
   Selects all elements.

### Selector: :animated
   Select all elements that are in the progress of an animation at the
   time the selector is run.

### Selector: Attribute Contains Prefix [name|=value]
   Selects elements that have the specified attribute with a value
   either equal to a given string or starting with that string
   followed by a hyphen (-).

### Selector: Attribute Contains [name*=value]
   Selects elements that have the specified attribute with a value
   containing the a given substring.

### Selector: Attribute Contains Word [name~=value]
   Selects elements that have the specified attribute with a value
   containing a given word, delimited by spaces.

### Selector: Attribute Ends With [name$=value]
   Selects elements that have the specified attribute with a value
   ending exactly with a given string.

### Selector: Attribute Equals Selector [name=value]
   Selects elements that have the specified attribute with a value
   exactly equal to a certain value.

### Selector: Attribute Not Equal Selector [name!=value]
   Select elements that either don't have the specified attribute, or
   do have the specified attribute but not with a certain value.

### Selector: Attribute Starts With Selector [name^=value]
   Selects elements that have the specified attribute with a value
   beginning exactly with a given string.

### Selector: :button Selector
   Selects all button elements and elements of type button.

### Selector: :checkbox Selector
   Selects all elements of type checkbox.

### Selector: :checked Selector
   Matches all elements that are checked.

### Selector: Child Selector ("parent > child")
   Selects all direct child elements specified by "child" of elements
   specified by "parent".

### Selector: Class Selector (".class")
   Selects all elements with the given class.

### Selector: :contains() Selector
   Select all elements that contain the specified text.

### Selector: Descendant Selector ("ancestor descendant")
   Selects all elements that are descendants of a given ancestor.

### Selector: :disabled Selector
   Selects all elements that are disabled.

### Selector: Element Selector ("element")
   Selects all elements with the given tag name.

### Selector: :empty Selector
   Select all elements that have no children (including text nodes).

### Selector: :enabled Selector
   Selects all elements that are enabled.

### Selector: :eq() Selector
   Select the element at index n within the matched set.

### Selector: :even Selector
   Selects even elements, zero-indexed. See also odd.

### Selector: :file Selector
   Selects all elements of type file.

### Selector: :first-child Selector
   Selects all elements that are the first child of their parent.

### Selector: :first Selector
   Selects the first matched element.

### Selector: :gt() Selector
   Select all elements at an index greater than index within the
   matched set.

### Selector: Has Attribute Selector [name]
   Selects elements that have the specified attribute, with any value.

### Selector: :has() Selector
   Selects elements which contain at least one element that matches
   the specified selector.

### Selector: :header Selector
   Selects all elements that are headers, like h1, h2, h3 and so on.

### Selector: :hidden Selector
   Selects all elements that are hidden.

### Selector: ID Selector ("#id")
   Selects a single element with the given id attribute.

### Selector: :image Selector
   Selects all elements of type image.

### Selector: :input Selector
   Selects all input, textarea, select and button elements.

### Selector: :last-child Selector
   Selects all elements that are the last child of their parent.

### Selector: :last Selector
   Selects the last matched element.

### Selector: :lt() Selector
   Select all elements at an index less than index within the matched
   set.

### Selector: Multiple Attribute Selector [name`value][name2`value2]
   Matches elements that match all of the specified attribute filters.

### Selector: Multiple Selector ("selector1, selector2, selectorN")
   Selects the combined results of all the specified selectors.

### Selector: Next Adjacent Selector ("prev + next")
   Selects all next elements matching "next" that are immediately
   preceded by a sibling "prev".

### Selector: Next Siblings Selector ("prev ~ siblings")
   Selects all sibling elements that follow after the "prev" element,
   have the same parent, and match the filtering "siblings" selector.

### Selector: :not() Selector
   Selects all elements that do not match the given selector.

### Selector: :nth-child Selector
   Selects all elements that are the nth-child of their parent.

### Selector: :odd Selector
   Selects odd elements, zero-indexed. See also even.

### Selector: :only-child Selector
   Selects all elements that are the only child of their parent.

### Selector: :parent Selector
   Select all elements that are the parent of another element,
   including text nodes.

### Selector: :password Selector
   Selects all elements of type password.

### Selector: :radio Selector
   Selects all elements of type radio.

### Selector: :reset Selector
   Selects all elements of type reset.

### Selector: :selected Selector
   Selects all elements that are selected.

### Selector: :submit Selector
   Selects all elements of type submit.

### Selector: :text Selector
   Selects all elements of type text.

### Selector: :visible Selector
   Selects all elements that are visible.
