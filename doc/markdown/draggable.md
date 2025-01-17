<!-- dash: #draggable | Element | ###:Section -->



## Draggable Element - #draggable {}

  The draggable element allows you to make a block of Nitrogen elements
  draggable by the user.

  Combine the draggable element with the droppable element to allow drag and
  drop behavior.

### Usage

```erlang
   #draggable { tag=drag1, clone=true, revert=false, handle=grip, body=[
	 #image { class=grip, url="/images/handle.png" },
	 #span { text="This is a draggable block." }
   ]}

```

### Attributes

   * `tag` (Erlang term) - The drag term to pass into the `drop_event/2`
	event.

   * `body` (Nitrogen elements) - The elements that will be draggable.

   * `group` (atom or string) - The name of this drag group, for use in the
	droppable element's accept_groups attribute.

   * `handle` (atom or string) - The class of the handle element on the page.
	Then handle will then be the part of the element that will be clicked and
	dragged around.

   * `clone` (boolean) - If true, the element will be cloned in the DOM while
	dragged. If false, the element will be detached from the DOM while dragging.

   * `revert` (boolean) - If true, the element will be reverted back to its
	original position if the drop fails.

   * `scroll` (boolean) - If true, the window or container will scroll when
	the item is dragged to the edge.

   * `distance` (integer) - Set to the minimum number of pixels the cursor
	must move with the mouse down before the drag actually begins. (default: 3)

   * `container` (atom or string) - How you wish to contain the draggable
	element. Common containers are `window`, `document`, or `parent`.
	Otherwise, it can be specified in the form of a jquery selector. See the
	documentation on the "Containment" option for the jquery Draggable element
	for more information.

   * `zindex` (integer) - The z-index of the element to be dragged around the
	page.

   * `options` (proplist) - A list of additional options to be passed to the
	draggable function. (See the jQuery Draggable Documentation below for the
	complete list of options).

### See Also

 *  [Droppable Element](./droppable.md)

 *  [Drag and Drop Demo](http://nitrogenproject.com/demos/dragdrop)

 *  [jQuery Draggable Documentation](http://api.jqueryui.com/draggable/)
