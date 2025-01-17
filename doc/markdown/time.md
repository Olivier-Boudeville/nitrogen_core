<!-- dash: #time | Element | ###:Section -->



## Time Element - #time {}

  Produces the HTML5 Time element. The time element serves as a content wrapper
  for any dates or times you might need to use and can be intelligently handled
  by the browser.

### Usage

```erlang
   #time{text="2013-06-13 16:00Z", text="June 6th at 4:00GMT"}

```

```erlang
   #article{body=[
	  #h1{text="My Favorite Fruit"},
	  "Apples, Oranges, and Bananas are my favorite",
	  #html5_footer{body=[
		 "Published ",
		 #time{text="Last Week", datetime="2013-08-20"},
		 #br{},
		 "Last Modifed ",
		 #time{text="Yesterday", datetime="2013-08-27"}
	  ]}
   ]}

```

### Attributes

   * `datetime` (String) - Date/Time data. Can be a string formatted any
	  number of ways: `2007-01-01`, or a year `2013`, or just a day and month
	  `01-11` (January 11th any year), or time in 24-hour format (`9:00`), or time
	  with timezone information (`09:00Z` for GMT or `09:00-5:00` to indicate 5
	  hours behind GMT). Please see the "About the Time Element" in "See Also"
	  below for more options. Note, that if this attribute is not set, then the
	  contents of text and/or body will be used as the semantic meaning for the
	  date time, and interpreted literally.

   * `text` (String) - The text of the time element. Could be a date, time,
	  or textual representation (like last week, last year, coming soon, whatever.
	  Note, that if the `datetime` attribute is not set, then the datetime used
	  will be the contents of `text` and/or `body`.

 *  html_encode (*boolean or whites or function arity 1*)  :: Whether or not
	  to safely encode the text attribute to HTML, and by what manner.
	  (Default: `true`)

   * `body` (Nitrogen Elements) - The body of the time element.

   * `role` (String) - The role of the HTML5 Time element.

### Other Information

   If you need to do date-time conversion, including dealing with timezones, we
   recommend the use of [qdate](https://github.com/choptastic/qdate), the
   Erlang date and timezone utility. Eventually, `qdate` will likely become a
   dependency for Nitrogen to integrate its functionality into the `#time`
   element, but for now, you'll have to work with the `#time` element's more
   rudimentary form.

### See Also

 *  [Base element](./element_base.md)

 *  [About the HTML Time Element](http://www.brucelawson.co.uk/2012/best-of-time/)

 *  [Erlang Date and Timezone handling with qdate](https://github.com/choptastic/qdate)
