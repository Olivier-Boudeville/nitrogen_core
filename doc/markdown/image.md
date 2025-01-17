<!-- dash: #image | Element | ###:Section -->



## Image Element - #image {}

   The image element produces an HTML image. (an HTML `<img>` tag).

### Usage

```erlang
#image { image="http://hostname.com/path/to/image.png" }
```
or
```erlang
 #image { id=my_image, image="/path/to/local/image.png" }

```

### Attributes

* `image` (string) - Set the HTML `src` attribute of the image. This can
  be either an absolute URL (`http://hostname.com/path/to/image.png`) or a
  relative URL (`/path/to/image`).

* `alt` (string) - Set he Alt-text (that is, the `alt` attribute of the
  image tag).

### To change the path of an image in a postback

* `wf:set(my_image, "/path/to/new/image.png")` - will change the `image`
  attribute of the `#image` element with `id` set to `my_image` to the provided
  string.

### About Static Root and Relative URLs

If using a relative URL (that is, one not starting with `"http://"` or
`"https://"`), then it must be understood how relative paths are interpreted in
HTML.  The final part of any URL after the last slash is considered the "page",
the section before the last slash is considered the current directory. So
assuming your page is located at `http://localhost/my/awesome/webpage`, the
"page" is "webpage" and its containing directory is `"/my/awesome"`.

Here are a few practical examples using the URL above examples:
 *  `image` set to `someimage.png`, the browser will request the image
	 from `http://localhost/my/awesome/someimage.png`.

 *  `image` set to `../otherimage.png`, the browser will request the image
	 from `http://localhost/my/otherimage.png`

 *  `image` set to `/images/profile_pic.png`, the browser will request the
	 image from `http://localhost/images/profile_pic.png`. Note how, just like
	 with filesystem directories, preceeding the path with a slash (`/`) will
	 remove any notion of "relative to the page", and will instead request from
	 the root of the server. **(This is the recommended way of referring to
	 images in Nitrogen)**

 *  `image` set to `images/profile_pic.png`, the browser will request the
	 image from `http://localhost/my/awesome/images/profile_pic.png`. **(Note:
	 This is a common typo resulting in a bug.)**

By default, Nitrogen has a handful of "static paths", which Nitrogen will then
treat as "special", and instead of trying to route the request to a module,
instead serves the files directly.  By default, these paths are "/images",
"/nitrogen", "/css", and "/js", which are then retrieved from the static root
on your filesystem as "site/static".

With this said, using the standard configuration, an image requesting a
relative path like `"/images/user_profiles/profilepic12345.png"` will actually
serve the image from your Nitrogen installation from
`"site/static/images/user_profiles/profilepic12345.png"`

To customize this for your own application, in Nitrogen 2.3, you can edit the
`static_paths` option in `etc/simple_bridge.config`, and prior to Nitrogen 2.3,
you must edit the server-specific configuration file in `etc/` such as
`etc/cowboy.config`. See the [configuration docs](config.md) for more.

### See Also

 *  [base element](./element_base.md)
 *  [Configuration Options](./config.md)
 *  [wf:set/2 in API docs](./api.md)
