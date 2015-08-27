# sweetroll [![unlicense](https://img.shields.io/badge/un-license-green.svg?style=flat)](http://unlicense.org)

A website engine for [the indie web] with curved swords. *Curved! Swords!*

- uses [Git]+[JSON] for storage
- supports [Micropub] for posting
- sends [Webmentions]
- supports [indie-config]
- has a [JSON Web Tokens]-based [token-endpoint]
- allows posting in [CommonMark Markdown] and other markup languages (powered by [Pandoc])
- written in [Haskell]

I'm running it on [my website](https://unrelenting.technology).

*Privacy notice*: if you post your git repo on a public host like GitHub or Bitbucket, your "deleted" entries are not really deleted.

[the indie web]: https://indiewebcamp.com
[Git]: https://git-scm.com
[JSON]: http://json.org
[JSON Web Tokens]: http://jwt.io
[CommonMark Markdown]: http://commonmark.org
[Pandoc]: http://johnmacfarlane.net/pandoc/
[Haskell]: http://haskell.org

[Micropub]: https://indiewebcamp.com/micropub
[Webmentions]: https://indiewebcamp.com/webmention
[indie-config]: https://indiewebcamp.com/indie-config
[token-endpoint]: https://indiewebcamp.com/token-endpoint


## Libraries I made for this project

- [gitson](https://github.com/myfreeweb/gitson), a git-backed storage engine
- [pcre-heavy](https://github.com/myfreeweb/pcre-heavy), a usable regular expressions library
- [http-link-header](https://github.com/myfreeweb/http-link-header), a parser for the Link header (RFC 5988)
- [microformats2-parser](https://github.com/myfreeweb/microformats2-parser), a Microformats 2 parser
- [indieweb-algorithms](https://github.com/myfreeweb/indieweb-algorithms), a collection of implementations of algorithms like [authorship](http://indiewebcamp.com/authorship) and link discovery

## TODO

- [x] add configurable webmention-to-syndication (brid.gy publish)
- [ ] archive pages, ie. unpaginated pages
- [ ] tags (add CSV support to gitson for storing stuff like this)
- [ ] micropub updating and deleting
- [ ] receiving webmention
- [ ] microformats2 based [comments-presentation](http://indiewebcamp.com/comments-presentation)
- [ ] hashcash in webmentions
- [ ] Atom feed
- [ ] posting [photos](http://indiewebcamp.com/photos) (note: we already depend on `JuicyPixels` somehow)
- [ ] custom non-entry html pages
- [ ] [JS](https://github.com/myfreeweb/hs-duktape) hooks for events like posting and webmentions (API: a Sweetroll object which is an EventEmitter and also has config/secrets getters; should be possible to make HTTP requests to e.g. send webmention notifications)
- [ ] built-in TLS server, since we depend on `tls` already because of the client
- [ ] [ede](https://github.com/brendanhay/ede) templates instead of simple-templates (??)

## License

This is free and unencumbered software released into the public domain.  
For more information, please refer to the `UNLICENSE` file or [unlicense.org](http://unlicense.org).
