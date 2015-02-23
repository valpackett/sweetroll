# sweetroll [![ISC License](https://img.shields.io/badge/license-ISC-red.svg?style=flat)](https://tldrlegal.com/license/-isc-license)

A personal website / [#IndieWeb] engine with curved swords. *Curved! Swords!*

Written in [Haskell], uses Git (via [gitson]) for storage and [pandoc] for markup.

*Privacy notice*: if you post your git repo on a public host like GitHub or Bitbucket, your "deleted" entries are not really deleted.

## Libraries I made for this project

- [gitson], a git-backed storage engine
- [http-link-header], a parser for the Link header (RFC 5988)
- [pcre-heavy], a usable regular expressions library
- [microformats2-types], the type definitions for Microformats 2
- [microformats2-parser], the Microformats 2 parser

[#IndieWeb]: http://indiewebcamp.com
[Haskell]: http://haskell.org
[gitson]: https://github.com/myfreeweb/gitson
[http-link-header]: https://github.com/myfreeweb/http-link-header
[pcre-heavy]: https://github.com/myfreeweb/pcre-heavy
[microformats2-types]: https://github.com/myfreeweb/microformats2-types
[microformats2-parser]: https://github.com/myfreeweb/microformats2-parser
[pandoc]: http://johnmacfarlane.net/pandoc/

## WORK IN PROGRESS

It's not feature complete, but it hasn't crashed on me ever :D
The current version is 0.0.0.

I'm running it on [my website](https://unrelenting.technology).

### Massive TODO list

- [x] basic display of entries :D
- [x] [micropub](http://indiewebcamp.com/micropub) posting
- [x] [token-endpoint](http://indiewebcamp.com/token-endpoint)
- [x] previous/next [navigation](http://indiewebcamp.com/navigation)
- [x] aggressive slugify / use id/datetime as slug for notes
- [x] [POSSE](http://indiewebcamp.com/POSSE) to App.net
- [x] POSSE to Twitter
- [x] autolinking in notes
- [x] a 404 page
- [x] pagination
- [ ] archive pages
- [ ] tags (add CSV support to gitson for storing stuff like this)
- [ ] micropub updating and deleting
- [x] sending [webmention](http://indiewebcamp.com/webmention)
- [ ] receiving webmention
- [ ] storing and sending [vouch](http://indiewebcamp.com/vouch) with webmention
- [x] microformats2 parser
- [ ] microformats2 based [comments-presentation](http://indiewebcamp.com/comments-presentation)
- [ ] microformats2 parse reply target, store as cite instead of just a linktype definitions for
- [ ] special POSSE for likes and reposts to App.net/Twitter
- [ ] Atom feed
- [ ] PubSubHubbub (w/ configurable delay because privacy and general {notice typo - delete - post again} things)
- [ ] posting [photos](http://indiewebcamp.com/photos)
- [x] posting in other formats supported by pandoc
- [ ] displaying in other formats supported by pandoc
- [ ] custom non-entry html pages
- [ ] caching headers
- [ ] automatic git push when posting (in gitson)
- [ ] support for running on PaaS (automatic git pull when starting), [Heroku Button](https://blog.heroku.com/archives/2014/8/7/heroku-button) + GitHub user friendly setup process
- [ ] built-in TLS server, since we depend on `tls` already because of the client

## License

Copyright 2014-2015 Greg V <greg@unrelenting.technology>
Available under the ISC license, see the `COPYING` file
