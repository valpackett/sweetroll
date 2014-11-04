# sweetroll [![Apache License 2.0](https://img.shields.io/badge/license-Apache%202.0-brightgreen.svg?style=flat)](https://www.tldrlegal.com/l/apache2)

A personal website / [#IndieWeb] engine with curved swords. *Curved! Swords!*

Written in [Haskell], uses Git (via [gitson]) for storage and [pandoc] for markup.

*Privacy notice*: if you post your git repo on a public host like GitHub or Bitbucket, your "deleted" entries are not really deleted.

[#IndieWeb]: http://indiewebcamp.com
[Haskell]: http://haskell.org
[gitson]: https://github.com/myfreeweb/gitson
[pandoc]: http://johnmacfarlane.net/pandoc/

## WORK IN PROGRESS

This is not a stable release!
The current version is 0.0.0.

**Do not touch it yet!**
But you can look ;-)
I'm testing it on [my website](https://unrelenting.technology).

### Massive TODO list

- [x] basic display of entries :D
- [x] [micropub](http://indiewebcamp.com/micropub) posting
- [x] [token-endpoint](http://indiewebcamp.com/token-endpoint)
- [x] previous/next [navigation](http://indiewebcamp.com/navigation)
- [x] aggressive slugify / use id/datetime as slug for notes
- [x] [POSSE](http://indiewebcamp.com/POSSE) to App.net
- [ ] POSSE to Twitter
- [ ] autolinking in notes
- [ ] a 404 page
- [ ] pagination
- [ ] archive pages
- [ ] tags (add CSV support to gitson for storing stuff like this)
- [ ] micropub updating and deleting
- [ ] sending [webmention](http://indiewebcamp.com/webmention)
- [ ] receiving webmention (with some kind of anti [spam](http://indiewebcamp.com/spam), optional premoderation)
- [ ] microformats2 parser (separate library)
- [ ] microformats2 based [comments-presentation](http://indiewebcamp.com/comments-presentation)
- [ ] Atom + PubSubHubbub (configurable delay for PuSH because privacy and general {notice typo - delete - post again} things)
- [ ] posting [photos](http://indiewebcamp.com/photos)
- [ ] posting and displaying in other formats supported by pandoc
- [ ] custom non-entry html pages
- [ ] caching headers
- [ ] automatic git push when posting
- [ ] support for running on PaaS (automatic git pull when starting), [Heroku Button](https://blog.heroku.com/archives/2014/8/7/heroku-button) + GitHub user friendly setup process

## License

Copyright 2014 Greg V <greg@unrelenting.technology>

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
