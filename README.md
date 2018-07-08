# sweetroll [![unlicense](https://img.shields.io/badge/un-license-green.svg?style=flat)](http://unlicense.org)

A powerful engine for [your personal website].

- lets you publish many [kinds of content]
  - notes (microblog posts), articles (big blog posts), replies, reposts, likes, quotations, bookmarks, reviews…
- supports categories, tags, full text search, content warnings, photos (with Exif metadata display), videos, audio files, private posts (drafts), automatic offline in-browser caching (ServiceWorkers), color theme customization
- lets you talk to other IndieWeb-style websites by sending and receiving [Webmentions] (including [Salmentions] for threaded conversations)
- lets you use [Micropub] to edit your site (post, update, delete, undelete)
  - has a media endpoint for file uploads that optimizes images, extracts Exif metadata, and works on AWS Lambda + S3
  - lets you use Markdown for post text, with nice [code highlighting](https://github.com/ben-eb/remark-highlight.js) and [math rendering](https://github.com/Rokt33r/remark-math)
  - the included admin panel (micro-panel) itself uses Micropub
- sends [WebSub] \(formerly PubSubHubbub) notifications on new posts (for [readers])
- uses [Granary] to provide content as Atom, ActivityStreams 2.0 and JSON Feed (with WebSub notifications too)
- represents *all the things* as [Microformats2] objects
- uses PostgreSQL with the [mf2sql] schema for storage
- consists of services written in [Haskell] and [Node.js]

I'm running it on [my website](https://unrelenting.technology).

[your personal website]: https://indieweb.org
[kinds of content]: https://indieweb.org/posts
[Haskell]: https://www.haskell.org
[Node.js]: https://nodejs.org/en/
[mf2sql]: https://github.com/myfreeweb/mf2sql

[Microformats2]: http://microformats.org/wiki/microformats2
[Micropub]: https://indieweb.org/micropub
[Webmentions]: https://indieweb.org/webmention
[Salmentions]: https://indieweb.org/Salmention
[WebSub]: https://indieweb.org/WebSub
[readers]: https://indieweb.org/readers
[Granary]: https://granary-demo.appspot.com

## Installation

*Installing Sweetroll on a server requires some UNIX sysadmin skills. If you can't do it, ask your friends for help or check out [other IndieWeb projects](https://indieweb.org/projects): some of them have hosted versions, some run on shared PHP hosting.*

[Read the Docs](https://sweetroll.readthedocs.io/en/latest/) for installation instructions!

## TODO

- frontend
  - [ ] webhooks (e.g. pushover notifications) on new/updated mentions, config in `site-settings`
- micropub
  - [ ] draft flag → draft tag and private acl
  - [ ] syndication to other micropub endpoints e.g. for silo.pub
- webmention
  - [ ] [link removal webmentions](https://webmention.rocks/update/2)
  - [ ] reverify/refetch to update user profiles and stuff
  - [ ] moderation tools
    - [ ] different modes in config: allow all (except blocked), allow known good domains (e.g. domains replied to), premoderate all, turn off webmention
    - [ ] [blocking](https://indieweb.org/block) domains
      - [ ] sharing block lists
  - [ ] deduplicate syndicated replies
  - [ ] vouch
  - [ ] private webmention

## License

This is free and unencumbered software released into the public domain.  
For more information, please refer to the `UNLICENSE` file or [unlicense.org](http://unlicense.org).
