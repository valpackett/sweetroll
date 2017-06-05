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
- represents *all the things* as [Microformats2] objects
- uses PostgreSQL with the [mf2sql] schema for storage
- consists of services written in [Haskell] and [Node.js]

I'm running it on [my website](https://unrelenting.technology).

[your personal website]: https://indieweb.org
[kinds of content]: https://indieweb.org/posts
[Haskell]: https://www.haskell.org
[Node.js]: https://nodejs.org/en/

[Microformats2]: http://microformats.org/wiki/microformats2
[Micropub]: https://indieweb.org/micropub
[Webmentions]: https://indieweb.org/webmention
[Salmentions]: https://indieweb.org/Salmention
[WebSub]: https://indieweb.org/WebSub
[readers]: https://indieweb.org/readers

## Usage

*Installing Sweetroll on a server requires some UNIX sysadmin skills. If you can't do it, ask your friends for help or check out [other IndieWeb projects](https://indieweb.org/projects): some of them have hosted versions, some run on shared PHP hosting.*

### Running on a server

Set up a Postgres database with the [mf2sql] schema.

Copy the binary to the server (using `scp`, usually).

Create a user account on the server (for example, `sweetroll`).

And configure your favorite service management program (don't forget to replace everything with your values!) to run Sweetroll as that user!

Here's an example for [runit](http://smarden.org/runit/index.html):

```bash
#!/bin/sh

umask g+w
export SWEETROLL_HTTPS_WORKS=True # this means HTTPS is *working*! i.e. you have it set up on your reverse proxy!
export SWEETROLL_DOMAIN=unrelenting.technology # your actual domain!
export SWEETROLL_SECRET="GENERATE YOUR LONG PSEUDORANDOM VALUE!...2MGy9ZkKgzexRpd7vl8" 
exec chpst -u sweetroll /home/sweetroll/.local/bin/sweetroll
        --protocol=unix --socket=/var/run/sweetroll/sock \
  # or: --protocol=http --port=3030 \
        2>&1
```

TODO same for frontend

(Use something like `head -c 1024 < /dev/random | openssl dgst -sha512` to get the random value for the `secret`. No, not dynamically in the script. Copy and paste the value into the script. Otherwise you'll be logged out on every restart.)

## TODO

- frontend
  - [ ] bring back the atom feed
  - [ ] webhooks (e.g. pushover notifications) on new/updated mentions, config in `site-settings`
- micropub
  - [ ] JWT scopes authorization
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

[mf2sql]: https://github.com/myfreeweb/mf2sql
