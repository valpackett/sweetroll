# sweetroll [![unlicense](https://img.shields.io/badge/un-license-green.svg?style=flat)](http://unlicense.org)

**Big changes in progress!**

A website engine for [the indie web] with curved swords. *Curved! Swords!*

- (WIP) uses PostgreSQL with the [mf2sql] schema for storage
- supports [Micropub] for posting, updating, deleting and undeleting (incl. a media endpoint for file uploads)
- allows posting in [CommonMark Markdown]
- sends and receives [Webmentions], including [Salmentions]
- supports the webmention-to-[syndication] / Syndicate by Reference process ([Bridgy Publish])
- sends [WebSub] \(formerly PubSubHubbub) notifications on new posts (for [readers])
- supports [indie-config]
- has a [JSON Web Tokens]-based [token-endpoint]
- written in [Haskell]

I'm running it on [my website](https://unrelenting.technology).

[the indie web]: https://indieweb.org
[JSON Web Tokens]: https://jwt.io
[CommonMark Markdown]: http://commonmark.org
[Pushover]: https://pushover.net
[Haskell]: https://www.haskell.org

[Micropub]: https://indieweb.org/micropub
[Webmentions]: https://indieweb.org/webmention
[Salmentions]: https://indieweb.org/Salmention
[syndication]: https://indieweb.org/POSSE
[Bridgy Publish]: https://brid.gy/about#publishing
[WebSub]: https://indieweb.org/PubSub
[readers]: https://indieweb.org/readers
[indie-config]: https://indieweb.org/indie-config
[token-endpoint]: https://indieweb.org/token-endpoint

## Usage

*Installing Sweetroll on a server requires some UNIX sysadmin skills. If you can't do it, ask your friends for help or check out [other IndieWeb projects](https://indieweb.org/projects): some of them have hosted versions, some run on shared PHP hosting.*

First, you need to get a binary of Sweetroll.
I haven't uploaded any yet, so you have to build from source.

### Buliding from source

- get [stack] \(from your OS package manager or `cabal install stack`)
- `git clone` the repo
- `cd` into it
- `stack build`

When it's done, it says where it put the binary (something like `.stack-work/install/your-platform/some/versions/.../bin`).

(NOTE: to get a smaller resulting binary, [enable split-objs globally in stack](https://github.com/commercialhaskell/stack/issues/1284#issuecomment-196639511), remove snapshots once and rebuild. And by smaller I do mean SMALLER. Linking it will take more time though. Unless you use lld, the LLVM linker, which is incredibly fast. Something like this: `stack build --fast --ghc-options "-pgml clang39 -optl -fuse-ld=lld"`)

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

(Use something like `head -c 1024 < /dev/random | openssl dgst -sha512` to get the random value for the `secret`. No, not dynamically in the script. Copy and paste the value into the script. Otherwise you'll be logged out on every restart.)

By the way, Sweetroll supports socket activation.
I wrote [soad](https://github.com/myfreeweb/soad) to run Sweetroll and other services on demand and shut them down when not used for some time.

Use Micropub clients like [Micropublish](https://micropublish.net) and [Quill](https://quill.p3k.io) to post.

## Development

Use [stack] to build.  
Use ghci to run the server while developing (see the `.ghci` file).

The `:serve` command in ghci runs the server in test mode, which means you don't need to authenticate using IndieAuth.

```bash
$ stack build

$ (mkdir /tmp/sroll && cd /tmp/sroll && git init)

$ stack ghci --no-load
:serve

$ http -f post localhost:3000/login x=x | sed -Ee 's/.*access_token=([^&]+).*/\1/' > token

$ http -f post localhost:3000/micropub "Authorization: Bearer $(cat token)" h=entry content=HelloWorld
```

(the `http` command in the examples is [HTTPie](https://github.com/jkbrzt/httpie))


## Libraries I made for this project

- [pcre-heavy](https://github.com/myfreeweb/pcre-heavy), a usable regular expressions library
- [http-link-header](https://github.com/myfreeweb/http-link-header), a parser for the Link header (RFC 5988)
- [microformats2-parser](https://github.com/myfreeweb/microformats2-parser), a Microformats 2 parser
- [indieweb-algorithms](https://github.com/myfreeweb/indieweb-algorithms), a collection of implementations of algorithms like [authorship](http://indieweb.org/authorship) and link discovery

### but stopped using here

- [gitson](https://github.com/myfreeweb/gitson), a git-backed storage engine
- [hs-duktape](https://github.com/myfreeweb/hs-duktape), Haskell bindings to [duktape](http://duktape.org), a lightweight ECMAScript (JavaScript) engine

## TODO

- [ ] gitson → postgres
- [ ] do something about category decisions
- [ ] multi domain support (use host header everywhere)
  - JWT iss = domain, authorize posting/etc. based on that
  - [ ] storing (per domain) configuration options in microformats pages
- [ ] authorization based on JWT scopes for micropub etc.
- webmention
  - [ ] reverify/refetch to update user profiles and stuff
  - [ ] moderation tools
    - [ ] different modes in config: allow all (except blocked), allow known good domains (e.g. domains replied to), premoderate all, turn off webmention
    - [ ] [blocking](https://indieweb.org/block) domains
      - [ ] sharing block lists
  - [ ] deduplicate threaded replies like [there](https://unrelenting.technology/replies/2015-09-06-20-29-54) (a reply to both my post and another reply) -- maybe that's already happening? need to test
  - [ ] deduplicate syndicated replies
  - [ ] queue up mention processing requests in the database ([`SELECT FOR UPDATE SKIP LOCKED`](http://blog.2ndquadrant.com/what-is-select-skip-locked-for-in-postgresql-9-5/), probably create that as a library) instead of sync processing
- [ ] media endpoint upload to openstack/s3
- [ ] indieweb-algorithms?: ensure the person you're replying to *never* gets picked up you when you're replying (caught in test without own h-card) (what?)
- [ ] extract a `WebPrelude` package: `Sweetroll.Prelude`, `Sweetroll.HTTPClient`, `formToObject`, more stuff

## License

This is free and unencumbered software released into the public domain.  
For more information, please refer to the `UNLICENSE` file or [unlicense.org](http://unlicense.org).

[mf2sql]: https://github.com/myfreeweb/mf2sql
[stack]: https://github.com/commercialhaskell/stack
