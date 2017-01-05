# sweetroll [![unlicense](https://img.shields.io/badge/un-license-green.svg?style=flat)](http://unlicense.org)

A website engine for [the indie web] with curved swords. *Curved! Swords!*

- uses [Git]+[JSON] for storage
- supports [Micropub] for posting, updating, deleting and undeleting (incl. a media endpoint for file uploads)
- allows posting in [CommonMark Markdown]
- sends and receives [Webmentions], including [Salmentions]
- supports the webmention-to-[syndication] / Syndicate by Reference process ([Bridgy Publish])
- sends [PubSubHubbub] notifications on new posts (for [readers])
- supports [indie-config]
- has a [JSON Web Tokens]-based [token-endpoint]
- extensible with JavaScript plugins (included example: [Pushover] notifications)
- written in [Haskell]

I'm running it on [my website](https://unrelenting.technology).

*Privacy notice*: if you expose your website's **git repo** publicly, your "deleted" entries are not really deleted.

[the indie web]: https://indieweb.org
[Git]: https://git-scm.com
[JSON]: http://json.org
[JSON Web Tokens]: https://jwt.io
[CommonMark Markdown]: http://commonmark.org
[Pushover]: https://pushover.net
[Haskell]: https://www.haskell.org

[Micropub]: https://indieweb.org/micropub
[Webmentions]: https://indieweb.org/webmention
[Salmentions]: https://indieweb.org/Salmention
[syndication]: https://indieweb.org/POSSE
[Bridgy Publish]: https://brid.gy/about#publishing
[PubSubHubbub]: https://indieweb.org/PubSubHubbub
[readers]: https://indieweb.org/readers
[indie-config]: https://indieweb.org/indie-config
[token-endpoint]: https://indieweb.org/token-endpoint

## Usage

*Installing Sweetroll on a server requires some UNIX sysadmin skills. If you can't do it, ask your friends for help or check out [other IndieWeb projects](https://indieweb.org/projects): some of them have hosted versions, some run on shared PHP hosting.*

First, you need to get a binary of Sweetroll.
I haven't uploaded any yet, so you have to build from source.

### Buliding from source

- get [stack] \(from your OS package manager or `cabal install stack`)
- get [bower] \(get node/[npm](https://www.npmjs.com) from your OS package manager, `npm install -g bower`)
- `git clone` the repo
- `cd` into it
- `bower install`
- `stack build`

When it's done, it says where it put the binary (something like `.stack-work/install/your-platform/some/versions/.../bin`).

(NOTE: to get a smaller resulting binary, [enable split-objs globally in stack](https://github.com/commercialhaskell/stack/issues/1284#issuecomment-196639511), remove snapshots once and rebuild. And by smaller I do mean SMALLER. About 22M instead of 56M.)

### Running on a server

Copy the binary to the server (using `scp`, usually).

Create a user account on the server (for example, `sweetroll`).

Create a directory where your website's content files will be, and run `git init` there.

Make sure the user has read and write permissions on the directory.

And configure your favorite service management program (don't forget to replace everything with your values!) to run Sweetroll as that user!

Here's an example for [runit](http://smarden.org/runit/index.html):

```bash
#!/bin/sh

umask g+w
exec chpst -u sweetroll /home/sweetroll/.local/bin/sweetroll
        --https \ # this means HTTPS is *working*! i.e. you have it set up on your reverse proxy!
        --protocol=unix \ # will run on /var/run/sweetroll/sweetroll.sock by default; you can override with --socket
  # or: --protocol=http --port=3030 \
        --domain=unrelenting.technology \ # your actual domain!
        --repo="/home/sweetroll/repo" \ # the site directory! don't forget to run `git init` inside of it first
        --secret="GENERATE YOUR LONG PSEUDORANDOM VALUE!...2MGy9ZkKgzexRpd7vl8" 2>&1
```

(Use something like `head -c 1024 < /dev/random | openssl dgst -sha512` to get the random value for the `secret`. No, not dynamically in the script. Copy and paste the value into the script. Otherwise you'll be logged out on every restart.)

Putting a reverse proxy in front of Sweetroll is not *required*, but you might want to run other software at different URLs, etc.
By the way, Sweetroll supports socket activation.
I wrote [soad](https://github.com/myfreeweb/soad) to run Sweetroll and other services on demand and shut them down when not used for some time.

After you start Sweetroll, open your new website.
It should write the default configuration to `conf/sweetroll.json` in your site directory.
Edit that file, you probably want to change some options.

Create a `templates` directory in your site directory.
You can override the HTML templates you see in this repo's `templates` directory with your own using your `templates` directory.
The templating engine is embedded JavaScript via [lodash](http://lodash.com)'s `_.template`.
You need to put your h-card and rel-me markup into `templates/author.ejs`.

Restart Sweetroll after any changes to the config file or the templates.

Use Micropub clients like [Micropublish](https://micropublish.herokuapp.com) and [Quill](https://quill.p3k.io) to post.

## Development

Use [stack] to build (and [bower] to get front-end dependencies).  
Use ghci to run tests and the server while developing (see the `.ghci` file).

The `:serve` command in ghci runs the server in test mode, which means you don't need to authenticate using IndieAuth.

```bash
$ bower install

$ stack build

$ stack test

$ (mkdir /tmp/sroll && cd /tmp/sroll && git init)

$ stack ghci --no-load
:serve

$ http -f post localhost:3000/login x=x | sed -Ee 's/.*access_token=([^&]+).*/\1/' > token

$ http -f post localhost:3000/micropub "Authorization: Bearer $(cat token)" h=entry content=HelloWorld
```

(the `http` command in the examples is [HTTPie](https://github.com/jkbrzt/httpie))


## Libraries I made for this project

- [gitson](https://github.com/myfreeweb/gitson), a git-backed storage engine
- [pcre-heavy](https://github.com/myfreeweb/pcre-heavy), a usable regular expressions library
- [http-link-header](https://github.com/myfreeweb/http-link-header), a parser for the Link header (RFC 5988)
- [microformats2-parser](https://github.com/myfreeweb/microformats2-parser), a Microformats 2 parser
- [indieweb-algorithms](https://github.com/myfreeweb/indieweb-algorithms), a collection of implementations of algorithms like [authorship](http://indieweb.org/authorship) and link discovery
- [hs-duktape](https://github.com/myfreeweb/hs-duktape), Haskell bindings to [duktape](http://duktape.org), a lightweight ECMAScript (JavaScript) engine

## TODO

- html/frontend/templating
  - [ ] support [WebFinger](https://webfinger.net) with HTML as the source of truth + additional links from config e.g. for [remoteStorage](https://remotestorage.io)
  - [ ] figure out URL/canonical/etc. handling for alternative networks & mirrors like .onion & IPFS -- including webmentions!
  - [ ] custom non-entry html pages
  - [ ] [archive](https://indieweb.org/archive) pages by month. see footer of anildash.com
  - [ ] sitemap.xml
  - [ ] indieweb-components: a component for a Medium-style popup on selection that offers a fragmention link and (?) indie-config repost-quote-something (look how [selection-sharer](https://github.com/xdamman/selection-sharer) works on mobile!! but probably should look the same just at the opposite direction than iOS's popup)
- event system
  - [ ] real-time page updates with Server-Sent Events (make a Web Component that will show the update button)
  - [ ] static mode: on these events, regenerate website pages into static HTML files (and serve them for better performance)
- plugin api
  - [ ] cron-style scheduling
  - [ ] post manipulation
  - [ ] HTTP request (webhook) handling
  - [ ] example plugin: Telegram bot (posting, webmention notifications, responding to them, deleting them, etc.)
- webmention
  - [ ] reverify/refetch to update user profiles and stuff
  - [ ] moderation tools
    - [ ] different modes in config: allow all (except blocked), allow known good domains (e.g. domains replied to), premoderate all, turn off webmention
    - [ ] [blocking](https://indieweb.org/block) domains
      - [ ] sharing block lists
  - [ ] deduplicate threaded replies like [there](https://unrelenting.technology/replies/2015-09-06-20-29-54) (a reply to both my post and another reply) -- maybe that's already happening? need to test
  - [ ] deduplicate syndicated replies
- micropub
  - [ ] check auth scopes
- [ ] indieweb-algorithms?: ensure the person you're replying to *never* gets picked up you when you're replying (caught in test without own h-card) (what?)
- [ ] tags? (kill the difference between categories and tags? // use symlinks to add to multiple categories/tags)
- [ ] extract a `WebPrelude` package: `Sweetroll.Prelude`, `Sweetroll.HTTPClient`, `formToObject`, more stuff

## License

This is free and unencumbered software released into the public domain.  
For more information, please refer to the `UNLICENSE` file or [unlicense.org](http://unlicense.org).

[stack]: https://github.com/commercialhaskell/stack
[bower]: http://bower.io
