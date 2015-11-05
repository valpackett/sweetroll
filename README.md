# sweetroll [![unlicense](https://img.shields.io/badge/un-license-green.svg?style=flat)](http://unlicense.org)

A website engine for [the indie web] with curved swords. *Curved! Swords!*

- uses [Git]+[JSON] for storage
- supports [Micropub] for posting
- sends [Webmentions]
- supports the webmention-to-[syndication] / Syndicate by Reference process ([Bridgy Publish])
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
[syndication]: https://indiewebcamp.com/POSSE
[Bridgy Publish]: https://brid.gy/about#publishing
[indie-config]: https://indiewebcamp.com/indie-config
[token-endpoint]: https://indiewebcamp.com/token-endpoint

## Usage

*Installing Sweetroll on a server requires basic UNIX sysadmin skills. If you can't do it, ask your friends for help or check out [other IndieWeb projects](https://indiewebcamp.com/projects): some of them have hosted versions, some run on shared PHP hosting.*

First, you need to get a binary of Sweetroll.
I haven't uploaded any yet, so you have to build from source.

Get [stack](https://github.com/commercialhaskell/stack), `git clone` the repo, `cd` into it and do a `stack build`.
When it's done, it says where it put the binary (something like `.stack-work/install/your-platform/some/versions/.../bin`).

*Note: running the binary might require `libgmp`.*

Then you need to pick a directory where your website's content files will be, and run `git init` there.

Write a script called something like `run-sweetroll`, don't forget to replace everything with your values:

```bash
#!/bin/sh

umask g+w
exec /home/greg/Stuff/bin/freebsd-amd64/sweetroll
        --https \ # this means HTTPS is *working*! i.e. you have it set up on your reverse proxy!
        --protocol=unix \ # will run on /var/run/sweetroll/sweetroll.sock by default; you can override with --socket
  # or: --protocol=http --port=3030 \
        --domain=unrelenting.technology \ # your actual domain!
        --repo="/home/greg/Stuff/website" \ # the site directory! don't forget to run `git init` inside of it first
        --secret="GENERATE YOUR LONG PSEUDORANDOM VALUE!...2MGy9ZkKgzexRpd7vl8"
```

(Use something like `head -c 1024 < /dev/random | openssl dgst -sha512` to get the random value for the `secret`. No, not dynamically in the script. Copy and paste the value into the script. Otherwise you'll be logged out on every restart.)

Run that script with [supervisord](http://supervisord.org) or whatever you prefer.
Don't run as root, run as a separate user that has read-write access to the site directory.

Putting a reverse proxy in front of Sweetroll is not *required*, but you might want to run other software at different URLs, etc.
I wrote [443d](https://github.com/myfreeweb/443d) as a lightweight alternative to nginx.

After you start Sweetroll, open your new website.
It should write the default configuration to `conf/sweetroll.json` in your site directory.
Edit that file, you probably want to change some options.

Create a `templates` directory in your site directory.
You can override the HTML templates you see in this repo's `templates` directory with your own using your `templates` directory.
The templating engine is embedded JavaScript via [lodash](http://lodash.com)'s `_.template`.
You need to put your h-card and rel-me markup into `templates/author.ejs`.

Restart Sweetroll after any changes to the config file or the templates.

Use Micropub clients like [Micropublish](https://micropublish.herokuapp.com) and [Quill](https://quill.p3k.io) to post.

## Libraries I made for this project

- [gitson](https://github.com/myfreeweb/gitson), a git-backed storage engine
- [pcre-heavy](https://github.com/myfreeweb/pcre-heavy), a usable regular expressions library
- [http-link-header](https://github.com/myfreeweb/http-link-header), a parser for the Link header (RFC 5988)
- [microformats2-parser](https://github.com/myfreeweb/microformats2-parser), a Microformats 2 parser
- [indieweb-algorithms](https://github.com/myfreeweb/indieweb-algorithms), a collection of implementations of algorithms like [authorship](http://indiewebcamp.com/authorship) and link discovery
- [hs-duktape](https://github.com/myfreeweb/hs-duktape), Haskell bindings to [duktape](http://duktape.org), a lightweight ECMAScript (JavaScript) engine

## TODO

- [x] receiving webmentions
- [x] [comments-presentation](https://indiewebcamp.com/comments-presentation)
- [x] handling [deleted](https://indiewebcamp.com/deleted#Handling) posts with `410 Gone`
- [ ] Syndicate by Reference: use the `Location` header instead of JSON
- [ ] hashcash in webmentions
- [ ] micropub updating and deleting (implement `FromFormUrlEncoded` for Value, To/FromJSON to support both form-urlencoded and JSON; // respond to ?q=syndicate-to with JSON too)
- [ ] indieweb-algorithms: [mf2-shim](https://github.com/indieweb/php-mf2-shim)
- [ ] templates: more consistency / abstraction with dates and reply buttons, etc.
- [ ] respond to [WebFinger](https://webfinger.net) with a link to the index page & links parsed from the index page by the mf2 parser & custom links from the config
- [ ] posting [photos](https://indiewebcamp.com/photos)
- [ ] proxying reply-context and comments-presentation images (to avoid mixed content and possible tracking) (note: we already depend on `JuicyPixels` through Pandoc)
- [ ] custom non-entry html pages
- [ ] JS hooks for events like posting and webmentions (API: a Sweetroll object which is an EventEmitter and also has config/secrets getters; should be possible to make HTTP requests to e.g. send webmention notifications)
- [ ] archive pages, ie. unpaginated pages
- [ ] tags
- [ ] built-in TLS server, since we depend on `tls` already because of the client
- [ ] Atom feed
- [ ] hs-duktape: add functions for getting ByteStrings (get rid of ByteString → Text → ByteString conversion)

## License

This is free and unencumbered software released into the public domain.  
For more information, please refer to the `UNLICENSE` file or [unlicense.org](http://unlicense.org).
