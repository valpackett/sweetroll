# sweetroll [![unlicense](https://img.shields.io/badge/un-license-green.svg?style=flat)](http://unlicense.org)

**Big changes in progress!**

A website engine for [the indie web] with curved swords. *Curved! Swords!*

- uses PostgreSQL with the [mf2sql] schema for storage
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
- `stack build`

When it's done, it says where it put the binary (something like `.stack-work/install/your-platform/some/versions/.../bin`).

#### Making the binary smaller

To get a smaller resulting binary, use the `split-objs` GHC option or the newer `split-sections`. Globally.
And use a faster linker.

Here's an example `~/.stack/config.yaml`:

```yaml
ghc-options:
  "*": "-split-sections -optl-fuse-ld=gold"

apply-ghc-options: everything
```

Do something like this, rebuild all your packages/snapshots and enjoy small binaries.

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


## TODO

- micropub
	- [ ] do something about category decisions
	- [ ] JWT scopes authorization
	- [ ] draft flag → draft tag and private acl
	- [ ] media endpoint app with upload to openstack/s3, thumbnails, exif metadata extraction
- webmention
  - [ ] fix all the things
  - [ ] reverify/refetch to update user profiles and stuff
  - [ ] moderation tools
    - [ ] different modes in config: allow all (except blocked), allow known good domains (e.g. domains replied to), premoderate all, turn off webmention
    - [ ] [blocking](https://indieweb.org/block) domains
      - [ ] sharing block lists
  - [ ] deduplicate syndicated replies
	- [ ] vouch
	- [ ] private webmention
  - [ ] queue up mention processing requests in the database instead of sync processing (worth it?)

## License

This is free and unencumbered software released into the public domain.  
For more information, please refer to the `UNLICENSE` file or [unlicense.org](http://unlicense.org).

[mf2sql]: https://github.com/myfreeweb/mf2sql
[stack]: https://github.com/commercialhaskell/stack
