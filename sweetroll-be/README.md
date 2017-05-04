# Sweetroll Backend

The "backend" is responsible for, roughly speaking, getting data *into* the database:

- processing Micropub requests
  - including fetching referenced posts (`in-reply-to`, `like-of`, etc.)
- processing incoming Webmentions
- authentication (not really "into the database" but it's necessary for Micropub)

## Development

Use [stack] to build.  
Use ghci to run the server while developing (see the `.ghci` file).

The `:serve` command in ghci runs the server in test mode, which means you don't need to authenticate using IndieAuth.

```bash
$ stack build

$ stack ghci --no-load
:serve

$ http -f post localhost:3000/login x=x | sed -Ee 's/.*access_token=([^&]+).*/\1/' > token

$ http -f post localhost:3000/micropub "Authorization: Bearer $(cat token)" h=entry content=HelloWorld
```

(the `http` command in the examples is [HTTPie](https://github.com/jkbrzt/httpie))

### Making the binary smaller

To get a smaller resulting binary, use the `split-objs` GHC option or the newer `split-sections`.
Globally.
Also, use a faster linker, especially with this option.

Here's an example `~/.stack/config.yaml`:

```yaml
ghc-options:
  "*": "-split-sections -optl-fuse-ld=gold"

apply-ghc-options: everything
```

Do something like this, rebuild all your packages/snapshots and enjoy small binaries.

[stack]: https://github.com/commercialhaskell/stack

## General project info

See `../README.md`.
