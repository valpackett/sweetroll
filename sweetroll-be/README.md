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

$ stack ghci
:serve

$ http -f post localhost:3000/login x=x | sed -Ee 's/.*access_token=([^&]+).*/\1/' > token

$ http -f post localhost:3000/micropub "Authorization: Bearer $(cat token)" h=entry content=HelloWorld
```

(the `http` command in the examples is [HTTPie](https://github.com/jkbrzt/httpie))

[stack]: https://github.com/commercialhaskell/stack

## General project info

See `../README.md`.
