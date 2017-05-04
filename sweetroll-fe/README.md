# Sweetroll Frontend

The "frontend" is responsible for, roughly speaking, getting data *out* of the database:

- rendering the websites
- hosting the live update feeds (Server-Sent Events)
- pushing updates to a WebSub hub
- sending Webmentions for updates

## Development

You need a recent version (at least 7.6.0) of [Node.js].

Use [yarn] to build.

```bash
$ yarn

$ yarn deps

$ yarn devserve
```

By default, the database URI is assumed to be `postgres://localhost/sweetroll`, change the `DATABASE_URI` environment variable if that's not the case.

[Node.js]: https://nodejs.org/en/
[yarn]: https://yarnpkg.com/en/

## General project info

See `../README.md`.
