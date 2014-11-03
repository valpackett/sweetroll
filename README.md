# sweetroll [![Hackage](https://img.shields.io/hackage/v/sweetroll.svg?style=flat)](https://hackage.haskell.org/package/sweetroll) [![Build Status](https://img.shields.io/travis/myfreeweb/sweetroll.svg?style=flat)](https://travis-ci.org/myfreeweb/sweetroll) [![Coverage Status](https://img.shields.io/coveralls/myfreeweb/sweetroll.svg?style=flat)](https://coveralls.io/r/myfreeweb/sweetroll) [![Apache License 2.0](https://img.shields.io/badge/license-Apache%202.0-brightgreen.svg?style=flat)](https://www.tldrlegal.com/l/apache2)

A personal website / [#indieweb] engine with curved swords. *Curved! Swords!*

Storage is powered by [gitson], a Git + JSON storage engine, so you can back up your website to GitHub, Bitbucket, etc. See also: [database antipattern].

Markup is powered by [pandoc], so you can use different markup languages for your posts.

[#indieweb]: http://indiewebcamp.com/
[database antipattern]: http://indiewebcamp.com/database-antipattern
[gitson]: https://github.com/myfreeweb/gitson
[pandoc]: http://johnmacfarlane.net/pandoc/

## Development

```bash
# Update to latest version of Cabal.
cabal update
cabal install cabal-install

# Initialize a sandbox and install the package's dependencies.
make install

# Configure & build the package.
make configure build

# Test package.
make test

# Start a REPL.
make repl

# Generate documentation.
make haddock
```

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
