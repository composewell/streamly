# Using `streamly` package

* [Latest stable release](https://hackage.haskell.org/package/streamly) is available on
  Hackage.
* [Latest development version](https://github.com/composewell/streamly) is
  available on github.
* See [recommended compilation options here](docs/Build.md).

If you are using `stack` or `nix` please make sure to add the latest version
from Hackage to your tool configuration.

## Using with `cabal`

Make sure you have `cabal` version 3.0 or later installed and you have `ghc`
available in your PATH. Refresh your package list:

```
$ cabal update
```

### Using hackage version in repl

```
# Run repl with the latest version from Hackage
$ cabal repl --build-depends streamly

# Run repl with a specific version from Hackage
$ cabal repl --build-depends streamly==0.7.1
```

### Using github version in repl

Create a directory for your playground (e.g. `test`) and create a
`cabal.project` file in it as follows:

```
packages: .
source-repository-package
  type: git
  location: https://github.com/composewell/streamly
  tag: master
```

Create a cabal file (e.g. `test.cabal`) as follows

```
cabal-version: 2.4
name: test
version: 0

library
  build-depends: base, streamly
```

Run repl:

```
$ cabal repl
```

### Using with ghc or ghci

Before you can use a package with `ghc` or `ghci` directly you need to install
the package:

```
$ cabal install streamly-0.7.1 --lib
```

```
$ cat hello.hs
import qualified Streamly.Prelude as S

main = S.runStream $ S.fromListM [putStrLn "hello", putStrLn "world"]
```

```
$ ghc hello.hs
```

### Using in a Project

Add `streamly` to the `build-depends` section of your library/executable in
your `<package>.cabal` file. Appropriate version from Hackage will be picked
depending on the version bounds that you specify. See the `github version in
repl` section above for a sample `<package>.cabal` file and optionally use the
`cabal.project` file if you want to use the development version from github in
your project.
