# Using `streamly` package

If you are new to Haskell please take a look at [Haskell Getting Started
Guide](https://github.com/composewell/haskell-dev/blob/master/getting-started.rst).

The following instructions assume that you are using a shell on a POSIX
system. On Windows you need a shell under `msys`, some of the commands
may have slight differences.

## Which version to use?

* [Latest stable release](https://hackage.haskell.org/package/streamly) is available on
  Hackage.
* [Latest development version](https://github.com/composewell/streamly) is
  available on github.

If you are using `stack` or `nix` please make sure to add the latest version
from Hackage to your tool configuration.

## Recommended build options

See [recommended compilation options here](docs/Build.md).

## Using with `cabal`

Make sure you have `cabal` version 3.0 or later installed and you have `ghc`
available in your PATH. Refresh your package list:

```
$ cabal update
```

Create a directory for isolated builds:

```
$ mkdir streamly-play
```

Create a `.cabal` file:

```
$ cd streamly-play
$ cabal init --minimal --dependency base --dependency streamly
```

You can add any other dependencies as well later on by editing the
`build-depends` section of the cabal file.

### Using hackage version in repl

Run repl with the latest version from Hackage:

```
$ cabal repl --build-depends streamly
GHCi, version 8.8.3: https://www.haskell.org/ghc/  :? for help
[1 of 1] Compiling Main             ( Main.hs, interpreted )
Ok, one module loaded.
*Main> S.drain $ S.mapM print $ S.fromList [1..3]
1
2
3
*Main>
```

Run repl with a specific version from Hackage:

```
$ cabal repl --build-depends streamly==0.7.2
```

### Using github version in repl

Create a `cabal.project` file with the following contents:

```
packages: .
source-repository-package
  type: git
  location: https://github.com/composewell/streamly
  tag: master
```

Run repl:

```
$ cabal repl
```

### Hello World!

Edit `Main.hs` as follows:

```
module Main where

import qualified Streamly.Prelude as S

main :: IO ()
main = S.drain $ S.mapM print $ S.fromList [1..3]
```

Build and run:

```
$ cabal run
```
