# Using `streamly` package

The latest stable release of the package is available 
[on Hackage](https://hackage.haskell.org/package/streamly). You can also try
the latest development version 
[from github](https://github.com/composewell/streamly).

The default version available to you and how to pick a specific version may
depend on the build tool you are using. We have provided instructions to use a
specific version for some tools below.

## cabal

Make sure you have `cabal` version 2.4 or later installed and you have `ghc`
available in your PATH. Refresh your package list:

```
$ cabal v2-update
```

### hackage version in repl

```
# Pick the latest version on Hackage
$ cabal v2-repl --build-depends streamly

# Pick a specific version on Hackage
$ cabal v2-repl --build-depends streamly==0.6.1
```

### github version in repl

Create a `test` directory and create a `cabal.project` file in it as follows:

```
packages: .
source-repository-package
  type: git
  location: https://github.com/composewell/streamly
  tag: master
```

Create a `test.cabal` file as follows

```
cabal-version: 2.4
name: test
version: 0

library
  build-depends: base, streamly
```

Run repl:

```
$ cabal v2-repl
```

### ghc

```
$ cat hello.hs
import qualified Streamly.Prelude as S

main = S.runStream $ S.fromListM [putStrLn "hello", putStrLn "world"]
```

```
$ cabal v2-install streamly-0.6.1
$ ghc -package streamly-0.6.1 hello.hs
```

### Project

Add `streamly` to the `build-depends` section of your library/executable in
your `<package>.cabal` file. Appropriate version from Hackage will be picked
depending on the version bounds that you specify. See the `github version in
repl` section above for a sample `<package>.cabal` file and optionally the
`cabal.project` file if you want to use the development version from github in
your project.
