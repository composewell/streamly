<!--
(c) 2019, Composewell Technologies.
Portions (c) 2020, Google LLC.
SPDX-License-Identifer: BSD-3-Clause
-->

# Getting started with `Haskell Streamly`

This guide shows you how to use Streamly in `ghci` REPL, a simple
program or a Haskell project.

<!-- TODO: Add instructions for `stack` and `nix`.

If you are using `stack` or `nix` please make sure to add the latest
version from Hackage to your tool configuration.  -->

No prior knowledge of Haskell is needed.  We do however assume that you
are using a command-line shell on a POSIX operating system.  If you are
running Windows&trade; then you may need to run a command-line shell
under `msys` and some of the commands below may also need to be changed
in small ways to make them work.

## Using `streamly` Interactively in REPL

You can try out `streamly` using `GHCi`, the interactive Haskell
read-eval-print-loop (REPL).

Streamly consists of two packages, namely `streamly-core` and
`streamly`. The former package provides basic functionality and the
latter provides higher level functionality. To start up the GHCi REPL
using the latest released `streamly-core` package from Hackage, please
use:

```
$ cabal repl --build-depends streamly-core
... plenty of build messages, the first time around ...
GHCi, version 9.2.2: https://www.haskell.org/ghc/  :? for help
ghci>
```

This command may take a while to build the package first time around,
please be patient.

Once at the `ghci>` prompt, you can import Haskell modules from the
`streamly-core` package and use the available functions in the REPL:

```
ghci> import qualified Streamly.Data.Stream as Stream
ghci> import qualified Streamly.Data.Fold as Fold

ghci> Stream.fold Fold.drain $ Stream.mapM print $ Stream.fromList [1..3]
1
2
3
ghci>
```

For the curious, here is a high level overview of what these lines
do:

1. `import qualified Streamly.Data.Stream as Stream` imports the Streamly
   Stream module into GHCi, and makes it available under the name `Stream`.
2. `[1..3]` generates the Haskell list `[1, 2, 3]`.
3. `Stream.fromList` transforms that list into a stream of integers.
4. `Stream.mapM print` transforms the stream of integers into a stream of
   actions that would print those integers when executed.
5. `Stream.fold Fold.drain` folds that stream using the `drain` fold,
   transforming it into an IO action that evaluates the stream.

### Using a specific version of `streamly` in the REPL

You can also ask `cabal` to use a specific version of `streamly-core` by
adding a version number constraint to the `--build-depends` flag:

```
$ cabal repl --build-depends streamly-core==0.1.0
...
ghci>
```

## Using `streamly` in a Project

Create a project directory for our example project.

```
$ mkdir streamly-project
```

### Add `streamly` as project dependency

Run `cabal init` in the project directory to create an initial set of
project files:

```
$ cd streamly-project
$ cabal init --non-interactive --minimal --dependency base --dependency streamly-core

...
Generating app/Main.hs...
Generating streamly-project.cabal...
...
```

This invocation sets up a Haskell package named `streamly-project`
with two build dependencies, namely `base` (the Haskell standard
library) and `streamly-core`.  You can add additional dependencies
later, by editing the `build-depends` section of the generated
cabal file `streamly-project.cabal`.  Please see the [Cabal User
Guide](https://www.haskell.org/cabal/users-guide/) for more information
on `.cabal` files.

IMPORTANT: Because of a bug in cabal 3.8, which is fixed in cabal 3.10,
this does not generate a dependency on `base`, please edit the generated
cabal file and add `base` in the `build-depends` section:

```
    build-depends: base, streamly-core
```

This invocation also creates a skeletal `app/Main.hs`, therefore, we can
compile and run it right away:

```
$ cabal run
Hello, Haskell!
```

### Import `streamly` modules in the project code

Let us now turn the single-line stanza we used in the REPL into a
standalone program.

Edit `app/Main.hs` to contain the following:

```haskell
module Main where

import qualified Streamly.Data.Stream as Stream
import qualified Streamly.Data.Fold as Fold

main :: IO ()
main = Stream.fold Fold.drain $ Stream.mapM print $ Stream.fromList [1..3]
```

Build and run this program using `cabal run`:

```
$ cabal run
... build messages ...
1
2
3
```

### Running GHCi REPL for the project code

To start up the GHCi REPL for your project, please use:

```
$ cabal repl
...
Ok, one module loaded.
ghci> main
1
2
3
ghci>
```

The repl will load all the dependencies and modules of the project. You can now
run your project code or any other code snippets as you desire.

### (Advanced) Using the development version of `streamly`

To use the development version of `streamly`, we need to configure
`cabal` to fetch it from Github.

Create a `cabal.project` file in the project directory with the
following content:

```
packages: .
-- fetch streamly-core dependency package from github
source-repository-package
  type: git
  location: https://github.com/composewell/streamly
  subdir: core
  tag: master
```

With this file present, `cabal` will fetch and build the current
version of `streamly-core` from Github.  For example:

```
$ cabal repl
Cloning into '/home/harendra/streamly-project/...
...
In order, the following will be built (use -v for more details):
 - streamly-core-0.1.0 (lib:streamly-core) (requires build)
 - streamly-project-0.1.0.0 (exe:streamly-project) (configuration changed)
 ...
ghci>
```

## Which version of `streamly` should you use?

If you are new to Streamly, we recommend using the latest [stable release
of streamly][streamly-hackage] on Hackage.

If you need access to cutting edge features (and do not mind the
occasional breakage), please use the [development version of
streamly][streamly-github] from Github.

## Next Steps

If you got this far successfully, congratulations!  For an overview
of the `streamly` package, please read the [Streamly Quick
Overview](/docs/User/Tutorials/quick-overview.md).

<!-- Markdown Links -->

 [Streamly]: https://streamly.composewell.com/
 [streamly-hackage]: https://hackage.haskell.org/package/streamly
 [streamly-github]: https://github.com/composewell/streamly
 [streamly-packages]: https://github.com/composewell/streamly-packages
