<!--
Portions (c) 2020, Google LLC.
SPDX-License-Identifer: BSD-3-Clause
-->

# Getting started with the `streamly` package

This guide will show you how to write a simple program that uses
[Streamly][], using the [`cabal`](https://www.haskell.org/cabal/) build
manager for Haskell.

<!-- TODO: Add instructions for `stack` and `nix`.

If you are using `stack` or `nix` please make sure to add the latest
version from Hackage to your tool configuration.  -->

No prior knowledge of either Haskell or of `cabal` is needed.  We do
however assume that you are using a command-line shell on a POSIX
operating system.  If you are running Windows&trade; then you may need
to run a command-line shell under `msys` and some of the commands
below may also need to be changed in small ways to make them work.

**Note**: If you are new to Haskell you may find the [Haskell Getting
Started Guide][haskell-getting-started] to be useful.

If you know your way around Haskell, and have an up to date toolchain
already installed, then you can jump straight to the section titled
"[_Prepare Your Build Directory_](#prepare-your-build-directory)".

## Getting started with `streamly` using `cabal`

To get started, you will need a recent `cabal` binary to be installed
on your system.

### Installing `cabal`

A pre-packaged `cabal` binary may already be available for your
operating system.

For example, on Debian GNU/Linux, you could install `cabal` using::

```
$ sudo apt install cabal-install
```

### Installing `ghc`

You would also need to install the Glasgow Haskell Compiler `ghc`.  In
most operating systems, using the system's package manager to install
`cabal` would also bring in a version of `ghc` as a dependency.

Please verify that `ghc` is present:

```
$ ghc --version
The Glorious Glasgow Haskell Compilation System, version 8.0.2
```

If `ghc` isn't present, please install it using your operating
system's package manager. For example, on Debian GNU/Linux, use:

```
$ sudo apt install ghc
```

### Update `cabal` if necessary

Many operating systems ship an old version of `cabal`; in order to use
`streamly` you should use `cabal` version 3.0 (or later).

You can check the installed version of `cabal` by using its
`--version` flag:

```
$ which cabal
/usr/bin/cabal
$ /usr/bin/cabal --version
cabal-install version 1.24.0.2
compiled using version 1.24.2.0 of the Cabal library
```

In this example, the installed `cabal` binary is too old for our
needs. It would need to be upgraded.

You can ask `cabal` to upgrade itself (please see
<https://www.haskell.org/cabal/>):

```
$ cabal install Cabal cabal-install
Resolving dependencies...
Downloading base16-bytestring-0.1.1.7...
... lots of build output ...
Building cabal-install-3.0.0.0...
Installed cabal-install-3.0.0.0
```

`cabal` will usually place its compiled binaries inside the directory
`$HOME/.cabal/bin`.

You can verify the version of your newly installed binary by using the
`--version` flag again:

```
$ $HOME/.cabal/bin/cabal --version
cabal-install version 3.0.0.0
compiled using version 3.0.2.0 of the Cabal library
```

If you have upgraded `cabal`, do be sure to specify `$HOME/.cabal/bin`
early in your `PATH` shell variable, so that the new binary gets used
instead of the older version on your system.

```
$ PATH=$HOME/.cabal/bin:$PATH
```

Once you have `cabal` version 3.0 (or later) successfully installed,
please refresh its package list by using `cabal update`.

```
$ cabal update
Downloading the latest package list from hackage.haskell.org
```

### Prepare Your Build Directory

Next, create a build directory, so that you can use `cabal`'s isolated
builds feature.

```
$ mkdir streamly-play
$ cd streamly-play
```

Run `cabal init` to create an initial set of project files:

```
$ cabal init --minimal --dependency base --dependency streamly

Generating LICENSE...
... other messages ...
Generating Main.hs...
Generating streamly-play.cabal...

Warning: no synopsis given. You should edit the .cabal file and add one.
You may want to edit the .cabal file and add a Description field.
```

This invocation will set up a build with two build dependencies,
namely `base` and `streamly`.  You can add additional dependencies
later, by editing the `build-depends` section of the generated cabal
file `streamly-play.cabal`.  Please see the [Cabal User
Guide](https://www.haskell.org/cabal/users-guide/) for more
information on how to configure `cabal`.

This invocation also creates a skeletal `Main.hs` program which we
will use later.

### Try `streamly` in the GHCi REPL

You can now try out `streamly` using the `GHCi` interpreter's
read-eval-print-loop (REPL) facility.

To start up the GHCi REPL using the released `streamly` version on
Hackage, please use:

```
$ cabal repl --build-depends streamly
... plenty of build messaages, the first time around ...
GHCi, version 8.8.3: https://www.haskell.org/ghc/  :? for help
[1 of 1] Compiling Main             ( Main.hs, interpreted )
Ok, one module loaded.
*Main>
```

Once at the `*Main>` prompt, you can import `streamly` and use it
directly:

```
*Main> import qualified Streamly.Prelude as Stream

*Main Stream> Stream.drain $ Stream.mapM print $ Stream.fromList [1..3]
1
2
3
*Main Stream>
```

For the curious, here is a high level overview of what these lines
do:

1. `import qualified Streamly.Prelude as Stream` imports the Streamly
   prelude into GHCi, and makes it available as module `Stream`.
2. `[1..3]` generates the Haskell list `[1, 2, 3]`.
3. `Stream.fromList` transforms that list into a stream of integers.
4. `Stream.mapM print` transforms the stream of integers into a stream of
   actions that would print those integers when executed.
5. `Stream.drain` transforms that stream of actions into an IO action that
   `main` or GHCi's REPL can execute.

#### Using a specific version of `streamly` in the REPL

You can also ask `cabal` to use a specific version of `streamly` by
adding a version number constraint to the `--build-depends` flag:

```
$ cabal repl --build-depends streamly==0.8.2
[1 of 1] Compiling Main             ( Main.hs, interpreted )
Ok, modules loaded: Main.
*Main>
```

#### (Advanced) Using the development version of `streamly` in the REPL

To use the development version of `streamly`, we need to configure
`cabal` to fetch it from Github.

Create a `cabal.project` file in the current directory with the
following content:

```
packages: .
source-repository-package
  type: git
  location: https://github.com/composewell/streamly
  tag: master
```

With this file present, `cabal` will fetch and build the current
version of `streamly` from Github every time it is run.  For example:

```
$ cabal repl
... fetches the 'master' branch of streamly from Github ...
... build messages ...
[1 of 1] Compiling Main     v        ( Main.hs, interpreted )
Ok, modules loaded: Main.
*Main>
```

### Using `streamly` in a standalone program

Let us now turn the single-line stanza above into a standalone program
that uses `streamly`.

Edit `Main.hs` to contain the following:

```haskell
module Main where

import qualified Streamly.Prelude as Stream

main :: IO ()
main = Stream.drain $ Stream.mapM print $ Stream.fromList [1..3]
```

Build and run this program using `cabal run`:

```
$ cabal run
... build messages ...
1
2
3
```

## Which version of `streamly` should you use?

If you are new to Streamly, we recommend using latest [stable release
of streamly][streamly-hackage] on Hackage.

If you need access to cutting edge features (and do not mind the
occasional breakage), please use the [development version of
streamly][streamly-github] on Github.


## Next Steps

If you got this far successfully, congratulations!

* For an overview of the `streamly` package, please read the
  [Streamly Quick Overview](/docs/Introduction.md).
* Please browse the code examples in the
  [`Streamly examples repository`][streamly-examples].
* If you would like to start on a real world project, please take a
  look at the [Streamly Build Guide](/docs/Compiling.md).
* For comprehensive documentation please visit the
  [Streamly Homepage][Streamly].

<!-- Markdown Links -->

 [Streamly]: https://streamly.composewell.com/
 [streamly-hackage]: https://hackage.haskell.org/package/streamly
 [streamly-github]: https://github.com/composewell/streamly
 [streamly-examples]: https://github.com/composewell/streamly-examples
 [haskell-getting-started]: https://github.com/composewell/haskell-dev/blob/master/getting-started.rst
