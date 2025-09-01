<!--
(c) 2022, Composewell Technologies.
SPDX-License-Identifer: BSD-3-Clause
-->

# Installing Haskell

In this tutorial, we assume basic knowledge of Haskell syntax, constructs and
lazy evaluation. The [Haskell wikibook](https://en.wikibooks.org/wiki/Haskell)
may be a good place to start and get familiar with Haskell.

If you wish to follow along and run the examples in this tutorial, you will
need to have Haskell tool chain installed.

You can choose one of the following options.

## Haskell Toolchain

To get started, you will need a fairly recent version (latest three major
versions will work) of the Haskell compiler `ghc` and the build tool
`cabal` installed on your system.  Please see the install instructions
at https://www.haskell.org/downloads/ .

## Development Environment using Nix

If you use the nix package manager, a nix shell for complete Haskell
development environment - including a consistent set of latest streamly
ecosystem packages, hoogle documentation, vim and vscode editors,
Haskell language server (HLS) and other tools - is available at
[streamly-packages](https://github.com/composewell/streamly-packages).
