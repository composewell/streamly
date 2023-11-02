# Build Guide

## Building

### Compiler (GHC) Versions

Do not use ghc 9.0.x and 9.2.1. GHC 9.6.x may have a small memory
leak issue in some concurrent stream cases, see below for more details.
For best performance use `fusion-plugin` (see below) when compiling.

### Memory requirements

Building streamly itself may require upto 4GB memory. Depending on the
size of the application you may require 1-16GB memory to build. For most
applications up to 8GB of memory should be sufficient.

To reduce the memory footprint you may want to break big modules
into smaller ones and reduce unnecessary INLINE pragmas on large
functions. You can also use the `-Rghc-timing` GHC option to report the
memory usage during compilation.

See the "Build times and space considerations" section below for more
details.

### GHC Plugin for Stream Fusion

Streamly usually performs very well without any compiler plugins.
However, we have fixed some deficiencies in GHC's optimizer using a
[compiler plugin](https://github.com/composewell/fusion-plugin).  We
hope to fold these optimizations into GHC in the future; until then we
recommend that you use this plugin for applications that are performance
sensitive.

### Compilation Options

#### Recommended Options

Add `fusion-plugin` to the `build-depends` section of your program in
the cabal file and use the following GHC options:

```
  -O2
  -fdicts-strict
  -fmax-worker-args=16
  -fspec-constr-recursive=16
  -fplugin Fusion.Plugin
```

See [Explanation](#explanation) section for details about these flags.

Important Notes:

1. [fusion-plugin](https://hackage.haskell.org/package/fusion-plugin) can
   improve performance significantly by better stream fusion, in many
   cases. If the performance regresses due to fusion-plugin please open
   an issue.  You may remove the `-fplugin` option for regular builds
   but it is recommended for deployment builds and performance
   benchmarking.
2. In certain cases it is possible that GHC takes too long to compile
   with `-fspec-constr-recursive=16`, if that happens please reduce the
   value or remove that option. This may happen especially when using Parsers.

#### Explanation

* `-fdicts-strict` is needed to avoid [a GHC
issue](https://gitlab.haskell.org/ghc/ghc/issues/17745) leading to
memory leak in some cases. In GHC 9.6.x this issue cannot be avoided
even with `-fdicts-strict` option.

* `-fspec-constr-recursive` is needed for better stream fusion, it
allows the `SpecConstr` optimization to occur in more cases. Large
values used with this flag may lead to huge compilation times and code
bloat, if that happens please avoid it or use a lower value (e.g. 3 or
4).

* `-fmax-worker-args` is needed for better stream fusion, it allows the
`SpecConstr` optimization to occur in some important cases.

* `-fplugin=Fusion.Plugin` enables predictable stream fusion
optimization in certain cases by helping the compiler inline internal
bindings, therefore, enabling case-of-case optimization. In some cases,
it can make a difference of 5-10x better performance.

### Multi-core Parallelism

Concurrency without a threaded runtime may be a bit more efficient. Do not use
threaded runtime unless you really need multi-core parallelism. To get
multi-core parallelism use the following GHC options:

  `-threaded -with-rtsopts "-N"`

## Platform Specific Features

Streamly supports Linux, macOS and Windows operating systems. Some
modules and functionality may depend on specific OS kernel features.
Features/modules may get disabled if the kernel/OS does not support it.

### Linux

File system events notification module is supported only for kernel versions
2.6.36 onwards.

### macOS

File system events notification module supports macOS 10.7+ . You must
have the ``Cocoa`` framework installed which is supplied by the macOS
SDK.  If ``Cocoa`` is not installed, you may see an error like this:

```
error: ld: framework not found Cocoa
```

#### cabal build

Usually, if you have a working GHC you would already have the SDK
installed. See the documentation of `Xcode` or `xcode-select` tool for
more details.

#### Nix build

Please note that `cabal2nix` may not always be able to generate a complete nix
expression on `macOS`. See [this
issue](https://github.com/NixOS/cabal2nix/issues/470).

You may need to add ``nixpkgs.darwin.apple_sdk.frameworks.Cocoa``
to ``librarySystemDepends``. For example, to create a streamly-0.9.0
derivation from Hackage:

```
  streamly =
    nixpkgs.haskell.lib.overrideCabal
        (
          super.callHackageDirect
            { pkg = "streamly";
              ver = "0.9.0";
              sha256 = "sha256-eOxVb8qQjZDo1+S7CStqYSExOg2QHWkMY+zlOYqwZak=";
            } {}
        )
        (old:
          { librarySystemDepends =
              if nixpkgs.lib.strings.hasInfix "darwin" builtins.currentSystem
              then [nixpkgs.darwin.apple_sdk.frameworks.Cocoa]
              else [];
          });
```
