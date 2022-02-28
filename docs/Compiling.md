# Build Guide

## Building

### Compiler (GHC) Versions

GHC 8.6 and above are recommended.  For best performance use GHC 8.8 or
8.10 along with `fusion-plugin` (see below).  Benchmarks show that GHC
8.8 has significantly better performance than GHC 8.6 in many cases.

GHC 9.0 and GHC 9.2 have some performance issues, please see [this
issue](https://github.com/composewell/streamly/issues/1061) for details.
However, upcoming releases may fix some of these issues.

### Distributions

Tested with stackage `lts-18.27` and `nix 21.05`.

### Memory requirements

Building streamly itself may require upto 4GB memory. Depending on the
size of the application you may require 1-16GB memory to build. For most
applications up to 8GB of memory should be sufficient.

To reduce the memory footprint you may want to break big modules into
smaller ones and reduce unnecessary inlining on large functions. You can
also use the `-Rghc-timing` GHC option to report the memory usage during
compilation.

See the "Build times and space considerations" section below for more
details.

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

Important Notes:

1. [fusion-plugin](https://hackage.haskell.org/package/fusion-plugin) can
   improve performance significantly by better stream fusion, many
   cases. If the perform regresses due to fusion-plugin please open
   an issue.  You may remove the `-fplugin` option for regular builds
   but it is recommended for deployment builds and performance
   benchmarking. Note, for GHC 8.4 or lower fusion-plugin cannot be used.
2. In certain cases it is possible that GHC takes too long to compile
   with `-fspec-constr-recursive=16`, if that happens please reduce the
   value or remove that option.
3. At the very least `-O -fdicts-strict` compilation options are
   absolutely required to avoid issues in some cases. For example, the
   program `main = S.drain $ S.concatMap S.fromList $ S.repeat []` may
   hog memory without these options.

See [Explanation](#explanation) for details about these flags.

#### Explanation

* `-fdicts-strict` is needed to avoid [a GHC
issue](https://gitlab.haskell.org/ghc/ghc/issues/17745) leading to
memory leak in some cases.

* `-fspec-constr-recursive` is needed for better stream fusion by enabling
the `SpecConstr` optimization in more cases. Large values used with this flag
may lead to huge compilation times and code bloat, if that happens please avoid
it or use a lower value (e.g. 3 or 4).

* `-fmax-worker-args` is needed for better stream fusion by enabling the
`SpecConstr` optimization in some important cases.

* `-fplugin=Fusion.Plugin` enables predictable stream fusion
optimization in certain cases by helping the compiler inline internal
bindings and therefore enabling case-of-case optimization. In some
cases, especially in some file IO benchmarks, it can make a difference of
5-10x better performance.

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

### Native build

Usually, if you have a working GHC you would already have the SDK
installed. See the documentation of `Xcode` or `xcode-select` tool for
more details.

### Nix build

Please note that cabal2nix may not always be able to generate a complete nix
expression on `macOS`. See [this
issue](https://github.com/NixOS/cabal2nix/issues/470).

You may need to add ``nixpkgs.darwin.apple_sdk.frameworks.Cocoa`` to
your ``buildInputs`` or ``executableFrameworkDepends``. Something like
this:

```
executableFrameworkDepends =
    if builtins.currentSystem == "x86_64-darwin"
    then [nixpkgs.darwin.apple_sdk.frameworks.Cocoa]
    else [];
```
