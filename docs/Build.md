# GHC Haskell
## Compiler Versions

GHC versions 8.0 onwards are supported by streamly per se. However, note
that some packages we depend on (e.g. `network`) support only last three
versions of GHC.

Use GHC 8.8 for best performance.  Benchmarks show that GHC 8.8 has
significantly better performance than GHC 8.6 in many cases.

GHC 8.2.2 may hog memory and hang when building certain applications using
streamly (particularly the benchmark programs in the streamly package).
Therefore, we recommend avoiding using the GHC version 8.2.x.

## Compilation Options

### Absolute Minimum

At the very least `-O -fdicts-strict` compilation options are
required. If these options are not used, the program may exhibit memory
hog.  For example, the following program, if compiled without an
optimization option, is known to hog memory:

```
main = S.drain $ S.concatMap S.fromList $ S.repeat []
```

### Recommended Options

Use the following GHC options:

```
  -O2
  -fdicts-strict
  -fmax-worker-args=16
  -fspec-constr-recursive=16
```

Important Note: In certain cases it is possible that GHC takes too long to
compile with `-fspec-constr-recursive=16`, if that happens please reduce the
value or remove that option.

See [Explanation](#explanation) for details about these flags.

### Using Fusion Plugin

In many cases
[fusion-plugin](https://hackage.haskell.org/package/fusion-plugin) can
improve performance by better stream fusion. However, in some cases
performance may also regress. Please note that the `fusion-plugin`
package works only for GHC >= 8.6.

* Add `fusion-plugin` to the `build-depends` section of your program in
  the cabal file.
* Add the following to GHC options:

```
  -fplugin=Fusion.Plugin 
```

### Explanation

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

## Multi-core Parallelism

Concurrency without a threaded runtime may be a bit more efficient. Do not use
threaded runtime unless you really need multi-core parallelism. To get
multi-core parallelism use the following GHC options:

  `-threaded -with-rtsopts "-N"`

# Platform Specific

Streamly supports Linux, macOS and Windows operating systems. Some
modules and functionality may depend on specific OS kernel features.
Features/modules may get disabled if the kernel/OS does not support it.

## Linux

* File system events notification module is supported only for kernel versions
  2.6.36 onwards.

## Mac OSX

* File system events notification module requires macOS 10.7+ with
  Xcode/macOS SDK installed (depends on `Cocoa` framework). However, we only
  test on latest three versions of the OS.

## Windows

* File system event notification module is not yet available for Windows.

# Performance Optimizations

A "closed loop" is any streamly code that generates a stream using
unfold (or conceptually any stream generation combinator) and ends
up eliminating it with a fold (conceptually any stream elimination
combinator). It is essentially a loop processing multiple elements in
a stream sequence, just like a `for` or `while` loop in imperative
programming.

Closed loops are generated in a modular fashion by stream generation,
transformation and elimination combinators in streamly. Combinators
transfer data to the next stage or combinator using data constructors.
These data constructors are eliminated by the compiler using `stream
fusion` optimizations, generating a very efficient loop.

However, stream fusion optimization depends on proper inlining of the
combinators involved. The fusion-plugin package mentioned earlier
fills gaps for several optimizations that GHC does not perform
automatically. It automatically inlines the internal definitions
that involve the constructors we want to eliminate. In some cases
fusion-plugin may not help and programmer may have to annotate the code
manually for complete fusion. In this section we mention some of the
cases where programmer annotation may help in stream fusion.

Remember, you need to worry about performance only where it matters, try
to optimize the fast path and not everything blindly.

## INLINE annotations

It may help to add INLINE annotations on any intermediate functions
involved in a closed loop. In some cases you may have to add an inline
phase as well as described below.

Usually GHC has three inline phases - the first phase is pahse-2, the
second phase is phase-1 and the last one is phase-0.

### Early INLINE

Generally, you only have to inline the combinators or functions
participating in a loop and not the whole loop itself.  But sometimes
you may want to inline the whole loop itself inside a larger
function. In most cases you can just add an INLINE pragma on
the function containing the loop. But you may need some special
considerations in some (not common) cases.

In some cases you may have to use INLINE[2] instead of INLINE which
means inline the function early in phase-2.  This may sometimes be
needed on the because the performance of several combinators in streamly
depends on getting inlined in phase-2 and if you use a plain `INLINE`
annotation GHC may decide to delay the inlining in some cases. This is
not very common but may be needed sometimes. Perhaps GHC can be fixed or
we can resolve this using fusion-plugin in future.

### Delayed INLINE

When a function is passed to a higher order function e.g. a function
passed to `concatMap` or `concatUnfold` then we want the function to be
inlined after the higher order is inlined so that proper fusion of the
higher order function can occur. For such cases we usually add INLINE[1]
on the function being passed to instruct GHC not to inline it too early.

## Strictness annotations

* Strictness annotations on data, specially the data used as accumulator in
  folds and scans, can help in improving performance.
* Strictness annotations on function arguments can help the compiler unbox
  constructors in certain cases, improving performance.
* Sometimes using `-XStrict` extension can help improve performance, if so you
  may be missing some strictness annotations. `-XStrict` can be used as an aid
  to detect missing annotations, using it blindly may regress performance.

## Use tail recursion

Do not use a strict `foldr` or lazy `foldl` unless you know what you are
doing.  Use lazy `foldr` for lazily transforming the stream and strict
`foldl` for reducing the stream.  If you are manually writing recursive
code, try to use tail recursion where possible.

# Build times and space considerations

Haskell, being a pure functional language, confers special powers on
GHC. It allows GHC to do whole program optimization. In a closed loop
all the components of the loop are inlined and GHC fuses them together,
performs many optimizing transformations and churns out an optimized
fused loop code. Let's call it whole-loop-optimization.

To be able to fuse the loop by whole-loop-optimization all the parts of the
loop must be operated on by GHC at the same time to fuse them together. The
amount of time and memory required to do so depends on the size of the loop.
Huge loops can take a lot of time and memory. We have seen GHC take 4-5 GB of
memory when a lot of combinators are used in a single module.

If a module takes too much time and space we can break it into multiple
modules moving some non-inlined parts in another module. There is
another advantage of breaking large modules, it can take advantage of
parallel compilation if they do not depend on each other.
