# Haskell Streamly Optimization Guide

See also the streamly developer's optimization guide.

## When to Optimize?

Focus on performance only where it truly matters. Avoid optimizing
everything blindly—concentrate on the *fast path* or the parts of the
code that run most frequently. Optimizing rarely executed code offers
little benefit and can introduce unnecessary complexity. Prioritize the
hot spots where performance gains will have the greatest impact.

It’s important to be deliberate and informed about what to
optimize. Many developers fall into the trap of over-optimizing
unimportant code, while overlooking bottlenecks that actually impact
performance. Strive to profile first, understand the real costs, and
then optimize where it makes a measurable difference.

## Loop Optimizations: Stream Fusion

A "closed loop" is any streamly code that generates a stream using
unfold (or conceptually any stream generation combinator) and ends
up eliminating it with a fold (conceptually any stream elimination
combinator). It is essentially a loop processing multiple elements in
a stream sequence, just like a `for` or `while` loop in imperative
programming.

Closed loops are synthesized in a modular fashion by stream generation,
transformation and elimination combinators in streamly. Combinators
transfer data to the next stream pipeline stage using data constructors.
These data constructors are eliminated by the compiler using `stream
fusion` optimizations, generating a very efficient loop. Streamly is designed
to utilize the stream fusion opitmization to the fullest such that the loops
are highly optimized providing similar performance as hand written C.

### Fusion Plugin

Stream fusion optimization depends on proper inlining of the combinators
involved. To take full advantage of stream fusion in streamly you should
compile your code with the `fusion-plugin` enabled. The fusion-plugin
package fills the gaps for several optimizations that GHC does not
perform automatically. It automatically inlines the internal definitions
that involve the constructors we want to eliminate. In some cases
fusion-plugin may not help and programmer may have to annotate the code
manually for complete fusion.

In the following sections we mention some of the cases where programmer
annotation may help in stream fusion.

### INLINE Annotations

It can be helpful to add `INLINE` annotations to intermediate functions
involved in a closed loop. In some cases, you may also need to specify
an `INLINE` phase, as discussed in the following sections.

As a rule of thumb, any function that takes `Stream`, `Fold`, or
`Unfold` types as arguments — or returns these types — should be
inlined.

GHC typically uses three inline phases:

- **Phase 2** (the earliest),
- **Phase 1**, and
- **Phase 0** (the latest).

Inlining can be controlled with phase-specific annotations like:
```haskell
{-# INLINE [1] functionName #-}
```
to guide when inlining occurs during compilation.

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
passed to `concatMap` or `unfoldMany` then we want the function to be
inlined after the higher order is inlined so that proper fusion of the
higher order function can occur. For such cases we usually add INLINE[1]
on the function being passed to instruct GHC not to inline it too early.

## Chunked Processing (Inner and Outer Loops)

Processing data in larger chunks can significantly reduce loop overhead
by minimizing the number of iterations. Chunked processing enables the
inner loop to run more efficiently through stream fusion—reducing
heap allocations, improving cache locality, minimizing branching, and
enhancing opportunities for vectorization.

In libraries like *streamly*, this can be achieved by streaming arrays
and applying a fused operation to each array—either using built-in
combinators or writing custom logic per chunk. Whether the **outer
loop** is fused or not is usually less important, so you can choose
either the fused `Stream` type or the more flexible CPS-based `StreamK`
type.

However, for the **inner loop**, fusion is more critical, especially
when it runs frequently. To achieve this, you should prefer using
`Unfold` or fused `Stream` operations for chunk processing. Likewise, on
the consumer side, it's recommended to use the fused `Fold` or `Parser`
types to maintain performance.

This chunking strategy is especially effective in performance-critical
applications, such as numerical computing or high-throughput data
processing.

## When Fusion Matters

In stream processing, **fusion is most important in inner loops**—the
parts of your program that run most frequently. Outer loops, which are
executed less often, can afford to use non-fused combinators without
significant performance penalties.

Some operations in Streamly explicitly break fusion. These are clearly
annotated in the documentation. It's best to place such fusion-breaking
operations in the outer loop to keep the fast path—the inner
loop—fully fused and efficient.

That said, breaking fusion doesn't always have a noticeable performance
impact. The cost is primarily proportional to the number of loop
iterations. For example:

- A stream of 1,000 items, each 1KB in size, will likely show negligible
impact from a fusion break.
- A stream of 1 million items, each just 1 byte, will suffer
significantly—even though the total data volume is the same—because
the loop runs far more times.

This overhead arises from copying intermediate structures when fusion is
broken. The more iterations, the greater the cumulative cost.

In imperative programming, it's easier to see this: the loop body
is explicit, and the number of iterations is obvious. In stream
abstractions, however, the loop is hidden, making it harder to notice
when fusion matters.

Think of a fused stream as a **superhighway** for data processing. A
fusion break is like a **toll booth** or **junction** where each item
pays a small tax. Whether that toll matters depends on how much load you are
carrying per trip.

To give you a rough idea: let's assume the overhead of breaking fusion is
around **30–50 nanoseconds per item**. If you're processing something
like a database row and the processing takes a few microseconds per
item, this overhead is negligible—less than 0.05% of the total
cost. But if you're doing lightweight processing—say, ~100ns per
item—the toll becomes significant.

In general:  **Optimize the core processing first. Once that’s
efficient, look at fusion.**

## Breaking fusion: Monad binds

If fusion matters it is always good to have function having a type `...
-> Stream m a` instead of `... -> m (Stream m a)`. The first one is a
stream that can fuse with other streams, whereas the second is a monadic
action that returns a stream, the monadic action needs to be flattened
e.g. using `concatEffect` to generate a stream. Also, note that since
fusion breaks anyway inlining a function returning a `m (Stream m a)`
may not matter as much from fusion perspective.

The simple rule is that you do not break a stream pipeline to perform
an action and return the stream from the monad. If you do that then you
are deliberately breaking a fused pipeline. You need to compose all the
stream functions without cutting the pipeline by sandwiching a monadic
effect. Stream has an underlying monad and you should use available
stream combinators to perform any action in that monad and not outside
of the stream.

## Breaking Fusion: Monad Binds

When fusion performance matters, it's generally better to use functions 
of type `... -> Stream m a` rather than `... -> m (Stream m a)`.

The former creates a stream directly, allowing it to fuse seamlessly
with other stream operations. The latter wraps the stream in a monadic
action, requiring an additional step—such as `concatEffect`—to
flatten it into a stream, which breaks fusion.

Guiding Principle:
Avoid interrupting a fused stream pipeline with monadic effects that
return new streams. Doing so effectively breaks the pipeline, making it
harder for the compiler to optimize and fuse subsequent operations.

Instead, try to express all effects within the stream itself. Streamly
provides combinators that work inside the stream’s underlying monad,
allowing you to stay within the fused pipeline. Use those to perform
effects without stepping outside the streaming abstraction.

## `concatMap` vs `unfoldEach`

As mentioned in the previous section, `concatEffect` belongs to the
class of `concatMap`-style operations. These operations do **not**
fuse, because they generate streams *dynamically* at runtime. Since the
structure of the resulting stream isn't statically known, the compiler
cannot apply fusion optimizations effectively.

To achieve full fusion in nested streams, use the `unfoldEach` family
of operations instead. These operate with statically known structure,
allowing the compiler to completely fuse the inner and outer loops. This
results in highly optimized code with minimal allocation and maximum
performance.

## Breaking Fusion: Stream Conversion

The `Stream` type in Streamly is fully fused—operations on it can
be composed without allocating intermediate structures. However,
when converting a `Stream` to or from other stream types—such as
the CPS-based `StreamK`, or third-party libraries like `streaming`,
`conduit`, or `pipes`—fusion is broken.

Such conversions involve exiting the fused pipeline and constructing
a new stream representation. This results in allocation of new data
constructors and loss of compiler optimizations that rely on the fusible
stream structure.

While interoperation with other libraries is sometimes necessary,
it's important to be aware that converting stream types introduces
overhead. To preserve fusion and maximize performance, stay within the
fused `Stream` type whenever possible.

## Strictness annotations

* Strictness annotations on data, specially the data used as accumulator in
  folds and scans, can help in improving performance.
* Strictness annotations on function arguments can help the compiler unbox
  constructors in certain cases, improving performance.
* Sometimes using `-XStrict` extension can help improve performance, if so you
  may be missing some strictness annotations. `-XStrict` can be used as an aid
  to detect missing annotations, but using it blindly may regress performance.

## Use tail recursion

If you are manually writing recursive code, try to use tail recursion
where possible. When using recursion via fold operations, do not use a
strict `foldr` or a lazy `foldl` unless you know what you are doing.
Always use lazy `foldr` for lazily transforming the stream and strict
`foldl` for reducing the stream. In streamly, this comes naturally as
the stream operations are lazy foldr like and the fold operations via
the Fold module are strict foldl like.

## Static Argument Transformation

Static Argument Transformation (SAT) is a valuable optimization for
reducing unnecessary overhead in recursive loops. It becomes especially
impactful when other inefficiencies have already been addressed.

GHC includes support for SAT via the `-fstatic-argument-transformation`
flag, which is enabled with the `-O2` optimization level. However, it
may not always trigger automatically or optimally in all cases.

For this reason, it's recommended that programmers understand and apply
this transformation manually when appropriate. Fortunately, it's simple
to do by habit, and in practice, we've observed substantial performance
improvements from applying it by hand in certain critical code paths.

See the `loopDir` function in [this example](https://github.com/composewell/streamly-examples/blob/ebf9470ce2e96f9882653bb1e0ae1b1a239487e3/examples/BasicDirStream.hsc)
for a case where static argument transformation was used to acheive
performance equivalent to C.

<!--
It will be nice if compiler can automatically detect and point out such
opportunities instead of doing it, the programmer can do it selectively.
-->

## Using `SpecConstr` in Recursive Loops

Consider leveraging the `SpecConstr` optimization when writing recursive
loops manually, this happens automatically when recursion is implemented
using combinators from streamly e.g. using streams or folds. By passing the
`SPEC` argument, you can guide GHC to specialize the loop based on the
constructor of the argument being threaded through the recursion.

This can lead to significant performance improvements in certain cases,
especially when the recursive argument has a known or predictable
constructor.

Understanding when and why this helps requires some familiarity with how
`SpecConstr` works internally. If you understand the mechanics of this
optimization, you'll be better equipped to spot opportunities where it
can make a real difference.

## Reducing Duplicate Branches

Duplicating branches in conditional expressions is a common pattern
in Haskell. This often occurs because an `if` expression in Haskell
**requires both** a `then` and an `else` branch.

In contrast, languages like C allow a series of single-branch `if`
statements, followed by a common operation. In Haskell, programmers
often end up duplicating that common operation in each `else` branch.

### Better Approaches

Instead of duplicating code:

- **Use `when`** from `Control.Monad` for single-branch conditions where
there's no `else`. This avoids unnecessary duplication.
- **Extract common code** into a `let` binding. This ensures the code is
only written once and reused across branches.

### Example

Instead of:

```haskell
if condition1
  then doSomething1
  else doCommonThing

if condition2
  then doSomething2
  else doCommonThing
```

Refactor to:

```haskell
    when condition1 doSomething1
    when condition2 doSomething2
    doCommonThing
```

Or, if multiple branches share a computation:

```haskell
    let common = doCommonThing
    in if condition
         then doSomething >> common
         else doSomethingElse >> common
```

Using shared definitions not only improves readability and
maintainability but also reduces code bloat and allows the compiler to
optimize common expressions more effectively.

## Delay the Specialization

Suppose we want to choose between two functions, `f` and `g`, based on
a flag. One option is to make this decision early and pass the chosen
(inner) function to other parts of the code (outer). Another option is
to pass the flag itself and defer the decision until inside the outer
function that needs it.

In many cases, **delaying the decision**—i.e., passing the flag—is
preferable. This makes it easier for the compiler to optimize, since it
doesn't need to inline the outer function in order to inline the inner
one. By postponing specialization, the code stays more flexible and
often performs better, especially in larger or more abstract codebases.

## Reduce Allocations

One useful way to gauge whether an optimization is effective is by
measuring memory allocations. Most optimizations work by reducing the
number of allocations, though some CPU-focused optimizations may not.

Fewer allocations result in lower CPU utilization and reduced GC
pressure. In general, when writing code, keep in mind how it will be
translated by the compiler, and aim to be frugal with allocations.

<!--
Our GHC perf patch can help in precisely pin-pointing the places where
more allocations are happening.

TODO: static analyzer for allocations. GHC support to statically calculate the
allocations in different parts of the code.

AI like optimizations. Try different ways of optimizing the code and choose the
one with least allocations. Do or don't do a particular optimization based on
the context, e.g. exitify in some cases helps but in other cases it makes
things worse. We can make it learn the cases using training.
-->

## Mutable Memory in Local Loops

Using mutable memory in local loops can improve performance
significantly due to better cache utilization and reduced allocations
and garbage collection overhead. Instead of repeatedly allocating and
deallocating memory, we can reuse the same memory buffer.

For example, consider a print buffer that's copied during the print
operation. Rather than allocating and freeing the buffer on each
iteration, we can use a mutable buffer that persists across loop
iterations. This avoids unnecessary heap traffic.

Haskell currently lacks ergonomic abstractions for mutable memory, which
is a gap that should be addressed. However, we can still use low-level
tools like `allocaBytes` to allocate pinned mutable memory within a
loop.

In C, local memory is typically allocated on the stack, which is
automatically and efficiently deallocated as the function returns. In
contrast, Haskell makes minimal use of the stack—most allocations
occur on the heap. While heap allocation in Haskell can be as cheap
as in C, deallocation involves garbage collection, which introduces
overhead. To mitigate this, we can simulate stack-like behavior using
reusable mutable buffers, improving performance in performance-critical
code.

Interestingly, in multithreaded programs, Haskell may have an advantage:
each C thread requires a dedicated stack (with potential memory limits),
whereas Haskell uses the heap for all memory needs, avoiding stack-size
constraints. This makes memory use in Haskell more flexible and often
more efficient in highly concurrent applications.

See the `loopDir0` function in [this example](https://github.com/composewell/streamly-examples/blob/ebf9470ce2e96f9882653bb1e0ae1b1a239487e3/examples/BasicDirStream.hsc)
for a case where a mutable `buffer` is passed to the function which is
reused in each iteration of the loop to avoid unnecessary allocation
work to achieve performance equivalent to C.

## `unsafePerformIO`

When using `unsafePerformIO`, you should almost always add the
`NOINLINE` pragma. Without it, the compiler may inline the expression,
potentially duplicating side effects or violating assumptions about
evaluation order—leading to unsafe or unpredictable behavior. It can
also result in reduced performance.

```haskell
    {-# NOINLINE myGlobal #-}
    myGlobal :: IORef Int
    myGlobal = unsafePerformIO (newIORef 0)
```

Using `NOINLINE` ensures that the `IO` action is performed only once,
helping preserve referential transparency as much as possible when
working with `unsafePerformIO`.

### File IO

Avoid using the standard file read/write functions from the `base`
library, as they are not optimized for performance. Instead, prefer
`streamly`’s file IO operations, which are significantly more
efficient—especially for high-throughput or streaming applications.

Alternatively, if you’re not using `streamly`, consider the lazy
`ByteString` or `Text`-based file IO functions from the `bytestring`
and `text` libraries. These provide better performance than the default
`String`-based APIs in `base`.

## Accumulating Folds vs Streaming

Consider the following fold function a Streamly user wanted to use for
stream processing:

```haskell
    runQueryFold
      :: QueryRunner columns haskells
      -> Postgres.Connection
      -> Query columns
      -> b
      -> (b -> haskells -> IO b)
      -> IO b
```

This function starts with an initial value `b`, repeatedly applies
the step function `(b -> haskells -> IO b)`, and finally returns the
accumulated result `b`.

The user proposed to use this fold to build a stream, then process that
stream—for example:

```haskell
    runQueryFold conn q StreamK.nil $ \xs x -> pure $ StreamK.cons x xs
```

This approach is counter-productive. The fold above runs an IO loop
that *accumulates* all input elements into a stream in memory before
returning it. The memory consumption grows proportionally with input
size, so for infinite or very large inputs, this will lead to memory
exhaustion.

### Why This Is a Problem

This is *not* how streaming is intended to be used. Streaming is
for *on-the-fly* processing, not accumulation. Instead of building
the entire stream first and then consuming it, the generation and
consumption should happen simultaneously in the same loop. This way,
each element is processed as soon as it is generated, keeping memory
usage constant regardless of input size.

Streaming operations are part of a *generate-transform-eliminate*
loop, fused together for efficiency. In each iteration, one element is
generated, processed by subsequent stream operations (like `map` or
`fold`), and then the loop continues with the next element.

### Proper Streaming Pipeline

To use streaming properly in this context, you can lift a step-wise
generation function into a stream with `unfoldrM`. For example, if your
generation logic provides a step function returning `Maybe` values, you
can convert it into a stream like this:

```haskell
    Stream.unfoldrM step  -- generate step in the pipeline
      & Stream.mapM transform  -- transform step
      & ...                   -- additional stream operations
      & Stream.fold Fold.drain  -- eliminate step
```

This way, the entire pipeline remains fused, processes elements
incrementally, and avoids accumulating the entire input in memory.

<!--
Need to provide exact way how folds can be used in this context. Specifically,
in the step part of the runQueryFold we can run a Fold one input at a time and
return the Fold. Thus the type b would actually be a `Fold m haskells b`.

### Using Consumer Stream

The signature of the function `runQueryFold` tells us that it wants you
to do all the processing in the `b -> haskells -> IO b` step and only
return a final value `b` after the entire processing is done.

If you want to use this function and compose your processing in a
streaming manner then you have to essentially compose your streams as
part of the step of this function itself. And, you can do that very well
using the Fold type in streamly. Fold is basically like Stream but it is
consumer side stream rather than producer side stream.

You can lift the `runQueryFold` function into a streaming `Fold` using:

```haskell
foldlM' :: Monad m => (b -> a -> m b) -> m b -> Fold m a b
```

Then you can use routines from the Fold module to compose and process
the stream. Fold and Stream provide you equivalent functionality, they
just compose in opposite manner.
-->

## Streamly Specific Optimizations

### `fold` vs `foldBreak`

`fold` and `foldBreak` provide two different ways to consume a
stream. We can compose the cosumer folds into a single `Fold` type and
then run it using `fold`. Alternatively, we can use `foldBreak` to run
individual folds on the stream and get back the remaining stream each
time, then run the next fold on the remaining stream. For streams that
use a mutable state e.g. a stream from a file handle or a socket, the
`foldBreak` method may turn out to be more efficient.

## Build times and space considerations

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

## perf-lint

A `perf-lint` tool is planned as part of the roadmap to help automate
many of these performance optimizations. The goal is to provide
actionable feedback on common performance pitfalls and suggest
improvements—similar to how `hlint` works for general code style.
