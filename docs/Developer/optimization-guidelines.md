# Guidelines for writing well optimized code

Stream operations are always part of a loop. Usually the loop consists of a
stream generation or an unfold operation followed by stream transformation
functions (e.g. map) and then a stream elimination operation or a fold
operation.

The default or most common stream representation used in streamly is
`StreamD` which is a direct style (compare with CPS representation
`StreamK`) stream representation.  All the direct style operations in
a loop "fuse" together to form a tight machine loop eliminating any
intermediate constructors, therefore, reducing allocations and cpu cost.
This elimination of intermediate constructors in a stream loop is known
as stream fusion.

## Writing high performance code using streamly

This section outlines guidelines for writing code using the available
combinators. The guidelines for writing new combinators are provided in
the following section.

Note that there is no absolute benchmark for performance, most of
the time even without following the guidelines you may get excellent
performance for the task at hand. Two important points to keep in mind
before you optimize:

* See if you actually need more performance
* See if the code you are optimizing is in fast path

### Stream Fusion

When your performance requirements are stringent you may want to care
about not breaking stream fusion unnecessarily.  Certain operations do
not fuse and act as a barrier to stream fusion. If these operations are
part of a loop, the loop won't fuse completely. In some cases these may
be necessary but in others it may be possible to replace these with
better fusing operations. These operations include:

* Avoid unnecessary use of stream append operations in code that should
  fuse.  Append operations in general force CPS style breaking stream
  fusion.  Some direct style append operations can fuse but they won't
  scale for more than a few appends.
* Avoid unnecessary use of the stream monad if fusion is important. You
  may use functor or applicative though.
* Concurrent stream operations cannot fuse
* Operations involving exception primitives like catch/throw/mask
  on elements of the stream cannot fuse.
* Stream loops involving FFI calls on the elements of the stream

The haddock documentation includes a note when an operation cannot fuse.

### Unfolds

* Use unfolds especially when higher order operations are involved. For
  example, `unfoldMany` can fuse completely whereas `concatMap` would
  not fuse.
* Use `outerProduct` in `Unfold` module instead of using the monad instance of
  streams to fuse nested loops where performance matters. See the unfold
  benchmarks for an example. `outerProduct` can give you C like nested loop
  performance.

### Inlining

To make sure fusion can occur INLINE any operations that are part of a
stream loop but factored out as separate functions.

### Strictness

Keep the fold and scan accumulators strict. You could also consider
using mutable state in a fold accumulator if the state is large. A
large immutable structure as an accumulator may cause optimization
issues. See the `WordCount` example for an example of this case.

## Writing streamly combinators

To enable stream fusion in a direct style stream all the operations that
are part of a loop must be inlined:

* Use an explicit INLINE pragma on any combinator that could be part of a loop
  to ensure they will be inlined.
* When a higher order function consumes another function then ensure that the
  higher order function is inlined in the same phase or before the function it
  is consuming. Use appropriate inline phases to ensure proper inline ordering
  when required.

### Streams and Unfolds

Stream and Unfold State.  Direct style stream and unfold combinators use a
state data type to represent the internal state of the stream/unfold generator.

* In some cases we may have to add a `FUSE` annotation on the state type to
  ensure that all the internal join points created by GHC are inlined with the
  help of the `fusion-plugin`.
* In rare cases we may need to use a strictness annotation on the state
  to allow fusion. `parseMany` and `splitOnSeq` are such examples.

Step function of a stream or unfold:

* Do not introduce unnecessary states. If there is only one entry point of a
  state then perhaps you want to collapse the state into the state from which
  it is called. More states means more jumps and may affect code locality, and
  efficiency of low level code (e.g. register allocation) because of
  independent placement of the code blocks in different states.

  In some cases we may have to go against the above guideline and introduce a
  separate state even though it is not necessary. See the GroupConsume state in
  foldMany for an example.
* Keep minimum possible data in state. More variables in state may lead
  to poor performance because the state may not fit into registers and the
  spill may cause allocations on each iteration of the loop. Mutable state
  may help in such cases.
* The step function must be annotated such that it gets inlined after the main
  combinator (`INLINE_LATE`).

Multiple yield points or single?:

* A single yield point is usually desirable, however, not always necessary.
  In some cases multiple yield points may in fact be needed for fusion,
  see `splitOnSeq` for an example. Or maybe its fusing because of a
  direct yield instead of going through an indirect common yielding
  state.

Recursion in step function:

* In general, we avoid making the step function recursive. Use the
  `Skip` constructor to remove recursion. Recursive step function can
  introduce optimization barriers that are harder to remove by the GHC
  simplifier.

  However, in some cases it may be better to have a local recursive
  loop. A recursive loop can help us avoid threading around some large
  state values every time. Values that do not change across a loop can
  be factored in the scope outside the loop (static argument transform),
  this way we can create a local loop which is more efficient than
  threading around the state in a larger loop.  See the `splitOnSeq`
  combinator as an example where we use a local recursive loop, it fuses
  and is significantly efficient compared to using `Skip`.

  In a local recursive loop use SPEC and annotate even the rest of the
  loop arguments as strict where needed. We have observed that when the
  arguments were not strict the loop does not fuse (splitOnSeq).

### Fold and Parser drivers

Recursive loop closing operations:

* When writing recursive looping combinators using StreamD (e.g. foldlM' in
  StreamD) use a strict SPEC argument in the recursive loops to ensure that
  "Spec Constructor" optimization removes boxed arguments and reduces
  allocations.

### Fold and Parser combinators

State of a Fold or Parser:

* The accumulator of a fold must be a strict data structure. Use the strict
  data structures provided in `Streamly.Internal.Data` for this purpose or use
  explicit bang annotation to make the data strict.
* In some cases you may need a `FUSE` annotation on the state type to ensure
  that any internal join points created by GHC are always inlined with the help
  of the fusion-plugin.

The step function of a Fold or Parser:

* Never make the fold step recursive, recursion creates an optimization barrier
  for the GHC simplifier. Use `Partial` or `Continue` constructors to avoid
  recursion.

* Sometimes you may need an explicit INLINE on the step function.

### NOINLINE, isolating the closed loop

We want the loop iterations to be optimized and the loop stages to be fused to
generate a tighter loop. However, it is not necessarily optimal to inline the
whole loop itself into a parent function. For example, consider the following
function in the `FileSystem.Handle` benchmarks:

```
{-# NOINLINE readWriteAfter_Stream #-}
readWriteAfter_Stream :: Handle -> Handle -> IO ()
readWriteAfter_Stream inh devNull =
    let readEx = IP.after_ (hClose inh) (S.unfold FH.read inh)
     in S.fold (FH.write devNull) readEx
```

If this is inlined into a parent benchmark group list, this leads to
many times performance degradation. That's because inlining the loop
into a bigger structure interferes with the optimization of the loop
body itself and it may not fuse. Whereas it is desirable to INLINE all
the stages of a loop, it is often not useful to inline the whole loop
itself, in fact we may have to occasionally use a `NOINLINE` so that the
compiler does not inline it.

We need to be careful about NOINLINE on polymorphic functions. When
NOINLINE is used, the function cannot be specialized causing huge
performance degradation.  To avoid that we can either make the
function monomorphic or sometimes using `NOINLINE [0]` can do the
trick. `NOINLINE [0]` blocks inlining in earlier phases but lets it
specialize in the last phase, inlining in the last phase is left to
the compiler. `INLINE [0]` may sound the same as `NOINLINE [0]`,
however, it behaves differently because we ask the compiler to INLINE
it compulsorily and it may not give us the desired results. The
Data.Fold.createOfLast benchmark is one such case where `NOINLINE [0]`
provides much better performance than `INLINE [0]`.

### NoSpecConstr

It is not always useful to specialize function calls for all constructors, some
constructors may just add to code bloat or add overhead of passing unboxed
values in a loop. In such cases the `NoSpecConstr` annotation can be useful.
See the `parselMx'` function in `StreamD` module.

### StreamK operations

StreamK uses foldr/build fusion to a very limited degree. StreamK is not the
primary representation in streamly but is used for several operations that
cannot scale in StreamD representation. StreamK is relatively immune to compiler
optimizations. In some cases you may need an INLINE pragma to improve the
performance.

### How to debug non-fusing code?

Strip down the code to a minimal version until it starts fusing and then
start building it up from there adding more things incrementally. At
each stage keep checking if the code is fusing. At some point it
won't fuse. See what we added that made the code not fuse. Go through
the guidelines above to check if we did something that that is not
recommended. Or raise an issue for GHC to be fixed if possible.

# GHC Optimizations In Streamly

There are three important levels of optimizations used in streamly:

* CPS style stream representation (StreamK) to direct style (StreamD)
  conversion and vice-versa using rewrite rules
* Stream fusion using case-of-case optimization in StreamD
* foldr/build fusion using rewrite rules in StreamK

## INLINE Phases

Inlining of functions at the right time is crucial for all these optimization
to work.  A missing inline or inline in an incorrect GHC simplifier phase can
adversely impact performance.  We use three builtin phases of GHC simplifier
for inlining i.e. phase 0, 1 and 2. We have defined them as follows in
  `src/inline.hs`:

```
#define INLINE_EARLY  INLINE [2]
#define INLINE_NORMAL INLINE [1]
#define INLINE_LATE   INLINE [0]
```

We also use INLINE [3] in some cases.

## `fromStreamD/toStreamD` Fusion

The combinators in `Streamly.Prelude` are defined in terms of combinators in
`Streamly.Internal.Data.Stream.StreamD` (Direct style streams) or
`Streamly.Internal.Data.StreamK` (CPS style streams). We convert the
stream from `StreamD` to `StreamK` representation or vice versa in some cases.

Most operations use the StreamD representation, however, stream append
operations and monad instances use StreamK representation because StreamD would
not perform well in these cases. We use rewrite rules to convert from one
representation to another when required. For this reason the combinators in
Streamly.Prelude are written using fromStreamD/fromStreamK etc.

In the first inlining phase (INLINE_EARLY or INLINE) we expand the combinators
in `Streamly.Prelude` into fromStreamD/fromStreamK/toStreamD/toStreamK and
combinators defined in StreamD or StreamK modules. Once we do that
fromStreamD/toStreamD get exposed and we can apply rewrite rules to rewrite
transformations like `fromStreamK .  toStreamK` to `id`. A plain `INLINE`
pragma is usually enough on combinators in `Streamly.Prelude`.

```
{-# RULES "fromStreamK/toStreamK fusion"
  forall s. toStreamK (fromStreamK s) = s #-}
```

Also, we have to prevent fromStreamK and toStreamK themselves from inlining in
this phase so that rewrite rules can be applied on them, therefore, we annotate
these functions with `INLINE_LATE`.

## Fallback Rules

In some cases, if the operation could not fuse we want to use a fallback
rewrite rule in the next phase. For such cases we use the INLINE_EARLY phase
for the first rewrite and the INLINE_NORMAL phase for the fallback rules.

The fallback rules make sure that if we could not fuse the direct style
operations then better use the CPS style operation, because unfused direct
style would have worse performance than the CPS style ops.

```
{-# INLINE_EARLY unfoldr #-}
unfoldr :: (Monad m, IsStream t) => (b -> Maybe (a, b)) -> b -> t m a
unfoldr step seed = fromStreamS (S.unfoldr step seed)
{-# RULES "unfoldr fallback to StreamK" [1]
     forall a b. S.toStreamK (S.unfoldr a b) = K.unfoldr a b #-}
```

## High Level Operation Fusion

Since each high level combinator in `Streamly.Prelude` is wrapped in
`fromStreamD/toStreamD` etc. the combinator fusion cannot work unless we have
removed those and exposed consecutive operations e.g. a `map` followed by
another `map`.  Assuming that redundant `fromStreamK/toStreamK` have been
removed in the `INLINE_EARLY` phase, we can then apply the combinator fusion
rules in the `INLINE_NORMAL` phase.  For example, we can fuse two `map`
operations into a single `map` operation.  Note that now we have exposed the
`StreamD/StreamK` implementations of combinators and the rules would apply on
those.

## Inlining Higher Order Functions

Note that partially applied functions cannot be inlined. So if we have a code
like this:

```
concatMap1 src = runStream $ S.concatMap (S.replicate 3) src
```

We want to ensure that `concatMap` gets inlined before `replicate` so that
`replicate` becomes fully applied before it gets inlined. Currently ensuring
that both of them are inlined in the same phase (`INLINE_NORMAL`) seems to be
enough to achieve that. In general, we should try to ensure that higher order
functions are inlined before or in the same phase as the functions they can
consume as arguments. This means `StreamD` combinators should not be marked
as `INLINE` or `INLINE_EARLY`, instead they should all be marked as
`INLINE_NORMAL` because higher order functions like `concatMap`/`map`/`mapM`
etc are marked as `INLINE_NORMAL`. `StreamD` functions in other modules like
`Streamly.Data.Array.Foreign` should also follow the same rules.

## Stream Fusion

In StreamD combinators, inlining the inner step or loop functions too early
i.e. in the same phase or before the outer function is inlined may block stream
fusion opportunities. Therefore, the inner step functions and folding loops are
marked as INLINE_LATE.

## Specialization

In some cases, the `step` function in `StreamD` does not get specialized when
inlined unless it is provided with an explicit signature or made a lambda, for
example, in the `replicate/replicateM` combinator we need the type annotation
on `i` to get it specialized:

```
    {-# INLINE_LATE step #-}
    step _ (i :: Int) =
        if i <= 0
        then return Stop
        else do
                x <- action
                return $ Yield x (i - 1)
```

`-flate-specialise` also helps in this case.

## Stream and Fold State Data Structures

Since state is an internal data structure threaded around in the loop, it is a
good practice to use strict unboxed fields for state data structures where
possible. In most cases it is not necessary, but in some cases it may affect
fusion and make a difference of 10x performance or more.  For example, using
non-strict fields can increase the code size for internal join points and
functions created during transformations, which can affect the inlining of
these code blocks which in turn can affect stream fusion.

See https://gitlab.haskell.org/ghc/ghc/issues/17075 .

Do not infer types by default
-----------------------------

Having polymorphic types may hinder specialization and hurt performance. See
the specialization section in design/inlining.md in streamly docs

In some cases, the `step` function in `StreamD` does not get specialized when
inlined unless it is provided with an explicit signature or made a lambda, for
example, in the `replicate/replicateM` combinator we need the type annotation
on `i` to get it specialized:

```
    {-# INLINE_LATE step #-}
    step _ (i :: Int) =
        if i <= 0
        then return Stop
        else do
                x <- action
                return $ Yield x (i - 1)
```

In this case if the compiler does not infer the type of i automatically then we
will be forced to provide the type and get it specialized.

Strict State in Streams
-----------------------

We should use the Strict data structures for stream state. In case the state
does not fuse, it will avoid performances problems due to state being lazy.
