# Streamly

This document is yet to be written. It is supposed to provide an overall
design overview for developers.

## Types

Basic producer and consumer types:

```
Stream m a
Fold m a b
```

`Stream m a` generates a stream consisting of values of type `a`. `Fold m a b`
consumes a stream consisting of values of type `a`. These two are duals of each
other.

The types do not look of dual shapes, the asymmetry is because of the fact that
folds have to return a value and streams have to consume a value. Folds make
the return type explicit in the type whereas streams encode that in the
generator function signature.

```
fromList :: [a] -> Stream m a
toList :: Fold m a [a]
```

Both folds and streams have an opaque internal state. In case of streams the
function argument automatically gets injected into the state. In case of folds
we need to explictly extract the output from the state.

Unfolds are a variation over stream where we make a component of the state as
global/static over the whole computation rather than keeping it dynamic via the
function argument. Similarly, refolds make a component of the fold state
global/static.

Returning the remaining seed value from the Stream type is like taking a
starting value of accumulator in each fold. That is equivalent to like the sum
fold being written as `sum :: Int -> Fold m a b` and all other folds also
written like that. But we do not do that.

Unfolds are a variation on stream that extend only the nature of the initial
value of state, making it static, it does not return the remaining seed.
Producer extends it to return the remaining seed. Refold on the other hand
introduces two things at the same time, we can now supply the initial value of
the accumulator (which we did not do before) and we make it static at the same
time. Refold and Producer are dual. What is the dual of Unfold? It would be
making the nature of "extract" static in the Fold? Basically exposing the state
type?

What is the duality in Folds and Streams in terms of the Monad instance? Folds
use a Result monad whereas Streams use a monad to compose stream elements.
ConcatMap starts another stream when one stream finishes, it then starts
generating another stream from the next seed. In folds, one fold finishes and
then the next fold starts taking previous fold's result into account.

Using stream concat if the previous seed has a leftover we cannot take that
into account in the next stream generation because streams do not return the
leftover state in the end. But we can use foldBreak in the base monad to be
able to achieve that. Or we can use Producer in the base monad.

However, foldBreak only gives you the leftover output stream. Sometimes we have
leftover input which may have to be combined with the next input. That can only
be done by the producer or "Stream a m r" monad. For example if we have a
stream of arrays and we are parsing it using Word8 folds then we may have some
leftover array that has to be combined with the next array. But we can combine
the leftover stream with the next stream instead of combining inputs.

With "Stream a m r" we can extract the residual seed at any point of time from
the stream.  We can extract it from a partially consumed stream e.g. from the
one returned by foldBreak. Extracting the residual seed gives us freedom to use
it in a different way, rather than using it in a specific type fo stream. For
example we may generate a Word8 stream from an Array and consume it using some
Word8 folds. But then we may want to extract the residue and generate an Array
chunks stream from it to consume via Array folds.

-- Without the result
Source m a <=> Sink m a (ListT)
  Source => Unfold m a b  (Unsink?)
  Sink m a => Resink m a b (static starting accum)

-- With result
-- fromList/fromFoldable seem to be the only generator functions that can
-- possibly return a result. And unfold from a Producer. Basically only the
-- functions that generate from a functor.
Stream a m r <=> Fold a m r
  Stream => ... m c a b  (Unfold?) (ReUnfold/ReStream? - resumable unfold/stream)
  Fold => Refold m c a b

Parser a m r -- error and backtracking (random access)
Generator a m r -- error and backtracking (random access)

## Tricky Parts

The state-passing through each API is currently fragile. Every time we run a
stream we need to be careful about the state we are passing to it. In case of
folds where there is no incoming state, we start with the initial state
`defState`. When we have an incoming state passed to us there are two cases:

1. When we are building a concurrent stream that needs to share the same `SVar`
   we pass the incoming state as is.
2. In all other cases we must not share the SVar and every time we pass on the
   state to run a stream we must use `adaptState` to reset the `SVar` in the
   state.

When in doubt just use `adaptState` on the state before passing it on, we will at
most lose concurrency but the behavior will be correct.

There is no type level enforcement about this as of now, and therefore we need
to be careful when coding. There are specific tests to detect and report any
problems due to this, all transform operations must be added to those tests.

## Direct vs CPS Types

In future we can remove the rewrite rules to make this explicitly StreamK, but
then the existing implementations may start showing bad performance. If we want
to separate these we should do that now.

Fused (Direct) (Data.Stream.Fused)
  Stream (Monad and semigroup won't scale)
  Unfold (fused n-ary appends/interleave/cross for finite n)

Nested (CPS - currently Data.Stream)
  Consability
  Append (infinite)
  Interleave (finite)
  Zip (finite)
  Cross (finite)

Concurrent
  Mostly nested
  Can use direct in some cases?

Data.Stream (fused)
Data.Stream.CPS (CPS.fromDirect/fromBasic/fromRaw) (Data.Stream.Cont?) (StreamK)
Data.Stream.Chunked (ArrayStream) (Direct inside CPS) (Data.Stream.Container?)
Data.Stream.Concurrent
Data.Unfold (fused with nesting) (Data.Stream.Unfold?)

CPS will supply the cons, append and concatMap operations. We can supply the
cross monad as the default monad instance. An appending monad is also possible
using a result type.

Data.Pipe (fused, consumer and producer)
Data.Pipe.CPS (fused)

Data.Fold
Data.Fold.CPS
Data.Fold.Chunked
Data.Fold.Concurrent
Data.Refold (fused with nesting)

Data.Parser
Data.Parser.CPS
Data.Parser.Chunked

Remove the rewrite rules from Data.Stream and just make it direct streams. We
can always add rewrite rules in future, but removing would be disruptive.

One problem with rewrite rules is the problem in recursive use of functions
that are using fromStreamK/toStreamK.

This will also solve the problem of using append2, interleave2 etc, now we can
have the direct versions and CPS versions of these.

We do not usually need to convert CPS to direct. But it can be useful in some
direct style operations if one stream is CPS and other is direct. We can
possibly get some perf boost by fusing the direct style stream in that
operation. e.g. appendD or zipWithD operations.

The exception operations are only supported by StreamD. Therefore, if we want
to use exception operations for StreamK we need to convert it to D. This means
we require both ways conversion.

Nested may not be the correct characterization for CPS streams, because we can
do nesting with Unfolds as well. But unfolds require a visible state. Also CPS
can do infinite appends but unfolds cannot. Just call it Data.Stream.CPS? or
Data.Stream.Functional (vs Data for basic streams).

Type naming:
  StreamK - CPS (ContStream)
  StreamZ - zipping (ZipStream)
  StreamC - Chunked (ArrayStream)

## Stream Types

Possible types and how they can possibly and usefully compose:

```
Stream m,output
    => output nesting (Stream m a)
Stream m,output,result
    => output nesting, output appending
Stream m,output,state injection
    => output nesting, static optimization (Unfold m a b)
Stream m,output,state injection/extraction
    => unfolding (stop and resume) (Producer m a b)

Fold m,input
    => distributing (Sink m a)
Fold m,input,result
    => distributing, splitting (Fold m a b)
Fold m,input,result,state injection
    => distributing, splitting, static optimization (Refold m a b)
```

Fold

Even though Source and Sink are the duals, because of our programming model,
Source makes sense as a default type and Fold makes more sense than Sink as
a default type.

The ListT style monad has the drawback that the second stream can be run
multiple times, which in some cases may not work as expected because the stream
is stateful. Can the appending monad be more useful than ListT?
