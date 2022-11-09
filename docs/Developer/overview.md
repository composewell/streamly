# Streamly

This document is yet to be written. It is supposed to provide an overall
design overview for developers.

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
