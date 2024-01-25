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
