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
