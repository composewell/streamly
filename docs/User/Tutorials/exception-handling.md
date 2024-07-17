# Exceptions

THIS DOC IS NOT READY.

Exceptions can be thrown at any point using the `MonadThrow` instance. Standard
exception handling combinators like `bracket`, `finally`, `handle`,
`onException` are provided in `Streamly.Prelude` module.

In presence of concurrency, synchronous exceptions work just the way they are
supposed to work in non-concurrent code. When concurrent streams
are combined together, exceptions from the constituent streams are propagated
to the consumer stream. When an exception occurs in any of the constituent
streams other concurrent streams are promptly terminated.

There is no notion of explicit threads in streamly, therefore, no
asynchronous exceptions to deal with. You can just ignore the zillions of
blogs, talks, caveats about async exceptions. Async exceptions just don't
exist.  Please don't use things like `myThreadId` and `throwTo` just for fun!
