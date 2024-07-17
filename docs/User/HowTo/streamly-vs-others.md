# Comparison with other streaming libraries

See the streaming-paradigms document. Streamly uses a simpler list like
representation for streams rather than a trasnducer style. streaming lib uses
the same style but complicates things by making it functor general. streamly
reamains true to plain haskell lists even though it provides very powerful
streaming functionality.

Unlike `pipes` or `conduit` and like `vector` and `streaming`, `streamly`
composes stream data instead of stream processors (functions).  A stream is
just like a list and is explicitly passed around to functions that process the
stream.  Therefore, no special operator is needed to join stages in a streaming
pipeline, just the standard function application (`$`) or reverse function
application (`&`) operator is enough.

## Advantages

* Simpler and intuitive types and operations. Just an extension of lists.
* Declarative concurrency
    * repeatM
    * concatMapWith
    * iterteMapLeftsWith
* Fused processing:
  * Bytestring processing can be done by streams
  * Text processing can be done by streams
  * Why did we need special types like bytestring and text? Because there was
    no general way of processing data faster using streams. In many other use
    cases where you would otherwise employ custom solutions for that kind of
    performance you can just use general purpose streams.
  * You won't require bytestring, text or vector.
  * Examples: Convert an array of chars to upper case - compare perf
    with other libs.
* Write otherwise complex programs in a simple idiomatic manner:
  * camelCase
  * encodeUtf8
  * sortBy
  * split (coreutil)
  * stream splitting (e.g. splitOnSeq)
  * ...
* Wholistic big picture view
  * Folds and parsers are integrated
  * Arrays are integrated
  * Reactive programming
  * Many other functionality

## Disadvantages

* Bidirectionality.
* Fusion may require more compilation time and resources to compile your
  program. But that's the price you pay for runtime performance and simplicity
  of programs.
