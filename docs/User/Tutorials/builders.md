# Builders

Builders are left associated data structures where you can incrementally build
a data structure by appending items.

You might think why not build a stream incrementally by using the `<>`
or `serial` operation, however, it is a naturally right associated
operation and does not perform well when left associated. However, you
can use `Stream.cons` to build a stream and reverse it in the end. That
is not ergonomic.

Mutable arrays ("Streamly.Data.MutArray") are basic builders. You can
use the 'Streamly.Data.MutArray.snoc' or 'Streamly.Data.MutArray.append'
operations to incrementally build mutable arrays.

To build a left associated stream in memory incrementally use `Fold.toStream`.
Use `Fold.snoc` to append an element to the fold, `Fold.append` to append a
Refold, extending the fold, and `Stream.foldOn` (addToFold/foldAdd/foldAppend)
to append a stream (or list), extending the fold.  When done use `Fold.finish`
(close?) to extract the structure.

Similarly, to build a list use `Fold.toList` and to build an array use
`Array.write`. To build a stream of arrays use `Fold.many Array.writeN
Fold.toStream` in the same way.

----

-- We can efficiently concat a stream of streams.
-- We can make builder as a stream of streams.

