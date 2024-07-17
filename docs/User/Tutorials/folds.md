## Folds

THIS DOC IS NOT READY.

Composable consumption of streams

Folds are consumers of streams.  `Streamly.Data.Fold` module provides a `Fold`
type that represents a `foldl'`.  Such folds can be efficiently composed
allowing the compiler to perform stream fusion and therefore implement high
performance combinators for consuming streams. A stream can be distributed to
multiple folds, or it can be partitioned across multiple folds, or
demultiplexed over multiple folds, or unzipped to two folds. We can also use
folds to fold segments of stream generating a stream of the folded results.

If you are familiar with the `foldl` library, these are the same composable
left folds but simpler and better integrated with streamly, and with many more
powerful ways of composing and applying them.

