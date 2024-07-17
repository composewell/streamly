## Unfolds

THIS DOC IS NOT READY.

Unfolds are duals of folds. Folds help us compose consumers of streams
efficiently and unfolds help us compose producers of streams efficiently.
`Streamly.Data.Unfold` provides an `Unfold` type that represents an `unfoldr`
or a stream generator. Such generators can be combined together efficiently
allowing the compiler to perform stream fusion and implement high performance
stream merging combinators.
