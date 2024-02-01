Efficient Builders
------------------

We can duplicate a Fold to run a builder partially. However, every time we
add something to the fold we have to rewrap the fold constructors, which may
lead to inefficiency. Can we use mutable state in the fold such that we can use
the same fold and resume it without having to rewrap like a Handle.

One layer of indirection (and therefore garbage generator) can be reduced if we
use  accumulator type, the Partial/Done constructors will go away with this.
Further if we assume mutable state we do not need to return the state in the
"step" function, that can reduce the garbage further.

An always Partial mutable fold can consume a value and not reconstruct any
state of the fold. it would just mutate the existing state.

mutableAppend :: Monad m => Fold m a b -> a -> m ()

The mutableAppend would not change the state of the fold from Done to Partial
or Partial to Done. It would not change any state values, it would only
in-place mutate or perform a side effect. At the end we can use extract on the
fold to extract the state.

Such a fold can build an array or an array stream without generating any
garbage.

Specific folds can provide such side-effects only semantics. We can
possibly use a newtype to wrap such folds.

# Related

* http://hackage.haskell.org/package/mason
* https://chshersh.com/posts/2019-03-25-comonadic-builders
