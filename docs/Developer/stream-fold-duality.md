# Generation and Elimination Duality

The process of stream generation and folding are time-reversed duals. A
stream generator takes a single value (a seed) and produces a sequence
of elements from that whereas an eliminator (fold) consumes a sequence
of elements and creates a single value in the end (accumulator).  Run
one backwards and you get the other.

The process of generating a stream is unfolding from the seed, the
process of consuming is folding to an accumulator.  The seed at the
*start* of the stream generation process is the dual of the result at
the *end* of a fold; the b's in `Stream m b` are dual to the b's in
`Fold m b a`.

Reversing the arrows changes an elimination function to a generator
function and vice-versa:
* Generation: `a -> Stream m b`
* Elimination: `Stream m b -> a`

While the Stream type represents a source (data), the Fold type
represents a stream eliminator function.

* Generation: `fromList :: [b] -> Stream m b`
* Elimination: `fold :: Fold m b a -> Stream m b -> m a`
* If we use the `toList` fold as the argument to `fold` we get a
  `Stream m b -> m [b]` result.
* `fold (f :: Fold m b a)` is equivalent to an eliminator function
  `Stream m b -> a`.
* `a -> Stream m b` is a generation function with an opaque (uncomposable)
  seed, `Stream m b -> a` is an elimination function with an opaque
  (uncomposable) accumulator.
* If `toList = fold Fold.toList` and `x <- toList $ fromList a` then x
  is same as `a`. Thus folding exactly reverses the generation process.

# Stream and Fold Type Duality

Note that the function `fold (f :: Fold m b a) :: Stream m b -> m a` is
the exact dual of the function `a -> Stream m b`.

While `Fold m b a` has a function like shape, `Stream m b` has a data
like shape. `Stream m b` is a curried representation of `a -> Stream m
b` whereas `fold f` is a representation of `Stream m b -> m a`. The seed
type gets erased in the Stream after currying, as it assumes the data
shape. This data is then supplied to the function shaped fold to get
back a final value. So the result type in a Fold cannot be erased.  So
these two concrete types are not exact duals in the shape but they are
conceptually duals as will see below.

The `Unfold m a b` type has a function shape similar to `Fold m b a` and
the way `fold f` gives us `Stream m b -> m a`, `unfold u` gives us `a
-> Stream m b`. So the Fold type is playing the exact same role as the
Unfold type and they are of the same shape. So are Fold and Unfold the right
duals? The answer is no, they are not duals.

The types Stream and Fold, even though of different shapes, are duals
in the sense that as the seed is opaque in Stream type and in the same
way the result is opaque in the Fold type. So Stream cannot compose
and fuse the seed and Fold cannot compose and fuse the accumulator. In
that aspect these two types are similar and sort of duals. The way we
use these two in the programs is very similar and opposite. Unfold is
not used in a similar way as Fold because of the transparent seed in
Unfold and opaque accumulator in Fold. Unfold is used in nested fusion
use cases whereas Stream is the regular Stream type providing the same
ergonomics as Fold. They have the same power and usage on production and
consumption side.

On the other hand the `Refold m c b a` type has the ability to inject
the accumulator in the fold, therefore it is more like the `Unfold m
a b` which can inject the seed in the stream. The way Unfold can be
used in nested producer fusion, Refold can be used in nested consumer
fusion. So in that sense these two types are the real duals.

# Dual Types

The producer types have one less arity then their dual consumer types. And that
is because in the producer types the seed can be implicitly absorbed and curried
reducing the arity by one. In the consumer types that is not possible.

* `Stream m b` (arity 0)  <=>  `Fold m b a` (arity 1)
* `Unfold m a b` (arity 1)  <=> `Refold m c b a` (arity 2)

# `Unfold` is a dual of `Refold`, not `Fold`

Time-reversing an `Unfold` yields a sink that consumes the emitted stream
and produces the seed's counterpart at the end -- but the reversed machine
still has to be *started*, and that starting accumulator is a free choice.
Keeping the duality exact means exposing it, mirroring the exposed seed on
the source side: hence `Refold`'s injectable accumulator `c -> m s`.

A plain `Fold` nails its initial down as `m s`, throwing that freedom
away — which is exactly why it pairs with `Stream`, whose seed is likewise
fixed inside the existential.

# Generalized Stream Type

We can generalize the stream type to "Source m a r", "Source a m r" or "Source
m r a" or "Source r m a". Where `r` is the result when the stream finishes by
itself or the driver extracts it if the driver is done. This type would be
symmetric to the "Fold m a r" type. Thus we have the duality:

* Stream m a <=> Sink m a (simplified stream and fold types)
* Source m a r <=> Fold m a r (types with a result)

What can the result be for a stream?  For example a fromList operation can
return the remaining list in the end. In general it could be the state of the
stream or the remaining seed or whatever makes sense. Thus that return type
could usually be the seed type. For example, `fromList :: [a] -> Stream m a
[a]`.

Stream is covariant in both "a" and "r", therefore, Monad instances are
possible both ways:
* Source m a r : Appending Monad
* Source a m r : Appending Monad, MonadTrans
* Stream m a   : Nesting Monad, MonadTrans
* Source r m a : Nesting Monad, MonadTrans
* Source m r a : Nesting Monad

Fold is covariant in "r" bit contravariant in "a", therefore, Monad instance is
possible only for the covariant cases, for the contravariant case we can have a
comonad:
* Fold m a r : Appending Monad
* Fold a m r : Appending Monad, MonadTrans
* Sink m a   : Nesting Monad, MonadTrans
* Fold r m a : Nesting Monad, MonadTrans
* Fold m r a : Nesting Monad

For folds the appending instance is more useful, for streams the nesting
instance is more useful.

We should come up with a scheme to manage the types, appending and nesting
instances can follow some naming scheme, and similarly the simplified types
without the result can also follow a scheme.

# Dual Typeclasses

Possible Stream type classes:
* Functor (fmap on r and a)
* Applicative (crossWith a, appendWith r, mergeBy a, zipWith a)
* Alternative (cross a, append a, merge a)
* Monad (concatMap a, appendMapResult r)

Make "append" an Alternative for streams like in list case.

Possible Fold type classes (Covariant):
* Functor (fmap r)
* Applicative (splitWith r, teeWith r)
* Monad (appendMapResult r)

Possible Fold type classes (Contravariant):
* Contravariant (lmap a)
* Divisible (unzipWith a)
* Decidable (partitionBy a)

Additional Scanl type classes (Covariant):
* Comonad (duplicate)

# Dual Operations

A dual is defined as: if we reverse the direction of an operation we get
the other operation. An analogue or correspondence is an operation which
is similar to another operation in some dimension or some sense.

The first column lists stream operations and the second column fold operations.

## Introduce <=> Eliminate

Constructors:
unfoldr                foldl'
nil/nilM               fromPure/fromEffect (produces nothing : consumes nothing)
fromPure/fromEffect    one                 (produces one : consumes one)

Drivers:
fold        drive        (whichever stops first)
foldBreak   addStream    (returns Stream, returns Fold)
foldEither  foldEither   (returns either fold or stream)
uncons      addOne/snoc  (produce/consume one at a time) -- rename to consume/consumeOne

Fold.duplicate ~=~ Fold.addStream.

Generators/Eliminators:
fromList    toList
repeat      the
replicate   _ (theN) (repeatN?)

# Correspondences

nil         drain
fromPure    fromFunction?     (fromStep)
fromEffect  fromFunctionM?    (fromStepM)
cons        snoc              (cons is stream builder, snoc is fold builder)

## Transformations: Output end <=> Input end

Same transformer, opposite end of the pipeline: downstream of a source
vs upstream of a sink.

mapM        lmapM
filter      filter
filterM     filterM
mapMaybe    mapMaybe
catMaybes   catMaybes
catLefts    catLefts
catRights   catRights
catEithers  catEithers
take        take
scanl       scanl
postscanl   postscanl
scanMaybe   scanMaybe
sequence    sequence
morphInner  morphInner
groupsOf    groupsOf

## Insertion <=> Deletion

insertBy    deleteBy

## Binary Combining

append      splitWith (dual)
interleave  interleave (two-to-one, one-to-two) -- missing fold API
mergeBy     partitionBy (merge two into one, split one into two)
cross       teeWith (forks the output, shares the input) -- monoidal/comonoidal
cross       classify (spreads for each, collects for each)
zipWith     unzip   (zip two into one, split one into two)
zipWith     teeWith (both are two-to-one zips)

Missing API: Fold side interleave is missing. The fold side n-ary interleave
can also be implemented.

## N-ary Combining

Stream/unfoldEach <=> Stream/foldMany (production and consumption duals)
Stream/foldMany === Fold/many
Stream/unfoldEach === Fold/unfoldEach
Stream/foldIterate === Fold/concatMap (dependency chaining of folds)
Fold/many <=> Fold/unfoldEach

Stream/foldMany: apply the same fold repeatedly and collect the results
    in a stream. If we look at the axis of what is being applied, here a
    fold is being applied repeatedly and the fold side operation where a
    fold is applied repeatedly on a fold is "many". The difference between this
    and Fold/many is the "collector", only the collector flips in the two
    cases, in one case it is a stream in the other case it is a fold.

Fold/many: consume with the same fold repeatedly and collect the results
    in a fold. It is a nesting of two folds where in the outer layer the same
    fold is applied repeatedly and the result is fed to the inner fold.
    In one way it is similar to Stream/foldMany, in another way it is similar
    to Fold/concatMap.

fold (Fold.many split collect) s  ⟷   fold collect (Stream.foldMany split s)
Stream/foldMany <-> Fold/many (collector flip)

Stream/concatMap: produce from each element and collect in a single stream.
    This is similar to Fold/many which consumes many elements and collects in a
    single Fold.

Stream/concatMap and Fold/many are companions but not arrow-duals
— they're the cardinality flip. concatMap is expand-and-flatten (1
element → many, glue into one stream); many is group-and-collect
(many → 1 per chunk, glue into one fold). Same shape ("map a
sub-computation over the sequence, flatten into one"), opposite
cardinality.

In Stream/concatMap we generate a stream from "a", in Fold/many we fold a group
elements into "b". So the "a" and "b" correspond to each other.

Stream/concatMap <-> Fold/many (cardinality flip)

concatMap :: (a -> Stream m b) -> Stream m a -> Stream m b
foldMany' :: Stream m b -> (Stream m b -> a) -> Stream m a
foldMany :: Stream m b -> Fold m b a -> Stream m a
many :: Fold m b a -> Fold m a c -> Fold m b c

"many" is just "foldMany" expressed in terms of Fold types.

Fold/concatMap: the current implementation is - a Fold consumes some
    elements, finishes and returns a value, the next Fold depends on this
    return value. This is different from the usual stream concatMap. A
    variant of stream concatMap dual to this would be a stream finishes and
    the next stream generated depends on the result of the consumption of
    previous stream.

Like we have foldIterate as stream side equivalent of the Fold/concatMap we can
have a fold side equivalent of of the Stream/concatMap: if the stream finishes,
the next stream is decided by the fold result and the fold resumes.
```
unfoldIterate :: Unfold m b a -> Fold m a b -> Fold m a b
streamIterate :: (b -> Stream m a) -> Fold m a b -> Fold m a b
foldIterateM :: (b -> m (Fold m a b)) -> m b -> Stream m a -> Stream m b
```

XXX The name concatMap on the fold side gives an impression that this is a dual
of the stream side concatMap but it is not, should we rename it to make the
meaning clearer? Something like chainMap, appendMapResult?

# Stream only operations

Operations which do not have sensible or perfect correspondences on the fold
side.

cross

If stream cloning operation were to exist, that would sort of correspond to
distribute. Scanl distribute performs that job for streams.

# Fold only operations

Operations which do not have sensible or perfect correspondences on the stream
side.

demux/classify style ops: they sort of correspond to cross, and when the
next fold is decided by the key, sort of corresponding to the stream side
concatMap.

distribute :: [Fold m a b] -> Fold m a [b]

# Stream Fold Naming

For naming consistency we consider the following axes:

* Duality of Stream and Scanl/Fold/Parser
* Correspondence of Stream/Scan
* Correspondence of Scanl/Fold/Parser
* Consistency with Data.List
* Consistency with type classes esp. Functor, Applicative and Monad
