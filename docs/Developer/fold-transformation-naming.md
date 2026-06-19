# Naming the input-side mapping transform

Current name: lmap
Options: lmap, contramap, premap, adapt, plug

Pros of lmap:

* related to map/fmap
* left map, right map symmetry
* profunctor familiarity
* established vocabulary
* short

Cons of lmap:

* related to "map" -- we think of "map" first and invert that. The association
  in the brain is with "map" first rather than directly to the concept. So when
  thinking about it our starting point is the post-composed "map" and we
  have to invert that to precompose it.
* When thinking about "lmap f fold", our starting point is the fold and
  not what we are mapping on it, we think of the fold and try to attach
  the function before it, which is inverted, not the same as the direction
  of data flow.
* jargon, opaque, not a direct intuitive association
* the associated Covariant, Contravariant, Profunctor jargon makes
  things even more difficult for a normal user.

How "plug" solves the problem:
* not related to "map", an independent brain hook
* evokes "plugging adaptor f into the socket fold", a direct way of thinking
  using "f" as the starting point, in the same direction as the data
  flow. "f" is an adaptor that we are plugging into the input of the
  fold which is a socket.
* no jargon, common intuitive name using the plug-and-socket analogy.
* The "plug" and "map" pair is better than "lmap" and "rmap" because of
  independent concept association hooks rather than a common "map" hook.
* combines nicely with other verbs, e.g. "plugUnfold" can be used to unfold the
  input.
* with "plug" we do not need rmapM on a fold, it can just be unambiguously
  "mapM".
* short, same length as lmap

Cons:
* not familiar, new vocabulary
* There is an entire family of "map" root based names:
  * mapMaybeM f = lmapM f . catMaybes
  * mapMaybe f = lmap f . catMaybes
  * drainMapM f = lmapM f drain
  * foldMap f = Fold.lmap f Fold.mconcat
  * foldMapM f = Fold.lmapM f Fold.mconcat

An advantage of keeping the "l" and "r " terminology is that we can use
those to disambiguate whenever there is a need. For example, if we ever
need a covariant mapMaybe we can call it rmapMaybe or if we need to
distinguish the left ones we can call them lmapMaybe, and when we don't
we can just call it mapMaybe.


The Profunctor "dimap" equivalent could be called "plugMap"; it is even better
because it has a self-documenting argument order, though we do not need
it.

If we want to provide profunctor instances in future, we anyway should not use
the name "lmap".

premap and contramap have the same pros and cons as lmap. premap is slightly
better than lmap. "premap" and "plug" differ in the same way as "." vs ">>>",
or "$" vs "&".

"adapt" is vague because it is symmetric between input and output and does not
tell which one.

## Code examples:

Folds:
```haskell
f =   Fold.filter odd -- data in
    $ Fold.plug (+1)
    $ Fold.sum -- data out
```

Unfolds:
```haskell
unfoldFirst :: Unfold m a b -> Unfold m (a, x) b
unfoldFirst u = Unfold.plug fst u
```

When you "plug fst" into an unfold, you will perform "fst" on the input first
and then feed the first element to the unfold. So the unfold got adapted to a
tuple.

Even though "plug" solves the problem to some extent, the problem
still remains to some extent, and the root of the problem is that the
final result of the actual transformation is inverse of the forward
operation being composed at the input e.g. the operation being composed
here is "fst" which projects the first element of a tuple, however the
result of the transformation is the input becoming a tuple.

# Pushing Fold/Scanl Combinators to Scan

Transformation combinators are contravariant for Streams and Covariant
for Scanl and Fold. Transformation combinators like , "filter", "take",
"mapMaybe" and even "map" would be written as "scans" in our new
composable "Scan" type. The "Scan" type does not have a left or right
distinction, scans can compose on right of streams or they can compose
on left of folds. So we can have a mapMaybe scan which can use with
stream or with fold, it will be just mapMaybe, no left or right. And
two scans anyway are symmetric and can be composed in any order e.g.
"take . filter" or "filter . take" so there is no rigid left or right,
it is based on how you compose them. With that scheme in future, the
left/rightness of most combinators will dissolve.

And if Scan pans out as expected then all the stream/fold
transformations will become l/r agnostic scans. So the question of l/r
naming becomes moot. So we should wait for that before renaming to
plug/plugM because even those would be map/mapM scans.

Only distinction that remains would be the lmapM in unfolds and
rmapM in Fold.
