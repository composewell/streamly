## INLINE Phases

A missing inline or inline in an incorrect GHC simplifier phase can adversely
impact performance.  We use three builtin phases of GHC simplifier for inlining
i.e. phase 0, 1 and 2. We have defined them as follows in `inline.h`:

```
#define INLINE_EARLY  INLINE [2]
#define INLINE_NORMAL INLINE [1]
#define INLINE_LATE   INLINE [0]
```

* The combinators in `Streamly.Prelude` are defined in terms of combinators in
  `Streamly.Streams.StreamD` (Direct style streams) or
  `Streamly.Streams.StreamK` (CPS style streams). We convert the stream from
  `StreamD` to `StreamK` representation or vice versa in some cases. In the
  first inlining phase we expand these combinators and apply the rewrite rules
  to rewrite transformations like `fromStreamK . toStreamK` to `id`. A plain
  `INLINE` pragma is usually enough to achieve that.

```
{-# RULES "fromStreamK/toStreamK fusion"
  forall s. toStreamK (fromStreamK s) = s #-}
```

* In some cases, if the operation could not fuse we want to use a fallback
  rewrite rule in the next phase. For such cases we use the INLINE_EARLY phase
  for the first rewrite and the INLINE_NORMAL phase for the fallback rules.

  The fallback rules make sure that if we could not fuse the direct style
  operations then better use the CPS style operation, because unfused direct
  style would have worse performance than the CPS style ops.

```
{-# INLINE_EARLY unfoldr #-}
unfoldr :: (Monad m, IsStream t) => (b -> Maybe (a, b)) -> b -> t m a
unfoldr step seed = fromStreamS (S.unfoldr step seed)
{-# RULES "unfoldr fallback to StreamK" [1]
     forall a b. S.toStreamK (S.unfoldr a b) = K.unfoldr a b #-}
```

*  Assuming that `fromStreamK/toStreamK` have been removed in the
   `INLINE_EARLY` phase, we can then apply the combinator fusion rules in the
   `INLINE_NORMAL` phase. For example, we can fuse two `map` operations into a
   single `map` operation. Note that without removing the
   `fromStreamK/toStreamK` wrapped around combinators, combinator fusion may
   not work.

* Note that partially applied functions cannot be inlined. So if we have a code
  like this:

```
concatMap1 src = runStream $ S.concatMap (S.replicate 3) src
```

  We want to ensure that `concatMap` gets inlined before `replicate` so that
  `replicate` becomes fully applied before it gets inlined. Currently ensuring
  that both of them are inlined in the same phase (`INLINE_NORMAL`) seems to be
  enough to achieve that. In general, we should try to ensure that higher order
  functions are inlined before or in the same phase as the functions they can
  consume as arguments. This means `StreamD` combinators should not be marked
  as `INLINE` or `INLINE_EARLY`, instead they should all be marked as
  `INLINE_NORMAL` because higher order funcitons like `concatMap`/`map`/`mapM`
  etc are marked as `INLINE_NORMAL`. `StreamD` functions in other modules like
  `Streamly.Mem.Array` should also follow the same rules.

* The inlining of the `step` function (in case of `StreamD`) is annotated as
  `INLINE_LATE` so that those are inlined after fusion rules have been applied.

* The inlining of, `fromStreamK/toStreamK` also happens in the `INLINE_LATE`
  phase because the INLINE_EARLY and INLINE_NORMAL phases require them
  uninlined for rewrite rules to work.

## Specialization

In some cases, the `step` function in `StreamD` does not get specialized when
inlined unless it is provided with an explicit signature or made a lambda, for
example, in the `replicate/replicateM` combinator we need the type annotation
on `i` to get it specialized:

```
    {-# INLINE_LATE step #-}
    step _ (i :: Int) =
        if i <= 0
        then return Stop
        else do
                x <- action
                return $ Yield x (i - 1)
```
