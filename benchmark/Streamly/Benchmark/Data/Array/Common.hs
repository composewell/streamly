-------------------------------------------------------------------------------
-- Transformation
-------------------------------------------------------------------------------

{-# INLINE composeN #-}
composeN :: P.Monad m
    => Int -> (Arr Int -> m (Arr Int)) -> Arr Int -> m (Arr Int)
composeN n f x =
    case n of
        1 -> f x
        2 -> f x P.>>= f
        3 -> f x P.>>= f P.>>= f
        4 -> f x P.>>= f P.>>= f P.>>= f
        _ -> undefined

{-# INLINE onArray #-}
onArray
    :: MonadIO m => Int -> (Stream.Stream m Int -> Stream.Stream m Int)
    -> Arr Int
    -> m (Arr Int)
onArray value f arr = S.fold (A.createOf value) $ f $ S.unfold A.reader arr

#if defined(ARRAY_UNBOXED)
{-# ANN scanl'_x1 (PermitPatternMatches
    [ ''FL.Tuple'Fused, ''MutArray.ArrayUnsafe, ''IO, ''Int
    , ''Stream.EnumToState, ''A.Array, ''Either, ''FL.Fold, ''FL.Step
    , ''(,)
    ]) #-}
{-# ANN scanl'_x1 (PermitConstructions
    [ ''Int, ''SrcLoc, ''CallStack, ''A.Array, ''Stream.Step
    , ''Stream.EnumToState, ''FL.Step, ''FL.Tuple'Fused
    , ''MutArray.ArrayUnsafe, ''Either, ''(,), ''Stream.Stream
    , ''FL.Fold
    ]) #-}
{-# ANN scanl'_x1 (PermitTypeClasses [''IP]) #-}
#elif defined(ARRAY_GENERIC)
{-# ANN scanl'_x1 (PermitPatternMatches
    [ ''FL.Tuple'Fused, ''MutArray.MutArray, ''Int, ''Stream.EnumToState
    , ''A.Array, ''Either, ''FL.Fold, ''FL.Step, ''IO, ''(,)
    ]) #-}
{-# ANN scanl'_x1 (PermitConstructions
    [ ''Int, ''SrcLoc, ''CallStack, ''A.Array, ''[], ''Char
    , ''Stream.Step, ''Stream.EnumToState, ''FL.Step, ''FL.Tuple'Fused
    , ''MutArray.MutArray, ''Either, ''(,), ''Stream.Stream, ''FL.Fold
    ]) #-}
{-# ANN scanl'_x1 (PermitTypeClasses [''IP]) #-}
#else
{-# ANN scanl'_x1 (PermitPatternMatches []) #-}
{-# ANN scanl'_x1 (PermitConstructions []) #-}
{-# ANN scanl'_x1 (PermitTypeClasses []) #-}
#endif
{-# NOINLINE scanl'_x1 #-}
scanl'_x1 :: Int -> Int -> IO (Arr Int)
scanl'_x1 value =
    withArray value
        $ composeN 1 $ onArray value $ S.scanl (Scanl.scanl' (+) 0)

#if defined(ARRAY_UNBOXED)
{-# ANN scanl'_x4 (PermitPatternMatches
    [ ''FL.Tuple'Fused, ''MutArray.ArrayUnsafe, ''IO, ''Int
    , ''Stream.EnumToState, ''A.Array, ''Either, ''FL.Fold, ''FL.Step
    , ''(,)
    ]) #-}
{-# ANN scanl'_x4 (PermitConstructions
    [ ''Int, ''SrcLoc, ''CallStack, ''A.Array, ''Stream.Step
    , ''Stream.EnumToState, ''FL.Step, ''FL.Tuple'Fused
    , ''MutArray.ArrayUnsafe, ''Either, ''(,), ''Stream.Stream
    , ''FL.Fold
    ]) #-}
{-# ANN scanl'_x4 (PermitTypeClasses [''IP]) #-}
#elif defined(ARRAY_GENERIC)
{-# ANN scanl'_x4 (PermitPatternMatches
    [ ''FL.Tuple'Fused, ''MutArray.MutArray, ''Int, ''Stream.EnumToState
    , ''A.Array, ''Either, ''FL.Fold, ''FL.Step, ''IO, ''(,)
    ]) #-}
{-# ANN scanl'_x4 (PermitConstructions
    [ ''Int, ''SrcLoc, ''CallStack, ''A.Array, ''[], ''Char
    , ''Stream.Step, ''Stream.EnumToState, ''FL.Step, ''FL.Tuple'Fused
    , ''MutArray.MutArray, ''Either, ''(,), ''Stream.Stream, ''FL.Fold
    ]) #-}
{-# ANN scanl'_x4 (PermitTypeClasses [''IP]) #-}
#else
{-# ANN scanl'_x4 (PermitPatternMatches []) #-}
{-# ANN scanl'_x4 (PermitConstructions []) #-}
{-# ANN scanl'_x4 (PermitTypeClasses []) #-}
#endif
{-# NOINLINE scanl'_x4 #-}
scanl'_x4 :: Int -> Int -> IO (Arr Int)
scanl'_x4 value =
    withArray value
        $ composeN 4 $ onArray value $ S.scanl (Scanl.scanl' (+) 0)

#if defined(ARRAY_UNBOXED)
{-# ANN scanl1'_x1 (PermitPatternMatches
    [ ''FL.Tuple'Fused, ''MutArray.ArrayUnsafe, ''IO, ''Int
    , ''Stream.EnumToState, ''A.Array, ''SPEC, ''Maybe
    , ''Stream.UnfoldState, ''Either, ''FL.Fold, ''FL.Step, ''(,)
    ]) #-}
{-# ANN scanl1'_x1 (PermitConstructions
    [ ''Int, ''SrcLoc, ''CallStack, ''A.Array, ''Stream.Step
    , ''Stream.EnumToState, ''FL.Step, ''FL.Tuple'Fused
    , ''MutArray.ArrayUnsafe, ''Stream.UnfoldState, ''SPEC, ''Maybe
    , ''Either, ''(,), ''Stream.Stream, ''FL.Fold
    ]) #-}
{-# ANN scanl1'_x1 (PermitTypeClasses [''IP]) #-}
#elif defined(ARRAY_GENERIC)
{-# ANN scanl1'_x1 (PermitPatternMatches
    [ ''FL.Tuple'Fused, ''MutArray.MutArray, ''Int, ''Stream.EnumToState
    , ''A.Array, ''SPEC, ''Maybe, ''Stream.UnfoldState, ''Either
    , ''FL.Fold, ''FL.Step, ''IO, ''(,)
    ]) #-}
{-# ANN scanl1'_x1 (PermitConstructions
    [ ''Int, ''SrcLoc, ''CallStack, ''A.Array, ''[], ''Char
    , ''Stream.Step, ''Stream.EnumToState, ''FL.Step, ''FL.Tuple'Fused
    , ''MutArray.MutArray, ''Stream.UnfoldState, ''SPEC, ''Maybe
    , ''Either, ''(,), ''Stream.Stream, ''FL.Fold
    ]) #-}
{-# ANN scanl1'_x1 (PermitTypeClasses [''IP]) #-}
#else
{-# ANN scanl1'_x1 (PermitPatternMatches []) #-}
{-# ANN scanl1'_x1 (PermitConstructions []) #-}
{-# ANN scanl1'_x1 (PermitTypeClasses []) #-}
#endif
{-# NOINLINE scanl1'_x1 #-}
scanl1'_x1 :: Int -> Int -> IO (Arr Int)
scanl1'_x1 value =
    withArray value $ composeN 1 $ onArray value $ Stream.scanl1' (+)

#if defined(ARRAY_UNBOXED)
{-# ANN scanl1'_x4 (PermitPatternMatches
    [ ''FL.Tuple'Fused, ''MutArray.ArrayUnsafe, ''IO, ''Int
    , ''Stream.EnumToState, ''A.Array, ''SPEC, ''Maybe
    , ''Stream.UnfoldState, ''Either, ''FL.Fold, ''FL.Step, ''(,)
    ]) #-}
{-# ANN scanl1'_x4 (PermitConstructions
    [ ''Int, ''SrcLoc, ''CallStack, ''A.Array, ''Stream.Step
    , ''Stream.EnumToState, ''FL.Step, ''FL.Tuple'Fused
    , ''MutArray.ArrayUnsafe, ''Stream.UnfoldState, ''SPEC, ''Maybe
    , ''Either, ''(,), ''Stream.Stream, ''FL.Fold
    ]) #-}
{-# ANN scanl1'_x4 (PermitTypeClasses [''IP]) #-}
#elif defined(ARRAY_GENERIC)
{-# ANN scanl1'_x4 (PermitPatternMatches
    [ ''FL.Tuple'Fused, ''MutArray.MutArray, ''Int, ''Stream.EnumToState
    , ''A.Array, ''SPEC, ''Maybe, ''Stream.UnfoldState, ''Either
    , ''FL.Fold, ''FL.Step, ''IO, ''(,)
    ]) #-}
{-# ANN scanl1'_x4 (PermitConstructions
    [ ''Int, ''SrcLoc, ''CallStack, ''A.Array, ''[], ''Char
    , ''Stream.Step, ''Stream.EnumToState, ''FL.Step, ''FL.Tuple'Fused
    , ''MutArray.MutArray, ''Stream.UnfoldState, ''SPEC, ''Maybe
    , ''Either, ''(,), ''Stream.Stream, ''FL.Fold
    ]) #-}
{-# ANN scanl1'_x4 (PermitTypeClasses [''IP]) #-}
#else
{-# ANN scanl1'_x4 (PermitPatternMatches []) #-}
{-# ANN scanl1'_x4 (PermitConstructions []) #-}
{-# ANN scanl1'_x4 (PermitTypeClasses []) #-}
#endif
{-# NOINLINE scanl1'_x4 #-}
scanl1'_x4 :: Int -> Int -> IO (Arr Int)
scanl1'_x4 value =
    withArray value $ composeN 4 $ onArray value $ Stream.scanl1' (+)

#if defined(ARRAY_UNBOXED)
{-# ANN fmap_x1 (PermitPatternMatches
    [ ''FL.Tuple'Fused, ''MutArray.ArrayUnsafe, ''IO, ''Int
    , ''Stream.EnumToState, ''A.Array, ''SPEC, ''Stream.UnfoldState
    , ''Either, ''FL.Fold, ''FL.Step, ''(,)
    ]) #-}
{-# ANN fmap_x1 (PermitConstructions
    [ ''Int, ''SrcLoc, ''CallStack, ''A.Array, ''Stream.Step
    , ''Stream.EnumToState, ''FL.Step, ''FL.Tuple'Fused
    , ''MutArray.ArrayUnsafe, ''Stream.UnfoldState, ''SPEC, ''Either
    , ''(,), ''Stream.Stream, ''FL.Fold
    ]) #-}
{-# ANN fmap_x1 (PermitTypeClasses [''IP]) #-}
#elif defined(ARRAY_GENERIC)
{-# ANN fmap_x1 (PermitPatternMatches
    [ ''FL.Tuple'Fused, ''MutArray.MutArray, ''Int, ''Stream.EnumToState
    , ''A.Array, ''SPEC, ''Stream.UnfoldState, ''Either, ''FL.Fold
    , ''FL.Step, ''IO, ''(,)
    ]) #-}
{-# ANN fmap_x1 (PermitConstructions
    [ ''Int, ''SrcLoc, ''CallStack, ''A.Array, ''[], ''Char
    , ''Stream.Step, ''Stream.EnumToState, ''FL.Step, ''FL.Tuple'Fused
    , ''MutArray.MutArray, ''Stream.UnfoldState, ''SPEC, ''Either, ''(,)
    , ''Stream.Stream, ''FL.Fold
    ]) #-}
{-# ANN fmap_x1 (PermitTypeClasses [''IP]) #-}
#else
{-# ANN fmap_x1 (PermitPatternMatches []) #-}
{-# ANN fmap_x1 (PermitConstructions []) #-}
{-# ANN fmap_x1 (PermitTypeClasses []) #-}
#endif
{-# NOINLINE fmap_x1 #-}
fmap_x1 :: Int -> Int -> IO (Arr Int)
fmap_x1 value = withArray value $ composeN 1 $ onArray value $ fmap (+1)

#if defined(ARRAY_UNBOXED)
{-# ANN fmap_x4 (PermitPatternMatches
    [ ''FL.Tuple'Fused, ''MutArray.ArrayUnsafe, ''IO, ''Int
    , ''Stream.EnumToState, ''A.Array, ''SPEC, ''Stream.UnfoldState
    , ''Either, ''FL.Fold, ''FL.Step, ''(,)
    ]) #-}
{-# ANN fmap_x4 (PermitConstructions
    [ ''Int, ''SrcLoc, ''CallStack, ''A.Array, ''Stream.Step
    , ''Stream.EnumToState, ''FL.Step, ''FL.Tuple'Fused
    , ''MutArray.ArrayUnsafe, ''Stream.UnfoldState, ''SPEC, ''Either
    , ''(,), ''Stream.Stream, ''FL.Fold
    ]) #-}
{-# ANN fmap_x4 (PermitTypeClasses [''IP]) #-}
#elif defined(ARRAY_GENERIC)
{-# ANN fmap_x4 (PermitPatternMatches
    [ ''FL.Tuple'Fused, ''MutArray.MutArray, ''Int, ''Stream.EnumToState
    , ''A.Array, ''SPEC, ''Stream.UnfoldState, ''Either, ''FL.Fold
    , ''FL.Step, ''IO, ''(,)
    ]) #-}
{-# ANN fmap_x4 (PermitConstructions
    [ ''Int, ''SrcLoc, ''CallStack, ''A.Array, ''[], ''Char
    , ''Stream.Step, ''Stream.EnumToState, ''FL.Step, ''FL.Tuple'Fused
    , ''MutArray.MutArray, ''Stream.UnfoldState, ''SPEC, ''Either, ''(,)
    , ''Stream.Stream, ''FL.Fold
    ]) #-}
{-# ANN fmap_x4 (PermitTypeClasses [''IP]) #-}
#else
{-# ANN fmap_x4 (PermitPatternMatches []) #-}
{-# ANN fmap_x4 (PermitConstructions []) #-}
{-# ANN fmap_x4 (PermitTypeClasses []) #-}
#endif
{-# NOINLINE fmap_x4 #-}
fmap_x4 :: Int -> Int -> IO (Arr Int)
fmap_x4 value = withArray value $ composeN 4 $ onArray value $ fmap (+1)

#if defined(ARRAY_UNBOXED)
{-# ANN createOfLast_1 (PermitPatternMatches [''RingArray.RingArray, ''IO]) #-}
{-# ANN createOfLast_1 (PermitConstructions
    [ ''A.Array, ''RingArray.RingArray
    ]) #-}
{-# ANN createOfLast_1 (PermitTypeClasses []) #-}
#elif defined(ARRAY_GENERIC)
{-# ANN createOfLast_1 (PermitPatternMatches [''MutArray.MutArray]) #-}
{-# ANN createOfLast_1 (PermitConstructions
    [ ''[], ''Char, ''Int, ''SrcLoc, ''CallStack, ''A.Array
    , ''MutArray.MutArray
    ]) #-}
{-# ANN createOfLast_1 (PermitTypeClasses [''IP]) #-}
#else
{-# ANN createOfLast_1 (PermitPatternMatches []) #-}
{-# ANN createOfLast_1 (PermitConstructions []) #-}
{-# ANN createOfLast_1 (PermitTypeClasses []) #-}
#endif
{-# NOINLINE createOfLast_1 #-}
createOfLast_1 :: Int -> Int -> IO (Arr Int)
createOfLast_1 value = withStream value (S.fold (A.createOfLast 1))

#if defined(ARRAY_UNBOXED)
{-# ANN createOfLast_10 (PermitPatternMatches [''RingArray.RingArray, ''IO]) #-}
{-# ANN createOfLast_10 (PermitConstructions
    [ ''A.Array, ''RingArray.RingArray
    ]) #-}
{-# ANN createOfLast_10 (PermitTypeClasses []) #-}
#elif defined(ARRAY_GENERIC)
{-# ANN createOfLast_10 (PermitPatternMatches [''MutArray.MutArray]) #-}
{-# ANN createOfLast_10 (PermitConstructions
    [ ''[], ''Char, ''Int, ''SrcLoc, ''CallStack, ''A.Array
    , ''MutArray.MutArray
    ]) #-}
{-# ANN createOfLast_10 (PermitTypeClasses [''IP]) #-}
#else
{-# ANN createOfLast_10 (PermitPatternMatches []) #-}
{-# ANN createOfLast_10 (PermitConstructions []) #-}
{-# ANN createOfLast_10 (PermitTypeClasses []) #-}
#endif
{-# NOINLINE createOfLast_10 #-}
createOfLast_10 :: Int -> Int -> IO (Arr Int)
createOfLast_10 value = withStream value (S.fold (A.createOfLast 10))

#if defined(ARRAY_UNBOXED)
{-# ANN createOfLast_Max (PermitPatternMatches
    [ ''RingArray.RingArray, ''IO, ''A.Array
    ]) #-}
{-# ANN createOfLast_Max (PermitConstructions
    [ ''A.Array, ''RingArray.RingArray
    ]) #-}
{-# ANN createOfLast_Max (PermitTypeClasses []) #-}
#elif defined(ARRAY_GENERIC)
{-# ANN createOfLast_Max (PermitPatternMatches
    [ ''MutArray.MutArray
    ]) #-}
{-# ANN createOfLast_Max (PermitConstructions
    [ ''[], ''Char, ''Int, ''SrcLoc, ''CallStack, ''A.Array
    , ''MutArray.MutArray
    ]) #-}
{-# ANN createOfLast_Max (PermitTypeClasses [''IP]) #-}
#else
{-# ANN createOfLast_Max (PermitPatternMatches []) #-}
{-# ANN createOfLast_Max (PermitConstructions []) #-}
{-# ANN createOfLast_Max (PermitTypeClasses []) #-}
#endif
{-# NOINLINE createOfLast_Max #-}
createOfLast_Max :: Int -> Int -> IO (Arr Int)
createOfLast_Max value = withStream value (S.fold (A.createOfLast (value + 1)))

-------------------------------------------------------------------------------
-- Bench groups
-------------------------------------------------------------------------------

commonBenchmarks :: Int -> [(SpaceComplexity, Benchmark)]
commonBenchmarks size =
      [ (SpaceO_1, benchIO "scanl'_x1" $ scanl'_x1 size)
      , (SpaceO_1, benchIO "scanl1'_x1" $ scanl1'_x1 size)
      , (SpaceO_1, benchIO "fmap_x1" $ fmap_x1 size)

      , (SpaceO_1, benchIO "scanl'_x4" $ scanl'_x4 size)
      , (SpaceO_1, benchIO "scanl1'_x4" $ scanl1'_x4 size)
      , (SpaceO_1, benchIO "fmap_x4" $ fmap_x4 size)

      , (SpaceO_1, benchIO "createOfLast_1" $ createOfLast_1 size)
      , (SpaceO_1, benchIO "createOfLast_10" $ createOfLast_10 size)

      , (HeapO_n, benchIO "createOfLast_Max" $ createOfLast_Max size)
      ]
