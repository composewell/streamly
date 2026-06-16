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

{-# INLINE scanl' #-}
scanl' :: Int -> IO (Arr Int)
scanl' value = withArray value $ composeN 1 $ onArray value $ S.scanl (Scanl.scanl' (+) 0)

{-# INLINE scanl'X4 #-}
scanl'X4 :: Int -> IO (Arr Int)
scanl'X4 value = withArray value $ composeN 4 $ onArray value $ S.scanl (Scanl.scanl' (+) 0)

{-# INLINE scanl1' #-}
scanl1' :: Int -> IO (Arr Int)
scanl1' value = withArray value $ composeN 1 $ onArray value $ Stream.scanl1' (+)

{-# INLINE scanl1'X4 #-}
scanl1'X4 :: Int -> IO (Arr Int)
scanl1'X4 value = withArray value $ composeN 4 $ onArray value $ Stream.scanl1' (+)

{-# INLINE map #-}
map :: Int -> IO (Arr Int)
map value = withArray value $ composeN 1 $ onArray value $ fmap (+1)

{-# INLINE mapX4 #-}
mapX4 :: Int -> IO (Arr Int)
mapX4 value = withArray value $ composeN 4 $ onArray value $ fmap (+1)

-------------------------------------------------------------------------------
-- Bench groups
-------------------------------------------------------------------------------

commonBenchmarks :: Int -> [(SpaceComplexity, Benchmark)]
commonBenchmarks size =
      [ (SpaceO_1, benchIO "scanl'" $ scanl' size)
      , (SpaceO_1, benchIO "scanl1'" $ scanl1' size)
      , (SpaceO_1, benchIO "map" $ map size)

      , (SpaceO_1, benchIO "scanl'X4" $ scanl'X4 size)
      , (SpaceO_1, benchIO "scanl1'X4" $ scanl1'X4 size)
      , (SpaceO_1, benchIO "mapX4" $ mapX4 size)
      ]
