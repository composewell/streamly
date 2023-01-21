-------------------------------------------------------------------------------
-- Benchmark helpers
-------------------------------------------------------------------------------

{-# INLINE benchIO #-}
benchIO :: NFData b => String -> (Int -> IO a) -> (a -> b) -> Benchmark
benchIO name src f = bench name $ nfIO $
    (randomRIO (1,1) >>= src) <&> f

{-# INLINE benchPureSink #-}
benchPureSink :: NFData b => Int -> String -> (Stream Int -> b) -> Benchmark
benchPureSink value name = benchIO name (sourceIntFromTo value)

{-# INLINE benchIO' #-}
benchIO' :: NFData b => String -> (Int -> IO a) -> (a -> IO b) -> Benchmark
benchIO' name src f = bench name $ nfIO $
    randomRIO (1,1) >>= src >>= f

{-# INLINE benchIOSink #-}
benchIOSink :: NFData b => Int -> String -> (Stream Int -> IO b) -> Benchmark
benchIOSink value name = benchIO' name (sourceIntFromTo value)

-------------------------------------------------------------------------------
-- Bench Ops
-------------------------------------------------------------------------------

{-# INLINE sourceUnfoldr #-}
sourceUnfoldr :: MonadIO m => Int -> Int -> m (Stream Int)
sourceUnfoldr value n = S.fold (A.writeN value) $ S.unfoldr step n
    where
    step cnt =
        if cnt > n + value
        then Nothing
        else Just (cnt, cnt + 1)

{-# INLINE sourceIntFromTo #-}
sourceIntFromTo :: MonadIO m => Int -> Int -> m (Stream Int)
sourceIntFromTo value n = S.fold (A.writeN value) $ S.enumerateFromTo n (n + value)

{-# INLINE sourceFromList #-}
sourceFromList :: MonadIO m => Int -> Int -> m (Stream Int)
sourceFromList value n = S.fold (A.writeN value) $ S.fromList [n..n+value]

-------------------------------------------------------------------------------
-- Transformation
-------------------------------------------------------------------------------

{-# INLINE composeN #-}
composeN :: P.Monad m
    => Int -> (Stream Int -> m (Stream Int)) -> Stream Int -> m (Stream Int)
composeN n f x =
    case n of
        1 -> f x
        2 -> f x P.>>= f
        3 -> f x P.>>= f P.>>= f
        4 -> f x P.>>= f P.>>= f P.>>= f
        _ -> undefined

{-# INLINE scanl' #-}
{-# INLINE scanl1' #-}
{-# INLINE map #-}

scanl' , scanl1', map
    :: MonadIO m => Int -> Int -> Stream Int -> m (Stream Int)


{-# INLINE onArray #-}
onArray
    :: MonadIO m => Int -> (Stream.Stream m Int -> Stream.Stream m Int)
    -> Stream Int
    -> m (Stream Int)
onArray value f arr = S.fold (A.writeN value) $ f $ S.unfold A.reader arr

scanl'  value n = composeN n $ onArray value $ S.scan (Fold.foldl' (+) 0)
scanl1' value n = composeN n $ onArray value $ Stream.scanl1' (+)
map     value n = composeN n $ onArray value $ fmap (+1)
-- map           n = composeN n $ A.map (+1)

{-# INLINE eqInstance #-}
eqInstance :: Stream Int -> Bool
eqInstance src = src == src

{-# INLINE eqInstanceNotEq #-}
eqInstanceNotEq :: Stream Int -> Bool
eqInstanceNotEq src = src P./= src

{-# INLINE ordInstance #-}
ordInstance :: Stream Int -> Bool
ordInstance src = src P.< src

{-# INLINE ordInstanceMin #-}
ordInstanceMin :: Stream Int -> Stream Int
ordInstanceMin src = P.min src src

{-# INLINE showInstance #-}
showInstance :: Stream Int -> P.String
showInstance = P.show

{-# INLINE pureFoldl' #-}
pureFoldl' :: MonadIO m => Stream Int -> m Int
pureFoldl' = S.fold (Fold.foldl' (+) 0) . S.unfold A.reader

-------------------------------------------------------------------------------
-- Elimination
-------------------------------------------------------------------------------

{-# INLINE unfoldReadDrain #-}
unfoldReadDrain :: MonadIO m => Stream Int -> m ()
unfoldReadDrain = S.fold Fold.drain . S.unfold A.reader

{-# INLINE toStreamRevDrain #-}
toStreamRevDrain :: MonadIO m => Stream Int -> m ()
toStreamRevDrain = S.fold Fold.drain . A.readRev

-------------------------------------------------------------------------------
-- Bench groups
-------------------------------------------------------------------------------

common_o_1_space_generation :: Int -> [Benchmark]
common_o_1_space_generation value =
    [ bgroup
        "generation"
        [ benchIOSrc "writeN . intFromTo" (sourceIntFromTo value)
        , benchIOSrc
              "fromList . intFromTo"
              (sourceIntFromToFromList value)
        , benchIOSrc "writeN . unfoldr" (sourceUnfoldr value)
        , benchIOSrc "writeN . fromList" (sourceFromList value)
        , benchPureSink value "show" showInstance
        ]
    ]

common_o_1_space_elimination :: Int -> [Benchmark]
common_o_1_space_elimination value =
    [ bgroup "elimination"
        [ benchPureSink value "id" id
        , benchPureSink value "==" eqInstance
        , benchPureSink value "/=" eqInstanceNotEq
        , benchPureSink value "<" ordInstance
        , benchPureSink value "min" ordInstanceMin
        , benchIOSink value "foldl'" pureFoldl'
        , benchIOSink value "read" unfoldReadDrain
        , benchIOSink value "toStreamRev" toStreamRevDrain
        ]
      ]

common_o_n_heap_serial :: Int -> [Benchmark]
common_o_n_heap_serial value =
    [ bgroup "elimination"
        [
        -- Converting the stream to an array
            benchFold "writeN" (S.fold (A.writeN value))
                (P.sourceUnfoldrM value)
         ]
    ]

common_o_1_space_transformation :: Int -> [Benchmark]
common_o_1_space_transformation value =
   [ bgroup "transformation"
        [ benchIOSink value "scanl'" (scanl' value 1)
        , benchIOSink value "scanl1'" (scanl1' value 1)
        , benchIOSink value "map" (map value 1)
        ]
   ]

common_o_1_space_transformationX4 :: Int -> [Benchmark]
common_o_1_space_transformationX4 value =
    [ bgroup "transformationX4"
        [ benchIOSink value "scanl'" (scanl' value 4)
        , benchIOSink value "scanl1'" (scanl1' value 4)
        , benchIOSink value "map" (map value 4)
        ]
      ]

commonBenchmarks :: Int -> [Benchmark]
commonBenchmarks size =
        [ bgroup (o_1_space_prefix moduleName) $ concat
            [ common_o_1_space_generation size
            , common_o_1_space_elimination size
            , common_o_1_space_transformation size
            , common_o_1_space_transformationX4 size
            ]
        , bgroup (o_n_space_prefix moduleName) $
             common_o_n_heap_serial size
        ]
