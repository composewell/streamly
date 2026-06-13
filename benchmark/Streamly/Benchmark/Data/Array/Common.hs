-------------------------------------------------------------------------------
-- Benchmark helpers
-------------------------------------------------------------------------------

{-# INLINE withRandomIntIO #-}
withRandomIntIO :: (Int -> IO b) -> IO b
withRandomIntIO f = randomRIO (1, 1 :: Int) >>= f

{-# INLINE benchIO #-}
benchIO :: NFData b => String -> IO b -> Benchmark
benchIO name = bench name . nfIO

{-# INLINE withArray #-}
withArray :: Int -> (Arr Int -> IO b) -> IO b
withArray value f = sourceIntFromTo value >>= f

{-# INLINE withStream #-}
withStream :: Int -> (S.Stream IO Int -> IO b) -> IO b
withStream value f = withRandomIntIO $ \n -> f $ P.sourceUnfoldrM value n

-------------------------------------------------------------------------------
-- Bench Ops
-------------------------------------------------------------------------------

{-# INLINE sourceIntFromTo #-}
sourceIntFromTo :: Int -> IO (Arr Int)
sourceIntFromTo value = withRandomIntIO $ \n ->
    S.fold (A.createOf value) $ S.enumerateFromTo n (n + value)

{-# INLINE sourceUnfoldr #-}
sourceUnfoldr :: Int -> IO (Arr Int)
sourceUnfoldr value = withRandomIntIO $ \n ->
    let step cnt =
            if cnt > n + value
            then Nothing
            else Just (cnt, cnt + 1)
    in S.fold (A.createOf value) $ S.unfoldr step n

{-# INLINE sourceFromList #-}
sourceFromList :: Int -> IO (Arr Int)
sourceFromList value = withRandomIntIO $ \n ->
    S.fold (A.createOf value) $ S.fromList [n..n+value]


{-# INLINE showStream #-}
showStream :: Int -> IO P.String
showStream value = withArray value (return . showInstance)

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

{-# INLINE idArr #-}
idArr :: Int -> IO (Arr Int)
idArr value = withArray value return

{-# INLINE eqInstance #-}
eqInstance :: Int -> IO Bool
eqInstance value = withArray value $ \src -> return (src == src)

{-# INLINE eqInstanceNotEq #-}
eqInstanceNotEq :: Int -> IO Bool
eqInstanceNotEq value = withArray value $ \src -> return (src P./= src)

{-# INLINE ordInstance #-}
ordInstance :: Int -> IO Bool
ordInstance value = withArray value $ \src -> return (src P.< src)

{-# INLINE ordInstanceMin #-}
ordInstanceMin :: Int -> IO (Arr Int)
ordInstanceMin value = withArray value $ \src -> return (P.min src src)

{-# INLINE showInstance #-}
showInstance :: Arr Int -> P.String
showInstance = P.show

{-# INLINE pureFoldl' #-}
pureFoldl' :: Int -> IO Int
pureFoldl' value = withArray value $ S.fold (Fold.foldl' (+) 0) . S.unfold A.reader

-------------------------------------------------------------------------------
-- Elimination
-------------------------------------------------------------------------------

{-# INLINE unfoldReadDrain #-}
unfoldReadDrain :: Int -> IO ()
unfoldReadDrain value = withArray value $ S.fold Fold.drain . S.unfold A.reader

{-# INLINE toStreamRevDrain #-}
toStreamRevDrain :: Int -> IO ()
toStreamRevDrain value = withArray value $ S.fold Fold.drain . A.readRev

{-# INLINE writeN #-}
writeN :: Int -> IO (Arr Int)
writeN value = withStream value (S.fold (A.createOf value))

-------------------------------------------------------------------------------
-- Bench groups
-------------------------------------------------------------------------------

common_o_1_space_generation :: Int -> [Benchmark]
common_o_1_space_generation value =
    [ benchIO "writeN . intFromTo" $ sourceIntFromTo value
    , benchIO "fromList . intFromTo" $ sourceIntFromToFromList value
    , benchIO "writeN . unfoldr" $ sourceUnfoldr value
    , benchIO "writeN . fromList" $ sourceFromList value
    , benchIO "show" $ showStream value
    ]

common_o_1_space_elimination :: Int -> [Benchmark]
common_o_1_space_elimination value =
    [ benchIO "id" $ idArr value
    , benchIO "==" $ eqInstance value
    , benchIO "/=" $ eqInstanceNotEq value
    , benchIO "<" $ ordInstance value
    , benchIO "min" $ ordInstanceMin value
    , benchIO "foldl'" $ pureFoldl' value
    , benchIO "unfoldRead" $ unfoldReadDrain value
    , benchIO "toStreamRev" $ toStreamRevDrain value
    ]

common_o_n_heap_serial :: Int -> [Benchmark]
common_o_n_heap_serial value =
    [ benchIO "writeN" $ writeN value
    ]

common_o_1_space_transformation :: Int -> [Benchmark]
common_o_1_space_transformation value =
    [ benchIO "scanl'" $ scanl' value
    , benchIO "scanl1'" $ scanl1' value
    , benchIO "map" $ map value
    ]

common_o_1_space_transformationX4 :: Int -> [Benchmark]
common_o_1_space_transformationX4 value =
    [ benchIO "scanl'X4" $ scanl'X4 value
    , benchIO "scanl1'X4" $ scanl1'X4 value
    , benchIO "mapX4" $ mapX4 value
    ]

commonBenchmarks :: Int -> [(SpaceComplexity, Benchmark)]
commonBenchmarks size =
       fmap (SpaceO_1,) (concat
            [ common_o_1_space_generation size
            , common_o_1_space_elimination size
            , common_o_1_space_transformation size
            , common_o_1_space_transformationX4 size
            ])
    ++ fmap (HeapO_n,) (common_o_n_heap_serial size)
