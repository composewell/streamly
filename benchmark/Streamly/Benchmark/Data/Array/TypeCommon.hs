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

typeCommonBenchmarks :: Int -> [(SpaceComplexity, Benchmark)]
typeCommonBenchmarks size =
      [ (SpaceO_1, benchIO "writeN . intFromTo" $ sourceIntFromTo size)
      , (SpaceO_1, benchIO "fromList . intFromTo" $ sourceIntFromToFromList size)
      , (SpaceO_1, benchIO "writeN . unfoldr" $ sourceUnfoldr size)
      , (SpaceO_1, benchIO "writeN . fromList" $ sourceFromList size)
      , (SpaceO_1, benchIO "show" $ showStream size)

      , (SpaceO_1, benchIO "id" $ idArr size)
      , (SpaceO_1, benchIO "==" $ eqInstance size)
      , (SpaceO_1, benchIO "/=" $ eqInstanceNotEq size)
      , (SpaceO_1, benchIO "<" $ ordInstance size)
      , (SpaceO_1, benchIO "min" $ ordInstanceMin size)
      , (SpaceO_1, benchIO "foldl'" $ pureFoldl' size)
      , (SpaceO_1, benchIO "unfoldRead" $ unfoldReadDrain size)
      , (SpaceO_1, benchIO "toStreamRev" $ toStreamRevDrain size)

      , (HeapO_n, benchIO "writeN" $ writeN size)
      ]
