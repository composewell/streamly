-------------------------------------------------------------------------------
-- Benchmark helpers
-------------------------------------------------------------------------------

{-# INLINE benchIO #-}
benchIO :: NFData b => String -> (Int -> IO b) -> Benchmark
benchIO name f = bench name $ nfIO $ randomRIO (1, 1 :: Int) >>= f

{-# INLINE withArray #-}
withArray :: Int -> (Arr Int -> IO b) -> Int -> IO b
withArray value f n = sourceIntFromTo value n >>= f

{-# INLINE withStream #-}
withStream :: Int -> (S.Stream IO Int -> IO b) -> Int -> IO b
withStream value f = f . P.sourceUnfoldrM value

-------------------------------------------------------------------------------
-- Bench Ops
-------------------------------------------------------------------------------

{-# INLINE sourceIntFromTo #-}
sourceIntFromTo :: Int -> Int -> IO (Arr Int)
sourceIntFromTo value n =
    S.fold (A.createOf value) $ S.enumerateFromTo n (n + value)

{-# INLINE sourceUnfoldr #-}
sourceUnfoldr :: Int -> Int -> IO (Arr Int)
sourceUnfoldr value n =
    let step cnt =
            if cnt > n + value
            then Nothing
            else Just (cnt, cnt + 1)
    in S.fold (A.createOf value) $ S.unfoldr step n

{-# INLINE sourceFromList #-}
sourceFromList :: Int -> Int -> IO (Arr Int)
sourceFromList value n =
    S.fold (A.createOf value) $ S.fromList [n..n+value]

{-# INLINE sourceIntFromToFromStream #-}
sourceIntFromToFromStream :: Int -> Int -> IO (Arr Int)
sourceIntFromToFromStream value n =
    S.fold A.create $ S.enumerateFromTo n (n + value)

{-# INLINE parseInstance #-}
parseInstance :: P.String -> Arr Int
parseInstance str =
    let r = P.reads str
    in case r of
        [(x,"")] -> x
        _ -> P.error "parseInstance: no parse"

{-# INLINE readInstance #-}
readInstance :: Int -> Int -> IO (Arr Int)
readInstance value n =
    let testStr = "fromList " ++ show [n..n+value]
    in return $! parseInstance testStr


{-# INLINE showStream #-}
showStream :: Int -> Int -> IO P.String
showStream value = withArray value (return . showInstance)

{-# INLINE idArr #-}
idArr :: Int -> Int -> IO (Arr Int)
idArr value = withArray value return

{-# INLINE eqInstance #-}
eqInstance :: Int -> Int -> IO Bool
eqInstance value = withArray value $ \src -> return (src == src)

{-# INLINE eqInstanceNotEq #-}
eqInstanceNotEq :: Int -> Int -> IO Bool
eqInstanceNotEq value = withArray value $ \src -> return (src P./= src)

{-# INLINE ordInstance #-}
ordInstance :: Int -> Int -> IO Bool
ordInstance value = withArray value $ \src -> return (src P.< src)

{-# INLINE ordInstanceMin #-}
ordInstanceMin :: Int -> Int -> IO (Arr Int)
ordInstanceMin value = withArray value $ \src -> return (P.min src src)

{-# INLINE showInstance #-}
showInstance :: Arr Int -> P.String
showInstance = P.show

{-# INLINE pureFoldl' #-}
pureFoldl' :: Int -> Int -> IO Int
pureFoldl' value = withArray value $ S.fold (Fold.foldl' (+) 0) . S.unfold A.reader

-------------------------------------------------------------------------------
-- Elimination
-------------------------------------------------------------------------------

{-# INLINE unfoldReadDrain #-}
unfoldReadDrain :: Int -> Int -> IO ()
unfoldReadDrain value = withArray value $ S.fold Fold.drain . S.unfold A.reader

{-# INLINE toStreamRevDrain #-}
toStreamRevDrain :: Int -> Int -> IO ()
toStreamRevDrain value = withArray value $ S.fold Fold.drain . A.readRev

{-# INLINE writeN #-}
writeN :: Int -> Int -> IO (Arr Int)
writeN value = withStream value (S.fold (A.createOf value))

-------------------------------------------------------------------------------
-- Bench groups
-------------------------------------------------------------------------------

typeCommonBenchmarks :: Int -> [(SpaceComplexity, Benchmark)]
typeCommonBenchmarks size =
      [ (SpaceO_1, benchIO "write . intFromTo" $ sourceIntFromToFromStream size)
      , (SpaceO_1, benchIO "writeN . intFromTo" $ sourceIntFromTo size)
      , (SpaceO_1, benchIO "fromList . intFromTo" $ sourceIntFromToFromList size)
      , (SpaceO_1, benchIO "writeN . unfoldr" $ sourceUnfoldr size)
      , (SpaceO_1, benchIO "writeN . fromList" $ sourceFromList size)
      , (SpaceO_1, benchIO "show" $ showStream size)
      , (SpaceO_1, benchIO "read" $ readInstance size)

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
