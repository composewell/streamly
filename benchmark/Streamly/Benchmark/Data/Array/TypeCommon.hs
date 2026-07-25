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

#if defined(ARRAY_UNBOXED)
{-# ANN createOf (PermitPatternMatches [''Int, ''IO]) #-}
{-# ANN createOf (PermitConstructions [''A.Array]) #-}
{-# ANN createOf (PermitTypeClasses []) #-}
#elif defined(ARRAY_GENERIC)
{-# ANN createOf (PermitPatternMatches [''Int]) #-}
{-# ANN createOf (PermitConstructions
    [ ''[], ''Char, ''Int, ''SrcLoc, ''CallStack, ''A.Array
    ]) #-}
{-# ANN createOf (PermitTypeClasses [''IP]) #-}
#else
{-# ANN createOf (PermitPatternMatches []) #-}
{-# ANN createOf (PermitConstructions []) #-}
{-# ANN createOf (PermitTypeClasses []) #-}
#endif
{-# NOINLINE createOf #-}
createOf :: Int -> Int -> IO (Arr Int)
createOf = sourceIntFromTo

#if defined(ARRAY_UNBOXED)
{-# ANN createOf_Unfoldr (PermitPatternMatches [''IO]) #-}
{-# ANN createOf_Unfoldr (PermitConstructions [''A.Array]) #-}
{-# ANN createOf_Unfoldr (PermitTypeClasses []) #-}
#elif defined(ARRAY_GENERIC)
{-# ANN createOf_Unfoldr (PermitPatternMatches []) #-}
{-# ANN createOf_Unfoldr (PermitConstructions
    [ ''[], ''Char, ''Int, ''SrcLoc, ''CallStack, ''A.Array
    ]) #-}
{-# ANN createOf_Unfoldr (PermitTypeClasses [''IP]) #-}
#else
{-# ANN createOf_Unfoldr (PermitPatternMatches []) #-}
{-# ANN createOf_Unfoldr (PermitConstructions []) #-}
{-# ANN createOf_Unfoldr (PermitTypeClasses []) #-}
#endif
{-# NOINLINE createOf_Unfoldr #-}
createOf_Unfoldr :: Int -> Int -> IO (Arr Int)
createOf_Unfoldr value n =
    let step cnt =
            if cnt > n + value
            then Nothing
            else Just (cnt, cnt + 1)
    in S.fold (A.createOf value) $ S.unfoldr step n

#if defined(ARRAY_UNBOXED)
{-# ANN createOf_FromList (PermitPatternMatches [''[], ''Int, ''IO]) #-}
{-# ANN createOf_FromList (PermitConstructions [''A.Array, ''[], ''Int]) #-}
{-# ANN createOf_FromList (PermitTypeClasses []) #-}
#elif defined(ARRAY_GENERIC)
{-# ANN createOf_FromList (PermitPatternMatches [''[]]) #-}
{-# ANN createOf_FromList (PermitConstructions
    [ ''[], ''Char, ''Int, ''SrcLoc, ''CallStack, ''A.Array
    ]) #-}
{-# ANN createOf_FromList (PermitTypeClasses [''IP]) #-}
#else
{-# ANN createOf_FromList (PermitPatternMatches []) #-}
{-# ANN createOf_FromList (PermitConstructions []) #-}
{-# ANN createOf_FromList (PermitTypeClasses []) #-}
#endif
{-# NOINLINE createOf_FromList #-}
createOf_FromList :: Int -> Int -> IO (Arr Int)
createOf_FromList value n =
    S.fold (A.createOf value) $ S.fromList [n..n+value]

#if defined(ARRAY_UNBOXED)
{-# ANN create (PermitPatternMatches [''MutArray.MutArray]) #-}
{-# ANN create (PermitConstructions
    [''A.Array, ''MutArray.MutArray, ''PinnedState]) #-}
{-# ANN create (PermitTypeClasses []) #-}
#elif defined(ARRAY_GENERIC)
{-# ANN create (PermitPatternMatches [''MutArray.MutArray]) #-}
{-# ANN create (PermitConstructions
    [ ''[], ''Char, ''Int, ''SrcLoc, ''CallStack, ''A.Array
    , ''MutArray.MutArray
    ]) #-}
{-# ANN create (PermitTypeClasses [''IP, ''MonadIO]) #-}
#else
{-# ANN create (PermitPatternMatches []) #-}
{-# ANN create (PermitConstructions []) #-}
{-# ANN create (PermitTypeClasses []) #-}
#endif
{-# NOINLINE create #-}
create :: Int -> Int -> IO (Arr Int)
create value n =
    S.fold A.create $ S.enumerateFromTo n (n + value)

{-# INLINE parseInstance #-}
parseInstance :: P.String -> Arr Int
parseInstance str =
    let r = P.reads str
    in case r of
        [(x,"")] -> x
        _ -> P.error "parseInstance: no parse"

#if defined(ARRAY_UNBOXED)
{-# ANN read_ReadInstance (PermitPatternMatches
    [''[], ''Arr, ''(,), ''A.Array]) #-}
{-# ANN read_ReadInstance (PermitConstructions
    [''Int, ''SrcLoc, ''CallStack, ''[]]) #-}
{-# ANN read_ReadInstance (PermitTypeClasses [''Read, ''Unbox, ''IP]) #-}
#elif defined(ARRAY_GENERIC)
-- No annotations here: the generic Read instance constructs
-- GHC.Internal.Text.ParserCombinators.ReadP.P, which base does not export
-- (Text.ParserCombinators.ReadP exports only ReadP and ReadS), so that
-- forbidden construction cannot be named in a PermitConstructions list. An
-- unannotated binding is not checked by the plugin.
#else
{-# ANN read_ReadInstance (PermitPatternMatches []) #-}
{-# ANN read_ReadInstance (PermitConstructions []) #-}
{-# ANN read_ReadInstance (PermitTypeClasses []) #-}
#endif
{-# NOINLINE read_ReadInstance #-}
read_ReadInstance :: Int -> Int -> IO (Arr Int)
read_ReadInstance value n =
    let testStr = "fromList " ++ show [n..n+value]
    in return $! parseInstance testStr


#if defined(ARRAY_UNBOXED)
{-# ANN show_ShowInstance (PermitPatternMatches [''A.Array, ''IO]) #-}
{-# ANN show_ShowInstance (PermitConstructions [''[], ''Int, ''A.Array]) #-}
{-# ANN show_ShowInstance (PermitTypeClasses []) #-}
#elif defined(ARRAY_GENERIC)
{-# ANN show_ShowInstance (PermitPatternMatches [''A.Array]) #-}
{-# ANN show_ShowInstance (PermitConstructions
    [ ''[], ''Char, ''Int, ''SrcLoc, ''CallStack, ''A.Array
    ]) #-}
{-# ANN show_ShowInstance (PermitTypeClasses [''IP]) #-}
#else
{-# ANN show_ShowInstance (PermitPatternMatches []) #-}
{-# ANN show_ShowInstance (PermitConstructions []) #-}
{-# ANN show_ShowInstance (PermitTypeClasses []) #-}
#endif
{-# NOINLINE show_ShowInstance #-}
show_ShowInstance :: Int -> Int -> IO P.String
show_ShowInstance value = withArray value (return . showInstance)

#if defined(ARRAY_UNBOXED)
{-# ANN eq_EqInstance (PermitPatternMatches [''UnsafeEquality, ''IO]) #-}
{-# ANN eq_EqInstance (PermitConstructions [''Bool]) #-}
{-# ANN eq_EqInstance (PermitTypeClasses []) #-}
#elif defined(ARRAY_GENERIC)
{-# ANN eq_EqInstance (PermitPatternMatches [''Int]) #-}
{-# ANN eq_EqInstance (PermitConstructions
    [ ''[], ''Char, ''Int, ''SrcLoc, ''CallStack, ''Bool
    ]) #-}
{-# ANN eq_EqInstance (PermitTypeClasses [''IP]) #-}
#else
{-# ANN eq_EqInstance (PermitPatternMatches []) #-}
{-# ANN eq_EqInstance (PermitConstructions []) #-}
{-# ANN eq_EqInstance (PermitTypeClasses []) #-}
#endif
{-# NOINLINE eq_EqInstance #-}
eq_EqInstance :: Int -> Int -> IO Bool
eq_EqInstance value = withArray value $ \src -> return (src == src)

#if defined(ARRAY_UNBOXED)
{-# ANN notEq_EqInstance (PermitPatternMatches
    [''UnsafeEquality, ''IO]) #-}
{-# ANN notEq_EqInstance (PermitConstructions [''Bool]) #-}
{-# ANN notEq_EqInstance (PermitTypeClasses []) #-}
#elif defined(ARRAY_GENERIC)
{-# ANN notEq_EqInstance (PermitPatternMatches [''Int]) #-}
{-# ANN notEq_EqInstance (PermitConstructions
    [ ''[], ''Char, ''Int, ''SrcLoc, ''CallStack, ''Bool
    ]) #-}
{-# ANN notEq_EqInstance (PermitTypeClasses [''IP]) #-}
#else
{-# ANN notEq_EqInstance (PermitPatternMatches []) #-}
{-# ANN notEq_EqInstance (PermitConstructions []) #-}
{-# ANN notEq_EqInstance (PermitTypeClasses []) #-}
#endif
{-# NOINLINE notEq_EqInstance #-}
notEq_EqInstance :: Int -> Int -> IO Bool
notEq_EqInstance value = withArray value $ \src -> return (src P./= src)

#if defined(ARRAY_UNBOXED)
{-# ANN lt_OrdInstance (PermitPatternMatches [''A.Array, ''IO]) #-}
{-# ANN lt_OrdInstance (PermitConstructions [''Bool, ''A.Array]) #-}
{-# ANN lt_OrdInstance (PermitTypeClasses []) #-}
#elif defined(ARRAY_GENERIC)
{-# ANN lt_OrdInstance (PermitPatternMatches [''Int]) #-}
{-# ANN lt_OrdInstance (PermitConstructions
    [ ''[], ''Char, ''Int, ''SrcLoc, ''CallStack, ''Bool
    ]) #-}
{-# ANN lt_OrdInstance (PermitTypeClasses [''IP]) #-}
#else
{-# ANN lt_OrdInstance (PermitPatternMatches []) #-}
{-# ANN lt_OrdInstance (PermitConstructions []) #-}
{-# ANN lt_OrdInstance (PermitTypeClasses []) #-}
#endif
{-# NOINLINE lt_OrdInstance #-}
lt_OrdInstance :: Int -> Int -> IO Bool
lt_OrdInstance value = withArray value $ \src -> return (src P.< src)

#if defined(ARRAY_UNBOXED)
{-# ANN min_OrdInstance (PermitPatternMatches [''A.Array, ''IO]) #-}
{-# ANN min_OrdInstance (PermitConstructions [''A.Array]) #-}
{-# ANN min_OrdInstance (PermitTypeClasses []) #-}
#elif defined(ARRAY_GENERIC)
{-# ANN min_OrdInstance (PermitPatternMatches [''Int]) #-}
{-# ANN min_OrdInstance (PermitConstructions
    [ ''[], ''Char, ''Int, ''SrcLoc, ''CallStack, ''A.Array
    ]) #-}
{-# ANN min_OrdInstance (PermitTypeClasses [''IP]) #-}
#else
{-# ANN min_OrdInstance (PermitPatternMatches []) #-}
{-# ANN min_OrdInstance (PermitConstructions []) #-}
{-# ANN min_OrdInstance (PermitTypeClasses []) #-}
#endif
{-# NOINLINE min_OrdInstance #-}
min_OrdInstance :: Int -> Int -> IO (Arr Int)
min_OrdInstance value = withArray value $ \src -> return (P.min src src)

{-# INLINE showInstance #-}
showInstance :: Arr Int -> P.String
showInstance = P.show

#if defined(ARRAY_UNBOXED)
{-# ANN foldl'_Reader (PermitPatternMatches [''A.Array, ''Int, ''IO]) #-}
{-# ANN foldl'_Reader (PermitConstructions [''Int, ''A.Array]) #-}
{-# ANN foldl'_Reader (PermitTypeClasses []) #-}
#elif defined(ARRAY_GENERIC)
{-# ANN foldl'_Reader (PermitPatternMatches [''Int, ''A.Array]) #-}
{-# ANN foldl'_Reader (PermitConstructions
    [ ''Int, ''[], ''Char, ''SrcLoc, ''CallStack, ''A.Array
    ]) #-}
{-# ANN foldl'_Reader (PermitTypeClasses [''IP]) #-}
#else
{-# ANN foldl'_Reader (PermitPatternMatches []) #-}
{-# ANN foldl'_Reader (PermitConstructions []) #-}
{-# ANN foldl'_Reader (PermitTypeClasses []) #-}
#endif
{-# NOINLINE foldl'_Reader #-}
foldl'_Reader :: Int -> Int -> IO Int
foldl'_Reader value =
    withArray value $ S.fold (Fold.foldl' (+) 0) . S.unfold A.reader

-------------------------------------------------------------------------------
-- Elimination
-------------------------------------------------------------------------------

#if defined(ARRAY_UNBOXED)
{-# ANN reader (PermitPatternMatches [''A.Array, ''Int, ''IO]) #-}
{-# ANN reader (PermitConstructions [''(), ''A.Array]) #-}
{-# ANN reader (PermitTypeClasses []) #-}
#elif defined(ARRAY_GENERIC)
{-# ANN reader (PermitPatternMatches [''A.Array, ''Int]) #-}
{-# ANN reader (PermitConstructions
    [ ''(), ''[], ''Char, ''Int, ''SrcLoc, ''CallStack, ''A.Array
    ]) #-}
{-# ANN reader (PermitTypeClasses [''IP]) #-}
#else
{-# ANN reader (PermitPatternMatches []) #-}
{-# ANN reader (PermitConstructions []) #-}
{-# ANN reader (PermitTypeClasses []) #-}
#endif
{-# NOINLINE reader #-}
reader :: Int -> Int -> IO ()
reader value = withArray value $ S.fold Fold.drain . S.unfold A.reader

#if defined(ARRAY_UNBOXED)
{-# ANN readRev (PermitPatternMatches [''IO]) #-}
{-# ANN readRev (PermitConstructions [''()]) #-}
{-# ANN readRev (PermitTypeClasses []) #-}
#elif defined(ARRAY_GENERIC)
{-# ANN readRev (PermitPatternMatches [''A.Array]) #-}
{-# ANN readRev (PermitConstructions
    [ ''(), ''[], ''Char, ''Int, ''SrcLoc, ''CallStack, ''A.Array
    ]) #-}
{-# ANN readRev (PermitTypeClasses [''IP]) #-}
#else
{-# ANN readRev (PermitPatternMatches []) #-}
{-# ANN readRev (PermitConstructions []) #-}
{-# ANN readRev (PermitTypeClasses []) #-}
#endif
{-# NOINLINE readRev #-}
readRev :: Int -> Int -> IO ()
readRev value = withArray value $ S.fold Fold.drain . A.readRev

#if defined(ARRAY_UNBOXED)
{-# ANN createOf_UnfoldrM (PermitPatternMatches [''IO]) #-}
{-# ANN createOf_UnfoldrM (PermitConstructions [''A.Array]) #-}
{-# ANN createOf_UnfoldrM (PermitTypeClasses []) #-}
#elif defined(ARRAY_GENERIC)
{-# ANN createOf_UnfoldrM (PermitPatternMatches []) #-}
{-# ANN createOf_UnfoldrM (PermitConstructions
    [ ''[], ''Char, ''Int, ''SrcLoc, ''CallStack, ''A.Array
    ]) #-}
{-# ANN createOf_UnfoldrM (PermitTypeClasses [''IP]) #-}
#else
{-# ANN createOf_UnfoldrM (PermitPatternMatches []) #-}
{-# ANN createOf_UnfoldrM (PermitConstructions []) #-}
{-# ANN createOf_UnfoldrM (PermitTypeClasses []) #-}
#endif
{-# NOINLINE createOf_UnfoldrM #-}
createOf_UnfoldrM :: Int -> Int -> IO (Arr Int)
createOf_UnfoldrM value = withStream value (S.fold (A.createOf value))

-------------------------------------------------------------------------------
-- Bench groups
-------------------------------------------------------------------------------

typeCommonBenchmarks :: Int -> [(SpaceComplexity, Benchmark)]
typeCommonBenchmarks size =
      [ (SpaceO_1, benchIO "create (enumerateFromTo)" $ create size)
      , (SpaceO_1, benchIO "createOf (enumerateFromTo)" $ createOf size)
      , (SpaceO_1, benchIO "fromListN (enumerateFromTo)"
            $ fromListN size)
      , (SpaceO_1, benchIO "createOf_Unfoldr" $ createOf_Unfoldr size)
      , (SpaceO_1, benchIO "createOf_FromList" $ createOf_FromList size)
      , (SpaceO_1, benchIO "show_ShowInstance" $ show_ShowInstance size)
      , (SpaceO_1, benchIO "read_ReadInstance" $ read_ReadInstance size)

      , (SpaceO_1, benchIO "eq_EqInstance" $ eq_EqInstance size)
      , (SpaceO_1, benchIO "notEq_EqInstance" $ notEq_EqInstance size)
      , (SpaceO_1, benchIO "lt_OrdInstance" $ lt_OrdInstance size)
      , (SpaceO_1, benchIO "min_OrdInstance" $ min_OrdInstance size)
      , (SpaceO_1, benchIO "foldl'_Reader" $ foldl'_Reader size)
      , (SpaceO_1, benchIO "reader" $ reader size)
      , (SpaceO_1, benchIO "readRev" $ readRev size)

      , (HeapO_n, benchIO "createOf_UnfoldrM" $ createOf_UnfoldrM size)
      ]
