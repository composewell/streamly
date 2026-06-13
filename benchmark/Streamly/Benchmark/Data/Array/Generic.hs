{-# OPTIONS_GHC -Wno-orphans #-}

{-# LANGUAGE CPP #-}

#include "Streamly/Benchmark/Data/Array/CommonImports.hs"

import qualified Streamly.Internal.Data.Array.Generic as IA
import qualified Streamly.Internal.Data.Array.Generic as A

type Arr = A.Array

#include "Streamly/Benchmark/Data/Array/Common.hs"

instance NFData a => NFData (A.Array a) where
    {-# INLINE rnf #-}
    rnf = A.foldl' (\_ x -> rnf x) ()

-------------------------------------------------------------------------------
-- Bench Ops
-------------------------------------------------------------------------------

{-# INLINE sourceIntFromToFromList #-}
sourceIntFromToFromList :: Int -> IO (Arr Int)
sourceIntFromToFromList value = withRandomIntIO $ \n ->
    P.return $ A.fromListN value [n..n + value]

{-# INLINE parseInstance #-}
parseInstance :: P.String -> Arr Int
parseInstance str =
    let r = P.reads str
    in case r of
        [(x,"")] -> x
        _ -> P.error "parseInstance: no parse"

{-# INLINE readInstance #-}
readInstance :: Int -> IO (Arr Int)
readInstance value = withRandomIntIO $ \n ->
    let testStr = "fromList " ++ show [n..n+value]
    in return $! parseInstance testStr

#ifdef DEVBUILD
{-
{-# INLINE foldableFoldl' #-}
foldableFoldl' :: Arr Int -> Int
foldableFoldl' = F.foldl' (+) 0

{-# INLINE foldableSum #-}
foldableSum :: Arr Int -> Int
foldableSum = P.sum
-}
#endif

{-# INLINE sourceIntFromToFromStream #-}
sourceIntFromToFromStream :: Int -> IO (Arr Int)
sourceIntFromToFromStream value = withRandomIntIO $ \n ->
    S.fold A.create $ S.enumerateFromTo n (n + value)

{-# INLINE createOfLast1 #-}
createOfLast1 :: Int -> IO (Arr Int)
createOfLast1 value = withStream value (S.fold (IA.createOfLast 1))

{-# INLINE createOfLast10 #-}
createOfLast10 :: Int -> IO (Arr Int)
createOfLast10 value = withStream value (S.fold (IA.createOfLast 10))

{-# INLINE createOfLastMax #-}
createOfLastMax :: Int -> IO (Arr Int)
createOfLastMax value = withStream value (S.fold (IA.createOfLast (value + 1)))

-------------------------------------------------------------------------------
-- Bench groups
-------------------------------------------------------------------------------

o_1_space_generation :: Int -> [Benchmark]
o_1_space_generation value =
    [ benchIO "write . intFromTo" $ sourceIntFromToFromStream value
    , benchIO "read" $ readInstance value
    ]

o_1_space_elimination :: Int -> [Benchmark]
o_1_space_elimination value =
    [ benchIO "createOfLast.1" $ createOfLast1 value
    , benchIO "createOfLast.10" $ createOfLast10 value
    ]

o_n_heap_serial :: Int -> [Benchmark]
o_n_heap_serial value =
    [ benchIO "createOfLast.Max" $ createOfLastMax value
    ]

moduleName :: String
moduleName = "Data.Array.Generic"

defStreamSize :: Int
defStreamSize = defaultStreamSize

benchmarks :: Int -> [(SpaceComplexity, Benchmark)]
benchmarks size =
       fmap (SpaceO_1,)
            (o_1_space_generation size ++ o_1_space_elimination size)
    ++ fmap (HeapO_n,) (o_n_heap_serial size)
    ++ commonBenchmarks size

main :: IO ()
main = runWithCLIOpts defStreamSize allBenchmarks

    where

    allBenchmarks size =
        let allBenches = benchmarks size
            get x = fmap snd $ filter ((==) x . fst) allBenches
            o_1_space = get SpaceO_1
            o_n_heap = get HeapO_n
            o_n_space = get SpaceO_n
        in
        [ bgroup (o_1_space_prefix moduleName) o_1_space
        , bgroup (o_n_heap_prefix moduleName) o_n_heap
        , bgroup (o_n_space_prefix moduleName) o_n_space
        ]
