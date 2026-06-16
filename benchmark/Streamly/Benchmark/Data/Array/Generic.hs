{-# OPTIONS_GHC -Wno-orphans #-}

{-# LANGUAGE CPP #-}

#include "Streamly/Benchmark/Data/Array/CommonImports.hs"

import qualified Streamly.Internal.Data.Array.Generic as A

type Arr = A.Array

#include "Streamly/Benchmark/Data/Array/TypeCommon.hs"

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

-------------------------------------------------------------------------------
-- Bench groups
-------------------------------------------------------------------------------

moduleName :: String
moduleName = "Data.Array.Generic"

defStreamSize :: Int
defStreamSize = defaultStreamSize

benchmarks :: Int -> [(SpaceComplexity, Benchmark)]
benchmarks size =
    typeCommonBenchmarks size
    ++ commonBenchmarks size
    -- Before adding any benchmarks here check if they can be added to
    -- typeCommonBenchmarks (Array.Type source module common with
    -- Array.Generic) or commonBenchmarks (Array module common with
    -- Array.Generic) above.

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
