{-# OPTIONS_GHC -Wno-deprecations #-}

{-# LANGUAGE CPP #-}

#include "Streamly/Benchmark/Data/Array/CommonImports.hs"

import Control.DeepSeq (deepseq)

import qualified Streamly.Internal.Data.SmallArray as A
type Stream = A.SmallArray

#include "Streamly/Benchmark/Data/Array/Common.hs"

-------------------------------------------------------------------------------
-- Benchmark helpers
-------------------------------------------------------------------------------

-- Drain a source that generates an array in the IO monad
{-# INLINE benchIOSrc #-}
benchIOSrc :: NFData a => String -> (Int -> IO (Stream a)) -> Benchmark
benchIOSrc name src = benchIO name src id

-------------------------------------------------------------------------------
-- Bench Ops
-------------------------------------------------------------------------------

{-# INLINE sourceIntFromToFromList #-}
sourceIntFromToFromList :: MonadIO m => Int -> Int -> m (Stream Int)
sourceIntFromToFromList value n = P.return $ A.fromListN value [n..n + value]

{-# INLINE readInstance #-}
readInstance :: P.String -> Stream Int
readInstance str =
    let r = P.reads str
    in case r of
        [(x,"")] -> x
        _ -> P.error "readInstance: no parse"

#ifdef DEVBUILD
{-
{-# INLINE foldableFoldl' #-}
foldableFoldl' :: Stream Int -> Int
foldableFoldl' = F.foldl' (+) 0

{-# INLINE foldableSum #-}
foldableSum :: Stream Int -> Int
foldableSum = P.sum
-}
#endif

-------------------------------------------------------------------------------
-- Bench groups
-------------------------------------------------------------------------------

o_1_space_generation :: Int -> [Benchmark]
o_1_space_generation value =
    [ bgroup
        "generation"
        [ let testStr =
                  "fromListN " ++
                  show (value + 1) ++
                  "[1" ++ concat (replicate value ",1") ++ "]"
           in testStr `deepseq` bench "read" (nf readInstance testStr)
        ]
    ]

{-
o_1_space_elimination :: Int -> [Benchmark]
o_1_space_elimination value =
    [ bgroup "elimination"
        [
#ifdef DEVBUILD
{-
          benchPureSink value "foldable/foldl'" foldableFoldl'
        , benchPureSink value "foldable/sum" foldableSum
-}
#endif
        ]
      ]
-}

moduleName :: String
moduleName = "Data.SmallArray"

defStreamSize :: Int
defStreamSize = 128

benchmarks :: Int -> [(SpaceComplexity, Benchmark)]
benchmarks size =
    fmap (SpaceO_1,) (o_1_space_generation size) ++ commonBenchmarks size

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
