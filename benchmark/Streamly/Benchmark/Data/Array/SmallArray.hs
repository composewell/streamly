{-# LANGUAGE CPP #-}

#include "Streamly/Benchmark/Data/Array/CommonImports.hs"

import qualified Streamly.Internal.Data.SmallArray as A
type Arr = A.SmallArray

#include "Streamly/Benchmark/Data/Array/Common.hs"

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
readInstance value =
    let testStr = "fromListN " ++ show (value + 1)
                    ++ "[1" ++ concat (replicate value ",1") ++ "]"
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

-------------------------------------------------------------------------------
-- Bench groups
-------------------------------------------------------------------------------

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
      [ (SpaceO_1, benchIO "read" $ readInstance size)
      ]
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
