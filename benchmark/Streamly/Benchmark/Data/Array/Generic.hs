{-# OPTIONS_GHC -Wno-deprecations #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{-# LANGUAGE CPP #-}

#include "Streamly/Benchmark/Data/Array/CommonImports.hs"

import Control.DeepSeq (deepseq)

import qualified Streamly.Internal.Data.Array.Generic as IA
import qualified Streamly.Internal.Data.Array.Generic as A

type Stream = A.Array

#include "Streamly/Benchmark/Data/Array/Common.hs"

instance NFData a => NFData (A.Array a) where
    {-# INLINE rnf #-}
    rnf = A.foldl' (\_ x -> rnf x) ()

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

{-# INLINE sourceIntFromToFromStream #-}
sourceIntFromToFromStream :: MonadIO m => Int -> Int -> m (Stream Int)
sourceIntFromToFromStream value n =
    S.fold A.write $ S.enumerateFromTo n (n + value)

-------------------------------------------------------------------------------
-- Bench groups
-------------------------------------------------------------------------------

o_1_space_generation :: Int -> [Benchmark]
o_1_space_generation value =
    [ bgroup
        "generation"
        [ benchIOSrc "write . intFromTo" (sourceIntFromToFromStream value)
        , let testStr = "fromList " ++ mkListString value
           in testStr `deepseq` bench "read" (nf readInstance testStr)
        ]
    ]

o_1_space_elimination :: Int -> [Benchmark]
o_1_space_elimination value =
    [ bgroup "elimination"
        [ benchFold "writeLastN.1"
            (S.fold (IA.writeLastN 1)) (P.sourceUnfoldrM value)
        , benchFold "writeLastN.10"
            (S.fold (IA.writeLastN 10)) (P.sourceUnfoldrM value)
#ifdef DEVBUILD
{-
          benchPureSink value "foldable/foldl'" foldableFoldl'
        , benchPureSink value "foldable/sum" foldableSum
-}
#endif
        ]
      ]

o_n_heap_serial :: Int -> [Benchmark]
o_n_heap_serial value =
    [ bgroup "elimination"
        [
        -- Converting the stream to an array
            benchFold "writeLastN.Max" (S.fold (IA.writeLastN (value + 1)))
                (P.sourceUnfoldrM value)
         ]
    ]

moduleName :: String
moduleName = "Data.Array.Generic"

defStreamSize :: Int
defStreamSize = defaultStreamSize

main :: IO ()
main = runWithCLIOpts defStreamSize allBenchmarks

    where

    allBenchmarks size =
        [ bgroup (o_1_space_prefix moduleName) $
             o_1_space_generation size ++ o_1_space_elimination size
        , bgroup (o_n_space_prefix moduleName) $
             o_n_heap_serial size
        ] ++ commonBenchmarks size
