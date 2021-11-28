{-# LANGUAGE CPP #-}

#include "Streamly/Benchmark/Data/Array/CommonImports.hs"

import qualified Streamly.Internal.Data.Array.Prim.Pinned as A
type Stream = A.Array

#include "Streamly/Benchmark/Data/Array/Common.hs"

-------------------------------------------------------------------------------
-- Benchmark helpers
-------------------------------------------------------------------------------

-- Drain a source that generates an array in the IO monad
{-# INLINE benchIOSrc #-}
benchIOSrc :: String -> (Int -> IO (Stream a)) -> Benchmark
benchIOSrc name src = benchIO name src id

-------------------------------------------------------------------------------
-- Bench Ops
-------------------------------------------------------------------------------

{-# INLINE sourceIntFromToFromList #-}
sourceIntFromToFromList :: MonadIO m => Int -> Int -> m (Stream Int)
sourceIntFromToFromList value n = P.return $ A.fromListN value [n..n + value]

-- CPP:
{-# INLINE sourceIntFromToFromStream #-}
sourceIntFromToFromStream :: MonadIO m => Int -> Int -> m (Stream Int)
sourceIntFromToFromStream value n = S.fold A.write $ S.enumerateFromTo n (n + value)

-------------------------------------------------------------------------------
-- Bench groups
-------------------------------------------------------------------------------

o_1_space_generation :: Int -> [Benchmark]
o_1_space_generation value =
    [ bgroup
        "generation"
        [ benchIOSrc "write . intFromTo" (sourceIntFromToFromStream value)
        ]
    ]

moduleName :: String
moduleName = "Data.Array.Prim.Pinned"

defStreamSize :: Int
defStreamSize = defaultStreamSize

main :: IO ()
main = runWithCLIOpts defStreamSize allBenchmarks

    where

    allBenchmarks size =
        bgroup (o_1_space_prefix moduleName) (o_1_space_generation size)
            : commonBenchmarks size
