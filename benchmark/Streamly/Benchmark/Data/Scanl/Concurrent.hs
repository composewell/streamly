module Main (main) where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import Data.Function ((&))
-- import Streamly.Internal.Data.Scanl (Scanl)
import Streamly.Internal.Data.Stream (Stream)
import System.Random (randomRIO)

import qualified Streamly.Data.Fold as Fold
import qualified Streamly.Data.Stream as Stream
import qualified Streamly.Internal.Data.Scanl as Scanl
import qualified Streamly.Internal.Data.Scanl.Prelude as Scanl

import Data.IORef
import Streamly.Benchmark.Common
import Test.Tasty.Bench

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

type Seed = Int

{-# INLINE source #-}
source :: (Monad m, Num a, Stream.Enumerable a) =>
    Int -> a -> Stream m a
source len from =
    Stream.enumerateFromThenTo from (from + 1) (from + fromIntegral len)

{-# INLINE mkBench #-}
mkBench :: String -> (Seed -> IO ()) -> Benchmark
mkBench name f =
    bench name $ nfIO $ randomRIO (1, 1 :: Int) >>= f

--------------------------------------------------------------------------------
-- Benchmarks
--------------------------------------------------------------------------------

parDistributeScan :: Int -> Seed -> IO ()
parDistributeScan len seed = do
    ref <- newIORef [Scanl.latest]
    let gen = atomicModifyIORef ref (\xs -> ([], xs))
    Scanl.parDistributeScan id gen (source len seed)
        & Stream.fold Fold.drain

--------------------------------------------------------------------------------
-- Groups
--------------------------------------------------------------------------------

o_1_space_scans :: Int -> [Benchmark]
o_1_space_scans numElements =
    [ bgroup "scan"
        [ mkBench "parDistributeScan" (parDistributeScan numElements)
        ]
    ]

--------------------------------------------------------------------------------
-- Main
--------------------------------------------------------------------------------

moduleName :: String
moduleName = "Data.Scanl.Concurrent"

main :: IO ()
main = runWithCLIOpts defaultStreamSize allBenchmarks

    where

    allBenchmarks value =
        [ bgroup (o_1_space_prefix moduleName)
            ( o_1_space_scans value
            )
        ]
