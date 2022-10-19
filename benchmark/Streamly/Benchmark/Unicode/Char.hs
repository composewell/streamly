{-# OPTIONS_GHC -Wno-deprecations #-}

--
-- Module      : Streamly.Unicode.Char
-- Copyright   : (c) 2021 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import Control.DeepSeq (NFData)
import Streamly.Internal.Data.Array.Unboxed (Array)
import System.FilePath (dropExtensions, takeFileName)
import System.FilePath.Posix ((</>))
import Gauge.Main (Benchmark, bench, bgroup, defaultMain, env, nfIO)
import Streamly.Internal.Unicode.Char
    ( NormalizationMode(NFC, NFD, NFKC, NFKD)
    , normalize
    )
import Streamly.Benchmark.Common (o_1_space_prefix)

import qualified Streamly.Internal.Data.Array.Unboxed as Array
import qualified Streamly.Internal.Data.Stream.IsStream as IsStream
import qualified System.Directory as Dir

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

moduleName :: String
moduleName = "Unicode.Char"

dataDir :: FilePath
dataDir = "benchmark/Streamly/Benchmark/Unicode/data"

-- Truncate or expand all datasets to this size to provide a normalized
-- measurement view across all datasets and to reduce the effect of noise
-- because of the datasets being too small.
dataSetSize :: Int
dataSetSize = 1000000

makeBench :: (NFData a) => (String, a -> IO ()) -> (String, IO a) -> Benchmark
makeBench (implName, func) (dataName, setup) =
    env setup (bench (implName ++ "/" ++ dataName) . nfIO . func)

strInput :: FilePath -> (String, IO String)
strInput file = (dataName file, fmap (take dataSetSize . cycle) (readFile file))

    where

    dataName = dropExtensions . takeFileName

arrInput :: FilePath -> (String, IO (Array Char))
arrInput file = second (fmap Array.fromList) (strInput file)

    where

    second f (a, b) = (a, f b)

--------------------------------------------------------------------------------
-- Benchmarks
--------------------------------------------------------------------------------

benchFunctions :: [(String, Array Char -> IO ())]
benchFunctions =
    [ ("NFD", IsStream.drain . normalize NFD . Array.read)
    , ("NFKD", IsStream.drain . normalize NFKD . Array.read)
    , ("NFC", IsStream.drain . normalize NFC . Array.read)
    , ("NFKC", IsStream.drain . normalize NFKC . Array.read)
    ]

main :: IO ()
main = do
    cdir <- Dir.getCurrentDirectory
    dataFiles <- fmap (dataDir </>) <$> Dir.listDirectory (cdir </> dataDir)
    defaultMain
        [ bgroup
              (o_1_space_prefix moduleName)
              (makeBench <$> benchFunctions <*> map arrInput dataFiles)
        ]
