
-- |
-- Module      : Stream.SplitChunks
-- Copyright   : (c) 2019 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

#ifdef __HADDOCK_VERSION__
#undef INSPECTION
#endif

#ifdef INSPECTION
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fplugin Test.Inspection.Plugin #-}
#endif

module Stream.ParseSplitChunks (benchmarks) where

import System.IO (Handle)

import qualified Streamly.Internal.Data.Array as Array
import qualified Streamly.Internal.Data.Fold as Fold
import qualified Streamly.Internal.Data.Stream as Stream
import qualified Streamly.Internal.FileSystem.Handle as Handle
import qualified Streamly.Internal.Unicode.Stream as Unicode

import Test.Tasty.Bench hiding (env)
import Prelude hiding (last, length)
import Streamly.Benchmark.Common
import Streamly.Benchmark.Common.Handle

#ifdef INSPECTION
import GHC.Types (SPEC(..))
import Test.Inspection
#endif

-------------------------------------------------------------------------------
-- reduce with splitting transformations
-------------------------------------------------------------------------------

-- | Split on a character sequence.
splitOnSeqUtf8 :: String -> Handle -> IO Int
splitOnSeqUtf8 str inh =
    Stream.fold Fold.length
        $ Stream.splitSepBySeq_ (Array.fromList str) Fold.drain
        $ Unicode.decodeUtf8Chunks
        $ Handle.readChunks inh -- >>= print

-------------------------------------------------------------------------------
-- Inspection
-------------------------------------------------------------------------------

#ifdef INSPECTION
-- splitOnSeqUtf8: sequence-matching state machine over UTF-8 decoded chunks;
-- Step constructors survive.
-- inspect $ hasNoTypeClasses 'splitOnSeqUtf8
-- inspect $ 'splitOnSeqUtf8 `hasNoType` ''Step
inspect $ 'splitOnSeqUtf8 `hasNoType` ''Fold.Step
inspect $ 'splitOnSeqUtf8 `hasNoType` ''SPEC
-- inspect $ 'splitOnSeqUtf8 `hasNoType` ''MutArray.ArrayUnsafe  -- FH.readChunks/A.read
-- inspect $ 'splitOnSeqUtf8 `hasNoType` ''Unfold.ConcatState    -- decodeUtf8Chunks
#endif

o_1_space_reduce_toChunks_split :: BenchEnv -> [Benchmark]
o_1_space_reduce_toChunks_split env =
    [ bgroup "FileSplitSeqUtf8"
        [ mkBenchSmall "splitOnSeqUtf8 word abcdefgh"
            env $ \inh _ -> splitOnSeqUtf8 "abcdefgh" inh
        , mkBenchSmall "splitOnSeqUtf8 KR abcdefghijklmnopqrstuvwxyz"
            env $ \inh _ -> splitOnSeqUtf8 "abcdefghijklmnopqrstuvwxyz" inh
        ]
    ]

benchmarks :: BenchEnv -> [(SpaceComplexity, Benchmark)]
benchmarks env =
    map (SpaceO_1,) (o_1_space_reduce_toChunks_split env)
