
-- |
-- Module      : CrossModule.SplitChunks
-- Copyright   : (c) 2019 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- {-# OPTIONS_GHC -fforce-recomp #-}

#ifdef __HADDOCK_VERSION__
#undef INSPECTION
#endif

#ifdef INSPECTION
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fplugin Test.Inspection.Plugin #-}
#endif

module CrossModule.SplitChunks (benchmarks) where

import Data.Word (Word32, Word8)
import Foreign.Ptr (Ptr)
import Streamly.Internal.Data.MutByteArray (PinnedState)
import GHC.Classes (IP)
import GHC.Stack (CallStack, SrcLoc)
import System.IO (Handle)
import Streamly.Data.MutByteArray (MutByteArray)
import Streamly.Internal.Data.Array (Array)
import Streamly.Internal.Data.MutArray (MutArray)
import Streamly.Internal.Data.RingArray (RingArray)
import Streamly.Internal.Data.Stream (SplitOnSeqState, Step)
import Streamly.Internal.Unicode.Stream (FlattenState)
import Unsafe.Coerce (UnsafeEquality)

import qualified Streamly.Internal.Data.Array as Array
import qualified Streamly.Internal.Data.Fold as Fold
import qualified Streamly.Internal.Data.Stream as Stream
import qualified Streamly.Internal.Data.SVar.Type as SVar
import qualified Streamly.Internal.FileSystem.Handle as Handle
import qualified Streamly.Internal.Unicode.Stream as Unicode

import Fusion.Plugin.Types
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

-- XXX Move the creation outside the benchamrked action to reduce the permitted
-- types.
-- | Split on a character sequence.
{-# ANN splitOnSeqUtf8 (PermitPatternMatches
    [''MutArray,''[],''Char,''Int,''SVar.State,''UnsafeEquality,''IO
    ,''Array,''Word,''Ptr,''Step,''(),''MutByteArray,''FlattenState
    ,''Maybe,''(,),''Word8,''Word32,''RingArray,''SplitOnSeqState]) #-}
{-# ANN splitOnSeqUtf8 (PermitConstructions
    [''MutArray,''SVar.State,''Maybe,''Bool,''Int,''SrcLoc,''CallStack
    ,''Word,''SplitOnSeqState,''(),''Word32,''[],''Array,''FlattenState
    ,''MutByteArray,''Char,''RingArray,''Ptr,''PinnedState]) #-}
{-# ANN splitOnSeqUtf8 (PermitTypeClasses [''IP]) #-}
{-# NOINLINE splitOnSeqUtf8 #-}
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

benchmarks :: BenchEnv -> [(SpaceComplexity, Benchmark)]
benchmarks env =
      [ (SpaceO_1, mkBenchSmall "splitOnSeqUtf8 word abcdefgh"
            env $ \inh _ -> splitOnSeqUtf8 "abcdefgh" inh)
      , (SpaceO_1, mkBenchSmall "splitOnSeqUtf8 KR abcdefghijklmnopqrstuvwxyz"
            env $ \inh _ -> splitOnSeqUtf8 "abcdefghijklmnopqrstuvwxyz" inh)
      ]
