
-- |
-- Module      : Stream.Split
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

module Stream.Parse.Split (benchmarks) where

import Data.Char (ord)
import Data.Word (Word8)
import System.IO (Handle)
import Streamly.Internal.Data.Array (Array)

import qualified Streamly.Internal.Data.Array as Array
import qualified Streamly.Internal.Data.Fold as Fold
import qualified Streamly.Internal.Data.Stream as Stream
import qualified Streamly.Internal.FileSystem.Handle as Handle

import Test.Tasty.Bench hiding (env)
import Prelude hiding (last, length)
import Streamly.Benchmark.Common
import Streamly.Benchmark.Common.Handle

#ifdef INSPECTION
import Streamly.Internal.Data.Stream (Step(..))

import qualified Streamly.Internal.Data.MutArray as MutArray
import qualified Streamly.Internal.Data.Producer as Producer

import GHC.Types (SPEC(..))
import Test.Inspection
#endif

 -------------------------------------------------------------------------------
-- reduce with splitting transformations
-------------------------------------------------------------------------------

lf :: Word8
lf = fromIntegral (ord '\n')

toarr :: String -> Array Word8
toarr = Array.fromList . map (fromIntegral . ord)

-- | Split on line feed.
splitOn :: Handle -> IO Int
splitOn inh =
    Stream.fold Fold.length
        $ Stream.splitSepBy_ (== lf) Fold.drain
        $ Handle.read inh -- >>= print

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'splitOn
inspect $ 'splitOn `hasNoType` ''Step
inspect $ 'splitOn `hasNoType` ''Fold.Step
inspect $ 'splitOn `hasNoType` ''SPEC
inspect $ 'splitOn `hasNoType` ''Stream.SplitSepBy
inspect $ 'splitOn `hasNoType` ''Producer.ConcatState -- FH.read/UF.many
inspect $ 'splitOn `hasNoType` ''MutArray.ArrayUnsafe  -- FH.read/A.read
#endif

-- | Words by space
wordsBy :: Handle -> IO Int
wordsBy inh =
    Stream.fold Fold.length
        $ Stream.wordsBy isSp Fold.drain
        $ Handle.read inh -- >>= print

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'wordsBy
inspect $ 'wordsBy `hasNoType` ''Step
inspect $ 'wordsBy `hasNoType` ''Fold.Step
inspect $ 'wordsBy `hasNoType` ''SPEC
inspect $ 'wordsBy `hasNoType` ''Producer.ConcatState -- FH.read/UF.many
inspect $ 'wordsBy `hasNoType` ''MutArray.ArrayUnsafe  -- FH.read/A.read
#endif

-- | Split on a word8 sequence.
splitOnSeq :: String -> Handle -> IO Int
splitOnSeq str inh =
    Stream.fold Fold.length $ Stream.splitSepBySeq_ (toarr str) Fold.drain
        $ Handle.read inh -- >>= print

#ifdef INSPECTION
-- splitOnSeq/splitOnSeq100k: sequence-matching state machine; Step constructors survive.
-- inspect $ hasNoTypeClasses 'splitOnSeq
-- inspect $ 'splitOnSeq `hasNoType` ''Step
inspect $ 'splitOnSeq `hasNoType` ''Fold.Step
inspect $ 'splitOnSeq `hasNoType` ''SPEC
#endif

takeEndBy :: Word8 -> Handle -> IO Int
takeEndBy c inh =
    Stream.fold Fold.length
        $ Stream.takeEndBy (== c)
        $ Stream.filter (/= fromIntegral (ord 'a'))
        $ Handle.read inh -- >>= print

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'takeEndBy
inspect $ 'takeEndBy `hasNoType` ''Step
inspect $ 'takeEndBy `hasNoType` ''Fold.Step
inspect $ 'takeEndBy `hasNoType` ''SPEC
inspect $ 'takeEndBy `hasNoType` ''Producer.ConcatState -- FH.read/UF.many
inspect $ 'takeEndBy `hasNoType` ''MutArray.ArrayUnsafe  -- FH.read/A.read
#endif

takeEndBy_ :: Word8 -> Handle -> IO Int
takeEndBy_ c inh =
    Stream.fold Fold.length
        $ Stream.takeEndBy_ (== c)
        $ Stream.filter (/= fromIntegral (ord 'a'))
        $ Handle.read inh -- >>= print

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'takeEndBy_
inspect $ 'takeEndBy_ `hasNoType` ''Step
inspect $ 'takeEndBy_ `hasNoType` ''Fold.Step
inspect $ 'takeEndBy_ `hasNoType` ''SPEC
inspect $ 'takeEndBy_ `hasNoType` ''Producer.ConcatState -- FH.read/UF.many
inspect $ 'takeEndBy_ `hasNoType` ''MutArray.ArrayUnsafe  -- FH.read/A.read
#endif

takeEndBySeq :: String -> Handle -> IO Int
takeEndBySeq str inh =
    Stream.fold Fold.length
        $ Stream.takeEndBySeq (toarr str)
        $ Stream.filter (/= fromIntegral (ord 'a'))
        $ Handle.read inh -- >>= print

#ifdef INSPECTION
-- takeEndBySeq/takeEndBySeq_: KR state machine; Step constructors survive
-- unless -fspec-constr-recursive=12 is in effect.
-- inspect $ hasNoTypeClasses 'takeEndBySeq
-- inspect $ 'takeEndBySeq `hasNoType` ''Step
inspect $ 'takeEndBySeq `hasNoType` ''Fold.Step
inspect $ 'takeEndBySeq `hasNoType` ''SPEC
#endif

takeEndBySeq_ :: String -> Handle -> IO Int
takeEndBySeq_ str inh =
    Stream.fold Fold.length
        $ Stream.takeEndBySeq_ (toarr str)
        $ Stream.filter (/= fromIntegral (ord 'a'))
        $ Handle.read inh -- >>= print

#ifdef INSPECTION
-- inspect $ hasNoTypeClasses 'takeEndBySeq_
-- inspect $ 'takeEndBySeq_ `hasNoType` ''Step
inspect $ 'takeEndBySeq_ `hasNoType` ''Fold.Step
inspect $ 'takeEndBySeq_ `hasNoType` ''SPEC
#endif

takeEndBySeq100k :: Handle -> IO Int
takeEndBySeq100k inh = do
    arr <- Stream.fold Array.create $ Stream.replicate 100000 123
    Stream.fold Fold.length
        $ Stream.takeEndBySeq arr
        $ Stream.filter (/= fromIntegral (ord 'a'))
        $ Handle.read inh -- >>= print

#ifdef INSPECTION
-- inspect $ hasNoTypeClasses 'takeEndBySeq100k
-- inspect $ 'takeEndBySeq100k `hasNoType` ''Step
inspect $ 'takeEndBySeq100k `hasNoType` ''Fold.Step
inspect $ 'takeEndBySeq100k `hasNoType` ''SPEC
#endif

takeEndBySeq_100k :: Handle -> IO Int
takeEndBySeq_100k inh = do
    arr <- Stream.fold Array.create $ Stream.replicate 100000 123
    Stream.fold Fold.length
        $ Stream.takeEndBySeq_ arr
        $ Stream.filter (/= fromIntegral (ord 'a'))
        $ Handle.read inh -- >>= print

#ifdef INSPECTION
-- inspect $ hasNoTypeClasses 'takeEndBySeq_100k
-- inspect $ 'takeEndBySeq_100k `hasNoType` ''Step
inspect $ 'takeEndBySeq_100k `hasNoType` ''Fold.Step
inspect $ 'takeEndBySeq_100k `hasNoType` ''SPEC
#endif

-- | Split on a word8 sequence.
splitOnSeq100k :: Handle -> IO Int
splitOnSeq100k inh = do
    arr <- Stream.fold Array.create $ Stream.replicate 100000 123
    Stream.fold Fold.length
        $ Stream.splitSepBySeq_ arr Fold.drain
        $ Handle.read inh -- >>= print

#ifdef INSPECTION
-- inspect $ hasNoTypeClasses 'splitOnSeq100k
-- inspect $ 'splitOnSeq100k `hasNoType` ''Step
inspect $ 'splitOnSeq100k `hasNoType` ''Fold.Step
inspect $ 'splitOnSeq100k `hasNoType` ''SPEC
#endif

-- | Split on suffix sequence.
splitOnSuffixSeq :: String -> Handle -> IO Int
splitOnSuffixSeq str inh =
    Stream.fold Fold.length
        $ Stream.splitOnSuffixSeq False (toarr str) Fold.drain
        $ Handle.read inh -- >>= print

#ifdef INSPECTION
-- splitOnSuffixSeq/splitWithSuffixSeq: sequence-matching state machine; Step constructors survive.
-- inspect $ hasNoTypeClasses 'splitOnSuffixSeq
-- inspect $ 'splitOnSuffixSeq `hasNoType` ''Step
inspect $ 'splitOnSuffixSeq `hasNoType` ''Fold.Step
inspect $ 'splitOnSuffixSeq `hasNoType` ''SPEC
#endif

-- | Split on suffix sequence.
splitWithSuffixSeq :: String -> Handle -> IO Int
splitWithSuffixSeq str inh =
    Stream.fold Fold.length
        $ Stream.splitOnSuffixSeq True (toarr str) Fold.drain
        $ Handle.read inh -- >>= print

#ifdef INSPECTION
-- inspect $ hasNoTypeClasses 'splitWithSuffixSeq
-- inspect $ 'splitWithSuffixSeq `hasNoType` ''Step
inspect $ 'splitWithSuffixSeq `hasNoType` ''Fold.Step
inspect $ 'splitWithSuffixSeq `hasNoType` ''SPEC
#endif

benchmarks :: BenchEnv -> [(SpaceComplexity, Benchmark)]
benchmarks env =
    -- NOTE: keep the benchmark names consistent with Data.Fold.takeEndBy*
      [ (SpaceO_1, mkBench "splitOn infix lf" env $ \inh _ ->
            splitOn inh)

      -- splitting on a sequence
      , (SpaceO_1, mkBench "wordsBy infix isSpace" env $ \inh _ ->
            wordsBy inh)

      -- Infix
      , (SpaceO_1, mkBench "splitOnSeq empty infix" env $ \inh _ ->
            splitOnSeq "" inh)
      , (SpaceO_1, mkBench "splitOnSeq single infix lf" env $ \inh _ ->
            splitOnSeq "\n" inh)
      , (SpaceO_1, mkBench "splitOnSeq single infix a" env $ \inh _ ->
            splitOnSeq "a" inh)
      , (SpaceO_1, mkBench "splitOnSeq word infix crlf" env $ \inh _ ->
            splitOnSeq "\r\n" inh)
      , (SpaceO_1, mkBench "splitOnSeq word infix aa" env $ \inh _ ->
            splitOnSeq "aa" inh)
      , (SpaceO_1, mkBench "splitOnSeq word infix aaaa" env $ \inh _ ->
            splitOnSeq "aaaa" inh)
      , (SpaceO_1, mkBench "splitOnSeq word infix abcdefgh" env $ \inh _ ->
            splitOnSeq "abcdefgh" inh)
      , (SpaceO_1, mkBench "splitOnSeq KR infix abcdefghi" env $ \inh _ ->
            splitOnSeq "abcdefghi" inh)
      , (SpaceO_1, mkBench "splitOnSeq KR infix catcatcatcatcat" env $ \inh _ ->
            splitOnSeq "catcatcatcatcat" inh)
      , (SpaceO_1, mkBench "splitOnSeq KR infix abcdefghijklmnopqrstuvwxyz"
            env $ \inh _ -> splitOnSeq "abcdefghijklmnopqrstuvwxyz" inh)
      , (SpaceO_1, mkBench "splitOnSeq KR infix 100k long pattern"
            env $ \inh _ -> splitOnSeq100k inh)

      -- Suffix
      , (SpaceO_1, mkBench "splitOnSuffixSeq empty suffix" env $ \inh _ ->
            splitOnSuffixSeq "" inh)
      , (SpaceO_1, mkBench "splitOnSuffixSeq single suffix lf" env $ \inh _ ->
            splitOnSuffixSeq "\n" inh)
      , (SpaceO_1, mkBench "splitOnSuffixSeq word suffix crlf" env $ \inh _ ->
            splitOnSuffixSeq "\r\n" inh)
      , (SpaceO_1, mkBenchSmall "splitOnSuffixSeq KR suffix abcdefghijklmnopqrstuvwxyz"
            env $ \inh _ -> splitOnSuffixSeq "abcdefghijklmnopqrstuvwxyz" inh)

      -- Suffix with separator
      , (SpaceO_1, mkBench "splitWithSuffixSeq single suffix lf" env $ \inh _ ->
            splitWithSuffixSeq "\n" inh)
      , (SpaceO_1, mkBench "splitWithSuffixSeq word suffix crlf" env $ \inh _ ->
            splitWithSuffixSeq "\r\n" inh)
      , (SpaceO_1, mkBench "splitWithSuffixSeq KR suffix abcdefghi" env $ \inh _ ->
            splitWithSuffixSeq "abcdefghi" inh)
      , (SpaceO_1, mkBenchSmall "splitWithSuffixSeq KR suffix abcdefghijklmnopqrstuvwxyz"
            env $ \inh _ -> splitWithSuffixSeq "abcdefghijklmnopqrstuvwxyz" inh)

      {-
        mkBench "takeEndBySeq empty" env $ \inh _ ->
              takeEndBySeq "" inh
      -}
      -- IMPORTANT: the pattern must contain a, because we filter a's out
      -- from the stream so that we do not terminate too early and
      -- unpredictably.
      , (SpaceO_1, mkBench "takeEndBy" env $ \inh _ ->
            takeEndBy (fromIntegral $ ord 'a') inh)
      , (SpaceO_1, mkBench "takeEndBy_" env $ \inh _ ->
            takeEndBy_ (fromIntegral $ ord 'a') inh)
      , (SpaceO_1, mkBench "takeEndBySeq single a" env $ \inh _ ->
            takeEndBySeq "a" inh)
      , (SpaceO_1, mkBench "takeEndBySeq word aa" env $ \inh _ ->
            takeEndBySeq "aa" inh)
      , (SpaceO_1, mkBench "takeEndBySeq word aaaa" env $ \inh _ ->
            takeEndBySeq "aaaa" inh)
      , (SpaceO_1, mkBench "takeEndBySeq word abcdefgh" env $ \inh _ ->
            takeEndBySeq "abcdefgh" inh)

      -- XXX takeEndBySeq KR requires @-fspec-constr-recursive=12@.
      , (SpaceO_1, mkBench "takeEndBySeq KR abcdefghi" env $ \inh _ ->
            takeEndBySeq "abcdefghi" inh)
      , (SpaceO_1, mkBench "takeEndBySeq KR catcatcatcatcat" env $ \inh _ ->
            takeEndBySeq "catcatcatcatcat" inh)
      , (SpaceO_1, mkBench "takeEndBySeq KR abcdefghijklmnopqrstuvwxyz"
            env $ \inh _ -> takeEndBySeq "abcdefghijklmnopqrstuvwxyz" inh)
      , (SpaceO_1, mkBench "takeEndBySeq KR 100k long pattern"
            env $ \inh _ -> takeEndBySeq100k inh)

      {-
      , mkBench "takeEndBySeq_ empty" env $ \inh _ ->
          takeEndBySeq_ "" inh
      -}
      , (SpaceO_1, mkBench "takeEndBySeq_ single a" env $ \inh _ ->
            takeEndBySeq_ "a" inh)
      , (SpaceO_1, mkBench "takeEndBySeq_ word aa" env $ \inh _ ->
            takeEndBySeq_ "aa" inh)
      , (SpaceO_1, mkBench "takeEndBySeq_ word aaaa" env $ \inh _ ->
            takeEndBySeq_ "aaaa" inh)
      , (SpaceO_1, mkBench "takeEndBySeq_ word abcdefgh" env $ \inh _ ->
            takeEndBySeq_ "abcdefgh" inh)
      , (SpaceO_1, mkBench "takeEndBySeq_ KR abcdefghi" env $ \inh _ ->
            takeEndBySeq_ "abcdefghi" inh)
      , (SpaceO_1, mkBench "takeEndBySeq_ KR catcatcatcatcat" env $ \inh _ ->
            takeEndBySeq_ "catcatcatcatcat" inh)
      , (SpaceO_1, mkBench "takeEndBySeq_ KR abcdefghijklmnopqrstuvwxyz"
            env $ \inh _ -> takeEndBySeq_ "abcdefghijklmnopqrstuvwxyz" inh)
      , (SpaceO_1, mkBench "takeEndBySeq_ KR 100k long pattern"
            env $ \inh _ -> takeEndBySeq_100k inh)
      ]
