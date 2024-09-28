{-# OPTIONS_GHC -Wno-deprecations #-}

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

module Stream.Split (benchmarks) where

import Data.Char (ord)
import Data.Word (Word8)
import System.IO (Handle)
import Streamly.Internal.Data.Array (Array)

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
import Streamly.Internal.Data.Stream (Step(..))

import qualified Streamly.Internal.Data.MutArray as MutArray
import qualified Streamly.Internal.Data.Unfold as Unfold

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
    (Stream.fold Fold.length
        $ Stream.splitOn (== lf) Fold.drain
        $ Handle.read inh) -- >>= print

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'splitOn
inspect $ 'splitOn `hasNoType` ''Step
inspect $ 'splitOn `hasNoType` ''Unfold.ConcatState -- FH.read/UF.many
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
inspect $ 'wordsBy `hasNoType` ''Unfold.ConcatState -- FH.read/UF.many
inspect $ 'wordsBy `hasNoType` ''MutArray.ArrayUnsafe  -- FH.read/A.read
#endif

-- | Split on a word8 sequence.
splitOnSeq :: String -> Handle -> IO Int
splitOnSeq str inh =
    (Stream.fold Fold.length $ Stream.splitOnSeq (toarr str) Fold.drain
        $ Handle.read inh) -- >>= print

#ifdef INSPECTION
-- inspect $ hasNoTypeClasses 'splitOnSeq
-- inspect $ 'splitOnSeq `hasNoType` ''Step
#endif

takeEndBySeq :: String -> Handle -> IO Int
takeEndBySeq str inh =
    (Stream.fold Fold.length
        $ Stream.takeEndBySeq (toarr str)
        $ Stream.filter (/= fromIntegral (ord 'a'))
        $ Handle.read inh) -- >>= print

takeEndBySeq_ :: String -> Handle -> IO Int
takeEndBySeq_ str inh =
    (Stream.fold Fold.length
        $ Stream.takeEndBySeq_ (toarr str)
        $ Stream.filter (/= fromIntegral (ord 'a'))
        $ Handle.read inh) -- >>= print

takeEndBySeq100k :: Handle -> IO Int
takeEndBySeq100k inh = do
    arr <- Stream.fold Array.create $ Stream.replicate 100000 123
    (Stream.fold Fold.length
        $ Stream.takeEndBySeq arr
        $ Stream.filter (/= fromIntegral (ord 'a'))
        $ Handle.read inh) -- >>= print

takeEndBySeq_100k :: Handle -> IO Int
takeEndBySeq_100k inh = do
    arr <- Stream.fold Array.create $ Stream.replicate 100000 123
    (Stream.fold Fold.length
        $ Stream.takeEndBySeq_ arr
        $ Stream.filter (/= fromIntegral (ord 'a'))
        $ Handle.read inh) -- >>= print

-- | Split on a word8 sequence.
splitOnSeq100k :: Handle -> IO Int
splitOnSeq100k inh = do
    arr <- Stream.fold Array.create $ Stream.replicate 100000 123
    (Stream.fold Fold.length
        $ Stream.splitOnSeq arr Fold.drain
        $ Handle.read inh) -- >>= print

-- | Split on suffix sequence.
splitOnSuffixSeq :: String -> Handle -> IO Int
splitOnSuffixSeq str inh =
    (Stream.fold Fold.length
        $ Stream.splitOnSuffixSeq False (toarr str) Fold.drain
        $ Handle.read inh) -- >>= print

#ifdef INSPECTION
-- inspect $ hasNoTypeClasses 'splitOnSuffixSeq
-- inspect $ 'splitOnSuffixSeq `hasNoType` ''Step
#endif

-- | Split on suffix sequence.
splitWithSuffixSeq :: String -> Handle -> IO Int
splitWithSuffixSeq str inh =
    Stream.fold Fold.length
        $ Stream.splitOnSuffixSeq True (toarr str) Fold.drain
        $ Handle.read inh -- >>= print

o_1_space_reduce_read_split :: BenchEnv -> [Benchmark]
o_1_space_reduce_read_split env =
    -- NOTE: keep the benchmark names consistent with Data.Fold.takeEndBy*
    [ bgroup "FileSplitElem"
        [ mkBench "splitOn infix lf" env $ \inh _ ->
            splitOn inh
        ]
    -- splitting on a sequence
    , bgroup "FileSplitSeq"
        [
          mkBench "wordsBy infix isSpace" env $ \inh _ ->
            wordsBy inh

        -- Infix
        , mkBench "splitOnSeq empty infix" env $ \inh _ ->
            splitOnSeq "" inh
        , mkBench "splitOnSeq single infix lf" env $ \inh _ ->
            splitOnSeq "\n" inh
        , mkBench "splitOnSeq single infix a" env $ \inh _ ->
            splitOnSeq "a" inh
        , mkBench "splitOnSeq word infix crlf" env $ \inh _ ->
            splitOnSeq "\r\n" inh
        , mkBench "splitOnSeq word infix aa" env $ \inh _ ->
            splitOnSeq "aa" inh
        , mkBench "splitOnSeq word infix aaaa" env $ \inh _ ->
            splitOnSeq "aaaa" inh
        , mkBench "splitOnSeq word infix abcdefgh" env $ \inh _ ->
            splitOnSeq "abcdefgh" inh
        , mkBench "splitOnSeq KR infix abcdefghi" env $ \inh _ ->
            splitOnSeq "abcdefghi" inh
        , mkBench "splitOnSeq KR infix catcatcatcatcat" env $ \inh _ ->
            splitOnSeq "catcatcatcatcat" inh
        , mkBench "splitOnSeq KR infix abcdefghijklmnopqrstuvwxyz"
            env $ \inh _ -> splitOnSeq "abcdefghijklmnopqrstuvwxyz" inh
        , mkBench "splitOnSeq KR infix 100k long pattern"
            env $ \inh _ -> splitOnSeq100k inh

        -- Suffix
        , mkBench "splitOnSuffixSeq empty suffix" env $ \inh _ ->
            splitOnSuffixSeq "" inh
        , mkBench "splitOnSuffixSeq single suffix lf" env $ \inh _ ->
            splitOnSuffixSeq "\n" inh
        , mkBench "splitOnSuffixSeq word suffix crlf" env $ \inh _ ->
            splitOnSuffixSeq "\r\n" inh
        , mkBenchSmall "splitOnSuffixSeq KR suffix abcdefghijklmnopqrstuvwxyz"
            env $ \inh _ -> splitOnSuffixSeq "abcdefghijklmnopqrstuvwxyz" inh

        -- Suffix with separator
        , mkBench "splitWithSuffixSeq single suffix lf" env $ \inh _ ->
            splitWithSuffixSeq "\n" inh
        , mkBench "splitWithSuffixSeq word suffix crlf" env $ \inh _ ->
            splitWithSuffixSeq "\r\n" inh
        , mkBench "splitWithSuffixSeq KR suffix abcdefghi" env $ \inh _ ->
            splitWithSuffixSeq "abcdefghi" inh
        , mkBenchSmall "splitWithSuffixSeq KR suffix abcdefghijklmnopqrstuvwxyz"
            env $ \inh _ -> splitWithSuffixSeq "abcdefghijklmnopqrstuvwxyz" inh
        ]

    , bgroup "FileTakeSeq"
        [
        {-
          mkBench "takeEndBySeq empty" env $ \inh _ ->
                takeEndBySeq "" inh
        -}
        -- IMPORTANT: the pattern must contain a, because we filter a's out
        -- from the stream so that we do not terminate too early and
        -- unpredictably.
          mkBench "takeEndBySeq single a" env $ \inh _ ->
            takeEndBySeq "a" inh
        , mkBench "takeEndBySeq word aa" env $ \inh _ ->
            takeEndBySeq "aa" inh
        , mkBench "takeEndBySeq word aaaa" env $ \inh _ ->
            takeEndBySeq "aaaa" inh
        , mkBench "takeEndBySeq word abcdefgh" env $ \inh _ ->
            takeEndBySeq "abcdefgh" inh
        , mkBench "takeEndBySeq KR abcdefghi" env $ \inh _ ->
            takeEndBySeq "abcdefghi" inh
        , mkBench "takeEndBySeq KR catcatcatcatcat" env $ \inh _ ->
            takeEndBySeq "catcatcatcatcat" inh
        , mkBench "takeEndBySeq KR abcdefghijklmnopqrstuvwxyz"
            env $ \inh _ -> takeEndBySeq "abcdefghijklmnopqrstuvwxyz" inh
        , mkBench "takeEndBySeq KR 100k long pattern"
            env $ \inh _ -> takeEndBySeq100k inh

        {-
        , mkBench "takeEndBySeq_ empty" env $ \inh _ ->
            takeEndBySeq_ "" inh
        -}
        , mkBench "takeEndBySeq_ single a" env $ \inh _ ->
            takeEndBySeq_ "a" inh
        , mkBench "takeEndBySeq_ word aa" env $ \inh _ ->
            takeEndBySeq_ "aa" inh
        , mkBench "takeEndBySeq_ word aaaa" env $ \inh _ ->
            takeEndBySeq_ "aaaa" inh
        , mkBench "takeEndBySeq_ word abcdefgh" env $ \inh _ ->
            takeEndBySeq_ "abcdefgh" inh
        , mkBench "takeEndBySeq_ KR abcdefghi" env $ \inh _ ->
            takeEndBySeq_ "abcdefghi" inh
        , mkBench "takeEndBySeq_ KR catcatcatcatcat" env $ \inh _ ->
            takeEndBySeq_ "catcatcatcatcat" inh
        , mkBench "takeEndBySeq_ KR abcdefghijklmnopqrstuvwxyz"
            env $ \inh _ -> takeEndBySeq_ "abcdefghijklmnopqrstuvwxyz" inh
        , mkBench "takeEndBySeq_ KR 100k long pattern"
            env $ \inh _ -> takeEndBySeq_100k inh
        ]
    ]

-- | Split on a character sequence.
splitOnSeqUtf8 :: String -> Handle -> IO Int
splitOnSeqUtf8 str inh =
    (Stream.fold Fold.length
        $ Stream.splitOnSeq (Array.fromList str) Fold.drain
        $ Unicode.decodeUtf8Chunks
        $ Handle.readChunks inh) -- >>= print

o_1_space_reduce_toChunks_split :: BenchEnv -> [Benchmark]
o_1_space_reduce_toChunks_split env =
    [ bgroup "FileSplitSeqUtf8"
        [ mkBenchSmall "splitOnSeqUtf8 word abcdefgh"
            env $ \inh _ -> splitOnSeqUtf8 "abcdefgh" inh
        , mkBenchSmall "splitOnSeqUtf8 KR abcdefghijklmnopqrstuvwxyz"
            env $ \inh _ -> splitOnSeqUtf8 "abcdefghijklmnopqrstuvwxyz" inh
        ]
    ]

benchmarks :: String -> BenchEnv -> [Benchmark]
benchmarks moduleName env =
        [ bgroup (o_1_space_prefix moduleName) $ concat
            [ o_1_space_reduce_read_split env
            , o_1_space_reduce_toChunks_split env
            ]
        ]
