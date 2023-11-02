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

import qualified Streamly.FileSystem.Handle as FH
import qualified Streamly.Internal.Data.Array as A
import qualified Streamly.Internal.Data.Fold as FL
import qualified Streamly.Internal.Data.Parser as PR
import qualified Streamly.Internal.Data.Stream.IsStream as IP
import qualified Streamly.Internal.FileSystem.Handle as IFH
import qualified Streamly.Internal.Unicode.Stream as IUS
import qualified Streamly.Prelude as S

import Test.Tasty.Bench hiding (env)
import Prelude hiding (last, length)
import Streamly.Benchmark.Common
import Streamly.Benchmark.Common.Handle

#ifdef INSPECTION
import Streamly.Internal.Data.Stream (Step(..))

import qualified Streamly.Internal.Data.MutArray as MA
import qualified Streamly.Internal.Data.Unfold as IUF

import Test.Inspection
#endif

 -------------------------------------------------------------------------------
-- reduce with splitting transformations
-------------------------------------------------------------------------------

lf :: Word8
lf = fromIntegral (ord '\n')

toarr :: String -> A.Array Word8
toarr = A.fromList . map (fromIntegral . ord)

-- | Split on line feed.
splitOn :: Handle -> IO Int
splitOn inh =
    (S.length $ S.splitOn (== lf) FL.drain
        $ S.unfold FH.read inh) -- >>= print

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'splitOn
inspect $ 'splitOn `hasNoType` ''Step
inspect $ 'splitOn `hasNoType` ''IUF.ConcatState -- FH.read/UF.many
inspect $ 'splitOn `hasNoType` ''MA.ArrayUnsafe  -- FH.read/A.read
#endif

-- | Split suffix on line feed.
splitOnSuffix :: Handle -> IO Int
splitOnSuffix inh =
    (S.length $ S.splitOnSuffix (== lf) FL.drain
        $ S.unfold FH.read inh) -- >>= print

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'splitOnSuffix
inspect $ 'splitOnSuffix `hasNoType` ''Step
inspect $ 'splitOnSuffix `hasNoType` ''IUF.ConcatState -- FH.read/UF.many
inspect $ 'splitOnSuffix `hasNoType` ''MA.ArrayUnsafe  -- FH.read/A.read
#endif

-- | Split suffix with line feed.
splitWithSuffix :: Handle -> IO Int
splitWithSuffix inh =
    (S.length $ S.splitWithSuffix (== lf) FL.drain
        $ S.unfold FH.read inh) -- >>= print

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'splitWithSuffix
inspect $ 'splitWithSuffix `hasNoType` ''Step
inspect $ 'splitWithSuffix `hasNoType` ''IUF.ConcatState -- FH.read/UF.many
inspect $ 'splitWithSuffix `hasNoType` ''MA.ArrayUnsafe  -- FH.read/A.read
#endif

-- | Split on line feed.
foldManySepBy :: Handle -> IO Int
foldManySepBy inh =
    (S.length
        $ IP.foldMany
            (FL.takeEndBy_ (== lf) FL.drain)
            (S.unfold FH.read inh)
    ) -- >>= print

-- | Split on line feed.
parseManySepBy :: Handle -> IO Int
parseManySepBy inh =
    (S.length
        $ IP.parseMany
            (PR.fromFold $ FL.takeEndBy_ (== lf) FL.drain)
            (S.unfold FH.read inh)
    ) -- >>= print

-- | Words by space
wordsBy :: Handle -> IO Int
wordsBy inh =
    (S.length $ S.wordsBy isSp FL.drain
        $ S.unfold FH.read inh) -- >>= print

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'wordsBy
inspect $ 'wordsBy `hasNoType` ''Step
inspect $ 'wordsBy `hasNoType` ''IUF.ConcatState -- FH.read/UF.many
inspect $ 'wordsBy `hasNoType` ''MA.ArrayUnsafe  -- FH.read/A.read
#endif

-- | Split on a word8 sequence.
splitOnSeq :: String -> Handle -> IO Int
splitOnSeq str inh =
    (S.length $ IP.splitOnSeq (toarr str) FL.drain
        $ S.unfold FH.read inh) -- >>= print

#ifdef INSPECTION
-- inspect $ hasNoTypeClasses 'splitOnSeq
-- inspect $ 'splitOnSeq `hasNoType` ''Step
#endif

-- | Split on a word8 sequence.
splitOnSeq100k :: Handle -> IO Int
splitOnSeq100k inh = do
    arr <- A.fromStream $ IP.toStream $ S.fromSerial $ S.replicate 100000 123
    (S.length $ IP.splitOnSeq arr FL.drain
        $ S.unfold FH.read inh) -- >>= print

-- | Split on suffix sequence.
splitOnSuffixSeq :: String -> Handle -> IO Int
splitOnSuffixSeq str inh =
    (S.length $ IP.splitOnSuffixSeq (toarr str) FL.drain
        $ S.unfold FH.read inh) -- >>= print

#ifdef INSPECTION
-- inspect $ hasNoTypeClasses 'splitOnSuffixSeq
-- inspect $ 'splitOnSuffixSeq `hasNoType` ''Step
#endif

-- | Split on suffix sequence.
splitWithSuffixSeq :: String -> Handle -> IO Int
splitWithSuffixSeq str inh =
    S.length $ IP.splitWithSuffixSeq (toarr str) FL.drain
        $ S.unfold FH.read inh -- >>= print

o_1_space_reduce_read_split :: BenchEnv -> [Benchmark]
o_1_space_reduce_read_split env =
    [ bgroup "split"
        [ mkBench "S.foldMany (FL.takeEndBy_ (== lf) FL.drain)" env
            $ \inh _ -> foldManySepBy inh
        , mkBench "S.parseMany (FL.takeEndBy_ (== lf) FL.drain)" env
            $ \inh _ -> parseManySepBy inh
        , mkBench "S.wordsBy isSpace FL.drain" env $ \inh _ ->
            wordsBy inh
        , mkBench "S.splitOn (== lf) FL.drain" env $ \inh _ ->
            splitOn inh
        , mkBench "S.splitOnSuffix (== lf) FL.drain" env $ \inh _ ->
            splitOnSuffix inh
        , mkBench "S.splitWithSuffix (== lf) FL.drain" env $ \inh _ ->
            splitWithSuffix inh
        , mkBench "S.splitOnSeq \"\" FL.drain" env $ \inh _ ->
            splitOnSeq "" inh
        , mkBench "S.splitOnSuffixSeq \"\" FL.drain" env $ \inh _ ->
            splitOnSuffixSeq "" inh
        , mkBench "S.splitOnSeq \"\\n\" FL.drain" env $ \inh _ ->
            splitOnSeq "\n" inh
        , mkBench "S.splitOnSuffixSeq \"\\n\" FL.drain" env $ \inh _ ->
            splitOnSuffixSeq "\n" inh
        , mkBench "S.splitOnSeq \"a\" FL.drain" env $ \inh _ ->
            splitOnSeq "a" inh
        , mkBench "S.splitOnSeq \"\\r\\n\" FL.drain" env $ \inh _ ->
            splitOnSeq "\r\n" inh
        , mkBench "S.splitOnSuffixSeq \"\\r\\n\" FL.drain" env $ \inh _ ->
            splitOnSuffixSeq "\r\n" inh
        , mkBench "S.splitWithSuffixSeq \"\\r\\n\" FL.drain" env $ \inh _ ->
            splitWithSuffixSeq "\r\n" inh
        , mkBench "S.splitOnSeq \"aa\" FL.drain" env $ \inh _ ->
            splitOnSeq "aa" inh
        , mkBench "S.splitOnSeq \"aaaa\" FL.drain" env $ \inh _ ->
            splitOnSeq "aaaa" inh
        , mkBench "S.splitOnSeq \"abcdefgh\" FL.drain" env $ \inh _ ->
            splitOnSeq "abcdefgh" inh
        , mkBench "S.splitOnSeq \"abcdefghi\" FL.drain" env $ \inh _ ->
            splitOnSeq "abcdefghi" inh
        , mkBench "S.splitOnSeq \"catcatcatcatcat\" FL.drain" env $ \inh _ ->
            splitOnSeq "catcatcatcatcat" inh
        , mkBench "S.splitOnSeq \"abcdefghijklmnopqrstuvwxyz\" FL.drain"
            env $ \inh _ -> splitOnSeq "abcdefghijklmnopqrstuvwxyz" inh
        , mkBench "S.splitOnSeq 100k long pattern"
            env $ \inh _ -> splitOnSeq100k inh
        , mkBenchSmall "S.splitOnSuffixSeq \"abcdefghijklmnopqrstuvwxyz\" FL.drain"
            env $ \inh _ -> splitOnSuffixSeq "abcdefghijklmnopqrstuvwxyz" inh
        , mkBenchSmall "S.splitWithSuffixSeq \"abcdefghijklmnopqrstuvwxyz\" FL.drain"
            env $ \inh _ -> splitWithSuffixSeq "abcdefghijklmnopqrstuvwxyz" inh
        ]
    ]

-- | Split on a character sequence.
splitOnSeqUtf8 :: String -> Handle -> IO Int
splitOnSeqUtf8 str inh =
    (S.length $ IP.splitOnSeq (A.fromList str) FL.drain
        $ IP.fromStream
        $ IUS.decodeUtf8Arrays
        $ IFH.readChunks inh) -- >>= print

o_1_space_reduce_toChunks_split :: BenchEnv -> [Benchmark]
o_1_space_reduce_toChunks_split env =
    [ bgroup "split/toChunks"
        [ mkBenchSmall ("S.splitOnSeqUtf8 \"abcdefgh\" FL.drain "
            ++ ". US.decodeUtf8Arrays") env $ \inh _ ->
                splitOnSeqUtf8 "abcdefgh" inh
        , mkBenchSmall "S.splitOnSeqUtf8 \"abcdefghijklmnopqrstuvwxyz\" FL.drain"
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
