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
import qualified Streamly.Internal.Data.Parser as Parser
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

-- | Split suffix on line feed.
splitOnSuffix :: Handle -> IO Int
splitOnSuffix inh =
    (Stream.fold Fold.length
        $ splitOnSuffix1 (== lf) Fold.drain
        $ Handle.read inh) -- >>= print

    where

    splitOnSuffix1 p f = Stream.foldMany (Fold.takeEndBy_ p f)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'splitOnSuffix
inspect $ 'splitOnSuffix `hasNoType` ''Step
inspect $ 'splitOnSuffix `hasNoType` ''Unfold.ConcatState -- FH.read/UF.many
inspect $ 'splitOnSuffix `hasNoType` ''MutArray.ArrayUnsafe  -- FH.read/A.read
#endif

-- | Split suffix with line feed.
splitWithSuffix :: Handle -> IO Int
splitWithSuffix inh =
    (Stream.fold Fold.length $ splitWithSuffix1 (== lf) Fold.drain
        $ Handle.read inh) -- >>= print

    where

    splitWithSuffix1 p f = Stream.foldMany (Fold.takeEndBy p f)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'splitWithSuffix
inspect $ 'splitWithSuffix `hasNoType` ''Step
inspect $ 'splitWithSuffix `hasNoType` ''Unfold.ConcatState -- FH.read/UF.many
inspect $ 'splitWithSuffix `hasNoType` ''MutArray.ArrayUnsafe  -- FH.read/A.read
#endif

-- | Split on line feed.
foldManySepBy :: Handle -> IO Int
foldManySepBy inh =
    (Stream.fold Fold.length
        $ Stream.foldMany
            (Fold.takeEndBy_ (== lf) Fold.drain)
            (Handle.read inh)
    ) -- >>= print

-- | Split on line feed.
parseManySepBy :: Handle -> IO Int
parseManySepBy inh =
    (Stream.fold Fold.length
        $ Stream.parseMany
            (Parser.fromFold $ Fold.takeEndBy_ (== lf) Fold.drain)
            (Handle.read inh)
    ) -- >>= print

-- | Words by space
wordsBy :: Handle -> IO Int
wordsBy inh =
    (Stream.fold Fold.length $ Stream.wordsBy isSp Fold.drain
        $ Handle.read inh) -- >>= print

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
        , mkBench "S.splitOnSeq empty-string FL.drain" env $ \inh _ ->
            splitOnSeq "" inh
        , mkBench "S.splitOnSuffixSeq empty-string FL.drain" env $ \inh _ ->
            splitOnSuffixSeq "" inh
        , mkBench "S.splitOnSeq lf FL.drain" env $ \inh _ ->
            splitOnSeq "\n" inh
        , mkBench "S.splitOnSuffixSeq lf FL.drain" env $ \inh _ ->
            splitOnSuffixSeq "\n" inh
        , mkBench "S.splitOnSeq a FL.drain" env $ \inh _ ->
            splitOnSeq "a" inh
        , mkBench "S.splitOnSeq crlf FL.drain" env $ \inh _ ->
            splitOnSeq "\r\n" inh
        , mkBench "S.splitOnSuffixSeq crlf FL.drain" env $ \inh _ ->
            splitOnSuffixSeq "\r\n" inh
        , mkBench "S.splitWithSuffixSeq crlf FL.drain" env $ \inh _ ->
            splitWithSuffixSeq "\r\n" inh
        , mkBench "S.splitOnSeq aa FL.drain" env $ \inh _ ->
            splitOnSeq "aa" inh
        , mkBench "S.splitOnSeq aaaa FL.drain" env $ \inh _ ->
            splitOnSeq "aaaa" inh
        , mkBench "S.splitOnSeq abcdefgh FL.drain" env $ \inh _ ->
            splitOnSeq "abcdefgh" inh
        , mkBench "S.splitOnSeq abcdefghi FL.drain" env $ \inh _ ->
            splitOnSeq "abcdefghi" inh
        , mkBench "S.splitOnSeq catcatcatcatcat FL.drain" env $ \inh _ ->
            splitOnSeq "catcatcatcatcat" inh
        , mkBench "S.splitOnSeq abcdefghijklmnopqrstuvwxyz FL.drain"
            env $ \inh _ -> splitOnSeq "abcdefghijklmnopqrstuvwxyz" inh
        , mkBench "S.splitOnSeq 100k long pattern"
            env $ \inh _ -> splitOnSeq100k inh
        , mkBenchSmall "S.splitOnSuffixSeq abcdefghijklmnopqrstuvwxyz FL.drain"
            env $ \inh _ -> splitOnSuffixSeq "abcdefghijklmnopqrstuvwxyz" inh
        , mkBenchSmall "S.splitWithSuffixSeq abcdefghijklmnopqrstuvwxyz FL.drain"
            env $ \inh _ -> splitWithSuffixSeq "abcdefghijklmnopqrstuvwxyz" inh
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
    [ bgroup "split/toChunks"
        [ mkBenchSmall ("S.splitOnSeqUtf8 abcdefgh FL.drain "
            ++ ". US.decodeUtf8Arrays") env $ \inh _ ->
                splitOnSeqUtf8 "abcdefgh" inh
        , mkBenchSmall "S.splitOnSeqUtf8 abcdefghijklmnopqrstuvwxyz FL.drain"
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
