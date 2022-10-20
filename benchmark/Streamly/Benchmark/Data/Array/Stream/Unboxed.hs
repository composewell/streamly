-- |
-- Module      : Streamly.Benchmark.Data.ParserD
-- Copyright   : (c) 2020 Composewell Technologies
--
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

#ifdef __HADDOCK_VERSION__
#undef INSPECTION
#endif

#ifdef INSPECTION
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fplugin Test.Inspection.Plugin #-}
#endif

module Main
  (
    main
  ) where

import Control.DeepSeq (NFData(..))
import Control.Monad (void, when)
import Control.Monad.Catch (MonadCatch, try, SomeException)
import Data.Functor.Identity (runIdentity)
import Data.Maybe (isJust)
import Data.Word (Word8)
import Streamly.Internal.Data.Stream (Stream)
import System.IO (Handle)
import System.Random (randomRIO)
import Prelude hiding ()

import qualified Streamly.Internal.Data.Array.Unboxed as Array
import qualified Streamly.Internal.Data.Array.Unboxed.Stream as ArrayStream
import qualified Streamly.Internal.Data.Fold as Fold
import qualified Streamly.Internal.Data.Parser as Parser
import qualified Streamly.Internal.Data.Stream as S
import qualified Streamly.Internal.Data.Stream.IsStream as Stream
import qualified Streamly.Internal.FileSystem.Handle as Handle
import qualified Streamly.Internal.Unicode.Stream as Unicode

import Gauge hiding (env)
import Streamly.Benchmark.Common
import Streamly.Benchmark.Common.Handle
import Control.Monad.IO.Class (MonadIO)

#ifdef INSPECTION
import Streamly.Internal.Data.Unboxed (Unboxed)
import Streamly.Internal.Data.Stream.StreamD.Type (Step(..))
import Test.Inspection
#endif

-------------------------------------------------------------------------------
-- Utilities
-------------------------------------------------------------------------------

-- XXX these can be moved to the common module

{-# INLINE sourceUnfoldrM #-}
sourceUnfoldrM :: MonadIO m => Int -> Int -> Stream m Int
sourceUnfoldrM value n = S.unfoldrM step n
    where
    step cnt =
        if cnt > n + value
        then return Nothing
        else return (Just (cnt, cnt + 1))

{-# INLINE benchIO #-}
benchIO
    :: NFData b
    => String -> (Int -> Stream IO a) -> (Stream IO a -> IO b) -> Benchmark
benchIO name src sink =
    bench name $ nfIO $ randomRIO (1,1) >>= sink . src

-------------------------------------------------------------------------------
-- read chunked using toChunks
-------------------------------------------------------------------------------

-- | Get the last byte from a file bytestream.
toChunksLast :: Handle -> IO (Maybe Word8)
toChunksLast inh = do
    let s = Handle.readChunks inh
    larr <- Stream.last s
    return $ case larr of
        Nothing -> Nothing
        Just arr -> Array.getIndex (Array.length arr - 1) arr

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'toChunksLast
inspect $ 'toChunksLast `hasNoType` ''Step
#endif

-- | Count the number of bytes in a file.
toChunksSumLengths :: Handle -> IO Int
toChunksSumLengths inh =
    let s = Handle.readChunks inh
    in Stream.sum (Stream.map Array.length s)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'toChunksSumLengths
inspect $ 'toChunksSumLengths `hasNoType` ''Step
#endif

-- | Sum the bytes in a file.
toChunksCountBytes :: Handle -> IO Word8
toChunksCountBytes inh = do
    let foldlArr' f z = runIdentity . Stream.foldl' f z . Array.read
    let s = Handle.readChunks inh
    Stream.foldl' (\acc arr -> acc + foldlArr' (+) 0 arr) 0 s

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'toChunksCountBytes
inspect $ 'toChunksCountBytes `hasNoType` ''Step
#endif

toChunksDecodeUtf8Arrays :: Handle -> IO ()
toChunksDecodeUtf8Arrays =
   Stream.drain . Unicode.decodeUtf8Arrays . Handle.readChunks

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'toChunksDecodeUtf8Arrays
-- inspect $ 'toChunksDecodeUtf8ArraysLenient `hasNoType` ''Step
#endif

-------------------------------------------------------------------------------
-- Splitting
-------------------------------------------------------------------------------

-- | Count the number of lines in a file.
toChunksSplitOnSuffix :: Handle -> IO Int
toChunksSplitOnSuffix =
    Stream.length . ArrayStream.splitOnSuffix 10 . Handle.readChunks

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'toChunksSplitOnSuffix
inspect $ 'toChunksSplitOnSuffix `hasNoType` ''Step
#endif

-- XXX use a word splitting combinator instead of splitOn and test it.
-- | Count the number of words in a file.
toChunksSplitOn :: Handle -> IO Int
toChunksSplitOn = Stream.length . ArrayStream.splitOn 32 . Handle.readChunks

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'toChunksSplitOn
inspect $ 'toChunksSplitOn `hasNoType` ''Step
#endif

o_1_space_read_chunked :: BenchEnv -> [Benchmark]
o_1_space_read_chunked env =
    -- read using toChunks instead of read
    [ bgroup "reduce/toChunks"
        [ mkBench "Stream.last" env $ \inH _ ->
            toChunksLast inH
        -- Note: this cannot be fairly compared with GNU wc -c or wc -m as
        -- wc uses lseek to just determine the file size rather than reading
        -- and counting characters.
        , mkBench "Stream.sum . Stream.map Array.length" env $ \inH _ ->
            toChunksSumLengths inH
        , mkBench "splitOnSuffix" env $ \inH _ ->
            toChunksSplitOnSuffix inH
        , mkBench "splitOn" env $ \inH _ ->
            toChunksSplitOn inH
        , mkBench "countBytes" env $ \inH _ ->
            toChunksCountBytes inH
        , mkBenchSmall "decodeUtf8Arrays" env $ \inH _ ->
            toChunksDecodeUtf8Arrays inH
        ]
    ]

-------------------------------------------------------------------------------
-- copy with group/ungroup transformations
-------------------------------------------------------------------------------

-- | Lines and unlines
{-# NOINLINE copyChunksSplitInterposeSuffix #-}
copyChunksSplitInterposeSuffix :: Handle -> Handle -> IO ()
copyChunksSplitInterposeSuffix inh outh =
    Stream.fold (Handle.write outh)
        $ ArrayStream.interposeSuffix 10
        $ ArrayStream.splitOnSuffix 10
        $ Handle.readChunks inh

#ifdef INSPECTION
inspect $ hasNoTypeClassesExcept 'copyChunksSplitInterposeSuffix [''Unboxed]
inspect $ 'copyChunksSplitInterposeSuffix `hasNoType` ''Step
#endif

-- | Words and unwords
{-# NOINLINE copyChunksSplitInterpose #-}
copyChunksSplitInterpose :: Handle -> Handle -> IO ()
copyChunksSplitInterpose inh outh =
    Stream.fold (Handle.write outh)
        $ ArrayStream.interpose 32
        -- XXX this is not correct word splitting combinator
        $ ArrayStream.splitOn 32
        $ Handle.readChunks inh

#ifdef INSPECTION
inspect $ hasNoTypeClassesExcept 'copyChunksSplitInterpose [''Unboxed]
inspect $ 'copyChunksSplitInterpose `hasNoType` ''Step
#endif

o_1_space_copy_toChunks_group_ungroup :: BenchEnv -> [Benchmark]
o_1_space_copy_toChunks_group_ungroup env =
    [ bgroup "copy/toChunks/group-ungroup"
        [ mkBench "interposeSuffix . splitOnSuffix" env $ \inh outh ->
            copyChunksSplitInterposeSuffix inh outh
        , mkBenchSmall "interpose . splitOn" env $ \inh outh ->
            copyChunksSplitInterpose inh outh
        ]
    ]

-------------------------------------------------------------------------------
-- Parsers
-------------------------------------------------------------------------------

{-# INLINE drainWhile #-}
drainWhile :: MonadCatch m => (a -> Bool) -> Parser.Parser m a ()
drainWhile p = Parser.takeWhile p Fold.drain

-------------------------------------------------------------------------------
-- Folds and parsers
-------------------------------------------------------------------------------

{-# INLINE fold #-}
fold :: Stream IO (Array.Array Int) -> IO ()
fold s = void $ ArrayStream.foldBreak Fold.drain s

{-# INLINE parse #-}
parse :: Int -> Stream IO (Array.Array Int) -> IO ()
parse value s = void $ ArrayStream.parseBreak (drainWhile (< value)) s

{-# INLINE foldBreak #-}
foldBreak :: Stream IO (Array.Array Int) -> IO ()
foldBreak s = do
    (r, s1) <- ArrayStream.foldBreak Fold.one s
    when (isJust r) $ foldBreak s1

{-# INLINE parseBreak #-}
parseBreak :: Stream IO (Array.Array Int) -> IO ()
parseBreak s = do
    r <- try $ ArrayStream.parseBreak Parser.one s
    case r of
        Left (_ :: SomeException) -> return ()
        Right (_, s1) -> parseBreak s1

o_1_space_serial_array ::
    Int -> [Array.Array Int] -> [Array.Array Int] -> [Benchmark]
o_1_space_serial_array bound arraysSmall arraysBig =
    [ benchIO "fold (of 100)" (\_ -> Stream.fromList arraysSmall) fold
    , benchIO "fold (single)" (\_ -> Stream.fromList arraysBig) fold
    , benchIO
        "foldBreak (recursive, small arrays)"
        (\_ -> Stream.fromList arraysSmall)
        foldBreak
    , benchIO "parse (of 100)" (\_ -> Stream.fromList arraysSmall)
        $ parse bound
    , benchIO "parse (single)" (\_ -> Stream.fromList arraysBig)
        $ parse bound
    , benchIO
        "parseBreak (recursive, small arrays)"
        (\_ -> Stream.fromList arraysSmall)
        parseBreak
    ]

-------------------------------------------------------------------------------
-- Driver
-------------------------------------------------------------------------------

moduleName :: String
moduleName = "Data.Array.Unboxed.Stream"

main :: IO ()
main = do
    env <- mkHandleBenchEnv
    runWithCLIOptsEnv defaultStreamSize alloc (allBenchmarks env)

    where

    alloc value = do
        small <- Stream.toList $ Stream.arraysOf 100 $ sourceUnfoldrM value 0
        big <- Stream.toList $ Stream.arraysOf value $ sourceUnfoldrM value 0
        return (small, big)

    allBenchmarks env arrays value =
        let (arraysSmall, arraysBig) = arrays
        in [ bgroup (o_1_space_prefix moduleName) $ Prelude.concat
          [ o_1_space_read_chunked env
          , o_1_space_serial_array value arraysSmall arraysBig
          , o_1_space_copy_toChunks_group_ungroup env
          ]
        ]
