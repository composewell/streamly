
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
import Control.Monad.Catch (MonadCatch)
import Data.Functor.Identity (runIdentity)
import Data.Maybe (isJust)
import Data.Word (Word8)
import Streamly.Internal.Data.Stream (Stream)
import Streamly.Internal.Data.StreamK (StreamK)
import System.IO (Handle)
import System.Random (randomRIO)
import Prelude hiding ()

import qualified Streamly.Data.Stream as Stream
import qualified Streamly.Internal.Data.Array as Array
import qualified Streamly.Internal.Data.Fold as Fold
import qualified Streamly.Internal.Data.Parser as Parser
import qualified Streamly.Internal.Data.Stream as Stream
import qualified Streamly.Internal.Data.StreamK as StreamK
import qualified Streamly.Internal.FileSystem.Handle as Handle
import qualified Streamly.Internal.Unicode.Stream as Unicode

import Test.Tasty.Bench hiding (env)
import Streamly.Benchmark.Common
import Streamly.Benchmark.Common.Handle
import Control.Monad.IO.Class (MonadIO)

#ifdef INSPECTION
import Streamly.Internal.Data.MutByteArray (Unbox)
import Streamly.Internal.Data.Stream (Step(..))
import Test.Inspection
#endif

-------------------------------------------------------------------------------
-- Utilities
-------------------------------------------------------------------------------

-- XXX these can be moved to the common module

{-# INLINE sourceUnfoldrM #-}
sourceUnfoldrM :: MonadIO m => Int -> Int -> Stream.Stream m Int
sourceUnfoldrM value n = Stream.unfoldrM step n
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
    in Stream.fold Fold.sum (Stream.map Array.length s)

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
   Stream.drain . Unicode.decodeUtf8Chunks . Handle.readChunks

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
    Stream.fold Fold.length
        . Array.compactEndByByte_ 10
        . Handle.readChunks

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'toChunksSplitOnSuffix
inspect $ 'toChunksSplitOnSuffix `hasNoType` ''Step
#endif

-- XXX use a word splitting combinator instead of splitOn and test it.
-- | Count the number of words in a file.
toChunksSplitOn :: Handle -> IO Int
toChunksSplitOn =
    Stream.fold Fold.length
        . Array.compactSepByByte_ 32
        . Handle.readChunks

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'toChunksSplitOn
inspect $ 'toChunksSplitOn `hasNoType` ''Step
#endif


-------------------------------------------------------------------------------
-- copy with group/ungroup transformations
-------------------------------------------------------------------------------

-- | Lines and unlines
{-# NOINLINE copyChunksSplitInterposeSuffix #-}
copyChunksSplitInterposeSuffix :: Handle -> Handle -> IO ()
copyChunksSplitInterposeSuffix inh outh =
    Stream.fold (Handle.write outh)
        $ Array.concatEndBy 10 . Array.compactEndByByte_ 10
        $ Handle.readChunks inh

#ifdef INSPECTION
inspect $ hasNoTypeClassesExcept 'copyChunksSplitInterposeSuffix [''Unbox]
inspect $ 'copyChunksSplitInterposeSuffix `hasNoType` ''Step
#endif

-- | Words and unwords
{-# NOINLINE copyChunksSplitInterpose #-}
copyChunksSplitInterpose :: Handle -> Handle -> IO ()
copyChunksSplitInterpose inh outh =
    Stream.fold (Handle.write outh)
        -- XXX requires @-fspec-constr-recursive=12@.
        -- XXX this is not correct word splitting combinator
        $ Array.concatSepBy 32 . Array.compactSepByByte_ 32
        $ Handle.readChunks inh

#ifdef INSPECTION
inspect $ hasNoTypeClassesExcept 'copyChunksSplitInterpose [''Unbox]
inspect $ 'copyChunksSplitInterpose `hasNoType` ''Step
#endif


-------------------------------------------------------------------------------
-- Parsers
-------------------------------------------------------------------------------

{-# INLINE drainWhile #-}
drainWhile :: MonadCatch m => (a -> Bool) -> Parser.Parser a m ()
drainWhile p = Parser.takeWhile p Fold.drain

-------------------------------------------------------------------------------
-- Folds and parsers
-------------------------------------------------------------------------------

{-# INLINE fold #-}
fold :: Stream IO (Array.Array Int) -> IO ()
fold s = void $ Array.foldBreak Fold.drain $ StreamK.fromStream s

{-# INLINE parse #-}
parse :: Int -> Stream IO (Array.Array Int) -> IO ()
parse value s =
    void $ Array.parseBreak
            (Array.toParserK (drainWhile (< value)))
            (StreamK.fromStream s)

{-# INLINE foldBreak #-}
foldBreak :: StreamK IO (Array.Array Int) -> IO ()
foldBreak s = do
    (r, s1) <- Array.foldBreak Fold.one s
    when (isJust r) $ foldBreak s1

{-# INLINE parseBreak #-}
parseBreak :: StreamK IO (Array.Array Int) -> IO ()
parseBreak s = do
    r <- Array.parseBreak (Array.toParserK Parser.one) s
    case r of
        (Left _, _) -> return ()
        (Right _, s1) -> parseBreak s1


-------------------------------------------------------------------------------
-- Driver
-------------------------------------------------------------------------------

moduleName :: String
moduleName = "Data.Array.Stream"

main :: IO ()
main = do
    env <- mkHandleBenchEnv
    runWithCLIOptsEnv defaultStreamSize alloc (allBenchmarks env)

    where

    alloc value =
        if value <= 0
        then return  (undefined, undefined)
        else
            do
            small <- Stream.toList $ Array.chunksOf 100 $ sourceUnfoldrM value 0
            big <- Stream.toList $ Array.chunksOf value $ sourceUnfoldrM value 0
            return (small, big)

    benchmarks env arrays value =
        let (arraysSmall, arraysBig) = arrays
        in
        -- read using toChunks instead of read
          [ (SpaceO_1, mkBench "Stream.last" env $ \inH _ ->
                toChunksLast inH)
          -- Note: this cannot be fairly compared with GNU wc -c or wc -m as
          -- wc uses lseek to just determine the file size rather than reading
          -- and counting characters.
          , (SpaceO_1, mkBench "Stream.sum . Stream.map Array.length" env $ \inH _ ->
                toChunksSumLengths inH)
          , (SpaceO_1, mkBench "splitOnSuffix" env $ \inH _ ->
                toChunksSplitOnSuffix inH)
          , (SpaceO_1, mkBench "splitOn" env $ \inH _ ->
                toChunksSplitOn inH)
          , (SpaceO_1, mkBench "countBytes" env $ \inH _ ->
                toChunksCountBytes inH)
          , (SpaceO_1, mkBenchSmall "decodeUtf8Arrays" env $ \inH _ ->
                toChunksDecodeUtf8Arrays inH)

          , (SpaceO_1, benchIO "fold (of 100)" (\_ -> Stream.fromList arraysSmall) fold)
          , (SpaceO_1, benchIO "fold (single)" (\_ -> Stream.fromList arraysBig) fold)
          , (SpaceO_1, benchIO
                "foldBreak (recursive, small arrays)"
                (\_ -> Stream.fromList arraysSmall)
                (foldBreak . StreamK.fromStream))
          , (SpaceO_1, benchIO "parse (of 100)" (\_ -> Stream.fromList arraysSmall)
                $ parse value)
          , (SpaceO_1, benchIO "parse (single)" (\_ -> Stream.fromList arraysBig)
                $ parse value)
          , (SpaceO_1, benchIO
                "parseBreak (recursive, small arrays)"
                (\_ -> Stream.fromList arraysSmall)
                (parseBreak . StreamK.fromStream))

          , (SpaceO_1, mkBench "interposeSuffix . splitOnSuffix" env $ \inh outh ->
                copyChunksSplitInterposeSuffix inh outh)
          , (SpaceO_1, mkBenchSmall "interpose . splitOn" env $ \inh outh ->
                copyChunksSplitInterpose inh outh)
          ]

    allBenchmarks env arrays value =
        let allBenches = benchmarks env arrays value
            get x = map snd $ filter ((==) x . fst) allBenches
            o_1_space = get SpaceO_1
        in
        [ bgroup (o_1_space_prefix moduleName) o_1_space
        ]
