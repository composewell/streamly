
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
import Data.Maybe (isJust)
import Streamly.Internal.Data.Stream (Stream)
import Streamly.Internal.Data.StreamK (StreamK)
import System.Random (randomRIO)
import Prelude hiding ()

import qualified Streamly.Data.Stream as Stream
import qualified Streamly.Internal.Data.Array as Array
import qualified Streamly.Internal.Data.Fold as Fold
import qualified Streamly.Internal.Data.Parser as Parser
import qualified Streamly.Internal.Data.StreamK as StreamK

import Test.Tasty.Bench hiding (env)
import Streamly.Benchmark.Common
import Streamly.Benchmark.Common.Handle
import Control.Monad.IO.Class (MonadIO)

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

    benchmarks _env arrays value =
        let (arraysSmall, arraysBig) = arrays
        in
          [ (SpaceO_1, benchIO "fold (of 100)" (\_ -> Stream.fromList arraysSmall) fold)
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
          ]

    allBenchmarks env arrays value =
        let allBenches = benchmarks env arrays value
            get x = map snd $ filter ((==) x . fst) allBenches
            o_1_space = get SpaceO_1
        in
        [ bgroup (o_1_space_prefix moduleName) o_1_space
        ]
