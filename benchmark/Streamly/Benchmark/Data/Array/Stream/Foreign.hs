-- |
-- Module      : Streamly.Benchmark.Data.ParserD
-- Copyright   : (c) 2020 Composewell Technologies
--
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fspec-constr-recursive=4 #-}

module Main
  (
    main
  ) where

import Control.DeepSeq (NFData(..))
import Control.Monad (void)
import Control.Monad.Catch (MonadThrow)
import System.Random (randomRIO)
import Prelude hiding ()

import qualified Streamly.Prelude  as Stream
import qualified Streamly.Internal.Data.Array.Foreign as Array
import qualified Streamly.Internal.Data.Array.Stream.Foreign as ArrayStream
import qualified Streamly.Internal.Data.Fold as Fold
import qualified Streamly.Internal.Data.Parser.ParserD as ParserD
import qualified Streamly.Internal.Data.Stream.IsStream as Stream (arraysOf)

import Gauge
import Streamly.Prelude (SerialT, MonadAsync, IsStream)
import Streamly.Benchmark.Common

-------------------------------------------------------------------------------
-- Utilities
-------------------------------------------------------------------------------

-- XXX these can be moved to the common module

{-# INLINE sourceUnfoldrM #-}
sourceUnfoldrM :: (IsStream t, MonadAsync m) => Int -> Int -> t m Int
sourceUnfoldrM value n = Stream.unfoldrM step n
    where
    step cnt =
        if cnt > n + value
        then return Nothing
        else return (Just (cnt, cnt + 1))

{-# INLINE benchIO #-}
benchIO
    :: NFData b
    => String -> (Int -> t IO a) -> (t IO a -> IO b) -> Benchmark
benchIO name src sink =
    bench name $ nfIO $ randomRIO (1,1) >>= sink . src

-------------------------------------------------------------------------------
-- Parsers
-------------------------------------------------------------------------------

{-# INLINE drainWhile #-}
drainWhile :: MonadThrow m => (a -> Bool) -> ParserD.Parser m a ()
drainWhile p = ParserD.takeWhile p Fold.drain

-------------------------------------------------------------------------------
-- Array parsers
-------------------------------------------------------------------------------

{-# INLINE parseArray #-}
parseArray :: Int -> SerialT IO (Array.Array Int) -> IO ()
parseArray value s = void $ ArrayStream.parse (drainWhile (< value)) s

o_1_space_serial_array ::
    Int -> [Array.Array Int] -> [Array.Array Int] -> [Benchmark]
o_1_space_serial_array bound arraysSmall arraysBig =
    [ benchIO "parseArray (100)" (\_ -> Stream.fromList arraysSmall)
        $ parseArray bound
    , benchIO "parseArray (bound)" (\_ -> Stream.fromList arraysBig)
        $ parseArray bound
    ]

-------------------------------------------------------------------------------
-- Driver
-------------------------------------------------------------------------------

moduleName :: String
moduleName = "Data.Array.Stream.Foreign"

main :: IO ()
main = do
    (value, cfg, benches) <- parseCLIOpts defaultStreamSize
    arraysSmall <- Stream.toList $ Stream.arraysOf 100 $ sourceUnfoldrM value 0
    arraysBig <- Stream.toList $ Stream.arraysOf value $ sourceUnfoldrM value 0
    value `seq` runMode (mode cfg) cfg benches
        (allBenchmarks value arraysSmall arraysBig)

    where

    allBenchmarks value arraysSmall arraysBig =
        [ bgroup (o_1_space_prefix moduleName)
            (o_1_space_serial_array value arraysSmall arraysBig)
        ]
