{-# LANGUAGE FlexibleContexts #-}

-- |
-- Module      : Main
-- Copyright   : (c) 2018 Composewell Technologies
--
-- License     : BSD3
-- Maintainer  : streamly@composewell.com

import Stream.Common (benchIOSrc, sourceUnfoldrM)
import Streamly.Data.Stream (Stream)
import Streamly.Internal.Data.Stream.Prelude (MonadAsync, Config)

import qualified Streamly.Data.Stream.Prelude as Stream

import Streamly.Benchmark.Common
import Test.Tasty.Bench

moduleName :: String
moduleName = "Data.Stream.Rate"

-------------------------------------------------------------------------------
-- Average Rate
-------------------------------------------------------------------------------

{-# INLINE rateNothing #-}
rateNothing :: MonadAsync m =>
    ((Config -> Config) -> Stream m Int -> Stream m Int)
    -> (Config -> Config) -> Int -> Int -> Stream m Int
rateNothing f cfg value = f (Stream.rate Nothing . cfg) . sourceUnfoldrM value

{-# INLINE avgRate #-}
avgRate :: MonadAsync m =>
    ((Config -> Config) -> Stream m Int -> Stream m Int)
    -> (Config -> Config) -> Int -> Double -> Int -> Stream m Int
avgRate f cfg value rt = f (Stream.avgRate rt . cfg) . sourceUnfoldrM value

{-
-- parEval should be maxThreads 1 anyway
{-# INLINE avgRateThreads1 #-}
avgRateThreads1 :: MonadAsync m =>
    ((Config -> Config) -> Stream m Int -> Stream m Int)
    -> (Config -> Config) -> Int -> Double -> Int -> Stream m Int
avgRateThreads1 f cfg value rt =
    f (Stream.maxThreads 1 . Stream.avgRate rt . cfg) . sourceUnfoldrM value
-}

{-# INLINE minRate #-}
minRate :: MonadAsync m =>
    ((Config -> Config) -> Stream m Int -> Stream m Int)
    -> (Config -> Config) -> Int -> Double -> Int -> Stream m Int
minRate f cfg value rt = f (Stream.minRate rt . cfg) . sourceUnfoldrM value

{-# INLINE maxRate #-}
maxRate :: MonadAsync m =>
    ((Config -> Config) -> Stream m Int -> Stream m Int)
    -> (Config -> Config) -> Int -> Double -> Int -> Stream m Int
maxRate f cfg value rt = f (Stream.minRate rt . cfg) . sourceUnfoldrM value

{-# INLINE constRate #-}
constRate :: MonadAsync m =>
    ((Config -> Config) -> Stream m Int -> Stream m Int)
    -> (Config -> Config) -> Int -> Double -> Int -> Stream m Int
constRate f cfg value rt = f (Stream.constRate rt . cfg) . sourceUnfoldrM value

-- XXX arbitrarily large rate should be the same as rate Nothing
-- XXX Add tests for multiworker cases as well - parMapM
o_1_space_async :: Int -> [Benchmark]
o_1_space_async value =
    [ bgroup
          "default/parEval"
          [ bgroup
                "avgRate"
                -- benchIO "unfoldr" $ toNull
                -- benchIOSrc "unfoldrM" (sourceUnfoldrM value)
                [ benchIOSrc "Nothing" $ rateNothing Stream.parEval id value
                , benchIOSrc "1M" $ avgRate Stream.parEval id value 1000000
                , benchIOSrc "3M" $ avgRate Stream.parEval id value 3000000
                -- , benchIOSrc "10M/maxThreads1" $ avgRateThreads1 Stream.parEval value 10000000
                , benchIOSrc "10M" $ avgRate Stream.parEval id value 10000000
                , benchIOSrc "20M" $ avgRate Stream.parEval id value 20000000
                ]
          , bgroup
                "minRate"
                [ benchIOSrc "1M" $ minRate Stream.parEval id value 1000000
                , benchIOSrc "10M" $ minRate Stream.parEval id value 10000000
                , benchIOSrc "20M" $ minRate Stream.parEval id value 20000000
                ]
          , bgroup
                "maxRate"
                [ -- benchIOSrc "10K" $ maxRate value 10000
                  benchIOSrc "10M" $ maxRate Stream.parEval id value 10000000
                ]
          , bgroup
                "constRate"
                [ -- benchIOSrc "10K" $ constRate value 10000
                  benchIOSrc "1M" $ constRate Stream.parEval id value 1000000
                , benchIOSrc "10M" $ constRate Stream.parEval id value 10000000
                ]
          ]
    ]

o_1_space_ahead :: Int -> [Benchmark]
o_1_space_ahead value =
    [ bgroup
        "ordered/parEval"
        [ benchIOSrc "avgRate/1M"
            $ avgRate Stream.parEval (Stream.ordered True) value 1000000
        , benchIOSrc "minRate/1M"
            $ minRate Stream.parEval (Stream.ordered True) value 1000000
        , benchIOSrc "maxRate/1M"
            $ maxRate Stream.parEval (Stream.ordered True) value 1000000
        , benchIOSrc "constRate/1M"
            $ constRate Stream.parEval (Stream.ordered True) value 1000000
        ]
    ]

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

main :: IO ()
main = runWithCLIOpts defaultStreamSize allBenchmarks

    where

    allBenchmarks value =
        [ bgroup (o_1_space_prefix moduleName)
        $ concat [o_1_space_async value, o_1_space_ahead value]]
