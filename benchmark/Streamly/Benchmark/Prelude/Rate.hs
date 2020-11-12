{-# LANGUAGE FlexibleContexts #-}

-- |
-- Module      : Main
-- Copyright   : (c) 2018 Composewell Technologies
--
-- License     : BSD3
-- Maintainer  : streamly@composewell.com

import Streamly.Prelude (asyncly, aheadly, maxThreads, IsStream, MonadAsync)
import qualified Streamly.Prelude as S

import Streamly.Benchmark.Common
import Streamly.Benchmark.Prelude

import Gauge

moduleName :: String
moduleName = "Prelude.Rate"

-------------------------------------------------------------------------------
-- Average Rate
-------------------------------------------------------------------------------

{-# INLINE rateNothing #-}
rateNothing :: (MonadAsync m, IsStream t) => Int -> Int -> t m Int
rateNothing value = S.rate Nothing . sourceUnfoldrM value

{-# INLINE avgRate #-}
avgRate :: (MonadAsync m, IsStream t) => Int -> Double -> Int -> t m Int
avgRate value rate_ = S.avgRate rate_ . sourceUnfoldrM value

{-# INLINE avgRateThreads1 #-}
avgRateThreads1 :: (MonadAsync m, IsStream t) => Int -> Double -> Int -> t m Int
avgRateThreads1 value rate_ =
    maxThreads 1 . S.avgRate rate_ . sourceUnfoldrM value

-- XXX arbitrarily large rate should be the same as rate Nothing
o_1_space_async_avgRate :: Int -> [Benchmark]
o_1_space_async_avgRate value =
    [ bgroup
          "asyncly"
          [ bgroup
                "avgRate"
                -- benchIO "unfoldr" $ toNull asyncly
                -- benchIOSrc asyncly "unfoldrM" (sourceUnfoldrM value)
                [ benchIOSrc asyncly "Nothing" $ rateNothing value
                , benchIOSrc asyncly "1M" $ avgRate value 1000000
                , benchIOSrc asyncly "3M" $ avgRate value 3000000
                , benchIOSrc asyncly "10M/maxThreads1"
                      $ avgRateThreads1 value 10000000
                , benchIOSrc asyncly "10M" $ avgRate value 10000000
                , benchIOSrc asyncly "20M" $ avgRate value 20000000
                ]
          ]
    ]

o_1_space_ahead_avgRate :: Int -> [Benchmark]
o_1_space_ahead_avgRate value =
    [ bgroup
          "aheadly"
          [bgroup "avgRate" [benchIOSrc aheadly "1M" $ avgRate value 1000000]]
    ]

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

main :: IO ()
main = do
    (value, cfg, benches) <- parseCLIOpts defaultStreamSize
    value `seq` runMode (mode cfg) cfg benches (allBenchmarks value)

    where

    allBenchmarks value =
        [ bgroup (o_1_space_prefix moduleName)
              $ concat
                    [ o_1_space_async_avgRate value
                    , o_1_space_ahead_avgRate value
                    ]
        ]
