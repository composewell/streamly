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

{-# INLINE minRate #-}
minRate :: (MonadAsync m, IsStream t) => Int -> Double -> Int -> t m Int
minRate value rate_ = S.minRate rate_ . sourceUnfoldrM value

{-# INLINE maxRate #-}
maxRate :: (MonadAsync m, IsStream t) => Int -> Double -> Int -> t m Int
maxRate value rate_ = S.minRate rate_ . sourceUnfoldrM value

{-# INLINE constRate #-}
constRate :: (MonadAsync m, IsStream t) => Int -> Double -> Int -> t m Int
constRate value rate_ = S.constRate rate_ . sourceUnfoldrM value

-- XXX arbitrarily large rate should be the same as rate Nothing
o_1_space_async :: Int -> [Benchmark]
o_1_space_async value =
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
          , bgroup
                "minRate"
                [ benchIOSrc asyncly "1M" $ minRate value 1000000
                , benchIOSrc asyncly "10M" $ minRate value 10000000
                , benchIOSrc asyncly "20M" $ minRate value 20000000
                ]
          , bgroup
                "maxRate"
                [ -- benchIOSrc asyncly "10K" $ maxRate value 10000
                  benchIOSrc asyncly "10M" $ maxRate value 10000000
                ]
          , bgroup
                "constRate"
                [ -- benchIOSrc asyncly "10K" $ constRate value 10000
                  benchIOSrc asyncly "1M" $ constRate value 1000000
                , benchIOSrc asyncly "10M" $ constRate value 10000000
                ]
          ]
    ]

o_1_space_ahead :: Int -> [Benchmark]
o_1_space_ahead value =
    [ bgroup
          "aheadly"
          [ benchIOSrc aheadly "avgRate/1M" $ avgRate value 1000000
          , benchIOSrc aheadly "minRate/1M" $ minRate value 1000000
          , benchIOSrc aheadly "maxRate/1M" $ maxRate value 1000000
          , benchIOSrc asyncly "constRate/1M" $ constRate value 1000000
          ]
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
              $ concat [o_1_space_async value, o_1_space_ahead value]
        ]
