-- |
-- Module      : Main
-- Copyright   : (c) 2018 Composewell Technologies
--
-- License     : BSD3
-- Maintainer  : streamly@composewell.com

import Streamly.Prelude (asyncly, aheadly, maxThreads)
import qualified Streamly.Prelude as S

import Streamly.Benchmark.Common
import Streamly.Benchmark.Prelude

import Gauge

moduleName :: String
moduleName = "Prelude.Rate"

-------------------------------------------------------------------------------
-- Average Rate
-------------------------------------------------------------------------------

-- XXX arbitrarily large rate should be the same as rate Nothing
o_1_space_async_avgRate :: Int -> [Benchmark]
o_1_space_async_avgRate value =
    [ bgroup "asyncly"
        [ bgroup "avgRate"
            -- benchIO "unfoldr" $ toNull asyncly
            -- benchIOSrc asyncly "unfoldrM" (sourceUnfoldrM value)
            [ benchIOSrc asyncly "unfoldrM/Nothing"
                (S.rate Nothing . sourceUnfoldrM value)
            , benchIOSrc asyncly "unfoldrM/1,000,000"
                (S.avgRate 1000000 . sourceUnfoldrM value)
            , benchIOSrc asyncly "unfoldrM/3,000,000"
                (S.avgRate 3000000 . sourceUnfoldrM value)
            , benchIOSrc asyncly "unfoldrM/10,000,000/maxThreads1"
                (maxThreads 1 .  S.avgRate 10000000 . sourceUnfoldrM value)
            , benchIOSrc asyncly "unfoldrM/10,000,000"
                (S.avgRate 10000000 . sourceUnfoldrM value)
            , benchIOSrc asyncly "unfoldrM/20,000,000"
                (S.avgRate 20000000 . sourceUnfoldrM value)
            ]
        ]
    ]

o_1_space_ahead_avgRate :: Int -> [Benchmark]
o_1_space_ahead_avgRate value =
    [ bgroup "aheadly"
        [ bgroup "avgRate"
            [ benchIOSrc aheadly "unfoldrM/1,000,000"
                  (S.avgRate 1000000 . sourceUnfoldrM value)
            ]
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
        [ bgroup (o_1_space_prefix moduleName) $ concat
            [ o_1_space_async_avgRate value
            , o_1_space_ahead_avgRate value
            ]
        ]
