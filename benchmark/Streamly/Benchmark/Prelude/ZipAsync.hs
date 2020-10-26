-- |
-- Module      : Main
-- Copyright   : (c) 2018 Composewell Technologies
--
-- License     : BSD3
-- Maintainer  : streamly@composewell.com

{-# LANGUAGE FlexibleContexts #-}

import Streamly.Prelude (serially)
import qualified Streamly.Prelude  as S

import Streamly.Benchmark.Common
import Streamly.Benchmark.Prelude

import Gauge

moduleName :: String
moduleName = "Prelude.ZipAsync"

-------------------------------------------------------------------------------
-- Zipping
-------------------------------------------------------------------------------

{-# INLINE zipAsync #-}
zipAsync :: (S.IsStream t, S.MonadAsync m) => Int -> Int -> t m (Int, Int)
zipAsync count n =
    S.zipAsyncWith (,) (sourceUnfoldrM count n) (sourceUnfoldrM count (n + 1))

{-# INLINE zipAsyncM #-}
zipAsyncM :: (S.IsStream t, S.MonadAsync m) => Int -> Int -> t m (Int, Int)
zipAsyncM count n =
    S.zipAsyncWithM
        (curry return)
        (sourceUnfoldrM count n)
        (sourceUnfoldrM count (n + 1))

{-# INLINE zipAsyncAp #-}
zipAsyncAp :: (S.IsStream t, S.MonadAsync m) => Int -> Int -> t m (Int, Int)
zipAsyncAp count n =
    S.zipAsyncly $
        (,) <$> sourceUnfoldrM count n <*> sourceUnfoldrM count (n + 1)

o_1_space_joining :: Int -> [Benchmark]
o_1_space_joining value =
    [ bgroup "joining"
        [ benchIOSrc serially "zipAsync (2,x/2)" (zipAsync (value `div` 2))
        , benchIOSrc serially "zipAsyncM (2,x/2)" (zipAsyncM (value `div` 2))
        , benchIOSrc serially "zipAsyncAp (2,x/2)" (zipAsyncAp (value `div` 2))
        , benchIOSink value "fmap zipAsyncly" $ fmapN S.zipAsyncly 1
        ]
    ]

-------------------------------------------------------------------------------
-- Monad outer product
-------------------------------------------------------------------------------

o_1_space_outerProduct :: Int -> [Benchmark]
o_1_space_outerProduct value =
    [ bgroup "monad-outer-product"
        [ benchIO "toNullAp" $ toNullAp value S.zipAsyncly
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

    allBenchmarks size =
        [ bgroup (o_1_space_prefix moduleName) $ concat
            [ o_1_space_joining size
            , o_1_space_outerProduct size
            ]
        ]
