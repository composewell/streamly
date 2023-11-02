{-# OPTIONS_GHC -Wno-deprecations #-}

-- |
-- Module      : Main
-- Copyright   : (c) 2018 Composewell Technologies
--
-- License     : BSD3
-- Maintainer  : streamly@composewell.com

{-# LANGUAGE FlexibleContexts #-}

import Streamly.Prelude (fromSerial)
import qualified Streamly.Prelude  as S

import Streamly.Benchmark.Common
import Streamly.Benchmark.Prelude

import Test.Tasty.Bench

moduleName :: String
moduleName = "Prelude.ZipAsync"

-------------------------------------------------------------------------------
-- Zipping
-------------------------------------------------------------------------------

{-# INLINE zipAsyncWith #-}
zipAsyncWith :: (S.IsStream t, S.MonadAsync m) => Int -> Int -> t m (Int, Int)
zipAsyncWith count n =
    S.zipAsyncWith (,) (sourceUnfoldrM count n) (sourceUnfoldrM count (n + 1))

{-# INLINE zipAsyncWithM #-}
zipAsyncWithM :: (S.IsStream t, S.MonadAsync m) => Int -> Int -> t m (Int, Int)
zipAsyncWithM count n =
    S.zipAsyncWithM
        (curry return)
        (sourceUnfoldrM count n)
        (sourceUnfoldrM count (n + 1))

{-# INLINE zipAsyncAp #-}
zipAsyncAp :: (S.IsStream t, S.MonadAsync m) => Int -> Int -> t m (Int, Int)
zipAsyncAp count n =
    S.fromZipAsync $
        (,) <$> sourceUnfoldrM count n <*> sourceUnfoldrM count (n + 1)

fromZipAsyncTraverse :: String -> Int -> Benchmark
fromZipAsyncTraverse name count =
    bench name
        $ nfIO
        $ S.drain $ S.fromZipAsync $ traverse S.fromPure [0 :: Int .. count]

o_1_space_joining :: Int -> [Benchmark]
o_1_space_joining value =
    [ bgroup "joining"
        [ benchIOSrc fromSerial "zipAsyncWith (2,x/2)" (zipAsyncWith
                                                      (value `div` 2))
        , benchIOSrc fromSerial "zipAsyncWithM (2,x/2)" (zipAsyncWithM
                                                       (value `div` 2))
        , benchIOSrc fromSerial "zipAsyncAp (2,x/2)" (zipAsyncAp (value `div` 2))
        , benchIOSink value "fmap zipAsyncly" $ fmapN S.fromZipAsync 1
        ]
    ]

o_n_heap_joining :: Int -> [Benchmark]
o_n_heap_joining value =
    [ bgroup "joining"
        [ fromZipAsyncTraverse "ZipAsync Applicative (x/100)" (value `div` 100)
        ]
    ]

-------------------------------------------------------------------------------
-- Monad outer product
-------------------------------------------------------------------------------

o_1_space_outerProduct :: Int -> [Benchmark]
o_1_space_outerProduct value =
    [ bgroup "monad-outer-product"
        [ benchIO "toNullAp" $ toNullAp value S.fromZipAsync
        ]
    ]

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

main :: IO ()
main = runWithCLIOpts defaultStreamSize allBenchmarks

    where

    allBenchmarks size =
        [ bgroup (o_1_space_prefix moduleName) $ concat
            [ o_1_space_joining size
            , o_1_space_outerProduct size
            ]
        , bgroup (o_n_heap_prefix moduleName) $ o_n_heap_joining size
        ]
