{-# OPTIONS_GHC -Wno-deprecations #-}

-- |
-- Module      : Main
-- Copyright   : (c) 2018 Composewell Technologies
--
-- License     : BSD3
-- Maintainer  : streamly@composewell.com

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

#ifdef __HADDOCK_VERSION__
#undef INSPECTION
#endif

#ifdef INSPECTION
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fplugin Test.Inspection.Plugin #-}
#endif

import Prelude hiding (zipWith)

import Streamly.Prelude (MonadAsync)
import qualified Streamly.Prelude  as S

import Streamly.Benchmark.Common
import Streamly.Benchmark.Prelude hiding (sourceUnfoldrM)

import Test.Tasty.Bench

#ifdef INSPECTION
import GHC.Types (SPEC(..))
import Test.Inspection

import qualified Streamly.Internal.Data.Stream as D
#endif

moduleName :: String
moduleName = "Prelude.ZipSerial"

-------------------------------------------------------------------------------
-- Zipping
-------------------------------------------------------------------------------

-- XXX somehow copying this definition here instead of importing it performs
-- better. Need to investigate why.
{-# INLINE sourceUnfoldrM #-}
sourceUnfoldrM :: (S.IsStream t, MonadAsync m) => Int -> Int -> t m Int
sourceUnfoldrM count start = S.unfoldrM step start
    where
    step cnt =
        if cnt > start + count
        then return Nothing
        else return (Just (cnt, cnt + 1))

{-# INLINE zipWith #-}
zipWith :: Int -> Int -> IO ()
zipWith count n =
    S.drain $
    S.zipWith
        (,)
        (S.fromSerial $ sourceUnfoldrM count n)
        (S.fromSerial $ sourceUnfoldrM count (n + 1))

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'zipWith
inspect $ 'zipWith `hasNoType` ''SPEC
inspect $ 'zipWith `hasNoType` ''D.Step
#endif

{-# INLINE zipWithM #-}
zipWithM :: Int -> Int -> IO ()
zipWithM count n =
    S.drain $
    S.zipWithM
        (curry return)
        (sourceUnfoldrM count n)
        (sourceUnfoldrM count (n + 1))

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'zipWithM
inspect $ 'zipWithM `hasNoType` ''SPEC
inspect $ 'zipWithM `hasNoType` ''D.Step
#endif

{-
{-# INLINE concatMapWithZip #-}
concatMapWithZip :: Int -> Int -> Int -> IO ()
concatMapWithZip = concatStreamsWith (S.zipWith (+))
-}

{-# INLINE concatPairsWithZip #-}
concatPairsWithZip :: Int -> Int -> Int -> IO ()
concatPairsWithZip = concatPairsWith (S.zipWith (+))

o_1_space_joining :: Int -> [Benchmark]
o_1_space_joining value =
    [ bgroup "joining"
        [ benchIOSrc1 "zip (2 of n/2)" (zipWith (value `div` 2))
        , benchIOSrc1 "zipM (2 of n/2)" (zipWithM (value `div` 2))
        {-
        -- Not correct because of nil stream at end issue.
        , benchIOSrc1 "concatMapWithZip (+) (2 of n/2)"
            (concatMapWithZip 2 (value `div` 2))
        -}
        ]
    ]

-------------------------------------------------------------------------------
-- Mapping
-------------------------------------------------------------------------------

o_1_space_mapping :: Int -> [Benchmark]
o_1_space_mapping value =
    [ bgroup "mapping"
        [ benchIOSink value "fmap" $ fmapN S.fromZipSerial 1
        ]
    ]

-------------------------------------------------------------------------------
-- Concat
-------------------------------------------------------------------------------

o_n_heap_concat :: Int -> [Benchmark]
o_n_heap_concat value =
    [ bgroup "concatPairsWith"
        [ {- -- This fails with stack overflow.
            benchIOSrc1 "concatMapWithZip (n of 1)"
            (concatMapWithZip value 1)
        -- Not correct because of nil stream at end issue.
        , benchIOSrc1 "concatMapWithZip (sqrtVal of sqrtVal)"
            (concatMapWithZip sqrtVal sqrtVal)
        -}
          benchIOSrc1 "concatPairsWithZip (n of 1)"
            (concatPairsWithZip value 1)
        , benchIOSrc1 "concatPairsWithZip (sqrtVal of sqrtVal)"
            (concatPairsWithZip sqrtVal sqrtVal)
        , benchIOSrc1 "concatPairsWithZip (+) (2 of n/2)"
            (concatPairsWithZip 2 (value `div` 2))
        ]
    ]

    where

    sqrtVal = round $ sqrt (fromIntegral value :: Double)

-------------------------------------------------------------------------------
-- Monad outer product
-------------------------------------------------------------------------------

{-
-- Not correct because of nil stream at end issue.
o_1_space_outerProduct :: Int -> [Benchmark]
o_1_space_outerProduct value =
    [ bgroup "monad-outer-product"
        -- XXX needs fixing
        [ benchIO "toNullAp" $ toNullAp value S.zipSerially
        ]
    ]
-}

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

-- In addition to gauge options, the number of elements in the stream can be
-- passed using the --stream-size option.
--
main :: IO ()
main = runWithCLIOpts defaultStreamSize allBenchmarks

    where

    allBenchmarks size =
        [ bgroup (o_1_space_prefix moduleName) $ concat
            [ o_1_space_joining size
            , o_1_space_mapping size
            -- XXX need to fix, timing in ns
            -- , o_1_space_outerProduct size
            ]
        , bgroup (o_n_heap_prefix moduleName) $ concat
            [ o_n_heap_concat size
            ]
        ]
