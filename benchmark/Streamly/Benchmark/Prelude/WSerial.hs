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

import Streamly.Prelude (wSerial, fromWSerial)
import qualified Streamly.Prelude as S
import qualified Streamly.Internal.Data.Stream.IsStream as Internal
import qualified Streamly.Internal.Data.Unfold as UF

import Streamly.Benchmark.Common
import Streamly.Benchmark.Prelude

import Test.Tasty.Bench

#ifdef INSPECTION
import GHC.Types (SPEC(..))
import Test.Inspection

import qualified Streamly.Internal.Data.Stream as D
#endif

moduleName :: String
moduleName = "Prelude.WSerial"

-------------------------------------------------------------------------------
-- Mapping
-------------------------------------------------------------------------------

o_1_space_mapping :: Int -> [Benchmark]
o_1_space_mapping value =
    [ bgroup "mapping"
        [ benchIOSink value "fmap" $ fmapN fromWSerial 1 ]
    ]

-------------------------------------------------------------------------------
-- Interleaving
-------------------------------------------------------------------------------

{-# INLINE wSerial2 #-}
wSerial2 :: Int -> Int -> IO ()
wSerial2 value n =
    S.drain $ wSerial
        (sourceUnfoldrM (value `div` 2) n)
        (sourceUnfoldrM (value `div` 2) (n + 1))

{-# INLINE interleave2 #-}
interleave2 :: Int -> Int -> IO ()
interleave2 value n =
    S.drain $
    Internal.interleave
        (sourceUnfoldrM (value `div` 2) n)
        (sourceUnfoldrM (value `div` 2) (n + 1))

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'interleave2
inspect $ 'interleave2 `hasNoType` ''SPEC
inspect $ 'interleave2 `hasNoType` ''D.InterleaveState
#endif

{-# INLINE roundRobin2 #-}
roundRobin2 :: Int -> Int -> IO ()
roundRobin2 value n =
    S.drain $
    Internal.roundrobin
        (sourceUnfoldrM (value `div` 2) n)
        (sourceUnfoldrM (value `div` 2) (n + 1))

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'roundRobin2
inspect $ 'roundRobin2 `hasNoType` ''SPEC
inspect $ 'roundRobin2 `hasNoType` ''D.InterleaveState
#endif

{-# INLINE sourceUnfoldrMUF #-}
-- (count, value)
sourceUnfoldrMUF :: Monad m => Int -> UF.Unfold m (Int, Int) Int
sourceUnfoldrMUF count = UF.unfoldrM step
    where
    step (cnt, start) =
        return $
            if cnt > start + count
            then Nothing
            else Just (cnt, (cnt + 1, start))

{-# INLINE unfoldManyInterleave #-}
unfoldManyInterleave :: Int -> Int -> Int -> IO ()
unfoldManyInterleave outer inner n =
    S.drain $ Internal.unfoldManyInterleave
        -- (UF.lmap return (UF.replicateM inner))
        (UF.lmap (\x -> (x,x)) (sourceUnfoldrMUF inner))
        (sourceUnfoldrM outer n)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'unfoldManyInterleave
-- inspect $ 'unfoldManyInterleave `hasNoType` ''SPEC
-- inspect $ 'unfoldManyInterleave `hasNoType`
--      ''D.ConcatUnfoldInterleaveState
#endif

{-# INLINE unfoldManyRoundRobin #-}
unfoldManyRoundRobin :: Int -> Int -> Int -> IO ()
unfoldManyRoundRobin outer inner n =
    S.drain $ Internal.unfoldManyRoundRobin
        -- (UF.lmap return (UF.replicateM inner))
        (UF.lmap (\x -> (x,x)) (sourceUnfoldrMUF inner))
        (sourceUnfoldrM outer n)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'unfoldManyRoundRobin
-- inspect $ 'unfoldManyRoundRobin `hasNoType` ''SPEC
-- inspect $ 'unfoldManyRoundRobin `hasNoType`
--      ''D.ConcatUnfoldInterleaveState
#endif

{-# INLINE concatPairsWithWSerial #-}
concatPairsWithWSerial :: Int -> Int -> Int -> IO ()
concatPairsWithWSerial = concatPairsWith Internal.wSerial

{-# INLINE concatPairsWithRoundrobin #-}
concatPairsWithRoundrobin :: Int -> Int -> Int -> IO ()
concatPairsWithRoundrobin = concatPairsWith Internal.roundrobin

{-# INLINE concatMapWithWSerial #-}
concatMapWithWSerial :: Int -> Int -> Int -> IO ()
concatMapWithWSerial = concatStreamsWith wSerial

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'concatMapWithWSerial
inspect $ 'concatMapWithWSerial `hasNoType` ''SPEC
#endif

o_1_space_joining :: Int -> [Benchmark]
o_1_space_joining value =
    [ bgroup "joining (2 of n/2)"
        [ benchIOSrc1 "wSerial" (wSerial2 value)
        , benchIOSrc1 "interleave" (interleave2 value)
        , benchIOSrc1 "roundRobin" (roundRobin2 value)
        , benchIOSrc1
              "concatMapWithWSerial"
              (concatMapWithWSerial 2 (value `div` 2))
        , benchIOSrc1
              "concatMapWithInterleave"
              (concatStreamsWith Internal.interleave 2 (value `div` 2))
        , benchIOSrc1
              "concatMapWithRoundrobin"
              (concatStreamsWith Internal.roundrobin 2 (value `div` 2))
        , benchIOSrc1
              "unfoldManyInterleave"
              (unfoldManyInterleave 2 (value `div` 2))
        , benchIOSrc1
            "concatPairsWithWSerial"
            (concatPairsWithWSerial 2 (value `div` 2))
        , benchIOSrc1
            "concatPairsWithRoundrobin"
            (concatPairsWithRoundrobin 2 (value `div` 2))
        ]
    ]

-------------------------------------------------------------------------------
-- Concat
-------------------------------------------------------------------------------

o_1_space_concat :: Int -> [Benchmark]
o_1_space_concat value =
    [ bgroup "concatMapWith"
        [ benchIOSrc1
            "concatMapWithWSerial (n of 1)"
            (concatStreamsWith wSerial value 1)
        , benchIOSrc1
            "concatMapWithWSerial (sqrtVal of sqrtVal)"
            (concatStreamsWith wSerial sqrtVal sqrtVal)
        ]
    ]

    where

    sqrtVal = round $ sqrt (fromIntegral value :: Double)

o_n_space_concat :: Int -> [Benchmark]
o_n_space_concat value =
    [ bgroup "concatMapWith"
        [
        -- concatMapWith using StreamD versions of interleave operations are
        -- all quadratic, we just measure the sqrtVal benchmark for comparison.
          benchIOSrc1
            "concatMapWithInterleave (sqrtVal of 1)"
            (concatStreamsWith Internal.interleave sqrtVal 1)
        , benchIOSrc1
            "concatMapWithInterleave (sqrtVal of sqrtVal)"
            (concatStreamsWith Internal.interleave sqrtVal sqrtVal)
        , benchIOSrc1
            "concatMapWithRoundrobin (sqrtVal of sqrtVal)"
            (concatStreamsWith Internal.roundrobin sqrtVal sqrtVal)
        ]
    ]

    where

    sqrtVal = round $ sqrt (fromIntegral value :: Double)

{-# INLINE concatPairsWithInterleave #-}
concatPairsWithInterleave :: Int -> Int -> Int -> IO ()
concatPairsWithInterleave = concatPairsWith Internal.interleave

{-# INLINE concatPairsWithInterleaveSuffix #-}
concatPairsWithInterleaveSuffix :: Int -> Int -> Int -> IO ()
concatPairsWithInterleaveSuffix = concatPairsWith Internal.interleaveSuffix

{-# INLINE concatPairsWithInterleaveInfix #-}
concatPairsWithInterleaveInfix :: Int -> Int -> Int -> IO ()
concatPairsWithInterleaveInfix = concatPairsWith Internal.interleaveInfix

{-# INLINE concatPairsWithInterleaveMin #-}
concatPairsWithInterleaveMin :: Int -> Int -> Int -> IO ()
concatPairsWithInterleaveMin = concatPairsWith Internal.interleaveMin

o_n_heap_concat :: Int -> [Benchmark]
o_n_heap_concat value =
    [ bgroup "concatPairsWith"
        [
          benchIOSrc1
              "unfoldManyInterleave (n of 1)"
              (unfoldManyInterleave value 1)
        , benchIOSrc1
              "unfoldManyInterleave (sqrtVal of sqrtVal)"
              (unfoldManyInterleave sqrtVal sqrtVal)

        , benchIOSrc1
              "unfoldManyRoundRobin (n of 1)"
              (unfoldManyRoundRobin value 1)
        , benchIOSrc1
              "unfoldManyRoundRobin (sqrtVal of sqrtVal)"
              (unfoldManyRoundRobin sqrtVal sqrtVal)

        , benchIOSrc1
            "concatPairsWithWSerial (n of 1)"
            (concatPairsWithWSerial value 1)
        , benchIOSrc1
            "concatPairsWithWSerial (sqrtVal of sqrtVal)"
            (concatPairsWithWSerial sqrtVal sqrtVal)

        , benchIOSrc1
            "concatPairsWithInterleave (n of 1)"
            (concatPairsWithInterleave value 1)
        , benchIOSrc1
            "concatPairsWithInterleave (sqrtVal of sqrtVal)"
            (concatPairsWithInterleave sqrtVal sqrtVal)

        , benchIOSrc1
            "concatPairsWithInterleaveSuffix (n of 1)"
            (concatPairsWithInterleaveSuffix value 1)
        , benchIOSrc1
            "concatPairsWithInterleaveSuffix (sqrtVal of sqrtVal)"
            (concatPairsWithInterleaveSuffix sqrtVal sqrtVal)

        , benchIOSrc1
            "concatPairsWithInterleaveInfix (n of 1)"
            (concatPairsWithInterleaveInfix value 1)
        , benchIOSrc1
            "concatPairsWithInterleaveInfix (sqrtVal of sqrtVal)"
            (concatPairsWithInterleaveInfix sqrtVal sqrtVal)

        , benchIOSrc1
            "concatPairsWithInterleaveMin (n of 1)"
            (concatPairsWithInterleaveMin value 1)
        , benchIOSrc1
            "concatPairsWithInterleaveMin (sqrtVal of sqrtVal)"
            (concatPairsWithInterleaveMin sqrtVal sqrtVal)

        , benchIOSrc1
            "concatPairsWithRoundrobin (n of 1)"
            (concatPairsWithRoundrobin value 1)
        , benchIOSrc1
            "concatPairsWithRoundrobin (sqrtVal of sqrtVal)"
            (concatPairsWithRoundrobin sqrtVal sqrtVal)
        ]
    ]

    where

    sqrtVal = round $ sqrt (fromIntegral value :: Double)

-------------------------------------------------------------------------------
-- Monad
-------------------------------------------------------------------------------

o_1_space_outerProduct :: Int -> [Benchmark]
o_1_space_outerProduct value =
    [ bgroup "outer-product"
        [ benchIO
            "drain applicative (+) (sqrtVal of sqrtVal)"
            (toNullAp value fromWSerial)
        , benchIO
            "drain monad (+) (sqrtVal of sqrtVal)"
            (toNullM value fromWSerial)
        , benchIO
            "drain monad (+) (cbrtVal of cbrtVal of cbrtVal)"
            (toNullM3 value fromWSerial)
        , benchIO
            "filterAllOut monad (+) (sqrtVal of sqrtVal)"
            (filterAllOutM value fromWSerial)
        , benchIO
            "filterAllIn monad (+) (sqrtVal of sqrtVal)"
            (filterAllInM value fromWSerial)
        , benchIO
            "filterSome monad (+) (sqrtVal of sqrtVal)"
            (filterSome value fromWSerial)
        , benchIO
            "breakAfterSome monad (+) (sqrtVal of sqrtVal)"
            (breakAfterSome value fromWSerial)
        ]
    ]

o_n_space_outerProduct :: Int -> [Benchmark]
o_n_space_outerProduct value =
    [ bgroup
        "outer-product"
        [ benchIO
            "toList monad (+) (sqrtVal of sqrtVal)"
            (toListM value fromWSerial)
        , benchIO
            "toListSome monad (+) (sqrtVal of sqrtVal)"
            (toListSome value fromWSerial)
        ]
    ]

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
            [ o_1_space_mapping size
            , o_1_space_joining size
            , o_1_space_concat size
            , o_1_space_outerProduct size
            ]
        , bgroup (o_n_heap_prefix moduleName) (o_n_heap_concat size)
        , bgroup (o_n_space_prefix moduleName) $
            o_n_space_outerProduct size ++ o_n_space_concat size
        ]
