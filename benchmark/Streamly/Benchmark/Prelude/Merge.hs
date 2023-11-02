{-# OPTIONS_GHC -Wno-deprecations #-}

-- |
-- Module      : Main
-- Copyright   : (c) 2018 Composewell Technologies
--
-- License     : BSD3
-- Maintainer  : streamly@composewell.com

{-# LANGUAGE BangPatterns #-}
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

import Streamly.Internal.Data.Stream.IsStream (SerialT)
import qualified Data.List as List
import qualified Streamly.Internal.Data.Stream.IsStream as Stream
-- import qualified Streamly.Internal.Data.Unfold as Unfold

import Streamly.Benchmark.Common
import Streamly.Benchmark.Prelude

import Test.Tasty.Bench

#ifdef INSPECTION
import GHC.Types (SPEC(..))
import Test.Inspection

import qualified Streamly.Internal.Data.Stream as D
#endif

moduleName :: String
moduleName = "Prelude.Merge"

-------------------------------------------------------------------------------
-- Merging
-------------------------------------------------------------------------------

{-# INLINE mergeWith #-}
mergeWith ::
    (  (Int -> Int -> Ordering)
    -> SerialT IO Int
    -> SerialT IO Int
    -> SerialT IO Int
    )
    -> (Int -> Int -> Ordering)
    -> Int -> Int -> IO ()
mergeWith g cmp count n =
    Stream.drain
        $ g
            cmp
            (sourceUnfoldrM count n)
            (sourceUnfoldrM count (n + 1))

{-# INLINE mergeWithM #-}
mergeWithM ::
    (  (Int -> Int -> IO Ordering)
    -> SerialT IO Int
    -> SerialT IO Int
    -> SerialT IO Int
    )
    -> (Int -> Int -> Ordering)
    -> Int -> Int -> IO ()
mergeWithM g cmp count n =
    Stream.drain
        $ g
            (\a b -> return $ cmp a b)
            (sourceUnfoldrM count n)
            (sourceUnfoldrM count (n + 1))

{-# INLINE mergeBy #-}
mergeBy :: (Int -> Int -> Ordering) -> Int -> Int -> IO ()
mergeBy = mergeWith Stream.mergeBy

{-# INLINE mergeByM #-}
mergeByM :: (Int -> Int -> Ordering) -> Int -> Int -> IO ()
mergeByM = mergeWithM Stream.mergeByM

{-# INLINE mergeByMFused #-}
mergeByMFused :: (Int -> Int -> Ordering) -> Int -> Int -> IO ()
mergeByMFused = mergeWithM Stream.mergeByMFused

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'mergeBy
inspect $ 'mergeBy `hasNoType` ''SPEC
inspect $ 'mergeBy `hasNoType` ''D.Step

inspect $ hasNoTypeClasses 'mergeByM
inspect $ 'mergeByM `hasNoType` ''SPEC
inspect $ 'mergeByM `hasNoType` ''D.Step

{-# INLINE _mergeByMFusedCheck #-}
_mergeByMFusedCheck :: IO ()
_mergeByMFusedCheck = mergeWithM Stream.mergeByMFused compare 0 0

inspect $ hasNoTypeClasses '_mergeByMFusedCheck
inspect $ '_mergeByMFusedCheck `hasNoType` ''SPEC
inspect $ '_mergeByMFusedCheck `hasNoType` ''D.Step
#endif

{-# INLINE concatPairsWithMergeBy #-}
concatPairsWithMergeBy :: (Int -> Int -> Ordering) -> Int -> Int -> Int -> IO ()
concatPairsWithMergeBy cmp = concatPairsWith (Stream.mergeBy cmp)

{-# INLINE concatPairsWithMergeByFused #-}
concatPairsWithMergeByFused ::
    (Int -> Int -> Ordering) -> Int -> Int -> Int -> IO ()
concatPairsWithMergeByFused cmp =
    concatPairsWith (Stream.mergeByMFused (\x y -> return $ cmp x y))

-------------------------------------------------------------------------------
-- Interleaving
-------------------------------------------------------------------------------

o_1_space_joining :: Int -> [Benchmark]
o_1_space_joining value =
    [ bgroup "joining (2 of n/2)"
        [ benchIOSrc1
            "mergeBy compare"
            (mergeBy compare (value `div` 2))
        , benchIOSrc1
            "mergeByM compare"
            (mergeByM compare (value `div` 2))
        , benchIOSrc1
            "mergeByMFused compare"
            (mergeByMFused compare (value `div` 2))

        , benchIOSrc1
            "mergeBy (flip compare)"
            (mergeBy (flip compare) (value `div` 2))
        , benchIOSrc1
            "mergeByM (flip compare)"
            (mergeByM (flip compare) (value `div` 2))
        , benchIOSrc1
            "mergeByMFused (flip compare)"
            (mergeByMFused (flip compare) (value `div` 2))

        , benchIOSrc1
            "concatPairsWithMergeBy compare"
            (concatPairsWithMergeBy compare 2 (value `div` 2))
        , benchIOSrc1
            "concatPairsWithMergeBy (flip compare)"
            (concatPairsWithMergeBy (flip compare) 2 (value `div` 2))

        , benchIOSrc1
            "concatPairsWithMergeByFused compare"
            (concatPairsWithMergeByFused compare 2 (value `div` 2))
        , benchIOSrc1
            "concatPairsWithMergeByFused (flip compare)"
            (concatPairsWithMergeByFused (flip compare) 2 (value `div` 2))
        ]
    ]

-------------------------------------------------------------------------------
-- Concat
-------------------------------------------------------------------------------

{-
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

{-# INLINE unfoldManyMergeBy #-}
unfoldManyMergeBy :: Int -> Int -> Int -> IO ()
unfoldManyMergeBy outer inner n =
    S.drain $ (Internal.unfoldManyMergeBy compare)
        -- (UF.lmap return (UF.replicateM inner))
        (UF.lmap (\x -> (x,x)) (sourceUnfoldrMUF inner))
        (sourceUnfoldrM outer n)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'unfoldManyMergeBy
-- inspect $ 'unfoldManyMergeBy `hasNoType` ''SPEC
-- inspect $ 'unfoldManyMergeBy `hasNoType`
--      ''D.ConcatUnfoldMergeState
#endif
-}

{-# INLINE sortBy #-}
sortBy :: (Int -> Int -> Ordering) -> SerialT IO Int -> IO ()
sortBy cmp = Stream.drain . Stream.sortBy cmp

{-
-- For fair comparison with concatPairs, removed sorted segmentation
{-# INLINE listSortBy #-}
listSortBy :: (a -> a -> Ordering) -> [a] -> [a]
listSortBy cmp = mergeAll . sequences
  where
    sequences = fmap (: [])

    mergeAll [x] = x
    mergeAll xs  = mergeAll (mergePairs xs)

    mergePairs (a:b:xs) = let !x = merge a b
                          in x : mergePairs xs
    mergePairs xs       = xs

    merge as@(a:as') bs@(b:bs')
      | a `cmp` b == GT = b:merge as  bs'
      | otherwise       = a:merge as' bs
    merge [] bs         = bs
    merge as []         = as
-}

o_n_heap_concat :: Int -> [Benchmark]
o_n_heap_concat value =
    [ bgroup "concatPairsWith"
        [ benchIOSrc1
            "concatPairsWithMergeBy compare (n of 1)"
            (concatPairsWithMergeBy compare value 1)
        , benchIOSrc1
            "concatPairsWithMergeBy compare (sqrtVal of sqrtVal)"
            (concatPairsWithMergeBy compare sqrtVal sqrtVal)

        , benchIOSrc1
            "concatPairsWithMergeBy (flip compare) (n of 1)"
            (concatPairsWithMergeBy (flip compare) value 1)
        , benchIOSrc1
            "concatPairsWithMergeBy (flip compare) (sqrtVal of sqrtVal)"
            (concatPairsWithMergeBy (flip compare) sqrtVal sqrtVal)

        , benchIOSrc1
            "concatPairsWithMergeByFused compare (n of 1)"
            (concatPairsWithMergeByFused compare value 1)
        , benchIOSrc1
            "concatPairsWithMergeByFused compare (sqrtVal of sqrtVal)"
            (concatPairsWithMergeByFused compare sqrtVal sqrtVal)
        ]
    -- TODO: change sourceUnfoldrM to generate alternating bigger and lower
    -- numbers to simulate a random input for a worst case sort benchmark. We
    -- can use 0 and value as two ints in the state and alternate each in the
    -- output streams, incrementing the lower number of decrementing the higher
    -- number.
    , bgroup "sorting"
        [ benchIOSink value "sortBy compare" (sortBy compare)
        , benchIOSink value "sortBy (flip compare)" (sortBy (flip compare))
        , benchIOSink value "sortBy compare randomized"
            (sortBy compare . Stream.map (\x -> if even x then x + 2 else x))
        {-
        , bench "sortByLists compare"
            $ nf (\x -> listSortBy compare [1..x]) value
        , bench "sortByLists (flip compare)"
            $ nf (\x -> listSortBy (flip compare) [1..x]) value
        , bench "sortByLists compare randomized"
            $ nf (\x -> listSortBy compare
                    (map (\n -> if even n then n + 2 else n) [1..x])
                 )
                 value
        -}
        , bench "sortByLists compare"
            $ nf (\x -> List.sortBy compare [1..x]) value
        , bench "sortByLists (flip compare)"
            $ nf (\x -> List.sortBy (flip compare) [1..x]) value
        , bench "sortByLists compare randomized"
            $ nf (\x -> List.sortBy compare
                    (map (\n -> if even n then n + 2 else n) [1..x])
                 )
                 value
        ]
    ]

    where

    sqrtVal = round $ sqrt (fromIntegral value :: Double)

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
        [ bgroup (o_1_space_prefix moduleName) (o_1_space_joining size)
        , bgroup (o_n_heap_prefix moduleName) (o_n_heap_concat size)
        ]
