-- |
-- Module      : Stream.Elimination
-- Copyright   : (c) 2018 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

module Stream.Elimination (benchmarks) where

import Control.DeepSeq (NFData(..))
import Data.Functor.Identity (Identity, runIdentity)
import Stream.Common
    ( sourceUnfoldr
    , sourceUnfoldrM
    , sourceUnfoldrAction
    , benchIOSink
    )
import Streamly.Internal.Data.Stream (Stream)

import qualified Data.Foldable as F
import qualified GHC.Exts as GHC
import qualified Streamly.Internal.Data.Stream as Stream

import Prelude hiding (length, sum, or, and, any, all, notElem, elem, (!!),
    lookup, repeat, minimum, maximum, product, last, mapM_, init)
import qualified Prelude

import Gauge
import Streamly.Benchmark.Common

-------------------------------------------------------------------------------
-- Foldable Instance
-------------------------------------------------------------------------------
{-# INLINE foldableFoldl' #-}
foldableFoldl' :: Int -> Int -> Int
foldableFoldl' value n =
    F.foldl' (+) 0 (sourceUnfoldr value n :: Stream Identity Int)

{-# INLINE foldableFoldrElem #-}
foldableFoldrElem :: Int -> Int -> Bool
foldableFoldrElem value n =
    F.foldr (\x xs -> x == value || xs)
            False
            (sourceUnfoldr value n :: Stream Identity Int)

{-# INLINE foldableSum #-}
foldableSum :: Int -> Int -> Int
foldableSum value n =
    Prelude.sum (sourceUnfoldr value n :: Stream Identity Int)

{-# INLINE foldableProduct #-}
foldableProduct :: Int -> Int -> Int
foldableProduct value n =
    Prelude.product (sourceUnfoldr value n :: Stream Identity Int)

{-# INLINE _foldableNull #-}
_foldableNull :: Int -> Int -> Bool
_foldableNull value n =
    Prelude.null (sourceUnfoldr value n :: Stream Identity Int)

{-# INLINE foldableElem #-}
foldableElem :: Int -> Int -> Bool
foldableElem value n =
    value `Prelude.elem` (sourceUnfoldr value n :: Stream Identity Int)

{-# INLINE foldableNotElem #-}
foldableNotElem :: Int -> Int -> Bool
foldableNotElem value n =
    value `Prelude.notElem` (sourceUnfoldr value n :: Stream Identity Int)

{-# INLINE foldableFind #-}
foldableFind :: Int -> Int -> Maybe Int
foldableFind value n =
    F.find (== (value + 1)) (sourceUnfoldr value n :: Stream Identity Int)

{-# INLINE foldableAll #-}
foldableAll :: Int -> Int -> Bool
foldableAll value n =
    Prelude.all (<= (value + 1)) (sourceUnfoldr value n :: Stream Identity Int)

{-# INLINE foldableAny #-}
foldableAny :: Int -> Int -> Bool
foldableAny value n =
    Prelude.any (> (value + 1)) (sourceUnfoldr value n :: Stream Identity Int)

{-# INLINE foldableAnd #-}
foldableAnd :: Int -> Int -> Bool
foldableAnd value n =
    Prelude.and $ Stream.mapM
        (return . (<= (value + 1))) (sourceUnfoldr value n :: Stream Identity Int)

{-# INLINE foldableOr #-}
foldableOr :: Int -> Int -> Bool
foldableOr value n =
    Prelude.or $ Stream.mapM
        (return . (> (value + 1))) (sourceUnfoldr value n :: Stream Identity Int)

{-# INLINE foldableLength #-}
foldableLength :: Int -> Int -> Int
foldableLength value n =
    Prelude.length (sourceUnfoldr value n :: Stream Identity Int)

{-# INLINE foldableMin #-}
foldableMin :: Int -> Int -> Int
foldableMin value n =
    Prelude.minimum (sourceUnfoldr value n :: Stream Identity Int)

{-# INLINE ordInstanceMin #-}
ordInstanceMin :: Stream Identity Int -> Stream Identity Int
ordInstanceMin src = min src src

{-# INLINE foldableMax #-}
foldableMax :: Int -> Int -> Int
foldableMax value n =
    Prelude.maximum (sourceUnfoldr value n :: Stream Identity Int)

{-# INLINE foldableMinBy #-}
foldableMinBy :: Int -> Int -> Int
foldableMinBy value n =
    F.minimumBy compare (sourceUnfoldr value n :: Stream Identity Int)

{-# INLINE foldableListMinBy #-}
foldableListMinBy :: Int -> Int -> Int
foldableListMinBy value n = F.minimumBy compare [1..value+n]

{-# INLINE foldableMaxBy #-}
foldableMaxBy :: Int -> Int -> Int
foldableMaxBy value n =
    F.maximumBy compare (sourceUnfoldr value n :: Stream Identity Int)

{-# INLINE foldableToList #-}
foldableToList :: Int -> Int -> [Int]
foldableToList value n =
    F.toList (sourceUnfoldr value n :: Stream Identity Int)

{-# INLINE foldableMapM_ #-}
foldableMapM_ :: Monad m => Int -> Int -> m ()
foldableMapM_ value n =
    F.mapM_ (\_ -> return ()) (sourceUnfoldr value n :: Stream Identity Int)

{-# INLINE foldableSequence_ #-}
foldableSequence_ :: Int -> Int -> IO ()
foldableSequence_ value n =
    F.sequence_ (sourceUnfoldrAction value n :: Stream Identity (IO Int))

{-# INLINE _foldableMsum #-}
_foldableMsum :: Int -> Int -> IO Int
_foldableMsum value n =
    F.msum (sourceUnfoldrAction value n :: Stream Identity (IO Int))

{-# INLINE showInstance #-}
showInstance :: Stream Identity Int -> String
showInstance = show

o_1_space_elimination_foldable :: Int -> [Benchmark]
o_1_space_elimination_foldable value =
    [ bgroup "foldable"
          -- Foldable instance
        [ bench "foldl'" $ nf (foldableFoldl' value) 1
        , bench "foldrElem" $ nf (foldableFoldrElem value) 1
        , bench "null" $ nf (_foldableNull value) 1
        , bench "elem" $ nf (foldableElem value) 1
        , bench "length" $ nf (foldableLength value) 1
        , bench "sum" $ nf (foldableSum value) 1
        , bench "product" $ nf (foldableProduct value) 1
        , bench "minimum" $ nf (foldableMin value) 1
        , benchPureSink value "min (ord)" ordInstanceMin
        , bench "maximum" $ nf (foldableMax value) 1
        , bench "length . toList"
            $ nf (Prelude.length . foldableToList value) 1
        , bench "notElem" $ nf (foldableNotElem value) 1
        , bench "find" $ nf (foldableFind value) 1
        , bench "all" $ nf (foldableAll value) 1
        , bench "any" $ nf (foldableAny value) 1
        , bench "and" $ nf (foldableAnd value) 1
        , bench "or" $ nf (foldableOr value) 1

        , benchIOSink1 "mapM_" (foldableMapM_ value)
        , benchIOSink1 "sequence_" (foldableSequence_ value)
        ]
    ]

o_n_space_elimination_foldable :: Int -> [Benchmark]
o_n_space_elimination_foldable value =
    -- Head recursive strict right folds.
    [ bgroup "foldl"
        -- XXX the definitions of minimumBy and maximumBy in Data.Foldable use
        -- foldl1 which does not work in constant memory for our
        -- implementation.  It works in constant memory for lists but even for
        -- lists it takes 15x more time compared to our foldl' based
        -- implementation.
        [ bench "minimumBy" $ nf (`foldableMinBy` 1) value
        , bench "maximumBy" $ nf (`foldableMaxBy` 1) value
        , bench "minimumByList" $ nf (`foldableListMinBy` 1) value
        ]
    ]

-------------------------------------------------------------------------------
-- Stream folds
-------------------------------------------------------------------------------

{-# INLINE benchPureSink #-}
benchPureSink :: NFData b
    => Int -> String -> (Stream Identity Int -> b) -> Benchmark
benchPureSink value name = benchPure name (sourceUnfoldr value)

-------------------------------------------------------------------------------
-- Reductions
-------------------------------------------------------------------------------

{-# INLINE uncons #-}
uncons :: Monad m => Stream m Int -> m ()
uncons s = do
    r <- Stream.uncons s
    case r of
        Nothing -> return ()
        Just (_, t) -> uncons t

{-# INLINE foldrMElem #-}
foldrMElem :: Monad m => Int -> Stream m Int -> m Bool
foldrMElem e =
    Stream.foldrM
        (\x xs ->
             if x == e
                 then return True
                 else xs)
        (return False)

{-# INLINE foldrMBuild #-}
foldrMBuild :: Monad m => Stream m Int -> m [Int]
foldrMBuild = Stream.foldrM (\x xs -> (x :) <$> xs) (return [])

o_1_space_elimination_folds :: Int -> [Benchmark]
o_1_space_elimination_folds value =
    [ bgroup "elimination"
        -- Basic folds
        [
         bgroup "build"
            [ bgroup "IO"
                  [ benchIOSink value "foldrMElem" (foldrMElem value)
                  ]
            , bgroup "Identity"
                  [
                   benchPureSink value "foldrMToListLength"
                        (Prelude.length . runIdentity . foldrMBuild)
                  ]
            ]

        -- deconstruction
        , benchIOSink value "uncons" uncons
        , benchPureSink value "drain (pure)" id

        -- length is used to check for foldr/build fusion
        , benchPureSink value "length . IsList.toList" (Prelude.length . GHC.toList)
        ]
    ]

-------------------------------------------------------------------------------
-- Buffered Transformations by fold
-------------------------------------------------------------------------------


-- For comparisons
{-# INLINE showInstanceList #-}
showInstanceList :: [Int] -> String
showInstanceList = show

o_n_heap_elimination_buffered :: Int -> [Benchmark]
o_n_heap_elimination_buffered value =
    [ bgroup "buffered"
        -- Buffers the output of show/read.
        -- XXX can the outputs be streaming? Can we have special read/show
        -- style type classes, readM/showM supporting streaming effects?
        [ bench "showPrec Haskell lists" $ nf showInstanceList (mkList value)
        -- XXX This is not o-1-space for GHC-8.10
        , benchPureSink value "showsPrec pure streams" showInstance
        ]
    ]

{-# INLINE foldrMReduce #-}
foldrMReduce :: Monad m => Stream m Int -> m Int
foldrMReduce = Stream.foldrM (\x xs -> (x +) <$> xs) (return 0)

o_n_space_elimination_foldr :: Int -> [Benchmark]
o_n_space_elimination_foldr value =
    -- Head recursive strict right folds.
    [ bgroup "foldr"
        -- accumulation due to strictness of IO monad
        [ benchIOSink value "foldrM/build/IO (toList)" foldrMBuild
        -- Right folds for reducing are inherently non-streaming as the
        -- expression needs to be fully built before it can be reduced.
       -- , benchIdentitySink value "foldrM/reduce/Identity (sum)" foldrMReduce
        , benchIOSink value "foldrM/reduce/IO (sum)" foldrMReduce
        ]
    ]


-------------------------------------------------------------------------------
-- Multi-stream folds
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- Multi-stream pure
-------------------------------------------------------------------------------

{-# INLINE eqBy' #-}
eqBy' :: (Monad m, Eq a) => Stream m a -> m Bool
eqBy' src = Stream.eqBy (==) src src

{-# INLINE eqByPure #-}
eqByPure :: Int -> Int -> Identity Bool
eqByPure value n = eqBy' (sourceUnfoldr value n)


{-# INLINE eqInstance #-}
eqInstance :: Stream Identity Int -> Bool
eqInstance src = src == src

{-# INLINE eqInstanceNotEq #-}
eqInstanceNotEq :: Stream Identity Int -> Bool
eqInstanceNotEq src = src /= src

{-# INLINE cmpBy' #-}
cmpBy' :: (Monad m, Ord a) => Stream m a -> m Ordering
cmpBy' src = Stream.cmpBy compare src src

{-# INLINE cmpByPure #-}
cmpByPure :: Int -> Int -> Identity Ordering
cmpByPure value n = cmpBy' (sourceUnfoldr value n)


{-# INLINE ordInstance #-}
ordInstance :: Stream Identity Int -> Bool
ordInstance src = src < src

o_1_space_elimination_multi_stream_pure :: Int -> [Benchmark]
o_1_space_elimination_multi_stream_pure value =
    [ bgroup "multi-stream-pure"
        [ benchPureSink1 "eqBy" (eqByPure value)
        , benchPureSink value "==" eqInstance
        , benchPureSink value "/=" eqInstanceNotEq
        , benchPureSink1 "cmpBy" (cmpByPure value)
        , benchPureSink value "<" ordInstance
        ]
    ]

{-# INLINE isPrefixOf #-}
isPrefixOf :: Monad m => Stream m Int -> m Bool
isPrefixOf src = Stream.isPrefixOf src src

{-# INLINE isSubsequenceOf #-}
isSubsequenceOf :: Monad m => Stream m Int -> m Bool
isSubsequenceOf src = Stream.isSubsequenceOf src src

{-# INLINE stripPrefix #-}
stripPrefix :: Monad m => Stream m Int -> m ()
stripPrefix src = do
    _ <- Stream.stripPrefix src src
    return ()

{-# INLINE eqBy #-}
eqBy :: Int -> Int -> IO Bool
eqBy value n = eqBy' (sourceUnfoldrM value n)

{-# INLINE cmpBy #-}
cmpBy :: Int -> Int -> IO Ordering
cmpBy value n = cmpBy' (sourceUnfoldrM value n)

o_1_space_elimination_multi_stream :: Int -> [Benchmark]
o_1_space_elimination_multi_stream value =
    [ bgroup "multi-stream"
        [ benchIOSink1 "eqBy" (eqBy value)
        , benchIOSink1 "cmpBy" (cmpBy value)
        , benchIOSink value "isPrefixOf" isPrefixOf
        , benchIOSink value "isSubsequenceOf" isSubsequenceOf
        , benchIOSink value "stripPrefix" stripPrefix
        ]
    ]

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

-- In addition to gauge options, the number of elements in the stream can be
-- passed using the --stream-size option.
--
benchmarks :: String -> Int -> [Benchmark]
benchmarks moduleName size =
        [ bgroup (o_1_space_prefix moduleName) $ concat
            [ o_1_space_elimination_foldable size
            , o_1_space_elimination_folds size
            , o_1_space_elimination_multi_stream_pure size
            , o_1_space_elimination_multi_stream size
            ]
        , bgroup (o_n_heap_prefix moduleName) $ o_n_heap_elimination_buffered size
        , bgroup (o_n_space_prefix moduleName) $ o_n_space_elimination_foldable size
                                                ++ o_n_space_elimination_foldr size
        ]
