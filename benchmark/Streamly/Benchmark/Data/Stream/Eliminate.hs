-- |
-- Module      : Stream.Eliminate
-- Copyright   : (c) 2018 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

#ifdef __HADDOCK_VERSION__
#undef INSPECTION
#endif

#ifdef INSPECTION
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fplugin Test.Inspection.Plugin #-}
#endif

module Stream.Eliminate (benchmarks) where

import Control.Monad ((>=>))
import Data.Functor.Identity (Identity(..), runIdentity)
import System.Random (randomRIO)

import qualified Streamly.Internal.Data.Fold as Fold

#ifdef INSPECTION
import GHC.Types (SPEC(..))
import Test.Inspection
#endif

import Stream.Common hiding (benchIO)
import Stream.Type (benchIO, withStream, withPureStream)
import Streamly.Internal.Data.Stream (Stream)
import qualified Streamly.Internal.Data.Stream as S

import Test.Tasty.Bench
import Streamly.Benchmark.Common
import Prelude hiding (length, sum, or, and, any, all, notElem, elem, (!!),
    lookup, repeat, minimum, maximum, product, last, mapM_, init)
import qualified Prelude

-------------------------------------------------------------------------------
-- Stream folds
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- Reductions
-------------------------------------------------------------------------------

{-# INLINE streamInit #-}
streamInit :: Int -> IO ()
streamInit value = withStream value (S.init >=> Prelude.mapM_ S.drain)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'streamInit
inspect $ 'streamInit `hasNoType` ''S.Step
inspect $ 'streamInit `hasNoType` ''Fold.Step
inspect $ 'streamInit `hasNoType` ''SPEC
#endif

{-# INLINE mapM_ #-}
mapM_ :: Int -> IO ()
mapM_ value = withStream value (S.mapM_ (\_ -> return ()))

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'mapM_
inspect $ 'mapM_ `hasNoType` ''S.Step
inspect $ 'mapM_ `hasNoType` ''Fold.Step
inspect $ 'mapM_ `hasNoType` ''SPEC
#endif

{-# INLINE streamLast #-}
streamLast :: Int -> IO (Maybe Int)
streamLast value = withStream value S.last

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'streamLast
inspect $ 'streamLast `hasNoType` ''S.Step
inspect $ 'streamLast `hasNoType` ''Fold.Step
inspect $ 'streamLast `hasNoType` ''SPEC
#endif

{-# INLINE foldl1'Reduce #-}
foldl1'Reduce :: Int -> IO (Maybe Int)
foldl1'Reduce value = withStream value (S.fold (Fold.foldl1' (+)))

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'foldl1'Reduce
inspect $ 'foldl1'Reduce `hasNoType` ''S.Step
#endif

{-# INLINE foldl1'ReduceIdentity #-}
foldl1'ReduceIdentity :: Int -> IO (Maybe Int)
foldl1'ReduceIdentity value =
    withPureStream value (runIdentity . S.fold (Fold.foldl1' (+)))

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'foldl1'ReduceIdentity
inspect $ 'foldl1'ReduceIdentity `hasNoType` ''S.Step
#endif

{-# INLINE elem #-}
elem :: Int -> IO Bool
elem value = withStream value (S.elem (value + 1))

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'elem
inspect $ 'elem `hasNoType` ''S.Step
inspect $ 'elem `hasNoType` ''Fold.Step
inspect $ 'elem `hasNoType` ''SPEC
#endif

{-# INLINE notElem #-}
notElem :: Int -> IO Bool
notElem value = withStream value (S.notElem (value + 1))

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'notElem
inspect $ 'notElem `hasNoType` ''S.Step
inspect $ 'notElem `hasNoType` ''Fold.Step
inspect $ 'notElem `hasNoType` ''SPEC
#endif

{-# INLINE length #-}
length :: Int -> IO Int
length value = withStream value (S.fold Fold.length)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'length
inspect $ 'length `hasNoType` ''S.Step
inspect $ 'length `hasNoType` ''Fold.Step
inspect $ 'length `hasNoType` ''SPEC
#endif

{-# INLINE all #-}
all :: Int -> IO Bool
all value = withStream value (S.all (<= (value + 1)))

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'all
inspect $ 'all `hasNoType` ''S.Step
inspect $ 'all `hasNoType` ''Fold.Step
inspect $ 'all `hasNoType` ''SPEC
#endif

{-# INLINE any #-}
any :: Int -> IO Bool
any value = withStream value (S.any (> (value + 1)))

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'any
inspect $ 'any `hasNoType` ''S.Step
inspect $ 'any `hasNoType` ''Fold.Step
inspect $ 'any `hasNoType` ''SPEC
#endif

{-# INLINE and #-}
and :: Int -> IO Bool
and value = withStream value (S.fold Fold.and . S.map (<= (value + 1)))

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'and
inspect $ 'and `hasNoType` ''S.Step
inspect $ 'and `hasNoType` ''Fold.Step
inspect $ 'and `hasNoType` ''SPEC
#endif

{-# INLINE or #-}
or :: Int -> IO Bool
or value = withStream value (S.fold Fold.or . S.map (> (value + 1)))

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'or
inspect $ 'or `hasNoType` ''S.Step
inspect $ 'or `hasNoType` ''Fold.Step
inspect $ 'or `hasNoType` ''SPEC
#endif

{-# INLINE find #-}
find :: Int -> IO (Maybe Int)
find value = withStream value (S.find (== (value + 1)))

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'find
inspect $ 'find `hasNoType` ''S.Step
inspect $ 'find `hasNoType` ''Fold.Step
inspect $ 'find `hasNoType` ''SPEC
#endif

{-# INLINE findM #-}
findM :: Int -> IO (Maybe Int)
findM value = withStream value (S.findM (\z -> return $ z == (value + 1)))

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'findM
inspect $ 'findM `hasNoType` ''S.Step
inspect $ 'findM `hasNoType` ''Fold.Step
inspect $ 'findM `hasNoType` ''SPEC
#endif

{-# INLINE maximum #-}
maximum :: Int -> IO (Maybe Int)
maximum value = withStream value S.maximum

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'maximum
inspect $ 'maximum `hasNoType` ''S.Step
inspect $ 'maximum `hasNoType` ''Fold.Step
inspect $ 'maximum `hasNoType` ''SPEC
#endif

{-# INLINE minimum #-}
minimum :: Int -> IO (Maybe Int)
minimum value = withStream value S.minimum

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'minimum
inspect $ 'minimum `hasNoType` ''S.Step
inspect $ 'minimum `hasNoType` ''Fold.Step
inspect $ 'minimum `hasNoType` ''SPEC
#endif

{-# INLINE sum #-}
sum :: Int -> IO Int
sum value = withStream value (S.fold Fold.sum)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'sum
inspect $ 'sum `hasNoType` ''S.Step
inspect $ 'sum `hasNoType` ''Fold.Step
inspect $ 'sum `hasNoType` ''SPEC
#endif

{-# INLINE product #-}
product :: Int -> IO Int
product value = withStream value (S.fold Fold.product)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'product
inspect $ 'product `hasNoType` ''S.Step
inspect $ 'product `hasNoType` ''Fold.Step
inspect $ 'product `hasNoType` ''SPEC
#endif

{-# INLINE minimumBy #-}
minimumBy :: Int -> IO (Maybe Int)
minimumBy value = withStream value (S.minimumBy compare)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'minimumBy
inspect $ 'minimumBy `hasNoType` ''S.Step
inspect $ 'minimumBy `hasNoType` ''Fold.Step
inspect $ 'minimumBy `hasNoType` ''SPEC
#endif

{-# INLINE maximumBy #-}
maximumBy :: Int -> IO (Maybe Int)
maximumBy value = withStream value (S.maximumBy compare)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'maximumBy
inspect $ 'maximumBy `hasNoType` ''S.Step
inspect $ 'maximumBy `hasNoType` ''Fold.Step
inspect $ 'maximumBy `hasNoType` ''SPEC
#endif

{-# INLINE the #-}
the :: Int -> IO (Maybe Int)
the value = randomRIO (1, 1) >>= S.the . repeat value

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'the
inspect $ 'the `hasNoType` ''S.Step
inspect $ 'the `hasNoType` ''Fold.Step
inspect $ 'the `hasNoType` ''SPEC
#endif

{-# INLINE indexOp #-}
indexOp :: Int -> IO (Maybe Int)
indexOp value = withStream value (S.!! value)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'indexOp
inspect $ 'indexOp `hasNoType` ''S.Step
inspect $ 'indexOp `hasNoType` ''Fold.Step
inspect $ 'indexOp `hasNoType` ''SPEC
#endif

{-# INLINE lookupNever #-}
lookupNever :: Int -> IO (Maybe Int)
lookupNever value =
    withStream value (S.lookup (value + 1) . S.map (\x -> (x, x)))

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'lookupNever
inspect $ 'lookupNever `hasNoType` ''S.Step
inspect $ 'lookupNever `hasNoType` ''Fold.Step
inspect $ 'lookupNever `hasNoType` ''SPEC
#endif

o_1_space_elimination_folds :: Int -> [Benchmark]
o_1_space_elimination_folds value =
    [ bgroup "elimination"
        -- Basic folds
        [
            bgroup "reduce"
            [ bgroup
                  "IO"
                  [ benchIO "foldl1'" $ foldl1'Reduce value
                  ]

            , bgroup
                  "Identity"
                  [ benchIO "foldl1'" $ foldl1'ReduceIdentity value
                  ]
            ]

        -- deconstruction
        , benchIO "mapM_" $ mapM_ value
        , benchIO "last" $ streamLast value
        , benchIO "init" $ streamInit value

        -- this is too fast, causes all benchmarks reported in ns
    -- , benchIO "head" $ ...
        , benchIO "length" $ length value
        , benchIO "sum" $ sum value
        , benchIO "product" $ product value
        , benchIO "maximumBy" $ maximumBy value
        , benchIO "maximum" $ maximum value
        , benchIO "minimumBy" $ minimumBy value
        , benchIO "minimum" $ minimum value

        , benchIO "the" $ the value
        , benchIO "find" $ find value
        , benchIO "findM" $ findM value
        -- , benchIO "lookupFirst" $ ...
        , benchIO "lookupNever" $ lookupNever value
        , benchIO "(!!)" $ indexOp value
        , benchIO "elem" $ elem value
        , benchIO "notElem" $ notElem value
        , benchIO "all" $ all value
        , benchIO "any" $ any value
        , benchIO "and" $ and value
        , benchIO "or" $ or value
        ]
    ]

{-# INLINE toListRev #-}
toListRev :: Int -> IO [Int]
toListRev value = withStream value S.toListRev

-- NOTE: this is a Fold benchmark, used here only for comparison with toListRev
{-# INLINE toStreamRev #-}
toStreamRev :: Int -> IO (Stream Identity Int)
toStreamRev value = withStream value (S.fold Fold.toStreamRev)

o_n_heap_elimination_toList :: Int -> [Benchmark]
o_n_heap_elimination_toList value =
    [ bgroup "toList"
        -- Converting the stream to a list or pure stream in a strict monad
        [ benchIO "toListRev" $ toListRev value
        , benchIO "toStreamRev" $ toStreamRev value
        ]
    ]

-- NOTE: this is a Fold benchmark, used here only for comparison with ToList
{-# INLINE toStream #-}
toStream :: Int -> IO (Stream Identity Int)
toStream value = withStream value (S.fold Fold.toStream)

o_n_space_elimination_toList :: Int -> [Benchmark]
o_n_space_elimination_toList value =
    [ bgroup "toList"
        -- Converting the stream to a list or pure stream in a strict monad
        [ benchIO "toStream" $ toStream value
        ]
    ]

-------------------------------------------------------------------------------
-- Multi-stream folds
-------------------------------------------------------------------------------

{-# INLINE isPrefixOf #-}
isPrefixOf :: Int -> IO Bool
isPrefixOf value = withStream value (\src -> S.isPrefixOf src src)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'isPrefixOf
inspect $ 'isPrefixOf `hasNoType` ''S.Step
inspect $ 'isPrefixOf `hasNoType` ''Fold.Step
inspect $ 'isPrefixOf `hasNoType` ''SPEC
#endif

{-# INLINE isSubsequenceOf #-}
isSubsequenceOf :: Int -> IO Bool
isSubsequenceOf value = withStream value (\src -> S.isSubsequenceOf src src)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'isSubsequenceOf
inspect $ 'isSubsequenceOf `hasNoType` ''S.Step
inspect $ 'isSubsequenceOf `hasNoType` ''Fold.Step
inspect $ 'isSubsequenceOf `hasNoType` ''SPEC
#endif

{-# INLINE stripPrefix #-}
stripPrefix :: Int -> IO ()
stripPrefix value = withStream value (\src -> do
    _ <- S.stripPrefix src src
    return ())

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'stripPrefix
inspect $ 'stripPrefix `hasNoType` ''S.Step
inspect $ 'stripPrefix `hasNoType` ''Fold.Step
inspect $ 'stripPrefix `hasNoType` ''SPEC
#endif

o_1_space_elimination_multi_stream :: Int -> [Benchmark]
o_1_space_elimination_multi_stream value =
    [ bgroup "multi-stream"
        [ benchIO "isPrefixOf" $ isPrefixOf value
        , benchIO "isSubsequenceOf" $ isSubsequenceOf value
        , benchIO "stripPrefix" $ stripPrefix value
        ]
    ]

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

-- In addition to gauge options, the number of elements in the stream can be
-- passed using the --stream-size option.
--
benchmarks :: Int -> [(SpaceComplexity, Benchmark)]
benchmarks size =
    map (SpaceO_1,) (concat
        [ o_1_space_elimination_folds size
        , o_1_space_elimination_multi_stream size
        ])
    ++ map (HeapO_n,) (o_n_heap_elimination_toList size)
    ++ map (SpaceO_n,) (o_n_space_elimination_toList size)
