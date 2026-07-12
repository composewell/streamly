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

import GHC.Types (SPEC(..))
import Control.Monad ((>=>), when)
import Data.Functor.Identity (Identity(..), runIdentity)
import Data.Maybe (isJust)

import qualified Streamly.Internal.Data.Fold as Fold

#ifdef INSPECTION
import Test.Inspection
#endif

import Stream.Common hiding (benchIO)
import Stream.Type (benchIO, withStream, withPureStream)
import Streamly.Internal.Data.Stream (Stream)
import qualified Streamly.Internal.Data.Stream as S

import Test.Tasty.Bench
import Streamly.Benchmark.Common
import Fusion.Plugin.Types
import qualified Streamly.Internal.Data.SVar.Type as SVar
import Prelude hiding (length, sum, or, and, any, all, notElem, elem, (!!),
    lookup, repeat, minimum, maximum, product, last, mapM_, init, tail)
import qualified Prelude

-------------------------------------------------------------------------------
-- Stream folds
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- Reductions
-------------------------------------------------------------------------------

{-# ANN streamInit (PermitPatternMatches [''Int]) #-}
{-# ANN streamInit (PermitConstructions [''()]) #-}
{-# ANN streamInit (PermitTypeClasses []) #-}
{-# NOINLINE streamInit #-}
streamInit :: Int -> Int -> IO ()
streamInit value = withStream value (S.init >=> Prelude.mapM_ S.drain)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'streamInit
inspect $ 'streamInit `hasNoType` ''S.Step
inspect $ 'streamInit `hasNoType` ''Fold.Step
inspect $ 'streamInit `hasNoType` ''SPEC
#endif

{-# ANN mapM_ (PermitPatternMatches [''Int]) #-}
{-# ANN mapM_ (PermitConstructions [''()]) #-}
{-# ANN mapM_ (PermitTypeClasses []) #-}
{-# NOINLINE mapM_ #-}
mapM_ :: Int -> Int -> IO ()
mapM_ value = withStream value (S.mapM_ (\_ -> return ()))

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'mapM_
inspect $ 'mapM_ `hasNoType` ''S.Step
inspect $ 'mapM_ `hasNoType` ''Fold.Step
inspect $ 'mapM_ `hasNoType` ''SPEC
#endif

{-# ANN streamLast (PermitPatternMatches [''Int]) #-}
{-# ANN streamLast (PermitConstructions [''Maybe,''Int]) #-}
{-# ANN streamLast (PermitTypeClasses []) #-}
{-# NOINLINE streamLast #-}
streamLast :: Int -> Int -> IO (Maybe Int)
streamLast value = withStream value S.last

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'streamLast
inspect $ 'streamLast `hasNoType` ''S.Step
inspect $ 'streamLast `hasNoType` ''Fold.Step
inspect $ 'streamLast `hasNoType` ''SPEC
#endif

{-# ANN foldl1'Reduce (PermitPatternMatches [''Int]) #-}
{-# ANN foldl1'Reduce (PermitConstructions [''Maybe,''Int]) #-}
{-# ANN foldl1'Reduce (PermitTypeClasses []) #-}
{-# NOINLINE foldl1'Reduce #-}
foldl1'Reduce :: Int -> Int -> IO (Maybe Int)
foldl1'Reduce value = withStream value (S.fold (Fold.foldl1' (+)))

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'foldl1'Reduce
inspect $ 'foldl1'Reduce `hasNoType` ''S.Step
#endif

-- NOTE: eta expansion is required to eliminate the pattern match on S.Step
-- type. Step is included to avoid accidental eta reduction.
{-# ANN foldl1'ReduceIdentity (PermitPatternMatches [''Int]) #-}
{-# ANN foldl1'ReduceIdentity (PermitConstructions [''Int,''Maybe]) #-}
{-# ANN foldl1'ReduceIdentity (PermitTypeClasses []) #-}
{-# NOINLINE foldl1'ReduceIdentity #-}
foldl1'ReduceIdentity :: Int -> Int -> IO (Maybe Int)
foldl1'ReduceIdentity value n =
    withPureStream value (runIdentity . S.fold (Fold.foldl1' (+))) n

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'foldl1'ReduceIdentity
inspect $ 'foldl1'ReduceIdentity `hasNoType` ''S.Step
#endif

{-# ANN elem (PermitPatternMatches [''Int]) #-}
{-# ANN elem (PermitConstructions [''Bool]) #-}
{-# ANN elem (PermitTypeClasses []) #-}
{-# NOINLINE elem #-}
elem :: Int -> Int -> IO Bool
elem value = withStream value (S.elem (value + 1))

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'elem
inspect $ 'elem `hasNoType` ''S.Step
inspect $ 'elem `hasNoType` ''Fold.Step
inspect $ 'elem `hasNoType` ''SPEC
#endif

{-# ANN notElem (PermitPatternMatches [''Int]) #-}
{-# ANN notElem (PermitConstructions [''Bool]) #-}
{-# ANN notElem (PermitTypeClasses []) #-}
{-# NOINLINE notElem #-}
notElem :: Int -> Int -> IO Bool
notElem value = withStream value (S.notElem (value + 1))

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'notElem
inspect $ 'notElem `hasNoType` ''S.Step
inspect $ 'notElem `hasNoType` ''Fold.Step
inspect $ 'notElem `hasNoType` ''SPEC
#endif

{-# ANN length (PermitPatternMatches [''Int]) #-}
{-# ANN length (PermitConstructions [''Int]) #-}
{-# ANN length (PermitTypeClasses []) #-}
{-# NOINLINE length #-}
length :: Int -> Int -> IO Int
length value = withStream value (S.fold Fold.length)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'length
inspect $ 'length `hasNoType` ''S.Step
inspect $ 'length `hasNoType` ''Fold.Step
inspect $ 'length `hasNoType` ''SPEC
#endif

{-# ANN all (PermitPatternMatches [''Int]) #-}
{-# ANN all (PermitConstructions [''Bool]) #-}
{-# ANN all (PermitTypeClasses []) #-}
{-# NOINLINE all #-}
all :: Int -> Int -> IO Bool
all value = withStream value (S.all (<= (value + 1)))

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'all
inspect $ 'all `hasNoType` ''S.Step
inspect $ 'all `hasNoType` ''Fold.Step
inspect $ 'all `hasNoType` ''SPEC
#endif

{-# ANN any (PermitPatternMatches [''Int]) #-}
{-# ANN any (PermitConstructions [''Bool]) #-}
{-# ANN any (PermitTypeClasses []) #-}
{-# NOINLINE any #-}
any :: Int -> Int -> IO Bool
any value = withStream value (S.any (> (value + 1)))

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'any
inspect $ 'any `hasNoType` ''S.Step
inspect $ 'any `hasNoType` ''Fold.Step
inspect $ 'any `hasNoType` ''SPEC
#endif

{-# ANN and (PermitPatternMatches [''Int]) #-}
{-# ANN and (PermitConstructions [''Bool]) #-}
{-# ANN and (PermitTypeClasses []) #-}
{-# NOINLINE and #-}
and :: Int -> Int -> IO Bool
and value = withStream value (S.fold Fold.and . S.map (<= (value + 1)))

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'and
inspect $ 'and `hasNoType` ''S.Step
inspect $ 'and `hasNoType` ''Fold.Step
inspect $ 'and `hasNoType` ''SPEC
#endif

{-# ANN or (PermitPatternMatches [''Int]) #-}
{-# ANN or (PermitConstructions [''Bool]) #-}
{-# ANN or (PermitTypeClasses []) #-}
{-# NOINLINE or #-}
or :: Int -> Int -> IO Bool
or value = withStream value (S.fold Fold.or . S.map (> (value + 1)))

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'or
inspect $ 'or `hasNoType` ''S.Step
inspect $ 'or `hasNoType` ''Fold.Step
inspect $ 'or `hasNoType` ''SPEC
#endif

{-# ANN find (PermitPatternMatches [''Int]) #-}
{-# ANN find (PermitConstructions [''Int,''Maybe]) #-}
{-# ANN find (PermitTypeClasses []) #-}
{-# NOINLINE find #-}
find :: Int -> Int -> IO (Maybe Int)
find value = withStream value (S.find (== (value + 1)))

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'find
inspect $ 'find `hasNoType` ''S.Step
inspect $ 'find `hasNoType` ''Fold.Step
inspect $ 'find `hasNoType` ''SPEC
#endif

{-# ANN findM (PermitPatternMatches [''Int]) #-}
{-# ANN findM (PermitConstructions [''Int,''Maybe]) #-}
{-# ANN findM (PermitTypeClasses []) #-}
{-# NOINLINE findM #-}
findM :: Int -> Int -> IO (Maybe Int)
findM value = withStream value (S.findM (\z -> return $ z == (value + 1)))

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'findM
inspect $ 'findM `hasNoType` ''S.Step
inspect $ 'findM `hasNoType` ''Fold.Step
inspect $ 'findM `hasNoType` ''SPEC
#endif

{-# ANN maximum (PermitPatternMatches [''Int]) #-}
{-# ANN maximum (PermitConstructions [''Maybe,''Int]) #-}
{-# ANN maximum (PermitTypeClasses []) #-}
{-# NOINLINE maximum #-}
maximum :: Int -> Int -> IO (Maybe Int)
maximum value = withStream value S.maximum

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'maximum
inspect $ 'maximum `hasNoType` ''S.Step
inspect $ 'maximum `hasNoType` ''Fold.Step
inspect $ 'maximum `hasNoType` ''SPEC
#endif

{-# ANN minimum (PermitPatternMatches [''Int]) #-}
{-# ANN minimum (PermitConstructions [''Maybe,''Int]) #-}
{-# ANN minimum (PermitTypeClasses []) #-}
{-# NOINLINE minimum #-}
minimum :: Int -> Int -> IO (Maybe Int)
minimum value = withStream value S.minimum

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'minimum
inspect $ 'minimum `hasNoType` ''S.Step
inspect $ 'minimum `hasNoType` ''Fold.Step
inspect $ 'minimum `hasNoType` ''SPEC
#endif

{-# ANN sum (PermitPatternMatches [''Int]) #-}
{-# ANN sum (PermitConstructions [''Int]) #-}
{-# ANN sum (PermitTypeClasses []) #-}
{-# NOINLINE sum #-}
sum :: Int -> Int -> IO Int
sum value = withStream value (S.fold Fold.sum)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'sum
inspect $ 'sum `hasNoType` ''S.Step
inspect $ 'sum `hasNoType` ''Fold.Step
inspect $ 'sum `hasNoType` ''SPEC
#endif

{-# ANN product (PermitPatternMatches [''Int]) #-}
{-# ANN product (PermitConstructions [''Int]) #-}
{-# ANN product (PermitTypeClasses []) #-}
{-# NOINLINE product #-}
product :: Int -> Int -> IO Int
product value = withStream value (S.fold Fold.product)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'product
inspect $ 'product `hasNoType` ''S.Step
inspect $ 'product `hasNoType` ''Fold.Step
inspect $ 'product `hasNoType` ''SPEC
#endif

{-# ANN minimumBy (PermitPatternMatches [''Int]) #-}
{-# ANN minimumBy (PermitConstructions [''Maybe,''Int]) #-}
{-# ANN minimumBy (PermitTypeClasses []) #-}
{-# NOINLINE minimumBy #-}
minimumBy :: Int -> Int -> IO (Maybe Int)
minimumBy value = withStream value (S.minimumBy compare)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'minimumBy
inspect $ 'minimumBy `hasNoType` ''S.Step
inspect $ 'minimumBy `hasNoType` ''Fold.Step
inspect $ 'minimumBy `hasNoType` ''SPEC
#endif

{-# ANN maximumBy (PermitPatternMatches [''Int]) #-}
{-# ANN maximumBy (PermitConstructions [''Maybe,''Int]) #-}
{-# ANN maximumBy (PermitTypeClasses []) #-}
{-# NOINLINE maximumBy #-}
maximumBy :: Int -> Int -> IO (Maybe Int)
maximumBy value = withStream value (S.maximumBy compare)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'maximumBy
inspect $ 'maximumBy `hasNoType` ''S.Step
inspect $ 'maximumBy `hasNoType` ''Fold.Step
inspect $ 'maximumBy `hasNoType` ''SPEC
#endif

{-# ANN the (PermitPatternMatches [''Int]) #-}
{-# ANN the (PermitConstructions [''Int,''Maybe]) #-}
{-# ANN the (PermitTypeClasses []) #-}
{-# NOINLINE the #-}
the :: Int -> Int -> IO (Maybe Int)
the value = S.the . repeat value

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'the
inspect $ 'the `hasNoType` ''S.Step
inspect $ 'the `hasNoType` ''Fold.Step
inspect $ 'the `hasNoType` ''SPEC
#endif

{-# ANN indexOp (PermitPatternMatches [''Int]) #-}
{-# ANN indexOp (PermitConstructions [''Int,''Maybe]) #-}
{-# ANN indexOp (PermitTypeClasses []) #-}
{-# NOINLINE indexOp #-}
indexOp :: Int -> Int -> IO (Maybe Int)
indexOp value = withStream value (S.!! value)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'indexOp
inspect $ 'indexOp `hasNoType` ''S.Step
inspect $ 'indexOp `hasNoType` ''Fold.Step
inspect $ 'indexOp `hasNoType` ''SPEC
#endif

{-# ANN lookupNever (PermitPatternMatches [''Int]) #-}
{-# ANN lookupNever (PermitConstructions [''Int,''Maybe]) #-}
{-# ANN lookupNever (PermitTypeClasses []) #-}
{-# NOINLINE lookupNever #-}
lookupNever :: Int -> Int -> IO (Maybe Int)
lookupNever value =
    withStream value (S.lookup (value + 1) . S.map (\x -> (x, x)))

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'lookupNever
inspect $ 'lookupNever `hasNoType` ''S.Step
inspect $ 'lookupNever `hasNoType` ''Fold.Step
inspect $ 'lookupNever `hasNoType` ''SPEC
#endif

-- {-# ANN toListRev (PermitPatternMatches []) #-}
{-# ANN toListRev (PermitConstructions [''Int,''[],''SPEC]) #-}
{-# ANN toListRev (PermitTypeClasses []) #-}
{-# NOINLINE toListRev #-}
toListRev :: Int -> Int -> IO [Int]
toListRev value = withStream value S.toListRev

-- NOTE: this is a Fold benchmark, used here only for comparison with toListRev
-- {-# ANN toStreamRev (PermitPatternMatches []) #-}
{-# ANN toStreamRev (PermitConstructions [''Int,''[],''Stream,''S.Step,''SPEC]) #-}
{-# ANN toStreamRev (PermitTypeClasses []) #-}
{-# NOINLINE toStreamRev #-}
toStreamRev :: Int -> Int -> IO (Stream Identity Int)
toStreamRev value = withStream value (S.fold Fold.toStreamRev)

-- NOTE: this is a Fold benchmark, used here only for comparison with ToList
{-# ANN toStream (PermitPatternMatches [''Int,''[]]) #-}
{-# ANN toStream (PermitConstructions [''Int,''[],''Stream,''S.Step]) #-}
{-# ANN toStream (PermitTypeClasses []) #-}
{-# NOINLINE toStream #-}
toStream :: Int -> Int -> IO (Stream Identity Int)
toStream value = withStream value (S.fold Fold.toStream)

-------------------------------------------------------------------------------
-- Multi-stream folds
-------------------------------------------------------------------------------

{-# ANN isPrefixOf (PermitPatternMatches [''Int]) #-}
{-# ANN isPrefixOf (PermitConstructions [''Bool]) #-}
{-# ANN isPrefixOf (PermitTypeClasses []) #-}
{-# NOINLINE isPrefixOf #-}
isPrefixOf :: Int -> Int -> IO Bool
isPrefixOf value = withStream value (\src -> S.isPrefixOf src src)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'isPrefixOf
inspect $ 'isPrefixOf `hasNoType` ''S.Step
inspect $ 'isPrefixOf `hasNoType` ''Fold.Step
inspect $ 'isPrefixOf `hasNoType` ''SPEC
#endif

{-# ANN isSubsequenceOf (PermitPatternMatches [''Int]) #-}
{-# ANN isSubsequenceOf (PermitConstructions [''Bool]) #-}
{-# ANN isSubsequenceOf (PermitTypeClasses []) #-}
{-# NOINLINE isSubsequenceOf #-}
isSubsequenceOf :: Int -> Int -> IO Bool
isSubsequenceOf value = withStream value (\src -> S.isSubsequenceOf src src)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'isSubsequenceOf
inspect $ 'isSubsequenceOf `hasNoType` ''S.Step
inspect $ 'isSubsequenceOf `hasNoType` ''Fold.Step
inspect $ 'isSubsequenceOf `hasNoType` ''SPEC
#endif

{-# ANN stripPrefix (PermitPatternMatches [''Int]) #-}
{-# ANN stripPrefix (PermitConstructions [''()]) #-}
{-# ANN stripPrefix (PermitTypeClasses []) #-}
{-# NOINLINE stripPrefix #-}
stripPrefix :: Int -> Int -> IO ()
stripPrefix value = withStream value (\src -> do
    _ <- S.stripPrefix src src
    return ())

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'stripPrefix
inspect $ 'stripPrefix `hasNoType` ''S.Step
inspect $ 'stripPrefix `hasNoType` ''Fold.Step
inspect $ 'stripPrefix `hasNoType` ''SPEC
#endif

-------------------------------------------------------------------------------
-- Iterating using tail
-------------------------------------------------------------------------------

{-# ANN tail (PermitPatternMatches [''Int,''S.Step]) #-}
{-# ANN tail (PermitConstructions [''Int,''S.Step]) #-}
{-# ANN tail (PermitTypeClasses []) #-}
{-# NOINLINE tail #-}
tail :: Int -> Int -> IO ()
tail value = withStream value go
    where go s = S.tail s >>= Prelude.mapM_ go

{-# ANN nullHeadTail (PermitPatternMatches [''Int,''S.Step]) #-}
{-# ANN nullHeadTail (PermitConstructions [''Int,''SVar.State,''Maybe,''S.Step,''Bool]) #-}
{-# ANN nullHeadTail (PermitTypeClasses []) #-}
{-# NOINLINE nullHeadTail #-}
nullHeadTail :: Int -> Int -> IO ()
nullHeadTail value = withStream value go
    where
    go s = do
        r <- S.null s
        when (not r) $ do
            _ <- S.head s
            S.tail s >>= Prelude.mapM_ go

{-# ANN nullTail (PermitPatternMatches [''Int,''S.Step]) #-}
{-# ANN nullTail (PermitConstructions [''Int,''SVar.State,''Maybe,''S.Step,''Bool]) #-}
{-# ANN nullTail (PermitTypeClasses []) #-}
{-# NOINLINE nullTail #-}
nullTail :: Int -> Int -> IO ()
nullTail value = withStream value go
    where
    go s = do
        r <- S.null s
        when (not r) $ S.tail s >>= Prelude.mapM_ go

{-# ANN headTail (PermitPatternMatches [''Int,''S.Step]) #-}
{-# ANN headTail (PermitConstructions [''Int,''SVar.State,''Maybe,''S.Step,''Bool]) #-}
{-# ANN headTail (PermitTypeClasses []) #-}
{-# NOINLINE headTail #-}
headTail :: Int -> Int -> IO ()
headTail value = withStream value go
    where
    go s = do
        h <- S.head s
        when (isJust h) $ S.tail s >>= Prelude.mapM_ go

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

-- In addition to gauge options, the number of elements in the stream can be
-- passed using the --stream-size option.
--
benchmarks :: Int -> [(SpaceComplexity, Benchmark)]
benchmarks size =
    -- Basic folds
      [ (SpaceO_1, benchIO "foldl1'/IO" $ foldl1'Reduce size)
      , (SpaceO_1, benchIO "foldl1'/Identity" $ foldl1'ReduceIdentity size)

      -- deconstruction
      , (SpaceO_1, benchIO "mapM_" $ mapM_ size)
      , (SpaceO_1, benchIO "last" $ streamLast size)
      , (SpaceO_1, benchIO "init" $ streamInit size)

      -- this is too fast, causes all benchmarks reported in ns
    -- , benchIO "head" $ ...
      , (SpaceO_1, benchIO "length" $ length size)
      , (SpaceO_1, benchIO "sum" $ sum size)
      , (SpaceO_1, benchIO "product" $ product size)
      , (SpaceO_1, benchIO "maximumBy" $ maximumBy size)
      , (SpaceO_1, benchIO "maximum" $ maximum size)
      , (SpaceO_1, benchIO "minimumBy" $ minimumBy size)
      , (SpaceO_1, benchIO "minimum" $ minimum size)

      , (SpaceO_1, benchIO "the" $ the size)
      , (SpaceO_1, benchIO "find" $ find size)
      , (SpaceO_1, benchIO "findM" $ findM size)
      -- , benchIO "lookupFirst" $ ...
      , (SpaceO_1, benchIO "lookupNever" $ lookupNever size)
      , (SpaceO_1, benchIO "(!!)" $ indexOp size)
      , (SpaceO_1, benchIO "elem" $ elem size)
      , (SpaceO_1, benchIO "notElem" $ notElem size)
      , (SpaceO_1, benchIO "all" $ all size)
      , (SpaceO_1, benchIO "any" $ any size)
      , (SpaceO_1, benchIO "and" $ and size)
      , (SpaceO_1, benchIO "or" $ or size)

      , (SpaceO_1, benchIO "isPrefixOf" $ isPrefixOf size)
      , (SpaceO_1, benchIO "isSubsequenceOf" $ isSubsequenceOf size)
      , (SpaceO_1, benchIO "stripPrefix" $ stripPrefix size)

      -- Converting the stream to a list or pure stream in a strict monad
      , (HeapO_n, benchIO "toListRev" $ toListRev size)
      , (HeapO_n, benchIO "toStreamRev" $ toStreamRev size)

      -- Converting the stream to a list or pure stream in a strict monad
      , (SpaceO_n, benchIO "toStream" $ toStream size)

      , (StackO_n, benchIO "iterated/tail" $ tail size)
      , (StackO_n, benchIO "iterated/nullTail" $ nullTail size)
      , (StackO_n, benchIO "iterated/headTail" $ headTail size)
      , (StackO_n, benchIO "iterated/nullHeadTail" $ nullHeadTail size)
      ]
