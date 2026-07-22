-- |
-- Module      : Stream.Type.Basic
-- Copyright   : (c) 2018 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

{-# OPTIONS_GHC -Wno-orphans #-}

#ifdef __HADDOCK_VERSION__
#undef INSPECTION
#endif

#ifdef INSPECTION
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fplugin Test.Inspection.Plugin #-}
#endif

-- Benchmarks for the basic stream operations: construction, the 'Foldable',
-- 'Show', 'Eq' and 'Ord' instances, reductions, mapping and filtering. This
-- module also hosts the low level benchmarking helpers shared by the other
-- @Stream.Type.*@ modules.
module Stream.Type.Basic
    ( benchmarks
    , benchIO
    , withDrain
    , withDrainPure
    , withRandomInt
    , withStream
    , withPureStream
    ) where

#ifdef INSPECTION
import Test.Inspection
import qualified Streamly.Internal.Data.Fold as FL
import qualified Streamly.Internal.Data.Producer as Producer
#endif

import GHC.Types (SPEC(..))
import GHC.Classes (IP)
import GHC.Stack (CallStack, SrcLoc)
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO(..))
import Control.DeepSeq (NFData(..))
import Data.Functor.Identity (Identity(..), runIdentity)
import Streamly.Internal.Data.Maybe.Strict (Maybe'(..))
import Streamly.Internal.Data.Stream (Stream)
import System.Random (randomRIO)

import qualified Data.Foldable as F
import qualified GHC.Exts as GHC

import qualified Streamly.Internal.Data.Fold as Fold
import qualified Streamly.Internal.Data.Stream as S
import qualified Streamly.Internal.Data.Stream as Stream

import Test.Tasty.Bench
import Stream.Common hiding (benchIO, drain)
import Streamly.Benchmark.Common
import Fusion.Plugin.Types
import Prelude hiding (mapM)

{-# INLINE benchIO #-}
benchIO :: NFData b => String -> (Int -> IO b) -> Benchmark
benchIO name f = bench name $ nfIO $ randomRIO (1, 1 :: Int) >>= f

{-# INLINE withDrain #-}
withDrain :: (Int -> Stream IO a) -> Int -> IO ()
withDrain f = S.drain . f

{-# INLINE withDrainPure #-}
withDrainPure :: (Int -> Stream Identity a) -> Int -> IO ()
withDrainPure f n = return $! runIdentity $ S.drain (f n)

{-# INLINE withRandomInt #-}
withRandomInt :: (Int -> b) -> Int -> IO b
withRandomInt f n = return (f n)

{-# INLINE withStream #-}
withStream :: Int -> (Stream IO Int -> IO b) -> Int -> IO b
withStream value f = f . sourceUnfoldrM value

{-# INLINE withPureStream #-}
withPureStream :: Int -> (Stream Identity Int -> b) -> Int -> IO b
withPureStream value f n = return (f (sourceUnfoldr value n))

-------------------------------------------------------------------------------
-- fromList
-------------------------------------------------------------------------------

{-# ANN fromList (PermitPatternMatches [''[],''Int]) #-}
{-# ANN fromList (PermitConstructions [''[],''Int,''()]) #-}
{-# ANN fromList (PermitTypeClasses []) #-}
{-# NOINLINE fromList #-}
fromList :: Int -> Int -> IO ()
fromList value = withDrain $ \n -> Stream.fromList [n..n+value]

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'fromList
inspect $ 'fromList `hasNoType` ''Stream.Step
inspect $ 'fromList `hasNoType` ''Fold.Step
inspect $ 'fromList `hasNoType` ''SPEC
#endif

-- | 'fromTuple' yields two elements per tuple. To emit and drain ~value
-- elements we generate value/2 tuples and reduce each tuple's 'fromTuple'
-- stream with a light 'sum' fold (avoiding a heavy, non-fusible 'concatMap'
-- that would mask the cost of 'fromTuple').
{-# ANN fromTuple (PermitPatternMatches [''[],''(,),''Int]) #-}
{-# ANN fromTuple (PermitConstructions [''[],''Int,''(,),''()]) #-}
{-# ANN fromTuple (PermitTypeClasses []) #-}
{-# NOINLINE fromTuple #-}
fromTuple :: Int -> Int -> IO ()
fromTuple value = withDrain $ \n ->
    Stream.mapM (Stream.fold Fold.sum . Stream.fromTuple)
        $ Stream.fromList (fmap (\i -> (i, i)) [n .. n + value `div` 2])

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'fromTuple
inspect $ 'fromTuple `hasNoType` ''Stream.Step
inspect $ 'fromTuple `hasNoType` ''Producer.TupleState
inspect $ 'fromTuple `hasNoType` ''Fold.Step
inspect $ 'fromTuple `hasNoType` ''SPEC
#endif

{-# ANN fromList_IsList (PermitPatternMatches [''[],''Int]) #-}
{-# ANN fromList_IsList (PermitConstructions [''[],''Int,''()]) #-}
{-# ANN fromList_IsList (PermitTypeClasses []) #-}
{-# NOINLINE fromList_IsList #-}
fromList_IsList :: Int -> Int -> IO ()
fromList_IsList value = withDrainPure $ \n -> GHC.fromList [n..n+value]

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'fromList_IsList
inspect $ 'fromList_IsList `hasNoType` ''Stream.Step
inspect $ 'fromList_IsList `hasNoType` ''Fold.Step
inspect $ 'fromList_IsList `hasNoType` ''SPEC
#endif

{-# ANN fromString_IsString (PermitPatternMatches [''[],''Int]) #-}
{-# ANN fromString_IsString (PermitConstructions [''Char,''[],''()]) #-}
{-# ANN fromString_IsString (PermitTypeClasses []) #-}
{-# NOINLINE fromString_IsString #-}
fromString_IsString :: Int -> Int -> IO ()
fromString_IsString value = withDrainPure $ \n ->
    GHC.fromString (Prelude.replicate (n + value) 'a')

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'fromString_IsString
inspect $ 'fromString_IsString `hasNoType` ''Stream.Step
inspect $ 'fromString_IsString `hasNoType` ''Fold.Step
inspect $ 'fromString_IsString `hasNoType` ''SPEC
#endif

{-# INLINE read_ReadInstance #-}
read_ReadInstance :: String -> Stream Identity Int
read_ReadInstance str =
    let r = reads str
    in case r of
        [(x,"")] -> x
        _ -> error "read_ReadInstance: no parse"

-- For comparisons
{-# ANN read_HaskellLists (PermitPatternMatches [''[],''(,)]) #-}
{-# ANN read_HaskellLists (PermitConstructions [''Int,''SrcLoc,''CallStack]) #-}
{-# ANN read_HaskellLists (PermitTypeClasses [''IP]) #-}
{-# NOINLINE read_HaskellLists #-}
read_HaskellLists :: String -> [Int]
read_HaskellLists str =
    let r = reads str
    in case r of
        [(x,"")] -> x
        _ -> error "read_HaskellLists: no parse"

instance NFData a => NFData (Stream Identity a) where
    {-# INLINE rnf #-}
    rnf xs = runIdentity $ Stream.fold (Fold.foldl' (\_ x -> rnf x) ()) xs

-------------------------------------------------------------------------------
-- Foldable Instance
-------------------------------------------------------------------------------

{-# ANN foldl'_Foldable (PermitPatternMatches [''Int]) #-}
{-# ANN foldl'_Foldable (PermitConstructions [''Int]) #-}
{-# ANN foldl'_Foldable (PermitTypeClasses []) #-}
{-# NOINLINE foldl'_Foldable #-}
foldl'_Foldable :: Int -> Int -> Int
foldl'_Foldable value n =
    F.foldl' (+) 0 (sourceUnfoldr value n :: Stream Identity Int)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'foldl'_Foldable
inspect $ 'foldl'_Foldable `hasNoType` ''Stream.Step
#endif

{-# ANN foldr_Elem_Foldable (PermitPatternMatches [''Int]) #-}
{-# ANN foldr_Elem_Foldable (PermitConstructions [''Bool]) #-}
{-# ANN foldr_Elem_Foldable (PermitTypeClasses []) #-}
{-# NOINLINE foldr_Elem_Foldable #-}
foldr_Elem_Foldable :: Int -> Int -> Bool
foldr_Elem_Foldable value n =
    F.foldr (\x xs -> x == value || xs)
            False
            (sourceUnfoldr value n :: Stream Identity Int)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'foldr_Elem_Foldable
inspect $ 'foldr_Elem_Foldable `hasNoType` ''Stream.Step
inspect $ 'foldr_Elem_Foldable `hasNoType` ''Fold.Step
inspect $ 'foldr_Elem_Foldable `hasNoType` ''SPEC
#endif

{-# ANN sum_Foldable (PermitPatternMatches [''Int]) #-}
{-# ANN sum_Foldable (PermitConstructions [''Int]) #-}
{-# ANN sum_Foldable (PermitTypeClasses []) #-}
{-# NOINLINE sum_Foldable #-}
sum_Foldable :: Int -> Int -> Int
sum_Foldable value n =
    Prelude.sum (sourceUnfoldr value n :: Stream Identity Int)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'sum_Foldable
inspect $ 'sum_Foldable `hasNoType` ''Stream.Step
inspect $ 'sum_Foldable `hasNoType` ''Fold.Step
inspect $ 'sum_Foldable `hasNoType` ''SPEC
#endif

{-# ANN product_Foldable (PermitPatternMatches [''Int]) #-}
{-# ANN product_Foldable (PermitConstructions [''Int]) #-}
{-# ANN product_Foldable (PermitTypeClasses []) #-}
{-# NOINLINE product_Foldable #-}
product_Foldable :: Int -> Int -> Int
product_Foldable value n =
    Prelude.product (sourceUnfoldr value n :: Stream Identity Int)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'product_Foldable
inspect $ 'product_Foldable `hasNoType` ''Stream.Step
inspect $ 'product_Foldable `hasNoType` ''Fold.Step
inspect $ 'product_Foldable `hasNoType` ''SPEC
#endif

{-# INLINE _null_Foldable #-}
_null_Foldable :: Int -> Int -> Bool
_null_Foldable value n =
    Prelude.null (sourceUnfoldr value n :: Stream Identity Int)

{-# ANN elem_Foldable (PermitPatternMatches [''Int]) #-}
{-# ANN elem_Foldable (PermitConstructions [''Bool]) #-}
{-# ANN elem_Foldable (PermitTypeClasses []) #-}
{-# NOINLINE elem_Foldable #-}
elem_Foldable :: Int -> Int -> Bool
elem_Foldable value n =
    value `Prelude.elem` (sourceUnfoldr value n :: Stream Identity Int)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'elem_Foldable
inspect $ 'elem_Foldable `hasNoType` ''Stream.Step
inspect $ 'elem_Foldable `hasNoType` ''Fold.Step
inspect $ 'elem_Foldable `hasNoType` ''SPEC
#endif

{-# ANN notElem_Foldable (PermitPatternMatches [''Int]) #-}
{-# ANN notElem_Foldable (PermitConstructions [''Bool]) #-}
{-# ANN notElem_Foldable (PermitTypeClasses []) #-}
{-# NOINLINE notElem_Foldable #-}
notElem_Foldable :: Int -> Int -> Bool
notElem_Foldable value n =
    value `Prelude.notElem` (sourceUnfoldr value n :: Stream Identity Int)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'notElem_Foldable
inspect $ 'notElem_Foldable `hasNoType` ''Stream.Step
inspect $ 'notElem_Foldable `hasNoType` ''Fold.Step
inspect $ 'notElem_Foldable `hasNoType` ''SPEC
#endif

{-# ANN find_Foldable (PermitPatternMatches [''Int]) #-}
{-# ANN find_Foldable (PermitConstructions [''Maybe,''Int]) #-}
{-# ANN find_Foldable (PermitTypeClasses []) #-}
{-# NOINLINE find_Foldable #-}
find_Foldable :: Int -> Int -> Maybe Int
find_Foldable value n =
    F.find (== (value + 1)) (sourceUnfoldr value n :: Stream Identity Int)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'find_Foldable
inspect $ 'find_Foldable `hasNoType` ''Stream.Step
inspect $ 'find_Foldable `hasNoType` ''Fold.Step
inspect $ 'find_Foldable `hasNoType` ''SPEC
#endif

{-# ANN all_Foldable (PermitPatternMatches [''Int]) #-}
{-# ANN all_Foldable (PermitConstructions [''Bool]) #-}
{-# ANN all_Foldable (PermitTypeClasses []) #-}
{-# NOINLINE all_Foldable #-}
all_Foldable :: Int -> Int -> Bool
all_Foldable value n =
    Prelude.all (<= (value + 1)) (sourceUnfoldr value n :: Stream Identity Int)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'all_Foldable
inspect $ 'all_Foldable `hasNoType` ''Stream.Step
inspect $ 'all_Foldable `hasNoType` ''Fold.Step
inspect $ 'all_Foldable `hasNoType` ''SPEC
#endif

{- HLINT ignore "Use any"-}
{-# ANN any_Foldable (PermitPatternMatches [''Int]) #-}
{-# ANN any_Foldable (PermitConstructions [''Bool]) #-}
{-# ANN any_Foldable (PermitTypeClasses []) #-}
{-# NOINLINE any_Foldable #-}
any_Foldable :: Int -> Int -> Bool
any_Foldable value n =
    Prelude.any (> (value + 1)) (sourceUnfoldr value n :: Stream Identity Int)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'any_Foldable
inspect $ 'any_Foldable `hasNoType` ''Stream.Step
inspect $ 'any_Foldable `hasNoType` ''Fold.Step
inspect $ 'any_Foldable `hasNoType` ''SPEC
#endif

{- HLINT ignore "Use all"-}
{-# ANN and_Foldable (PermitPatternMatches [''Int]) #-}
{-# ANN and_Foldable (PermitConstructions [''Bool]) #-}
{-# ANN and_Foldable (PermitTypeClasses []) #-}
{-# NOINLINE and_Foldable #-}
and_Foldable :: Int -> Int -> Bool
and_Foldable value n =
    Prelude.and $ fmap
        (<= (value + 1)) (sourceUnfoldr value n :: Stream Identity Int)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'and_Foldable
inspect $ 'and_Foldable `hasNoType` ''Stream.Step
inspect $ 'and_Foldable `hasNoType` ''Fold.Step
inspect $ 'and_Foldable `hasNoType` ''SPEC
#endif

{- HLINT ignore "Use any"-}
{-# ANN or_Foldable (PermitPatternMatches [''Int]) #-}
{-# ANN or_Foldable (PermitConstructions [''Bool]) #-}
{-# ANN or_Foldable (PermitTypeClasses []) #-}
{-# NOINLINE or_Foldable #-}
or_Foldable :: Int -> Int -> Bool
or_Foldable value n =
    Prelude.or $ fmap
        (> (value + 1)) (sourceUnfoldr value n :: Stream Identity Int)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'or_Foldable
inspect $ 'or_Foldable `hasNoType` ''Stream.Step
inspect $ 'or_Foldable `hasNoType` ''Fold.Step
inspect $ 'or_Foldable `hasNoType` ''SPEC
#endif

{-# ANN length_Foldable (PermitPatternMatches [''Int]) #-}
{-# ANN length_Foldable (PermitConstructions [''Int]) #-}
{-# ANN length_Foldable (PermitTypeClasses []) #-}
{-# NOINLINE length_Foldable #-}
length_Foldable :: Int -> Int -> Int
length_Foldable value n =
    Prelude.length (sourceUnfoldr value n :: Stream Identity Int)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'length_Foldable
inspect $ 'length_Foldable `hasNoType` ''Stream.Step
inspect $ 'length_Foldable `hasNoType` ''Fold.Step
inspect $ 'length_Foldable `hasNoType` ''SPEC
#endif

{-# ANN minimum_Foldable (PermitPatternMatches [''Int,''Maybe']) #-}
{-# ANN minimum_Foldable (PermitConstructions [''Maybe',''Int]) #-}
{-# ANN minimum_Foldable (PermitTypeClasses []) #-}
{-# NOINLINE minimum_Foldable #-}
minimum_Foldable :: Int -> Int -> Int
minimum_Foldable value n =
    Prelude.minimum (sourceUnfoldr value n :: Stream Identity Int)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'minimum_Foldable
inspect $ 'minimum_Foldable `hasNoType` ''Stream.Step
inspect $ 'minimum_Foldable `hasNoType` ''Fold.Step
inspect $ 'minimum_Foldable `hasNoType` ''SPEC
#endif

{-# ANN min_OrdInstance (PermitPatternMatches [''Int]) #-}
{-# ANN min_OrdInstance (PermitConstructions [''()]) #-}
{-# ANN min_OrdInstance (PermitTypeClasses []) #-}
{-# NOINLINE min_OrdInstance #-}
min_OrdInstance :: Int -> Int -> ()
min_OrdInstance value n =
    let src = sourceUnfoldr value n
     in runIdentity $ S.drain $ min src src

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'min_OrdInstance
inspect $ 'min_OrdInstance `hasNoType` ''Stream.Step
inspect $ 'min_OrdInstance `hasNoType` ''Fold.Step
inspect $ 'min_OrdInstance `hasNoType` ''SPEC
#endif

{-# ANN maximum_Foldable (PermitPatternMatches [''Int,''Maybe']) #-}
{-# ANN maximum_Foldable (PermitConstructions [''Maybe',''Int]) #-}
{-# ANN maximum_Foldable (PermitTypeClasses []) #-}
{-# NOINLINE maximum_Foldable #-}
maximum_Foldable :: Int -> Int -> Int
maximum_Foldable value n =
    Prelude.maximum (sourceUnfoldr value n :: Stream Identity Int)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'maximum_Foldable
inspect $ 'maximum_Foldable `hasNoType` ''Stream.Step
inspect $ 'maximum_Foldable `hasNoType` ''Fold.Step
inspect $ 'maximum_Foldable `hasNoType` ''SPEC
#endif

{-# ANN minimumBy_Foldable (PermitPatternMatches [''Int,''Maybe]) #-}
{-# ANN minimumBy_Foldable (PermitConstructions [''Maybe,''Int]) #-}
{-# ANN minimumBy_Foldable (PermitTypeClasses []) #-}
{-# NOINLINE minimumBy_Foldable #-}
minimumBy_Foldable :: Int -> Int -> Int
minimumBy_Foldable value n =
    F.minimumBy compare (sourceUnfoldr value n :: Stream Identity Int)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'minimumBy_Foldable
inspect $ 'minimumBy_Foldable `hasNoType` ''Stream.Step
inspect $ 'minimumBy_Foldable `hasNoType` ''Fold.Step
inspect $ 'minimumBy_Foldable `hasNoType` ''SPEC
#endif

{-# ANN minimumBy_List_Foldable (PermitPatternMatches []) #-}
{-# ANN minimumBy_List_Foldable (PermitConstructions [''Int]) #-}
{-# ANN minimumBy_List_Foldable (PermitTypeClasses []) #-}
{-# NOINLINE minimumBy_List_Foldable #-}
minimumBy_List_Foldable :: Int -> Int -> Int
minimumBy_List_Foldable value n = F.minimumBy compare [1..value+n]

{-# ANN maximumBy_Foldable (PermitPatternMatches [''Int,''Maybe]) #-}
{-# ANN maximumBy_Foldable (PermitConstructions [''Maybe,''Int]) #-}
{-# ANN maximumBy_Foldable (PermitTypeClasses []) #-}
{-# NOINLINE maximumBy_Foldable #-}
maximumBy_Foldable :: Int -> Int -> Int
maximumBy_Foldable value n =
    F.maximumBy compare (sourceUnfoldr value n :: Stream Identity Int)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'maximumBy_Foldable
inspect $ 'maximumBy_Foldable `hasNoType` ''Stream.Step
inspect $ 'maximumBy_Foldable `hasNoType` ''Fold.Step
inspect $ 'maximumBy_Foldable `hasNoType` ''SPEC
#endif

{-# ANN toList_Foldable (PermitPatternMatches [''Int]) #-}
{-# ANN toList_Foldable (PermitConstructions [''[],''Int]) #-}
{-# ANN toList_Foldable (PermitTypeClasses []) #-}
{-# NOINLINE toList_Foldable #-}
toList_Foldable :: Int -> Int -> [Int]
toList_Foldable value n =
    F.toList (sourceUnfoldr value n :: Stream Identity Int)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'toList_Foldable
inspect $ 'toList_Foldable `hasNoType` ''Stream.Step
inspect $ 'toList_Foldable `hasNoType` ''Fold.Step
inspect $ 'toList_Foldable `hasNoType` ''SPEC
#endif

{-# ANN mapM__Foldable (PermitPatternMatches [''Int]) #-}
{-# ANN mapM__Foldable (PermitConstructions [''()]) #-}
{-# ANN mapM__Foldable (PermitTypeClasses []) #-}
{-# NOINLINE mapM__Foldable #-}
mapM__Foldable :: Int -> Int -> IO ()
mapM__Foldable value n =
    F.mapM_ (\_ -> return ()) (sourceUnfoldr value n :: Stream Identity Int)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'mapM__Foldable
inspect $ 'mapM__Foldable `hasNoType` ''Stream.Step
inspect $ 'mapM__Foldable `hasNoType` ''Fold.Step
inspect $ 'mapM__Foldable `hasNoType` ''SPEC
#endif

{-# ANN sequence__Foldable (PermitPatternMatches [''Int]) #-}
{-# ANN sequence__Foldable (PermitConstructions [''()]) #-}
{-# ANN sequence__Foldable (PermitTypeClasses []) #-}
{-# NOINLINE sequence__Foldable #-}
sequence__Foldable :: Int -> Int -> IO ()
sequence__Foldable value n =
    F.sequence_ (sourceUnfoldrAction value n :: Stream Identity (IO Int))

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'sequence__Foldable
inspect $ 'sequence__Foldable `hasNoType` ''Stream.Step
inspect $ 'sequence__Foldable `hasNoType` ''Fold.Step
inspect $ 'sequence__Foldable `hasNoType` ''SPEC
#endif

{-# INLINE _msum_Foldable #-}
_msum_Foldable :: Int -> Int -> IO Int
_msum_Foldable value n =
    F.msum (sourceUnfoldrAction value n :: Stream Identity (IO Int))

-------------------------------------------------------------------------------
-- Show instance
-------------------------------------------------------------------------------

{-# ANN show_ShowInstance (PermitPatternMatches [''Int]) #-}
{-# ANN show_ShowInstance (PermitConstructions
    [''Int,''Stream.Step,''Stream]) #-}
{-# ANN show_ShowInstance (PermitTypeClasses [''Show]) #-}
{-# NOINLINE show_ShowInstance #-}
show_ShowInstance :: Int -> Int -> IO String
show_ShowInstance value = withPureStream value show

{-# ANN show_HaskellLists (PermitPatternMatches []) #-}
{-# ANN show_HaskellLists (PermitConstructions []) #-}
{-# ANN show_HaskellLists (PermitTypeClasses []) #-}
{-# NOINLINE show_HaskellLists #-}
show_HaskellLists :: [Int] -> String
show_HaskellLists = show

-------------------------------------------------------------------------------
-- Eq and Ord instances
-------------------------------------------------------------------------------

{-# ANN eq_EqInstance (PermitPatternMatches [''Int]) #-}
{-# ANN eq_EqInstance (PermitConstructions [''Bool]) #-}
{-# ANN eq_EqInstance (PermitTypeClasses []) #-}
{-# NOINLINE eq_EqInstance #-}
eq_EqInstance :: Int -> Int -> IO Bool
eq_EqInstance value = withPureStream value $ \src -> src == src

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'eq_EqInstance
inspect $ 'eq_EqInstance `hasNoType` ''Stream.Step
inspect $ 'eq_EqInstance `hasNoType` ''Fold.Step
inspect $ 'eq_EqInstance `hasNoType` ''SPEC
#endif

{-# ANN notEq_EqInstance (PermitPatternMatches [''Int]) #-}
{-# ANN notEq_EqInstance (PermitConstructions [''Bool]) #-}
{-# ANN notEq_EqInstance (PermitTypeClasses []) #-}
{-# NOINLINE notEq_EqInstance #-}
notEq_EqInstance :: Int -> Int -> IO Bool
notEq_EqInstance value = withPureStream value $ \src -> src /= src

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'notEq_EqInstance
inspect $ 'notEq_EqInstance `hasNoType` ''Stream.Step
inspect $ 'notEq_EqInstance `hasNoType` ''Fold.Step
inspect $ 'notEq_EqInstance `hasNoType` ''SPEC
#endif

{-# ANN lt_OrdInstance (PermitPatternMatches [''Int]) #-}
{-# ANN lt_OrdInstance (PermitConstructions [''Bool]) #-}
{-# ANN lt_OrdInstance (PermitTypeClasses []) #-}
{-# NOINLINE lt_OrdInstance #-}
lt_OrdInstance :: Int -> Int -> IO Bool
lt_OrdInstance value = withPureStream value $ \src -> src < src

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'lt_OrdInstance
inspect $ 'lt_OrdInstance `hasNoType` ''Stream.Step
inspect $ 'lt_OrdInstance `hasNoType` ''Fold.Step
inspect $ 'lt_OrdInstance `hasNoType` ''SPEC
#endif

-------------------------------------------------------------------------------
-- Reductions
-------------------------------------------------------------------------------

{-# ANN uncons (PermitPatternMatches [''Stream.Step,''Int]) #-}
{-# ANN uncons (PermitConstructions [''Int,''Stream.Step,''()]) #-}
{-# ANN uncons (PermitTypeClasses []) #-}
{-# NOINLINE uncons #-}
uncons :: Int -> Int -> IO ()
uncons value = withStream value go

    where

    go s = do
        r <- S.uncons s
        case r of
            Nothing -> return ()
            Just (_, t) -> go t

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'uncons
-- inspect $ 'uncons `hasNoType` ''S.Step
inspect $ 'uncons `hasNoType` ''Fold.Step
inspect $ 'uncons `hasNoType` ''SPEC
#endif

{-# ANN foldBreak (PermitPatternMatches [''Stream.Step,''Int]) #-}
{-# ANN foldBreak (PermitConstructions [''Int,''Stream.Step,''()]) #-}
{-# ANN foldBreak (PermitTypeClasses []) #-}
{-# NOINLINE foldBreak #-}
foldBreak :: Int -> Int -> IO ()
foldBreak value = withStream value go

    where

    go s = do
        (r, s1) <- S.foldBreak (Fold.take 1 Fold.length) s
        when (r /= 0) $ go s1

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'foldBreak
-- inspect $ 'foldBreak `hasNoType` ''S.Step
inspect $ 'foldBreak `hasNoType` ''Fold.Step
inspect $ 'foldBreak `hasNoType` ''SPEC
#endif

{-# ANN foldrM_Elem (PermitPatternMatches [''Int]) #-}
{-# ANN foldrM_Elem (PermitConstructions [''Bool]) #-}
{-# ANN foldrM_Elem (PermitTypeClasses []) #-}
{-# NOINLINE foldrM_Elem #-}
foldrM_Elem :: Int -> Int -> IO Bool
foldrM_Elem value =
    withStream value
        (S.foldrM
             (\x xs -> if x == value then return True else xs)
             (return False))

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'foldrM_Elem
inspect $ 'foldrM_Elem `hasNoType` ''S.Step
inspect $ 'foldrM_Elem `hasNoType` ''Fold.Step
inspect $ 'foldrM_Elem `hasNoType` ''SPEC
#endif

{-# ANN foldrM_Elem_Identity (PermitPatternMatches [''Int]) #-}
{-# ANN foldrM_Elem_Identity (PermitConstructions [''Bool]) #-}
{-# ANN foldrM_Elem_Identity (PermitTypeClasses []) #-}
{-# NOINLINE foldrM_Elem_Identity #-}
foldrM_Elem_Identity :: Int -> Int -> IO Bool
foldrM_Elem_Identity value =
    withPureStream value $
        runIdentity . S.foldrM
            (\x xs -> if x == value then return True else xs)
            (return False)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'foldrM_Elem_Identity
inspect $ 'foldrM_Elem_Identity `hasNoType` ''S.Step
inspect $ 'foldrM_Elem_Identity `hasNoType` ''Fold.Step
inspect $ 'foldrM_Elem_Identity `hasNoType` ''SPEC
#endif

{-# ANN foldrM_ToList (PermitPatternMatches [''Int]) #-}
{-# ANN foldrM_ToList (PermitConstructions [''[],''Int]) #-}
{-# ANN foldrM_ToList (PermitTypeClasses []) #-}
{-# NOINLINE foldrM_ToList #-}
foldrM_ToList :: Int -> Int -> IO [Int]
foldrM_ToList value =
    withStream value $ S.foldrM (\x xs -> (x :) <$> xs) (return [])

{-# ANN foldrM_ToList_Identity (PermitPatternMatches [''Int]) #-}
{-# ANN foldrM_ToList_Identity (PermitConstructions [''Int,''[]]) #-}
{-# ANN foldrM_ToList_Identity (PermitTypeClasses []) #-}
{-# NOINLINE foldrM_ToList_Identity #-}
foldrM_ToList_Identity :: Int -> Int -> IO [Int]
foldrM_ToList_Identity value =
    withPureStream value
        (runIdentity . S.foldrM (\x xs -> (x :) <$> xs) (return []))

{-# ANN foldl'_Reduce (PermitPatternMatches []) #-}
{-# ANN foldl'_Reduce (PermitConstructions []) #-}
{-# ANN foldl'_Reduce (PermitTypeClasses []) #-}
{-# NOINLINE foldl'_Reduce #-}
foldl'_Reduce :: Int -> Int -> IO Int
foldl'_Reduce value = withStream value (S.foldl' (+) 0)

{-# ANN foldl'_Reduce_Identity (PermitPatternMatches [''Int]) #-}
{-# ANN foldl'_Reduce_Identity (PermitConstructions [''Int]) #-}
{-# ANN foldl'_Reduce_Identity (PermitTypeClasses []) #-}
{-# NOINLINE foldl'_Reduce_Identity #-}
foldl'_Reduce_Identity :: Int -> Int -> IO Int
foldl'_Reduce_Identity value =
    withPureStream value $ runIdentity . S.foldl' (+) 0

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'foldl'_Reduce_Identity
inspect $ 'foldl'_Reduce_Identity `hasNoType` ''S.Step
#endif

{-# ANN foldlM'_Reduce (PermitPatternMatches []) #-}
{-# ANN foldlM'_Reduce (PermitConstructions []) #-}
{-# ANN foldlM'_Reduce (PermitTypeClasses []) #-}
{-# NOINLINE foldlM'_Reduce #-}
foldlM'_Reduce :: Int -> Int -> IO Int
foldlM'_Reduce value =
    withStream value (S.foldlM' (\xs a -> return $ a + xs) (return 0))

{-# ANN foldlM'_Reduce_Identity (PermitPatternMatches [''Int]) #-}
{-# ANN foldlM'_Reduce_Identity (PermitConstructions [''Int]) #-}
{-# ANN foldlM'_Reduce_Identity (PermitTypeClasses []) #-}
{-# NOINLINE foldlM'_Reduce_Identity #-}
foldlM'_Reduce_Identity :: Int -> Int -> IO Int
foldlM'_Reduce_Identity value =
    withPureStream value $
        runIdentity . S.foldlM' (\xs a -> return $ a + xs) (return 0)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'foldlM'_Reduce_Identity
inspect $ 'foldlM'_Reduce_Identity `hasNoType` ''S.Step
#endif

{-# ANN drain (PermitPatternMatches [''Int]) #-}
{-# ANN drain (PermitConstructions [''()]) #-}
{-# ANN drain (PermitTypeClasses []) #-}
{-# NOINLINE drain #-}
drain :: Int -> Int -> IO ()
drain value = withStream value S.drain

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'drain
inspect $ 'drain `hasNoType` ''Stream.Step
inspect $ 'drain `hasNoType` ''Fold.Step
inspect $ 'drain `hasNoType` ''SPEC
#endif

{-# ANN drain_Identity (PermitPatternMatches [''Int]) #-}
{-# ANN drain_Identity (PermitConstructions [''()]) #-}
{-# ANN drain_Identity (PermitTypeClasses []) #-}
{-# NOINLINE drain_Identity #-}
drain_Identity :: Int -> Int -> IO ()
drain_Identity value = withPureStream value $ runIdentity . S.drain

-- This has unfused constructors but those are eliminated by SpecConstr rather
-- than inlining, therefore force inlining has no use except that it issues a
-- warning.
{-# ANN drainN (PermitPatternMatches [''Int]) #-}
{-# ANN drainN (PermitConstructions [''()]) #-}
{-# ANN drainN (PermitTypeClasses []) #-}
{-# NOINLINE drainN #-}
drainN :: Int -> Int -> IO ()
drainN value = withStream value (S.fold (Fold.drainN value))

{-# ANN foldl'_Build (PermitPatternMatches [''Int,''[],''SPEC]) #-}
{-# ANN foldl'_Build (PermitConstructions [''Int,''[],''SPEC]) #-}
{-# ANN foldl'_Build (PermitTypeClasses []) #-}
{-# NOINLINE foldl'_Build #-}
foldl'_Build :: Int -> Int -> IO [Int]
foldl'_Build value = withStream value (S.foldl' (flip (:)) [])

{-# ANN foldl'_Build_Identity (PermitPatternMatches [''Int,''[],''SPEC]) #-}
{-# ANN foldl'_Build_Identity (PermitConstructions [''Int,''[],''SPEC]) #-}
{-# ANN foldl'_Build_Identity (PermitTypeClasses []) #-}
{-# NOINLINE foldl'_Build_Identity #-}
foldl'_Build_Identity :: Int -> Int -> IO [Int]
foldl'_Build_Identity value =
    withPureStream value (runIdentity . S.foldl' (flip (:)) [])

{-# ANN foldlM'_Build (PermitPatternMatches [''Int,''[],''SPEC]) #-}
{-# ANN foldlM'_Build (PermitConstructions [''Int,''[],''SPEC]) #-}
{-# ANN foldlM'_Build (PermitTypeClasses []) #-}
{-# NOINLINE foldlM'_Build #-}
foldlM'_Build :: Int -> Int -> IO [Int]
foldlM'_Build value =
    withStream value (S.foldlM' (\xs x -> return $ x : xs) (return []))

{-# ANN foldlM'_Build_Identity (PermitPatternMatches [''Int,''[],''SPEC]) #-}
{-# ANN foldlM'_Build_Identity (PermitConstructions [''Int,''[],''SPEC]) #-}
{-# ANN foldlM'_Build_Identity (PermitTypeClasses []) #-}
{-# NOINLINE foldlM'_Build_Identity #-}
foldlM'_Build_Identity :: Int -> Int -> IO [Int]
foldlM'_Build_Identity value =
    withPureStream value
        (runIdentity . S.foldlM' (\xs x -> return $ x : xs) (return []))

{-# ANN foldrM_ToSum (PermitPatternMatches [''Int]) #-}
{-# ANN foldrM_ToSum (PermitConstructions [''Int]) #-}
{-# ANN foldrM_ToSum (PermitTypeClasses []) #-}
{-# NOINLINE foldrM_ToSum #-}
foldrM_ToSum :: Int -> Int -> IO Int
foldrM_ToSum value =
    withStream value (S.foldrM (\x xs -> (x +) <$> xs) (return 0))

{-# ANN foldrM_ToSum_Identity (PermitPatternMatches [''Int]) #-}
{-# ANN foldrM_ToSum_Identity (PermitConstructions [''Int]) #-}
{-# ANN foldrM_ToSum_Identity (PermitTypeClasses []) #-}
{-# NOINLINE foldrM_ToSum_Identity #-}
foldrM_ToSum_Identity :: Int -> Int -> IO Int
foldrM_ToSum_Identity value =
    withPureStream value
        (runIdentity . S.foldrM (\x xs -> (x +) <$> xs) (return 0))

{-# ANN toList_Stream (PermitPatternMatches [''Int]) #-}
{-# ANN toList_Stream (PermitConstructions [''[],''Int]) #-}
{-# ANN toList_Stream (PermitTypeClasses []) #-}
{-# NOINLINE toList_Stream #-}
toList_Stream :: Int -> Int -> IO [Int]
toList_Stream value = withStream value S.toList

{-# ANN eqBy_Identity (PermitPatternMatches [''Int]) #-}
{-# ANN eqBy_Identity (PermitConstructions [''Bool]) #-}
{-# ANN eqBy_Identity (PermitTypeClasses []) #-}
{-# NOINLINE eqBy_Identity #-}
eqBy_Identity :: Int -> Int -> IO Bool
eqBy_Identity value =
    withPureStream value $ \src -> runIdentity $ S.eqBy (==) src src

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'eqBy_Identity
inspect $ 'eqBy_Identity `hasNoType` ''SPEC
inspect $ 'eqBy_Identity `hasNoType` ''S.Step
inspect $ 'eqBy_Identity `hasNoType` ''Fold.Step
#endif

{-# ANN cmpBy_Identity (PermitPatternMatches [''Int]) #-}
{-# ANN cmpBy_Identity (PermitConstructions [''Ordering]) #-}
{-# ANN cmpBy_Identity (PermitTypeClasses []) #-}
{-# NOINLINE cmpBy_Identity #-}
cmpBy_Identity :: Int -> Int -> IO Ordering
cmpBy_Identity value =
    withPureStream value $ \src -> runIdentity $ S.cmpBy compare src src

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'cmpBy_Identity
inspect $ 'cmpBy_Identity `hasNoType` ''SPEC
inspect $ 'cmpBy_Identity `hasNoType` ''S.Step
inspect $ 'cmpBy_Identity `hasNoType` ''Fold.Step
#endif

{-# ANN eqBy (PermitPatternMatches [''Int]) #-}
{-# ANN eqBy (PermitConstructions [''Bool]) #-}
{-# ANN eqBy (PermitTypeClasses []) #-}
{-# NOINLINE eqBy #-}
eqBy :: Int -> Int -> IO Bool
eqBy value = withStream value $ \src -> S.eqBy (==) src src

{-# ANN cmpBy (PermitPatternMatches [''Int]) #-}
{-# ANN cmpBy (PermitConstructions [''Ordering]) #-}
{-# ANN cmpBy (PermitTypeClasses []) #-}
{-# NOINLINE cmpBy #-}
cmpBy :: Int -> Int -> IO Ordering
cmpBy value = withStream value $ \src -> S.cmpBy compare src src

-------------------------------------------------------------------------------
-- Mapping
-------------------------------------------------------------------------------

{-# INLINE mapN #-}
mapN :: Monad m => Int -> Stream m Int -> m ()
mapN n = composeN n $ fmap (+ 1)

{-# INLINE mapM #-}
mapM :: MonadAsync m => Int -> Stream m Int -> m ()
mapM n = composeN n $ Stream.mapM return

{-# ANN fmap_x1 (PermitPatternMatches [''Int]) #-}
{-# ANN fmap_x1 (PermitConstructions [''()]) #-}
{-# ANN fmap_x1 (PermitTypeClasses []) #-}
{-# NOINLINE fmap_x1 #-}
fmap_x1 :: Int -> Int -> IO ()
fmap_x1 value = withStream value (mapN 1)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'fmap_x1
inspect $ 'fmap_x1 `hasNoType` ''Stream.Step
inspect $ 'fmap_x1 `hasNoType` ''FL.Step
inspect $ 'fmap_x1 `hasNoType` ''SPEC
#endif

{-# ANN mapM_x1 (PermitPatternMatches [''Int]) #-}
{-# ANN mapM_x1 (PermitConstructions [''()]) #-}
{-# ANN mapM_x1 (PermitTypeClasses []) #-}
{-# NOINLINE mapM_x1 #-}
mapM_x1 :: Int -> Int -> IO ()
mapM_x1 value = withStream value (mapM 1)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'mapM_x1
inspect $ 'mapM_x1 `hasNoType` ''Stream.Step
inspect $ 'mapM_x1 `hasNoType` ''FL.Step
inspect $ 'mapM_x1 `hasNoType` ''SPEC
#endif

{-# ANN fmap_x4 (PermitPatternMatches [''Int]) #-}
{-# ANN fmap_x4 (PermitConstructions [''()]) #-}
{-# ANN fmap_x4 (PermitTypeClasses []) #-}
{-# NOINLINE fmap_x4 #-}
fmap_x4 :: Int -> Int -> IO ()
fmap_x4 value = withStream value (mapN 4)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'fmap_x4
inspect $ 'fmap_x4 `hasNoType` ''Stream.Step
inspect $ 'fmap_x4 `hasNoType` ''FL.Step
inspect $ 'fmap_x4 `hasNoType` ''SPEC
#endif

{-# ANN mapM_x4 (PermitPatternMatches [''Int]) #-}
{-# ANN mapM_x4 (PermitConstructions [''()]) #-}
{-# ANN mapM_x4 (PermitTypeClasses []) #-}
{-# NOINLINE mapM_x4 #-}
mapM_x4 :: Int -> Int -> IO ()
mapM_x4 value = withStream value (mapM 4)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'mapM_x4
inspect $ 'mapM_x4 `hasNoType` ''Stream.Step
inspect $ 'mapM_x4 `hasNoType` ''FL.Step
inspect $ 'mapM_x4 `hasNoType` ''SPEC
#endif

-------------------------------------------------------------------------------
-- Filtering
-------------------------------------------------------------------------------

{-# INLINE _take_One #-}
_take_One :: MonadIO m => Int -> Stream m Int -> m ()
_take_One n = composeN n $ Stream.take 1

{-# INLINE takeAll #-}
takeAll :: MonadIO m => Int -> Int -> Stream m Int -> m ()
takeAll value n = composeN n $ Stream.take (value + 1)

{-# ANN take_All_x1 (PermitPatternMatches [''Int]) #-}
{-# ANN take_All_x1 (PermitConstructions [''()]) #-}
{-# ANN take_All_x1 (PermitTypeClasses []) #-}
{-# NOINLINE take_All_x1 #-}
take_All_x1 :: Int -> Int -> IO ()
take_All_x1 value = withStream value (takeAll value 1)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'take_All_x1
inspect $ 'take_All_x1 `hasNoType` ''Stream.Step
inspect $ 'take_All_x1 `hasNoType` ''FL.Step
inspect $ 'take_All_x1 `hasNoType` ''SPEC
#endif

{-# ANN take_All_x4 (PermitPatternMatches [''Int]) #-}
{-# ANN take_All_x4 (PermitConstructions [''()]) #-}
{-# ANN take_All_x4 (PermitTypeClasses []) #-}
{-# NOINLINE take_All_x4 #-}
take_All_x4 :: Int -> Int -> IO ()
take_All_x4 value = withStream value (takeAll value 4)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'take_All_x4
inspect $ 'take_All_x4 `hasNoType` ''Stream.Step
inspect $ 'take_All_x4 `hasNoType` ''FL.Step
inspect $ 'take_All_x4 `hasNoType` ''SPEC
#endif

{-# INLINE takeWhileTrue #-}
takeWhileTrue :: MonadIO m => Int -> Int -> Stream m Int -> m ()
takeWhileTrue value n = composeN n $ Stream.takeWhile (<= (value + 1))

{-# ANN takeWhile_True_x1 (PermitPatternMatches [''Int]) #-}
{-# ANN takeWhile_True_x1 (PermitConstructions [''()]) #-}
{-# ANN takeWhile_True_x1 (PermitTypeClasses []) #-}
{-# NOINLINE takeWhile_True_x1 #-}
takeWhile_True_x1 :: Int -> Int -> IO ()
takeWhile_True_x1 value = withStream value (takeWhileTrue value 1)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'takeWhile_True_x1
inspect $ 'takeWhile_True_x1 `hasNoType` ''Stream.Step
inspect $ 'takeWhile_True_x1 `hasNoType` ''FL.Step
inspect $ 'takeWhile_True_x1 `hasNoType` ''SPEC
#endif

{-# ANN takeWhile_True_x4 (PermitPatternMatches [''Int]) #-}
{-# ANN takeWhile_True_x4 (PermitConstructions [''()]) #-}
{-# ANN takeWhile_True_x4 (PermitTypeClasses []) #-}
{-# NOINLINE takeWhile_True_x4 #-}
takeWhile_True_x4 :: Int -> Int -> IO ()
takeWhile_True_x4 value = withStream value (takeWhileTrue value 4)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'takeWhile_True_x4
inspect $ 'takeWhile_True_x4 `hasNoType` ''Stream.Step
inspect $ 'takeWhile_True_x4 `hasNoType` ''FL.Step
inspect $ 'takeWhile_True_x4 `hasNoType` ''SPEC
#endif

{-# INLINE takeWhileMTrue #-}
takeWhileMTrue :: MonadIO m => Int -> Int -> Stream m Int -> m ()
takeWhileMTrue value n =
    composeN n $ Stream.takeWhileM (return . (<= (value + 1)))

{-# ANN takeWhileM_True_x4 (PermitPatternMatches [''Int]) #-}
{-# ANN takeWhileM_True_x4 (PermitConstructions [''()]) #-}
{-# ANN takeWhileM_True_x4 (PermitTypeClasses []) #-}
{-# NOINLINE takeWhileM_True_x4 #-}
takeWhileM_True_x4 :: Int -> Int -> IO ()
takeWhileM_True_x4 value = withStream value (takeWhileMTrue value 4)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'takeWhileM_True_x4
inspect $ 'takeWhileM_True_x4 `hasNoType` ''Stream.Step
inspect $ 'takeWhileM_True_x4 `hasNoType` ''FL.Step
inspect $ 'takeWhileM_True_x4 `hasNoType` ''SPEC
#endif

-------------------------------------------------------------------------------
-- Benchmarks
-------------------------------------------------------------------------------

-- XXX if we are using Fold.* for folding then those benchmarks should be moved
-- to the Fold module. If the fold is simply a custom stream defined fold then
-- we should keep it here.

{-# ANN benchmarks "HLint: ignore" #-}
-- Benchmark naming: name each benchmark (and its IO action) after the exported
-- function it benchmarks, using combinator_dimension1_dimension2..., where the
-- dimensions are optional variants/type specializations (used esp. when more
-- than one specialization is benchmarked). Keep extra info in parenthetical
-- notes in the description; these also disambiguate benchmarks that reuse a
-- single IO action with different arguments. If the name has a trailing
-- underscore, add one more underscore.
benchmarks :: Int -> [(SpaceComplexity, Benchmark)]
benchmarks size =
    -- Construction
    [ (SpaceO_1, benchIO "fromList" $ fromList size)
    , (SpaceO_1, benchIO "fromTuple" $ fromTuple size)
    , (SpaceO_1, benchIO "fromList_IsList" $ fromList_IsList size)
    , (SpaceO_1, benchIO "fromString_IsString" $ fromString_IsString size)
    -- Buffers the output of show/read.
    -- XXX can the outputs be streaming? Can we have special read/show
    -- style type classes, readM/showM supporting streaming effects?
    , (HeapO_n, bench "read_ReadInstance (readsPrec)" $
          nf (read_ReadInstance . mkString) size)
    , (HeapO_n, bench "read_HaskellLists (readsPrec)" $
          nf (read_HaskellLists . mkListString) size)

    -- Elimination/Foldable instance
    , (SpaceO_1, benchIO "foldl'_Foldable" $
          withRandomInt (foldl'_Foldable size))
    , (SpaceO_1, benchIO "foldr_Elem_Foldable" $
          withRandomInt (foldr_Elem_Foldable size))
 -- , (SpaceO_1, benchIO "null_Foldable" $ withRandomInt (_null_Foldable size))
    , (SpaceO_1, benchIO "elem_Foldable" $ withRandomInt (elem_Foldable size))
    , (SpaceO_1, benchIO "length_Foldable" $
          withRandomInt (length_Foldable size))
    , (SpaceO_1, benchIO "sum_Foldable" $ withRandomInt (sum_Foldable size))
    , (SpaceO_1, benchIO "product_Foldable" $
          withRandomInt (product_Foldable size))
    , (SpaceO_1, benchIO "minimum_Foldable" $
          withRandomInt (minimum_Foldable size))
    , (SpaceO_1, benchIO "min_OrdInstance" $
          withRandomInt (min_OrdInstance size))
    , (SpaceO_1, benchIO "maximum_Foldable" $
          withRandomInt (maximum_Foldable size))
    , (SpaceO_1, benchIO "minimumBy_Foldable" $
          withRandomInt (minimumBy_Foldable size))
    , (SpaceO_1, benchIO "maximumBy_Foldable" $
          withRandomInt (maximumBy_Foldable size))
    , (SpaceO_1, benchIO "minimumBy_List_Foldable" $
          withRandomInt (minimumBy_List_Foldable size))
    , (SpaceO_1, benchIO "toList_length_Foldable" $
          withRandomInt (Prelude.length . toList_Foldable size))
    , (SpaceO_1, benchIO "notElem_Foldable" $
          withRandomInt (notElem_Foldable size))
    , (SpaceO_1, benchIO "find_Foldable" $ withRandomInt (find_Foldable size))
    , (SpaceO_1, benchIO "all_Foldable" $ withRandomInt (all_Foldable size))
    , (SpaceO_1, benchIO "any_Foldable" $ withRandomInt (any_Foldable size))
    , (SpaceO_1, benchIO "and_Foldable" $ withRandomInt (and_Foldable size))
    , (SpaceO_1, benchIO "or_Foldable" $ withRandomInt (or_Foldable size))

    -- Applicative and Traversable operations
    -- TBD: traverse_
    , (SpaceO_1, benchIO "mapM__Foldable" (mapM__Foldable size))
    -- TBD: for_
    -- TBD: forM_
    , (SpaceO_1, benchIO "sequence__Foldable" (sequence__Foldable size))
    -- TBD: sequenceA_
    -- TBD: asum
    -- XXX needs to be fixed, results are in ns
    -- , (SpaceO_1, benchIOSink1 "msum_Foldable" (_msum_Foldable size))

    -- Elimination/folds
    , (SpaceO_1, benchIO "foldl'_Reduce" $ foldl'_Reduce size)
    , (SpaceO_1, benchIO "foldlM'_Reduce" $ foldlM'_Reduce size)
    , (SpaceO_1, benchIO "foldl'_Reduce_Identity" $ foldl'_Reduce_Identity size)
    , (SpaceO_1, benchIO "foldlM'_Reduce_Identity" $
          foldlM'_Reduce_Identity size)
    , (SpaceO_1, benchIO "foldrM_Elem" $ foldrM_Elem size)
    , (SpaceO_1, benchIO "foldrM_Elem_Identity" $ foldrM_Elem_Identity size)
    , (SpaceO_1, benchIO "foldrM_ToList_Identity" $ foldrM_ToList_Identity size)

    -- Left folds for building a structure are inherently non-streaming
    -- as the structure cannot be lazily consumed until fully built.
    , (HeapO_n, benchIO "foldl'_Build" $ foldl'_Build size)
    , (HeapO_n, benchIO "foldl'_Build_Identity" $ foldl'_Build_Identity size)
    , (HeapO_n, benchIO "foldlM'_Build" $ foldlM'_Build size)
    , (HeapO_n, benchIO "foldlM'_Build_Identity" $ foldlM'_Build_Identity size)

    -- Head recursive strict right folds.
    -- accumulation due to strictness of IO monad
    , (SpaceO_n, benchIO "foldrM_ToList" $ foldrM_ToList size)
    -- Right folds for reducing are inherently non-streaming as the
    -- expression needs to be fully built before it can be reduced.
    , (SpaceO_n, benchIO "foldrM_ToSum_Identity" $ foldrM_ToSum_Identity size)
    , (SpaceO_n, benchIO "foldrM_ToSum" $ foldrM_ToSum size)
    -- Converting the stream to a list or pure stream in a strict monad
    , (SpaceO_n, benchIO "toList_Stream" $ toList_Stream size)

    -- this is too fast, causes all benchmarks reported in ns
    -- , (SpaceO_1, benchIO "null" $ ...)

    -- deconstruction
    , (SpaceO_1, benchIO "uncons" $ uncons size)
    , (SpaceO_1, benchIO "foldBreak" $ foldBreak size)

    -- draining
    , (SpaceO_1, benchIO "drain" $ drain size)
    , (SpaceO_1, benchIO "drainN" $ drainN size)
    , (SpaceO_1, benchIO "drain_Identity" $ drain_Identity size)

    -- length is used to check for foldr/build fusion
    , (SpaceO_1, benchIO "toList_length_IsList" $
          withPureStream size (Prelude.length . GHC.toList))

    -- Buffers the output of show/read.
    -- XXX can the outputs be streaming? Can we have special read/show
    -- style type classes, readM/showM supporting streaming effects?
    , (HeapO_n, bench "show_HaskellLists (showsPrec)" $
          nf show_HaskellLists (mkList size))
    -- XXX This is not o-1-space for GHC-8.10
    , (HeapO_n, benchIO "show_ShowInstance (showsPrec)" $
          show_ShowInstance size)

    , (SpaceO_1, benchIO "eq_EqInstance (==)" $ eq_EqInstance size)
    , (SpaceO_1, benchIO "notEq_EqInstance (/=)" $ notEq_EqInstance size)
    , (SpaceO_1, benchIO "lt_OrdInstance (<)" $ lt_OrdInstance size)
    , (SpaceO_1, benchIO "eqBy_Identity" $ eqBy_Identity size)
    , (SpaceO_1, benchIO "cmpBy_Identity" $ cmpBy_Identity size)
    , (SpaceO_1, benchIO "eqBy" $ eqBy size)
    , (SpaceO_1, benchIO "cmpBy" $ cmpBy size)

    -- Mapping
    , (SpaceO_1, benchIO "fmap_x1" $ fmap_x1 size)
    , (SpaceO_1, benchIO "fmap_x4" $ fmap_x4 size)
    , (SpaceO_1, benchIO "mapM_x1" $ mapM_x1 size)
    , (SpaceO_1, benchIO "mapM_x4" $ mapM_x4 size)

    -- Trimming
    , (SpaceO_1, benchIO "take_All_x1" $ take_All_x1 size)
    , (SpaceO_1, benchIO "takeWhile_True_x1" $ takeWhile_True_x1 size)
 -- , (SpaceO_1, benchIO "takeWhileM_True_x1" ...)
    , (SpaceO_1, benchIO "take_All_x4" $ take_All_x4 size)
    , (SpaceO_1, benchIO "takeWhile_True_x4" $ takeWhile_True_x4 size)
    , (SpaceO_1, benchIO "takeWhileM_True_x4" $ takeWhileM_True_x4 size)
    ]
