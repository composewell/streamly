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
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO(..))
import Control.DeepSeq (NFData(..))
import Data.Functor.Identity (Identity(..), runIdentity)
import Streamly.Internal.Data.Stream (Stream)
import System.Random (randomRIO)

import qualified Data.Foldable as F
import qualified GHC.Exts as GHC

import qualified Streamly.Internal.Data.Fold as Fold
import qualified Streamly.Internal.Data.Stream as S
import qualified Streamly.Internal.Data.Stream as Stream

import Test.Tasty.Bench
import Stream.Common hiding (benchIO)
import Streamly.Benchmark.Common
import Fusion.Plugin.Types
import Prelude hiding (mapM)

{-# INLINE benchIO #-}
benchIO :: NFData b => String -> (Int -> IO b) -> Benchmark
benchIO name f = bench name $ nfIO $ randomRIO (1, 1 :: Int) >>= f

{-# INLINE withDrain #-}
withDrain :: (Int -> Stream IO a) -> Int -> IO ()
withDrain f = drain . f

{-# INLINE withDrainPure #-}
withDrainPure :: (Int -> Stream Identity a) -> Int -> IO ()
withDrainPure f n = return $! runIdentity $ drain (f n)

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

{-# ANN sourceFromList (PermitPatternMatches [''[], ''Int]) #-}
{-# ANN sourceFromList (PermitConstructions [''[],''Int,''()]) #-}
{-# ANN sourceFromList (PermitTypeClasses []) #-}
{-# NOINLINE sourceFromList #-}
sourceFromList :: Int -> Int -> IO ()
sourceFromList value = withDrain $ \n -> Stream.fromList [n..n+value]

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'sourceFromList
inspect $ 'sourceFromList `hasNoType` ''Stream.Step
inspect $ 'sourceFromList `hasNoType` ''Fold.Step
inspect $ 'sourceFromList `hasNoType` ''SPEC
#endif

-- | 'fromTuple' yields two elements per tuple. To emit and drain ~value
-- elements we generate value/2 tuples and reduce each tuple's 'fromTuple'
-- stream with a light 'sum' fold (avoiding a heavy, non-fusible 'concatMap'
-- that would mask the cost of 'fromTuple').
{-# ANN sourceFromTuple (PermitPatternMatches [''[], ''(,), ''Int]) #-}
{-# ANN sourceFromTuple (PermitConstructions [''[],''Int,''(,),''()]) #-}
{-# ANN sourceFromTuple (PermitTypeClasses []) #-}
{-# NOINLINE sourceFromTuple #-}
sourceFromTuple :: Int -> Int -> IO ()
sourceFromTuple value = withDrain $ \n ->
    Stream.mapM (Stream.fold Fold.sum . Stream.fromTuple)
        $ Stream.fromList (fmap (\i -> (i, i)) [n .. n + value `div` 2])

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'sourceFromTuple
inspect $ 'sourceFromTuple `hasNoType` ''Stream.Step
inspect $ 'sourceFromTuple `hasNoType` ''Producer.TupleState
inspect $ 'sourceFromTuple `hasNoType` ''Fold.Step
inspect $ 'sourceFromTuple `hasNoType` ''SPEC
#endif

{-# ANN sourceIsList (PermitPatternMatches [''[], ''Int]) #-}
{-# ANN sourceIsList (PermitConstructions [''[],''Int,''()]) #-}
{-# ANN sourceIsList (PermitTypeClasses []) #-}
{-# NOINLINE sourceIsList #-}
sourceIsList :: Int -> Int -> IO ()
sourceIsList value = withDrainPure $ \n -> GHC.fromList [n..n+value]

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'sourceIsList
inspect $ 'sourceIsList `hasNoType` ''Stream.Step
inspect $ 'sourceIsList `hasNoType` ''Fold.Step
inspect $ 'sourceIsList `hasNoType` ''SPEC
#endif

{-# ANN sourceIsString (PermitPatternMatches [''[], ''Int]) #-}
{-# ANN sourceIsString (PermitConstructions [''Char,''[],''()]) #-}
{-# ANN sourceIsString (PermitTypeClasses []) #-}
{-# NOINLINE sourceIsString #-}
sourceIsString :: Int -> Int -> IO ()
sourceIsString value = withDrainPure $ \n ->
    GHC.fromString (Prelude.replicate (n + value) 'a')

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'sourceIsString
inspect $ 'sourceIsString `hasNoType` ''Stream.Step
inspect $ 'sourceIsString `hasNoType` ''Fold.Step
inspect $ 'sourceIsString `hasNoType` ''SPEC
#endif

{-# INLINE readInstance #-}
readInstance :: String -> Stream Identity Int
readInstance str =
    let r = reads str
    in case r of
        [(x,"")] -> x
        _ -> error "readInstance: no parse"

-- For comparisons
{-# INLINE readInstanceList #-}
readInstanceList :: String -> [Int]
readInstanceList str =
    let r = reads str
    in case r of
        [(x,"")] -> x
        _ -> error "readInstance: no parse"

instance NFData a => NFData (Stream Identity a) where
    {-# INLINE rnf #-}
    rnf xs = runIdentity $ Stream.fold (Fold.foldl' (\_ x -> rnf x) ()) xs

-------------------------------------------------------------------------------
-- Foldable Instance
-------------------------------------------------------------------------------

{-# INLINE foldableFoldl' #-}
foldableFoldl' :: Int -> Int -> Int
foldableFoldl' value n =
    F.foldl' (+) 0 (sourceUnfoldr value n :: Stream Identity Int)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'foldableFoldl'
inspect $ 'foldableFoldl' `hasNoType` ''Stream.Step
#endif

{-# INLINE foldableFoldrElem #-}
foldableFoldrElem :: Int -> Int -> Bool
foldableFoldrElem value n =
    F.foldr (\x xs -> x == value || xs)
            False
            (sourceUnfoldr value n :: Stream Identity Int)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'foldableFoldrElem
inspect $ 'foldableFoldrElem `hasNoType` ''Stream.Step
inspect $ 'foldableFoldrElem `hasNoType` ''Fold.Step
inspect $ 'foldableFoldrElem `hasNoType` ''SPEC
#endif

{-# INLINE foldableSum #-}
foldableSum :: Int -> Int -> Int
foldableSum value n =
    Prelude.sum (sourceUnfoldr value n :: Stream Identity Int)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'foldableSum
inspect $ 'foldableSum `hasNoType` ''Stream.Step
inspect $ 'foldableSum `hasNoType` ''Fold.Step
inspect $ 'foldableSum `hasNoType` ''SPEC
#endif

{-# INLINE foldableProduct #-}
foldableProduct :: Int -> Int -> Int
foldableProduct value n =
    Prelude.product (sourceUnfoldr value n :: Stream Identity Int)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'foldableProduct
inspect $ 'foldableProduct `hasNoType` ''Stream.Step
inspect $ 'foldableProduct `hasNoType` ''Fold.Step
inspect $ 'foldableProduct `hasNoType` ''SPEC
#endif

{-# INLINE _foldableNull #-}
_foldableNull :: Int -> Int -> Bool
_foldableNull value n =
    Prelude.null (sourceUnfoldr value n :: Stream Identity Int)

{-# INLINE foldableElem #-}
foldableElem :: Int -> Int -> Bool
foldableElem value n =
    value `Prelude.elem` (sourceUnfoldr value n :: Stream Identity Int)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'foldableElem
inspect $ 'foldableElem `hasNoType` ''Stream.Step
inspect $ 'foldableElem `hasNoType` ''Fold.Step
inspect $ 'foldableElem `hasNoType` ''SPEC
#endif

{-# INLINE foldableNotElem #-}
foldableNotElem :: Int -> Int -> Bool
foldableNotElem value n =
    value `Prelude.notElem` (sourceUnfoldr value n :: Stream Identity Int)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'foldableNotElem
inspect $ 'foldableNotElem `hasNoType` ''Stream.Step
inspect $ 'foldableNotElem `hasNoType` ''Fold.Step
inspect $ 'foldableNotElem `hasNoType` ''SPEC
#endif

{-# INLINE foldableFind #-}
foldableFind :: Int -> Int -> Maybe Int
foldableFind value n =
    F.find (== (value + 1)) (sourceUnfoldr value n :: Stream Identity Int)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'foldableFind
inspect $ 'foldableFind `hasNoType` ''Stream.Step
inspect $ 'foldableFind `hasNoType` ''Fold.Step
inspect $ 'foldableFind `hasNoType` ''SPEC
#endif

{-# INLINE foldableAll #-}
foldableAll :: Int -> Int -> Bool
foldableAll value n =
    Prelude.all (<= (value + 1)) (sourceUnfoldr value n :: Stream Identity Int)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'foldableAll
inspect $ 'foldableAll `hasNoType` ''Stream.Step
inspect $ 'foldableAll `hasNoType` ''Fold.Step
inspect $ 'foldableAll `hasNoType` ''SPEC
#endif

{- HLINT ignore "Use any"-}
{-# INLINE foldableAny #-}
foldableAny :: Int -> Int -> Bool
foldableAny value n =
    Prelude.any (> (value + 1)) (sourceUnfoldr value n :: Stream Identity Int)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'foldableAny
inspect $ 'foldableAny `hasNoType` ''Stream.Step
inspect $ 'foldableAny `hasNoType` ''Fold.Step
inspect $ 'foldableAny `hasNoType` ''SPEC
#endif

{- HLINT ignore "Use all"-}
{-# INLINE foldableAnd #-}
foldableAnd :: Int -> Int -> Bool
foldableAnd value n =
    Prelude.and $ fmap
        (<= (value + 1)) (sourceUnfoldr value n :: Stream Identity Int)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'foldableAnd
inspect $ 'foldableAnd `hasNoType` ''Stream.Step
inspect $ 'foldableAnd `hasNoType` ''Fold.Step
inspect $ 'foldableAnd `hasNoType` ''SPEC
#endif

{- HLINT ignore "Use any"-}
{-# INLINE foldableOr #-}
foldableOr :: Int -> Int -> Bool
foldableOr value n =
    Prelude.or $ fmap
        (> (value + 1)) (sourceUnfoldr value n :: Stream Identity Int)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'foldableOr
inspect $ 'foldableOr `hasNoType` ''Stream.Step
inspect $ 'foldableOr `hasNoType` ''Fold.Step
inspect $ 'foldableOr `hasNoType` ''SPEC
#endif

{-# INLINE foldableLength #-}
foldableLength :: Int -> Int -> Int
foldableLength value n =
    Prelude.length (sourceUnfoldr value n :: Stream Identity Int)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'foldableLength
inspect $ 'foldableLength `hasNoType` ''Stream.Step
inspect $ 'foldableLength `hasNoType` ''Fold.Step
inspect $ 'foldableLength `hasNoType` ''SPEC
#endif

{-# INLINE foldableMin #-}
foldableMin :: Int -> Int -> Int
foldableMin value n =
    Prelude.minimum (sourceUnfoldr value n :: Stream Identity Int)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'foldableMin
inspect $ 'foldableMin `hasNoType` ''Stream.Step
inspect $ 'foldableMin `hasNoType` ''Fold.Step
inspect $ 'foldableMin `hasNoType` ''SPEC
#endif

{-# INLINE ordInstanceMin #-}
ordInstanceMin :: Int -> Int -> ()
ordInstanceMin value n =
    let src = sourceUnfoldr value n
     in runIdentity $ drain $ min src src

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'ordInstanceMin
inspect $ 'ordInstanceMin `hasNoType` ''Stream.Step
inspect $ 'ordInstanceMin `hasNoType` ''Fold.Step
inspect $ 'ordInstanceMin `hasNoType` ''SPEC
#endif

{-# INLINE foldableMax #-}
foldableMax :: Int -> Int -> Int
foldableMax value n =
    Prelude.maximum (sourceUnfoldr value n :: Stream Identity Int)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'foldableMax
inspect $ 'foldableMax `hasNoType` ''Stream.Step
inspect $ 'foldableMax `hasNoType` ''Fold.Step
inspect $ 'foldableMax `hasNoType` ''SPEC
#endif

{-# INLINE foldableMinBy #-}
foldableMinBy :: Int -> Int -> Int
foldableMinBy value n =
    F.minimumBy compare (sourceUnfoldr value n :: Stream Identity Int)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'foldableMinBy
inspect $ 'foldableMinBy `hasNoType` ''Stream.Step
inspect $ 'foldableMinBy `hasNoType` ''Fold.Step
inspect $ 'foldableMinBy `hasNoType` ''SPEC
#endif

{-# INLINE foldableListMinBy #-}
foldableListMinBy :: Int -> Int -> Int
foldableListMinBy value n = F.minimumBy compare [1..value+n]

{-# INLINE foldableMaxBy #-}
foldableMaxBy :: Int -> Int -> Int
foldableMaxBy value n =
    F.maximumBy compare (sourceUnfoldr value n :: Stream Identity Int)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'foldableMaxBy
inspect $ 'foldableMaxBy `hasNoType` ''Stream.Step
inspect $ 'foldableMaxBy `hasNoType` ''Fold.Step
inspect $ 'foldableMaxBy `hasNoType` ''SPEC
#endif

{-# INLINE foldableToList #-}
foldableToList :: Int -> Int -> [Int]
foldableToList value n =
    F.toList (sourceUnfoldr value n :: Stream Identity Int)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'foldableToList
inspect $ 'foldableToList `hasNoType` ''Stream.Step
inspect $ 'foldableToList `hasNoType` ''Fold.Step
inspect $ 'foldableToList `hasNoType` ''SPEC
#endif

{-# INLINE foldableMapM_ #-}
foldableMapM_ :: Int -> Int -> IO ()
foldableMapM_ value n =
    F.mapM_ (\_ -> return ()) (sourceUnfoldr value n :: Stream Identity Int)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'foldableMapM_
inspect $ 'foldableMapM_ `hasNoType` ''Stream.Step
inspect $ 'foldableMapM_ `hasNoType` ''Fold.Step
inspect $ 'foldableMapM_ `hasNoType` ''SPEC
#endif

{-# INLINE foldableSequence_ #-}
foldableSequence_ :: Int -> Int -> IO ()
foldableSequence_ value n =
    F.sequence_ (sourceUnfoldrAction value n :: Stream Identity (IO Int))

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'foldableSequence_
inspect $ 'foldableSequence_ `hasNoType` ''Stream.Step
inspect $ 'foldableSequence_ `hasNoType` ''Fold.Step
inspect $ 'foldableSequence_ `hasNoType` ''SPEC
#endif

{-# INLINE _foldableMsum #-}
_foldableMsum :: Int -> Int -> IO Int
_foldableMsum value n =
    F.msum (sourceUnfoldrAction value n :: Stream Identity (IO Int))

-------------------------------------------------------------------------------
-- Show instance
-------------------------------------------------------------------------------

{-# ANN showInstance (PermitPatternMatches [''Int]) #-}
{-# ANN showInstance (PermitConstructions [''Int,''Stream.Step,''Stream]) #-}
{-# ANN showInstance (PermitTypeClasses [''Show]) #-}
{-# NOINLINE showInstance #-}
showInstance :: Int -> Int -> IO String
showInstance value = withPureStream value show

{-# INLINE showInstanceList #-}
showInstanceList :: [Int] -> String
showInstanceList = show

-------------------------------------------------------------------------------
-- Eq and Ord instances
-------------------------------------------------------------------------------

{-# ANN eqInstance (PermitPatternMatches [''Int]) #-}
{-# ANN eqInstance (PermitConstructions [''Bool]) #-}
{-# ANN eqInstance (PermitTypeClasses []) #-}
{-# NOINLINE eqInstance #-}
eqInstance :: Int -> Int -> IO Bool
eqInstance value = withPureStream value $ \src -> src == src

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'eqInstance
inspect $ 'eqInstance `hasNoType` ''Stream.Step
inspect $ 'eqInstance `hasNoType` ''Fold.Step
inspect $ 'eqInstance `hasNoType` ''SPEC
#endif

{-# ANN eqInstanceNotEq (PermitPatternMatches [''Int]) #-}
{-# ANN eqInstanceNotEq (PermitConstructions [''Bool]) #-}
{-# ANN eqInstanceNotEq (PermitTypeClasses []) #-}
{-# NOINLINE eqInstanceNotEq #-}
eqInstanceNotEq :: Int -> Int -> IO Bool
eqInstanceNotEq value = withPureStream value $ \src -> src /= src

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'eqInstanceNotEq
inspect $ 'eqInstanceNotEq `hasNoType` ''Stream.Step
inspect $ 'eqInstanceNotEq `hasNoType` ''Fold.Step
inspect $ 'eqInstanceNotEq `hasNoType` ''SPEC
#endif

{-# ANN ordInstance (PermitPatternMatches [''Int]) #-}
{-# ANN ordInstance (PermitConstructions [''Bool]) #-}
{-# ANN ordInstance (PermitTypeClasses []) #-}
{-# NOINLINE ordInstance #-}
ordInstance :: Int -> Int -> IO Bool
ordInstance value = withPureStream value $ \src -> src < src

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'ordInstance
inspect $ 'ordInstance `hasNoType` ''Stream.Step
inspect $ 'ordInstance `hasNoType` ''Fold.Step
inspect $ 'ordInstance `hasNoType` ''SPEC
#endif

-------------------------------------------------------------------------------
-- Reductions
-------------------------------------------------------------------------------

{-# ANN uncons (PermitPatternMatches [''Stream.Step, ''Int]) #-}
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

{-# ANN foldBreak (PermitPatternMatches [''Stream.Step, ''Int]) #-}
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

{-# ANN foldrMElem (PermitPatternMatches [''Int]) #-}
{-# ANN foldrMElem (PermitConstructions [''Bool]) #-}
{-# ANN foldrMElem (PermitTypeClasses []) #-}
{-# NOINLINE foldrMElem #-}
foldrMElem :: Int -> Int -> IO Bool
foldrMElem value =
    withStream value
        (S.foldrM
             (\x xs -> if x == value then return True else xs)
             (return False))

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'foldrMElem
inspect $ 'foldrMElem `hasNoType` ''S.Step
inspect $ 'foldrMElem `hasNoType` ''Fold.Step
inspect $ 'foldrMElem `hasNoType` ''SPEC
#endif

{-# ANN foldrMElemIdentity (PermitPatternMatches [''Int]) #-}
{-# ANN foldrMElemIdentity (PermitConstructions [''Bool]) #-}
{-# ANN foldrMElemIdentity (PermitTypeClasses []) #-}
{-# NOINLINE foldrMElemIdentity #-}
foldrMElemIdentity :: Int -> Int -> IO Bool
foldrMElemIdentity value =
    withPureStream value $
        runIdentity . S.foldrM
            (\x xs -> if x == value then return True else xs)
            (return False)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'foldrMElemIdentity
inspect $ 'foldrMElemIdentity `hasNoType` ''S.Step
inspect $ 'foldrMElemIdentity `hasNoType` ''Fold.Step
inspect $ 'foldrMElemIdentity `hasNoType` ''SPEC
#endif

{-# ANN foldrMToList (PermitPatternMatches [''Int]) #-}
{-# ANN foldrMToList (PermitConstructions [''[],''Int]) #-}
{-# ANN foldrMToList (PermitTypeClasses []) #-}
{-# NOINLINE foldrMToList #-}
foldrMToList :: Int -> Int -> IO [Int]
foldrMToList value =
    withStream value $ S.foldrM (\x xs -> (x :) <$> xs) (return [])

{-# ANN foldrMToListIdentity (PermitPatternMatches [''Int]) #-}
{-# ANN foldrMToListIdentity (PermitConstructions [''Int,''[]]) #-}
{-# ANN foldrMToListIdentity (PermitTypeClasses []) #-}
{-# NOINLINE foldrMToListIdentity #-}
foldrMToListIdentity :: Int -> Int -> IO [Int]
foldrMToListIdentity value =
    withPureStream value
        (runIdentity . S.foldrM (\x xs -> (x :) <$> xs) (return []))

{-# ANN foldl'Reduce (PermitPatternMatches []) #-}
{-# ANN foldl'Reduce (PermitConstructions []) #-}
{-# ANN foldl'Reduce (PermitTypeClasses []) #-}
{-# NOINLINE foldl'Reduce #-}
foldl'Reduce :: Int -> Int -> IO Int
foldl'Reduce value = withStream value (S.foldl' (+) 0)

{-# ANN foldl'ReduceIdentity (PermitPatternMatches [''Int]) #-}
{-# ANN foldl'ReduceIdentity (PermitConstructions [''Int]) #-}
{-# ANN foldl'ReduceIdentity (PermitTypeClasses []) #-}
{-# NOINLINE foldl'ReduceIdentity #-}
foldl'ReduceIdentity :: Int -> Int -> IO Int
foldl'ReduceIdentity value =
    withPureStream value $ runIdentity . S.foldl' (+) 0

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'foldl'ReduceIdentity
inspect $ 'foldl'ReduceIdentity `hasNoType` ''S.Step
#endif

{-# ANN foldlM'Reduce (PermitPatternMatches []) #-}
{-# ANN foldlM'Reduce (PermitConstructions []) #-}
{-# ANN foldlM'Reduce (PermitTypeClasses []) #-}
{-# NOINLINE foldlM'Reduce #-}
foldlM'Reduce :: Int -> Int -> IO Int
foldlM'Reduce value =
    withStream value (S.foldlM' (\xs a -> return $ a + xs) (return 0))

{-# ANN foldlM'ReduceIdentity (PermitPatternMatches [''Int]) #-}
{-# ANN foldlM'ReduceIdentity (PermitConstructions [''Int]) #-}
{-# ANN foldlM'ReduceIdentity (PermitTypeClasses []) #-}
{-# NOINLINE foldlM'ReduceIdentity #-}
foldlM'ReduceIdentity :: Int -> Int -> IO Int
foldlM'ReduceIdentity value =
    withPureStream value $
        runIdentity . S.foldlM' (\xs a -> return $ a + xs) (return 0)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'foldlM'ReduceIdentity
inspect $ 'foldlM'ReduceIdentity `hasNoType` ''S.Step
#endif

{-# ANN toNull (PermitPatternMatches [''Int]) #-}
{-# ANN toNull (PermitConstructions [''()]) #-}
{-# ANN toNull (PermitTypeClasses []) #-}
{-# NOINLINE toNull #-}
toNull :: Int -> Int -> IO ()
toNull value = withStream value S.drain

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'toNull
inspect $ 'toNull `hasNoType` ''Stream.Step
inspect $ 'toNull `hasNoType` ''Fold.Step
inspect $ 'toNull `hasNoType` ''SPEC
#endif

{-# ANN drainPure (PermitPatternMatches [''Int]) #-}
{-# ANN drainPure (PermitConstructions [''()]) #-}
{-# ANN drainPure (PermitTypeClasses []) #-}
{-# NOINLINE drainPure #-}
drainPure :: Int -> Int -> IO ()
drainPure value = withPureStream value $ runIdentity . drain

-- This has unfused constructors but those are eliminated by SpecConstr rather
-- than inlining, therefore force inlining has no use except that it issues a
-- warning.
{-# ANN drainN (PermitPatternMatches [''Int]) #-}
{-# ANN drainN (PermitConstructions [''()]) #-}
{-# ANN drainN (PermitTypeClasses []) #-}
{-# NOINLINE drainN #-}
drainN :: Int -> Int -> IO ()
drainN value = withStream value (S.fold (Fold.drainN value))

{-# ANN foldl'Build (PermitPatternMatches [''Int,''[],''SPEC]) #-}
{-# ANN foldl'Build (PermitConstructions [''Int,''[],''SPEC]) #-}
{-# ANN foldl'Build (PermitTypeClasses []) #-}
{-# NOINLINE foldl'Build #-}
foldl'Build :: Int -> Int -> IO [Int]
foldl'Build value = withStream value (S.foldl' (flip (:)) [])

{-# ANN foldl'BuildIdentity (PermitPatternMatches [''Int,''[],''SPEC]) #-}
{-# ANN foldl'BuildIdentity (PermitConstructions [''Int,''[],''SPEC]) #-}
{-# ANN foldl'BuildIdentity (PermitTypeClasses []) #-}
{-# NOINLINE foldl'BuildIdentity #-}
foldl'BuildIdentity :: Int -> Int -> IO [Int]
foldl'BuildIdentity value =
    withPureStream value (runIdentity . S.foldl' (flip (:)) [])

{-# ANN foldlM'Build (PermitPatternMatches [''Int,''[],''SPEC]) #-}
{-# ANN foldlM'Build (PermitConstructions [''Int,''[],''SPEC]) #-}
{-# ANN foldlM'Build (PermitTypeClasses []) #-}
{-# NOINLINE foldlM'Build #-}
foldlM'Build :: Int -> Int -> IO [Int]
foldlM'Build value =
    withStream value (S.foldlM' (\xs x -> return $ x : xs) (return []))

{-# ANN foldlM'BuildIdentity (PermitPatternMatches [''Int,''[],''SPEC]) #-}
{-# ANN foldlM'BuildIdentity (PermitConstructions [''Int,''[],''SPEC]) #-}
{-# ANN foldlM'BuildIdentity (PermitTypeClasses []) #-}
{-# NOINLINE foldlM'BuildIdentity #-}
foldlM'BuildIdentity :: Int -> Int -> IO [Int]
foldlM'BuildIdentity value =
    withPureStream value
        (runIdentity . S.foldlM' (\xs x -> return $ x : xs) (return []))

{-# ANN foldrMToSum (PermitPatternMatches [''Int]) #-}
{-# ANN foldrMToSum (PermitConstructions [''Int]) #-}
{-# ANN foldrMToSum (PermitTypeClasses []) #-}
{-# NOINLINE foldrMToSum #-}
foldrMToSum :: Int -> Int -> IO Int
foldrMToSum value =
    withStream value (S.foldrM (\x xs -> (x +) <$> xs) (return 0))

{-# ANN foldrMToSumIdentity (PermitPatternMatches [''Int]) #-}
{-# ANN foldrMToSumIdentity (PermitConstructions [''Int]) #-}
{-# ANN foldrMToSumIdentity (PermitTypeClasses []) #-}
{-# NOINLINE foldrMToSumIdentity #-}
foldrMToSumIdentity :: Int -> Int -> IO Int
foldrMToSumIdentity value =
    withPureStream value
        (runIdentity . S.foldrM (\x xs -> (x +) <$> xs) (return 0))

{-# ANN toList' (PermitPatternMatches [''Int]) #-}
{-# ANN toList' (PermitConstructions [''[],''Int]) #-}
{-# ANN toList' (PermitTypeClasses []) #-}
{-# NOINLINE toList' #-}
toList' :: Int -> Int -> IO [Int]
toList' value = withStream value S.toList

{-# ANN eqByPure (PermitPatternMatches [''Int]) #-}
{-# ANN eqByPure (PermitConstructions [''Bool]) #-}
{-# ANN eqByPure (PermitTypeClasses []) #-}
{-# NOINLINE eqByPure #-}
eqByPure :: Int -> Int -> IO Bool
eqByPure value =
    withPureStream value $ \src -> runIdentity $ S.eqBy (==) src src

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'eqByPure
inspect $ 'eqByPure `hasNoType` ''SPEC
inspect $ 'eqByPure `hasNoType` ''S.Step
inspect $ 'eqByPure `hasNoType` ''Fold.Step
#endif

{-# ANN cmpByPure (PermitPatternMatches [''Int]) #-}
{-# ANN cmpByPure (PermitConstructions [''Ordering]) #-}
{-# ANN cmpByPure (PermitTypeClasses []) #-}
{-# NOINLINE cmpByPure #-}
cmpByPure :: Int -> Int -> IO Ordering
cmpByPure value =
    withPureStream value $ \src -> runIdentity $ S.cmpBy compare src src

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'cmpByPure
inspect $ 'cmpByPure `hasNoType` ''SPEC
inspect $ 'cmpByPure `hasNoType` ''S.Step
inspect $ 'cmpByPure `hasNoType` ''Fold.Step
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

{-# ANN map1 (PermitPatternMatches [''Int]) #-}
{-# ANN map1 (PermitConstructions [''()]) #-}
{-# ANN map1 (PermitTypeClasses []) #-}
{-# NOINLINE map1 #-}
map1 :: Int -> Int -> IO ()
map1 value = withStream value (mapN 1)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'map1
inspect $ 'map1 `hasNoType` ''Stream.Step
inspect $ 'map1 `hasNoType` ''FL.Step
inspect $ 'map1 `hasNoType` ''SPEC
#endif

{-# ANN mapM1 (PermitPatternMatches [''Int]) #-}
{-# ANN mapM1 (PermitConstructions [''()]) #-}
{-# ANN mapM1 (PermitTypeClasses []) #-}
{-# NOINLINE mapM1 #-}
mapM1 :: Int -> Int -> IO ()
mapM1 value = withStream value (mapM 1)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'mapM1
inspect $ 'mapM1 `hasNoType` ''Stream.Step
inspect $ 'mapM1 `hasNoType` ''FL.Step
inspect $ 'mapM1 `hasNoType` ''SPEC
#endif

{-# ANN mapN4 (PermitPatternMatches [''Int]) #-}
{-# ANN mapN4 (PermitConstructions [''()]) #-}
{-# ANN mapN4 (PermitTypeClasses []) #-}
{-# NOINLINE mapN4 #-}
mapN4 :: Int -> Int -> IO ()
mapN4 value = withStream value (mapN 4)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'mapN4
inspect $ 'mapN4 `hasNoType` ''Stream.Step
inspect $ 'mapN4 `hasNoType` ''FL.Step
inspect $ 'mapN4 `hasNoType` ''SPEC
#endif

{-# ANN mapM4 (PermitPatternMatches [''Int]) #-}
{-# ANN mapM4 (PermitConstructions [''()]) #-}
{-# ANN mapM4 (PermitTypeClasses []) #-}
{-# NOINLINE mapM4 #-}
mapM4 :: Int -> Int -> IO ()
mapM4 value = withStream value (mapM 4)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'mapM4
inspect $ 'mapM4 `hasNoType` ''Stream.Step
inspect $ 'mapM4 `hasNoType` ''FL.Step
inspect $ 'mapM4 `hasNoType` ''SPEC
#endif

-------------------------------------------------------------------------------
-- Filtering
-------------------------------------------------------------------------------

{-# INLINE _takeOne #-}
_takeOne :: MonadIO m => Int -> Stream m Int -> m ()
_takeOne n = composeN n $ Stream.take 1

{-# INLINE takeAll #-}
takeAll :: MonadIO m => Int -> Int -> Stream m Int -> m ()
takeAll value n = composeN n $ Stream.take (value + 1)

{-# ANN takeAll1 (PermitPatternMatches [''Int]) #-}
{-# ANN takeAll1 (PermitConstructions [''()]) #-}
{-# ANN takeAll1 (PermitTypeClasses []) #-}
{-# NOINLINE takeAll1 #-}
takeAll1 :: Int -> Int -> IO ()
takeAll1 value = withStream value (takeAll value 1)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'takeAll1
inspect $ 'takeAll1 `hasNoType` ''Stream.Step
inspect $ 'takeAll1 `hasNoType` ''FL.Step
inspect $ 'takeAll1 `hasNoType` ''SPEC
#endif

{-# ANN takeAll4 (PermitPatternMatches [''Int]) #-}
{-# ANN takeAll4 (PermitConstructions [''()]) #-}
{-# ANN takeAll4 (PermitTypeClasses []) #-}
{-# NOINLINE takeAll4 #-}
takeAll4 :: Int -> Int -> IO ()
takeAll4 value = withStream value (takeAll value 4)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'takeAll4
inspect $ 'takeAll4 `hasNoType` ''Stream.Step
inspect $ 'takeAll4 `hasNoType` ''FL.Step
inspect $ 'takeAll4 `hasNoType` ''SPEC
#endif

{-# INLINE takeWhileTrue #-}
takeWhileTrue :: MonadIO m => Int -> Int -> Stream m Int -> m ()
takeWhileTrue value n = composeN n $ Stream.takeWhile (<= (value + 1))

{-# ANN takeWhileTrue1 (PermitPatternMatches [''Int]) #-}
{-# ANN takeWhileTrue1 (PermitConstructions [''()]) #-}
{-# ANN takeWhileTrue1 (PermitTypeClasses []) #-}
{-# NOINLINE takeWhileTrue1 #-}
takeWhileTrue1 :: Int -> Int -> IO ()
takeWhileTrue1 value = withStream value (takeWhileTrue value 1)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'takeWhileTrue1
inspect $ 'takeWhileTrue1 `hasNoType` ''Stream.Step
inspect $ 'takeWhileTrue1 `hasNoType` ''FL.Step
inspect $ 'takeWhileTrue1 `hasNoType` ''SPEC
#endif

{-# ANN takeWhileTrue4 (PermitPatternMatches [''Int]) #-}
{-# ANN takeWhileTrue4 (PermitConstructions [''()]) #-}
{-# ANN takeWhileTrue4 (PermitTypeClasses []) #-}
{-# NOINLINE takeWhileTrue4 #-}
takeWhileTrue4 :: Int -> Int -> IO ()
takeWhileTrue4 value = withStream value (takeWhileTrue value 4)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'takeWhileTrue4
inspect $ 'takeWhileTrue4 `hasNoType` ''Stream.Step
inspect $ 'takeWhileTrue4 `hasNoType` ''FL.Step
inspect $ 'takeWhileTrue4 `hasNoType` ''SPEC
#endif

{-# INLINE takeWhileMTrue #-}
takeWhileMTrue :: MonadIO m => Int -> Int -> Stream m Int -> m ()
takeWhileMTrue value n = composeN n $ Stream.takeWhileM (return . (<= (value + 1)))

{-# ANN takeWhileMTrue4 (PermitPatternMatches [''Int]) #-}
{-# ANN takeWhileMTrue4 (PermitConstructions [''()]) #-}
{-# ANN takeWhileMTrue4 (PermitTypeClasses []) #-}
{-# NOINLINE takeWhileMTrue4 #-}
takeWhileMTrue4 :: Int -> Int -> IO ()
takeWhileMTrue4 value = withStream value (takeWhileMTrue value 4)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'takeWhileMTrue4
inspect $ 'takeWhileMTrue4 `hasNoType` ''Stream.Step
inspect $ 'takeWhileMTrue4 `hasNoType` ''FL.Step
inspect $ 'takeWhileMTrue4 `hasNoType` ''SPEC
#endif

-------------------------------------------------------------------------------
-- Benchmarks
-------------------------------------------------------------------------------

{-# ANN benchmarks "HLint: ignore" #-}
benchmarks :: Int -> [(SpaceComplexity, Benchmark)]
benchmarks size =
    -- Construction
    [ (SpaceO_1, benchIO "fromList" $ sourceFromList size)
    , (SpaceO_1, benchIO "fromTuple" $ sourceFromTuple size)
    , (SpaceO_1, benchIO "IsList.fromList" $ sourceIsList size)
    , (SpaceO_1, benchIO "IsString.fromString" $ sourceIsString size)
    -- Buffers the output of show/read.
    -- XXX can the outputs be streaming? Can we have special read/show
    -- style type classes, readM/showM supporting streaming effects?
    , (HeapO_n, bench "readsPrec pure streams" $
          nf (readInstance . mkString) size)
    , (HeapO_n, bench "readsPrec Haskell lists" $
          nf (readInstanceList . mkListString) size)

    -- Elimination/Foldable instance
    , (SpaceO_1, benchIO "Foldable/foldl'" $ withRandomInt (foldableFoldl' size))
    , (SpaceO_1, benchIO "Foldable/foldrElem" $ withRandomInt (foldableFoldrElem size))
 -- , (SpaceO_1, benchIO "Foldable/null" $ withRandomInt (_foldableNull size))
    , (SpaceO_1, benchIO "Foldable/elem" $ withRandomInt (foldableElem size))
    , (SpaceO_1, benchIO "Foldable/length" $ withRandomInt (foldableLength size))
    , (SpaceO_1, benchIO "Foldable/sum" $ withRandomInt (foldableSum size))
    , (SpaceO_1, benchIO "Foldable/product" $ withRandomInt (foldableProduct size))
    , (SpaceO_1, benchIO "Foldable/minimum" $ withRandomInt (foldableMin size))
    , (SpaceO_1, benchIO "Foldable/min (ord)" $ withRandomInt (ordInstanceMin size))
    , (SpaceO_1, benchIO "Foldable/maximum" $ withRandomInt (foldableMax size))
    , (SpaceO_1, benchIO "Foldable/minimumBy" $ withRandomInt (foldableMinBy size))
    , (SpaceO_1, benchIO "Foldable/maximumBy" $ withRandomInt (foldableMaxBy size))
    , (SpaceO_1, benchIO "Foldable/minimumByList" $ withRandomInt (foldableListMinBy size))
    , (SpaceO_1, benchIO "Foldable/length . toList" $
          withRandomInt (Prelude.length . foldableToList size))
    , (SpaceO_1, benchIO "Foldable/notElem" $ withRandomInt (foldableNotElem size))
    , (SpaceO_1, benchIO "Foldable/find" $ withRandomInt (foldableFind size))
    , (SpaceO_1, benchIO "Foldable/all" $ withRandomInt (foldableAll size))
    , (SpaceO_1, benchIO "Foldable/any" $ withRandomInt (foldableAny size))
    , (SpaceO_1, benchIO "Foldable/and" $ withRandomInt (foldableAnd size))
    , (SpaceO_1, benchIO "Foldable/or" $ withRandomInt (foldableOr size))

    -- Applicative and Traversable operations
    -- TBD: traverse_
    , (SpaceO_1, benchIO "Foldable/mapM_" (foldableMapM_ size))
    -- TBD: for_
    -- TBD: forM_
    , (SpaceO_1, benchIO "Foldable/sequence_" (foldableSequence_ size))
    -- TBD: sequenceA_
    -- TBD: asum
    -- XXX needs to be fixed, results are in ns
    -- , (SpaceO_1, benchIOSink1 "Foldable/msum" (foldableMsum size))

    -- Elimination/folds
    , (SpaceO_1, benchIO "foldl'/IO" $ foldl'Reduce size)
    , (SpaceO_1, benchIO "foldlM'/IO" $ foldlM'Reduce size)
    , (SpaceO_1, benchIO "foldl'/Identity" $ foldl'ReduceIdentity size)
    , (SpaceO_1, benchIO "foldlM'/Identity" $ foldlM'ReduceIdentity size)
    , (SpaceO_1, benchIO "foldrMElem/IO" $ foldrMElem size)
    , (SpaceO_1, benchIO "foldrMElem/Identity" $ foldrMElemIdentity size)
    , (SpaceO_1, benchIO "foldrMToList" $ foldrMToListIdentity size)

    -- Left folds for building a structure are inherently non-streaming
    -- as the structure cannot be lazily consumed until fully built.
    , (HeapO_n, benchIO "foldl'/build/IO" $ foldl'Build size)
    , (HeapO_n, benchIO "foldl'/build/Identity" $ foldl'BuildIdentity size)
    , (HeapO_n, benchIO "foldlM'/build/IO" $ foldlM'Build size)
    , (HeapO_n, benchIO "foldlM'/build/Identity" $ foldlM'BuildIdentity size)

    -- Head recursive strict right folds.
    -- accumulation due to strictness of IO monad
    , (SpaceO_n, benchIO "foldrM/build/IO (toList)" $ foldrMToList size)
    -- Right folds for reducing are inherently non-streaming as the
    -- expression needs to be fully built before it can be reduced.
    , (SpaceO_n, benchIO "foldrM/reduce/Identity (sum)" $ foldrMToSumIdentity size)
    , (SpaceO_n, benchIO "foldrM/reduce/IO (sum)" $ foldrMToSum size)
    -- Converting the stream to a list or pure stream in a strict monad
    , (SpaceO_n, benchIO "toList" $ toList' size)

    -- this is too fast, causes all benchmarks reported in ns
    -- , (SpaceO_1, benchIO "null" $ ...)

    -- deconstruction
    , (SpaceO_1, benchIO "uncons" $ uncons size)
    , (SpaceO_1, benchIO "foldBreak" $ foldBreak size)

    -- draining
    , (SpaceO_1, benchIO "toNull" $ toNull size)
    , (SpaceO_1, benchIO "drainN" $ drainN size)
    , (SpaceO_1, benchIO "drain (pure)" $ drainPure size)

    -- length is used to check for foldr/build fusion
    , (SpaceO_1, benchIO "length . IsList.toList" $
          withPureStream size (Prelude.length . GHC.toList))

    -- Buffers the output of show/read.
    -- XXX can the outputs be streaming? Can we have special read/show
    -- style type classes, readM/showM supporting streaming effects?
    , (HeapO_n, bench "showsPrec Haskell lists" $ nf showInstanceList (mkList size))
    -- XXX This is not o-1-space for GHC-8.10
    , (HeapO_n, benchIO "showsPrec pure streams" $ showInstance size)

    , (SpaceO_1, benchIO "==" $ eqInstance size)
    , (SpaceO_1, benchIO "/=" $ eqInstanceNotEq size)
    , (SpaceO_1, benchIO "<" $ ordInstance size)
    , (SpaceO_1, benchIO "eqBy (pure)" $ eqByPure size)
    , (SpaceO_1, benchIO "cmpBy (pure)" $ cmpByPure size)
    , (SpaceO_1, benchIO "eqBy" $ eqBy size)
    , (SpaceO_1, benchIO "cmpBy" $ cmpBy size)

    -- Mapping
    , (SpaceO_1, benchIO "fmap" $ map1 size)
    , (SpaceO_1, benchIO "fmap x 4" $ mapN4 size)
    , (SpaceO_1, benchIO "map" $ map1 size)
    , (SpaceO_1, benchIO "mapM" $ mapM1 size)
    , (SpaceO_1, benchIO "map x 4" $ mapN4 size)
    , (SpaceO_1, benchIO "mapM x 4" $ mapM4 size)

    -- Trimming
    , (SpaceO_1, benchIO "take-all" $ takeAll1 size)
    , (SpaceO_1, benchIO "takeWhile-true" $ takeWhileTrue1 size)
 -- , (SpaceO_1, benchIO "takeWhileM-true" ...)
    , (SpaceO_1, benchIO "take-all x 4" $ takeAll4 size)
    , (SpaceO_1, benchIO "takeWhile-true x 4" $ takeWhileTrue4 size)
    , (SpaceO_1, benchIO "takeWhileM-true x 4" $ takeWhileMTrue4 size)
    ]
