-- |
-- Module      : Stream.Type
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

module Stream.Type
    ( benchmarks
    , boundedInts
    , infiniteInts
    , boundedIntsUnfold
    , checkStream
    , checkPair
    , result
    , withPureStream
    , withStream
    , withDrain
    , withDrainPure
    , withRandomInt
    , withRandomIntIO
    , benchIO
    ) where

#ifdef INSPECTION
import GHC.Types (SPEC(..))
import Test.Inspection
import qualified Streamly.Internal.Data.Producer as Producer
#endif

import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO(..))
import Control.DeepSeq (NFData(..))
import Data.Functor ((<&>))
import Data.Functor.Identity (Identity(..), runIdentity)
import Data.Monoid (Sum(..))
import Streamly.Internal.Data.Stream (Stream)
import Streamly.Data.Unfold (Unfold)
import System.Random (randomRIO)

import qualified Data.Foldable as F
import qualified GHC.Exts as GHC

import qualified Streamly.Internal.Data.Fold as FL
import qualified Streamly.Internal.Data.Fold as Fold
import qualified Streamly.Internal.Data.Refold.Type as Refold
import qualified Streamly.Internal.Data.Stream as S
import qualified Streamly.Internal.Data.Stream as Stream
import qualified Streamly.Internal.Data.Unfold as UF
import qualified Streamly.Internal.Data.Unfold as Unfold

import Test.Tasty.Bench
import qualified Stream.Common as Common
import Stream.Common hiding (benchIO)
import Streamly.Benchmark.Common
import Prelude hiding (concatMap, mapM, zipWith)

{-# INLINE benchIO #-}
benchIO :: NFData b => String -> IO b -> Benchmark
benchIO name = bench name . nfIO

{-# INLINE withRandomIntIO #-}
withRandomIntIO :: (Int -> IO b) -> IO b
withRandomIntIO f = randomRIO (1, 1 :: Int) >>= f

{-# INLINE withDrain #-}
withDrain :: (Int -> Stream IO a) -> IO ()
withDrain f = withRandomIntIO $ \n -> drain (f n)

{-# INLINE withDrainPure #-}
withDrainPure :: (Int -> Stream Identity a) -> IO ()
withDrainPure f = withRandomIntIO $ \n -> return $! runIdentity $ drain (f n)

{-# INLINE withRandomInt #-}
withRandomInt :: (Int -> b) -> IO b
withRandomInt f = randomRIO (1, 1 :: Int) <&> f

{-# INLINE withStream #-}
withStream :: Int -> (Stream IO Int -> IO b) -> IO b
withStream value f = withRandomIntIO (f . sourceUnfoldrM value)

{-# INLINE withPureStream #-}
withPureStream :: Int -> (Stream Identity Int -> b) -> IO b
withPureStream value f = randomRIO (1, 1) <&> (f . sourceUnfoldr value)

mkCross :: Stream m a -> Stream.Nested m a
mkCross = Stream.Nested

unCross :: Stream.Nested m a -> Stream m a
unCross = Stream.unNested

-------------------------------------------------------------------------------
-- fromList
-------------------------------------------------------------------------------

sourceFromList :: Int -> IO ()
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
sourceFromTuple :: Int -> IO ()
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

sourceIsList :: Int -> IO ()
sourceIsList value = withDrainPure $ \n -> GHC.fromList [n..n+value]

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'sourceIsList
inspect $ 'sourceIsList `hasNoType` ''Stream.Step
inspect $ 'sourceIsList `hasNoType` ''Fold.Step
inspect $ 'sourceIsList `hasNoType` ''SPEC
#endif

sourceIsString :: Int -> IO ()
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

showInstance :: Int -> IO String
showInstance value = withPureStream value show

{-# INLINE showInstanceList #-}
showInstanceList :: [Int] -> String
showInstanceList = show

-------------------------------------------------------------------------------
-- Eq and Ord instances
-------------------------------------------------------------------------------

eqInstance :: Int -> IO Bool
eqInstance value = withPureStream value $ \src -> src == src

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'eqInstance
inspect $ 'eqInstance `hasNoType` ''Stream.Step
inspect $ 'eqInstance `hasNoType` ''Fold.Step
inspect $ 'eqInstance `hasNoType` ''SPEC
#endif

eqInstanceNotEq :: Int -> IO Bool
eqInstanceNotEq value = withPureStream value $ \src -> src /= src

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'eqInstanceNotEq
inspect $ 'eqInstanceNotEq `hasNoType` ''Stream.Step
inspect $ 'eqInstanceNotEq `hasNoType` ''Fold.Step
inspect $ 'eqInstanceNotEq `hasNoType` ''SPEC
#endif

ordInstance :: Int -> IO Bool
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

uncons :: Int -> IO ()
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

foldBreak :: Int -> IO ()
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

foldrMElem :: Int -> IO Bool
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

foldrMElemIdentity :: Int -> IO Bool
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

foldrMToList :: Int -> IO [Int]
foldrMToList value =
    withStream value $ S.foldrM (\x xs -> (x :) <$> xs) (return [])

foldrMToListIdentity :: Int -> IO [Int]
foldrMToListIdentity value =
    withPureStream value
        (runIdentity . S.foldrM (\x xs -> (x :) <$> xs) (return []))

foldl'Reduce :: Int -> IO Int
foldl'Reduce value = withStream value (S.foldl' (+) 0)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'foldl'Reduce
inspect $ 'foldl'Reduce `hasNoType` ''S.Step
#endif

foldl'ReduceIdentity :: Int -> IO Int
foldl'ReduceIdentity value =
    withPureStream value $ runIdentity . S.foldl' (+) 0

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'foldl'ReduceIdentity
inspect $ 'foldl'ReduceIdentity `hasNoType` ''S.Step
#endif

foldlM'Reduce :: Int -> IO Int
foldlM'Reduce value =
    withStream value (S.foldlM' (\xs a -> return $ a + xs) (return 0))

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'foldlM'Reduce
inspect $ 'foldlM'Reduce `hasNoType` ''S.Step
#endif

foldlM'ReduceIdentity :: Int -> IO Int
foldlM'ReduceIdentity value =
    withPureStream value $
        runIdentity . S.foldlM' (\xs a -> return $ a + xs) (return 0)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'foldlM'ReduceIdentity
inspect $ 'foldlM'ReduceIdentity `hasNoType` ''S.Step
#endif

toNull :: Int -> IO ()
toNull value = withStream value S.drain

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'toNull
inspect $ 'toNull `hasNoType` ''Stream.Step
inspect $ 'toNull `hasNoType` ''Fold.Step
inspect $ 'toNull `hasNoType` ''SPEC
#endif

drainPure :: Int -> IO ()
drainPure value = withPureStream value $ runIdentity . drain

drainN :: Int -> IO ()
drainN value = withStream value (S.fold (Fold.drainN value))

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'drainN
inspect $ 'drainN `hasNoType` ''S.Step
inspect $ 'drainN `hasNoType` ''Fold.Step
inspect $ 'drainN `hasNoType` ''SPEC
#endif

foldl'Build :: Int -> IO [Int]
foldl'Build value = withStream value (S.foldl' (flip (:)) [])

foldl'BuildIdentity :: Int -> IO [Int]
foldl'BuildIdentity value =
    withPureStream value (runIdentity . S.foldl' (flip (:)) [])

foldlM'Build :: Int -> IO [Int]
foldlM'Build value =
    withStream value (S.foldlM' (\xs x -> return $ x : xs) (return []))

foldlM'BuildIdentity :: Int -> IO [Int]
foldlM'BuildIdentity value =
    withPureStream value
        (runIdentity . S.foldlM' (\xs x -> return $ x : xs) (return []))

foldrMToSum :: Int -> IO Int
foldrMToSum value =
    withStream value (S.foldrM (\x xs -> (x +) <$> xs) (return 0))

foldrMToSumIdentity :: Int -> IO Int
foldrMToSumIdentity value =
    withPureStream value
        (runIdentity . S.foldrM (\x xs -> (x +) <$> xs) (return 0))

toList' :: Int -> IO [Int]
toList' value = withStream value S.toList

eqByPure :: Int -> IO Bool
eqByPure value =
    withPureStream value $ \src -> runIdentity $ S.eqBy (==) src src

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'eqByPure
inspect $ 'eqByPure `hasNoType` ''SPEC
inspect $ 'eqByPure `hasNoType` ''S.Step
inspect $ 'eqByPure `hasNoType` ''Fold.Step
#endif

cmpByPure :: Int -> IO Ordering
cmpByPure value =
    withPureStream value $ \src -> runIdentity $ S.cmpBy compare src src

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'cmpByPure
inspect $ 'cmpByPure `hasNoType` ''SPEC
inspect $ 'cmpByPure `hasNoType` ''S.Step
inspect $ 'cmpByPure `hasNoType` ''Fold.Step
#endif

eqBy :: Int -> IO Bool
eqBy value = withStream value $ \src -> S.eqBy (==) src src

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'eqBy
inspect $ 'eqBy `hasNoType` ''SPEC
inspect $ 'eqBy `hasNoType` ''S.Step
inspect $ 'eqBy `hasNoType` ''Fold.Step
#endif

cmpBy :: Int -> IO Ordering
cmpBy value = withStream value $ \src -> S.cmpBy compare src src

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'cmpBy
inspect $ 'cmpBy `hasNoType` ''SPEC
inspect $ 'cmpBy `hasNoType` ''S.Step
inspect $ 'cmpBy `hasNoType` ''Fold.Step
#endif

-------------------------------------------------------------------------------
-- Mapping
-------------------------------------------------------------------------------

{-# INLINE mapN #-}
mapN :: Monad m => Int -> Stream m Int -> m ()
mapN n = composeN n $ fmap (+ 1)

{-# INLINE mapM #-}
mapM :: MonadAsync m => Int -> Stream m Int -> m ()
mapM n = composeN n $ Stream.mapM return

map1 :: Int -> IO ()
map1 value = withStream value (mapN 1)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'map1
inspect $ 'map1 `hasNoType` ''Stream.Step
inspect $ 'map1 `hasNoType` ''FL.Step
inspect $ 'map1 `hasNoType` ''SPEC
#endif

mapM1 :: Int -> IO ()
mapM1 value = withStream value (mapM 1)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'mapM1
inspect $ 'mapM1 `hasNoType` ''Stream.Step
inspect $ 'mapM1 `hasNoType` ''FL.Step
inspect $ 'mapM1 `hasNoType` ''SPEC
#endif

mapN4 :: Int -> IO ()
mapN4 value = withStream value (mapN 4)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'mapN4
inspect $ 'mapN4 `hasNoType` ''Stream.Step
inspect $ 'mapN4 `hasNoType` ''FL.Step
inspect $ 'mapN4 `hasNoType` ''SPEC
#endif

mapM4 :: Int -> IO ()
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

takeAll1 :: Int -> IO ()
takeAll1 value = withStream value (takeAll value 1)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'takeAll1
inspect $ 'takeAll1 `hasNoType` ''Stream.Step
inspect $ 'takeAll1 `hasNoType` ''FL.Step
inspect $ 'takeAll1 `hasNoType` ''SPEC
#endif

takeAll4 :: Int -> IO ()
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

takeWhileTrue1 :: Int -> IO ()
takeWhileTrue1 value = withStream value (takeWhileTrue value 1)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'takeWhileTrue1
inspect $ 'takeWhileTrue1 `hasNoType` ''Stream.Step
inspect $ 'takeWhileTrue1 `hasNoType` ''FL.Step
inspect $ 'takeWhileTrue1 `hasNoType` ''SPEC
#endif

takeWhileTrue4 :: Int -> IO ()
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

takeWhileMTrue4 :: Int -> IO ()
takeWhileMTrue4 value = withStream value (takeWhileMTrue value 4)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'takeWhileMTrue4
inspect $ 'takeWhileMTrue4 `hasNoType` ''Stream.Step
inspect $ 'takeWhileMTrue4 `hasNoType` ''FL.Step
inspect $ 'takeWhileMTrue4 `hasNoType` ''SPEC
#endif

-------------------------------------------------------------------------------
-- Multi-stream
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- Appending
-------------------------------------------------------------------------------

serial2 :: Int -> IO ()
serial2 count = withRandomIntIO $ \n ->
    drain $
        Common.append
            (sourceUnfoldrM count n)
            (sourceUnfoldrM count (n + 1))

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'serial2
inspect $ 'serial2 `hasNoType` ''SPEC
inspect $ 'serial2 `hasNoType` ''S.AppendState
inspect $ 'serial2 `hasNoType` ''S.Step
inspect $ 'serial2 `hasNoType` ''Fold.Step
#endif

serial4 :: Int -> IO ()
serial4 count = withRandomIntIO $ \n ->
    drain $
    Common.append
        (Common.append
            (sourceUnfoldrM count n)
            (sourceUnfoldrM count (n + 1)))
        (Common.append
              (sourceUnfoldrM count (n + 2))
              (sourceUnfoldrM count (n + 3)))

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'serial4
inspect $ 'serial4 `hasNoType` ''SPEC
inspect $ 'serial4 `hasNoType` ''S.AppendState
inspect $ 'serial4 `hasNoType` ''S.Step
inspect $ 'serial4 `hasNoType` ''Fold.Step
#endif

-------------------------------------------------------------------------------
-- Zipping
-------------------------------------------------------------------------------

zipWith :: Int -> IO ()
zipWith value = withRandomIntIO $ \n ->
    let src = sourceUnfoldrM value n
    in drain $ S.zipWith (,) src src

#ifdef INSPECTION
inspect $ 'zipWith `hasNoType` ''SPEC
-- inspect $ 'zipWith `hasNoType` ''S.Step
inspect $ 'zipWith `hasNoType` ''Fold.Step
#endif

zipWithM :: Int -> IO ()
zipWithM value = withRandomIntIO $ \n ->
    let src = sourceUnfoldrM value n
    in drain $ S.zipWithM (curry return) src src

#ifdef INSPECTION
inspect $ 'zipWithM `hasNoType` ''SPEC
-- inspect $ 'zipWithM `hasNoType` ''S.Step
inspect $ 'zipWithM `hasNoType` ''Fold.Step
#endif

-------------------------------------------------------------------------------
-- Concat
-------------------------------------------------------------------------------

{-# INLINE sourceConcatMapSingletonStreams #-}
sourceConcatMapSingletonStreams :: Monad m => Int -> Int -> Stream m (Stream m Int)
sourceConcatMapSingletonStreams count start =
    fmap Stream.fromPure $ sourceUnfoldr count start

{-# INLINE sourceConcatMapStreams #-}
sourceConcatMapStreams :: Monad m => Int -> Int -> Int -> Stream m (Stream m Int)
sourceConcatMapStreams outer inner start =
    fmap (sourceUnfoldr inner) $ sourceUnfoldr outer start

concatMap :: Int -> Int -> IO ()
concatMap outer inner = withRandomIntIO $ \n ->
    drain $ S.concatMap
        (sourceUnfoldrM inner)
        (sourceUnfoldrM outer n)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'concatMap
inspect $ 'concatMap `hasNoType` ''SPEC
-- inspect $ 'concatMap `hasNoType` ''S.Step
inspect $ 'concatMap `hasNoType` ''Fold.Step
#endif

concatMapM2 :: Int -> IO ()
concatMapM2 value = withStream value $ \s ->
    drain $ do
        Stream.concatMapM (\x ->
            pure $ Stream.concatMapM (\y ->
                pure $ Stream.fromPure $ x + y) s) s

concatMapM3 :: Int -> IO ()
concatMapM3 value = withStream value $ \s ->
    drain $ do
        Stream.concatMapM (\x ->
            pure $ Stream.concatMapM (\y ->
                pure $ Stream.concatMapM (\z ->
                    pure $ Stream.fromPure $ x + y + z) s) s) s

concatMapViaUnfoldEach :: Int -> Int -> IO ()
concatMapViaUnfoldEach outer inner = withRandomIntIO $ \n ->
    drain $ cmap
        (sourceUnfoldrM inner)
        (sourceUnfoldrM outer n)

    where

    cmap f = Stream.unfoldEach (UF.lmap f UF.fromStream)

concatMapM :: Int -> Int -> IO ()
concatMapM outer inner = withRandomIntIO $ \n ->
    drain $ S.concatMapM
        (return . sourceUnfoldrM inner)
        (sourceUnfoldrM outer n)

-- concatMap Streams

concatMapSingletonStreams :: Int -> IO ()
concatMapSingletonStreams value =
    withRandomIntIO (drain . S.concatMap id . sourceConcatMapSingletonStreams value)

concatMapStreams :: Int -> Int -> IO ()
concatMapStreams outer inner =
    withRandomIntIO (S.drain . S.concatMap id . sourceConcatMapStreams outer inner)

-- concatMap unfoldr/unfoldr

concatMapPure :: Int -> Int -> IO ()
concatMapPure outer inner = withRandomIntIO $ \n ->
    drain $ S.concatMap
        (sourceUnfoldr inner)
        (sourceUnfoldr outer n)

#ifdef INSPECTION
#if __GLASGOW_HASKELL__ >= 906
inspect $ hasNoTypeClassesExcept 'concatMapPure [''Applicative]
#else
inspect $ hasNoTypeClasses 'concatMapPure
#endif
inspect $ 'concatMapPure `hasNoType` ''SPEC
-- inspect $ 'concatMapPure `hasNoType` ''S.Step
inspect $ 'concatMapPure `hasNoType` ''Fold.Step
#endif

{-# INLINE sourceUnfoldrMUnfold #-}
sourceUnfoldrMUnfold :: Monad m => Int -> Int -> Unfold m Int Int
sourceUnfoldrMUnfold size start = UF.unfoldrM step

    where

    step i =
        return
            $ if i < start + size
              then Just (i, i + 1)
              else Nothing

unfoldEach :: Int -> Int -> IO ()
unfoldEach outer inner = withRandomIntIO $ \start -> drain $
     S.unfoldEach (sourceUnfoldrMUnfold inner start)
        $ sourceUnfoldrM outer start

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'unfoldEach
inspect $ 'unfoldEach `hasNoType` ''Producer.ConcatState
inspect $ 'unfoldEach `hasNoType` ''SPEC
inspect $ 'unfoldEach `hasNoType` ''S.Step
inspect $ 'unfoldEach `hasNoType` ''Fold.Step
#endif

unfoldEach2 :: Int -> Int -> IO ()
unfoldEach2 outer inner = withRandomIntIO $ \start -> drain $
     S.unfoldEach (UF.carryInput (sourceUnfoldrMUnfold inner start))
        $ sourceUnfoldrM outer start

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'unfoldEach2
inspect $ 'unfoldEach2 `hasNoType` ''Producer.ConcatState
inspect $ 'unfoldEach2 `hasNoType` ''S.Step
inspect $ 'unfoldEach2 `hasNoType` ''Fold.Step
inspect $ 'unfoldEach2 `hasNoType` ''SPEC
#endif

unfoldEach3 :: Int -> IO ()
unfoldEach3 linearCount = withRandomIntIO $ \start -> drain $ do
    S.unfoldEach (UF.carryInput (UF.lmap snd (sourceUnfoldrMUnfold nestedCount3 start)))
         $ S.unfoldEach (UF.carryInput (sourceUnfoldrMUnfold nestedCount3 start))
            $ sourceUnfoldrM nestedCount3 start
    where

    nestedCount3 = round (fromIntegral linearCount**(1/3::Double))

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'unfoldEach3
inspect $ 'unfoldEach3 `hasNoType` ''Producer.ConcatState
inspect $ 'unfoldEach3 `hasNoType` ''S.Step
inspect $ 'unfoldEach3 `hasNoType` ''Fold.Step
inspect $ 'unfoldEach3 `hasNoType` ''SPEC
#endif

unfoldCross :: Int -> Int -> IO ()
unfoldCross outer inner = withRandomIntIO $ \start -> drain $
    Stream.unfoldCross
        UF.identity
        (sourceUnfoldrM outer start)
        (sourceUnfoldrM inner start)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'unfoldCross
inspect $ 'unfoldCross `hasNoType` ''Producer.CrossState
inspect $ 'unfoldCross `hasNoType` ''Producer.ConcatState
inspect $ 'unfoldCross `hasNoType` ''S.Step
inspect $ 'unfoldCross `hasNoType` ''Fold.Step
inspect $ 'unfoldCross `hasNoType` ''SPEC
#endif

-------------------------------------------------------------------------------
-- Applicative
-------------------------------------------------------------------------------

{-# INLINE toNullApPure #-}
toNullApPure :: MonadAsync m => Int -> Int -> m ()
toNullApPure linearCount start = drain $ unCross $
    (+) <$> mkCross (sourceUnfoldr nestedCount2 start)
        <*> mkCross (sourceUnfoldr nestedCount2 start)

    where

    nestedCount2 = round (fromIntegral linearCount**(1/2::Double))

{-# INLINE toNullMPure #-}
toNullMPure :: MonadAsync m => Int -> Int -> m ()
toNullMPure linearCount start = drain $ unCross $ do
    x <- mkCross (sourceUnfoldr nestedCount2 start)
    y <- mkCross (sourceUnfoldr nestedCount2 start)
    return $ x + y

    where

    nestedCount2 = round (fromIntegral linearCount**(1/2::Double))

{-# INLINE toNullM3Pure #-}
toNullM3Pure :: MonadAsync m => Int -> Int -> m ()
toNullM3Pure linearCount start = drain $ unCross $ do
    x <- mkCross (sourceUnfoldr nestedCount3 start)
    y <- mkCross (sourceUnfoldr nestedCount3 start)
    z <- mkCross (sourceUnfoldr nestedCount3 start)
    return $ x + y + z

    where

    nestedCount3 = round (fromIntegral linearCount**(1/3::Double))

{-# INLINE filterAllOutMPure #-}
filterAllOutMPure :: MonadAsync m => Int -> Int -> m ()
filterAllOutMPure linearCount start = drain $ unCross $ do
    x <- mkCross (sourceUnfoldr nestedCount2 start)
    y <- mkCross (sourceUnfoldr nestedCount2 start)
    let s = x + y
    if s < 0
    then return s
    else mkCross Stream.nil

    where

    nestedCount2 = round (fromIntegral linearCount**(1/2::Double))

{-# INLINE filterAllInMPure #-}
filterAllInMPure :: MonadAsync m => Int -> Int -> m ()
filterAllInMPure linearCount start = drain $ unCross $ do
    x <- mkCross (sourceUnfoldr nestedCount2 start)
    y <- mkCross (sourceUnfoldr nestedCount2 start)
    let s = x + y
    if s > 0
    then return s
    else mkCross Stream.nil

    where

    nestedCount2 = round (fromIntegral linearCount**(1/2::Double))

cross2 :: Int -> IO ()
cross2 linearCount = withRandomIntIO $ \start -> drain $
    Stream.crossWith (+)
        (sourceUnfoldr nestedCount2 start)
        (sourceUnfoldr nestedCount2 start)

    where

    nestedCount2 = round (fromIntegral linearCount**(1/2::Double))

crossApply :: Int -> IO ()
crossApply linearCount = withRandomIntIO $ \start -> drain $
    Stream.crossApply
        ((+) <$> sourceUnfoldrM nestedCount2 start)
        (sourceUnfoldrM nestedCount2 start)

    where

    nestedCount2 = round (fromIntegral linearCount**(1/2::Double))

crossApplyFst :: Int -> IO ()
crossApplyFst linearCount = withRandomIntIO $ \start -> drain $
    Stream.crossApplyFst
        (sourceUnfoldrM nestedCount2 start)
        (sourceUnfoldrM nestedCount2 start)

    where

    nestedCount2 = round (fromIntegral linearCount**(1/2::Double))

crossApplySnd :: Int -> IO ()
crossApplySnd linearCount = withRandomIntIO $ \start -> drain $
    Stream.crossApplySnd
        (sourceUnfoldrM nestedCount2 start)
        (sourceUnfoldrM nestedCount2 start)

    where

    nestedCount2 = round (fromIntegral linearCount**(1/2::Double))

-------------------------------------------------------------------------------
-- Monad
-------------------------------------------------------------------------------

drainConcatFor1 :: Int -> IO ()
drainConcatFor1 count = withStream count $ \s ->
    drain $ Stream.concatFor s $ \x ->
        Stream.fromPure $ x + 1

drainConcatFor :: Int -> IO ()
drainConcatFor count = withStream count $ \s ->
    drain $ do
        Stream.concatFor s $ \x ->
            Stream.concatFor s $ \y ->
                Stream.fromPure $ x + y

drainConcatForM :: Int -> IO ()
drainConcatForM count = withStream count $ \s ->
    drain $ do
        Stream.concatForM s $ \x ->
            pure $ Stream.concatForM s $ \y ->
                pure $ Stream.fromPure $ x + y

drainConcatFor3 :: Int -> IO ()
drainConcatFor3 count = withStream count $ \s ->
    drain $ do
        Stream.concatFor s $ \x ->
            Stream.concatFor s $ \y ->
                Stream.concatFor s $ \z ->
                    Stream.fromPure $ x + y + z

drainConcatFor4 :: Int -> IO ()
drainConcatFor4 count = withStream count $ \s ->
    drain $ do
        Stream.concatFor s $ \x ->
            Stream.concatFor s $ \y ->
                Stream.concatFor s $ \z ->
                    Stream.concatFor s $ \w ->
                        Stream.fromPure $ x + y + z + w

drainConcatFor5 :: Int -> IO ()
drainConcatFor5 count = withStream count $ \s ->
    drain $ do
        Stream.concatFor s $ \x ->
            Stream.concatFor s $ \y ->
                Stream.concatFor s $ \z ->
                    Stream.concatFor s $ \w ->
                        Stream.concatFor s $ \u ->
                            Stream.fromPure $ x + y + z + w + u

drainConcatFor3M :: Int -> IO ()
drainConcatFor3M count = withStream count $ \s ->
    drain $ do
        Stream.concatForM s $ \x ->
            pure $ Stream.concatForM s $ \y ->
                pure $ Stream.concatForM s $ \z ->
                    pure $ Stream.fromPure $ x + y + z

filterAllInConcatFor :: Int -> IO ()
filterAllInConcatFor count = withStream count $ \s ->
    drain $ do
        Stream.concatFor s $ \x ->
            Stream.concatFor s $ \y ->
                let s1 = x + y
                 in if s1 > 0
                    then Stream.fromPure s1
                    else Stream.nil

filterAllOutConcatFor :: Int -> IO ()
filterAllOutConcatFor count = withStream count $ \s ->
    drain $ do
        Stream.concatFor s $ \x ->
            Stream.concatFor s $ \y ->
                let s1 = x + y
                 in if s1 < 0
                    then Stream.fromPure s1
                    else Stream.nil

-- search space |x| = 1000, |y| = 1000
{-# INLINE boundedInts #-}
boundedInts :: Monad m => Int -> Int -> Stream m Int
boundedInts n _ =
    Stream.interleave
        (Stream.enumerateFromTo (0 :: Int) n)
        (Stream.enumerateFromThenTo (-1) (-2) (-n))

{-# INLINE infiniteInts #-}
infiniteInts :: Monad m => Int -> Int -> Stream m Int
infiniteInts _ _ =
    Stream.interleave
        (Stream.enumerateFrom (0 :: Int))
        (Stream.enumerateFromThen (-1) (-2))

{-# INLINE boundedIntsUnfold #-}
boundedIntsUnfold :: Monad m => Int -> Int -> Unfold m ((), ()) Int
boundedIntsUnfold n _ =
    Unfold.interleave
        (Unfold.supply (0 :: Int, n) Unfold.enumerateFromTo)
        (Unfold.supply (-1, -2, -n) Unfold.enumerateFromThenTo)

{-# INLINE checkStream #-}
checkStream :: Applicative m =>
    Int -> Int -> Int -> Stream m (Maybe (Maybe (Int, Int)))
checkStream maxVal x y =
    let eq1 = x + y == 0
        eq2 = x - y == 2 * maxVal
     in if eq1 && eq2
        then Stream.fromPure (Just (Just (x,y)))
        else if abs x > maxVal && abs y > maxVal
        then Stream.fromPure (Just Nothing)
        else Stream.fromPure Nothing

{-# INLINE checkPair #-}
checkPair :: Monad m => Int -> (Int, Int) -> m (Maybe (Maybe (Int, Int)))
checkPair maxVal (x, y) =
    let eq1 = x + y == 0
        eq2 = x - y == 2 * maxVal
     in if eq1 && eq2
        then pure (Just (Just (x,y)))
        else if abs x > maxVal && abs y > maxVal
        then pure (Just Nothing)
        else pure Nothing

-- Terminate the stream as soon as we get a Just value
{-# INLINE result #-}
result :: Monad m => Stream m (Maybe a) -> m ()
result = Stream.fold (Fold.take 1 Fold.drain) . Stream.catMaybes

{-# INLINE concatForEqn #-}
concatForEqn :: Monad m => Int -> Stream m Int -> m ()
concatForEqn maxVal input =
    result
        $ Stream.concatFor input $ \x ->
              Stream.concatForM input $ \y -> do
                return $ checkStream maxVal x y

{-# INLINE streamCrossEqn #-}
streamCrossEqn :: Monad m => Int -> Stream m Int -> m ()
streamCrossEqn maxVal input =
    result
        $ Stream.mapM (checkPair maxVal)
        $ Stream.cross input input

{-# INLINE fairStreamCrossEqn #-}
fairStreamCrossEqn :: Monad m => Int -> Stream m Int -> m ()
fairStreamCrossEqn maxVal input =
    result
        $ Stream.mapM (checkPair maxVal)
        $ Stream.fairCross input input

{-# INLINE unfoldEachEqn #-}
unfoldEachEqn :: Monad m => Int -> Unfold m ((), ()) Int -> Stream m Int -> m ()
unfoldEachEqn maxVal input ints =
    let intu = Unfold.carryInput $ Unfold.lmap (const (undefined, undefined)) input
     in result
        $ Stream.mapM (checkPair maxVal)
        $ Stream.unfoldEach intu ints

concatForBounded :: Int -> IO ()
concatForBounded maxVal = withRandomIntIO $ \n ->
    concatForEqn maxVal (boundedInts maxVal n)

streamCrossBounded :: Int -> IO ()
streamCrossBounded maxVal = withRandomIntIO $ \n ->
    streamCrossEqn maxVal (boundedInts maxVal n)

fairStreamCrossBounded :: Int -> IO ()
fairStreamCrossBounded maxVal = withRandomIntIO $ \n ->
    fairStreamCrossEqn maxVal (boundedInts maxVal n)

fairStreamCrossInfinite :: Int -> IO ()
fairStreamCrossInfinite maxVal = withRandomIntIO $ \n ->
    fairStreamCrossEqn maxVal (infiniteInts maxVal n)

unfoldEachBounded :: Int -> IO ()
unfoldEachBounded maxVal = withRandomIntIO $ \n ->
    unfoldEachEqn maxVal (boundedIntsUnfold maxVal 0) (boundedInts maxVal n)

-------------------------------------------------------------------------------
-- Fold Many
-------------------------------------------------------------------------------

foldMany :: Int -> IO ()
foldMany value =
    withStream value $
          Common.drain
        . fmap getSum
        . S.foldMany (FL.take 2 FL.mconcat)
        . fmap Sum

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'foldMany
inspect $ 'foldMany `hasNoType` ''S.Step
inspect $ 'foldMany `hasNoType` ''S.FoldMany
inspect $ 'foldMany `hasNoType` ''FL.Step
inspect $ 'foldMany `hasNoType` ''SPEC
#endif

foldMany1 :: Int -> IO ()
foldMany1 value =
    withStream value $
          Common.drain
        . fmap getSum
        . S.foldManyPost (FL.take 2 FL.mconcat)
        . fmap Sum

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'foldMany1
inspect $ 'foldMany1 `hasNoType` ''S.Step
inspect $ 'foldMany1 `hasNoType` ''S.FoldManyPost
inspect $ 'foldMany1 `hasNoType` ''FL.Step
inspect $ 'foldMany1 `hasNoType` ''SPEC
#endif

refoldMany :: Int -> IO ()
refoldMany value =
    withStream value $
          Common.drain
        . fmap getSum
        . S.refoldMany (Refold.take 2 Refold.sconcat) (return mempty)
        . fmap Sum

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'refoldMany
inspect $ 'refoldMany `hasNoType` ''S.Step
inspect $ 'refoldMany `hasNoType` ''S.FoldMany
inspect $ 'refoldMany `hasNoType` ''FL.Step
inspect $ 'refoldMany `hasNoType` ''SPEC
#endif

-- {-# INLINE refoldIterateM #-}
refoldIterateM :: Int -> IO ()
refoldIterateM value =
    withStream value $
        Common.drain
            . fmap getSum
            . S.refoldIterateM
                (Refold.take 2 Refold.sconcat) (return (Sum 0))
            . fmap Sum

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'refoldIterateM
inspect $ 'refoldIterateM `hasNoType` ''S.Step
inspect $ 'refoldIterateM `hasNoType` ''S.CIterState
inspect $ 'refoldIterateM `hasNoType` ''FL.Step
inspect $ 'refoldIterateM `hasNoType` ''Refold.Tuple'Fused
inspect $ 'refoldIterateM `hasNoType` ''SPEC
#endif

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

-- In addition to gauge options, the number of elements in the stream can be
-- passed using the --stream-size option.
--
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
    -- Elimination
    -- Foldable instance
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
    , (SpaceO_1, benchIO "Foldable/mapM_" $ withRandomIntIO (foldableMapM_ size))
    -- TBD: for_
    -- TBD: forM_
    , (SpaceO_1, benchIO "Foldable/sequence_" $ withRandomIntIO (foldableSequence_ size))
    -- TBD: sequenceA_
    -- TBD: asum
    -- XXX needs to be fixed, results are in ns
    -- , (SpaceO_1, benchIOSink1 "Foldable/msum" (foldableMsum size))
    , (SpaceO_1, benchIO "foldl'/IO" $ foldl'Reduce size)
    , (SpaceO_1, benchIO "foldlM'/IO" $ foldlM'Reduce size)

    , (SpaceO_1, benchIO "foldl'/Identity" $ foldl'ReduceIdentity size)
    , (SpaceO_1, benchIO "foldlM'/Identity" $ foldlM'ReduceIdentity size)

    , (SpaceO_1, benchIO "foldrMElem/IO" $ foldrMElem size)

    , (SpaceO_1, benchIO "foldrMElem/Identity" $ foldrMElemIdentity size)
    , (SpaceO_1, benchIO "foldrMToList" $ foldrMToListIdentity size)

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
    -- Left folds for building a structure are inherently non-streaming
    -- as the structure cannot be lazily consumed until fully built.
    , (HeapO_n, benchIO "foldl'/build/IO" $ foldl'Build size)
    , (HeapO_n, benchIO "foldl'/build/Identity" $ foldl'BuildIdentity size)
    , (HeapO_n, benchIO "foldlM'/build/IO" $ foldlM'Build size)
    , (HeapO_n, benchIO "foldlM'/build/Identity" $ foldlM'BuildIdentity size)
    -- Buffers the output of show/read.
    -- XXX can the outputs be streaming? Can we have special read/show
    -- style type classes, readM/showM supporting streaming effects?
    , (HeapO_n, bench "showsPrec Haskell lists" $ nf showInstanceList (mkList size))
    -- XXX This is not o-1-space for GHC-8.10
    , (HeapO_n, benchIO "showsPrec pure streams" $ showInstance size)
    -- Head recursive strict right folds.
    -- accumulation due to strictness of IO monad
    , (SpaceO_n, benchIO "foldrM/build/IO (toList)" $ foldrMToList size)
    -- Right folds for reducing are inherently non-streaming as the
    -- expression needs to be fully built before it can be reduced.
    , (SpaceO_n, benchIO "foldrM/reduce/Identity (sum)" $ foldrMToSumIdentity size)
    , (SpaceO_n, benchIO "foldrM/reduce/IO (sum)" $ foldrMToSum size)
    -- Converting the stream to a list or pure stream in a strict monad
    , (SpaceO_n, benchIO "toList" $ toList' size)
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
    -- Filtering
    -- Trimming
    , (SpaceO_1, benchIO "take-all" $ takeAll1 size)
    , (SpaceO_1, benchIO "takeWhile-true" $ takeWhileTrue1 size)
 -- , (SpaceO_1, benchIO "takeWhileM-true" ...)
    -- trimming
    , (SpaceO_1, benchIO "take-all x 4" $ takeAll4 size)
    , (SpaceO_1, benchIO "takeWhile-true x 4" $ takeWhileTrue4 size)
    , (SpaceO_1, benchIO "takeWhileM-true x 4" $ takeWhileMTrue4 size)
    -- Multi-stream
    , (SpaceO_1, benchIO "serial" $ serial2 (size `div` 2))
    , (SpaceO_1, benchIO "serial (2,2,x/4)" $ serial4 (size `div` 4))
    , (SpaceO_1, benchIO "zipWith" $ zipWith size)
    , (SpaceO_1, benchIO "zipWithM" $ zipWithM size)
    , (SpaceO_1, benchIO "concatMap" $ concatMap 2 (size `div` 2))
    , (SpaceO_1, benchIO "concatMap unfoldr outer=Max inner=1" $
          concatMapPure size 1)
    , (SpaceO_1, benchIO "concatMap unfoldr outer=inner=(sqrt Max)" $
          concatMapPure sqrtVal sqrtVal)
    , (SpaceO_1, benchIO "concatMap unfoldr outer=1 inner=Max" $
          concatMapPure 1 size)
    , (SpaceO_1, benchIO "concatMap unfoldrM outer=max inner=1" $
          concatMap size 1)
    , (SpaceO_1, benchIO "concatMap unfoldrM outer=inner=(sqrt Max)" $
          concatMap sqrtVal sqrtVal)
    , (SpaceO_1, benchIO "concatMap unfoldrM outer=1 inner=Max" $
          concatMap 1 size)
    -- Using boxed values/streams may have entirely different perf profile
    , (SpaceO_1, benchIO "concatMap Streams fromPure outer=max inner=1" $
          concatMapSingletonStreams size)
    , (SpaceO_1, benchIO "concatMap Streams unfoldr outer=max inner=1" $
          concatMapStreams size 1)
    , (SpaceO_1, benchIO "concatMap Streams unfoldr outer=inner=(sqrt Max)" $
          concatMapStreams sqrtVal sqrtVal)
    , (SpaceO_1, benchIO "concatMap Streams unfoldr outer=1 inner=Max" $
          concatMapStreams 1 size)
    , (SpaceO_1, benchIO "concatMapM unfoldrM outer=max inner=1" $
          concatMapM size 1)
    , (SpaceO_1, benchIO "concatMapM unfoldrM outer=inner=(sqrt Max)" $
          concatMapM sqrtVal sqrtVal)
    , (SpaceO_1, benchIO "concatMapM unfoldrM outer=1 inner=Max" $
          concatMapM 1 size)
    , (SpaceO_1, benchIO "concatMapM2 fromPure" $ concatMapM2 sqrtVal)
    , (SpaceO_1, benchIO "concatMapM3 fromPure" $ concatMapM3 cubertVal)
    , (SpaceO_1, benchIO "concatMapViaUnfoldEach outer=max inner=1" $
          concatMapViaUnfoldEach size 1)
    , (SpaceO_1, benchIO "concatMapViaUnfoldEach outer=inner=(sqrt Max)" $
          concatMapViaUnfoldEach sqrtVal sqrtVal)
    , (SpaceO_1, benchIO "concatMapViaUnfoldEach outer=1 inner=Max" $
          concatMapViaUnfoldEach 1 size)
    , (SpaceO_1, benchIO "unfoldCross outer=max inner=1" $ unfoldCross size 1)
    , (SpaceO_1, benchIO "unfoldCross outer=inner=(sqrt Max)" $
          unfoldCross sqrtVal sqrtVal)
    , (SpaceO_1, benchIO "unfoldCross outer=1 inner=Max" $ unfoldCross 1 size)
    -- concatMap vs unfoldEach
    , (SpaceO_1, benchIO "unfoldEach outer=Max inner=1" $ unfoldEach size 1)
    , (SpaceO_1, benchIO "unfoldEach outer=inner=(sqrt Max)" $
          unfoldEach sqrtVal sqrtVal)
    , (SpaceO_1, benchIO "unfoldEach outer=1 inner=Max" $ unfoldEach 1 size)
    , (SpaceO_1, benchIO "unfoldEach2 outer=Max inner=1" $ unfoldEach2 size 1)
    , (SpaceO_1, benchIO "unfoldEach2 outer=inner=(sqrt Max)" $
          unfoldEach2 sqrtVal sqrtVal)
    , (SpaceO_1, benchIO "unfoldEach2 outer=1 inner=Max" $ unfoldEach2 1 size)
    , (SpaceO_1, benchIO "unfoldEach3 outer=inner=(cubert Max)" $ unfoldEach3 size)
    , (SpaceO_1, benchIO "(*>)" $ withRandomIntIO (apDiscardFst size))
    , (SpaceO_1, benchIO "(<*)" $ withRandomIntIO (apDiscardSnd size))
    , (SpaceO_1, benchIO "(<*>)" $ withRandomIntIO (toNullAp size))
    , (SpaceO_1, benchIO "liftA2" $ withRandomIntIO (apLiftA2 size))
    , (SpaceO_1, benchIO "crossApply" $ crossApply size)
    , (SpaceO_1, benchIO "crossApplyFst" $ crossApplyFst size)
    , (SpaceO_1, benchIO "crossApplySnd" $ crossApplySnd size)
    , (SpaceO_1, benchIO "pureDrain2" $ withRandomIntIO (toNullApPure size))
    , (SpaceO_1, benchIO "pureCross2" $ cross2 size)
    , (SpaceO_1, benchIO "then2M" $ withRandomIntIO (monadThen size))
    , (SpaceO_1, benchIO "drain2M" $ withRandomIntIO (toNullM size))
    , (SpaceO_1, benchIO "drain3M" $ withRandomIntIO (toNullM3 size))
    , (SpaceO_1, benchIO "filterAllOut2M" $ withRandomIntIO (filterAllOutM size))
    , (SpaceO_1, benchIO "filterAllIn2M" $ withRandomIntIO (filterAllInM size))
    , (SpaceO_1, benchIO "filterSome2M" $ withRandomIntIO (filterSome size))
    , (SpaceO_1, benchIO "breakAfterSome2M" $ withRandomIntIO (breakAfterSome size))
    , (SpaceO_1, benchIO "pureDrain2M" $ withRandomIntIO (toNullMPure size))
    , (SpaceO_1, benchIO "pureDrain3M" $ withRandomIntIO (toNullM3Pure size))
    , (SpaceO_1, benchIO "pureFilterAllIn2M" $ withRandomIntIO (filterAllInMPure size))
    , (SpaceO_1, benchIO "pureFilterAllOut2M" $ withRandomIntIO (filterAllOutMPure size))
    , (SpaceO_n, benchIO "toList2M" $ withRandomIntIO (toListM size))
    , (SpaceO_n, benchIO "toListSome2M" $ withRandomIntIO (toListSome size))
    , (SpaceO_1, benchIO "concatFor/drain1" $ drainConcatFor1 size)
    , (SpaceO_1, benchIO "concatFor/drain2" $ drainConcatFor sqrtVal)
    , (SpaceO_1, benchIO "concatFor/drain3" $ drainConcatFor3 cubertVal)
    , (SpaceO_1, benchIO "concatFor/drain4" $ drainConcatFor4 size4)
    , (SpaceO_1, benchIO "concatFor/drain5" $ drainConcatFor5 size5)
    , (SpaceO_1, benchIO "concatFor/drainM2" $ drainConcatForM sqrtVal)
    , (SpaceO_1, benchIO "concatFor/drainM3" $ drainConcatFor3M cubertVal)
    , (SpaceO_1, benchIO "concatFor/filterAllIn2" $ filterAllInConcatFor sqrtVal)
    , (SpaceO_1, benchIO "concatFor/filterAllOut2" $ filterAllOutConcatFor sqrtVal)
    -- Solve simultaneous equations by exploring all possibilities
    , (SpaceO_1, benchIO "equations/concatFor (bounded)" $
          concatForBounded sqrtVal)
    , (SpaceO_1, benchIO "equations/streamCross (bounded)" $
          streamCrossBounded sqrtVal)
    , (SpaceO_1, benchIO "equations/fairStreamCross (bounded)" $
          fairStreamCrossBounded sqrtVal)
    , (SpaceO_1, benchIO "equations/fairStreamCross (infinite)" $
          fairStreamCrossInfinite sqrtVal)
    , (SpaceO_1, benchIO "equations/unfoldEach (bounded)" $
          unfoldEachBounded sqrtVal)
    -- Fold Many
    , (SpaceO_1, benchIO "foldMany" $ foldMany size)
    , (SpaceO_1, benchIO "foldMany1" $ foldMany1 size)
    , (SpaceO_1, benchIO "refoldMany" $ refoldMany size)
    , (SpaceO_1, benchIO "refoldIterateM" $ refoldIterateM size)
    ]

    where

    sqrtVal = round $ sqrt (fromIntegral size :: Double) -- double nested loop
    cubertVal = round (fromIntegral size**(1/3::Double)) -- triple nested loop
    size4 = round (fromIntegral size**(1/4::Double)) -- 4 times nested loop
    size5 = round (fromIntegral size**(1/5::Double)) -- 5 times nested loop
