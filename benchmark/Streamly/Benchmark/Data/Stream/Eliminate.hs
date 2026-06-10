-- |
-- Module      : Stream.Eliminate
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

module Stream.Eliminate (benchmarks) where

import Control.Monad (when, (>=>))
import Data.Functor ((<&>))
import Control.DeepSeq (NFData(..))
import Data.Functor.Identity (Identity(..), runIdentity)
import System.Random (randomRIO)

import qualified Data.Foldable as F
import qualified GHC.Exts as GHC
import qualified Streamly.Internal.Data.Fold as Fold

#ifdef INSPECTION
import GHC.Types (SPEC(..))
import Test.Inspection
#endif

import Stream.Common hiding (benchIO)
import Streamly.Internal.Data.Stream (Stream)
import qualified Streamly.Internal.Data.Stream as S

import Test.Tasty.Bench
import Streamly.Benchmark.Common
import Prelude hiding (length, sum, or, and, any, all, notElem, elem, (!!),
    lookup, repeat, minimum, maximum, product, last, mapM_, init)
import qualified Prelude

-------------------------------------------------------------------------------
-- Helpers
-------------------------------------------------------------------------------

{-# INLINE benchIO #-}
benchIO :: NFData b => String -> IO b -> Benchmark
benchIO name = bench name . nfIO

{-# INLINE withRandomInt #-}
withRandomInt :: (Int -> b) -> IO b
withRandomInt f = randomRIO (1, 1 :: Int) <&> f

{-# INLINE withRandomIntIO #-}
withRandomIntIO :: (Int -> IO b) -> IO b
withRandomIntIO f = randomRIO (1, 1 :: Int) >>= f

{-# INLINE withStream #-}
withStream :: Int -> (Stream IO Int -> IO b) -> IO b
withStream value f = randomRIO (1, 1) >>= f . sourceUnfoldrM value

{-# INLINE withPureStream #-}
withPureStream :: Int -> (Stream Identity Int -> b) -> IO b
withPureStream value f = randomRIO (1, 1) <&> (f . sourceUnfoldr value)

-------------------------------------------------------------------------------
-- Elimination
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- Foldable Instance
-------------------------------------------------------------------------------

instance NFData a => NFData (Stream Identity a) where
    {-# INLINE rnf #-}
    rnf xs = runIdentity $ S.fold (Fold.foldl' (\_ x -> rnf x) ()) xs

{-# INLINE foldableFoldl' #-}
foldableFoldl' :: Int -> Int -> Int
foldableFoldl' value n =
    F.foldl' (+) 0 (sourceUnfoldr value n :: Stream Identity Int)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'foldableFoldl'
inspect $ 'foldableFoldl' `hasNoType` ''S.Step
#endif

{-# INLINE foldableFoldrElem #-}
foldableFoldrElem :: Int -> Int -> Bool
foldableFoldrElem value n =
    F.foldr (\x xs -> x == value || xs)
            False
            (sourceUnfoldr value n :: Stream Identity Int)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'foldableFoldrElem
inspect $ 'foldableFoldrElem `hasNoType` ''S.Step
inspect $ 'foldableFoldrElem `hasNoType` ''Fold.Step
inspect $ 'foldableFoldrElem `hasNoType` ''SPEC
#endif

{-# INLINE foldableSum #-}
foldableSum :: Int -> Int -> Int
foldableSum value n =
    Prelude.sum (sourceUnfoldr value n :: Stream Identity Int)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'foldableSum
inspect $ 'foldableSum `hasNoType` ''S.Step
inspect $ 'foldableSum `hasNoType` ''Fold.Step
inspect $ 'foldableSum `hasNoType` ''SPEC
#endif

{-# INLINE foldableProduct #-}
foldableProduct :: Int -> Int -> Int
foldableProduct value n =
    Prelude.product (sourceUnfoldr value n :: Stream Identity Int)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'foldableProduct
inspect $ 'foldableProduct `hasNoType` ''S.Step
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
inspect $ 'foldableElem `hasNoType` ''S.Step
inspect $ 'foldableElem `hasNoType` ''Fold.Step
inspect $ 'foldableElem `hasNoType` ''SPEC
#endif

{-# INLINE foldableNotElem #-}
foldableNotElem :: Int -> Int -> Bool
foldableNotElem value n =
    value `Prelude.notElem` (sourceUnfoldr value n :: Stream Identity Int)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'foldableNotElem
inspect $ 'foldableNotElem `hasNoType` ''S.Step
inspect $ 'foldableNotElem `hasNoType` ''Fold.Step
inspect $ 'foldableNotElem `hasNoType` ''SPEC
#endif

{-# INLINE foldableFind #-}
foldableFind :: Int -> Int -> Maybe Int
foldableFind value n =
    F.find (== (value + 1)) (sourceUnfoldr value n :: Stream Identity Int)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'foldableFind
inspect $ 'foldableFind `hasNoType` ''S.Step
inspect $ 'foldableFind `hasNoType` ''Fold.Step
inspect $ 'foldableFind `hasNoType` ''SPEC
#endif

{-# INLINE foldableAll #-}
foldableAll :: Int -> Int -> Bool
foldableAll value n =
    Prelude.all (<= (value + 1)) (sourceUnfoldr value n :: Stream Identity Int)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'foldableAll
inspect $ 'foldableAll `hasNoType` ''S.Step
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
inspect $ 'foldableAny `hasNoType` ''S.Step
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
inspect $ 'foldableAnd `hasNoType` ''S.Step
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
inspect $ 'foldableOr `hasNoType` ''S.Step
inspect $ 'foldableOr `hasNoType` ''Fold.Step
inspect $ 'foldableOr `hasNoType` ''SPEC
#endif

{-# INLINE foldableLength #-}
foldableLength :: Int -> Int -> Int
foldableLength value n =
    Prelude.length (sourceUnfoldr value n :: Stream Identity Int)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'foldableLength
inspect $ 'foldableLength `hasNoType` ''S.Step
inspect $ 'foldableLength `hasNoType` ''Fold.Step
inspect $ 'foldableLength `hasNoType` ''SPEC
#endif

{-# INLINE foldableMin #-}
foldableMin :: Int -> Int -> Int
foldableMin value n =
    Prelude.minimum (sourceUnfoldr value n :: Stream Identity Int)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'foldableMin
inspect $ 'foldableMin `hasNoType` ''S.Step
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
inspect $ 'ordInstanceMin `hasNoType` ''S.Step
inspect $ 'ordInstanceMin `hasNoType` ''Fold.Step
inspect $ 'ordInstanceMin `hasNoType` ''SPEC
#endif

{-# INLINE foldableMax #-}
foldableMax :: Int -> Int -> Int
foldableMax value n =
    Prelude.maximum (sourceUnfoldr value n :: Stream Identity Int)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'foldableMax
inspect $ 'foldableMax `hasNoType` ''S.Step
inspect $ 'foldableMax `hasNoType` ''Fold.Step
inspect $ 'foldableMax `hasNoType` ''SPEC
#endif

{-# INLINE foldableMinBy #-}
foldableMinBy :: Int -> Int -> Int
foldableMinBy value n =
    F.minimumBy compare (sourceUnfoldr value n :: Stream Identity Int)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'foldableMinBy
inspect $ 'foldableMinBy `hasNoType` ''S.Step
inspect $ 'foldableMinBy `hasNoType` ''Fold.Step
inspect $ 'foldableMinBy `hasNoType` ''SPEC
#endif

{-# INLINE foldableListMinBy #-}
foldableListMinBy :: Int -> Int -> Int
foldableListMinBy value n = F.minimumBy compare [1..value+n]

-- Not inspection-tested: 'foldableListMinBy' folds a plain list, not a stream.

{-# INLINE foldableMaxBy #-}
foldableMaxBy :: Int -> Int -> Int
foldableMaxBy value n =
    F.maximumBy compare (sourceUnfoldr value n :: Stream Identity Int)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'foldableMaxBy
inspect $ 'foldableMaxBy `hasNoType` ''S.Step
inspect $ 'foldableMaxBy `hasNoType` ''Fold.Step
inspect $ 'foldableMaxBy `hasNoType` ''SPEC
#endif

{-# INLINE foldableToList #-}
foldableToList :: Int -> Int -> [Int]
foldableToList value n =
    F.toList (sourceUnfoldr value n :: Stream Identity Int)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'foldableToList
inspect $ 'foldableToList `hasNoType` ''S.Step
inspect $ 'foldableToList `hasNoType` ''Fold.Step
inspect $ 'foldableToList `hasNoType` ''SPEC
#endif

{-# INLINE foldableMapM_ #-}
foldableMapM_ :: Int -> Int -> IO ()
foldableMapM_ value n =
    F.mapM_ (\_ -> return ()) (sourceUnfoldr value n :: Stream Identity Int)

-- 'foldableMapM_' is the only 'Foldable' benchmark that is polymorphic in the
-- monad; the rest already bake a concrete 'Stream Identity' source and are
-- inspected directly.
#ifdef INSPECTION
inspect $ hasNoTypeClasses 'foldableMapM_
inspect $ 'foldableMapM_ `hasNoType` ''S.Step
inspect $ 'foldableMapM_ `hasNoType` ''Fold.Step
inspect $ 'foldableMapM_ `hasNoType` ''SPEC
#endif

{-# INLINE foldableSequence_ #-}
foldableSequence_ :: Int -> Int -> IO ()
foldableSequence_ value n =
    F.sequence_ (sourceUnfoldrAction value n :: Stream Identity (IO Int))

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'foldableSequence_
inspect $ 'foldableSequence_ `hasNoType` ''S.Step
inspect $ 'foldableSequence_ `hasNoType` ''Fold.Step
inspect $ 'foldableSequence_ `hasNoType` ''SPEC
#endif

{-# INLINE _foldableMsum #-}
_foldableMsum :: Int -> Int -> IO Int
_foldableMsum value n =
    F.msum (sourceUnfoldrAction value n :: Stream Identity (IO Int))

{-# INLINE showInstance #-}
showInstance :: Int -> IO String
showInstance value = withPureStream value show

o_1_space_elimination_foldable :: Int -> [Benchmark]
o_1_space_elimination_foldable value =
    [ bgroup "foldable"
          -- Foldable instance
        [ benchIO "foldl'" $ withRandomInt (foldableFoldl' value)
        , benchIO "foldrElem" $ withRandomInt (foldableFoldrElem value)
     -- , benchIO "null" $ withRandomInt (_foldableNull value)
        , benchIO "elem" $ withRandomInt (foldableElem value)
        , benchIO "length" $ withRandomInt (foldableLength value)
        , benchIO "sum" $ withRandomInt (foldableSum value)
        , benchIO "product" $ withRandomInt (foldableProduct value)
        , benchIO "minimum" $ withRandomInt (foldableMin value)
        , benchIO "min (ord)" $ withRandomInt (ordInstanceMin value)
        , benchIO "maximum" $ withRandomInt (foldableMax value)
        , benchIO "minimumBy" $ withRandomInt (foldableMinBy value)
        , benchIO "maximumBy" $ withRandomInt (foldableMaxBy value)
        , benchIO "minimumByList" $ withRandomInt (foldableListMinBy value)
        , benchIO "length . toList" $
              withRandomInt (Prelude.length . foldableToList value)
        , benchIO "notElem" $ withRandomInt (foldableNotElem value)
        , benchIO "find" $ withRandomInt (foldableFind value)
        , benchIO "all" $ withRandomInt (foldableAll value)
        , benchIO "any" $ withRandomInt (foldableAny value)
        , benchIO "and" $ withRandomInt (foldableAnd value)
        , benchIO "or" $ withRandomInt (foldableOr value)

        -- Applicative and Traversable operations
        -- TBD: traverse_
        , benchIO "mapM_" $ withRandomIntIO (foldableMapM_ value)
        -- TBD: for_
        -- TBD: forM_
        , benchIO "sequence_" $ withRandomIntIO (foldableSequence_ value)
        -- TBD: sequenceA_
        -- TBD: asum
        -- XXX needs to be fixed, results are in ns
        -- , benchIOSink1 "msum" (foldableMsum value)
        ]
    ]

-------------------------------------------------------------------------------
-- Stream folds
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- Reductions
-------------------------------------------------------------------------------

{-# INLINE uncons #-}
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

{-# INLINE toNull #-}
toNull :: Int -> IO ()
toNull value = withStream value S.drain

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'toNull
inspect $ 'toNull `hasNoType` ''S.Step
inspect $ 'toNull `hasNoType` ''Fold.Step
inspect $ 'toNull `hasNoType` ''SPEC
#endif

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

{-# INLINE foldBreak #-}
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

{-# INLINE foldrMElem #-}
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

{-# INLINE foldrMElemIdentity #-}
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

-- {-# INLINE foldrToStream #-}
-- foldrToStream :: Monad m => Stream m Int -> m (Stream Identity Int)
-- foldrToStream = S.foldr S.cons S.nil

{-# INLINE foldrMToList #-}
foldrMToList :: Int -> IO [Int]
foldrMToList value =
    withStream value $ S.foldrM (\x xs -> (x :) <$> xs) (return [])

{-# INLINE foldrMToListIdentity #-}
foldrMToListIdentity :: Int -> IO [Int]
foldrMToListIdentity value =
    withPureStream value
        (runIdentity . S.foldrM (\x xs -> (x :) <$> xs) (return []))

{-# INLINE foldl'Reduce #-}
foldl'Reduce :: Int -> IO Int
foldl'Reduce value = withStream value (S.foldl' (+) 0)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'foldl'Reduce
inspect $ 'foldl'Reduce `hasNoType` ''S.Step
#endif

{-# INLINE foldl'ReduceIdentity #-}
foldl'ReduceIdentity :: Int -> IO Int
foldl'ReduceIdentity value =
    withPureStream value $ runIdentity . S.foldl' (+) 0

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'foldl'ReduceIdentity
inspect $ 'foldl'ReduceIdentity `hasNoType` ''S.Step
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

{-# INLINE foldlM'Reduce #-}
foldlM'Reduce :: Int -> IO Int
foldlM'Reduce value =
    withStream value (S.foldlM' (\xs a -> return $ a + xs) (return 0))

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'foldlM'Reduce
inspect $ 'foldlM'Reduce `hasNoType` ''S.Step
#endif

{-# INLINE foldlM'ReduceIdentity #-}
foldlM'ReduceIdentity :: Int -> IO Int
foldlM'ReduceIdentity value =
    withPureStream value $
        runIdentity . S.foldlM' (\xs a -> return $ a + xs) (return 0)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'foldlM'ReduceIdentity
inspect $ 'foldlM'ReduceIdentity `hasNoType` ''S.Step
#endif

{-# INLINE _head #-}
_head :: Monad m => Stream m Int -> m (Maybe Int)
_head = S.head

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

{-# INLINE findIndex #-}
findIndex :: Int -> IO (Maybe Int)
findIndex value = withStream value (S.head . S.findIndices (== (value + 1)))

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'findIndex
inspect $ 'findIndex `hasNoType` ''S.Step
inspect $ 'findIndex `hasNoType` ''Fold.Step
inspect $ 'findIndex `hasNoType` ''SPEC
#endif

{-# INLINE elemIndex #-}
elemIndex :: Int -> IO (Maybe Int)
elemIndex value = withStream value (S.head . S.elemIndices (value + 1))

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'elemIndex
inspect $ 'elemIndex `hasNoType` ''S.Step
inspect $ 'elemIndex `hasNoType` ''Fold.Step
inspect $ 'elemIndex `hasNoType` ''SPEC
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

{-# INLINE drainN #-}
drainN :: Int -> IO ()
drainN value = withStream value (S.fold (Fold.drainN value))

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'drainN
inspect $ 'drainN `hasNoType` ''S.Step
inspect $ 'drainN `hasNoType` ''Fold.Step
inspect $ 'drainN `hasNoType` ''SPEC
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

{-# INLINE generalizeInner #-}
generalizeInner :: Int -> IO Int
generalizeInner value =
    withPureStream value $
        runIdentity . S.fold Fold.length . S.generalizeInner

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'generalizeInner
inspect $ 'generalizeInner `hasNoType` ''S.Step
inspect $ 'generalizeInner `hasNoType` ''Fold.Step
inspect $ 'generalizeInner `hasNoType` ''SPEC
#endif

{-# INLINE drainPure #-}
drainPure :: Int -> IO ()
drainPure value = withPureStream value $ runIdentity . drain

o_1_space_elimination_folds :: Int -> [Benchmark]
o_1_space_elimination_folds value =
    [ bgroup "elimination"
        -- Basic folds
        [
            bgroup "reduce"
            [ bgroup
                  "IO"
                  [ benchIO "foldl'" $ foldl'Reduce value
                  , benchIO "foldl1'" $ foldl1'Reduce value
                  , benchIO "foldlM'" $ foldlM'Reduce value
                  ]

            , bgroup
                  "Identity"
                  [ benchIO "foldl'" $ foldl'ReduceIdentity value
                  , benchIO "foldl1'" $ foldl1'ReduceIdentity value
                  , benchIO "foldlM'" $ foldlM'ReduceIdentity value
                  ]
            ] ,
         bgroup "build"
            [ bgroup "IO"
                  [ benchIO "foldrMElem" $ foldrMElem value
                  ]
            , bgroup "Identity"
                  [ benchIO "foldrMElem" $ foldrMElemIdentity value
                  , benchIO "foldrMToList" $ foldrMToListIdentity value
                  ]
            ]

        -- deconstruction
        , benchIO "uncons" $ uncons value
        , benchIO "mapM_" $ mapM_ value
        , benchIO "last" $ streamLast value
        , benchIO "length . generalizeInner" $ generalizeInner value
        , benchIO "toNull" $ toNull value
        , benchIO "foldBreak" $ foldBreak value
        , benchIO "init" $ streamInit value

        -- draining
        , benchIO "drainN" $ drainN value
        , benchIO "drain (pure)" $ drainPure value

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
        , benchIO "findIndex" $ findIndex value
        , benchIO "elemIndex" $ elemIndex value
        -- this is too fast, causes all benchmarks reported in ns
    -- , benchIO "null" $ ...
        , benchIO "elem" $ elem value
        , benchIO "notElem" $ notElem value
        , benchIO "all" $ all value
        , benchIO "any" $ any value
        , benchIO "and" $ and value
        , benchIO "or" $ or value

        -- length is used to check for foldr/build fusion
        , benchIO "length . IsList.toList" $
              withPureStream value (Prelude.length . GHC.toList)
        ]
    ]

-------------------------------------------------------------------------------
-- Buffered Transformations by fold
-------------------------------------------------------------------------------

{-# INLINE foldl'Build #-}
foldl'Build :: Int -> IO [Int]
foldl'Build value = withStream value (S.foldl' (flip (:)) [])

{-# INLINE foldl'BuildIdentity #-}
foldl'BuildIdentity :: Int -> IO [Int]
foldl'BuildIdentity value =
    withPureStream value (runIdentity . S.foldl' (flip (:)) [])

{-# INLINE foldlM'Build #-}
foldlM'Build :: Int -> IO [Int]
foldlM'Build value =
    withStream value (S.foldlM' (\xs x -> return $ x : xs) (return []))

{-# INLINE foldlM'BuildIdentity #-}
foldlM'BuildIdentity :: Int -> IO [Int]
foldlM'BuildIdentity value =
    withPureStream value
        (runIdentity . S.foldlM' (\xs x -> return $ x : xs) (return []))

o_n_heap_elimination_foldl :: Int -> [Benchmark]
o_n_heap_elimination_foldl value =
    [ bgroup "foldl"
        -- Left folds for building a structure are inherently non-streaming
        -- as the structure cannot be lazily consumed until fully built.
        [ benchIO "foldl'/build/IO" $ foldl'Build value
        , benchIO "foldl'/build/Identity" $ foldl'BuildIdentity value
        , benchIO "foldlM'/build/IO" $ foldlM'Build value
        , benchIO "foldlM'/build/Identity" $ foldlM'BuildIdentity value
        ]
    ]

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
        [ bench "showsPrec Haskell lists" $ nf showInstanceList (mkList value)
        -- XXX This is not o-1-space for GHC-8.10
        , benchIO "showsPrec pure streams" $ showInstance value
        ]
    ]

{-# INLINE foldrMToSum #-}
foldrMToSum :: Int -> IO Int
foldrMToSum value =
    withStream value (S.foldrM (\x xs -> (x +) <$> xs) (return 0))

{-# INLINE foldrMToSumIdentity #-}
foldrMToSumIdentity :: Int -> IO Int
foldrMToSumIdentity value =
    withPureStream value
        (runIdentity . S.foldrM (\x xs -> (x +) <$> xs) (return 0))

o_n_space_elimination_foldr :: Int -> [Benchmark]
o_n_space_elimination_foldr value =
    -- Head recursive strict right folds.
    [ bgroup "foldr"
        -- accumulation due to strictness of IO monad
        [ benchIO "foldrM/build/IO (toList)" $ foldrMToList value
        -- Right folds for reducing are inherently non-streaming as the
        -- expression needs to be fully built before it can be reduced.
        , benchIO "foldrM/reduce/Identity (sum)" $ foldrMToSumIdentity value
        , benchIO "foldrM/reduce/IO (sum)" $ foldrMToSum value

        -- This is horribly slow, never finishes
        -- let foldlS = composeN n $ S.foldlS (flip S.cons) S.nil
        --  in benchFold "foldlS"  (foldlS    1) sourceUnfoldrM
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

{-# INLINE toList' #-}
toList' :: Int -> IO [Int]
toList' value = withStream value S.toList

-- NOTE: this is a Fold benchmark, used here only for comparison with ToList
{-# INLINE toStream #-}
toStream :: Int -> IO (Stream Identity Int)
toStream value = withStream value (S.fold Fold.toStream)

o_n_space_elimination_toList :: Int -> [Benchmark]
o_n_space_elimination_toList value =
    [ bgroup "toList"
        -- Converting the stream to a list or pure stream in a strict monad
        [ benchIO "toList" $ toList' value
        , benchIO "toStream" $ toStream value
        ]
    ]

-------------------------------------------------------------------------------
-- Multi-stream folds
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- Multi-stream pure
-------------------------------------------------------------------------------

{-# INLINE eqByPure #-}
eqByPure :: Int -> IO Bool
eqByPure value =
    withPureStream value $ \src -> runIdentity $ S.eqBy (==) src src

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'eqByPure
inspect $ 'eqByPure `hasNoType` ''SPEC
inspect $ 'eqByPure `hasNoType` ''S.Step
inspect $ 'eqByPure `hasNoType` ''Fold.Step
#endif

{-# INLINE eqInstance #-}
eqInstance :: Int -> IO Bool
eqInstance value = withPureStream value $ \src -> src == src

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'eqInstance
inspect $ 'eqInstance `hasNoType` ''S.Step
inspect $ 'eqInstance `hasNoType` ''Fold.Step
inspect $ 'eqInstance `hasNoType` ''SPEC
#endif

{-# INLINE eqInstanceNotEq #-}
eqInstanceNotEq :: Int -> IO Bool
eqInstanceNotEq value = withPureStream value $ \src -> src /= src

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'eqInstanceNotEq
inspect $ 'eqInstanceNotEq `hasNoType` ''S.Step
inspect $ 'eqInstanceNotEq `hasNoType` ''Fold.Step
inspect $ 'eqInstanceNotEq `hasNoType` ''SPEC
#endif

{-# INLINE cmpByPure #-}
cmpByPure :: Int -> IO Ordering
cmpByPure value =
    withPureStream value $ \src -> runIdentity $ S.cmpBy compare src src

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'cmpByPure
inspect $ 'cmpByPure `hasNoType` ''SPEC
inspect $ 'cmpByPure `hasNoType` ''S.Step
inspect $ 'cmpByPure `hasNoType` ''Fold.Step
#endif

{-# INLINE ordInstance #-}
ordInstance :: Int -> IO Bool
ordInstance value = withPureStream value $ \src -> src < src

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'ordInstance
inspect $ 'ordInstance `hasNoType` ''S.Step
inspect $ 'ordInstance `hasNoType` ''Fold.Step
inspect $ 'ordInstance `hasNoType` ''SPEC
#endif

o_1_space_elimination_multi_stream_pure :: Int -> [Benchmark]
o_1_space_elimination_multi_stream_pure value =
    [ bgroup "multi-stream-pure"
        [ benchIO "eqBy" $ eqByPure value
        , benchIO "==" $ eqInstance value
        , benchIO "/=" $ eqInstanceNotEq value
        , benchIO "cmpBy" $ cmpByPure value
        , benchIO "<" $ ordInstance value
        ]
    ]

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

{-# INLINE eqBy #-}
eqBy :: Int -> IO Bool
eqBy value = withStream value $ \src -> S.eqBy (==) src src

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'eqBy
inspect $ 'eqBy `hasNoType` ''SPEC
inspect $ 'eqBy `hasNoType` ''S.Step
inspect $ 'eqBy `hasNoType` ''Fold.Step
#endif

{-# INLINE cmpBy #-}
cmpBy :: Int -> IO Ordering
cmpBy value = withStream value $ \src -> S.cmpBy compare src src

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'cmpBy
inspect $ 'cmpBy `hasNoType` ''SPEC
inspect $ 'cmpBy `hasNoType` ''S.Step
inspect $ 'cmpBy `hasNoType` ''Fold.Step
#endif

o_1_space_elimination_multi_stream :: Int -> [Benchmark]
o_1_space_elimination_multi_stream value =
    [ bgroup "multi-stream"
        [ benchIO "eqBy" $ eqBy value
        , benchIO "cmpBy" $ cmpBy value
        , benchIO "isPrefixOf" $ isPrefixOf value
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
        [ o_1_space_elimination_foldable size
        , o_1_space_elimination_folds size
        , o_1_space_elimination_multi_stream_pure size
        , o_1_space_elimination_multi_stream size
        ])
    ++ map (HeapO_n,) (
           o_n_heap_elimination_buffered size
        ++ o_n_heap_elimination_foldl size
        ++ o_n_heap_elimination_toList size)
    ++ map (SpaceO_n,) (
        o_n_space_elimination_foldr size
        ++ o_n_space_elimination_toList size)
