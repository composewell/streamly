-- |
-- Module      : Streamly.Benchmark.Prelude
-- Copyright   : (c) 2018 Harendra Kumar
--
-- License     : MIT
-- Maintainer  : streamly@composewell.com

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}

#ifdef __HADDOCK_VERSION__
#undef INSPECTION
#endif

#ifdef INSPECTION
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fplugin Test.Inspection.Plugin #-}
#endif

module Streamly.Benchmark.Prelude.Elimination
    ( o_1_space_serial_pure
    , o_1_space_serial_foldable
    , o_1_space_serial_elimination
    , o_1_space_serial_foldMultiStream

    , o_n_space_serial_toList

    , o_n_space_serial_foldr
    , o_n_heap_serial_foldl
    ) where

import Control.DeepSeq (NFData(..))
import Control.Monad.IO.Class (MonadIO(..))
import Data.Functor.Identity (Identity, runIdentity)

import System.Random (randomRIO)
import Prelude
       (Monad, String, Int, (+), ($), (.), return, (>), (<=), (==),
        undefined, Maybe(..), Bool, (>>=), IO, compare, flip)
import qualified Prelude as P
import qualified Data.Foldable as F
import qualified GHC.Exts as GHC

#ifdef INSPECTION
import GHC.Types (SPEC(..))
import Test.Inspection

import qualified Streamly.Internal.Data.Stream.StreamD as D
#endif

import qualified Streamly as S hiding (runStream)
import qualified Streamly.Prelude  as S
import qualified Streamly.Internal.Prelude as Internal

import qualified Streamly.Internal.Prelude as IP


import Gauge
import Streamly hiding (runStream)
import Streamly.Benchmark.Common

type Stream m a = S.SerialT m a


-------------------------------------------------------------------------------
-- Stream generation
-------------------------------------------------------------------------------

-- unfoldr

{-# INLINE sourceUnfoldr #-}
sourceUnfoldr :: (Monad m, S.IsStream t) => Int -> Int -> t m Int
sourceUnfoldr value n = S.unfoldr step n
    where
    step cnt =
        if cnt > n + value
        then Nothing
        else Just (cnt, cnt + 1)

{-# INLINE sourceUnfoldrM #-}
sourceUnfoldrM :: (S.IsStream t, S.MonadAsync m) => Int -> Int -> t m Int
sourceUnfoldrM value n = S.unfoldrM step n
    where
    step cnt =
        if cnt > n + value
        then return Nothing
        else return (Just (cnt, cnt + 1))

{-# INLINE source #-}
source :: (S.MonadAsync m, S.IsStream t) => Int -> Int -> t m Int
source = sourceUnfoldrM

{-# INLINE sourceUnfoldrAction #-}
sourceUnfoldrAction :: (S.IsStream t, Monad m, Monad m1)
    => Int -> Int -> t m (m1 Int)
sourceUnfoldrAction value n = S.serially $ S.unfoldr step n
    where
    step cnt =
        if cnt > n + value
        then Nothing
        else (Just (return cnt, cnt + 1))

-- fromIndices

{-# INLINE sourceIsList #-}
sourceIsList :: Int -> Int -> S.SerialT Identity Int
sourceIsList value n = GHC.fromList [n..n+value]

{-# INLINE sourceIsString #-}
sourceIsString :: Int -> Int -> S.SerialT Identity P.Char
sourceIsString value n = GHC.fromString (P.replicate (n + value) 'a')

-------------------------------------------------------------------------------
-- Elimination
-------------------------------------------------------------------------------

{-# INLINE runStream #-}
runStream :: Monad m => Stream m a -> m ()
runStream = S.drain

{-# INLINE toNull #-}
toNull :: Monad m => (t m a -> S.SerialT m a) -> t m a -> m ()
toNull t = runStream . t

{-# INLINE uncons #-}
uncons :: Monad m => Stream m Int -> m ()
uncons s = do
    r <- S.uncons s
    case r of
        Nothing -> return ()
        Just (_, t) -> uncons t

{-# INLINE init #-}
init :: Monad m => Stream m a -> m ()
init s = S.init s >>= P.mapM_ S.drain

{-# INLINE mapM_ #-}
mapM_ :: Monad m => Stream m Int -> m ()
mapM_ = S.mapM_ (\_ -> return ())

{-# INLINE toList #-}
toList :: Monad m => Stream m Int -> m [Int]
toList = S.toList

{-# INLINE toListRev #-}
toListRev :: Monad m => Stream m Int -> m [Int]
toListRev = Internal.toListRev

{-# INLINE foldrMElem #-}
foldrMElem :: Monad m => Int -> Stream m Int -> m Bool
foldrMElem e m =
    S.foldrM
        (\x xs ->
             if x == e
                 then return P.True
                 else xs)
        (return P.False)
        m

{-# INLINE foldrMToStream #-}
foldrMToStream :: Monad m => Stream m Int -> m (Stream Identity Int)
foldrMToStream = S.foldr S.cons S.nil

{-# INLINE foldrMBuild #-}
foldrMBuild :: Monad m => Stream m Int -> m [Int]
foldrMBuild = S.foldrM (\x xs -> xs >>= return . (x :)) (return [])

{-# INLINE foldl'Build #-}
foldl'Build :: Monad m => Stream m Int -> m [Int]
foldl'Build = S.foldl' (flip (:)) []

{-# INLINE foldlM'Build #-}
foldlM'Build :: Monad m => Stream m Int -> m [Int]
foldlM'Build = S.foldlM' (\xs x -> return $ x : xs) []

{-# INLINE foldrMReduce #-}
foldrMReduce :: Monad m => Stream m Int -> m Int
foldrMReduce = S.foldrM (\x xs -> xs >>= return . (x +)) (return 0)

{-# INLINE foldl'Reduce #-}
foldl'Reduce :: Monad m => Stream m Int -> m Int
foldl'Reduce = S.foldl' (+) 0

{-# INLINE foldl1'Reduce #-}
foldl1'Reduce :: Monad m => Stream m Int -> m (Maybe Int)
foldl1'Reduce = S.foldl1' (+)

{-# INLINE foldlM'Reduce #-}
foldlM'Reduce :: Monad m => Stream m Int -> m Int
foldlM'Reduce = S.foldlM' (\xs a -> return $ a + xs) 0

{-# INLINE last #-}
last :: Monad m => Stream m Int -> m (Maybe Int)
last = S.last

{-# INLINE _null #-}
_null :: Monad m => Stream m Int -> m Bool
_null = S.null

{-# INLINE _head #-}
_head :: Monad m => Stream m Int -> m (Maybe Int)
_head = S.head

{-# INLINE elem #-}
elem :: Monad m => Int -> Stream m Int -> m Bool
elem value = S.elem (value + 1)

{-# INLINE notElem #-}
notElem :: Monad m => Int -> Stream m Int -> m Bool
notElem value = S.notElem (value + 1)

{-# INLINE length #-}
length :: Monad m => Stream m Int -> m Int
length = S.length

{-# INLINE all #-}
all :: Monad m => Int -> Stream m Int -> m Bool
all value = S.all (<= (value + 1))

{-# INLINE any #-}
any :: Monad m => Int -> Stream m Int -> m Bool
any value = S.any (> (value + 1))

{-# INLINE and #-}
and :: Monad m => Int -> Stream m Int -> m Bool
and value = S.and . S.map (<= (value + 1))

{-# INLINE or #-}
or :: Monad m => Int -> Stream m Int -> m Bool
or value = S.or . S.map (> (value + 1))

{-# INLINE find #-}
find :: Monad m => Int -> Stream m Int -> m (Maybe Int)
find value = S.find (== (value + 1))

{-# INLINE findIndex #-}
findIndex :: Monad m => Int -> Stream m Int -> m (Maybe Int)
findIndex value = S.findIndex (== (value + 1))

{-# INLINE elemIndex #-}
elemIndex :: Monad m => Int -> Stream m Int -> m (Maybe Int)
elemIndex value = S.elemIndex (value + 1)

{-# INLINE maximum #-}
maximum :: Monad m => Stream m Int -> m (Maybe Int)
maximum = S.maximum

{-# INLINE minimum #-}
minimum :: Monad m => Stream m Int -> m (Maybe Int)
minimum = S.minimum

{-# INLINE sum #-}
sum :: Monad m => Stream m Int -> m Int
sum = S.sum

{-# INLINE product #-}
product :: Monad m => Stream m Int -> m Int
product = S.product

{-# INLINE minimumBy #-}
minimumBy :: Monad m => Stream m Int -> m (Maybe Int)
minimumBy = S.minimumBy compare

{-# INLINE maximumBy #-}
maximumBy :: Monad m => Stream m Int -> m (Maybe Int)
maximumBy = S.maximumBy compare

-------------------------------------------------------------------------------
-- Transformation
-------------------------------------------------------------------------------

{-# INLINE transform #-}
transform :: Monad m => Stream m a -> m ()
transform = runStream

{-# INLINE composeN #-}
composeN ::
       MonadIO m
    => Int
    -> (Stream m Int -> Stream m Int)
    -> Stream m Int
    -> m ()
composeN n f =
    case n of
        1 -> transform . f
        2 -> transform . f . f
        3 -> transform . f . f . f
        4 -> transform . f . f . f . f
        _ -> undefined

{-# INLINE reverse #-}
reverse :: MonadIO m => Int -> Stream m Int -> m ()
reverse n = composeN n $ S.reverse

{-# INLINE reverse' #-}
reverse' :: MonadIO m => Int -> Stream m Int -> m ()
reverse' n = composeN n $ Internal.reverse'

-------------------------------------------------------------------------------
-- Multi-stream folds
-------------------------------------------------------------------------------

{-# INLINE isPrefixOf #-}
isPrefixOf :: Monad m => Stream m Int -> m Bool
isPrefixOf src = S.isPrefixOf src src

{-# INLINE isSubsequenceOf #-}
isSubsequenceOf :: Monad m => Stream m Int -> m Bool
isSubsequenceOf src = S.isSubsequenceOf src src

{-# INLINE stripPrefix #-}
stripPrefix :: Monad m => Stream m Int -> m ()
stripPrefix src = do
    _ <- S.stripPrefix src src
    return ()

{-# INLINE eqBy' #-}
eqBy' :: (Monad m, P.Eq a) => Stream m a -> m P.Bool
eqBy' src = S.eqBy (==) src src

{-# INLINE eqBy #-}
eqBy :: Int -> Int -> IO Bool
eqBy value n = eqBy' (source value n)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'eqBy
inspect $ 'eqBy `hasNoType` ''SPEC
inspect $ 'eqBy `hasNoType` ''D.Step
#endif


{-# INLINE eqByPure #-}
eqByPure :: Int -> Int -> Identity Bool
eqByPure value n = eqBy' (sourceUnfoldr value n)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'eqByPure
inspect $ 'eqByPure `hasNoType` ''SPEC
inspect $ 'eqByPure `hasNoType` ''D.Step
#endif

{-# INLINE cmpBy' #-}
cmpBy' :: (Monad m, P.Ord a) => Stream m a -> m P.Ordering
cmpBy' src = S.cmpBy P.compare src src

{-# INLINE cmpBy #-}
cmpBy :: Int -> Int -> IO P.Ordering
cmpBy value n = cmpBy' (source value n)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'cmpBy
inspect $ 'cmpBy `hasNoType` ''SPEC
inspect $ 'cmpBy `hasNoType` ''D.Step
#endif

{-# INLINE cmpByPure #-}
cmpByPure :: Int -> Int -> Identity P.Ordering
cmpByPure value n = cmpBy' (sourceUnfoldr value n)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'cmpByPure
inspect $ 'cmpByPure `hasNoType` ''SPEC
inspect $ 'cmpByPure `hasNoType` ''D.Step
#endif

-------------------------------------------------------------------------------
-- Type class instances
-------------------------------------------------------------------------------

{-# INLINE eqInstance #-}
eqInstance :: Stream Identity Int -> Bool
eqInstance src = src == src

{-# INLINE eqInstanceNotEq #-}
eqInstanceNotEq :: Stream Identity Int -> Bool
eqInstanceNotEq src = src P./= src

{-# INLINE ordInstance #-}
ordInstance :: Stream Identity Int -> Bool
ordInstance src = src P.< src

{-# INLINE ordInstanceMin #-}
ordInstanceMin :: Stream Identity Int -> Stream Identity Int
ordInstanceMin src = P.min src src

{-# INLINE showInstance #-}
showInstance :: Stream Identity Int -> P.String
showInstance src = P.show src

-------------------------------------------------------------------------------
-- Pure (Identity) streams
-------------------------------------------------------------------------------

{-# INLINE pureFoldl' #-}
pureFoldl' :: Stream Identity Int -> Int
pureFoldl' = runIdentity . S.foldl' (+) 0

-------------------------------------------------------------------------------
-- Foldable Instance
-------------------------------------------------------------------------------

{-# INLINE foldableFoldl' #-}
foldableFoldl' :: Int -> Int -> Int
foldableFoldl' value n =
    F.foldl' (+) 0 (sourceUnfoldr value n :: S.SerialT Identity Int)

{-# INLINE foldableFoldrElem #-}
foldableFoldrElem :: Int -> Int -> Bool
foldableFoldrElem value n =
    F.foldr (\x xs -> if x == value then P.True else xs)
            (P.False)
            (sourceUnfoldr value n :: S.SerialT Identity Int)

{-# INLINE foldableSum #-}
foldableSum :: Int -> Int -> Int
foldableSum value n =
    P.sum (sourceUnfoldr value n :: S.SerialT Identity Int)

{-# INLINE foldableProduct #-}
foldableProduct :: Int -> Int -> Int
foldableProduct value n =
    P.product (sourceUnfoldr value n :: S.SerialT Identity Int)

{-# INLINE _foldableNull #-}
_foldableNull :: Int -> Int -> Bool
_foldableNull value n =
    P.null (sourceUnfoldr value n :: S.SerialT Identity Int)

{-# INLINE foldableElem #-}
foldableElem :: Int -> Int -> Bool
foldableElem value n =
    P.elem value (sourceUnfoldr value n :: S.SerialT Identity Int)

{-# INLINE foldableNotElem #-}
foldableNotElem :: Int -> Int -> Bool
foldableNotElem value n =
    P.notElem value (sourceUnfoldr value n :: S.SerialT Identity Int)

{-# INLINE foldableFind #-}
foldableFind :: Int -> Int -> Maybe Int
foldableFind value n =
    F.find (== (value + 1)) (sourceUnfoldr value n :: S.SerialT Identity Int)

{-# INLINE foldableAll #-}
foldableAll :: Int -> Int -> Bool
foldableAll value n =
    P.all (<= (value + 1)) (sourceUnfoldr value n :: S.SerialT Identity Int)

{-# INLINE foldableAny #-}
foldableAny :: Int -> Int -> Bool
foldableAny value n =
    P.any (> (value + 1)) (sourceUnfoldr value n :: S.SerialT Identity Int)

{-# INLINE foldableAnd #-}
foldableAnd :: Int -> Int -> Bool
foldableAnd value n =
    P.and $ S.map (<= (value + 1)) (sourceUnfoldr value n :: S.SerialT Identity Int)

{-# INLINE foldableOr #-}
foldableOr :: Int -> Int -> Bool
foldableOr value n =
    P.or $ S.map (> (value + 1)) (sourceUnfoldr value n :: S.SerialT Identity Int)

{-# INLINE foldableLength #-}
foldableLength :: Int -> Int -> Int
foldableLength value n =
    P.length (sourceUnfoldr value n :: S.SerialT Identity Int)

{-# INLINE foldableMin #-}
foldableMin :: Int -> Int -> Int
foldableMin value n =
    P.minimum (sourceUnfoldr value n :: S.SerialT Identity Int)

{-# INLINE foldableMax #-}
foldableMax :: Int -> Int -> Int
foldableMax value n =
    P.maximum (sourceUnfoldr value n :: S.SerialT Identity Int)

{-# INLINE foldableMinBy #-}
foldableMinBy :: Int -> Int -> Int
foldableMinBy value n =
    F.minimumBy compare (sourceUnfoldr value n :: S.SerialT Identity Int)

{-# INLINE foldableListMinBy #-}
foldableListMinBy :: Int -> Int -> Int
foldableListMinBy value n = F.minimumBy compare [1..value+n]

{-# INLINE foldableMaxBy #-}
foldableMaxBy :: Int -> Int -> Int
foldableMaxBy value n =
    F.maximumBy compare (sourceUnfoldr value n :: S.SerialT Identity Int)

{-# INLINE foldableToList #-}
foldableToList :: Int -> Int -> [Int]
foldableToList value n =
    F.toList (sourceUnfoldr value n :: S.SerialT Identity Int)

{-# INLINE foldableMapM_ #-}
foldableMapM_ :: Monad m => Int -> Int -> m ()
foldableMapM_ value n =
    F.mapM_ (\_ -> return ()) (sourceUnfoldr value n :: S.SerialT Identity Int)

{-# INLINE foldableSequence_ #-}
foldableSequence_ :: Int -> Int -> IO ()
foldableSequence_ value n =
    F.sequence_ (sourceUnfoldrAction value n :: S.SerialT Identity (IO Int))

{-# INLINE _foldableMsum #-}
_foldableMsum :: Int -> Int -> IO Int
_foldableMsum value n =
    F.msum (sourceUnfoldrAction value n :: S.SerialT Identity (IO Int))

-------------------------------------------------------------------------------
-- Benchmark groups
-------------------------------------------------------------------------------

-- We need a monadic bind here to make sure that the function f does not get
-- completely optimized out by the compiler in some cases.

-- | Takes a fold method, and uses it with a default source.
{-# INLINE benchIOSink #-}
benchIOSink
    :: (IsStream t, NFData b)
    => Int -> String -> (t IO Int -> IO b) -> Benchmark
benchIOSink value name f = bench name $ nfIO $ randomRIO (1,1) >>= f . source value

{-# INLINE benchHoistSink #-}
benchHoistSink
    :: (IsStream t, NFData b)
    => Int -> String -> (t Identity Int -> IO b) -> Benchmark
benchHoistSink value name f =
    bench name $ nfIO $ randomRIO (1,1) >>= f .  sourceUnfoldr value

-- XXX We should be using sourceUnfoldrM for fair comparison with IO monad, but
-- we can't use it as it requires MonadAsync constraint.
{-# INLINE benchIdentitySink #-}
benchIdentitySink
    :: (IsStream t, NFData b)
    => Int -> String -> (t Identity Int -> Identity b) -> Benchmark
benchIdentitySink value name f = bench name $ nf (f . sourceUnfoldr value) 1

{-# INLINE benchPureSink #-}
benchPureSink :: NFData b => Int -> String -> (SerialT Identity Int -> b) -> Benchmark
benchPureSink value name f = benchPure name (sourceUnfoldr value) f

-------------------------------------------------------------------------------
-- Benchmark groups
-------------------------------------------------------------------------------

o_1_space_serial_pure :: Int -> [Benchmark]
o_1_space_serial_pure value =
    [ bgroup
          "serially"
          [ bgroup
                "pure"
                [ benchPureSink value "id" P.id
                , benchPureSink1 "eqBy" (eqByPure value)
                , benchPureSink value "==" eqInstance
                , benchPureSink value "/=" eqInstanceNotEq
                , benchPureSink1 "cmpBy" (cmpByPure value)
                , benchPureSink value "<" ordInstance
                , benchPureSink value "min" ordInstanceMin
                , benchPureSrc "IsList.fromList" (sourceIsList value)
            -- length is used to check for foldr/build fusion
                , benchPureSink
                      value
                      "length . IsList.toList"
                      (P.length . GHC.toList)
                , benchPureSrc "IsString.fromString" (sourceIsString value)
                , benchPureSink value "showsPrec pure streams" showInstance
                , benchPureSink value "foldl'" pureFoldl'
                ]
          ]
    ]


o_1_space_serial_foldable :: Int -> [Benchmark]
o_1_space_serial_foldable value =
    [ bgroup
          "serially"
          [ bgroup
                "foldable"
              -- Foldable instance
              -- type class operations
                [ bench "foldl'" $ nf (foldableFoldl' value) 1
                , bench "foldrElem" $ nf (foldableFoldrElem value) 1
            -- , bench "null" $ nf (_foldableNull value) 1
                , bench "elem" $ nf (foldableElem value) 1
                , bench "length" $ nf (foldableLength value) 1
                , bench "sum" $ nf (foldableSum value) 1
                , bench "product" $ nf (foldableProduct value) 1
                , bench "minimum" $ nf (foldableMin value) 1
                , bench "maximum" $ nf (foldableMax value) 1
                , bench "length . toList" $
                  nf (P.length . foldableToList value) 1
            -- folds
                , bench "notElem" $ nf (foldableNotElem value) 1
                , bench "find" $ nf (foldableFind value) 1
                , bench "all" $ nf (foldableAll value) 1
                , bench "any" $ nf (foldableAny value) 1
                , bench "and" $ nf (foldableAnd value) 1
                , bench "or" $ nf (foldableOr value) 1
            -- Note: minimumBy/maximumBy do not work in constant memory they are in
            -- the O(n) group of benchmarks down below in this file.
            -- Applicative and Traversable operations
            -- TBD: traverse_
                , benchIOSink1 "mapM_" (foldableMapM_ value)
            -- TBD: for_
            -- TBD: forM_
                , benchIOSink1 "sequence_" (foldableSequence_ value)
            -- TBD: sequenceA_
            -- TBD: asum
            -- , benchIOSink1 "msum" (_foldableMsum value)
                ]
          ]
    ]


o_1_space_serial_elimination :: Int -> [Benchmark]
o_1_space_serial_elimination value =
    [ bgroup
          "serially"
          [ bgroup
                "elimination"
                [ bgroup
                      "reduce"
                      [ bgroup
                            "IO"
                            [ benchIOSink value "foldl'" foldl'Reduce
                            , benchIOSink value "foldl1'" foldl1'Reduce
                            , benchIOSink value "foldlM'" foldlM'Reduce
                            ]
                      , bgroup
                            "Identity"
                            [ benchIdentitySink value "foldl'" foldl'Reduce
                            , benchIdentitySink
                                  value
                                  "foldl1'"
                                  foldl1'Reduce
                            , benchIdentitySink
                                  value
                                  "foldlM'"
                                  foldlM'Reduce
                            ]
                      ]
                , bgroup
                      "build"
                      [ bgroup
                            "IO"
                            [ benchIOSink
                                  value
                                  "foldrMElem"
                                  (foldrMElem value)
                            ]
                      , bgroup
                            "Identity"
                            [ benchIdentitySink
                                  value
                                  "foldrMElem"
                                  (foldrMElem value)
                            , benchIdentitySink
                                  value
                                  "foldrMToStreamLength"
                                  (S.length . runIdentity . foldrMToStream)
                            , benchPureSink
                                  value
                                  "foldrMToListLength"
                                  (P.length . runIdentity . foldrMBuild)
                            ]
                      ]
                , benchIOSink value "uncons" uncons
                , benchIOSink value "toNull" $ toNull serially
                , benchIOSink value "mapM_" mapM_
                , benchIOSink value "init" init
            -- this is too low and causes all benchmarks reported in ns
            -- , benchIOSink value "head" head
                , benchIOSink value "last" last
            -- , benchIOSink value "lookup" lookup
                , benchIOSink value "find" (find value)
                , benchIOSink value "findIndex" (findIndex value)
                , benchIOSink value "elemIndex" (elemIndex value)
            -- this is too low and causes all benchmarks reported in ns
            -- , benchIOSink value "null" null
                , benchIOSink value "elem" (elem value)
                , benchIOSink value "notElem" (notElem value)
                , benchIOSink value "all" (all value)
                , benchIOSink value "any" (any value)
                , benchIOSink value "and" (and value)
                , benchIOSink value "or" (or value)
                , benchIOSink value "length" length
                , benchHoistSink
                      value
                      "length . generally"
                      (length . IP.generally)
                , benchIOSink value "sum" sum
                , benchIOSink value "product" product
                , benchIOSink value "maximumBy" maximumBy
                , benchIOSink value "maximum" maximum
                , benchIOSink value "minimumBy" minimumBy
                , benchIOSink value "minimum" minimum
                ]
          ]
    ]



o_1_space_serial_foldMultiStream :: Int -> [Benchmark]
o_1_space_serial_foldMultiStream value =
    [ bgroup
          "serially"
          [ bgroup
                "fold-multi-stream"
                [ benchIOSink1 "eqBy" (eqBy value)
                , benchIOSink1 "cmpBy" (cmpBy value)
                , benchIOSink value "isPrefixOf" isPrefixOf
                , benchIOSink value "isSubsequenceOf" isSubsequenceOf
                , benchIOSink value "stripPrefix" stripPrefix
                ]
          ]
    ]


o_n_space_serial_toList :: Int -> [Benchmark]
o_n_space_serial_toList value =
    [ bgroup
          "serially"
          [ bgroup
                "toList" -- < 2MB
          -- Converting the stream to a list or pure stream in a strict monad
                [ benchIOSink value "foldrMToList" foldrMBuild
                , benchIOSink value "toList" toList
                , benchIOSink value "toListRev" toListRev
          -- , benchIOSink value "toPure" toPure
          -- , benchIOSink value "toPureRev" toPureRev
                ]
          ]
    ]


o_n_space_serial_foldr :: Int -> [Benchmark]
o_n_space_serial_foldr value =
    [ bgroup
          "serially"
        -- Head recursive strict right folds.
          [ bgroup
                "foldr"
            -- < 2MB
          -- accumulation due to strictness of IO monad
                [ benchIOSink value "foldrM/build/IO" foldrMBuild
          -- Right folds for reducing are inherently non-streaming as the
          -- expression needs to be fully built before it can be reduced.
                , benchIdentitySink
                      value
                      "foldrM/reduce/Identity"
                      foldrMReduce
          -- takes < 4MB
                , benchIOSink value "foldrM/reduce/IO" foldrMReduce
          -- XXX the definitions of minimumBy and maximumBy in Data.Foldable use
          -- foldl1 which does not work in constant memory for our implementation.
          -- It works in constant memory for lists but even for lists it takes 15x
          -- more time compared to our foldl' based implementation.
          -- XXX these take < 16M stack space
                , bench "minimumBy" $ nf (flip foldableMinBy 1) value
                , bench "maximumBy" $ nf (flip foldableMaxBy 1) value
                , bench "minimumByList" $ nf (flip foldableListMinBy 1) value
                ]
          ]
    ]


o_n_heap_serial_foldl :: Int -> [Benchmark]
o_n_heap_serial_foldl value =
    [ bgroup
          "serially"
          [ bgroup
                "foldl"
          -- Left folds for building a structure are inherently non-streaming
          -- as the structure cannot be lazily consumed until fully built.
                [ benchIOSink value "foldl'/build/IO" foldl'Build
                , benchIdentitySink value "foldl'/build/Identity" foldl'Build
                , benchIOSink value "foldlM'/build/IO" foldlM'Build
                , benchIdentitySink
                      value
                      "foldlM'/build/Identity"
                      foldlM'Build
          -- Reversing/sorting a stream
                , benchIOSink value "reverse" (reverse 1)
                , benchIOSink value "reverse'" (reverse' 1)
                ]
          ]
    ]
