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
#ifdef USE_PRELUDE
{-# OPTIONS_GHC -Wno-deprecations #-}
#endif

#ifdef __HADDOCK_VERSION__
#undef INSPECTION
#endif

#ifdef INSPECTION
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fplugin Test.Inspection.Plugin #-}
#endif

module Stream.Eliminate (benchmarks) where

import Control.DeepSeq (NFData(..))
import Data.Functor.Identity (Identity, runIdentity)
import System.Random (randomRIO)

import qualified Data.Foldable as F
import qualified GHC.Exts as GHC
import qualified Streamly.Internal.Data.Fold as Fold

#ifdef INSPECTION
import GHC.Types (SPEC(..))
import Test.Inspection
import qualified Streamly.Internal.Data.Stream as D
#endif

#ifdef USE_PRELUDE
import Streamly.Prelude (fromSerial)
import Streamly.Benchmark.Prelude
import qualified Streamly.Internal.Data.Stream.IsStream as S
import qualified Streamly.Internal.Data.Stream.IsStream as StreamK
#else
import Stream.Common
import Streamly.Internal.Data.Stream (Stream)
import qualified Streamly.Internal.Data.Stream as S
#ifdef USE_STREAMK
import Streamly.Internal.Data.StreamK (StreamK)
import qualified Streamly.Data.StreamK as StreamK
#else
import qualified Streamly.Internal.Data.Stream as StreamK
#endif
#endif

import Test.Tasty.Bench
import Streamly.Benchmark.Common
import Prelude hiding (length, sum, or, and, any, all, notElem, elem, (!!),
    lookup, repeat, minimum, maximum, product, last, mapM_, init)
import qualified Prelude

#ifdef USE_PRELUDE
type Stream = S.SerialT
fromStream = id

{-# INLINE repeat #-}
repeat :: (Monad m, S.IsStream t) => Int -> Int -> t m Int
repeat count = S.take count . S.repeat
#endif

-------------------------------------------------------------------------------
-- Elimination
-------------------------------------------------------------------------------

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

{- HLINT ignore "Use all"-}
{-# INLINE foldableAnd #-}
foldableAnd :: Int -> Int -> Bool
foldableAnd value n =
    Prelude.and $ fmap
        (<= (value + 1)) (sourceUnfoldr value n :: Stream Identity Int)

{- HLINT ignore "Use any"-}
{-# INLINE foldableOr #-}
foldableOr :: Int -> Int -> Bool
foldableOr value n =
    Prelude.or $ fmap
        (> (value + 1)) (sourceUnfoldr value n :: Stream Identity Int)

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
     -- , bench "null" $ nf (_foldableNull value) 1
        , bench "elem" $ nf (foldableElem value) 1
        , bench "length" $ nf (foldableLength value) 1
        , bench "sum" $ nf (foldableSum value) 1
        , bench "product" $ nf (foldableProduct value) 1
        , bench "minimum" $ nf (foldableMin value) 1
        , benchPureSink value "min (ord)" ordInstanceMin
        , bench "maximum" $ nf (foldableMax value) 1
        , bench "minimumBy" $ nf (`foldableMinBy` 1) value
        , bench "maximumBy" $ nf (`foldableMaxBy` 1) value
        , bench "minimumByList" $ nf (`foldableListMinBy` 1) value
        , bench "length . toList"
            $ nf (Prelude.length . foldableToList value) 1
        , bench "notElem" $ nf (foldableNotElem value) 1
        , bench "find" $ nf (foldableFind value) 1
        , bench "all" $ nf (foldableAll value) 1
        , bench "any" $ nf (foldableAny value) 1
        , bench "and" $ nf (foldableAnd value) 1
        , bench "or" $ nf (foldableOr value) 1

        -- Applicative and Traversable operations
        -- TBD: traverse_
        , benchIOSink1 "mapM_" (foldableMapM_ value)
        -- TBD: for_
        -- TBD: forM_
        , benchIOSink1 "sequence_" (foldableSequence_ value)
        -- TBD: sequenceA_
        -- TBD: asum
        -- XXX needs to be fixed, results are in ns
        -- , benchIOSink1 "msum" (foldableMsum value)
        ]
    ]

-------------------------------------------------------------------------------
-- Stream folds
-------------------------------------------------------------------------------

#ifndef USE_PRELUDE
instance NFData a => NFData (Stream Identity a) where
    {-# INLINE rnf #-}
    rnf xs = runIdentity $ S.fold (Fold.foldl' (\_ x -> rnf x) ()) xs
#endif

{-# INLINE benchPureSink #-}
benchPureSink :: NFData b
    => Int -> String -> (Stream Identity Int -> b) -> Benchmark
benchPureSink value name = benchPure name (sourceUnfoldr value)

{-# INLINE benchHoistSink #-}
benchHoistSink
    :: (NFData b)
    => Int -> String -> (Stream Identity Int -> IO b) -> Benchmark
benchHoistSink value name f =
    bench name $ nfIO $ randomRIO (1,1) >>= f .  sourceUnfoldr value

-- XXX We should be using sourceUnfoldrM for fair comparison with IO monad, but
-- we can't use it as it requires MonadAsync constraint.
{-# INLINE benchIdentitySink #-}
benchIdentitySink
    :: (NFData b)
    => Int -> String -> (Stream Identity Int -> Identity b) -> Benchmark
benchIdentitySink value name f = bench name $ nf (f . sourceUnfoldr value) 1

-------------------------------------------------------------------------------
-- Reductions
-------------------------------------------------------------------------------

{-# INLINE uncons #-}
#ifdef USE_STREAMK
uncons :: Monad m => StreamK m Int -> m ()
#else
uncons :: Monad m => Stream m Int -> m ()
#endif
uncons s = do
    r <- StreamK.uncons s
    case r of
        Nothing -> return ()
        Just (_, t) -> uncons t

#ifdef USE_PRELUDE
{-# INLINE init #-}
init :: Monad m => Stream m a -> m ()
init s = S.init s >>= Prelude.mapM_ S.drain

{-# INLINE mapM_ #-}
mapM_ :: Monad m => Stream m Int -> m ()
mapM_ = S.mapM_ (\_ -> return ())
#endif

{-# INLINE foldrMElem #-}
foldrMElem :: Monad m => Int -> Stream m Int -> m Bool
foldrMElem e =
    S.foldrM
        (\x xs ->
             if x == e
                 then return True
                 else xs)
        (return False)

#ifdef USE_STREAMK
{-# INLINE foldrToStream #-}
foldrToStream :: Monad m => Stream m Int -> m (StreamK Identity Int)
foldrToStream = S.foldr StreamK.cons StreamK.nil
#else
-- {-# INLINE foldrToStream #-}
-- foldrToStream :: Monad m => Stream m Int -> m (Stream Identity Int)
-- foldrToStream = S.foldr S.cons S.nil
#endif

{-# INLINE foldrMBuild #-}
foldrMBuild :: Monad m => Stream m Int -> m [Int]
foldrMBuild = S.foldrM (\x xs -> (x :) <$> xs) (return [])

#ifdef USE_PRELUDE
{-# INLINE foldl'Reduce #-}
foldl'Reduce :: Monad m => Stream m Int -> m Int
foldl'Reduce = S.foldl' (+) 0

{-# INLINE foldl1'Reduce #-}
foldl1'Reduce :: Monad m => Stream m Int -> m (Maybe Int)
foldl1'Reduce = S.foldl1' (+)

{-# INLINE foldlM'Reduce #-}
foldlM'Reduce :: Monad m => Stream m Int -> m Int
foldlM'Reduce = S.foldlM' (\xs a -> return $ a + xs) (return 0)

{-# INLINE last #-}
last :: Monad m => Stream m Int -> m (Maybe Int)
last = S.last

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

{-# INLINE findM #-}
findM :: Monad m => Int -> Stream m Int -> m (Maybe Int)
findM value = S.findM (\z -> return $ z == (value + 1))

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

{-# INLINE the #-}
the :: Monad m => Stream m Int -> m (Maybe Int)
the = S.the

{-# INLINE drainN #-}
drainN :: Monad m => Int -> Stream m Int -> m ()
drainN = S.drainN

{-# INLINE drainWhile #-}
drainWhile :: Monad m => Stream m Int -> m ()
drainWhile = S.drainWhile (const True)

{-# INLINE (!!) #-}
(!!) :: Monad m => Int -> Stream m Int -> m (Maybe Int)
(!!) = flip (S.!!)

{-# INLINE lookup #-}
lookup :: Monad m => Int -> Stream m Int -> m (Maybe Int)
lookup val = S.lookup val . S.map (\x -> (x, x))
#endif

o_1_space_elimination_folds :: Int -> [Benchmark]
o_1_space_elimination_folds value =
    [ bgroup "elimination"
        -- Basic folds
        [
#ifdef USE_PRELUDE
            bgroup "reduce"
            [ bgroup
                  "IO"
                  [ benchIOSink value "foldl'" foldl'Reduce
                  , benchIOSink value "foldl1'" foldl1'Reduce
                  , benchIOSink value "foldlM'" foldlM'Reduce
                  ]

            , bgroup
                  "Identity"
                  [ benchIdentitySink value "foldl'" foldl'Reduce
                  , benchIdentitySink value "foldl1'" foldl1'Reduce
                  , benchIdentitySink value "foldlM'" foldlM'Reduce
                  ]
            ] ,
#endif
         bgroup "build"
            [ bgroup "IO"
                  [ benchIOSink value "foldrMElem" (foldrMElem value)
                  ]
            , bgroup "Identity"
                  [ benchIdentitySink value "foldrMElem" (foldrMElem value)
#ifdef USE_STREAMK
                  , benchIdentitySink value "foldrToStreamLength"
                        (S.fold Fold.length . toStream . runIdentity . foldrToStream)
                  {-
                  , benchIdentitySink 16 "foldrToStreamLength (16)"
                        (S.fold Fold.length . toStream . runIdentity . foldrToStream)
                  -}
#else
                  {-
                  , benchIdentitySink 16 "foldrToStreamLength (16)"
                        (S.fold Fold.length . runIdentity . foldrToStream)
                  -}
#endif
                  {-
                  , benchPureSink 16 "foldrMToListLength (16)"
                        (Prelude.length . runIdentity . foldrMBuild)
                  -}
                  , benchPureSink value "foldrMToListLength"
                        (Prelude.length . runIdentity . foldrMBuild)
                  ]
            ]

        -- deconstruction
        , benchIOSink value "uncons" (uncons . fromStream)
#ifndef USE_PRELUDE
        , benchHoistSink value "length . generalizeInner"
              (S.fold Fold.length . S.generalizeInner)
#endif
#ifdef USE_PRELUDE
        , benchIOSink value "init" init

        -- draining
        , benchIOSink value "drain" $ toNull fromSerial
        , benchIOSink value "drainN" $ drainN value
        , benchIOSink value "drainWhile" drainWhile
        , benchPureSink value "drain (pure)" id
        , benchIOSink value "mapM_" mapM_

        -- this is too fast, causes all benchmarks reported in ns
    -- , benchIOSink value "head" head
        , benchIOSink value "last" last
        , benchIOSink value "length" length
        , benchIOSink value "sum" sum
        , benchIOSink value "product" product
        , benchIOSink value "maximumBy" maximumBy
        , benchIOSink value "maximum" maximum
        , benchIOSink value "minimumBy" minimumBy
        , benchIOSink value "minimum" minimum

        , bench "the" $ nfIO $ randomRIO (1,1) >>= the . repeat value
        , benchIOSink value "find" (find value)
        , benchIOSink value "findM" (findM value)
        -- , benchIOSink value "lookupFirst" (lookup 1)
        , benchIOSink value "lookupNever" (lookup (value + 1))
        , benchIOSink value "(!!)" (value !!)
        , benchIOSink value "findIndex" (findIndex value)
        , benchIOSink value "elemIndex" (elemIndex value)
        -- this is too fast, causes all benchmarks reported in ns
    -- , benchIOSink value "null" S.null
        , benchIOSink value "elem" (elem value)
        , benchIOSink value "notElem" (notElem value)
        , benchIOSink value "all" (all value)
        , benchIOSink value "any" (any value)
        , benchIOSink value "and" (and value)
        , benchIOSink value "or" (or value)
#endif

        -- length is used to check for foldr/build fusion
        , benchPureSink value "length . IsList.toList" (Prelude.length . GHC.toList)
        ]
    ]

-------------------------------------------------------------------------------
-- Buffered Transformations by fold
-------------------------------------------------------------------------------

#ifdef USE_PRELUDE
{-# INLINE foldl'Build #-}
foldl'Build :: Monad m => Stream m Int -> m [Int]
foldl'Build = S.foldl' (flip (:)) []

{-# INLINE foldlM'Build #-}
foldlM'Build :: Monad m => Stream m Int -> m [Int]
foldlM'Build = S.foldlM' (\xs x -> return $ x : xs) (return [])

o_n_heap_elimination_foldl :: Int -> [Benchmark]
o_n_heap_elimination_foldl value =
    [ bgroup "foldl"
        -- Left folds for building a structure are inherently non-streaming
        -- as the structure cannot be lazily consumed until fully built.
        [ benchIOSink value "foldl'/build/IO" foldl'Build
        , benchIdentitySink value "foldl'/build/Identity" foldl'Build
        , benchIOSink value "foldlM'/build/IO" foldlM'Build
        , benchIdentitySink value "foldlM'/build/Identity" foldlM'Build
        ]
    ]
#endif

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
foldrMReduce = S.foldrM (\x xs -> (x +) <$> xs) (return 0)

o_n_space_elimination_foldr :: Int -> [Benchmark]
o_n_space_elimination_foldr value =
    -- Head recursive strict right folds.
    [ bgroup "foldr"
        -- accumulation due to strictness of IO monad
        [ benchIOSink value "foldrM/build/IO (toList)" foldrMBuild
        -- Right folds for reducing are inherently non-streaming as the
        -- expression needs to be fully built before it can be reduced.
        , benchIdentitySink value "foldrM/reduce/Identity (sum)" foldrMReduce
        , benchIOSink value "foldrM/reduce/IO (sum)" foldrMReduce
        ]
    ]

#ifdef USE_PRELUDE
o_n_heap_elimination_toList :: Int -> [Benchmark]
o_n_heap_elimination_toList value =
    [ bgroup "toList"
        -- Converting the stream to a list or pure stream in a strict monad
        [ benchIOSink value "toListRev" S.toListRev
        , benchIOSink value "toStreamRev"
            (S.toStreamRev :: (Stream IO Int -> IO (Stream Identity Int)))
        ]
    ]

o_n_space_elimination_toList :: Int -> [Benchmark]
o_n_space_elimination_toList value =
    [ bgroup "toList"
        -- Converting the stream to a list or pure stream in a strict monad
        [ benchIOSink value "toList" S.toList
#ifndef USE_PRELUDE
        , benchIOSink value "toStream"
            (S.toStream :: (Stream IO Int -> IO (Stream Identity Int)))
#endif
        ]
    ]
#endif

-------------------------------------------------------------------------------
-- Multi-stream folds
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- Multi-stream pure
-------------------------------------------------------------------------------

{-# INLINE eqBy' #-}
eqBy' :: (Monad m, Eq a) => Stream m a -> m Bool
eqBy' src = S.eqBy (==) src src

{-# INLINE eqByPure #-}
eqByPure :: Int -> Int -> Identity Bool
eqByPure value n = eqBy' (sourceUnfoldr value n)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'eqByPure
inspect $ 'eqByPure `hasNoType` ''SPEC
inspect $ 'eqByPure `hasNoType` ''D.Step
#endif

{-# INLINE eqInstance #-}
eqInstance :: Stream Identity Int -> Bool
eqInstance src = src == src

{-# INLINE eqInstanceNotEq #-}
eqInstanceNotEq :: Stream Identity Int -> Bool
eqInstanceNotEq src = src /= src

{-# INLINE cmpBy' #-}
cmpBy' :: (Monad m, Ord a) => Stream m a -> m Ordering
cmpBy' src = S.cmpBy compare src src

{-# INLINE cmpByPure #-}
cmpByPure :: Int -> Int -> Identity Ordering
cmpByPure value n = cmpBy' (sourceUnfoldr value n)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'cmpByPure
inspect $ 'cmpByPure `hasNoType` ''SPEC
inspect $ 'cmpByPure `hasNoType` ''D.Step
#endif

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
isPrefixOf src = S.isPrefixOf src src

{-# INLINE isSubsequenceOf #-}
isSubsequenceOf :: Monad m => Stream m Int -> m Bool
isSubsequenceOf src = S.isSubsequenceOf src src

{-# INLINE stripPrefix #-}
stripPrefix :: Monad m => Stream m Int -> m ()
stripPrefix src = do
    _ <- S.stripPrefix src src
    return ()

{-# INLINE eqBy #-}
eqBy :: Int -> Int -> IO Bool
eqBy value n = eqBy' (sourceUnfoldrM value n)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'eqBy
inspect $ 'eqBy `hasNoType` ''SPEC
inspect $ 'eqBy `hasNoType` ''D.Step
#endif

{-# INLINE cmpBy #-}
cmpBy :: Int -> Int -> IO Ordering
cmpBy value n = cmpBy' (sourceUnfoldrM value n)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'cmpBy
inspect $ 'cmpBy `hasNoType` ''SPEC
inspect $ 'cmpBy `hasNoType` ''D.Step
#endif

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

        , bgroup (o_n_heap_prefix moduleName) $
               o_n_heap_elimination_buffered size
#ifdef USE_PRELUDE
            ++ o_n_heap_elimination_foldl size
            ++ o_n_heap_elimination_toList size
            ++ o_n_space_elimination_toList size
#endif
        , bgroup (o_n_space_prefix moduleName) $
            o_n_space_elimination_foldr size
        ]
