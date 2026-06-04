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

import Control.Monad (when)
import Control.DeepSeq (NFData(..))
import Data.Functor.Identity (Identity, runIdentity)
import System.Random (randomRIO)

import qualified Data.Foldable as F
import qualified GHC.Exts as GHC
import qualified Streamly.Internal.Data.Fold as Fold

#ifdef INSPECTION
import GHC.Types (SPEC(..))
import Test.Inspection
#endif

import Stream.Common
import Streamly.Internal.Data.Stream (Stream)
import qualified Streamly.Internal.Data.Stream as S

import Test.Tasty.Bench
import Streamly.Benchmark.Common
import Prelude hiding (length, sum, or, and, any, all, notElem, elem, (!!),
    lookup, repeat, minimum, maximum, product, last, mapM_, init)
import qualified Prelude

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

instance NFData a => NFData (Stream Identity a) where
    {-# INLINE rnf #-}
    rnf xs = runIdentity $ S.fold (Fold.foldl' (\_ x -> rnf x) ()) xs

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
uncons :: Monad m => Stream m Int -> m ()
uncons s = do
    r <- S.uncons s
    case r of
        Nothing -> return ()
        Just (_, t) -> uncons t

{-# INLINE toNull #-}
toNull :: Monad m => Stream m Int -> m ()
toNull = S.drain

{-# INLINE init #-}
init :: Monad m => Stream m a -> m ()
init s = S.init s >>= Prelude.mapM_ S.drain

{-# INLINE mapM_ #-}
mapM_ :: Monad m => Stream m Int -> m ()
mapM_ = S.mapM_ (\_ -> return ())

{-# INLINE foldBreak #-}
foldBreak :: Monad m => Stream m Int -> m ()
foldBreak s = do
    (r, s1) <- S.foldBreak (Fold.take 1 Fold.length) s
    when (r /= 0) $ foldBreak s1

{-# INLINE foldrMElem #-}
foldrMElem :: Monad m => Int -> Stream m Int -> m Bool
foldrMElem e =
    S.foldrM
        (\x xs ->
             if x == e
                 then return True
                 else xs)
        (return False)

-- {-# INLINE foldrToStream #-}
-- foldrToStream :: Monad m => Stream m Int -> m (Stream Identity Int)
-- foldrToStream = S.foldr S.cons S.nil

{-# INLINE foldrMBuild #-}
foldrMBuild :: Monad m => Stream m Int -> m [Int]
foldrMBuild = S.foldrM (\x xs -> (x :) <$> xs) (return [])

{-# INLINE foldl'Reduce #-}
foldl'Reduce :: Monad m => Stream m Int -> m Int
foldl'Reduce = S.foldl' (+) 0

{-# INLINE last #-}
last :: Monad m => Stream m Int -> m (Maybe Int)
last = S.last

{-# INLINE foldl1'Reduce #-}
foldl1'Reduce :: Monad m => Stream m Int -> m (Maybe Int)
foldl1'Reduce = S.fold (Fold.foldl1' (+))

{-# INLINE foldlM'Reduce #-}
foldlM'Reduce :: Monad m => Stream m Int -> m Int
foldlM'Reduce = S.foldlM' (\xs a -> return $ a + xs) (return 0)

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
length = S.fold Fold.length

{-# INLINE all #-}
all :: Monad m => Int -> Stream m Int -> m Bool
all value = S.all (<= (value + 1))

{-# INLINE any #-}
any :: Monad m => Int -> Stream m Int -> m Bool
any value = S.any (> (value + 1))

{-# INLINE and #-}
and :: Monad m => Int -> Stream m Int -> m Bool
and value = S.fold Fold.and . S.map (<= (value + 1))

{-# INLINE or #-}
or :: Monad m => Int -> Stream m Int -> m Bool
or value = S.fold Fold.or . S.map (> (value + 1))

{-# INLINE find #-}
find :: Monad m => Int -> Stream m Int -> m (Maybe Int)
find value = S.find (== (value + 1))

{-# INLINE findM #-}
findM :: Monad m => Int -> Stream m Int -> m (Maybe Int)
findM value = S.findM (\z -> return $ z == (value + 1))

{-# INLINE findIndex #-}
findIndex :: Monad m => Int -> Stream m Int -> m (Maybe Int)
findIndex value = S.head . S.findIndices (== (value + 1))

{-# INLINE elemIndex #-}
elemIndex :: Monad m => Int -> Stream m Int -> m (Maybe Int)
elemIndex value = S.head . S.elemIndices (value + 1)

{-# INLINE maximum #-}
maximum :: Monad m => Stream m Int -> m (Maybe Int)
maximum = S.maximum

{-# INLINE minimum #-}
minimum :: Monad m => Stream m Int -> m (Maybe Int)
minimum = S.minimum

{-# INLINE sum #-}
sum :: Monad m => Stream m Int -> m Int
sum = S.fold Fold.sum

{-# INLINE product #-}
product :: Monad m => Stream m Int -> m Int
product = S.fold Fold.product

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
drainN n = S.fold (Fold.drainN n)

{-# INLINE (!!) #-}
(!!) :: Monad m => Int -> Stream m Int -> m (Maybe Int)
(!!) = flip (S.!!)

{-# INLINE lookup #-}
lookup :: Monad m => Int -> Stream m Int -> m (Maybe Int)
lookup val = S.lookup val . S.map (\x -> (x, x))
o_1_space_elimination_folds :: Int -> [Benchmark]
o_1_space_elimination_folds value =
    [ bgroup "elimination"
        -- Basic folds
        [
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
         bgroup "build"
            [ bgroup "IO"
                  [ benchIOSink value "foldrMElem" (foldrMElem value)
                  ]
            , bgroup "Identity"
                  [ benchIdentitySink value "foldrMElem" (foldrMElem value)
                  {-
                  , benchIdentitySink 16 "foldrToStreamLength (16)"
                        (S.fold Fold.length . runIdentity . foldrToStream)
                  -}
                  {-
                  , benchPureSink 16 "foldrMToListLength (16)"
                        (Prelude.length . runIdentity . foldrMBuild)
                  -}
                  , benchPureSink value "foldrMToListLength"
                        (Prelude.length . runIdentity . foldrMBuild)
                  ]
            ]

        -- deconstruction
        , benchIOSink value "uncons" uncons
        , benchIOSink value "mapM_" mapM_
        , benchIOSink value "last" last
        , benchHoistSink value "length . generalizeInner"
              (S.fold Fold.length . S.generalizeInner)
        , benchIOSink value "toNull" toNull
        , benchIOSink value "foldBreak" foldBreak
        , benchIOSink value "init" init

        -- draining
        , benchIOSink value "drainN" $ drainN value
        , benchPureSink value "drain (pure)" id

        -- this is too fast, causes all benchmarks reported in ns
    -- , benchIOSink value "head" head
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

        -- length is used to check for foldr/build fusion
        , benchPureSink value "length . IsList.toList" (Prelude.length . GHC.toList)
        ]
    ]

-------------------------------------------------------------------------------
-- Buffered Transformations by fold
-------------------------------------------------------------------------------

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

        -- This is horribly slow, never finishes
        -- let foldlS = composeN n $ S.foldlS (flip S.cons) S.nil
        --  in benchFold "foldlS"  (foldlS    1) sourceUnfoldrM
        ]
    ]

o_n_heap_elimination_toList :: Int -> [Benchmark]
o_n_heap_elimination_toList value =
    [ bgroup "toList"
        -- Converting the stream to a list or pure stream in a strict monad
        [ benchIOSink value "toListRev" S.toListRev
        , benchIOSink value "toStreamRev"
            (S.fold Fold.toStreamRev :: (Stream IO Int -> IO (Stream Identity Int)))
        ]
    ]

o_n_space_elimination_toList :: Int -> [Benchmark]
o_n_space_elimination_toList value =
    [ bgroup "toList"
        -- Converting the stream to a list or pure stream in a strict monad
        [ benchIOSink value "toList" S.toList
        , benchIOSink value "toStream"
            (S.fold Fold.toStream :: (Stream IO Int -> IO (Stream Identity Int)))
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
eqBy' src = S.eqBy (==) src src

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
cmpBy' src = S.cmpBy compare src src

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
-- Inspection
-------------------------------------------------------------------------------

#ifdef INSPECTION
-- Most elimination benchmarks consume a stream down to a scalar, so the whole
-- generate+fold pipeline must fully fuse: no 'Step' constructors should remain
-- in the optimized core. We bake in a concrete source (the same one the
-- benchmark supplies) and assert 'Step'-freedom. Folds that build a structure
-- (toList/toStream, foldrM reduce, list/show buffering) are inherently
-- non-streaming and keep their 'Step'/list constructors; those are not tested
-- (see the reminders at the end).

-- Stream folds: IO wrappers that bake a monadic 'sourceUnfoldrM' source into
-- the polymorphic folds.

{-# INLINE inspToNull #-}
inspToNull :: Int -> Int -> IO ()
inspToNull value n = toNull (sourceUnfoldrM value n)

{-# INLINE inspUncons #-}
inspUncons :: Int -> Int -> IO ()
inspUncons value n = uncons (sourceUnfoldrM value n)

{-# INLINE inspInit #-}
inspInit :: Int -> Int -> IO ()
inspInit value n = init (sourceUnfoldrM value n)

{-# INLINE inspMapM_ #-}
inspMapM_ :: Int -> Int -> IO ()
inspMapM_ value n = mapM_ (sourceUnfoldrM value n)

{-# INLINE inspFoldBreak #-}
inspFoldBreak :: Int -> Int -> IO ()
inspFoldBreak value n = foldBreak (sourceUnfoldrM value n)

{-# INLINE inspFoldrMElem #-}
inspFoldrMElem :: Int -> Int -> IO Bool
inspFoldrMElem value n = foldrMElem value (sourceUnfoldrM value n)

{-# INLINE inspFoldl' #-}
inspFoldl' :: Int -> Int -> IO Int
inspFoldl' value n = foldl'Reduce (sourceUnfoldrM value n)

{-# INLINE inspFoldl1' #-}
inspFoldl1' :: Int -> Int -> IO (Maybe Int)
inspFoldl1' value n = foldl1'Reduce (sourceUnfoldrM value n)

{-# INLINE inspFoldlM' #-}
inspFoldlM' :: Int -> Int -> IO Int
inspFoldlM' value n = foldlM'Reduce (sourceUnfoldrM value n)

{-# INLINE inspLast #-}
inspLast :: Int -> Int -> IO (Maybe Int)
inspLast value n = last (sourceUnfoldrM value n)

{-# INLINE inspLength #-}
inspLength :: Int -> Int -> IO Int
inspLength value n = length (sourceUnfoldrM value n)

{-# INLINE inspSum #-}
inspSum :: Int -> Int -> IO Int
inspSum value n = sum (sourceUnfoldrM value n)

{-# INLINE inspProduct #-}
inspProduct :: Int -> Int -> IO Int
inspProduct value n = product (sourceUnfoldrM value n)

{-# INLINE inspMaximum #-}
inspMaximum :: Int -> Int -> IO (Maybe Int)
inspMaximum value n = maximum (sourceUnfoldrM value n)

{-# INLINE inspMinimum #-}
inspMinimum :: Int -> Int -> IO (Maybe Int)
inspMinimum value n = minimum (sourceUnfoldrM value n)

{-# INLINE inspMaximumBy #-}
inspMaximumBy :: Int -> Int -> IO (Maybe Int)
inspMaximumBy value n = maximumBy (sourceUnfoldrM value n)

{-# INLINE inspMinimumBy #-}
inspMinimumBy :: Int -> Int -> IO (Maybe Int)
inspMinimumBy value n = minimumBy (sourceUnfoldrM value n)

{-# INLINE inspThe #-}
inspThe :: Int -> Int -> IO (Maybe Int)
inspThe value n = the (sourceUnfoldrM value n)

{-# INLINE inspDrainN #-}
inspDrainN :: Int -> Int -> IO ()
inspDrainN value n = drainN value (sourceUnfoldrM value n)

{-# INLINE inspElem #-}
inspElem :: Int -> Int -> IO Bool
inspElem value n = elem value (sourceUnfoldrM value n)

{-# INLINE inspNotElem #-}
inspNotElem :: Int -> Int -> IO Bool
inspNotElem value n = notElem value (sourceUnfoldrM value n)

{-# INLINE inspAll #-}
inspAll :: Int -> Int -> IO Bool
inspAll value n = all value (sourceUnfoldrM value n)

{-# INLINE inspAny #-}
inspAny :: Int -> Int -> IO Bool
inspAny value n = any value (sourceUnfoldrM value n)

{-# INLINE inspAnd #-}
inspAnd :: Int -> Int -> IO Bool
inspAnd value n = and value (sourceUnfoldrM value n)

{-# INLINE inspOr #-}
inspOr :: Int -> Int -> IO Bool
inspOr value n = or value (sourceUnfoldrM value n)

{-# INLINE inspFind #-}
inspFind :: Int -> Int -> IO (Maybe Int)
inspFind value n = find value (sourceUnfoldrM value n)

{-# INLINE inspFindM #-}
inspFindM :: Int -> Int -> IO (Maybe Int)
inspFindM value n = findM value (sourceUnfoldrM value n)

{-# INLINE inspFindIndex #-}
inspFindIndex :: Int -> Int -> IO (Maybe Int)
inspFindIndex value n = findIndex value (sourceUnfoldrM value n)

{-# INLINE inspElemIndex #-}
inspElemIndex :: Int -> Int -> IO (Maybe Int)
inspElemIndex value n = elemIndex value (sourceUnfoldrM value n)

{-# INLINE inspIndexOp #-}
inspIndexOp :: Int -> Int -> IO (Maybe Int)
inspIndexOp value n = value !! sourceUnfoldrM value n

{-# INLINE inspLookup #-}
inspLookup :: Int -> Int -> IO (Maybe Int)
inspLookup value n = lookup (value + 1) (sourceUnfoldrM value n)

-- Multi-stream folds.
{-# INLINE inspIsPrefixOf #-}
inspIsPrefixOf :: Int -> Int -> IO Bool
inspIsPrefixOf value n = isPrefixOf (sourceUnfoldrM value n)

{-# INLINE inspIsSubsequenceOf #-}
inspIsSubsequenceOf :: Int -> Int -> IO Bool
inspIsSubsequenceOf value n = isSubsequenceOf (sourceUnfoldrM value n)

{-# INLINE inspStripPrefix #-}
inspStripPrefix :: Int -> Int -> IO ()
inspStripPrefix value n = stripPrefix (sourceUnfoldrM value n)

-- 'foldableMapM_' is the only 'Foldable' benchmark that is polymorphic in the
-- monad; the rest already bake a concrete 'Stream Identity' source and are
-- inspected directly.
{-# INLINE inspFoldableMapM_ #-}
inspFoldableMapM_ :: Int -> Int -> IO ()
inspFoldableMapM_ = foldableMapM_

-- Type-class instance comparisons over a pure 'Stream Identity' source.
{-# INLINE inspEqInstance #-}
inspEqInstance :: Int -> Int -> Bool
inspEqInstance value n = eqInstance (sourceUnfoldr value n)

{-# INLINE inspEqInstanceNotEq #-}
inspEqInstanceNotEq :: Int -> Int -> Bool
inspEqInstanceNotEq value n = eqInstanceNotEq (sourceUnfoldr value n)

{-# INLINE inspOrdInstance #-}
inspOrdInstance :: Int -> Int -> Bool
inspOrdInstance value n = ordInstance (sourceUnfoldr value n)

-- 'min' (the 'Ord' instance) returns a 'Stream', so we drain it purely.
{-# INLINE inspOrdInstanceMin #-}
inspOrdInstanceMin :: Int -> Int -> ()
inspOrdInstanceMin value n =
    runIdentity $ drain $ ordInstanceMin (sourceUnfoldr value n)

-- 'generalizeInner' lifts a pure 'Stream Identity' source into 'IO'.
{-# INLINE inspGeneralizeInner #-}
inspGeneralizeInner :: Int -> Int -> IO Int
inspGeneralizeInner value n =
    (S.fold Fold.length . S.generalizeInner) (sourceUnfoldr value n)

-- 'Identity'-monad reduce/build variants over the pure source (the IO variants
-- with 'sourceUnfoldrM' are tested above).
{-# INLINE inspFoldl'Identity #-}
inspFoldl'Identity :: Int -> Int -> Identity Int
inspFoldl'Identity value n = foldl'Reduce (sourceUnfoldr value n)

{-# INLINE inspFoldl1'Identity #-}
inspFoldl1'Identity :: Int -> Int -> Identity (Maybe Int)
inspFoldl1'Identity value n = foldl1'Reduce (sourceUnfoldr value n)

{-# INLINE inspFoldlM'Identity #-}
inspFoldlM'Identity :: Int -> Int -> Identity Int
inspFoldlM'Identity value n = foldlM'Reduce (sourceUnfoldr value n)

{-# INLINE inspFoldrMElemIdentity #-}
inspFoldrMElemIdentity :: Int -> Int -> Identity Bool
inspFoldrMElemIdentity value n = foldrMElem value (sourceUnfoldr value n)

-- Stream folds
inspect $ hasNoTypeClasses 'inspToNull
inspect $ 'inspToNull `hasNoType` ''S.Step
inspect $ 'inspToNull `hasNoType` ''Fold.Step
inspect $ 'inspToNull `hasNoType` ''SPEC
-- 'uncons' and 'foldBreak' recurse explicitly, reconstructing (and re-matching)
-- the stream on each step, so a 'Step' constructor survives.
inspect $ hasNoTypeClasses 'inspUncons
-- inspect $ 'inspUncons `hasNoType` ''S.Step
inspect $ 'inspUncons `hasNoType` ''Fold.Step
inspect $ 'inspUncons `hasNoType` ''SPEC
inspect $ hasNoTypeClasses 'inspInit
inspect $ 'inspInit `hasNoType` ''S.Step
inspect $ 'inspInit `hasNoType` ''Fold.Step
inspect $ 'inspInit `hasNoType` ''SPEC
inspect $ hasNoTypeClasses 'inspMapM_
inspect $ 'inspMapM_ `hasNoType` ''S.Step
inspect $ 'inspMapM_ `hasNoType` ''Fold.Step
inspect $ 'inspMapM_ `hasNoType` ''SPEC
inspect $ hasNoTypeClasses 'inspFoldBreak
-- inspect $ 'inspFoldBreak `hasNoType` ''S.Step
inspect $ 'inspFoldBreak `hasNoType` ''Fold.Step
inspect $ 'inspFoldBreak `hasNoType` ''SPEC
inspect $ hasNoTypeClasses 'inspFoldrMElem
inspect $ 'inspFoldrMElem `hasNoType` ''S.Step
inspect $ 'inspFoldrMElem `hasNoType` ''Fold.Step
inspect $ 'inspFoldrMElem `hasNoType` ''SPEC
inspect $ hasNoTypeClasses 'inspFoldl'
inspect $ 'inspFoldl' `hasNoType` ''S.Step
inspect $ hasNoTypeClasses 'inspFoldl1'
inspect $ 'inspFoldl1' `hasNoType` ''S.Step
inspect $ hasNoTypeClasses 'inspFoldlM'
inspect $ 'inspFoldlM' `hasNoType` ''S.Step
inspect $ hasNoTypeClasses 'inspLast
inspect $ 'inspLast `hasNoType` ''S.Step
inspect $ 'inspLast `hasNoType` ''Fold.Step
inspect $ 'inspLast `hasNoType` ''SPEC
inspect $ hasNoTypeClasses 'inspLength
inspect $ 'inspLength `hasNoType` ''S.Step
inspect $ 'inspLength `hasNoType` ''Fold.Step
inspect $ 'inspLength `hasNoType` ''SPEC
inspect $ hasNoTypeClasses 'inspSum
inspect $ 'inspSum `hasNoType` ''S.Step
inspect $ 'inspSum `hasNoType` ''Fold.Step
inspect $ 'inspSum `hasNoType` ''SPEC
inspect $ hasNoTypeClasses 'inspProduct
inspect $ 'inspProduct `hasNoType` ''S.Step
inspect $ 'inspProduct `hasNoType` ''Fold.Step
inspect $ 'inspProduct `hasNoType` ''SPEC
inspect $ hasNoTypeClasses 'inspMaximum
inspect $ 'inspMaximum `hasNoType` ''S.Step
inspect $ 'inspMaximum `hasNoType` ''Fold.Step
inspect $ 'inspMaximum `hasNoType` ''SPEC
inspect $ hasNoTypeClasses 'inspMinimum
inspect $ 'inspMinimum `hasNoType` ''S.Step
inspect $ 'inspMinimum `hasNoType` ''Fold.Step
inspect $ 'inspMinimum `hasNoType` ''SPEC
inspect $ hasNoTypeClasses 'inspMaximumBy
inspect $ 'inspMaximumBy `hasNoType` ''S.Step
inspect $ 'inspMaximumBy `hasNoType` ''Fold.Step
inspect $ 'inspMaximumBy `hasNoType` ''SPEC
inspect $ hasNoTypeClasses 'inspMinimumBy
inspect $ 'inspMinimumBy `hasNoType` ''S.Step
inspect $ 'inspMinimumBy `hasNoType` ''Fold.Step
inspect $ 'inspMinimumBy `hasNoType` ''SPEC
inspect $ hasNoTypeClasses 'inspThe
inspect $ 'inspThe `hasNoType` ''S.Step
inspect $ 'inspThe `hasNoType` ''Fold.Step
inspect $ 'inspThe `hasNoType` ''SPEC
inspect $ hasNoTypeClasses 'inspDrainN
inspect $ 'inspDrainN `hasNoType` ''S.Step
inspect $ 'inspDrainN `hasNoType` ''Fold.Step
inspect $ 'inspDrainN `hasNoType` ''SPEC
inspect $ hasNoTypeClasses 'inspElem
inspect $ 'inspElem `hasNoType` ''S.Step
inspect $ 'inspElem `hasNoType` ''Fold.Step
inspect $ 'inspElem `hasNoType` ''SPEC
inspect $ hasNoTypeClasses 'inspNotElem
inspect $ 'inspNotElem `hasNoType` ''S.Step
inspect $ 'inspNotElem `hasNoType` ''Fold.Step
inspect $ 'inspNotElem `hasNoType` ''SPEC
inspect $ hasNoTypeClasses 'inspAll
inspect $ 'inspAll `hasNoType` ''S.Step
inspect $ 'inspAll `hasNoType` ''Fold.Step
inspect $ 'inspAll `hasNoType` ''SPEC
inspect $ hasNoTypeClasses 'inspAny
inspect $ 'inspAny `hasNoType` ''S.Step
inspect $ 'inspAny `hasNoType` ''Fold.Step
inspect $ 'inspAny `hasNoType` ''SPEC
inspect $ hasNoTypeClasses 'inspAnd
inspect $ 'inspAnd `hasNoType` ''S.Step
inspect $ 'inspAnd `hasNoType` ''Fold.Step
inspect $ 'inspAnd `hasNoType` ''SPEC
inspect $ hasNoTypeClasses 'inspOr
inspect $ 'inspOr `hasNoType` ''S.Step
inspect $ 'inspOr `hasNoType` ''Fold.Step
inspect $ 'inspOr `hasNoType` ''SPEC
inspect $ hasNoTypeClasses 'inspFind
inspect $ 'inspFind `hasNoType` ''S.Step
inspect $ 'inspFind `hasNoType` ''Fold.Step
inspect $ 'inspFind `hasNoType` ''SPEC
inspect $ hasNoTypeClasses 'inspFindM
inspect $ 'inspFindM `hasNoType` ''S.Step
inspect $ 'inspFindM `hasNoType` ''Fold.Step
inspect $ 'inspFindM `hasNoType` ''SPEC
inspect $ hasNoTypeClasses 'inspFindIndex
inspect $ 'inspFindIndex `hasNoType` ''S.Step
inspect $ 'inspFindIndex `hasNoType` ''Fold.Step
inspect $ 'inspFindIndex `hasNoType` ''SPEC
inspect $ hasNoTypeClasses 'inspElemIndex
inspect $ 'inspElemIndex `hasNoType` ''S.Step
inspect $ 'inspElemIndex `hasNoType` ''Fold.Step
inspect $ 'inspElemIndex `hasNoType` ''SPEC
inspect $ hasNoTypeClasses 'inspIndexOp
inspect $ 'inspIndexOp `hasNoType` ''S.Step
inspect $ 'inspIndexOp `hasNoType` ''Fold.Step
inspect $ 'inspIndexOp `hasNoType` ''SPEC
inspect $ hasNoTypeClasses 'inspLookup
inspect $ 'inspLookup `hasNoType` ''S.Step
inspect $ 'inspLookup `hasNoType` ''Fold.Step
inspect $ 'inspLookup `hasNoType` ''SPEC

-- Multi-stream pure
inspect $ hasNoTypeClasses 'eqByPure
inspect $ 'eqByPure `hasNoType` ''SPEC
inspect $ 'eqByPure `hasNoType` ''S.Step
inspect $ 'eqByPure `hasNoType` ''Fold.Step
inspect $ hasNoTypeClasses 'cmpByPure
inspect $ 'cmpByPure `hasNoType` ''SPEC
inspect $ 'cmpByPure `hasNoType` ''S.Step
inspect $ 'cmpByPure `hasNoType` ''Fold.Step

-- Multi-stream
inspect $ hasNoTypeClasses 'eqBy
inspect $ 'eqBy `hasNoType` ''SPEC
inspect $ 'eqBy `hasNoType` ''S.Step
inspect $ 'eqBy `hasNoType` ''Fold.Step
inspect $ hasNoTypeClasses 'cmpBy
inspect $ 'cmpBy `hasNoType` ''SPEC
inspect $ 'cmpBy `hasNoType` ''S.Step
inspect $ 'cmpBy `hasNoType` ''Fold.Step
inspect $ hasNoTypeClasses 'inspIsPrefixOf
inspect $ 'inspIsPrefixOf `hasNoType` ''S.Step
inspect $ 'inspIsPrefixOf `hasNoType` ''Fold.Step
inspect $ 'inspIsPrefixOf `hasNoType` ''SPEC
inspect $ hasNoTypeClasses 'inspIsSubsequenceOf
inspect $ 'inspIsSubsequenceOf `hasNoType` ''S.Step
inspect $ 'inspIsSubsequenceOf `hasNoType` ''Fold.Step
inspect $ 'inspIsSubsequenceOf `hasNoType` ''SPEC
inspect $ hasNoTypeClasses 'inspStripPrefix
inspect $ 'inspStripPrefix `hasNoType` ''S.Step
inspect $ 'inspStripPrefix `hasNoType` ''Fold.Step
inspect $ 'inspStripPrefix `hasNoType` ''SPEC

-- Foldable (pure 'Stream Identity' source baked in)
inspect $ hasNoTypeClasses 'foldableFoldl'
inspect $ 'foldableFoldl' `hasNoType` ''S.Step
inspect $ hasNoTypeClasses 'foldableFoldrElem
inspect $ 'foldableFoldrElem `hasNoType` ''S.Step
inspect $ 'foldableFoldrElem `hasNoType` ''Fold.Step
inspect $ 'foldableFoldrElem `hasNoType` ''SPEC
inspect $ hasNoTypeClasses 'foldableSum
inspect $ 'foldableSum `hasNoType` ''S.Step
inspect $ 'foldableSum `hasNoType` ''Fold.Step
inspect $ 'foldableSum `hasNoType` ''SPEC
inspect $ hasNoTypeClasses 'foldableProduct
inspect $ 'foldableProduct `hasNoType` ''S.Step
inspect $ 'foldableProduct `hasNoType` ''Fold.Step
inspect $ 'foldableProduct `hasNoType` ''SPEC
inspect $ hasNoTypeClasses 'foldableElem
inspect $ 'foldableElem `hasNoType` ''S.Step
inspect $ 'foldableElem `hasNoType` ''Fold.Step
inspect $ 'foldableElem `hasNoType` ''SPEC
inspect $ hasNoTypeClasses 'foldableNotElem
inspect $ 'foldableNotElem `hasNoType` ''S.Step
inspect $ 'foldableNotElem `hasNoType` ''Fold.Step
inspect $ 'foldableNotElem `hasNoType` ''SPEC
inspect $ hasNoTypeClasses 'foldableFind
inspect $ 'foldableFind `hasNoType` ''S.Step
inspect $ 'foldableFind `hasNoType` ''Fold.Step
inspect $ 'foldableFind `hasNoType` ''SPEC
inspect $ hasNoTypeClasses 'foldableAll
inspect $ 'foldableAll `hasNoType` ''S.Step
inspect $ 'foldableAll `hasNoType` ''Fold.Step
inspect $ 'foldableAll `hasNoType` ''SPEC
inspect $ hasNoTypeClasses 'foldableAny
inspect $ 'foldableAny `hasNoType` ''S.Step
inspect $ 'foldableAny `hasNoType` ''Fold.Step
inspect $ 'foldableAny `hasNoType` ''SPEC
inspect $ hasNoTypeClasses 'foldableAnd
inspect $ 'foldableAnd `hasNoType` ''S.Step
inspect $ 'foldableAnd `hasNoType` ''Fold.Step
inspect $ 'foldableAnd `hasNoType` ''SPEC
inspect $ hasNoTypeClasses 'foldableOr
inspect $ 'foldableOr `hasNoType` ''S.Step
inspect $ 'foldableOr `hasNoType` ''Fold.Step
inspect $ 'foldableOr `hasNoType` ''SPEC
inspect $ hasNoTypeClasses 'foldableLength
inspect $ 'foldableLength `hasNoType` ''S.Step
inspect $ 'foldableLength `hasNoType` ''Fold.Step
inspect $ 'foldableLength `hasNoType` ''SPEC
inspect $ hasNoTypeClasses 'foldableMin
inspect $ 'foldableMin `hasNoType` ''S.Step
inspect $ 'foldableMin `hasNoType` ''Fold.Step
inspect $ 'foldableMin `hasNoType` ''SPEC
inspect $ hasNoTypeClasses 'foldableMax
inspect $ 'foldableMax `hasNoType` ''S.Step
inspect $ 'foldableMax `hasNoType` ''Fold.Step
inspect $ 'foldableMax `hasNoType` ''SPEC
inspect $ hasNoTypeClasses 'foldableMinBy
inspect $ 'foldableMinBy `hasNoType` ''S.Step
inspect $ 'foldableMinBy `hasNoType` ''Fold.Step
inspect $ 'foldableMinBy `hasNoType` ''SPEC
inspect $ hasNoTypeClasses 'foldableMaxBy
inspect $ 'foldableMaxBy `hasNoType` ''S.Step
inspect $ 'foldableMaxBy `hasNoType` ''Fold.Step
inspect $ 'foldableMaxBy `hasNoType` ''SPEC
inspect $ hasNoTypeClasses 'foldableToList
inspect $ 'foldableToList `hasNoType` ''S.Step
inspect $ 'foldableToList `hasNoType` ''Fold.Step
inspect $ 'foldableToList `hasNoType` ''SPEC
inspect $ hasNoTypeClasses 'foldableSequence_
inspect $ 'foldableSequence_ `hasNoType` ''S.Step
inspect $ 'foldableSequence_ `hasNoType` ''Fold.Step
inspect $ 'foldableSequence_ `hasNoType` ''SPEC
inspect $ hasNoTypeClasses 'inspFoldableMapM_
inspect $ 'inspFoldableMapM_ `hasNoType` ''S.Step
inspect $ 'inspFoldableMapM_ `hasNoType` ''Fold.Step
inspect $ 'inspFoldableMapM_ `hasNoType` ''SPEC

-- Type-class instance comparisons (pure source)
inspect $ hasNoTypeClasses 'inspEqInstance
inspect $ 'inspEqInstance `hasNoType` ''S.Step
inspect $ 'inspEqInstance `hasNoType` ''Fold.Step
inspect $ 'inspEqInstance `hasNoType` ''SPEC
inspect $ hasNoTypeClasses 'inspEqInstanceNotEq
inspect $ 'inspEqInstanceNotEq `hasNoType` ''S.Step
inspect $ 'inspEqInstanceNotEq `hasNoType` ''Fold.Step
inspect $ 'inspEqInstanceNotEq `hasNoType` ''SPEC
inspect $ hasNoTypeClasses 'inspOrdInstance
inspect $ 'inspOrdInstance `hasNoType` ''S.Step
inspect $ 'inspOrdInstance `hasNoType` ''Fold.Step
inspect $ 'inspOrdInstance `hasNoType` ''SPEC
inspect $ hasNoTypeClasses 'inspOrdInstanceMin
inspect $ 'inspOrdInstanceMin `hasNoType` ''S.Step
inspect $ 'inspOrdInstanceMin `hasNoType` ''Fold.Step
inspect $ 'inspOrdInstanceMin `hasNoType` ''SPEC

-- generalizeInner
inspect $ hasNoTypeClasses 'inspGeneralizeInner
inspect $ 'inspGeneralizeInner `hasNoType` ''S.Step
inspect $ 'inspGeneralizeInner `hasNoType` ''Fold.Step
inspect $ 'inspGeneralizeInner `hasNoType` ''SPEC

-- Identity-monad reduce/build variants (pure source)
inspect $ hasNoTypeClasses 'inspFoldl'Identity
inspect $ 'inspFoldl'Identity `hasNoType` ''S.Step
inspect $ hasNoTypeClasses 'inspFoldl1'Identity
inspect $ 'inspFoldl1'Identity `hasNoType` ''S.Step
inspect $ hasNoTypeClasses 'inspFoldlM'Identity
inspect $ 'inspFoldlM'Identity `hasNoType` ''S.Step
inspect $ hasNoTypeClasses 'inspFoldrMElemIdentity
inspect $ 'inspFoldrMElemIdentity `hasNoType` ''S.Step
inspect $ 'inspFoldrMElemIdentity `hasNoType` ''Fold.Step
inspect $ 'inspFoldrMElemIdentity `hasNoType` ''SPEC

-- Not inspection-tested (not 'Step'-free fusion targets):
--   * Structure-building folds buffer the whole stream, so they keep their
--     'Step'/list constructors: 'foldl'Build'/'foldlM'Build' (o_n_heap),
--     'foldrMBuild'/'foldrMReduce' (o_n_space foldr), 'S.toList'/'S.toListRev'
--     and the 'Fold.toStream'/'Fold.toStreamRev' folds.
--   * 'showInstance'/'showInstanceList' (o_n_heap buffered) render via 'show'.
--   * 'foldableListMinBy' folds a plain list, not a stream.
#endif

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
            ++ o_n_heap_elimination_foldl size
            ++ o_n_heap_elimination_toList size
        , bgroup (o_n_space_prefix moduleName) $
            o_n_space_elimination_foldr size
            ++ o_n_space_elimination_toList size
        ]
