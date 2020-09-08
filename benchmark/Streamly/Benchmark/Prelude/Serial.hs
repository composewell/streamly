-- |
-- Module      : Main
-- Copyright   : (c) 2018 Composewell Technologies
--
-- License     : BSD3
-- Maintainer  : streamly@composewell.com

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}

#ifdef __HADDOCK_VERSION__
#undef INSPECTION
#endif

#ifdef INSPECTION
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fplugin Test.Inspection.Plugin #-}
#endif

module Main where

import Control.DeepSeq (NFData(..))
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.State.Strict (StateT, get, put, MonadState)
import qualified Control.Monad.State.Strict as State
import Control.Monad.Trans.Class (lift)
import Data.Functor.Identity (Identity, runIdentity)
import Data.IORef (newIORef, modifyIORef')
import GHC.Generics (Generic)
import System.Random (randomRIO)
import Prelude hiding
       ( concatMap, mapM_, init, last, elem, notElem, all, any
       , and, or, length, sum, product, maximum, minimum, reverse, fmap, map
       , sequence, mapM, tail, repeat, replicate, iterate, (!!), lookup)

import qualified Control.Applicative as AP
import qualified Prelude as P
import qualified Data.Foldable as F
import qualified GHC.Exts as GHC

#ifdef INSPECTION
import GHC.Types (SPEC(..))
import Test.Inspection

import qualified Streamly.Internal.Data.Stream.StreamD as D
#endif

import qualified Streamly.Prelude  as S
import qualified Streamly.Internal.Data.Stream.IsStream as Internal
import qualified Streamly.Internal.Data.Fold as FL
import qualified Streamly.Internal.Data.Unfold as UF

import Gauge
import Streamly.Prelude (SerialT, IsStream, serially, serial, MonadAsync)
import Streamly.Benchmark.Common
import Streamly.Benchmark.Prelude
import Streamly.Internal.Data.Time.Units

moduleName :: String
moduleName = "Prelude.Serial"

-------------------------------------------------------------------------------
-- Generation
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- fromList
-------------------------------------------------------------------------------

{-# INLINE sourceIsList #-}
sourceIsList :: Int -> Int -> SerialT Identity Int
sourceIsList value n = GHC.fromList [n..n+value]

{-# INLINE sourceIsString #-}
sourceIsString :: Int -> Int -> SerialT Identity P.Char
sourceIsString value n = GHC.fromString (P.replicate (n + value) 'a')

{-# INLINE readInstance #-}
readInstance :: P.String -> SerialT Identity Int
readInstance str =
    let r = P.reads str
    in case r of
        [(x,"")] -> x
        _ -> P.error "readInstance: no parse"

-- For comparisons
{-# INLINE readInstanceList #-}
readInstanceList :: P.String -> [Int]
readInstanceList str =
    let r = P.reads str
    in case r of
        [(x,"")] -> x
        _ -> P.error "readInstance: no parse"

{-# INLINE repeat #-}
repeat :: (Monad m, S.IsStream t) => Int -> Int -> t m Int
repeat count = S.take count . S.repeat

{-# INLINE repeatM #-}
repeatM :: (MonadAsync m, S.IsStream t) => Int -> Int -> t m Int
repeatM count = S.take count . S.repeatM . return

{-# INLINE replicate #-}
replicate :: (Monad m, S.IsStream t) => Int -> Int -> t m Int
replicate count = S.replicate count

{-# INLINE replicateM #-}
replicateM :: (MonadAsync m, S.IsStream t) => Int -> Int -> t m Int
replicateM count = S.replicateM count . return

{-# INLINE enumerateFrom #-}
enumerateFrom :: (Monad m, S.IsStream t) => Int -> Int -> t m Int
enumerateFrom count = S.take count . S.enumerateFrom

{-# INLINE enumerateFromTo #-}
enumerateFromTo :: (Monad m, S.IsStream t) => Int -> Int -> t m Int
enumerateFromTo = sourceIntFromTo

{-# INLINE enumerateFromThen #-}
enumerateFromThen :: (Monad m, S.IsStream t) => Int -> Int -> t m Int
enumerateFromThen count inp = S.take count $ S.enumerateFromThen inp (inp + 1)

{-# INLINE enumerateFromThenTo #-}
enumerateFromThenTo :: (Monad m, S.IsStream t) => Int -> Int -> t m Int
enumerateFromThenTo = sourceIntFromThenTo

-- n ~ 1
{-# INLINE enumerate #-}
enumerate :: (Monad m, S.IsStream t) => Int -> Int -> t m Int
enumerate count n = S.take (count + n) $ S.enumerate

-- n ~ 1
{-# INLINE enumerateTo #-}
enumerateTo :: (Monad m, S.IsStream t) => Int -> Int -> t m Int
enumerateTo count n = S.enumerateFrom (minBound + count + n)

{-# INLINE iterate #-}
iterate :: (Monad m, S.IsStream t) => Int -> Int -> t m Int
iterate count = S.take count . S.iterate (+1)

{-# INLINE iterateM #-}
iterateM :: (MonadAsync m, S.IsStream t) => Int -> Int -> t m Int
iterateM count = S.take count . S.iterateM (return . (+1)) . return

{-# INLINE fromIndices #-}
fromIndices :: (Monad m, S.IsStream t) => Int -> Int -> t m Int
fromIndices value n = S.take value $ S.fromIndices (+ n)

{-# INLINE fromIndicesM #-}
fromIndicesM :: (MonadAsync m, S.IsStream t) => Int -> Int -> t m Int
fromIndicesM value n = S.take value $ S.fromIndicesM (return <$> (+ n))

o_1_space_generation :: Int -> [Benchmark]
o_1_space_generation value =
    [ bgroup "generation"
        [ benchIOSrc serially "unfoldr" (sourceUnfoldr value)
        , benchIOSrc serially "unfoldrM" (sourceUnfoldrM value)
        , benchIOSrc serially "repeat" (repeat value)
        , benchIOSrc serially "repeatM" (repeatM value)
        , benchIOSrc serially "replicate" (replicate value)
        , benchIOSrc serially "replicateM" (replicateM value)
        , benchIOSrc serially "iterate" (iterate value)
        , benchIOSrc serially "iterateM" (iterateM value)
        , benchIOSrc serially "fromIndices" (fromIndices value)
        , benchIOSrc serially "fromIndicesM" (fromIndicesM value)
        , benchIOSrc serially "intFromTo" (sourceIntFromTo value)
        , benchIOSrc serially "intFromThenTo" (sourceIntFromThenTo value)
        , benchIOSrc serially "integerFromStep" (sourceIntegerFromStep value)
        , benchIOSrc serially "fracFromThenTo" (sourceFracFromThenTo value)
        , benchIOSrc serially "fracFromTo" (sourceFracFromTo value)
        , benchIOSrc serially "fromList" (sourceFromList value)
        , benchPureSrc "IsList.fromList" (sourceIsList value)
        , benchPureSrc "IsString.fromString" (sourceIsString value)
        , benchIOSrc serially "fromListM" (sourceFromListM value)
        , benchIOSrc serially "enumerateFrom" (enumerateFrom value)
        , benchIOSrc serially "enumerateFromThen" (enumerateFromThen value)
        , benchIOSrc serially "enumerate" (enumerate value)
        , benchIOSrc serially "enumerateTo" (enumerateTo value)

          -- These essentially test cons and consM
        , benchIOSrc serially "fromFoldable" (sourceFromFoldable value)
        , benchIOSrc serially "fromFoldableM" (sourceFromFoldableM value)

        , benchIOSrc serially "absTimes" $ absTimes value
        ]
    ]

o_n_heap_generation :: Int -> [Benchmark]
o_n_heap_generation value =
    [ bgroup "buffered"
    -- Buffers the output of show/read.
    -- XXX can the outputs be streaming? Can we have special read/show
    -- style type classes, readM/showM supporting streaming effects?
        [ bench "readsPrec pure streams" $
          nf readInstance (mkString value)
        , bench "readsPrec Haskell lists" $
          nf readInstanceList (mkListString value)
        ]
    ]

-------------------------------------------------------------------------------
-- Elimination
-------------------------------------------------------------------------------

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
    F.foldr (\x xs -> x == value || xs)
            P.False
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
    value `P.elem` (sourceUnfoldr value n :: S.SerialT Identity Int)

{-# INLINE foldableNotElem #-}
foldableNotElem :: Int -> Int -> Bool
foldableNotElem value n =
    value `P.notElem` (sourceUnfoldr value n :: S.SerialT Identity Int)

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
    P.and $ S.map
        (<= (value + 1)) (sourceUnfoldr value n :: S.SerialT Identity Int)

{-# INLINE foldableOr #-}
foldableOr :: Int -> Int -> Bool
foldableOr value n =
    P.or $ S.map
        (> (value + 1)) (sourceUnfoldr value n :: S.SerialT Identity Int)

{-# INLINE foldableLength #-}
foldableLength :: Int -> Int -> Int
foldableLength value n =
    P.length (sourceUnfoldr value n :: S.SerialT Identity Int)

{-# INLINE foldableMin #-}
foldableMin :: Int -> Int -> Int
foldableMin value n =
    P.minimum (sourceUnfoldr value n :: S.SerialT Identity Int)

{-# INLINE ordInstanceMin #-}
ordInstanceMin :: SerialT Identity Int -> SerialT Identity Int
ordInstanceMin src = P.min src src

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

{-# INLINE showInstance #-}
showInstance :: SerialT Identity Int -> P.String
showInstance = P.show

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
        , bench "length . toList" $ nf (P.length . foldableToList value) 1
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
    => Int -> String -> (SerialT Identity Int -> b) -> Benchmark
benchPureSink value name = benchPure name (sourceUnfoldr value)

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

-------------------------------------------------------------------------------
-- Reductions
-------------------------------------------------------------------------------

{-# INLINE uncons #-}
uncons :: Monad m => SerialT m Int -> m ()
uncons s = do
    r <- S.uncons s
    case r of
        Nothing -> return ()
        Just (_, t) -> uncons t

{-# INLINE init #-}
init :: Monad m => SerialT m a -> m ()
init s = S.init s >>= P.mapM_ S.drain

{-# INLINE mapM_ #-}
mapM_ :: Monad m => SerialT m Int -> m ()
mapM_ = S.mapM_ (\_ -> return ())

{-# INLINE foldrMElem #-}
foldrMElem :: Monad m => Int -> SerialT m Int -> m Bool
foldrMElem e =
    S.foldrM
        (\x xs ->
             if x == e
                 then return P.True
                 else xs)
        (return P.False)

{-# INLINE foldrMToStream #-}
foldrMToStream :: Monad m => SerialT m Int -> m (SerialT Identity Int)
foldrMToStream = S.foldr S.cons S.nil

{-# INLINE foldrMBuild #-}
foldrMBuild :: Monad m => SerialT m Int -> m [Int]
foldrMBuild = S.foldrM (\x xs -> (x :) <$> xs) (return [])

{-# INLINE foldl'Reduce #-}
foldl'Reduce :: Monad m => SerialT m Int -> m Int
foldl'Reduce = S.foldl' (+) 0

{-# INLINE foldl1'Reduce #-}
foldl1'Reduce :: Monad m => SerialT m Int -> m (Maybe Int)
foldl1'Reduce = S.foldl1' (+)

{-# INLINE foldlM'Reduce #-}
foldlM'Reduce :: Monad m => SerialT m Int -> m Int
foldlM'Reduce = S.foldlM' (\xs a -> return $ a + xs) (return 0)

{-# INLINE last #-}
last :: Monad m => SerialT m Int -> m (Maybe Int)
last = S.last

{-# INLINE _head #-}
_head :: Monad m => SerialT m Int -> m (Maybe Int)
_head = S.head

{-# INLINE elem #-}
elem :: Monad m => Int -> SerialT m Int -> m Bool
elem value = S.elem (value + 1)

{-# INLINE notElem #-}
notElem :: Monad m => Int -> SerialT m Int -> m Bool
notElem value = S.notElem (value + 1)

{-# INLINE length #-}
length :: Monad m => SerialT m Int -> m Int
length = S.length

{-# INLINE all #-}
all :: Monad m => Int -> SerialT m Int -> m Bool
all value = S.all (<= (value + 1))

{-# INLINE any #-}
any :: Monad m => Int -> SerialT m Int -> m Bool
any value = S.any (> (value + 1))

{-# INLINE and #-}
and :: Monad m => Int -> SerialT m Int -> m Bool
and value = S.and . S.map (<= (value + 1))

{-# INLINE or #-}
or :: Monad m => Int -> SerialT m Int -> m Bool
or value = S.or . S.map (> (value + 1))

{-# INLINE find #-}
find :: Monad m => Int -> SerialT m Int -> m (Maybe Int)
find value = S.find (== (value + 1))

{-# INLINE findIndex #-}
findIndex :: Monad m => Int -> SerialT m Int -> m (Maybe Int)
findIndex value = S.findIndex (== (value + 1))

{-# INLINE elemIndex #-}
elemIndex :: Monad m => Int -> SerialT m Int -> m (Maybe Int)
elemIndex value = S.elemIndex (value + 1)

{-# INLINE maximum #-}
maximum :: Monad m => SerialT m Int -> m (Maybe Int)
maximum = S.maximum

{-# INLINE minimum #-}
minimum :: Monad m => SerialT m Int -> m (Maybe Int)
minimum = S.minimum

{-# INLINE sum #-}
sum :: Monad m => SerialT m Int -> m Int
sum = S.sum

{-# INLINE product #-}
product :: Monad m => SerialT m Int -> m Int
product = S.product

{-# INLINE minimumBy #-}
minimumBy :: Monad m => SerialT m Int -> m (Maybe Int)
minimumBy = S.minimumBy compare

{-# INLINE maximumBy #-}
maximumBy :: Monad m => SerialT m Int -> m (Maybe Int)
maximumBy = S.maximumBy compare

{-# INLINE the #-}
the :: Monad m => SerialT m Int -> m (Maybe Int)
the = S.the

{-# INLINE drainN #-}
drainN :: Monad m => Int -> SerialT m Int -> m ()
drainN val = S.drainN val

{-# INLINE drainWhile #-}
drainWhile :: Monad m => SerialT m Int -> m ()
drainWhile = S.drainWhile (const True)

{-# INLINE (!!) #-}
(!!) :: Monad m => Int -> SerialT m Int -> m (Maybe Int)
(!!) val = flip (Internal.!!) val

{-# INLINE lookup #-}
lookup :: Monad m => Int -> SerialT m Int -> m (Maybe Int)
lookup val = S.lookup val . S.map (\x -> (x, x))

o_1_space_elimination_folds :: Int -> [Benchmark]
o_1_space_elimination_folds value =
    [ bgroup "elimination"
        -- Basic folds
        [ bgroup "reduce"
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
            ]
        , bgroup "build"
            [ bgroup "IO"
                  [ benchIOSink value "foldrMElem" (foldrMElem value)
                  ]
            , bgroup "Identity"
                  [ benchIdentitySink value "foldrMElem" (foldrMElem value)
                  , benchIdentitySink value "foldrMToStreamLength"
                        (S.length . runIdentity . foldrMToStream)
                  , benchPureSink value "foldrMToListLength"
                        (P.length . runIdentity . foldrMBuild)
                  ]
            ]

        -- deconstruction
        , benchIOSink value "uncons" uncons
        , benchIOSink value "init" init

        -- draining
        , benchIOSink value "drain" $ toNull serially
        , benchIOSink value "drainN" $ drainN value
        , benchIOSink value "drainWhile" $ drainWhile
        , benchPureSink value "drain (pure)" P.id
        , benchIOSink value "mapM_" mapM_

        -- this is too fast, causes all benchmarks reported in ns
    -- , benchIOSink value "head" head
        , benchIOSink value "last" last
        , benchIOSink value "length" length
        , benchHoistSink value "length . generally"
              (length . Internal.generally)
        , benchIOSink value "sum" sum
        , benchIOSink value "product" product
        , benchIOSink value "maximumBy" maximumBy
        , benchIOSink value "maximum" maximum
        , benchIOSink value "minimumBy" minimumBy
        , benchIOSink value "minimum" minimum

        , bench "the" $ nfIO $ randomRIO (1,1) >>= the . repeat value
        , benchIOSink value "find" (find value)
        -- , benchIOSink value "lookupFirst" (lookup 1)
        , benchIOSink value "lookupNever" (lookup (value + 1))
        , benchIOSink value "(!!)" ((!!) value)
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

        , benchPureSink value "showsPrec pure streams" showInstance
        -- length is used to check for foldr/build fusion
        , benchPureSink value "length . IsList.toList" (P.length . GHC.toList)
        ]
    ]

-------------------------------------------------------------------------------
-- Buffered Transformations by fold
-------------------------------------------------------------------------------

{-# INLINE foldl'Build #-}
foldl'Build :: Monad m => SerialT m Int -> m [Int]
foldl'Build = S.foldl' (flip (:)) []

{-# INLINE foldlM'Build #-}
foldlM'Build :: Monad m => SerialT m Int -> m [Int]
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
showInstanceList :: [Int] -> P.String
showInstanceList = P.show

o_n_heap_elimination_buffered :: Int -> [Benchmark]
o_n_heap_elimination_buffered value =
    [ bgroup "buffered"
        -- Buffers the output of show/read.
        -- XXX can the outputs be streaming? Can we have special read/show
        -- style type classes, readM/showM supporting streaming effects?
        [ bench "showPrec Haskell lists" $ nf showInstanceList (mkList value)
        ]
    ]

{-# INLINE foldrMReduce #-}
foldrMReduce :: Monad m => SerialT m Int -> m Int
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

o_n_heap_elimination_toList :: Int -> [Benchmark]
o_n_heap_elimination_toList value =
    [ bgroup "toList"
        -- Converting the stream to a list or pure stream in a strict monad
        [ benchIOSink value "toListRev" Internal.toListRev
        , benchIOSink value "toPureRev" Internal.toPureRev
        ]
    ]

o_n_space_elimination_toList :: Int -> [Benchmark]
o_n_space_elimination_toList value =
    [ bgroup "toList"
        -- Converting the stream to a list or pure stream in a strict monad
        [ benchIOSink value "toList" S.toList
        , benchIOSink value "toPure" Internal.toPure
        ]
    ]

-------------------------------------------------------------------------------
-- Multi-stream folds
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- Multi-stream pure
-------------------------------------------------------------------------------

{-# INLINE eqBy' #-}
eqBy' :: (Monad m, P.Eq a) => SerialT m a -> m P.Bool
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
eqInstance :: SerialT Identity Int -> Bool
eqInstance src = src == src

{-# INLINE eqInstanceNotEq #-}
eqInstanceNotEq :: SerialT Identity Int -> Bool
eqInstanceNotEq src = src P./= src

{-# INLINE cmpBy' #-}
cmpBy' :: (Monad m, P.Ord a) => SerialT m a -> m P.Ordering
cmpBy' src = S.cmpBy P.compare src src

{-# INLINE cmpByPure #-}
cmpByPure :: Int -> Int -> Identity P.Ordering
cmpByPure value n = cmpBy' (sourceUnfoldr value n)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'cmpByPure
inspect $ 'cmpByPure `hasNoType` ''SPEC
inspect $ 'cmpByPure `hasNoType` ''D.Step
#endif

{-# INLINE ordInstance #-}
ordInstance :: SerialT Identity Int -> Bool
ordInstance src = src P.< src

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
isPrefixOf :: Monad m => SerialT m Int -> m Bool
isPrefixOf src = S.isPrefixOf src src

{-# INLINE isSubsequenceOf #-}
isSubsequenceOf :: Monad m => SerialT m Int -> m Bool
isSubsequenceOf src = S.isSubsequenceOf src src

{-# INLINE stripPrefix #-}
stripPrefix :: Monad m => SerialT m Int -> m ()
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
cmpBy :: Int -> Int -> IO P.Ordering
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
-- Pipelines (stream-to-stream transformations)
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- one-to-one transformations
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- Traversable Instance
-------------------------------------------------------------------------------

{-# INLINE traversableTraverse #-}
traversableTraverse :: SerialT Identity Int -> IO (SerialT Identity Int)
traversableTraverse = P.traverse return

{-# INLINE traversableSequenceA #-}
traversableSequenceA :: SerialT Identity Int -> IO (SerialT Identity Int)
traversableSequenceA = P.sequenceA . P.fmap return

{-# INLINE traversableMapM #-}
traversableMapM :: SerialT Identity Int -> IO (SerialT Identity Int)
traversableMapM = P.mapM return

{-# INLINE traversableSequence #-}
traversableSequence :: SerialT Identity Int -> IO (SerialT Identity Int)
traversableSequence = P.sequence . P.fmap return

{-# INLINE benchPureSinkIO #-}
benchPureSinkIO
    :: NFData b
    => Int -> String -> (SerialT Identity Int -> IO b) -> Benchmark
benchPureSinkIO value name f =
    bench name $ nfIO $ randomRIO (1, 1) >>= f . sourceUnfoldr value

o_n_space_traversable :: Int -> [Benchmark]
o_n_space_traversable value =
    -- Buffering operations using heap proportional to number of elements.
    [ bgroup "traversable"
        -- Traversable instance
        [ benchPureSinkIO value "traverse" traversableTraverse
        , benchPureSinkIO value "sequenceA" traversableSequenceA
        , benchPureSinkIO value "mapM" traversableMapM
        , benchPureSinkIO value "sequence" traversableSequence
        ]
    ]

-------------------------------------------------------------------------------
-- maps and scans
-------------------------------------------------------------------------------

{-# INLINE scan #-}
scan :: MonadIO m => Int -> SerialT m Int -> m ()
scan n = composeN n $ S.scanl' (+) 0

{-# INLINE scanlM' #-}
scanlM' :: MonadIO m => Int -> SerialT m Int -> m ()
scanlM' n = composeN n $ S.scanlM' (\b a -> return $ b + a) (return 0)

{-# INLINE scanl1' #-}
scanl1' :: MonadIO m => Int -> SerialT m Int -> m ()
scanl1' n = composeN n $ S.scanl1' (+)

{-# INLINE postscanl' #-}
postscanl' :: MonadIO m => Int -> SerialT m Int -> m ()
postscanl' n = composeN n $ S.postscanl' (+) 0

{-# INLINE postscanlM' #-}
postscanlM' :: MonadIO m => Int -> SerialT m Int -> m ()
postscanlM' n = composeN n $ S.postscanlM' (\b a -> return $ b + a) (return 0)

{-# INLINE sequence #-}
sequence ::
       (S.IsStream t, S.MonadAsync m)
    => (t m Int -> S.SerialT m Int)
    -> t m (m Int)
    -> m ()
sequence t = S.drain . t . S.sequence

{-# INLINE tap #-}
tap :: MonadIO m => Int -> SerialT m Int -> m ()
tap n = composeN n $ S.tap FL.sum

{-# INLINE tapRate #-}
tapRate :: Int -> SerialT IO Int -> IO ()
tapRate n str = do
    cref <- newIORef 0
    composeN n (Internal.tapRate 1 (\c -> modifyIORef' cref (c +))) str

{-# INLINE pollCounts #-}
pollCounts :: Int -> SerialT IO Int -> IO ()
pollCounts n =
    composeN n (Internal.pollCounts (P.const P.True) f FL.drain)

    where

    f = Internal.rollingMap (P.-) . Internal.delayPost 1

{-# INLINE timestamped #-}
timestamped :: (S.MonadAsync m) => SerialT m Int -> m ()
timestamped = S.drain . Internal.timestamped

{-# INLINE foldrS #-}
foldrS :: MonadIO m => Int -> SerialT m Int -> m ()
foldrS n = composeN n $ Internal.foldrS S.cons S.nil

{-# INLINE foldrSMap #-}
foldrSMap :: MonadIO m => Int -> SerialT m Int -> m ()
foldrSMap n = composeN n $ Internal.foldrS (\x xs -> x + 1 `S.cons` xs) S.nil

{-# INLINE foldrT #-}
foldrT :: MonadIO m => Int -> SerialT m Int -> m ()
foldrT n = composeN n $ Internal.foldrT S.cons S.nil

{-# INLINE foldrTMap #-}
foldrTMap :: MonadIO m => Int -> SerialT m Int -> m ()
foldrTMap n = composeN n $ Internal.foldrT (\x xs -> x + 1 `S.cons` xs) S.nil


{-# INLINE trace #-}
trace :: MonadAsync m => Int -> SerialT m Int -> m ()
trace n = composeN n $ Internal.trace return

o_1_space_mapping :: Int -> [Benchmark]
o_1_space_mapping value =
    [ bgroup
        "mapping"
        [
        -- Right folds
          benchIOSink value "foldrS" (foldrS 1)
        , benchIOSink value "foldrSMap" (foldrSMap 1)
        , benchIOSink value "foldrT" (foldrT 1)
        , benchIOSink value "foldrTMap" (foldrTMap 1)

        -- Mapping
        , benchIOSink value "map" (mapN serially 1)
        , bench "sequence" $ nfIO $ randomRIO (1, 1000) >>= \n ->
              sequence serially (sourceUnfoldrAction value n)
        , benchIOSink value "mapM" (mapM serially 1)
        , benchIOSink value "tap" (tap 1)
        , benchIOSink value "tapRate 1 second" (tapRate 1)
        , benchIOSink value "pollCounts 1 second" (pollCounts 1)
        , benchIOSink value "timestamped" timestamped

        -- Scanning
        , benchIOSink value "scanl'" (scan 1)
        , benchIOSink value "scanl1'" (scanl1' 1)
        , benchIOSink value "scanlM'" (scanlM' 1)
        , benchIOSink value "postscanl'" (postscanl' 1)
        , benchIOSink value "postscanlM'" (postscanlM' 1)

        ]
    ]

o_1_space_mappingX4 :: Int -> [Benchmark]
o_1_space_mappingX4 value =
    [ bgroup "mappingX4"
        [ benchIOSink value "map" (mapN serially 4)
        , benchIOSink value "mapM" (mapM serially 4)
        , benchIOSink value "trace" (trace 4)

        , benchIOSink value "scan" (scan 4)
        , benchIOSink value "scanl1'" (scanl1' 4)
        , benchIOSink value "scanlM'" (scanlM' 4)
        , benchIOSink value "postscanl'" (postscanl' 4)
        , benchIOSink value "postscanlM'" (postscanlM' 4)

        ]
    ]

{-# INLINE sieveScan #-}
sieveScan :: Monad m => SerialT m Int -> SerialT m Int
sieveScan =
      S.mapMaybe snd
    . S.scanlM' (\(primes, _) n -> do
            return $
                let ps = takeWhile (\p -> p * p <= n) primes
                 in if P.all (\p -> n `mod` p /= 0) ps
                    then (primes ++ [n], Just n)
                    else (primes, Nothing)) (return ([2], Just 2))

o_n_space_mapping :: Int -> [Benchmark]
o_n_space_mapping value =
    [ bgroup "mapping"
        [ benchIO "naive prime sieve"
            (\n -> S.sum $ sieveScan $ S.enumerateFromTo 2 (value + n))
        ]
    ]

-------------------------------------------------------------------------------
-- Iteration/looping utilities
-------------------------------------------------------------------------------

{-# INLINE iterateN #-}
iterateN :: (Int -> a -> a) -> a -> Int -> a
iterateN g initial count = f count initial

    where

    f (0 :: Int) x = x
    f i x = f (i - 1) (g i x)

-- Iterate a transformation over a singleton stream
{-# INLINE iterateSingleton #-}
iterateSingleton :: S.MonadAsync m
    => (Int -> SerialT m Int -> SerialT m Int)
    -> Int
    -> Int
    -> SerialT m Int
iterateSingleton g count n = iterateN g (return n) count

-- XXX need to check why this is slower than the explicit recursion above, even
-- if the above code is written in a foldr like head recursive way. We also
-- need to try this with foldlM' once #150 is fixed.
-- However, it is perhaps best to keep the iteration benchmarks independent of
-- foldrM and any related fusion issues.
{-# INLINE _iterateSingleton #-}
_iterateSingleton ::
       S.MonadAsync m
    => (Int -> SerialT m Int -> SerialT m Int)
    -> Int
    -> Int
    -> SerialT m Int
_iterateSingleton g value n = S.foldrM g (return n) $ sourceIntFromTo value n

-- Apply transformation g count times on a stream of length len
{-# INLINE iterateSource #-}
iterateSource ::
       S.MonadAsync m
    => (SerialT m Int -> SerialT m Int)
    -> Int
    -> Int
    -> Int
    -> SerialT m Int
iterateSource g count len n = f count (sourceUnfoldrM len n)

    where

    f (0 :: Int) stream = stream
    f i stream = f (i - 1) (g stream)

-------------------------------------------------------------------------------
-- Functor
-------------------------------------------------------------------------------

o_1_space_functor :: Int -> [Benchmark]
o_1_space_functor value =
    [ bgroup "Functor"
        [ benchIOSink value "fmap" (fmapN serially 1)
        , benchIOSink value "fmap x 4" (fmapN serially 4)
        ]
    ]

o_n_space_functor :: Int -> [Benchmark]
o_n_space_functor value =
    [ bgroup "Functor"
        [ benchIO "(+) (n times) (baseline)" $ \i0 ->
            iterateN (\i acc -> acc >>= \n -> return $ i + n) (return i0) value
        , benchIOSrc serially "(<$) (n times)" $
            iterateSingleton (<$) value
        , benchIOSrc serially "fmap (n times)" $
            iterateSingleton (P.fmap . (+)) value
        {-
        , benchIOSrc serially "_(<$) (n times)" $
            _iterateSingleton (<$) value
        , benchIOSrc serially "_fmap (n times)" $
            _iterateSingleton (P.fmap . (+)) value
        -}
        ]
    ]

-------------------------------------------------------------------------------
-- Size reducing transformations (filtering)
-------------------------------------------------------------------------------

{-# INLINE filterEven #-}
filterEven :: MonadIO m => Int -> SerialT m Int -> m ()
filterEven n = composeN n $ S.filter even

{-# INLINE filterAllOut #-}
filterAllOut :: MonadIO m => Int -> Int -> SerialT m Int -> m ()
filterAllOut value n = composeN n $ S.filter (> (value + 1))

{-# INLINE filterAllIn #-}
filterAllIn :: MonadIO m => Int -> Int -> SerialT m Int -> m ()
filterAllIn value n = composeN n $ S.filter (<= (value + 1))

{-# INLINE filterMEven #-}
filterMEven :: MonadIO m => Int -> SerialT m Int -> m ()
filterMEven n = composeN n $ S.filterM (return . even)

{-# INLINE filterMAllOut #-}
filterMAllOut :: MonadIO m => Int -> Int -> SerialT m Int -> m ()
filterMAllOut value n = composeN n $ S.filterM (\x -> return $ x > (value + 1))

{-# INLINE filterMAllIn #-}
filterMAllIn :: MonadIO m => Int -> Int -> SerialT m Int -> m ()
filterMAllIn value n = composeN n $ S.filterM (\x -> return $ x <= (value + 1))

{-# INLINE _takeOne #-}
_takeOne :: MonadIO m => Int -> SerialT m Int -> m ()
_takeOne n = composeN n $ S.take 1

{-# INLINE takeAll #-}
takeAll :: MonadIO m => Int -> Int -> SerialT m Int -> m ()
takeAll value n = composeN n $ S.take (value + 1)

{-# INLINE takeWhileTrue #-}
takeWhileTrue :: MonadIO m => Int -> Int -> SerialT m Int -> m ()
takeWhileTrue value n = composeN n $ S.takeWhile (<= (value + 1))

{-# INLINE takeWhileMTrue #-}
takeWhileMTrue :: MonadIO m => Int -> Int -> SerialT m Int -> m ()
takeWhileMTrue value n = composeN n $ S.takeWhileM (return . (<= (value + 1)))

{-# INLINE takeByTime #-}
takeByTime :: NanoSecond64 -> Int -> SerialT IO Int -> IO ()
takeByTime i n = composeN n (Internal.takeByTime i)

#ifdef INSPECTION
-- inspect $ hasNoType 'takeByTime ''SPEC
inspect $ hasNoTypeClasses 'takeByTime
-- inspect $ 'takeByTime `hasNoType` ''D.Step
#endif

{-# INLINE dropOne #-}
dropOne :: MonadIO m => Int -> SerialT m Int -> m ()
dropOne n = composeN n $ S.drop 1

{-# INLINE dropAll #-}
dropAll :: MonadIO m => Int -> Int -> SerialT m Int -> m ()
dropAll value n = composeN n $ S.drop (value + 1)

{-# INLINE dropWhileTrue #-}
dropWhileTrue :: MonadIO m => Int -> Int -> SerialT m Int -> m ()
dropWhileTrue value n = composeN n $ S.dropWhile (<= (value + 1))

{-# INLINE dropWhileMTrue #-}
dropWhileMTrue :: MonadIO m => Int -> Int -> SerialT m Int -> m ()
dropWhileMTrue value n = composeN n $ S.dropWhileM (return . (<= (value + 1)))

{-# INLINE dropWhileFalse #-}
dropWhileFalse :: MonadIO m => Int -> Int -> SerialT m Int -> m ()
dropWhileFalse value n = composeN n $ S.dropWhile (> (value + 1))

-- XXX Decide on the time interval
{-# INLINE _intervalsOfSum #-}
_intervalsOfSum :: MonadAsync m => Double -> Int -> SerialT m Int -> m ()
_intervalsOfSum i n = composeN n (S.intervalsOf i FL.sum)

{-# INLINE dropByTime #-}
dropByTime :: NanoSecond64 -> Int -> SerialT IO Int -> IO ()
dropByTime i n = composeN n (Internal.dropByTime i)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'dropByTime
-- inspect $ 'dropByTime `hasNoType` ''D.Step
#endif

{-# INLINE findIndices #-}
findIndices :: MonadIO m => Int -> Int -> SerialT m Int -> m ()
findIndices value n = composeN n $ S.findIndices (== (value + 1))

{-# INLINE elemIndices #-}
elemIndices :: MonadIO m => Int -> Int -> SerialT m Int -> m ()
elemIndices value n = composeN n $ S.elemIndices (value + 1)

{-# INLINE deleteBy #-}
deleteBy :: MonadIO m => Int -> Int -> SerialT m Int -> m ()
deleteBy value n = composeN n $ S.deleteBy (>=) (value + 1)

-- uniq . uniq == uniq, composeN 2 ~ composeN 1
{-# INLINE uniq #-}
uniq :: MonadIO m => Int -> SerialT m Int -> m ()
uniq n = composeN n S.uniq

{-# INLINE mapMaybe #-}
mapMaybe :: MonadIO m => Int -> SerialT m Int -> m ()
mapMaybe n =
    composeN n $
    S.mapMaybe
        (\x ->
             if P.odd x
                 then Nothing
                 else Just x)

{-# INLINE mapMaybeM #-}
mapMaybeM :: S.MonadAsync m => Int -> SerialT m Int -> m ()
mapMaybeM n =
    composeN n $
    S.mapMaybeM
        (\x ->
             if P.odd x
                 then return Nothing
                 else return $ Just x)

o_1_space_filtering :: Int -> [Benchmark]
o_1_space_filtering value =
    [ bgroup "filtering"
        [ benchIOSink value "filter-even" (filterEven 1)
        , benchIOSink value "filter-all-out" (filterAllOut value 1)
        , benchIOSink value "filter-all-in" (filterAllIn value 1)

        , benchIOSink value "filterM-even" (filterMEven 1)
        , benchIOSink value "filterM-all-out" (filterMAllOut value 1)
        , benchIOSink value "filterM-all-in" (filterMAllIn value 1)

        -- Trimming
        , benchIOSink value "take-all" (takeAll value 1)
        , benchIOSink
              value
              "takeByTime-all"
              (takeByTime (NanoSecond64 maxBound) 1)
        , benchIOSink value "takeWhile-true" (takeWhileTrue value 1)
     -- , benchIOSink value "takeWhileM-true" (_takeWhileMTrue value 1)
        , benchIOSink value "drop-one" (dropOne 1)
        , benchIOSink value "drop-all" (dropAll value 1)
        , benchIOSink
              value
              "dropByTime-all"
              (dropByTime (NanoSecond64 maxBound) 1)
        , benchIOSink value "dropWhile-true" (dropWhileTrue value 1)
     -- , benchIOSink value "dropWhileM-true" (_dropWhileMTrue value 1)
        , benchIOSink
              value
              "dropWhile-false"
              (dropWhileFalse value 1)
        , benchIOSink value "deleteBy" (deleteBy value 1)

        , benchIOSink value "uniq" (uniq 1)

        -- Map and filter
        , benchIOSink value "mapMaybe" (mapMaybe 1)
        , benchIOSink value "mapMaybeM" (mapMaybeM 1)

        -- Searching (stateful map and filter)
        , benchIOSink value "findIndices" (findIndices value 1)
        , benchIOSink value "elemIndices" (elemIndices value 1)
        ]
    ]

o_1_space_filteringX4 :: Int -> [Benchmark]
o_1_space_filteringX4 value =
    [ bgroup "filteringX4"
        [ benchIOSink value "filter-even" (filterEven 4)
        , benchIOSink value "filter-all-out" (filterAllOut value 4)
        , benchIOSink value "filter-all-in" (filterAllIn value 4)

        , benchIOSink value "filterM-even" (filterMEven 4)
        , benchIOSink value "filterM-all-out" (filterMAllOut value 4)
        , benchIOSink value "filterM-all-in" (filterMAllIn value 4)

        -- trimming
        , benchIOSink value "take-all" (takeAll value 4)
        , benchIOSink value "takeWhile-true" (takeWhileTrue value 4)
        , benchIOSink value "takeWhileM-true" (takeWhileMTrue value 4)
        , benchIOSink value "drop-one" (dropOne 4)
        , benchIOSink value "drop-all" (dropAll value 4)
        , benchIOSink value "dropWhile-true" (dropWhileTrue value 4)
        , benchIOSink value "dropWhileM-true" (dropWhileMTrue value 4)
        , benchIOSink
              value
              "dropWhile-false"
              (dropWhileFalse value 4)
        , benchIOSink value "deleteBy" (deleteBy value 4)

        , benchIOSink value "uniq" (uniq 4)

        -- map and filter
        , benchIOSink value "mapMaybe" (mapMaybe 4)
        , benchIOSink value "mapMaybeM" (mapMaybeM 4)

        -- searching
        , benchIOSink value "findIndices" (findIndices value 4)
        , benchIOSink value "elemIndices" (elemIndices value 4)
        ]
    ]

-------------------------------------------------------------------------------
-- Grouping transformations
-------------------------------------------------------------------------------

{-# INLINE groups #-}
groups :: MonadIO m => SerialT m Int -> m ()
groups = S.drain . S.groups FL.drain

-- XXX Change this test when the order of comparison is later changed
{-# INLINE groupsByGT #-}
groupsByGT :: MonadIO m => SerialT m Int -> m ()
groupsByGT = S.drain . S.groupsBy (>) FL.drain

{-# INLINE groupsByEq #-}
groupsByEq :: MonadIO m => SerialT m Int -> m ()
groupsByEq = S.drain . S.groupsBy (>) FL.drain

-- XXX Change this test when the order of comparison is later changed
{-# INLINE groupsByRollingLT #-}
groupsByRollingLT :: MonadIO m => SerialT m Int -> m ()
groupsByRollingLT =
    S.drain . S.groupsByRolling (<) FL.drain

{-# INLINE groupsByRollingEq #-}
groupsByRollingEq :: MonadIO m => SerialT m Int -> m ()
groupsByRollingEq =
    S.drain . S.groupsByRolling (>) FL.drain

o_1_space_grouping :: Int -> [Benchmark]
o_1_space_grouping value =
    -- Buffering operations using heap proportional to group/window sizes.
    [ bgroup "grouping"
        [ benchIOSink value "groups" groups
        , benchIOSink value "groupsByGT" groupsByGT
        , benchIOSink value "groupsByEq" groupsByEq
        , benchIOSink value "groupsByRollingLT" groupsByRollingLT
        , benchIOSink value "groupsByRollingEq" groupsByRollingEq
        ]
    ]

-------------------------------------------------------------------------------
-- Size increasing transformations (insertions)
-------------------------------------------------------------------------------

{-# INLINE intersperse #-}
intersperse :: S.MonadAsync m => Int -> Int -> SerialT m Int -> m ()
intersperse value n = composeN n $ S.intersperse (value + 1)

{-# INLINE intersperseM #-}
intersperseM :: S.MonadAsync m => Int -> Int -> SerialT m Int -> m ()
intersperseM value n = composeN n $ S.intersperseM (return $ value + 1)

{-# INLINE insertBy #-}
insertBy :: MonadIO m => Int -> Int -> SerialT m Int -> m ()
insertBy value n = composeN n $ S.insertBy compare (value + 1)

o_1_space_inserting :: Int -> [Benchmark]
o_1_space_inserting value =
    [ bgroup "filtering"
        [ benchIOSink value "intersperse" (intersperse value 1)
        , benchIOSink value "intersperseM" (intersperseM value 1)
        , benchIOSink value "insertBy" (insertBy value 1)
        ]
    ]

o_1_space_insertingX4 :: Int -> [Benchmark]
o_1_space_insertingX4 value =
    [ bgroup "insertingX4"
        [ benchIOSink value "intersperse" (intersperse value 4)
        , benchIOSink value "insertBy" (insertBy value 4)
        ]
    ]

-------------------------------------------------------------------------------
-- Indexing
-------------------------------------------------------------------------------

{-# INLINE indexed #-}
indexed :: MonadIO m => Int -> SerialT m Int -> m ()
indexed n = composeN n (S.map snd . S.indexed)

{-# INLINE indexedR #-}
indexedR :: MonadIO m => Int -> Int -> SerialT m Int -> m ()
indexedR value n = composeN n (S.map snd . S.indexedR value)

o_1_space_indexing :: Int -> [Benchmark]
o_1_space_indexing value =
    [ bgroup "indexing"
        [ benchIOSink value "indexed" (indexed 1)
        , benchIOSink value "indexedR" (indexedR value 1)
        ]
    ]

o_1_space_indexingX4 :: Int -> [Benchmark]
o_1_space_indexingX4 value =
    [ bgroup "indexingx4"
        [ benchIOSink value "indexed" (indexed 4)
        , benchIOSink value "indexedR" (indexedR value 4)
        ]
    ]

-------------------------------------------------------------------------------
-- Reordering
-------------------------------------------------------------------------------

{-# INLINE reverse #-}
reverse :: MonadIO m => Int -> SerialT m Int -> m ()
reverse n = composeN n S.reverse

{-# INLINE reverse' #-}
reverse' :: MonadIO m => Int -> SerialT m Int -> m ()
reverse' n = composeN n Internal.reverse'

o_n_heap_reordering :: Int -> [Benchmark]
o_n_heap_reordering value =
    [ bgroup "buffered"
        [
        -- Reversing/sorting a stream
          benchIOSink value "reverse" (reverse 1)
        , benchIOSink value "reverse'" (reverse' 1)
        ]
    ]

-------------------------------------------------------------------------------
-- Grouping/Splitting
-------------------------------------------------------------------------------

{-# INLINE classifySessionsOf #-}
classifySessionsOf :: (S.MonadAsync m) => SerialT m Int -> m ()
classifySessionsOf =
      S.drain
    . Internal.classifySessionsOf
        3 (const (return False)) (P.fmap Right FL.drain)
    . S.map (\(ts,(k,a)) -> (k, a, ts))
    . Internal.timestamped
    . S.concatMap (\x -> S.map (x,) (S.enumerateFromTo 1 (10 :: Int)))

o_n_space_grouping :: Int -> [Benchmark]
o_n_space_grouping value =
    -- Buffering operations using heap proportional to group/window sizes.
    [ bgroup "grouping"
        [ benchIOSink (value `div` 10) "classifySessionsOf" classifySessionsOf
        ]
    ]

-------------------------------------------------------------------------------
-- Mixed Transformation
-------------------------------------------------------------------------------

{-# INLINE scanMap #-}
scanMap :: MonadIO m => Int -> SerialT m Int -> m ()
scanMap n = composeN n $ S.map (subtract 1) . S.scanl' (+) 0

{-# INLINE dropMap #-}
dropMap :: MonadIO m => Int -> SerialT m Int -> m ()
dropMap n = composeN n $ S.map (subtract 1) . S.drop 1

{-# INLINE dropScan #-}
dropScan :: MonadIO m => Int -> SerialT m Int -> m ()
dropScan n = composeN n $ S.scanl' (+) 0 . S.drop 1

{-# INLINE takeDrop #-}
takeDrop :: MonadIO m => Int -> Int -> SerialT m Int -> m ()
takeDrop value n = composeN n $ S.drop 1 . S.take (value + 1)

{-# INLINE takeScan #-}
takeScan :: MonadIO m => Int -> Int -> SerialT m Int -> m ()
takeScan value n = composeN n $ S.scanl' (+) 0 . S.take (value + 1)

{-# INLINE takeMap #-}
takeMap :: MonadIO m => Int -> Int -> SerialT m Int -> m ()
takeMap value n = composeN n $ S.map (subtract 1) . S.take (value + 1)

{-# INLINE filterDrop #-}
filterDrop :: MonadIO m => Int -> Int -> SerialT m Int -> m ()
filterDrop value n = composeN n $ S.drop 1 . S.filter (<= (value + 1))

{-# INLINE filterTake #-}
filterTake :: MonadIO m => Int -> Int -> SerialT m Int -> m ()
filterTake value n = composeN n $ S.take (value + 1) . S.filter (<= (value + 1))

{-# INLINE filterScan #-}
filterScan :: MonadIO m => Int -> SerialT m Int -> m ()
filterScan n = composeN n $ S.scanl' (+) 0 . S.filter (<= maxBound)

{-# INLINE filterScanl1 #-}
filterScanl1 :: MonadIO m => Int -> SerialT m Int -> m ()
filterScanl1 n = composeN n $ S.scanl1' (+) . S.filter (<= maxBound)

{-# INLINE filterMap #-}
filterMap :: MonadIO m => Int -> Int -> SerialT m Int -> m ()
filterMap value n = composeN n $ S.map (subtract 1) . S.filter (<= (value + 1))

-------------------------------------------------------------------------------
-- Scan and fold
-------------------------------------------------------------------------------

data Pair a b =
    Pair !a !b
    deriving (Generic, NFData)

{-# INLINE sumProductFold #-}
sumProductFold :: Monad m => SerialT m Int -> m (Int, Int)
sumProductFold = S.foldl' (\(s, p) x -> (s + x, p P.* x)) (0, 1)

{-# INLINE sumProductScan #-}
sumProductScan :: Monad m => SerialT m Int -> m (Pair Int Int)
sumProductScan =
    S.foldl' (\(Pair _ p) (s0, x) -> Pair s0 (p P.* x)) (Pair 0 1) .
    S.scanl' (\(s, _) x -> (s + x, x)) (0, 0)

{-# INLINE foldl'ReduceMap #-}
foldl'ReduceMap :: Monad m => SerialT m Int -> m Int
foldl'ReduceMap = P.fmap (+ 1) . S.foldl' (+) 0

o_1_space_transformations_mixed :: Int -> [Benchmark]
o_1_space_transformations_mixed value =
    -- scanl-map and foldl-map are equivalent to the scan and fold in the foldl
    -- library. If scan/fold followed by a map is efficient enough we may not
    -- need monolithic implementations of these.
    [ bgroup "mixed"
        [ benchIOSink value "scanl-map" (scanMap 1)
        , benchIOSink value "foldl-map" foldl'ReduceMap
        , benchIOSink value "sum-product-fold" sumProductFold
        , benchIOSink value "sum-product-scan" sumProductScan
        ]
    ]

o_1_space_transformations_mixedX4 :: Int -> [Benchmark]
o_1_space_transformations_mixedX4 value =
    [ bgroup "mixedX4"
        [ benchIOSink value "scan-map" (scanMap 4)
        , benchIOSink value "drop-map" (dropMap 4)
        , benchIOSink value "drop-scan" (dropScan 4)
        , benchIOSink value "take-drop" (takeDrop value 4)
        , benchIOSink value "take-scan" (takeScan value 4)
        , benchIOSink value "take-map" (takeMap value 4)
        , benchIOSink value "filter-drop" (filterDrop value 4)
        , benchIOSink value "filter-take" (filterTake value 4)
        , benchIOSink value "filter-scan" (filterScan 4)
        , benchIOSink value "filter-scanl1" (filterScanl1 4)
        , benchIOSink value "filter-map" (filterMap value 4)
        ]
    ]

-------------------------------------------------------------------------------
-- Iterating a transformation over and over again
-------------------------------------------------------------------------------

-- this is quadratic
{-# INLINE iterateScan #-}
iterateScan :: S.MonadAsync m => Int -> Int -> Int -> SerialT m Int
iterateScan = iterateSource (S.scanl' (+) 0)

-- this is quadratic
{-# INLINE iterateScanl1 #-}
iterateScanl1 :: S.MonadAsync m => Int -> Int -> Int -> SerialT m Int
iterateScanl1 = iterateSource (S.scanl1' (+))

{-# INLINE iterateMapM #-}
iterateMapM :: S.MonadAsync m => Int -> Int -> Int -> SerialT m Int
iterateMapM = iterateSource (S.mapM return)

{-# INLINE iterateFilterEven #-}
iterateFilterEven :: S.MonadAsync m => Int -> Int -> Int -> SerialT m Int
iterateFilterEven = iterateSource (S.filter even)

{-# INLINE iterateTakeAll #-}
iterateTakeAll :: S.MonadAsync m => Int -> Int -> Int -> Int -> SerialT m Int
iterateTakeAll value = iterateSource (S.take (value + 1))

{-# INLINE iterateDropOne #-}
iterateDropOne :: S.MonadAsync m => Int -> Int -> Int -> SerialT m Int
iterateDropOne = iterateSource (S.drop 1)

{-# INLINE iterateDropWhileFalse #-}
iterateDropWhileFalse :: S.MonadAsync m
    => Int -> Int -> Int -> Int -> SerialT m Int
iterateDropWhileFalse value = iterateSource (S.dropWhile (> (value + 1)))

{-# INLINE iterateDropWhileTrue #-}
iterateDropWhileTrue :: S.MonadAsync m
    => Int -> Int -> Int -> Int -> SerialT m Int
iterateDropWhileTrue value = iterateSource (S.dropWhile (<= (value + 1)))

{-# INLINE tail #-}
tail :: Monad m => SerialT m a -> m ()
tail s = S.tail s >>= P.mapM_ tail

{-# INLINE nullHeadTail #-}
nullHeadTail :: Monad m => SerialT m Int -> m ()
nullHeadTail s = do
    r <- S.null s
    when (not r) $ do
        _ <- S.head s
        S.tail s >>= P.mapM_ nullHeadTail

-- Head recursive operations.
o_n_stack_iterated :: Int -> [Benchmark]
o_n_stack_iterated value = by10 `seq` by100 `seq`
    [ bgroup "iterated"
        [ benchIOSrc serially "mapM (n/10 x 10)" $ iterateMapM by10 10
        , benchIOSrc serially "scanl' (quadratic) (n/100 x 100)" $
            iterateScan by100 100
        , benchIOSrc serially "scanl1' (n/10 x 10)" $ iterateScanl1 by10 10
        , benchIOSrc serially "filterEven (n/10 x 10)" $
            iterateFilterEven by10 10
        , benchIOSrc serially "takeAll (n/10 x 10)" $
            iterateTakeAll value by10 10
        , benchIOSrc serially "dropOne (n/10 x 10)" $ iterateDropOne by10 10
        , benchIOSrc serially "dropWhileFalse (n/10 x 10)" $
            iterateDropWhileFalse value by10 10
        , benchIOSrc serially "dropWhileTrue (n/10 x 10)" $
            iterateDropWhileTrue value by10 10
        , benchIOSink value "tail" tail
        , benchIOSink value "nullHeadTail" nullHeadTail
        ]
    ]

    where

    by10 = value `div` 10
    by100 = value `div` 100

-------------------------------------------------------------------------------
-- Pipes
-------------------------------------------------------------------------------

o_1_space_pipes :: Int -> [Benchmark]
o_1_space_pipes value =
    [ bgroup "pipes"
        [ benchIOSink value "mapM" (transformMapM serially 1)
        , benchIOSink value "compose" (transformComposeMapM serially 1)
        , benchIOSink value "tee" (transformTeeMapM serially 1)
        , benchIOSink value "zip" (transformZipMapM serially 1)
        ]
    ]

o_1_space_pipesX4 :: Int -> [Benchmark]
o_1_space_pipesX4 value =
    [ bgroup "pipesX4"
        [ benchIOSink value "mapM" (transformMapM serially 4)
        , benchIOSink value "compose" (transformComposeMapM serially 4)
        , benchIOSink value "tee" (transformTeeMapM serially 4)
        , benchIOSink value "zip" (transformZipMapM serially 4)
        ]
    ]

-------------------------------------------------------------------------------
-- Multi-Stream
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- Appending
-------------------------------------------------------------------------------

{-# INLINE serial2 #-}
serial2 :: Int -> Int -> IO ()
serial2 count n =
    S.drain $
        S.serial (sourceUnfoldrM count n) (sourceUnfoldrM count (n + 1))

{-# INLINE serial4 #-}
serial4 :: Int -> Int -> IO ()
serial4 count n =
    S.drain $
    S.serial
        (S.serial (sourceUnfoldrM count n) (sourceUnfoldrM count (n + 1)))
        (S.serial
              (sourceUnfoldrM count (n + 2))
              (sourceUnfoldrM count (n + 3)))

{-# INLINE append2 #-}
append2 :: Int -> Int -> IO ()
append2 count n =
    S.drain $
    Internal.append (sourceUnfoldrM count n) (sourceUnfoldrM count (n + 1))

{-# INLINE append4 #-}
append4 :: Int -> Int -> IO ()
append4 count n =
    S.drain $
    Internal.append
        (Internal.append
              (sourceUnfoldrM count n)
              (sourceUnfoldrM count (n + 1)))
        (Internal.append
              (sourceUnfoldrM count (n + 2))
              (sourceUnfoldrM count (n + 3)))

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'append2
inspect $ 'append2 `hasNoType` ''SPEC
inspect $ 'append2 `hasNoType` ''D.AppendState
#endif

-------------------------------------------------------------------------------
-- Merging
-------------------------------------------------------------------------------

{-# INLINE mergeBy #-}
mergeBy :: Int -> Int -> IO ()
mergeBy count n =
    S.drain $
    S.mergeBy
        P.compare
        (sourceUnfoldrM count n)
        (sourceUnfoldrM count (n + 1))

{-# INLINE mergeByM #-}
mergeByM :: Int -> Int -> IO ()
mergeByM count n =
    S.drain $
    S.mergeByM
        (\a b -> return $ P.compare a b)
        (sourceUnfoldrM count n)
        (sourceUnfoldrM count (n + 1))

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'mergeBy
inspect $ 'mergeBy `hasNoType` ''SPEC
inspect $ 'mergeBy `hasNoType` ''D.Step

inspect $ hasNoTypeClasses 'mergeByM
inspect $ 'mergeByM `hasNoType` ''SPEC
inspect $ 'mergeByM `hasNoType` ''D.Step
#endif

o_1_space_joining :: Int -> [Benchmark]
o_1_space_joining value =
    [ bgroup "joining"
        [ benchIOSrc1 "serial (2,x/2)" (serial2 (value `div` 2))
        , benchIOSrc1 "append (2,x/2)" (append2 (value `div` 2))
        , benchIOSrc1 "serial (2,2,x/4)" (serial4 (value `div` 4))
        , benchIOSrc1 "append (2,2,x/4)" (append4 (value `div` 4))
        , benchIOSrc1 "mergeBy (2,x/2)" (mergeBy (value `div` 2))
        , benchIOSrc1 "mergeByM (2,x/2)" (mergeByM (value `div` 2))
        ]
    ]

-------------------------------------------------------------------------------
-- Concat Foldable containers
-------------------------------------------------------------------------------

o_1_space_concatFoldable :: Int -> [Benchmark]
o_1_space_concatFoldable value =
    [ bgroup "concat-foldable"
        [ benchIOSrc serially "foldMapWith (<>) (List)"
            (sourceFoldMapWith value)
        , benchIOSrc serially "foldMapWith (<>) (Stream)"
            (sourceFoldMapWithStream value)
        , benchIOSrc serially "foldMapWithM (<>) (List)"
            (sourceFoldMapWithM value)
        , benchIOSrc serially "foldMapM (List)" (sourceFoldMapM value)
        ]
    ]

-------------------------------------------------------------------------------
-- Concat
-------------------------------------------------------------------------------

-- concatMap unfoldrM/unfoldrM

{-# INLINE concatMap #-}
concatMap :: Int -> Int -> Int -> IO ()
concatMap outer inner n =
    S.drain $ S.concatMap
        (\_ -> sourceUnfoldrM inner n)
        (sourceUnfoldrM outer n)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'concatMap
inspect $ 'concatMap `hasNoType` ''SPEC
#endif

-- concatMap unfoldr/unfoldr

{-# INLINE concatMapPure #-}
concatMapPure :: Int -> Int -> Int -> IO ()
concatMapPure outer inner n =
    S.drain $ S.concatMap
        (\_ -> sourceUnfoldr inner n)
        (sourceUnfoldr outer n)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'concatMapPure
inspect $ 'concatMapPure `hasNoType` ''SPEC
#endif

-- concatMap replicate/unfoldrM

{-# INLINE concatMapRepl #-}
concatMapRepl :: Int -> Int -> Int -> IO ()
concatMapRepl outer inner n =
    S.drain $ S.concatMap (S.replicate inner) (sourceUnfoldrM outer n)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'concatMapRepl
inspect $ 'concatMapRepl `hasNoType` ''SPEC
#endif

-- concatMapWith

{-# INLINE concatMapWithSerial #-}
concatMapWithSerial :: Int -> Int -> Int -> IO ()
concatMapWithSerial = concatStreamsWith S.serial

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'concatMapWithSerial
inspect $ 'concatMapWithSerial `hasNoType` ''SPEC
#endif

{-# INLINE concatMapWithAppend #-}
concatMapWithAppend :: Int -> Int -> Int -> IO ()
concatMapWithAppend = concatStreamsWith Internal.append

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'concatMapWithAppend
inspect $ 'concatMapWithAppend `hasNoType` ''SPEC
#endif

-- concatUnfold

-- concatUnfold replicate/unfoldrM

{-# INLINE concatUnfoldRepl #-}
concatUnfoldRepl :: Int -> Int -> Int -> IO ()
concatUnfoldRepl outer inner n =
    S.drain
         $ S.concatUnfold
               (UF.lmap return (UF.replicateM inner))
               (sourceUnfoldrM outer n)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'concatUnfoldRepl
inspect $ 'concatUnfoldRepl `hasNoType` ''D.ConcatMapUState
inspect $ 'concatUnfoldRepl `hasNoType` ''SPEC
#endif

o_1_space_concat :: Int -> [Benchmark]
o_1_space_concat value = sqrtVal `seq`
    [ bgroup "concat"
        [ benchIOSrc1 "concatMapPure (n of 1)"
            (concatMapPure value 1)
        , benchIOSrc1 "concatMapPure (sqrt n of sqrt n)"
            (concatMapPure sqrtVal sqrtVal)
        , benchIOSrc1 "concatMapPure (1 of n)"
            (concatMapPure 1 value)

        -- This is for comparison with foldMapWith
        , benchIOSrc serially "concatMapId (n of 1) (fromFoldable)"
            (S.concatMap id . sourceConcatMapId value)

        , benchIOSrc1 "concatMap (n of 1)"
            (concatMap value 1)
        , benchIOSrc1 "concatMap (sqrt n of sqrt n)"
            (concatMap sqrtVal sqrtVal)
        , benchIOSrc1 "concatMap (1 of n)"
            (concatMap 1 value)

        -- This is for comparison with foldMapWith
        , benchIOSrc serially "concatMapWithId (n of 1) (fromFoldable)"
            (S.concatMapWith serial id . sourceConcatMapId value)

        , benchIOSrc1 "concatMapWith (n of 1)"
            (concatMapWithSerial value 1)
        , benchIOSrc1 "concatMapWith (sqrt n of sqrt n)"
            (concatMapWithSerial sqrtVal sqrtVal)
        , benchIOSrc1 "concatMapWith (1 of n)"
            (concatMapWithSerial 1 value)

        -- quadratic with number of outer streams
        , benchIOSrc1 "concatMapWithAppend (2 of n/2)"
            (concatMapWithAppend 2 (value `div` 2))

        -- concatMap vs concatUnfold
        , benchIOSrc1 "concatMapRepl (sqrt n of sqrt n)"
            (concatMapRepl sqrtVal sqrtVal)
        , benchIOSrc1 "concatUnfoldRepl (sqrt n of sqrt n)"
            (concatUnfoldRepl sqrtVal sqrtVal)
        ]
    ]

    where

    sqrtVal = round $ sqrt (fromIntegral value :: Double)

-------------------------------------------------------------------------------
-- Applicative
-------------------------------------------------------------------------------

o_1_space_applicative :: Int -> [Benchmark]
o_1_space_applicative value =
    [ bgroup "Applicative"
        [ benchIO "(*>) (sqrt n x sqrt n)" $ apDiscardFst value serially
        , benchIO "(<*) (sqrt n x sqrt n)" $ apDiscardSnd value serially
        , benchIO "(<*>) (sqrt n x sqrt n)" $ toNullAp value serially
        , benchIO "liftA2 (sqrt n x sqrt n)" $ apLiftA2 value serially
        ]
    ]

o_n_space_applicative :: Int -> [Benchmark]
o_n_space_applicative value =
    [ bgroup "Applicative"
        [ benchIOSrc serially "(*>) (n times)" $
            iterateSingleton ((*>) . pure) value
        , benchIOSrc serially "(<*) (n times)" $
            iterateSingleton (\x xs -> xs <* pure x) value
        , benchIOSrc serially "(<*>) (n times)" $
            iterateSingleton (\x xs -> (P.fmap (+) (pure x) <*> xs)) value
        , benchIOSrc serially "liftA2 (n times)" $
            iterateSingleton (\x xs -> AP.liftA2 (+) (pure x) xs) value
        ]
    ]

-------------------------------------------------------------------------------
-- Monad
-------------------------------------------------------------------------------

o_1_space_monad :: Int -> [Benchmark]
o_1_space_monad value =
    [ bgroup "Monad"
        [ benchIO "(>>) (sqrt n x sqrt n)" $ monadThen value serially
        , benchIO "(>>=) (sqrt n x sqrt n)" $ toNullM value serially
        , benchIO "(>>=) (sqrt n x sqrt n) (filterAllOut)" $
            filterAllOutM value serially
        , benchIO "(>>=) (sqrt n x sqrt n) (filterAllIn)" $
            filterAllInM value serially
        , benchIO "(>>=) (sqrt n x sqrt n) (filterSome)" $
            filterSome value serially
        , benchIO "(>>=) (sqrt n x sqrt n) (breakAfterSome)" $
            breakAfterSome value serially
        , benchIO "(>>=) (cubert n x cubert n x cubert n)" $
            toNullM3 value serially
        ]
    ]

-- This is a good benchmark but inefficient way to compute primes. As we see a
-- new prime we keep appending a division filter for all the future numbers.
{-# INLINE sieve #-}
sieve :: Monad m => SerialT m Int -> SerialT m Int
sieve s = do
    r <- lift $ S.uncons s
    case r of
        Just (prime, rest) ->
            prime `S.cons` sieve (S.filter (\n -> n `mod` prime /= 0) rest)
        Nothing -> S.nil

o_n_space_monad :: Int -> [Benchmark]
o_n_space_monad value =
    [ bgroup "Monad"
        [ benchIOSrc serially "(>>) (n times)" $
            iterateSingleton ((>>) . pure) value
        , benchIOSrc serially "(>>=) (n times)" $
            iterateSingleton (\x xs -> xs >>= \y -> return (x + y)) value
        , benchIO "(>>=) (sqrt n x sqrt n) (toList)" $
            toListM value serially
        , benchIO "(>>=) (sqrt n x sqrt n) (toListSome)" $
            toListSome value serially
        , benchIO "naive prime sieve (n/4)"
            (\n -> S.sum $ sieve $ S.enumerateFromTo 2 (value `div` 4 + n))
        ]
    ]

-------------------------------------------------------------------------------
-- Monad transformation (hoisting etc.)
-------------------------------------------------------------------------------

{-# INLINE sourceUnfoldrState #-}
sourceUnfoldrState :: (S.IsStream t, S.MonadAsync m)
                   => Int -> Int -> t (StateT Int m) Int
sourceUnfoldrState value n = S.unfoldrM step n
    where
    step cnt =
        if cnt > n + value
        then return Nothing
        else do
            s <- get
            put (s + 1)
            return (Just (s, cnt + 1))

{-# INLINE evalStateT #-}
evalStateT :: S.MonadAsync m => Int -> Int -> SerialT m Int
evalStateT value n =
    Internal.evalStateT (return 0) (sourceUnfoldrState value n)

{-# INLINE withState #-}
withState :: S.MonadAsync m => Int -> Int -> SerialT m Int
withState value n =
    Internal.evalStateT
        (return (0 :: Int)) (Internal.liftInner (sourceUnfoldrM value n))

o_1_space_hoisting :: Int -> [Benchmark]
o_1_space_hoisting value =
    [ bgroup "hoisting"
        [ benchIOSrc serially "evalState" (evalStateT value)
        , benchIOSrc serially "withState" (withState value)
        ]
    ]

{-# INLINE iterateStateIO #-}
iterateStateIO ::
       (S.MonadAsync m)
    => Int
    -> StateT Int m Int
iterateStateIO n = do
    x <- get
    if x > n
    then do
        put (x - 1)
        iterateStateIO n
    else return x

{-# INLINE iterateStateT #-}
iterateStateT :: Int -> SerialT (StateT Int IO) Int
iterateStateT n = do
    x <- lift get
    if x > n
    then do
        lift $ put (x - 1)
        iterateStateT n
    else return x

{-# INLINE iterateState #-}
iterateState ::
       (S.MonadAsync m, MonadState Int m)
    => Int
    -> SerialT m Int
iterateState n = do
    x <- get
    if x > n
    then do
        put (x - 1)
        iterateState n
    else return x

o_n_heap_transformer :: Int -> [Benchmark]
o_n_heap_transformer value =
    [ bgroup "transformer"
        [ benchIO "StateT Int IO (n times) (baseline)" $ \n ->
            State.evalStateT (iterateStateIO n) value
        , benchIO "SerialT (StateT Int IO) (n times)" $ \n ->
            State.evalStateT (S.drain (iterateStateT n)) value
        , benchIO "MonadState Int m => SerialT m Int" $ \n ->
            State.evalStateT (S.drain (iterateState n)) value
        ]
    ]


-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

-- In addition to gauge options, the number of elements in the stream can be
-- passed using the --stream-size option.
--
main :: IO ()
main = do
    (value, cfg, benches) <- parseCLIOpts defaultStreamSize
    value `seq` runMode (mode cfg) cfg benches (allBenchmarks value)

    where

    allBenchmarks size =
        [ bgroup (o_1_space_prefix moduleName) $ Prelude.concat
            [ o_1_space_generation size

            -- elimination
            , o_1_space_elimination_foldable size
            , o_1_space_elimination_folds size
            , o_1_space_elimination_multi_stream_pure size
            , o_1_space_elimination_multi_stream size

            -- transformation
            , o_1_space_functor size
            , o_1_space_mapping size
            , o_1_space_mappingX4 size
            , o_1_space_filtering size
            , o_1_space_filteringX4 size
            , o_1_space_inserting size
            , o_1_space_insertingX4 size
            , o_1_space_transformations_mixed size
            , o_1_space_transformations_mixedX4 size
            , o_1_space_indexing size
            , o_1_space_indexingX4 size

            -- pipes
            , o_1_space_pipes size
            , o_1_space_pipesX4 size

            -- multi-stream
            , o_1_space_joining size
            , o_1_space_concatFoldable size
            , o_1_space_concat size

            , o_1_space_applicative size
            , o_1_space_monad size

            -- Monad transformation
            , o_1_space_hoisting size
            ]
        , bgroup (o_n_stack_prefix moduleName) (o_n_stack_iterated size)
        , bgroup (o_n_heap_prefix moduleName) $ Prelude.concat
            [ o_n_heap_generation size
            , o_n_heap_elimination_foldl size
            , o_n_heap_elimination_toList size
            , o_n_heap_elimination_buffered size

            -- transformation
            , o_n_heap_reordering size
            , o_n_heap_transformer size
            ]
        , bgroup (o_n_space_prefix moduleName) $ Prelude.concat
            [ o_n_space_elimination_foldable size
            , o_n_space_elimination_toList size
            , o_n_space_elimination_foldr size

            -- transformation
            , o_n_space_traversable size
            , o_n_space_mapping size
            , o_n_space_grouping size

            -- multi-stream
            , o_n_space_functor size
            , o_n_space_applicative size
            , o_n_space_monad size
            ]
        ]
