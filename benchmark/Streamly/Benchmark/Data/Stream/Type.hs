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

{-# INLINE sourceFromList #-}
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
{-# INLINE sourceFromTuple #-}
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

o_1_space_generation :: Int -> [Benchmark]
o_1_space_generation value =
    [ bgroup "generation"
        [ benchIO "fromList" $ sourceFromList value
        , benchIO "fromTuple" $ sourceFromTuple value
        ]
    ]

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

o_1_space_elimination_folds :: Int -> [Benchmark]
o_1_space_elimination_folds value =
    [ bgroup "elimination"
        [
            bgroup "reduce"
            [ bgroup
                  "IO"
                  [ benchIO "foldl'" $ foldl'Reduce value
                  , benchIO "foldlM'" $ foldlM'Reduce value
                  ]

            , bgroup
                  "Identity"
                  [ benchIO "foldl'" $ foldl'ReduceIdentity value
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
        , benchIO "foldBreak" $ foldBreak value
        ]
    ]

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
        ]
    ]

{-# INLINE toList' #-}
toList' :: Int -> IO [Int]
toList' value = withStream value S.toList

o_n_space_elimination_toList :: Int -> [Benchmark]
o_n_space_elimination_toList value =
    [ bgroup "toList"
        -- Converting the stream to a list or pure stream in a strict monad
        [ benchIO "toList" $ toList' value
        ]
    ]

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

o_1_space_elimination_multi_stream_pure :: Int -> [Benchmark]
o_1_space_elimination_multi_stream_pure value =
    [ bgroup "multi-stream-pure"
        [ benchIO "eqBy" $ eqByPure value
        , benchIO "cmpBy" $ cmpByPure value
        ]
    ]

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
        ]
    ]

-------------------------------------------------------------------------------
-- Mapping
-------------------------------------------------------------------------------

{-# INLINE mapN #-}
mapN :: Monad m => Int -> Stream m Int -> m ()
mapN n = composeN n $ fmap (+ 1)

{-# INLINE mapM #-}
mapM :: MonadAsync m => Int -> Stream m Int -> m ()
mapM n = composeN n $ Stream.mapM return

{-# INLINE map1 #-}
map1 :: Int -> IO ()
map1 value = withStream value (mapN 1)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'map1
inspect $ 'map1 `hasNoType` ''Stream.Step
inspect $ 'map1 `hasNoType` ''FL.Step
inspect $ 'map1 `hasNoType` ''SPEC
#endif

{-# INLINE mapM1 #-}
mapM1 :: Int -> IO ()
mapM1 value = withStream value (mapM 1)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'mapM1
inspect $ 'mapM1 `hasNoType` ''Stream.Step
inspect $ 'mapM1 `hasNoType` ''FL.Step
inspect $ 'mapM1 `hasNoType` ''SPEC
#endif

{-# INLINE mapN4 #-}
mapN4 :: Int -> IO ()
mapN4 value = withStream value (mapN 4)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'mapN4
inspect $ 'mapN4 `hasNoType` ''Stream.Step
inspect $ 'mapN4 `hasNoType` ''FL.Step
inspect $ 'mapN4 `hasNoType` ''SPEC
#endif

{-# INLINE mapM4 #-}
mapM4 :: Int -> IO ()
mapM4 value = withStream value (mapM 4)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'mapM4
inspect $ 'mapM4 `hasNoType` ''Stream.Step
inspect $ 'mapM4 `hasNoType` ''FL.Step
inspect $ 'mapM4 `hasNoType` ''SPEC
#endif

o_1_space_functor :: Int -> [Benchmark]
o_1_space_functor value =
    [ bgroup "Functor"
        [ benchIO "fmap" $ map1 value
        , benchIO "fmap x 4" $ mapN4 value
        ]
    ]

o_1_space_mapping :: Int -> [Benchmark]
o_1_space_mapping value =
    [ bgroup "mapping"
        [ benchIO "map" $ map1 value
        , benchIO "mapM" $ mapM1 value
        ]
    ]

o_1_space_mappingX4 :: Int -> [Benchmark]
o_1_space_mappingX4 value =
    [ bgroup "mappingX4"
        [ benchIO "map" $ mapN4 value
        , benchIO "mapM" $ mapM4 value
        ]
    ]

-------------------------------------------------------------------------------
-- Filtering
-------------------------------------------------------------------------------

{-# INLINE _takeOne #-}
_takeOne :: MonadIO m => Int -> Stream m Int -> m ()
_takeOne n = composeN n $ Stream.take 1

{-# INLINE takeAll #-}
takeAll :: MonadIO m => Int -> Int -> Stream m Int -> m ()
takeAll value n = composeN n $ Stream.take (value + 1)

{-# INLINE takeAll1 #-}
takeAll1 :: Int -> IO ()
takeAll1 value = withStream value (takeAll value 1)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'takeAll1
inspect $ 'takeAll1 `hasNoType` ''Stream.Step
inspect $ 'takeAll1 `hasNoType` ''FL.Step
inspect $ 'takeAll1 `hasNoType` ''SPEC
#endif

{-# INLINE takeAll4 #-}
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

{-# INLINE takeWhileTrue1 #-}
takeWhileTrue1 :: Int -> IO ()
takeWhileTrue1 value = withStream value (takeWhileTrue value 1)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'takeWhileTrue1
inspect $ 'takeWhileTrue1 `hasNoType` ''Stream.Step
inspect $ 'takeWhileTrue1 `hasNoType` ''FL.Step
inspect $ 'takeWhileTrue1 `hasNoType` ''SPEC
#endif

{-# INLINE takeWhileTrue4 #-}
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

{-# INLINE takeWhileMTrue4 #-}
takeWhileMTrue4 :: Int -> IO ()
takeWhileMTrue4 value = withStream value (takeWhileMTrue value 4)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'takeWhileMTrue4
inspect $ 'takeWhileMTrue4 `hasNoType` ''Stream.Step
inspect $ 'takeWhileMTrue4 `hasNoType` ''FL.Step
inspect $ 'takeWhileMTrue4 `hasNoType` ''SPEC
#endif

o_1_space_filtering :: Int -> [Benchmark]
o_1_space_filtering value =
    [ bgroup "filtering"
        [ -- Trimming
          benchIO "take-all" $ takeAll1 value
        , benchIO "takeWhile-true" $ takeWhileTrue1 value
     -- , benchIO "takeWhileM-true" ...
        ]
    ]

o_1_space_filteringX4 :: Int -> [Benchmark]
o_1_space_filteringX4 value =
    [ bgroup "filteringX4"
        [ -- trimming
          benchIO "take-all" $ takeAll4 value
        , benchIO "takeWhile-true" $ takeWhileTrue4 value
        , benchIO "takeWhileM-true" $ takeWhileMTrue4 value
        ]
    ]

-------------------------------------------------------------------------------
-- Multi-stream
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- Appending
-------------------------------------------------------------------------------

{-# INLINE serial2 #-}
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

{-# INLINE serial4 #-}
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

{-# INLINE zipWith #-}
zipWith :: Int -> IO ()
zipWith value = withRandomIntIO $ \n ->
    let src = sourceUnfoldrM value n
    in drain $ S.zipWith (,) src src

#ifdef INSPECTION
inspect $ 'zipWith `hasNoType` ''SPEC
-- inspect $ 'zipWith `hasNoType` ''S.Step
inspect $ 'zipWith `hasNoType` ''Fold.Step
#endif

{-# INLINE zipWithM #-}
zipWithM :: Int -> IO ()
zipWithM value = withRandomIntIO $ \n ->
    let src = sourceUnfoldrM value n
    in drain $ S.zipWithM (curry return) src src

#ifdef INSPECTION
inspect $ 'zipWithM `hasNoType` ''SPEC
-- inspect $ 'zipWithM `hasNoType` ''S.Step
inspect $ 'zipWithM `hasNoType` ''Fold.Step
#endif

o_1_space_joining :: Int -> [Benchmark]
o_1_space_joining value =
    [ bgroup "joining (2 of n/2)"
        [ benchIO "serial" $ serial2 (value `div` 2)
        , benchIO "serial (2,2,x/4)" $ serial4 (value `div` 4)
        , benchIO "zipWith" $ zipWith value
        , benchIO "zipWithM" $ zipWithM value
        , benchIO "concatMap" $ concatMap 2 (value `div` 2)
        ]
    ]

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

{-# INLINE concatMap #-}
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

{-# INLINE concatMapM2 #-}
concatMapM2 :: Int -> IO ()
concatMapM2 value = withStream value $ \s ->
    drain $ do
        Stream.concatMapM (\x ->
            pure $ Stream.concatMapM (\y ->
                pure $ Stream.fromPure $ x + y) s) s

{-# INLINE concatMapM3 #-}
concatMapM3 :: Int -> IO ()
concatMapM3 value = withStream value $ \s ->
    drain $ do
        Stream.concatMapM (\x ->
            pure $ Stream.concatMapM (\y ->
                pure $ Stream.concatMapM (\z ->
                    pure $ Stream.fromPure $ x + y + z) s) s) s

{-# INLINE concatMapViaUnfoldEach #-}
concatMapViaUnfoldEach :: Int -> Int -> IO ()
concatMapViaUnfoldEach outer inner = withRandomIntIO $ \n ->
    drain $ cmap
        (sourceUnfoldrM inner)
        (sourceUnfoldrM outer n)

    where

    cmap f = Stream.unfoldEach (UF.lmap f UF.fromStream)

{-# INLINE concatMapM #-}
concatMapM :: Int -> Int -> IO ()
concatMapM outer inner = withRandomIntIO $ \n ->
    drain $ S.concatMapM
        (return . sourceUnfoldrM inner)
        (sourceUnfoldrM outer n)

-- concatMap Streams

{-# INLINE concatMapSingletonStreams #-}
concatMapSingletonStreams :: Int -> IO ()
concatMapSingletonStreams value =
    withRandomIntIO (drain . S.concatMap id . sourceConcatMapSingletonStreams value)

{-# INLINE concatMapStreams #-}
concatMapStreams :: Int -> Int -> IO ()
concatMapStreams outer inner =
    withRandomIntIO (S.drain . S.concatMap id . sourceConcatMapStreams outer inner)

-- concatMap unfoldr/unfoldr

{-# INLINE concatMapPure #-}
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

{-# INLINE unfoldEach #-}
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

{-# INLINE unfoldEach2 #-}
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

{-# INLINE unfoldEach3 #-}
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

{-# INLINE unfoldCross #-}
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

o_1_space_concat :: Int -> [Benchmark]
o_1_space_concat value = sqrtVal `seq`
    [ bgroup "concat"
        [ benchIO "concatMap unfoldr outer=Max inner=1" $ concatMapPure value 1
        , benchIO "concatMap unfoldr outer=inner=(sqrt Max)" $ concatMapPure sqrtVal sqrtVal
        , benchIO "concatMap unfoldr outer=1 inner=Max" $ concatMapPure 1 value

        , benchIO "concatMap unfoldrM outer=max inner=1" $ concatMap value 1
        , benchIO "concatMap unfoldrM outer=inner=(sqrt Max)" $ concatMap sqrtVal sqrtVal
        , benchIO "concatMap unfoldrM outer=1 inner=Max" $ concatMap 1 value

        -- Using boxed values/streams may have entirely different perf profile
        , benchIO "concatMap Streams fromPure outer=max inner=1" $
            concatMapSingletonStreams value
        , benchIO "concatMap Streams unfoldr outer=max inner=1" $
            concatMapStreams value 1
        , benchIO "concatMap Streams unfoldr outer=inner=(sqrt Max)" $
            concatMapStreams sqrtVal sqrtVal
        , benchIO "concatMap Streams unfoldr outer=1 inner=Max" $
            concatMapStreams 1 value

        , benchIO "concatMapM unfoldrM outer=max inner=1" $ concatMapM value 1
        , benchIO "concatMapM unfoldrM outer=inner=(sqrt Max)" $ concatMapM sqrtVal sqrtVal
        , benchIO "concatMapM unfoldrM outer=1 inner=Max" $ concatMapM 1 value

        , benchIO "concatMapM2 fromPure" $ concatMapM2 sqrtVal
        , benchIO "concatMapM3 fromPure" $ concatMapM3 cubertVal

        , benchIO "concatMapViaUnfoldEach outer=max inner=1" $ concatMapViaUnfoldEach value 1
        , benchIO "concatMapViaUnfoldEach outer=inner=(sqrt Max)" $ concatMapViaUnfoldEach sqrtVal sqrtVal
        , benchIO "concatMapViaUnfoldEach outer=1 inner=Max" $ concatMapViaUnfoldEach 1 value

        , benchIO "unfoldCross outer=max inner=1" $ unfoldCross value 1
        , benchIO "unfoldCross outer=inner=(sqrt Max)" $ unfoldCross sqrtVal sqrtVal
        , benchIO "unfoldCross outer=1 inner=Max" $ unfoldCross 1 value

        -- concatMap vs unfoldEach
        , benchIO "unfoldEach outer=Max inner=1" $ unfoldEach value 1
        , benchIO "unfoldEach outer=inner=(sqrt Max)" $ unfoldEach sqrtVal sqrtVal
        , benchIO "unfoldEach outer=1 inner=Max" $ unfoldEach 1 value

        , benchIO "unfoldEach2 outer=Max inner=1" $ unfoldEach2 value 1
        , benchIO "unfoldEach2 outer=inner=(sqrt Max)" $ unfoldEach2 sqrtVal sqrtVal
        , benchIO "unfoldEach2 outer=1 inner=Max" $ unfoldEach2 1 value

        , benchIO "unfoldEach3 outer=inner=(cubert Max)" $ unfoldEach3 value
        ]
    ]

    where

    sqrtVal = round $ sqrt (fromIntegral value :: Double)
    cubertVal = round (fromIntegral value**(1/3::Double)) -- triple nested loop

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

{-# INLINE cross2 #-}
cross2 :: Int -> IO ()
cross2 linearCount = withRandomIntIO $ \start -> drain $
    Stream.crossWith (+)
        (sourceUnfoldr nestedCount2 start)
        (sourceUnfoldr nestedCount2 start)

    where

    nestedCount2 = round (fromIntegral linearCount**(1/2::Double))

{-# INLINE crossApply #-}
crossApply :: Int -> IO ()
crossApply linearCount = withRandomIntIO $ \start -> drain $
    Stream.crossApply
        ((+) <$> sourceUnfoldrM nestedCount2 start)
        (sourceUnfoldrM nestedCount2 start)

    where

    nestedCount2 = round (fromIntegral linearCount**(1/2::Double))

{-# INLINE crossApplyFst #-}
crossApplyFst :: Int -> IO ()
crossApplyFst linearCount = withRandomIntIO $ \start -> drain $
    Stream.crossApplyFst
        (sourceUnfoldrM nestedCount2 start)
        (sourceUnfoldrM nestedCount2 start)

    where

    nestedCount2 = round (fromIntegral linearCount**(1/2::Double))

{-# INLINE crossApplySnd #-}
crossApplySnd :: Int -> IO ()
crossApplySnd linearCount = withRandomIntIO $ \start -> drain $
    Stream.crossApplySnd
        (sourceUnfoldrM nestedCount2 start)
        (sourceUnfoldrM nestedCount2 start)

    where

    nestedCount2 = round (fromIntegral linearCount**(1/2::Double))

o_1_space_applicative :: Int -> [Benchmark]
o_1_space_applicative value =
    [ bgroup "Applicative"
        [ benchIO "(*>)" $ withRandomIntIO (apDiscardFst value)
        , benchIO "(<*)" $ withRandomIntIO (apDiscardSnd value)
        , benchIO "(<*>)" $ withRandomIntIO (toNullAp value)
        , benchIO "liftA2" $ withRandomIntIO (apLiftA2 value)
        , benchIO "crossApply" $ crossApply value
        , benchIO "crossApplyFst" $ crossApplyFst value
        , benchIO "crossApplySnd" $ crossApplySnd value
        , benchIO "pureDrain2" $ withRandomIntIO (toNullApPure value)
        , benchIO "pureCross2" $ cross2 value
        ]
    ]

-------------------------------------------------------------------------------
-- Monad
-------------------------------------------------------------------------------

o_1_space_monad :: Int -> [Benchmark]
o_1_space_monad value =
    [ bgroup "Monad"
        [ benchIO "then2" $ withRandomIntIO (monadThen value)
        , benchIO "drain2" $ withRandomIntIO (toNullM value)
        , benchIO "drain3" $ withRandomIntIO (toNullM3 value)
        , benchIO "filterAllOut2" $ withRandomIntIO (filterAllOutM value)
        , benchIO "filterAllIn2" $ withRandomIntIO (filterAllInM value)
        , benchIO "filterSome2" $ withRandomIntIO (filterSome value)
        , benchIO "breakAfterSome2" $ withRandomIntIO (breakAfterSome value)
        , benchIO "pureDrain2" $ withRandomIntIO (toNullMPure value)
        , benchIO "pureDrain3" $ withRandomIntIO (toNullM3Pure value)
        , benchIO "pureFilterAllIn2" $ withRandomIntIO (filterAllInMPure value)
        , benchIO "pureFilterAllOut2" $ withRandomIntIO (filterAllOutMPure value)
        ]
    ]

o_n_space_monad :: Int -> [Benchmark]
o_n_space_monad value =
    [ bgroup "Monad"
        [ benchIO "toList2" $ withRandomIntIO (toListM value)
        , benchIO "toListSome2" $ withRandomIntIO (toListSome value)
        ]
    ]

{-# INLINE drainConcatFor1 #-}
drainConcatFor1 :: Int -> IO ()
drainConcatFor1 count = withStream count $ \s ->
    drain $ Stream.concatFor s $ \x ->
        Stream.fromPure $ x + 1

{-# INLINE drainConcatFor #-}
drainConcatFor :: Int -> IO ()
drainConcatFor count = withStream count $ \s ->
    drain $ do
        Stream.concatFor s $ \x ->
            Stream.concatFor s $ \y ->
                Stream.fromPure $ x + y

{-# INLINE drainConcatForM #-}
drainConcatForM :: Int -> IO ()
drainConcatForM count = withStream count $ \s ->
    drain $ do
        Stream.concatForM s $ \x ->
            pure $ Stream.concatForM s $ \y ->
                pure $ Stream.fromPure $ x + y

{-# INLINE drainConcatFor3 #-}
drainConcatFor3 :: Int -> IO ()
drainConcatFor3 count = withStream count $ \s ->
    drain $ do
        Stream.concatFor s $ \x ->
            Stream.concatFor s $ \y ->
                Stream.concatFor s $ \z ->
                    Stream.fromPure $ x + y + z

{-# INLINE drainConcatFor4 #-}
drainConcatFor4 :: Int -> IO ()
drainConcatFor4 count = withStream count $ \s ->
    drain $ do
        Stream.concatFor s $ \x ->
            Stream.concatFor s $ \y ->
                Stream.concatFor s $ \z ->
                    Stream.concatFor s $ \w ->
                        Stream.fromPure $ x + y + z + w

{-# INLINE drainConcatFor5 #-}
drainConcatFor5 :: Int -> IO ()
drainConcatFor5 count = withStream count $ \s ->
    drain $ do
        Stream.concatFor s $ \x ->
            Stream.concatFor s $ \y ->
                Stream.concatFor s $ \z ->
                    Stream.concatFor s $ \w ->
                        Stream.concatFor s $ \u ->
                            Stream.fromPure $ x + y + z + w + u

{-# INLINE drainConcatFor3M #-}
drainConcatFor3M :: Int -> IO ()
drainConcatFor3M count = withStream count $ \s ->
    drain $ do
        Stream.concatForM s $ \x ->
            pure $ Stream.concatForM s $ \y ->
                pure $ Stream.concatForM s $ \z ->
                    pure $ Stream.fromPure $ x + y + z

{-# INLINE filterAllInConcatFor #-}
filterAllInConcatFor :: Int -> IO ()
filterAllInConcatFor count = withStream count $ \s ->
    drain $ do
        Stream.concatFor s $ \x ->
            Stream.concatFor s $ \y ->
                let s1 = x + y
                 in if s1 > 0
                    then Stream.fromPure s1
                    else Stream.nil

{-# INLINE filterAllOutConcatFor #-}
filterAllOutConcatFor :: Int -> IO ()
filterAllOutConcatFor count = withStream count $ \s ->
    drain $ do
        Stream.concatFor s $ \x ->
            Stream.concatFor s $ \y ->
                let s1 = x + y
                 in if s1 < 0
                    then Stream.fromPure s1
                    else Stream.nil

o_1_space_bind :: Int -> [Benchmark]
o_1_space_bind streamLen =
    [ bgroup "concatFor"
        [ benchIO "drain1" $ drainConcatFor1 streamLen
        , benchIO "drain2" $ drainConcatFor streamLen2
        , benchIO "drain3" $ drainConcatFor3 streamLen3
        , benchIO "drain4" $ drainConcatFor4 streamLen4
        , benchIO "drain5" $ drainConcatFor5 streamLen5
        , benchIO "drainM2" $ drainConcatForM streamLen2
        , benchIO "drainM3" $ drainConcatFor3M streamLen3
        , benchIO "filterAllIn2" $ filterAllInConcatFor streamLen2
        , benchIO "filterAllOut2" $ filterAllOutConcatFor streamLen2
        ]
    ]

    where

    streamLen2 = round (fromIntegral streamLen**(1/2::Double)) -- double nested loop
    streamLen3 = round (fromIntegral streamLen**(1/3::Double)) -- triple nested loop
    streamLen4 = round (fromIntegral streamLen**(1/4::Double)) -- 4 times nested loop
    streamLen5 = round (fromIntegral streamLen**(1/5::Double)) -- 5 times nested loop

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

-- Solve simultaneous equations by exploring all possibilities
o_1_space_equations :: Int -> [Benchmark]
o_1_space_equations value =
    [ bgroup "equations"
        [ benchIO "concatFor (bounded)" $ concatForBounded sqrtVal
        , benchIO "streamCross (bounded)" $ streamCrossBounded sqrtVal
        , benchIO "fairStreamCross (bounded)" $ fairStreamCrossBounded sqrtVal
        , benchIO "fairStreamCross (infinite)" $ fairStreamCrossInfinite sqrtVal
        , benchIO "unfoldEach (bounded)" $ unfoldEachBounded sqrtVal
        ]
    ]

    where

    sqrtVal = round $ sqrt (fromIntegral value :: Double)

-------------------------------------------------------------------------------
-- Fold Many
-------------------------------------------------------------------------------

{-# INLINE foldMany #-}
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

{-# INLINE foldMany1 #-}
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

{-# INLINE refoldMany #-}
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

o_1_space_grouping :: Int -> [Benchmark]
o_1_space_grouping value =
    [ bgroup "grouping"
        [ benchIO "foldMany" $ foldMany value
        , benchIO "foldMany1" $ foldMany1 value
        , benchIO "refoldMany" $ refoldMany value
        , benchIO "refoldIterateM" $ refoldIterateM value
        ]
    ]

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
    map (SpaceO_1,) (o_1_space_generation size)
    -- Elimination
    ++ map (SpaceO_1,) (o_1_space_elimination_folds size)
    ++ map (HeapO_n,) (o_n_heap_elimination_foldl size)
    ++ map (SpaceO_n,) (o_n_space_elimination_foldr size)
    ++ map (SpaceO_n,) (o_n_space_elimination_toList size)
    ++ map (SpaceO_1,) (o_1_space_elimination_multi_stream_pure size)
    ++ map (SpaceO_1,) (o_1_space_elimination_multi_stream size)
    -- Mapping
    ++ map (SpaceO_1,) (o_1_space_functor size)
    ++ map (SpaceO_1,) (o_1_space_mapping size)
    ++ map (SpaceO_1,) (o_1_space_mappingX4 size)
    -- Filtering
    ++ map (SpaceO_1,) (o_1_space_filtering size)
    ++ map (SpaceO_1,) (o_1_space_filteringX4 size)
    -- Multi-stream
    ++ map (SpaceO_1,) (o_1_space_joining size)
    ++ map (SpaceO_1,) (o_1_space_concat size)
    ++ map (SpaceO_1,) (o_1_space_applicative size)
    ++ map (SpaceO_1,) (o_1_space_monad size)
    ++ map (SpaceO_n,) (o_n_space_monad size)
    ++ map (SpaceO_1,) (o_1_space_bind size)
    ++ map (SpaceO_1,) (o_1_space_equations size)
    -- Fold Many
    ++ map (SpaceO_1,) (o_1_space_grouping size)
