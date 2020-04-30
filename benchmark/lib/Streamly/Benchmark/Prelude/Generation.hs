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
{-# LANGUAGE TupleSections #-}

#ifdef __HADDOCK_VERSION__
#undef INSPECTION
#endif

#ifdef INSPECTION
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fplugin Test.Inspection.Plugin #-}
#endif

module Streamly.Benchmark.Prelude.Generation
    ( o_1_space_serial_generation
    , o_1_space_serial_joining
    , o_1_space_serial_concatFoldable
    , o_1_space_serial_concatSerial
    , o_1_space_serial_outerProductStreams

    , o_1_space_wSerial_concatMap
    , o_1_space_wSerial_outerProduct

    , o_n_space_serial_outerProductStreams

    , o_n_space_wSerial_outerProductStreams

    , o_n_heap_serial_buffering

    , o_1_space_async_generation
    , o_1_space_async_concatFoldable
    , o_1_space_async_concatMap

    , o_1_space_wAsync_generation
    , o_1_space_wAsync_concatFoldable
    , o_1_space_wAsync_concatMap

    , o_1_space_ahead_generation
    , o_1_space_ahead_concatFoldable
    , o_1_space_ahead_concatMap

    , o_1_space_async_zip

    , o_n_space_parallel_generation
    , o_n_space_parallel_concatFoldable
    , o_n_space_parallel_concatMap
    , o_n_space_parallel_outerProductStreams
    , o_n_space_parallel_outerProductStreams2

    , o_1_space_async_avgRate

    , o_1_space_ahead_avgRate
    ) where


import Control.DeepSeq (NFData(..))
import Data.Functor.Identity (Identity)
import System.Random (randomRIO)
import Prelude
       (Monad, String, Int, (+), ($), (.), return, (>),
        undefined, Maybe(..), (>>=), curry,
        div, IO, compare, Double, fromIntegral, Integer, (<$>),
        (<*>), sqrt, round, (*), seq)
import qualified Prelude as P
import qualified Data.Foldable as F

#ifdef INSPECTION
import GHC.Types (SPEC(..))
import Test.Inspection

import qualified Streamly.Internal.Data.Stream.StreamD as D
#endif

import qualified Streamly as S hiding (runStream)
import qualified Streamly.Prelude  as S
import qualified Streamly.Internal.Prelude as Internal
import qualified Streamly.Internal.Data.Unfold as UF
import Streamly.Internal.Data.Time.Units

import qualified Streamly.Benchmark.Prelude.NestedOps as Nested

import Gauge
import Streamly hiding (runStream)
import Streamly.Benchmark.Common

type Stream m a = S.SerialT m a


-------------------------------------------------------------------------------
-- Stream generation
-------------------------------------------------------------------------------

-- enumerate

{-# INLINE sourceIntFromTo #-}
sourceIntFromTo :: (Monad m, S.IsStream t) => Int -> Int -> t m Int
sourceIntFromTo value n = S.enumerateFromTo n (n + value)

{-# INLINE sourceIntFromThenTo #-}
sourceIntFromThenTo :: (Monad m, S.IsStream t) => Int -> Int -> t m Int
sourceIntFromThenTo value n = S.enumerateFromThenTo n (n + 1) (n + value)

{-# INLINE sourceFracFromTo #-}
sourceFracFromTo :: (Monad m, S.IsStream t) => Int -> Int -> t m Double
sourceFracFromTo value n =
    S.enumerateFromTo (fromIntegral n) (fromIntegral (n + value))

{-# INLINE sourceFracFromThenTo #-}
sourceFracFromThenTo :: (Monad m, S.IsStream t) => Int -> Int -> t m Double
sourceFracFromThenTo value n = S.enumerateFromThenTo (fromIntegral n)
    (fromIntegral n + 1.0001) (fromIntegral (n + value))

{-# INLINE sourceIntegerFromStep #-}
sourceIntegerFromStep :: (Monad m, S.IsStream t) => Int -> Int -> t m Integer
sourceIntegerFromStep value n =
    S.take value $ S.enumerateFromThen (fromIntegral n) (fromIntegral n + 1)

-- unfoldr

{-# INLINE sourceUnfoldr #-}
sourceUnfoldr :: (Monad m, S.IsStream t) => Int -> Int -> t m Int
sourceUnfoldr value n = S.unfoldr step n
    where
    step cnt =
        if cnt > n + value
        then Nothing
        else Just (cnt, cnt + 1)

{-# INLINE sourceUnfoldrN #-}
sourceUnfoldrN :: (Monad m, S.IsStream t) => Int -> Int -> t m Int
sourceUnfoldrN upto start = S.unfoldr step start
    where
    step cnt =
        if cnt > start + upto
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

{-# INLINE sourceUnfoldrMN #-}
sourceUnfoldrMN :: (S.IsStream t, S.MonadAsync m) => Int -> Int -> t m Int
sourceUnfoldrMN upto start = S.unfoldrM step start
    where
    step cnt =
        if cnt > start + upto
        then return Nothing
        else return (Just (cnt, cnt + 1))

-- fromIndices

{-# INLINE _sourceFromIndices #-}
_sourceFromIndices :: (Monad m, S.IsStream t) => Int -> Int -> t m Int
_sourceFromIndices value n = S.take value $ S.fromIndices (+ n)

{-# INLINE _sourceFromIndicesM #-}
_sourceFromIndicesM :: (S.MonadAsync m, S.IsStream t) => Int -> Int -> t m Int
_sourceFromIndicesM value n = S.take value $ S.fromIndicesM (P.fmap return (+ n))

-- fromList

{-# INLINE sourceFromList #-}
sourceFromList :: (Monad m, S.IsStream t) => Int -> Int -> t m Int
sourceFromList value n = S.fromList [n..n+value]

{-# INLINE sourceFromListM #-}
sourceFromListM :: (S.MonadAsync m, S.IsStream t) => Int -> Int -> t m Int
sourceFromListM value n = S.fromListM (P.fmap return [n..n+value])

-- fromFoldable

{-# INLINE sourceFromFoldable #-}
sourceFromFoldable :: S.IsStream t => Int -> Int -> t m Int
sourceFromFoldable value n = S.fromFoldable [n..n+value]

{-# INLINE sourceFromFoldableM #-}
sourceFromFoldableM :: (S.IsStream t, S.MonadAsync m) => Int -> Int -> t m Int
sourceFromFoldableM value n = S.fromFoldableM (P.fmap return [n..n+value])

{-# INLINE absTimes #-}
absTimes :: (S.IsStream t, S.MonadAsync m, P.Functor (t m))
    => Int -> Int -> t m AbsTime
absTimes value _ = S.take value $ Internal.absTimes

-------------------------------------------------------------------------------
-- Elimination
-------------------------------------------------------------------------------

{-# INLINE runStream #-}
runStream :: Monad m => Stream m a -> m ()
runStream = S.drain

{-# INLINE toNull #-}
toNull :: Monad m => (t m a -> S.SerialT m a) -> t m a -> m ()
toNull t = runStream . t

-------------------------------------------------------------------------------
-- Transformation
-------------------------------------------------------------------------------

{-# INLINE transform #-}
transform :: Monad m => Stream m a -> m ()
transform = runStream

-- polymorphic stream version of composeN
{-# INLINE composeN' #-}
composeN' ::
       (S.IsStream t, Monad m)
    => Int
    -> (t m Int -> Stream m Int)
    -> t m Int
    -> m ()
composeN' n f =
    case n of
        1 -> transform . f
        2 -> transform . f . S.adapt . f
        3 -> transform . f . S.adapt . f . S.adapt . f
        4 -> transform . f . S.adapt . f . S.adapt . f . S.adapt . f
        _ -> undefined

{-# INLINE fmap' #-}
fmap' ::
       (S.IsStream t, S.MonadAsync m, P.Functor (t m))
    => (t m Int -> S.SerialT m Int)
    -> Int
    -> t m Int
    -> m ()
fmap' t n = composeN' n $ t . P.fmap (+ 1)

-------------------------------------------------------------------------------
-- Combining streams
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- Appending
-------------------------------------------------------------------------------

{-# INLINE serial2 #-}
serial2 :: Int -> Int -> IO ()
serial2 count n =
    S.drain $ S.serial (sourceUnfoldrMN count n) (sourceUnfoldrMN count (n + 1))

{-# INLINE serial4 #-}
serial4 :: Int -> Int -> IO ()
serial4 count n =
    S.drain $
    S.serial
        ((S.serial (sourceUnfoldrMN count n) (sourceUnfoldrMN count (n + 1))))
        ((S.serial
              (sourceUnfoldrMN count (n + 2))
              (sourceUnfoldrMN count (n + 3))))

{-# INLINE append2 #-}
append2 :: Int -> Int -> IO ()
append2 count n =
    S.drain $
    Internal.append (sourceUnfoldrMN count n) (sourceUnfoldrMN count (n + 1))

{-# INLINE append4 #-}
append4 :: Int -> Int -> IO ()
append4 count n =
    S.drain $
    Internal.append
        ((Internal.append
              (sourceUnfoldrMN count n)
              (sourceUnfoldrMN count (n + 1))))
        ((Internal.append
              (sourceUnfoldrMN count (n + 2))
              (sourceUnfoldrMN count (n + 3))))

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'append2
inspect $ 'append2 `hasNoType` ''SPEC
inspect $ 'append2 `hasNoType` ''D.AppendState
#endif

-------------------------------------------------------------------------------
-- Interleaving
-------------------------------------------------------------------------------

{-# INLINE wSerial2 #-}
wSerial2 :: Int -> Int -> IO ()
wSerial2 value n =
    S.drain $
    S.wSerial
        (sourceUnfoldrMN (value `div` 2) n)
        (sourceUnfoldrMN (value `div` 2) (n + 1))

{-# INLINE interleave2 #-}
interleave2 :: Int -> Int -> IO ()
interleave2 value n =
    S.drain $
    Internal.interleave
        (sourceUnfoldrMN (value `div` 2) n)
        (sourceUnfoldrMN (value `div` 2) (n + 1))

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'interleave2
inspect $ 'interleave2 `hasNoType` ''SPEC
inspect $ 'interleave2 `hasNoType` ''D.InterleaveState
#endif

{-# INLINE roundRobin2 #-}
roundRobin2 :: Int -> Int -> IO ()
roundRobin2 value n =
    S.drain $
    Internal.roundrobin
        (sourceUnfoldrMN (value `div` 2) n)
        (sourceUnfoldrMN (value `div` 2) (n + 1))

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'roundRobin2
inspect $ 'roundRobin2 `hasNoType` ''SPEC
inspect $ 'roundRobin2 `hasNoType` ''D.InterleaveState
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
        (sourceUnfoldrMN count n)
        (sourceUnfoldrMN count (n + 1))

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'mergeBy
inspect $ 'mergeBy `hasNoType` ''SPEC
inspect $ 'mergeBy `hasNoType` ''D.Step
#endif

-------------------------------------------------------------------------------
-- Zipping
-------------------------------------------------------------------------------

{-# INLINE zip #-}
zip :: Int -> Int -> IO ()
zip count n =
    S.drain $
    S.zipWith (,) (sourceUnfoldrMN count n) (sourceUnfoldrMN count (n + 1))

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'zip
inspect $ 'zip `hasNoType` ''SPEC
inspect $ 'zip `hasNoType` ''D.Step
#endif

{-# INLINE zipM #-}
zipM :: Int -> Int -> IO ()
zipM count n =
    S.drain $
    S.zipWithM
        (curry return)
        (sourceUnfoldrMN count n)
        (sourceUnfoldrMN count (n + 1))

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'zipM
inspect $ 'zipM `hasNoType` ''SPEC
inspect $ 'zipM `hasNoType` ''D.Step
#endif

{-# INLINE zipAsync #-}
zipAsync :: (S.IsStream t, S.MonadAsync m) => Int -> Int -> t m (Int, Int)
zipAsync count n = do
    S.zipAsyncWith (,) (sourceUnfoldrMN count n) (sourceUnfoldrMN count (n + 1))

{-# INLINE zipAsyncM #-}
zipAsyncM :: (S.IsStream t, S.MonadAsync m) => Int -> Int -> t m (Int, Int)
zipAsyncM count n = do
    S.zipAsyncWithM
        (curry return)
        (sourceUnfoldrMN count n)
        (sourceUnfoldrMN count (n + 1))

{-# INLINE zipAsyncAp #-}
zipAsyncAp :: (S.IsStream t, S.MonadAsync m) => Int -> Int -> t m (Int, Int)
zipAsyncAp count n = do
    S.zipAsyncly $
        (,) <$> (sourceUnfoldrMN count n) <*> (sourceUnfoldrMN count (n + 1))

{-# INLINE mergeAsyncByM #-}
mergeAsyncByM :: (S.IsStream t, S.MonadAsync m) => Int -> Int -> t m Int
mergeAsyncByM count n = do
    S.mergeAsyncByM
        (\a b -> return (a `compare` b))
        (sourceUnfoldrMN count n)
        (sourceUnfoldrMN count (n + 1))

{-# INLINE mergeAsyncBy #-}
mergeAsyncBy :: (S.IsStream t, S.MonadAsync m) => Int -> Int -> t m Int
mergeAsyncBy count n = do
    S.mergeAsyncBy
        compare
        (sourceUnfoldrMN count n)
        (sourceUnfoldrMN count (n + 1))

-------------------------------------------------------------------------------
-- Streams of streams
-------------------------------------------------------------------------------

-- Special cases of concatMap

{-# INLINE sourceFoldMapWith #-}
sourceFoldMapWith :: (S.IsStream t, S.Semigroup (t m Int))
                  => Int -> Int -> t m Int
sourceFoldMapWith value n = S.foldMapWith (S.<>) S.yield [n..n+value]

{-# INLINE sourceFoldMapWithM #-}
sourceFoldMapWithM :: (S.IsStream t, Monad m, S.Semigroup (t m Int))
                   => Int -> Int -> t m Int
sourceFoldMapWithM value n = S.foldMapWith (S.<>) (S.yieldM . return) [n..n+value]

{-# INLINE sourceFoldMapM #-}
sourceFoldMapM :: (S.IsStream t, Monad m, P.Monoid (t m Int))
               => Int -> Int -> t m Int
sourceFoldMapM value n = F.foldMap (S.yieldM . return) [n..n+value]

{-# INLINE sourceConcatMapId #-}
sourceConcatMapId :: (S.IsStream t, Monad m)
                  => Int -> Int -> t m Int
sourceConcatMapId value n =
    S.concatMap P.id $ S.fromFoldable $ P.map (S.yieldM . return) [n..n+value]

-- concatMap unfoldrM/unfoldrM

{-# INLINE concatMap #-}
concatMap :: Int -> Int -> Int -> IO ()
concatMap outer inner n =
    S.drain $ S.concatMap
        (\_ -> sourceUnfoldrMN inner n)
        (sourceUnfoldrMN outer n)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'concatMap
inspect $ 'concatMap `hasNoType` ''SPEC
#endif

-- concatMap unfoldr/unfoldr

{-# INLINE concatMapPure #-}
concatMapPure :: Int -> Int -> Int -> IO ()
concatMapPure outer inner n =
    S.drain $ S.concatMap
        (\_ -> sourceUnfoldrN inner n)
        (sourceUnfoldrN outer n)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'concatMapPure
inspect $ 'concatMapPure `hasNoType` ''SPEC
#endif

-- concatMap replicate/unfoldrM

{-# INLINE concatMapRepl4xN #-}
concatMapRepl4xN :: Int -> Int -> IO ()
concatMapRepl4xN value n = S.drain $ S.concatMap (S.replicate 4)
                          (sourceUnfoldrMN (value `div` 4) n)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'concatMapRepl4xN
inspect $ 'concatMapRepl4xN `hasNoType` ''SPEC
#endif

-- concatMapWith

{-# INLINE concatStreamsWith #-}
concatStreamsWith
    :: (forall c. S.SerialT IO c -> S.SerialT IO c -> S.SerialT IO c)
    -> Int
    -> Int
    -> Int
    -> IO ()
concatStreamsWith op outer inner n =
    S.drain $ S.concatMapWith op
        (\i -> sourceUnfoldrMN inner i)
        (sourceUnfoldrMN outer n)

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

{-# INLINE concatMapWithWSerial #-}
concatMapWithWSerial :: Int -> Int -> Int -> IO ()
concatMapWithWSerial = concatStreamsWith S.wSerial

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'concatMapWithWSerial
inspect $ 'concatMapWithSerial `hasNoType` ''SPEC
#endif

-- concatUnfold

-- concatUnfold replicate/unfoldrM

{-# INLINE concatUnfoldRepl4xN #-}
concatUnfoldRepl4xN :: Int -> Int -> IO ()
concatUnfoldRepl4xN value n =
    S.drain $ S.concatUnfold
        (UF.replicateM 4)
        (sourceUnfoldrMN (value `div` 4) n)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'concatUnfoldRepl4xN
inspect $ 'concatUnfoldRepl4xN `hasNoType` ''D.ConcatMapUState
inspect $ 'concatUnfoldRepl4xN `hasNoType` ''SPEC
#endif

{-# INLINE concatUnfoldInterleaveRepl4xN #-}
concatUnfoldInterleaveRepl4xN :: Int -> Int -> IO ()
concatUnfoldInterleaveRepl4xN value n =
    S.drain $ Internal.concatUnfoldInterleave
        (UF.replicateM 4)
        (sourceUnfoldrMN (value `div` 4) n)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'concatUnfoldInterleaveRepl4xN
-- inspect $ 'concatUnfoldInterleaveRepl4xN `hasNoType` ''SPEC
-- inspect $ 'concatUnfoldInterleaveRepl4xN `hasNoType` ''D.ConcatUnfoldInterleaveState
#endif

{-# INLINE concatUnfoldRoundrobinRepl4xN #-}
concatUnfoldRoundrobinRepl4xN :: Int -> Int -> IO ()
concatUnfoldRoundrobinRepl4xN value n =
    S.drain $ Internal.concatUnfoldRoundrobin
        (UF.replicateM 4)
        (sourceUnfoldrMN (value `div` 4) n)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'concatUnfoldRoundrobinRepl4xN
-- inspect $ 'concatUnfoldRoundrobinRepl4xN `hasNoType` ''SPEC
-- inspect $ 'concatUnfoldRoundrobinRepl4xN `hasNoType` ''D.ConcatUnfoldInterleaveState
#endif

-------------------------------------------------------------------------------
-- Concurrent application/fold
-------------------------------------------------------------------------------

{-# INLINE parAppMap #-}
parAppMap :: S.MonadAsync m => Stream m Int -> m ()
parAppMap src = S.drain $ S.map (+1) S.|$ src

{-# INLINE parAppSum #-}
parAppSum :: S.MonadAsync m => Stream m Int -> m ()
parAppSum src = (S.sum S.|$. src) >>= \x -> P.seq x (return ())

-------------------------------------------------------------------------------
-- Type class instances
-------------------------------------------------------------------------------

{-# INLINE showInstanceList #-}
showInstanceList :: [Int] -> P.String
showInstanceList src = P.show src

{-# INLINE readInstance #-}
readInstance :: P.String -> Stream Identity Int
readInstance str =
    let r = P.reads str
    in case r of
        [(x,"")] -> x
        _ -> P.error "readInstance: no parse"

{-# INLINE readInstanceList #-}
readInstanceList :: P.String -> [Int]
readInstanceList str =
    let r = P.reads str
    in case r of
        [(x,"")] -> x
        _ -> P.error "readInstance: no parse"

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

-- | Takes a source, and uses it with a default drain/fold method.
{-# INLINE benchIOSrc #-}
benchIOSrc
    :: (t IO a -> SerialT IO a)
    -> String
    -> (Int -> t IO a)
    -> Benchmark
benchIOSrc t name f =
    bench name $ nfIO $ randomRIO (1,1) >>= toNull t . f

{-# INLINE benchIO #-}
benchIO :: (NFData b) => String -> (Int -> IO b) -> Benchmark
benchIO name f = bench name $ nfIO $ randomRIO (1,1) >>= f

-- | Takes a source, and uses it with a default drain/fold method.
{-# INLINE benchSrcIO #-}
benchSrcIO
    :: (t IO a -> SerialT IO a)
    -> String
    -> (Int -> t IO a)
    -> Benchmark
benchSrcIO t name f
    = bench name $ nfIO $ randomRIO (1,1) >>= toNull t . f

{-# INLINE benchMonadicSrcIO #-}
benchMonadicSrcIO :: String -> (Int -> IO ()) -> Benchmark
benchMonadicSrcIO name f = bench name $ nfIO $ randomRIO (1,1) >>= f

-------------------------------------------------------------------------------
-- Benchmark groups
-------------------------------------------------------------------------------

o_1_space_serial_generation :: Int -> [Benchmark]
o_1_space_serial_generation value =
    [ bgroup
          "serially"
          [ bgroup
                "generation"
              -- Most basic, barely stream continuations running
                [ benchIOSrc serially "unfoldr" (sourceUnfoldr value)
                , benchIOSrc serially "unfoldrM" (sourceUnfoldrM value)
                , benchIOSrc serially "intFromTo" (sourceIntFromTo value)
                , benchIOSrc
                      serially
                      "intFromThenTo"
                      (sourceIntFromThenTo value)
                , benchIOSrc
                      serially
                      "integerFromStep"
                      (sourceIntegerFromStep value)
                , benchIOSrc
                      serially
                      "fracFromThenTo"
                      (sourceFracFromThenTo value)
                , benchIOSrc serially "fracFromTo" (sourceFracFromTo value)
                , benchIOSrc serially "fromList" (sourceFromList value)
                , benchIOSrc serially "fromListM" (sourceFromListM value)
            -- These are essentially cons and consM
                , benchIOSrc
                      serially
                      "fromFoldable"
                      (sourceFromFoldable value)
                , benchIOSrc
                      serially
                      "fromFoldableM"
                      (sourceFromFoldableM value)
                , benchIOSrc serially "absTimes" $ absTimes value
                ]
          ]
    ]



o_1_space_serial_joining :: Int -> [Benchmark]
o_1_space_serial_joining value =
    [ bgroup
          "serially"
          [ bgroup
                "joining"
                [ benchIOSrc1 "zip (2,x/2)" (zip (value `div` 2))
                , benchIOSrc1 "zipM (2,x/2)" (zipM (value `div` 2))
                , benchIOSrc1 "mergeBy (2,x/2)" (mergeBy (value `div` 2))
                , benchIOSrc1 "serial (2,x/2)" (serial2 (value `div` 2))
                , benchIOSrc1 "append (2,x/2)" (append2 (value `div` 2))
                , benchIOSrc1 "serial (2,2,x/4)" (serial4 (value `div` 4))
                , benchIOSrc1 "append (2,2,x/4)" (append4 (value `div` 4))
                , benchIOSrc1 "wSerial (2,x/2)" (wSerial2 value) -- XXX Move this elsewhere?
                , benchIOSrc1 "interleave (2,x/2)" (interleave2 value)
                , benchIOSrc1 "roundRobin (2,x/2)" (roundRobin2 value)
                ]
          ]
    ]


o_1_space_serial_concatFoldable :: Int -> [Benchmark]
o_1_space_serial_concatFoldable value =
    [ bgroup
          "serially"
          [ bgroup
                "concat-foldable"
                [ benchIOSrc
                      serially
                      "foldMapWith"
                      (sourceFoldMapWith value)
                , benchIOSrc
                      serially
                      "foldMapWithM"
                      (sourceFoldMapWithM value)
                , benchIOSrc serially "foldMapM" (sourceFoldMapM value)
                , benchIOSrc
                      serially
                      "foldWithConcatMapId"
                      (sourceConcatMapId value)
                ]
          ]
    ]

o_1_space_serial_concatSerial :: Int -> [Benchmark]
o_1_space_serial_concatSerial value =
    [ bgroup
          "serially"
          [ bgroup
                "concat-serial"
                [ benchIOSrc1
                      "concatMapPure (2,x/2)"
                      (concatMapPure 2 (value `div` 2))
                , benchIOSrc1
                      "concatMap (2,x/2)"
                      (concatMap 2 (value `div` 2))
                , benchIOSrc1
                      "concatMap (x/2,2)"
                      (concatMap (value `div` 2) 2)
                , benchIOSrc1
                      "concatMapRepl (x/4,4)"
                      (concatMapRepl4xN value)
                , benchIOSrc1
                      "concatUnfoldRepl (x/4,4)"
                      (concatUnfoldRepl4xN value)
                , benchIOSrc1
                      "concatMapWithSerial (2,x/2)"
                      (concatMapWithSerial 2 (value `div` 2))
                , benchIOSrc1
                      "concatMapWithSerial (x/2,2)"
                      (concatMapWithSerial (value `div` 2) 2)
                , benchIOSrc1
                      "concatMapWithAppend (2,x/2)"
                      (concatMapWithAppend 2 (value `div` 2))
                ]
          ]
    ]

o_1_space_serial_outerProductStreams :: Int -> [Benchmark]
o_1_space_serial_outerProductStreams value =
    [ bgroup
          "serially"
          [ bgroup
                "outer-product-streams"
                [ benchIO "toNullAp" $ Nested.toNullAp value serially
                , benchIO "toNull" $ Nested.toNull value serially
                , benchIO "toNull3" $ Nested.toNull3 value serially
                , benchIO "filterAllOut" $ Nested.filterAllOut value serially
                , benchIO "filterAllIn" $ Nested.filterAllIn value serially
                , benchIO "filterSome" $ Nested.filterSome value serially
                , benchIO "breakAfterSome" $
                  Nested.breakAfterSome value serially
                ]
          ]
    ]


o_1_space_wSerial_concatMap :: Int -> [Benchmark]
o_1_space_wSerial_concatMap value =
    [ bgroup
          "wSerially"
          [ bgroup
                "concatMap"
                [ benchIOSrc1
                      "concatMapWithWSerial (2,x/2)"
                      (concatMapWithWSerial 2 (value `div` 2))
                , benchIOSrc1
                      "concatMapWithWSerial (x/2,2)"
                      (concatMapWithWSerial (value `div` 2) 2)
                ]
          ]
    ]

o_1_space_wSerial_outerProduct :: Int -> [Benchmark]
o_1_space_wSerial_outerProduct value =
    [ bgroup
          "wSerially"
          [ bgroup
                "outer-product"
                [ benchIO "toNullAp" $ Nested.toNullAp value wSerially
                , benchIO "toNull" $ Nested.toNull value wSerially
                , benchIO "toNull3" $ Nested.toNull3 value wSerially
                , benchIO "filterAllOut" $ Nested.filterAllOut value wSerially
                , benchIO "filterAllIn" $ Nested.filterAllIn value wSerially
                , benchIO "filterSome" $ Nested.filterSome value wSerially
                , benchIO "breakAfterSome" $
                  Nested.breakAfterSome value wSerially
                ]
          ]
    ]


o_n_space_serial_outerProductStreams :: Int -> [Benchmark]
o_n_space_serial_outerProductStreams value =
    [ bgroup
          "serially"
          [ bgroup
                "outer-product-streams"
                [ benchIO "toList" $ Nested.toList value serially
                , benchIO "toListSome" $ Nested.toListSome value serially
                ]
          ]
    ]

o_n_space_wSerial_outerProductStreams :: Int -> [Benchmark]
o_n_space_wSerial_outerProductStreams value =
    [ bgroup
          "wSerially"
          [ bgroup
                "outer-product-streams"
                [ benchIO "toList" $ Nested.toList value wSerially
                , benchIO "toListSome" $ Nested.toListSome value wSerially
                ]
          ]
    ]


o_n_heap_serial_buffering :: Int -> [Benchmark]
o_n_heap_serial_buffering value =
    [ bgroup
          "serially"
          [ bgroup
                "buffering"
            -- Buffers the output of show/read.
            -- XXX can the outputs be streaming? Can we have special read/show
            -- style type classes, readM/showM supporting streaming effects?
                [ bench "readsPrec pure streams" $
                  nf readInstance (mkString value)
                , bench "readsPrec Haskell lists" $
                  nf readInstanceList (mkListString value)
                , bench "showPrec Haskell lists" $
                  nf showInstanceList (mkList value)
          -- interleave x/4 streams of 4 elements each. Needs to buffer
          -- proportional to x/4. This is different from WSerial because
          -- WSerial expands slowly because of binary interleave behavior and
          -- this expands immediately because of Nary interleave behavior.
                , benchIOSrc1
                      "concatUnfoldInterleaveRepl (x/4,4)"
                      (concatUnfoldInterleaveRepl4xN value)
                , benchIOSrc1
                      "concatUnfoldRoundrobinRepl (x/4,4)"
                      (concatUnfoldRoundrobinRepl4xN value)
                ]
          ]
    ]


o_1_space_async_generation :: Int -> [Benchmark]
o_1_space_async_generation value =
    [ bgroup
          "asyncly"
          [ bgroup
                "generation"
                [ benchSrcIO asyncly "unfoldr" (sourceUnfoldr value)
                , benchSrcIO asyncly "unfoldrM" (sourceUnfoldrM value)
                , benchSrcIO asyncly "fromFoldable" (sourceFromFoldable value)
                , benchSrcIO asyncly "fromFoldableM" (sourceFromFoldableM value)
                , benchSrcIO
                      asyncly
                      "unfoldrM maxThreads 1"
                      (maxThreads 1 . sourceUnfoldrM value)
                , benchSrcIO
                      asyncly
                      "unfoldrM maxBuffer 1 (x/10 ops)"
                      (maxBuffer 1 . sourceUnfoldrMN (value `div` 10))
                ]
          ]
    ]


o_1_space_async_concatFoldable :: Int -> [Benchmark]
o_1_space_async_concatFoldable value =
    [ bgroup
          "asyncly"
          [ bgroup
                "concat-foldable"
                [ benchSrcIO asyncly "foldMapWith" (sourceFoldMapWith value)
                , benchSrcIO
                      asyncly
                      "foldMapWithM"
                      (sourceFoldMapWithM value)
                , benchSrcIO asyncly "foldMapM" (sourceFoldMapM value)
                ]
          ]
    ]

o_1_space_async_concatMap :: Int -> [Benchmark]
o_1_space_async_concatMap value =
    value2 `seq`
    [ bgroup
          "asyncly"
          [ bgroup
                "concatMap"
                [ benchMonadicSrcIO
                      "concatMapWith (2,x/2)"
                      (concatStreamsWith async 2 (value `div` 2))
                , benchMonadicSrcIO
                      "concatMapWith (sqrt x,sqrt x)"
                      (concatStreamsWith async value2 value2)
                , benchMonadicSrcIO
                      "concatMapWith (sqrt x * 2,sqrt x / 2)"
                      (concatStreamsWith async (value2 * 2) (value2 `div` 2))
                ]
          ]
    ]
  where
    value2 = round $ sqrt $ (fromIntegral value :: Double)


o_1_space_wAsync_generation :: Int -> [Benchmark]
o_1_space_wAsync_generation value =
    [ bgroup
          "wAsyncly"
          [ bgroup
                "generation"
                [ benchSrcIO wAsyncly "unfoldr" (sourceUnfoldr value)
                , benchSrcIO wAsyncly "unfoldrM" (sourceUnfoldrM value)
                , benchSrcIO wAsyncly "fromFoldable" (sourceFromFoldable value)
                , benchSrcIO
                      wAsyncly
                      "fromFoldableM"
                      (sourceFromFoldableM value)
                , benchSrcIO
                      wAsyncly
                      "unfoldrM maxThreads 1"
                      (maxThreads 1 . sourceUnfoldrM value)
                , benchSrcIO
                      wAsyncly
                      "unfoldrM maxBuffer 1 (x/10 ops)"
                      (maxBuffer 1 . sourceUnfoldrMN (value `div` 10))
                ]
          ]
    ]


o_1_space_wAsync_concatFoldable :: Int -> [Benchmark]
o_1_space_wAsync_concatFoldable value =
    [ bgroup
          "wAsyncly"
          [ bgroup
                "concat-foldable"
                [ benchSrcIO wAsyncly "foldMapWith" (sourceFoldMapWith value)
                , benchSrcIO wAsyncly "foldMapWithM" (sourceFoldMapWithM value)
                , benchSrcIO wAsyncly "foldMapM" (sourceFoldMapM value)
                ]
          ]
    ]


-- When we merge streams using wAsync the size of the queue increases
-- slowly because of the binary composition adding just one more item
-- to the work queue only after every scheduling pass through the
-- work queue.
--
-- We should see the memory consumption increasing slowly if these
-- benchmarks are left to run on infinite number of streams of infinite
-- sizes.
o_1_space_wAsync_concatMap :: Int -> [Benchmark]
o_1_space_wAsync_concatMap value =
    value2 `seq`
    [ bgroup
          "wAsyncly"
          [ benchMonadicSrcIO
                "concatMapWith (2,x/2)"
                (concatStreamsWith wAsync 2 (value `div` 2))
          , benchMonadicSrcIO
                "concatMapWith (sqrt x,sqrt x)"
                (concatStreamsWith wAsync value2 value2)
          , benchMonadicSrcIO
                "concatMapWith (sqrt x * 2,sqrt x / 2)"
                (concatStreamsWith wAsync (value2 * 2) (value2 `div` 2))
          ]
    ]
  where
    value2 = round $ sqrt $ (fromIntegral value :: Double)


-- unfoldr and fromFoldable are always serial and therefore the same for
-- all stream types. They can be removed to reduce the number of benchmarks.
o_1_space_ahead_generation :: Int -> [Benchmark]
o_1_space_ahead_generation value =
    [ bgroup
          "aheadly"
          [ bgroup
                "generation"
                [ benchSrcIO aheadly "unfoldr" (sourceUnfoldr value)
                , benchSrcIO aheadly "unfoldrM" (sourceUnfoldrM value)
--                , benchSrcIO aheadly "fromFoldable" (sourceFromFoldable value)
                , benchSrcIO
                      aheadly
                      "fromFoldableM"
                      (sourceFromFoldableM value)
                , benchSrcIO
                      aheadly
                      "unfoldrM maxThreads 1"
                      (maxThreads 1 . sourceUnfoldrM value)
                , benchSrcIO
                      aheadly
                      "unfoldrM maxBuffer 1 (x/10 ops)"
                      (maxBuffer 1 . sourceUnfoldrMN (value `div` 10))
                ]
          ]
    ]

o_1_space_ahead_concatFoldable :: Int -> [Benchmark]
o_1_space_ahead_concatFoldable value =
    [ bgroup
          "aheadly"
          [ bgroup
                "concat-foldable"
                [ benchSrcIO aheadly "foldMapWith" (sourceFoldMapWith value)
                , benchSrcIO aheadly "foldMapWithM" (sourceFoldMapWithM value)
                , benchSrcIO aheadly "foldMapM" (sourceFoldMapM value)
                ]
          ]
    ]

o_1_space_ahead_concatMap :: Int -> [Benchmark]
o_1_space_ahead_concatMap value =
    value2 `seq`
    [ bgroup
          "aheadly"
          [ benchMonadicSrcIO
                "concatMapWith (2,x/2)"
                (concatStreamsWith ahead 2 (value `div` 2))
          , benchMonadicSrcIO
                "concatMapWith (sqrt x,sqrt x)"
                (concatStreamsWith ahead value2 value2)
          , benchMonadicSrcIO
                "concatMapWith (sqrt x * 2,sqrt x / 2)"
                (concatStreamsWith ahead (value2 * 2) (value2 `div` 2))
          ]
    ]
  where
    value2 = round $ sqrt $ (fromIntegral value :: Double)


o_1_space_async_zip :: Int -> [Benchmark]
o_1_space_async_zip value =
    [ bgroup
          "asyncly"
          [ bgroup
                "zip"
                [ benchSrcIO
                      serially
                      "zipAsync (2,x/2)"
                      (zipAsync (value `div` 2))
                , benchSrcIO
                      serially
                      "zipAsyncM (2,x/2)"
                      (zipAsyncM (value `div` 2))
                , benchSrcIO
                      serially
                      "zipAsyncAp (2,x/2)"
                      (zipAsyncAp (value `div` 2))
                , benchIOSink value "fmap zipAsyncly" $ fmap' S.zipAsyncly 1
                , benchSrcIO
                      serially
                      "mergeAsyncBy (2,x/2)"
                      (mergeAsyncBy (value `div` 2))
                , benchSrcIO
                      serially
                      "mergeAsyncByM (2,x/2)"
                      (mergeAsyncByM (value `div` 2))
        -- Parallel stages in a pipeline
                , benchIOSink value "parAppMap" parAppMap
                , benchIOSink value "parAppSum" parAppSum
                ]
          ]
    ]

o_n_space_parallel_generation :: Int -> [Benchmark]
o_n_space_parallel_generation value =
    [ bgroup
          "parallely"
          [ bgroup
                "generation"
                [ benchSrcIO parallely "unfoldr" (sourceUnfoldr value)
                , benchSrcIO parallely "unfoldrM" (sourceUnfoldrM value)
--                , benchSrcIO parallely "fromFoldable" (sourceFromFoldable value)
                , benchSrcIO
                      parallely
                      "fromFoldableM"
                      (sourceFromFoldableM value)
                , benchSrcIO
                      parallely
                      "unfoldrM maxThreads 1"
                      (maxThreads 1 . sourceUnfoldrM value)
                , benchSrcIO
                      parallely
                      "unfoldrM maxBuffer 1 (x/10 ops)"
                      (maxBuffer 1 . sourceUnfoldrMN (value `div` 10))
                ]
          ]
    ]

o_n_space_parallel_concatFoldable :: Int -> [Benchmark]
o_n_space_parallel_concatFoldable value =
    [ bgroup
          "parallely"
          [ bgroup
                "concat-foldable"
                [ benchSrcIO parallely "foldMapWith" (sourceFoldMapWith value)
                , benchSrcIO parallely "foldMapWithM" (sourceFoldMapWithM value)
                , benchSrcIO parallely "foldMapM" (sourceFoldMapM value)
                ]
          ]
    ]

o_n_space_parallel_concatMap :: Int -> [Benchmark]
o_n_space_parallel_concatMap value =
    value2 `seq`
    [ bgroup
          "parallely"
          [ benchMonadicSrcIO
                "concatMapWith (2,x/2)"
                (concatStreamsWith parallel 2 (value `div` 2))
          , benchMonadicSrcIO
                "concatMapWith (sqrt x,sqrt x)"
                (concatStreamsWith parallel value2 value2)
          , benchMonadicSrcIO
                "concatMapWith (sqrt x * 2,sqrt x / 2)"
                (concatStreamsWith parallel (value2 * 2) (value2 `div` 2))
          ]
    ]
  where
    value2 = round $ sqrt $ (fromIntegral value :: Double)


o_n_space_parallel_outerProductStreams :: Int -> [Benchmark]
o_n_space_parallel_outerProductStreams value =
    [ bgroup
          "parallely"
          [ bgroup
                "outer-product-streams"
                [ benchIO "toNullAp" $ Nested.toNullAp value parallely
                , benchIO "toNull" $ Nested.toNull value parallely
                , benchIO "toNull3" $ Nested.toNull3 value parallely
                , benchIO "filterAllOut" $ Nested.filterAllOut value parallely
                , benchIO "filterAllIn" $ Nested.filterAllIn value parallely
                , benchIO "filterSome" $ Nested.filterSome value parallely
                , benchIO "breakAfterSome" $
                  Nested.breakAfterSome value parallely
                ]
          ]
    ]

o_n_space_parallel_outerProductStreams2 :: Int -> [Benchmark]
o_n_space_parallel_outerProductStreams2 value =
    [ bgroup
          "parallely"
          [ bgroup
                "outer-product-streams"
                [ benchIO "toList" $ Nested.toList value parallely
                , benchIO "toListSome" $ Nested.toListSome value parallely
                ]
          ]
    ]



-- XXX arbitrarily large rate should be the same as rate Nothing
o_1_space_async_avgRate :: Int -> [Benchmark]
o_1_space_async_avgRate value =
    [ bgroup
          "asyncly"
          [ bgroup
                "avgRate"
          -- benchIO "unfoldr" $ toNull asyncly
          -- benchSrcIO asyncly "unfoldrM" (sourceUnfoldrM value)
                [ benchSrcIO
                      asyncly
                      "unfoldrM/Nothing"
                      (S.rate Nothing . sourceUnfoldrM value)
                , benchSrcIO
                      asyncly
                      "unfoldrM/1,000,000"
                      (S.avgRate 1000000 . sourceUnfoldrM value)
                , benchSrcIO
                      asyncly
                      "unfoldrM/3,000,000"
                      (S.avgRate 3000000 . sourceUnfoldrM value)
                , benchSrcIO
                      asyncly
                      "unfoldrM/10,000,000/maxThreads1"
                      (maxThreads 1 .
                       S.avgRate 10000000 . sourceUnfoldrM value)
                , benchSrcIO
                      asyncly
                      "unfoldrM/10,000,000"
                      (S.avgRate 10000000 . sourceUnfoldrM value)
                , benchSrcIO
                      asyncly
                      "unfoldrM/20,000,000"
                      (S.avgRate 20000000 . sourceUnfoldrM value)
                ]
          ]
    ]

o_1_space_ahead_avgRate :: Int -> [Benchmark]
o_1_space_ahead_avgRate value =
    [ bgroup
          "aheadly"
          [ bgroup
                "avgRate"
                [ benchSrcIO
                      aheadly
                      "unfoldrM/1,000,000"
                      (S.avgRate 1000000 . sourceUnfoldrM value)
                ]
          ]
    ]
