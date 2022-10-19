-- |
-- Module      : Stream.Generate
-- Copyright   : (c) 2018 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

#ifdef USE_PRELUDE
{-# OPTIONS_GHC -Wno-deprecations #-}
#endif

module Stream.Generate (benchmarks) where

import Data.Functor.Identity (Identity)

import qualified GHC.Exts as GHC
import qualified Stream.Common as Common
#ifdef USE_PRELUDE
import Streamly.Benchmark.Prelude (sourceFromFoldableM, absTimes)
import qualified Streamly.Prelude as S
import qualified Streamly.Internal.Data.Stream.IsStream as Stream
#else
import qualified Streamly.Internal.Data.Stream as Stream
#endif
import qualified Prelude

import Gauge
import Streamly.Benchmark.Common
import Streamly.Internal.Data.Stream (Stream)
#ifdef USE_PRELUDE
import Streamly.Prelude (MonadAsync)
import Stream.Common hiding (MonadAsync)
#else
import Stream.Common
#endif

import System.IO.Unsafe (unsafeInterleaveIO)

import Prelude hiding (repeat, replicate, iterate)

-------------------------------------------------------------------------------
-- Generation
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- fromList
-------------------------------------------------------------------------------

{-# INLINE sourceFromList #-}
sourceFromList :: Monad m => Int -> Int -> Stream m Int
sourceFromList value n = Stream.fromList [n..n+value]

{-# INLINE sourceFromListM #-}
sourceFromListM :: MonadAsync m => Int -> Int -> Stream m Int
sourceFromListM value n = fromListM (fmap return [n..n+value])

{-# INLINE sourceIsList #-}
sourceIsList :: Int -> Int -> Stream Identity Int
sourceIsList value n = GHC.fromList [n..n+value]

{-# INLINE sourceIsString #-}
sourceIsString :: Int -> Int -> Stream Identity Char
sourceIsString value n = GHC.fromString (Prelude.replicate (n + value) 'a')

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

{-# INLINE repeat #-}
repeat :: Monad m => Int -> Int -> Stream m Int
repeat count = Stream.take count . Stream.repeat

{-# INLINE replicate #-}
replicate :: Monad m => Int -> Int -> Stream m Int
replicate = Stream.replicate

-------------------------------------------------------------------------------
-- enumerate
-------------------------------------------------------------------------------

{-# INLINE sourceIntFromTo #-}
sourceIntFromTo :: Monad m => Int -> Int -> Stream m Int
sourceIntFromTo value n = Stream.enumerateFromTo n (n + value)

{-# INLINE sourceIntFromThenTo #-}
sourceIntFromThenTo :: Monad m => Int -> Int -> Stream m Int
sourceIntFromThenTo value n = Stream.enumerateFromThenTo n (n + 1) (n + value)

{-# INLINE sourceFracFromTo #-}
sourceFracFromTo :: Monad m => Int -> Int -> Stream m Double
sourceFracFromTo value n =
    Stream.enumerateFromTo (fromIntegral n) (fromIntegral (n + value))

{-# INLINE sourceFracFromThenTo #-}
sourceFracFromThenTo :: Monad m => Int -> Int -> Stream m Double
sourceFracFromThenTo value n = Stream.enumerateFromThenTo (fromIntegral n)
    (fromIntegral n + 1.0001) (fromIntegral (n + value))

{-# INLINE sourceIntegerFromStep #-}
sourceIntegerFromStep :: Monad m => Int -> Int -> Stream m Integer
sourceIntegerFromStep value n =
    Stream.take value $ Stream.enumerateFromThen (fromIntegral n) (fromIntegral n + 1)

{-# INLINE enumerateFrom #-}
enumerateFrom :: Monad m => Int -> Int -> Stream m Int
enumerateFrom count = Stream.take count . Stream.enumerateFrom

{-# INLINE enumerateFromTo #-}
enumerateFromTo :: Monad m => Int -> Int -> Stream m Int
enumerateFromTo = sourceIntFromTo

{-# INLINE enumerateFromThen #-}
enumerateFromThen :: Monad m => Int -> Int -> Stream m Int
enumerateFromThen count inp = Stream.take count $ Stream.enumerateFromThen inp (inp + 1)

{-# INLINE enumerateFromThenTo #-}
enumerateFromThenTo :: Monad m => Int -> Int -> Stream m Int
enumerateFromThenTo = sourceIntFromThenTo

-- n ~ 1
{-# INLINE enumerate #-}
enumerate :: Monad m => Int -> Int -> Stream m Int
enumerate count n = Stream.take (count + n) Stream.enumerate

-- n ~ 1
{-# INLINE enumerateTo #-}
enumerateTo :: Monad m => Int -> Int -> Stream m Int
enumerateTo count n = Stream.enumerateTo (minBound + count + n)

{-# INLINE iterate #-}
iterate :: Monad m => Int -> Int -> Stream m Int
iterate count = Stream.take count . Stream.iterate (+1)

{-# INLINE iterateM #-}
iterateM :: MonadAsync m => Int -> Int -> Stream m Int
iterateM count = Stream.take count . Stream.iterateM (return . (+1)) . return

#ifdef USE_PRELUDE
{-# INLINE repeatM #-}
repeatM :: (MonadAsync m, S.IsStream t) => Int -> Int -> t m Int
repeatM count = S.take count . S.repeatM . return

{-# INLINE replicateM #-}
replicateM :: (MonadAsync m, S.IsStream t) => Int -> Int -> t m Int
replicateM count = S.replicateM count . return

{-# INLINE fromIndices #-}
fromIndices :: (Monad m, S.IsStream t) => Int -> Int -> t m Int
fromIndices value n = S.take value $ S.fromIndices (+ n)

{-# INLINE fromIndicesM #-}
fromIndicesM :: (MonadAsync m, S.IsStream t) => Int -> Int -> t m Int
fromIndicesM value n = S.take value $ S.fromIndicesM (return <$> (+ n))
#endif

{-# INLINE mfixUnfold #-}
mfixUnfold :: Int -> Int -> Stream IO (Int, Int)
mfixUnfold count start = Stream.mfix f
    where
    f action = do
        let incr n act = fmap ((+n) . snd)  $ unsafeInterleaveIO act
        x <- Common.fromListM [incr 1 action, incr 2 action]
        y <- Common.sourceUnfoldr count start
        return (x, y)

o_1_space_generation :: Int -> [Benchmark]
o_1_space_generation value =
    [ bgroup "generation"
        [ benchIOSrc "unfoldr" (sourceUnfoldr value)
        , benchIOSrc "unfoldrM" (sourceUnfoldrM value)
        , benchIOSrc "repeat" (repeat value)
        , benchIOSrc "replicate" (replicate value)
        , benchIOSrc "iterate" (iterate value)
        , benchIOSrc "iterateM" (iterateM value)
        , benchIOSrc "intFromTo" (sourceIntFromTo value)
        , benchIOSrc "intFromThenTo" (sourceIntFromThenTo value)
        , benchIOSrc "integerFromStep" (sourceIntegerFromStep value)
        , benchIOSrc "fracFromThenTo" (sourceFracFromThenTo value)
        , benchIOSrc "fracFromTo" (sourceFracFromTo value)
        , benchIOSrc "fromList" (sourceFromList value)
        , benchIOSrc "fromListM" (sourceFromListM value)
        , benchPureSrc "IsList.fromList" (sourceIsList value)
        , benchPureSrc "IsString.fromString" (sourceIsString value)
        , benchIOSrc "enumerateFrom" (enumerateFrom value)
        , benchIOSrc "enumerateFromTo" (enumerateFromTo value)
        , benchIOSrc "enumerateFromThen" (enumerateFromThen value)
        , benchIOSrc "enumerateFromThenTo" (enumerateFromThenTo value)
        , benchIOSrc "enumerate" (enumerate value)
        , benchIOSrc "enumerateTo" (enumerateTo value)
#ifdef USE_PRELUDE
        , benchIOSrc "repeatM" (repeatM value)
        , benchIOSrc "replicateM" (replicateM value)
        , benchIOSrc "fromIndices" (fromIndices value)
        , benchIOSrc "fromIndicesM" (fromIndicesM value)
#endif

          -- These essentially test cons and consM
        , benchIOSrc "fromFoldable" (sourceFromFoldable value)

#ifdef USE_PRELUDE
        , benchIOSrc "fromFoldableM" (sourceFromFoldableM value)
        , benchIOSrc "absTimes" $ absTimes value
#endif
        , Common.benchIOSrc "mfix_10" (mfixUnfold 10)
        , Common.benchIOSrc "mfix_100" (mfixUnfold 100)
        , Common.benchIOSrc "mfix_1000" (mfixUnfold 1000)
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
-- Main
-------------------------------------------------------------------------------

-- In addition to gauge options, the number of elements in the stream can be
-- passed using the --stream-size option.
--
benchmarks :: String -> Int -> [Benchmark]
benchmarks moduleName size =
        [ bgroup (o_1_space_prefix moduleName) (o_1_space_generation size)
        , bgroup (o_n_heap_prefix moduleName) (o_n_heap_generation size)
        ]
