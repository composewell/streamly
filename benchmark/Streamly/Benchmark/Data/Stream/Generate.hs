-- |
-- Module      : Stream.Generate
-- Copyright   : (c) 2018 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

module Stream.Generate (benchmarks) where

import Data.Functor.Identity (Identity)

#ifdef USE_PRELUDE
import qualified GHC.Exts as GHC
import qualified Streamly.Prelude as S
import qualified Prelude
#endif
import qualified Streamly.Internal.Data.Stream as Stream
import qualified Streamly.Internal.Data.Unfold as Unfold

import Gauge
import Streamly.Benchmark.Common
import Streamly.Internal.Data.Stream.Serial (SerialT)
#ifdef USE_PRELUDE
import Streamly.Benchmark.Prelude
import Streamly.Prelude (fromSerial, MonadAsync)
#endif
import qualified Stream.Common as SC

import System.IO.Unsafe (unsafeInterleaveIO)

import Prelude hiding (repeat, replicate, iterate)

-------------------------------------------------------------------------------
-- Generation
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- fromList
-------------------------------------------------------------------------------
#ifdef USE_PRELUDE
{-# INLINE sourceIsList #-}
sourceIsList :: Int -> Int -> SerialT Identity Int
sourceIsList value n = GHC.fromList [n..n+value]

{-# INLINE sourceIsString #-}
sourceIsString :: Int -> Int -> SerialT Identity Char
sourceIsString value n = GHC.fromString (Prelude.replicate (n + value) 'a')
#endif

{-# INLINE readInstance #-}
readInstance :: String -> SerialT Identity Int
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

#ifdef USE_PRELUDE
{-# INLINE repeat #-}
repeat :: (Monad m, S.IsStream t) => Int -> Int -> t m Int
repeat count = S.take count . S.repeat

{-# INLINE repeatM #-}
repeatM :: (MonadAsync m, S.IsStream t) => Int -> Int -> t m Int
repeatM count = S.take count . S.repeatM . return

{-# INLINE replicate #-}
replicate :: (Monad m, S.IsStream t) => Int -> Int -> t m Int
replicate = S.replicate

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
enumerate count n = S.take (count + n) S.enumerate

-- n ~ 1
{-# INLINE enumerateTo #-}
enumerateTo :: (Monad m, S.IsStream t) => Int -> Int -> t m Int
enumerateTo count n = S.enumerateTo (minBound + count + n)

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

o_1_space_generation_prel :: Int -> [Benchmark]
o_1_space_generation_prel value =
    [ bgroup "generation"
        [ benchIOSrc fromSerial "unfoldr" (sourceUnfoldr value)
        , benchIOSrc fromSerial "unfoldrM" (sourceUnfoldrM value)
        , benchIOSrc fromSerial "repeat" (repeat value)
        , benchIOSrc fromSerial "repeatM" (repeatM value)
        , benchIOSrc fromSerial "replicate" (replicate value)
        , benchIOSrc fromSerial "replicateM" (replicateM value)
        , benchIOSrc fromSerial "iterate" (iterate value)
        , benchIOSrc fromSerial "iterateM" (iterateM value)
        , benchIOSrc fromSerial "fromIndices" (fromIndices value)
        , benchIOSrc fromSerial "fromIndicesM" (fromIndicesM value)
        , benchIOSrc fromSerial "intFromTo" (sourceIntFromTo value)
        , benchIOSrc fromSerial "intFromThenTo" (sourceIntFromThenTo value)
        , benchIOSrc fromSerial "integerFromStep" (sourceIntegerFromStep value)
        , benchIOSrc fromSerial "fracFromThenTo" (sourceFracFromThenTo value)
        , benchIOSrc fromSerial "fracFromTo" (sourceFracFromTo value)
        , benchIOSrc fromSerial "fromList" (sourceFromList value)
        , benchPureSrc "IsList.fromList" (sourceIsList value)
        , benchPureSrc "IsString.fromString" (sourceIsString value)
        , benchIOSrc fromSerial "fromListM" (sourceFromListM value)
        , benchIOSrc fromSerial "enumerateFrom" (enumerateFrom value)
        , benchIOSrc fromSerial "enumerateFromTo" (enumerateFromTo value)
        , benchIOSrc fromSerial "enumerateFromThen" (enumerateFromThen value)
        , benchIOSrc fromSerial "enumerateFromThenTo" (enumerateFromThenTo value)
        , benchIOSrc fromSerial "enumerate" (enumerate value)
        , benchIOSrc fromSerial "enumerateTo" (enumerateTo value)

          -- These essentially test cons and consM
        , benchIOSrc fromSerial "fromFoldable" (sourceFromFoldable value)
        , benchIOSrc fromSerial "fromFoldableM" (sourceFromFoldableM value)
        , benchIOSrc fromSerial "absTimes" $ absTimes value
        ]
    ]
#endif

{-# INLINE mfixUnfold #-}
mfixUnfold :: Int -> Int -> SerialT IO (Int, Int)
mfixUnfold count start = Stream.mfix f
    where
    f action = do
        let incr n act = fmap ((+n) . snd)  $ unsafeInterleaveIO act
        x <- Stream.unfold Unfold.fromListM [incr 1 action, incr 2 action]
        y <- SC.sourceUnfoldr count start
        return (x, y)

{-# INLINE fromFoldable #-}
fromFoldable :: Int -> Int -> SerialT m Int
fromFoldable count start =
    Stream.fromFoldable (Prelude.enumFromTo count start)

{-# INLINE fromFoldableM #-}
fromFoldableM :: Monad m => Int -> Int -> SerialT m Int
fromFoldableM count start =
    Stream.fromFoldableM (fmap return (Prelude.enumFromTo count start))

o_1_space_generation :: Int -> [Benchmark]
o_1_space_generation value =
    [ bgroup "generation"
        [ SC.benchIOSrc "unfold" (SC.sourceUnfoldr value)
        , SC.benchIOSrc "fromFoldable" (fromFoldable value)
        , SC.benchIOSrc "fromFoldableM" (fromFoldableM value)
        , SC.benchIOSrc "mfix_10" (mfixUnfold 10)
        , SC.benchIOSrc "mfix_100" (mfixUnfold 100)
        , SC.benchIOSrc "mfix_1000" (mfixUnfold 1000)
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
        [
#ifdef USE_PRELUDE
        bgroup (o_1_space_prefix moduleName) (o_1_space_generation_prel size)
        ,
#endif
        bgroup (o_1_space_prefix moduleName) (o_1_space_generation size)
        , bgroup (o_n_heap_prefix moduleName) (o_n_heap_generation size)
        ]
