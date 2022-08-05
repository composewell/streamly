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

import qualified Stream.Common as Common
#ifdef USE_PRELUDE
import qualified GHC.Exts as GHC
import qualified Streamly.Prelude as S
import qualified Streamly.Internal.Data.Stream.IsStream as Stream
import qualified Prelude
#else
import qualified Streamly.Internal.Data.Stream as Stream
#endif

import Gauge
import Streamly.Benchmark.Common
import Streamly.Internal.Data.Stream.Serial (SerialT)
#ifdef USE_PRELUDE
import Streamly.Prelude (MonadAsync)
import Stream.Common hiding (MonadAsync, replicate, enumerateFromTo)
import Streamly.Benchmark.Prelude hiding
    (benchIOSrc, sourceUnfoldrM, apDiscardFst, apDiscardSnd, apLiftA2, toNullAp
    , monadThen, toNullM, toNullM3, filterAllInM, filterAllOutM, filterSome
    , breakAfterSome, toListM, toListSome)
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
#endif

{-# INLINE mfixUnfold #-}
mfixUnfold :: Int -> Int -> SerialT IO (Int, Int)
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
#ifdef USE_PRELUDE
        , benchIOSrc "repeat" (repeat value)
        , benchIOSrc "repeatM" (repeatM value)
        , benchIOSrc "replicate" (replicate value)
        , benchIOSrc "replicateM" (replicateM value)
        , benchIOSrc "iterate" (iterate value)
        , benchIOSrc "iterateM" (iterateM value)
        , benchIOSrc "fromIndices" (fromIndices value)
        , benchIOSrc "fromIndicesM" (fromIndicesM value)
        , benchIOSrc "intFromTo" (sourceIntFromTo value)
        , benchIOSrc "intFromThenTo" (sourceIntFromThenTo value)
        , benchIOSrc "integerFromStep" (sourceIntegerFromStep value)
        , benchIOSrc "fracFromThenTo" (sourceFracFromThenTo value)
        , benchIOSrc "fracFromTo" (sourceFracFromTo value)
        , benchIOSrc "fromList" (sourceFromList value)
        , benchPureSrc "IsList.fromList" (sourceIsList value)
        , benchPureSrc "IsString.fromString" (sourceIsString value)
        , benchIOSrc "fromListM" (sourceFromListM value)
        , benchIOSrc "enumerateFrom" (enumerateFrom value)
        , benchIOSrc "enumerateFromTo" (enumerateFromTo value)
        , benchIOSrc "enumerateFromThen" (enumerateFromThen value)
        , benchIOSrc "enumerateFromThenTo" (enumerateFromThenTo value)
        , benchIOSrc "enumerate" (enumerate value)
        , benchIOSrc "enumerateTo" (enumerateTo value)
#endif

          -- These essentially test cons and consM
        , benchIOSrc "fromFoldable" (sourceFromFoldable value)
        , benchIOSrc "fromFoldableM" (sourceFromFoldableM value)

#ifdef USE_PRELUDE
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
