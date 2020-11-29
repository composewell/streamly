-- |
-- Module      : Serial.Generation
-- Copyright   : (c) 2018 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

module Serial.Generation (benchmarks) where

import Data.Functor.Identity (Identity)

import qualified Prelude
import qualified GHC.Exts as GHC

import qualified Streamly.Prelude  as S

import Gauge
import Streamly.Prelude (SerialT, serially, MonadAsync)
import Streamly.Benchmark.Common
import Streamly.Benchmark.Prelude
import Prelude hiding (repeat, replicate, iterate)

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
sourceIsString :: Int -> Int -> SerialT Identity Char
sourceIsString value n = GHC.fromString (Prelude.replicate (n + value) 'a')

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
        , benchIOSrc serially "enumerateFromTo" (enumerateFromTo value)
        , benchIOSrc serially "enumerateFromThen" (enumerateFromThen value)
        , benchIOSrc serially "enumerateFromThenTo" (enumerateFromThenTo value)
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
