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
import System.Random (randomRIO)

import qualified Prelude
import qualified GHC.Exts as GHC

import qualified Streamly.Prelude  as S
import qualified Streamly.Internal.Data.Stream.IsStream.Generate as Generate
import qualified Streamly.Internal.Data.IORef.Storable as Storable

import Gauge
import Streamly.Prelude (SerialT, fromSerial, MonadAsync)
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

{-# INLINE fromStorableIORef #-}
fromStorableIORef :: Int -> Benchmark
fromStorableIORef size =
    env (Storable.newIORef (1 :: Int)) $ \ref ->
        bench "fromStorableIORef" $ nfIO $ do
            val <- randomRIO (size, size)
            S.drain $ S.take val $ Generate.fromStorableIORef ref

o_1_space_generation :: Int -> [Benchmark]
o_1_space_generation value =
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
        , fromStorableIORef value
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
