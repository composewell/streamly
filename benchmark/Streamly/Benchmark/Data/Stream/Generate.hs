-- |
-- Module      : Stream.Generate
-- Copyright   : (c) 2018 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

#ifdef __HADDOCK_VERSION__
#undef INSPECTION
#endif

#ifdef INSPECTION
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fplugin Test.Inspection.Plugin #-}
#endif

module Stream.Generate (benchmarks) where

#ifdef INSPECTION
import GHC.Types (SPEC(..))
import Test.Inspection
import qualified Streamly.Internal.Data.Fold as Fold
#endif

import Control.Monad.IO.Class (MonadIO)
import Streamly.Internal.Data.Stream (Stream)
import Streamly.Internal.Data.Time.Units (AbsTime)

import qualified Streamly.Internal.Data.Stream as Stream

import Test.Tasty.Bench
import Stream.Common hiding (benchIO)
import Stream.Type (benchIO, withDrain)
import Streamly.Benchmark.Common

import Prelude hiding (repeat, replicate, iterate)

-- XXX should we use rnf to evaluate the result?
{-# INLINE fromListM #-}
fromListM :: Monad m => [m a] -> Stream m a
fromListM = Stream.sequence . Stream.fromList

{-# INLINE sourceFromListM #-}
sourceFromListM :: Int -> IO ()
sourceFromListM value = withDrain $ \n -> fromListM (fmap return [n..n+value])

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'sourceFromListM
inspect $ 'sourceFromListM `hasNoType` ''Stream.Step
inspect $ 'sourceFromListM `hasNoType` ''Fold.Step
inspect $ 'sourceFromListM `hasNoType` ''SPEC
#endif

{-# INLINE replicate #-}
replicate :: Int -> IO ()
replicate value = withDrain (Stream.replicate value)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'replicate
inspect $ 'replicate `hasNoType` ''Stream.Step
inspect $ 'replicate `hasNoType` ''Fold.Step
inspect $ 'replicate `hasNoType` ''SPEC
#endif

-------------------------------------------------------------------------------
-- enumerate
-------------------------------------------------------------------------------

{-# INLINE sourceIntFromTo #-}
sourceIntFromTo :: Int -> IO ()
sourceIntFromTo value = withDrain $ \n -> Stream.enumerateFromTo n (n + value)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'sourceIntFromTo
inspect $ 'sourceIntFromTo `hasNoType` ''Stream.Step
inspect $ 'sourceIntFromTo `hasNoType` ''Fold.Step
inspect $ 'sourceIntFromTo `hasNoType` ''SPEC
#endif

{-# INLINE sourceIntFromThenTo #-}
sourceIntFromThenTo :: Int -> IO ()
sourceIntFromThenTo value = withDrain $ \n ->
    Stream.enumerateFromThenTo n (n + 1) (n + value)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'sourceIntFromThenTo
inspect $ 'sourceIntFromThenTo `hasNoType` ''Stream.Step
inspect $ 'sourceIntFromThenTo `hasNoType` ''Stream.EnumState
inspect $ 'sourceIntFromThenTo `hasNoType` ''Fold.Step
inspect $ 'sourceIntFromThenTo `hasNoType` ''SPEC
#endif

{-# INLINE sourceFracFromTo #-}
sourceFracFromTo :: Int -> IO ()
sourceFracFromTo value = withDrain $ \n ->
    Stream.enumerateFromTo (fromIntegral n :: Double) (fromIntegral (n + value))

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'sourceFracFromTo
inspect $ 'sourceFracFromTo `hasNoType` ''Stream.Step
inspect $ 'sourceFracFromTo `hasNoType` ''Fold.Step
inspect $ 'sourceFracFromTo `hasNoType` ''SPEC
#endif

{-# INLINE sourceFracFromThenTo #-}
sourceFracFromThenTo :: Int -> IO ()
sourceFracFromThenTo value = withDrain $ \n ->
    Stream.enumerateFromThenTo
        (fromIntegral n) (fromIntegral n + 1.0001 :: Double) (fromIntegral (n + value))

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'sourceFracFromThenTo
inspect $ 'sourceFracFromThenTo `hasNoType` ''Stream.Step
inspect $ 'sourceFracFromThenTo `hasNoType` ''Fold.Step
inspect $ 'sourceFracFromThenTo `hasNoType` ''SPEC
#endif

{-# INLINE sourceIntegerFromStep #-}
sourceIntegerFromStep :: Int -> IO ()
sourceIntegerFromStep value = withDrain $ \n ->
    Stream.take value
        $ Stream.enumerateFromThen (fromIntegral n :: Integer) (fromIntegral n + 1)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'sourceIntegerFromStep
inspect $ 'sourceIntegerFromStep `hasNoType` ''Stream.Step
inspect $ 'sourceIntegerFromStep `hasNoType` ''Fold.Step
inspect $ 'sourceIntegerFromStep `hasNoType` ''SPEC
#endif

{-# INLINE enumerateFrom #-}
enumerateFrom :: Int -> IO ()
enumerateFrom count = withDrain (Stream.take count . Stream.enumerateFrom)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'enumerateFrom
inspect $ 'enumerateFrom `hasNoType` ''Stream.Step
inspect $ 'enumerateFrom `hasNoType` ''Fold.Step
inspect $ 'enumerateFrom `hasNoType` ''SPEC
#endif

{-# INLINE enumerateFromTo #-}
enumerateFromTo :: Int -> IO ()
enumerateFromTo = sourceIntFromTo

-- 'enumerateFromTo' is an alias for 'sourceIntFromTo', already covered above.

{-# INLINE enumerateFromThen #-}
enumerateFromThen :: Int -> IO ()
enumerateFromThen count = withDrain $ \n ->
    Stream.take count $ Stream.enumerateFromThen n (n + 1)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'enumerateFromThen
inspect $ 'enumerateFromThen `hasNoType` ''Stream.Step
inspect $ 'enumerateFromThen `hasNoType` ''Fold.Step
inspect $ 'enumerateFromThen `hasNoType` ''SPEC
#endif

{-# INLINE enumerateFromThenTo #-}
enumerateFromThenTo :: Int -> IO ()
enumerateFromThenTo = sourceIntFromThenTo

-- 'enumerateFromThenTo' is an alias for 'sourceIntFromThenTo', already covered above.

-- n ~ 1
{-# INLINE enumerate #-}
enumerate :: Int -> IO ()
enumerate count = withDrain $ \n ->
    Stream.take (count + n) Stream.enumerate :: Stream IO Int

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'enumerate
inspect $ 'enumerate `hasNoType` ''Stream.Step
inspect $ 'enumerate `hasNoType` ''Fold.Step
inspect $ 'enumerate `hasNoType` ''SPEC
#endif

-- n ~ 1
{-# INLINE enumerateTo #-}
enumerateTo :: Int -> IO ()
enumerateTo count = withDrain $ \n -> Stream.enumerateTo (minBound + count + n)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'enumerateTo
inspect $ 'enumerateTo `hasNoType` ''Stream.Step
inspect $ 'enumerateTo `hasNoType` ''Fold.Step
inspect $ 'enumerateTo `hasNoType` ''SPEC
#endif

{-# INLINE iterate #-}
iterate :: Int -> IO ()
iterate count = withDrain (Stream.take count . Stream.iterate (+1))

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'iterate
inspect $ 'iterate `hasNoType` ''Stream.Step
inspect $ 'iterate `hasNoType` ''Fold.Step
inspect $ 'iterate `hasNoType` ''SPEC
#endif

{-# INLINE iterateM #-}
iterateM :: Int -> IO ()
iterateM count =
    withDrain (Stream.take count . Stream.iterateM (return . (+1)) . return)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'iterateM
inspect $ 'iterateM `hasNoType` ''Stream.Step
inspect $ 'iterateM `hasNoType` ''Fold.Step
inspect $ 'iterateM `hasNoType` ''SPEC
#endif

{-# INLINE repeatM #-}
repeatM :: Int -> IO ()
repeatM count = withDrain (Stream.take count . Stream.repeatM . return)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'repeatM
inspect $ 'repeatM `hasNoType` ''Stream.Step
inspect $ 'repeatM `hasNoType` ''Fold.Step
inspect $ 'repeatM `hasNoType` ''SPEC
#endif

{-# INLINE replicateM #-}
replicateM :: Int -> IO ()
replicateM count = withDrain (Stream.replicateM count . return)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'replicateM
inspect $ 'replicateM `hasNoType` ''Stream.Step
inspect $ 'replicateM `hasNoType` ''Fold.Step
inspect $ 'replicateM `hasNoType` ''SPEC
#endif

{-# INLINE fromIndices #-}
fromIndices :: Int -> IO ()
fromIndices value = withDrain $ \n -> Stream.take value $ Stream.fromIndices (+ n)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'fromIndices
inspect $ 'fromIndices `hasNoType` ''Stream.Step
inspect $ 'fromIndices `hasNoType` ''Fold.Step
inspect $ 'fromIndices `hasNoType` ''SPEC
#endif

{-# INLINE fromIndicesM #-}
fromIndicesM :: Int -> IO ()
fromIndicesM value = withDrain $ \n ->
    Stream.take value $ Stream.fromIndicesM (return <$> (+ n))

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'fromIndicesM
inspect $ 'fromIndicesM `hasNoType` ''Stream.Step
inspect $ 'fromIndicesM `hasNoType` ''Fold.Step
inspect $ 'fromIndicesM `hasNoType` ''SPEC
#endif

{-# INLINE _absTimes #-}
_absTimes :: MonadIO m => Int -> Int -> Stream m AbsTime
_absTimes value _ = Stream.take value Stream.absTimes

o_1_space_generation :: Int -> [Benchmark]
o_1_space_generation value =
        -- 'sourceUnfoldr', 'sourceUnfoldrM', and 'repeat' are from Stream.Common.
        [ benchIO "unfoldr" $ withDrain (sourceUnfoldr value)
        , benchIO "unfoldrM" $ withDrain (sourceUnfoldrM value)
        , benchIO "repeat" $ withDrain (repeat value)
        , benchIO "replicate" $ replicate value
        , benchIO "iterate" $ iterate value
        , benchIO "iterateM" $ iterateM value
        , benchIO "intFromTo" $ sourceIntFromTo value
        , benchIO "intFromThenTo" $ sourceIntFromThenTo value
        , benchIO "integerFromStep" $ sourceIntegerFromStep value
        , benchIO "fracFromThenTo" $ sourceFracFromThenTo value
        , benchIO "fracFromTo" $ sourceFracFromTo value
        , benchIO "fromListM" $ sourceFromListM value
        , benchIO "enumerateFrom" $ enumerateFrom value
        , benchIO "enumerateFromTo" $ enumerateFromTo value
        , benchIO "enumerateFromThen" $ enumerateFromThen value
        , benchIO "enumerateFromThenTo" $ enumerateFromThenTo value
        , benchIO "enumerate" $ enumerate value
        , benchIO "enumerateTo" $ enumerateTo value
        , benchIO "repeatM" $ repeatM value
        , benchIO "replicateM" $ replicateM value
        , benchIO "fromIndices" $ fromIndices value
        , benchIO "fromIndicesM" $ fromIndicesM value

        -- fromFoldable essentially tests cons and consM which does not scale
        -- for the Stream type.
        -- , benchIO "fromFoldable 16" (sourceFromFoldable 16)
        -- , benchIO "fromFoldableM 16" (sourceFromFoldableM 16)
        --  XXX tasty-bench hangs benchmarking this
        -- , benchIO "absTimes" $ _absTimes value
        ]

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

-- In addition to gauge options, the number of elements in the stream can be
-- passed using the --stream-size option.
--
benchmarks :: Int -> [(SpaceComplexity, Benchmark)]
benchmarks size =
    map (SpaceO_1,) (o_1_space_generation size)
