-- |
-- Module      : Stream.Generate
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

module Stream.Generate (benchmarks) where

#ifdef INSPECTION
import GHC.Types (SPEC(..))
import Test.Inspection
#endif

import Control.DeepSeq (NFData(..))
import Control.Monad.IO.Class (MonadIO)
import Data.Functor.Identity (Identity(..))
import Streamly.Internal.Data.Stream (Stream)
import Streamly.Internal.Data.Time.Units (AbsTime)

import qualified GHC.Exts as GHC
import qualified Streamly.Internal.Data.Fold as Fold
import qualified Streamly.Internal.Data.Stream as Stream

import Test.Tasty.Bench
import Stream.Common
import Streamly.Benchmark.Common
import qualified Prelude

import Prelude hiding (repeat, replicate, iterate)

-------------------------------------------------------------------------------
-- fromList
-------------------------------------------------------------------------------

{-# INLINE sourceFromList #-}
sourceFromList :: Monad m => Int -> Int -> Stream m Int
sourceFromList value n = Stream.fromList [n..n+value]

-- | 'fromTuple' yields two elements per tuple. To emit and drain ~value
-- elements we generate value/2 tuples and reduce each tuple's 'fromTuple'
-- stream with a light 'sum' fold (avoiding a heavy, non-fusible 'concatMap'
-- that would mask the cost of 'fromTuple').
{-# INLINE sourceFromTuple #-}
sourceFromTuple :: Monad m => Int -> Int -> Stream m Int
sourceFromTuple value n =
    Stream.mapM (Stream.fold Fold.sum . Stream.fromTuple)
        $ Stream.fromList (fmap (\i -> (i, i)) [n .. n + value `div` 2])

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

{-# INLINE repeatM #-}
repeatM :: MonadAsync m => Int -> Int -> Stream m Int
repeatM count = Stream.take count . Stream.repeatM . return

{-# INLINE replicateM #-}
replicateM :: MonadAsync m => Int -> Int -> Stream m Int
replicateM count = Stream.replicateM count . return

{-# INLINE fromIndices #-}
fromIndices :: Monad m => Int -> Int -> Stream m Int
fromIndices value n = Stream.take value $ Stream.fromIndices (+ n)

{-# INLINE fromIndicesM #-}
fromIndicesM :: Monad m => Int -> Int -> Stream m Int
fromIndicesM value n = Stream.take value $ Stream.fromIndicesM (return <$> (+ n))

{-# INLINE _absTimes #-}
_absTimes :: MonadIO m => Int -> Int -> Stream m AbsTime
_absTimes value _ = Stream.take value Stream.absTimes

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
        , benchIOSrc "fromTuple" (sourceFromTuple value)
        , benchIOSrc "fromListM" (sourceFromListM value)
        , benchPureSrc "IsList.fromList" (sourceIsList value)
        , benchPureSrc "IsString.fromString" (sourceIsString value)
        , benchIOSrc "enumerateFrom" (enumerateFrom value)
        , benchIOSrc "enumerateFromTo" (enumerateFromTo value)
        , benchIOSrc "enumerateFromThen" (enumerateFromThen value)
        , benchIOSrc "enumerateFromThenTo" (enumerateFromThenTo value)
        , benchIOSrc "enumerate" (enumerate value)
        , benchIOSrc "enumerateTo" (enumerateTo value)
        , benchIOSrc "repeatM" (repeatM value)
        , benchIOSrc "replicateM" (replicateM value)
        , benchIOSrc "fromIndices" (fromIndices value)
        , benchIOSrc "fromIndicesM" (fromIndicesM value)

        -- fromFoldable essentially tests cons and consM which does not scale
        -- for the Stream type.
        -- , benchIOSrc "fromFoldable 16" (sourceFromFoldable 16)
        -- , benchIOSrc "fromFoldableM 16" (sourceFromFoldableM 16)
        --  XXX tasty-bench hangs benchmarking this
        -- , benchIOSrc "absTimes" $ _absTimes value
        ]
    ]

instance NFData a => NFData (Stream Identity a) where
    {-# INLINE rnf #-}
    rnf xs = runIdentity $ Stream.fold (Fold.foldl' (\_ x -> rnf x) ()) xs

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
-- Inspection
-------------------------------------------------------------------------------

#ifdef INSPECTION
-- The generators above produce a 'Stream', which is built from the 'Step'
-- constructors ('Yield', 'Skip', 'Stop'), so a bare generator cannot be free
-- of 'Step'. To check that generation fully fuses with consumption we drain
-- each generator in 'IO' (the same 'drain . source' pipeline that 'benchIOSrc'
-- runs) and assert that no 'Step' constructors survive in the optimized core.

{-# INLINE drainUnfoldr #-}
drainUnfoldr :: Int -> Int -> IO ()
drainUnfoldr value n = drain $ sourceUnfoldr value n

{-# INLINE drainUnfoldrM #-}
drainUnfoldrM :: Int -> Int -> IO ()
drainUnfoldrM value n = drain $ sourceUnfoldrM value n

{-# INLINE drainRepeat #-}
drainRepeat :: Int -> Int -> IO ()
drainRepeat value n = drain $ repeat value n

{-# INLINE drainRepeatM #-}
drainRepeatM :: Int -> Int -> IO ()
drainRepeatM value n = drain $ repeatM value n

{-# INLINE drainReplicate #-}
drainReplicate :: Int -> Int -> IO ()
drainReplicate value n = drain $ replicate value n

{-# INLINE drainReplicateM #-}
drainReplicateM :: Int -> Int -> IO ()
drainReplicateM value n = drain $ replicateM value n

{-# INLINE drainIterate #-}
drainIterate :: Int -> Int -> IO ()
drainIterate value n = drain $ iterate value n

{-# INLINE drainIterateM #-}
drainIterateM :: Int -> Int -> IO ()
drainIterateM value n = drain $ iterateM value n

{-# INLINE drainIntFromTo #-}
drainIntFromTo :: Int -> Int -> IO ()
drainIntFromTo value n = drain $ sourceIntFromTo value n

{-# INLINE drainIntFromThenTo #-}
drainIntFromThenTo :: Int -> Int -> IO ()
drainIntFromThenTo value n = drain $ sourceIntFromThenTo value n

{-# INLINE drainIntegerFromStep #-}
drainIntegerFromStep :: Int -> Int -> IO ()
drainIntegerFromStep value n = drain $ sourceIntegerFromStep value n

{-# INLINE drainFracFromTo #-}
drainFracFromTo :: Int -> Int -> IO ()
drainFracFromTo value n = drain $ sourceFracFromTo value n

{-# INLINE drainFracFromThenTo #-}
drainFracFromThenTo :: Int -> Int -> IO ()
drainFracFromThenTo value n = drain $ sourceFracFromThenTo value n

{-# INLINE drainFromList #-}
drainFromList :: Int -> Int -> IO ()
drainFromList value n = drain $ sourceFromList value n

{-# INLINE drainFromTuple #-}
drainFromTuple :: Int -> Int -> IO ()
drainFromTuple value n = drain $ sourceFromTuple value n

{-# INLINE drainFromListM #-}
drainFromListM :: Int -> Int -> IO ()
drainFromListM value n = drain $ sourceFromListM value n

{-# INLINE drainEnumerateFrom #-}
drainEnumerateFrom :: Int -> Int -> IO ()
drainEnumerateFrom value n = drain $ enumerateFrom value n

{-# INLINE drainEnumerateFromThen #-}
drainEnumerateFromThen :: Int -> Int -> IO ()
drainEnumerateFromThen value n = drain $ enumerateFromThen value n

{-# INLINE drainEnumerate #-}
drainEnumerate :: Int -> Int -> IO ()
drainEnumerate value n = drain $ enumerate value n

{-# INLINE drainEnumerateTo #-}
drainEnumerateTo :: Int -> Int -> IO ()
drainEnumerateTo value n = drain $ enumerateTo value n

{-# INLINE drainFromIndices #-}
drainFromIndices :: Int -> Int -> IO ()
drainFromIndices value n = drain $ fromIndices value n

{-# INLINE drainFromIndicesM #-}
drainFromIndicesM :: Int -> Int -> IO ()
drainFromIndicesM value n = drain $ fromIndicesM value n

-- 'enumerateFromTo'/'enumerateFromThenTo' are aliases for 'sourceIntFromTo'/
-- 'sourceIntFromThenTo', already covered above.

-- The 'IsList'/'IsString' and 'readsPrec' benchmarks run in the 'Identity'
-- monad, so we drain them purely with 'runIdentity'.

{-# INLINE drainIsList #-}
drainIsList :: Int -> Int -> ()
drainIsList value n = runIdentity $ drain $ sourceIsList value n

{-# INLINE drainIsString #-}
drainIsString :: Int -> Int -> ()
drainIsString value n = runIdentity $ drain $ sourceIsString value n

-- 'readsPrec' (the 'Read' instance) parses and buffers its input, so it neither
-- fully specialises nor eliminates the 'Step' constructors. The corresponding
-- 'readsPrec Haskell lists' benchmark operates on a plain list, not a stream.
-- {-# INLINE drainReadInstance #-}
-- drainReadInstance :: String -> ()
-- drainReadInstance str = runIdentity $ drain $ readInstance str

inspect $ hasNoTypeClasses 'drainUnfoldr
inspect $ 'drainUnfoldr `hasNoType` ''Stream.Step
inspect $ 'drainUnfoldr `hasNoType` ''Fold.Step
inspect $ 'drainUnfoldr `hasNoType` ''SPEC
inspect $ hasNoTypeClasses 'drainUnfoldrM
inspect $ 'drainUnfoldrM `hasNoType` ''Stream.Step
inspect $ 'drainUnfoldrM `hasNoType` ''Fold.Step
inspect $ 'drainUnfoldrM `hasNoType` ''SPEC
inspect $ hasNoTypeClasses 'drainRepeat
inspect $ 'drainRepeat `hasNoType` ''Stream.Step
inspect $ 'drainRepeat `hasNoType` ''Fold.Step
inspect $ 'drainRepeat `hasNoType` ''SPEC
inspect $ hasNoTypeClasses 'drainRepeatM
inspect $ 'drainRepeatM `hasNoType` ''Stream.Step
inspect $ 'drainRepeatM `hasNoType` ''Fold.Step
inspect $ 'drainRepeatM `hasNoType` ''SPEC
inspect $ hasNoTypeClasses 'drainReplicate
inspect $ 'drainReplicate `hasNoType` ''Stream.Step
inspect $ 'drainReplicate `hasNoType` ''Fold.Step
inspect $ 'drainReplicate `hasNoType` ''SPEC
inspect $ hasNoTypeClasses 'drainReplicateM
inspect $ 'drainReplicateM `hasNoType` ''Stream.Step
inspect $ 'drainReplicateM `hasNoType` ''Fold.Step
inspect $ 'drainReplicateM `hasNoType` ''SPEC
inspect $ hasNoTypeClasses 'drainIterate
inspect $ 'drainIterate `hasNoType` ''Stream.Step
inspect $ 'drainIterate `hasNoType` ''Fold.Step
inspect $ 'drainIterate `hasNoType` ''SPEC
inspect $ hasNoTypeClasses 'drainIterateM
inspect $ 'drainIterateM `hasNoType` ''Stream.Step
inspect $ 'drainIterateM `hasNoType` ''Fold.Step
inspect $ 'drainIterateM `hasNoType` ''SPEC
inspect $ hasNoTypeClasses 'drainIntFromTo
inspect $ 'drainIntFromTo `hasNoType` ''Stream.Step
inspect $ 'drainIntFromTo `hasNoType` ''Fold.Step
inspect $ 'drainIntFromTo `hasNoType` ''SPEC
inspect $ hasNoTypeClasses 'drainIntFromThenTo
inspect $ 'drainIntFromThenTo `hasNoType` ''Stream.Step
inspect $ 'drainIntFromThenTo `hasNoType` ''Fold.Step
inspect $ 'drainIntFromThenTo `hasNoType` ''SPEC
inspect $ hasNoTypeClasses 'drainIntegerFromStep
inspect $ 'drainIntegerFromStep `hasNoType` ''Stream.Step
inspect $ 'drainIntegerFromStep `hasNoType` ''Fold.Step
inspect $ 'drainIntegerFromStep `hasNoType` ''SPEC
inspect $ hasNoTypeClasses 'drainFracFromTo
inspect $ 'drainFracFromTo `hasNoType` ''Stream.Step
inspect $ 'drainFracFromTo `hasNoType` ''Fold.Step
inspect $ 'drainFracFromTo `hasNoType` ''SPEC
inspect $ hasNoTypeClasses 'drainFracFromThenTo
inspect $ 'drainFracFromThenTo `hasNoType` ''Stream.Step
inspect $ 'drainFracFromThenTo `hasNoType` ''Fold.Step
inspect $ 'drainFracFromThenTo `hasNoType` ''SPEC
inspect $ hasNoTypeClasses 'drainFromList
inspect $ 'drainFromList `hasNoType` ''Stream.Step
inspect $ 'drainFromList `hasNoType` ''Fold.Step
inspect $ 'drainFromList `hasNoType` ''SPEC
inspect $ hasNoTypeClasses 'drainFromTuple
inspect $ 'drainFromTuple `hasNoType` ''Stream.Step
inspect $ 'drainFromTuple `hasNoType` ''Fold.Step
inspect $ 'drainFromTuple `hasNoType` ''SPEC
inspect $ hasNoTypeClasses 'drainFromListM
inspect $ 'drainFromListM `hasNoType` ''Stream.Step
inspect $ 'drainFromListM `hasNoType` ''Fold.Step
inspect $ 'drainFromListM `hasNoType` ''SPEC
inspect $ hasNoTypeClasses 'drainEnumerateFrom
inspect $ 'drainEnumerateFrom `hasNoType` ''Stream.Step
inspect $ 'drainEnumerateFrom `hasNoType` ''Fold.Step
inspect $ 'drainEnumerateFrom `hasNoType` ''SPEC
inspect $ hasNoTypeClasses 'drainEnumerateFromThen
inspect $ 'drainEnumerateFromThen `hasNoType` ''Stream.Step
inspect $ 'drainEnumerateFromThen `hasNoType` ''Fold.Step
inspect $ 'drainEnumerateFromThen `hasNoType` ''SPEC
inspect $ hasNoTypeClasses 'drainEnumerate
inspect $ 'drainEnumerate `hasNoType` ''Stream.Step
inspect $ 'drainEnumerate `hasNoType` ''Fold.Step
inspect $ 'drainEnumerate `hasNoType` ''SPEC
inspect $ hasNoTypeClasses 'drainEnumerateTo
inspect $ 'drainEnumerateTo `hasNoType` ''Stream.Step
inspect $ 'drainEnumerateTo `hasNoType` ''Fold.Step
inspect $ 'drainEnumerateTo `hasNoType` ''SPEC
inspect $ hasNoTypeClasses 'drainFromIndices
inspect $ 'drainFromIndices `hasNoType` ''Stream.Step
inspect $ 'drainFromIndices `hasNoType` ''Fold.Step
inspect $ 'drainFromIndices `hasNoType` ''SPEC
inspect $ hasNoTypeClasses 'drainFromIndicesM
inspect $ 'drainFromIndicesM `hasNoType` ''Stream.Step
inspect $ 'drainFromIndicesM `hasNoType` ''Fold.Step
inspect $ 'drainFromIndicesM `hasNoType` ''SPEC
inspect $ hasNoTypeClasses 'drainIsList
inspect $ 'drainIsList `hasNoType` ''Stream.Step
inspect $ 'drainIsList `hasNoType` ''Fold.Step
inspect $ 'drainIsList `hasNoType` ''SPEC
inspect $ hasNoTypeClasses 'drainIsString
inspect $ 'drainIsString `hasNoType` ''Stream.Step
inspect $ 'drainIsString `hasNoType` ''Fold.Step
inspect $ 'drainIsString `hasNoType` ''SPEC
-- inspect $ hasNoTypeClasses 'drainReadInstance
-- inspect $ 'drainReadInstance `hasNoType` ''Stream.Step
-- inspect $ 'drainReadInstance `hasNoType` ''Fold.Step
-- inspect $ 'drainReadInstance `hasNoType` ''SPEC
#endif

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
