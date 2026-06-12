-- |
-- Module      : Streamly.Benchmark.Data.Fold
-- Copyright   : (c) 2018 Composewell
-- License     : MIT
-- Maintainer  : streamly@composewell.com

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

#undef FUSION_CHECK
#ifdef FUSION_CHECK
{-# OPTIONS_GHC -ddump-simpl -ddump-to-file -dsuppress-all #-}
#endif

#ifdef __HADDOCK_VERSION__
#undef INSPECTION
#endif

#ifdef INSPECTION
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fplugin Test.Inspection.Plugin #-}
#endif

module Main (main) where

import Control.DeepSeq (NFData(..))
import Control.Exception (SomeException)
import Data.Char (ord)
import Data.Word (Word8)
import Streamly.Internal.Data.Unfold (Unfold)
import System.IO (Handle, hClose)
import System.Random (randomRIO)

import qualified Prelude
import qualified Streamly.FileSystem.Handle as FH
import qualified Streamly.Internal.Data.Fold as FL
import qualified Streamly.Internal.Data.Scanl as Scanl
import qualified Streamly.Internal.Data.Unfold as UF
import qualified Streamly.Internal.Data.Stream as S
import qualified Streamly.Internal.Data.StreamK as K

import qualified Unfold.Enumeration as Enumeration
import qualified Unfold.Type as Type

import Test.Tasty.Bench hiding (env)
import Prelude hiding (take, filter, zipWith, map, mapM, takeWhile, scanl, repeat, dropWhile)
import Streamly.Benchmark.Common
import Streamly.Benchmark.Common.Handle

#ifdef INSPECTION
import Control.Monad.Catch (MonadCatch)
import GHC.Types (SPEC(..))
import Test.Inspection

import qualified Streamly.Internal.Data.Producer as Producer
#endif

{-# INLINE benchIO #-}
benchIO :: (NFData b) => String -> (Int -> IO b) -> Benchmark
benchIO name f = bench name $ nfIO $ randomRIO (1,1) >>= f

-------------------------------------------------------------------------------
-- Stream generation and elimination
-------------------------------------------------------------------------------

-- generate numbers up to the argument value
{-# INLINE source #-}
source :: Monad m => Int -> Unfold m Int Int
source n = UF.supplySecond n UF.enumerateFromToIntegral

-------------------------------------------------------------------------------
-- Benchmark helpers
-------------------------------------------------------------------------------

{-# INLINE drainGeneration #-}
drainGeneration :: Monad m => Unfold m a b -> a -> m ()
drainGeneration unf seed = UF.fold FL.drain unf seed

{-# INLINE drainTransformation #-}
drainTransformation ::
       Monad m => Unfold m a b -> (Unfold m a b -> Unfold m c d) -> c -> m ()
drainTransformation unf f seed = drainGeneration (f unf) seed

{-# INLINE drainTransformationDefault #-}
drainTransformationDefault ::
       Monad m => Int -> (Unfold m Int Int -> Unfold m c d) -> c -> m ()
drainTransformationDefault to =
    drainTransformation (UF.supplySecond to UF.enumerateFromToIntegral)

-------------------------------------------------------------------------------
-- Operations on input
-------------------------------------------------------------------------------

{-# INLINE discardFirst #-}
discardFirst :: Int -> Int -> IO ()
discardFirst size start =
    drainTransformationDefault (size + start) UF.discardFirst (start, start)

#ifdef INSPECTION
inspect $ 'discardFirst `hasNoType` ''S.Step
inspect $ 'discardFirst `hasNoType` ''FL.Step
inspect $ 'discardFirst `hasNoType` ''SPEC
#endif

{-# INLINE discardSecond #-}
discardSecond :: Int -> Int -> IO ()
discardSecond size start =
    drainTransformationDefault (size + start) UF.discardSecond (start, start)

#ifdef INSPECTION
inspect $ 'discardSecond `hasNoType` ''S.Step
inspect $ 'discardSecond `hasNoType` ''FL.Step
inspect $ 'discardSecond `hasNoType` ''SPEC
#endif

-------------------------------------------------------------------------------
-- Stream generation
-------------------------------------------------------------------------------

{-# INLINE fromStream #-}
fromStream :: Int -> Int -> IO ()
fromStream size start =
    drainGeneration UF.fromStream (S.replicate size start :: S.Stream IO Int)

-- 'fromStream', 'fromStreamD' and 'consM' wrap an opaque stream/cons cell, so
-- the 'Step' is not eliminated.
#ifdef INSPECTION
-- inspect $ 'fromStream `hasNoType` ''S.Step
inspect $ 'fromStream `hasNoType` ''FL.Step
inspect $ 'fromStream `hasNoType` ''SPEC
#endif

-- XXX INVESTIGATE: Although the performance of this should be equivalant to
-- fromStream, this is considerably worse. More than 4x worse.
{-# INLINE fromStreamK #-}
fromStreamK :: Int -> Int -> IO ()
fromStreamK size start = drainGeneration UF.fromStreamK (K.replicate size start)

#ifdef INSPECTION
inspect $ 'fromStreamK `hasNoType` ''S.Step
inspect $ 'fromStreamK `hasNoType` ''FL.Step
inspect $ 'fromStreamK `hasNoType` ''SPEC
#endif

{-# INLINE fromStreamD #-}
fromStreamD :: Int -> Int -> IO ()
fromStreamD size start =
    drainGeneration UF.fromStreamD (S.replicate size start)

#ifdef INSPECTION
-- inspect $ 'fromStreamD `hasNoType` ''S.Step
inspect $ 'fromStreamD `hasNoType` ''FL.Step
inspect $ 'fromStreamD `hasNoType` ''SPEC
#endif

-- 'nilM' runs its action on the seed but yields no output, so unfold it over an
-- outer source of value seeds to run it ~value times.
{-# INLINE nilM #-}
nilM :: Int -> Int -> IO ()
nilM value start =
    drainGeneration (UF.unfoldEach (UF.nilM return) (source (start + value))) start

#ifdef INSPECTION
inspect $ 'nilM `hasNoType` ''S.Step
inspect $ 'nilM `hasNoType` ''FL.Step
inspect $ 'nilM `hasNoType` ''SPEC
inspect $ 'nilM `hasNoType` ''Producer.ConcatState
#endif

{-# INLINE nil #-}
nil :: Int -> Int -> IO ()
nil value start =
    drainGeneration (UF.unfoldEach UF.nil (source (start + value))) start

#ifdef INSPECTION
inspect $ 'nil `hasNoType` ''S.Step
inspect $ 'nil `hasNoType` ''FL.Step
inspect $ 'nil `hasNoType` ''SPEC
inspect $ 'nil `hasNoType` ''Producer.ConcatState
#endif

{-# INLINE consM #-}
consM :: Int -> Int -> IO ()
consM size start =
    drainTransformationDefault (size + start) (UF.consM return) start

#ifdef INSPECTION
-- inspect $ 'consM `hasNoType` ''S.Step
inspect $ 'consM `hasNoType` ''FL.Step
inspect $ 'consM `hasNoType` ''SPEC
#endif

{-# INLINE _fromSVar #-}
_fromSVar :: Int -> Int -> m ()
_fromSVar = undefined

{-# INLINE _fromProducer #-}
_fromProducer :: Int -> Int -> m ()
_fromProducer = undefined

{-# INLINE fromListM #-}
fromListM :: Int -> Int -> IO ()
fromListM size start =
    drainGeneration UF.fromListM (Prelude.map return [start .. start + size])

#ifdef INSPECTION
inspect $ 'fromListM `hasNoType` ''S.Step
inspect $ 'fromListM `hasNoType` ''FL.Step
inspect $ 'fromListM `hasNoType` ''SPEC
#endif

{-# INLINE replicateM #-}
replicateM :: Int -> Int -> IO ()
replicateM size start = drainGeneration UF.replicateM (size, return start)

#ifdef INSPECTION
inspect $ 'replicateM `hasNoType` ''S.Step
inspect $ 'replicateM `hasNoType` ''FL.Step
inspect $ 'replicateM `hasNoType` ''SPEC
#endif

{-# INLINE repeatM #-}
repeatM :: Int -> Int -> IO ()
repeatM size start = drainGeneration (UF.take size UF.repeatM) (return start)

#ifdef INSPECTION
inspect $ 'repeatM `hasNoType` ''S.Step
inspect $ 'repeatM `hasNoType` ''FL.Step
inspect $ 'repeatM `hasNoType` ''SPEC
#endif

{-# INLINE repeat #-}
repeat :: Int -> Int -> IO ()
repeat size start = drainGeneration (UF.take size UF.repeat) start

#ifdef INSPECTION
inspect $ 'repeat `hasNoType` ''S.Step
inspect $ 'repeat `hasNoType` ''FL.Step
inspect $ 'repeat `hasNoType` ''SPEC
#endif

{-# INLINE iterateM #-}
iterateM :: Int -> Int -> IO ()
iterateM size start =
    drainGeneration (UF.take size (UF.iterateM return)) (return start)

#ifdef INSPECTION
inspect $ 'iterateM `hasNoType` ''S.Step
inspect $ 'iterateM `hasNoType` ''FL.Step
inspect $ 'iterateM `hasNoType` ''SPEC
#endif

{-# INLINE fromIndicesM #-}
fromIndicesM :: Int -> Int -> IO ()
fromIndicesM size start =
    drainGeneration (UF.take size (UF.fromIndicesM return)) start

#ifdef INSPECTION
inspect $ 'fromIndicesM `hasNoType` ''S.Step
inspect $ 'fromIndicesM `hasNoType` ''FL.Step
inspect $ 'fromIndicesM `hasNoType` ''SPEC
#endif

-------------------------------------------------------------------------------
-- Stream transformation
-------------------------------------------------------------------------------

{-# INLINE postscan #-}
postscan :: Int -> Int -> IO ()
postscan size start =
    drainTransformationDefault (size + start) (UF.postscanl Scanl.sum) start

#ifdef INSPECTION
inspect $ 'postscan `hasNoType` ''S.Step
inspect $ 'postscan `hasNoType` ''FL.Step
inspect $ 'postscan `hasNoType` ''SPEC
#endif

{-# INLINE scanl #-}
scanl :: Int -> Int -> IO ()
scanl size start =
    drainTransformationDefault (size + start) (UF.scanl Scanl.sum) start

#ifdef INSPECTION
inspect $ 'scanl `hasNoType` ''S.Step
inspect $ 'scanl `hasNoType` ''FL.Step
inspect $ 'scanl `hasNoType` ''SPEC
#endif

{-# INLINE scanlMany #-}
scanlMany :: Int -> Int -> IO ()
scanlMany size start =
    drainTransformationDefault (size + start) (UF.scanlMany (Scanl.take 2 Scanl.sum)) start

#ifdef INSPECTION
inspect $ 'scanlMany `hasNoType` ''S.Step
inspect $ 'scanlMany `hasNoType` ''FL.Step
inspect $ 'scanlMany `hasNoType` ''SPEC
#endif

-------------------------------------------------------------------------------
-- Stream filtering
-------------------------------------------------------------------------------

{-# INLINE take #-}
take :: Int -> Int -> IO ()
take size start = drainTransformationDefault (size + start) (UF.take size) start

#ifdef INSPECTION
inspect $ 'take `hasNoType` ''S.Step
inspect $ 'take `hasNoType` ''FL.Step
inspect $ 'take `hasNoType` ''SPEC
#endif

{-# INLINE filter #-}
filter :: Int -> Int -> IO ()
filter size start =
    drainTransformationDefault (size + start) (UF.filter (\_ -> True)) start

#ifdef INSPECTION
inspect $ 'filter `hasNoType` ''S.Step
inspect $ 'filter `hasNoType` ''FL.Step
inspect $ 'filter `hasNoType` ''SPEC
#endif

{-# INLINE filterM #-}
filterM :: Int -> Int -> IO ()
filterM size start =
    drainTransformationDefault
        (size + start)
        (UF.filterM (\_ -> (return True)))
        start

#ifdef INSPECTION
inspect $ 'filterM `hasNoType` ''S.Step
inspect $ 'filterM `hasNoType` ''FL.Step
inspect $ 'filterM `hasNoType` ''SPEC
#endif

-- Dropping one element from a large stream is dominated by generation, so
-- instead exercise 'drop' ~value/2 times: generate value/2 two-element streams
-- with 'fromTuple', 'drop' the first element of each, and flatten the rest.
{-# INLINE dropOne #-}
dropOne :: Int -> Int -> IO ()
dropOne value start =
    let outer = UF.map (\i -> (i, i)) (source (start + value `div` 2))
     in drainGeneration (UF.unfoldEach (UF.drop 1 UF.fromTuple) outer) start

#ifdef INSPECTION
inspect $ 'dropOne `hasNoType` ''S.Step
inspect $ 'dropOne `hasNoType` ''FL.Step
inspect $ 'dropOne `hasNoType` ''SPEC
inspect $ 'dropOne `hasNoType` ''Producer.ConcatState
inspect $ 'dropOne `hasNoType` ''Producer.TupleState
#endif

{-# INLINE dropAll #-}
dropAll :: Int -> Int -> IO ()
dropAll size start =
    drainTransformationDefault (size + start) (UF.drop (size + 1)) start

#ifdef INSPECTION
inspect $ 'dropAll `hasNoType` ''S.Step
inspect $ 'dropAll `hasNoType` ''FL.Step
inspect $ 'dropAll `hasNoType` ''SPEC
#endif

{-# INLINE dropWhileTrue #-}
dropWhileTrue :: Int -> Int -> IO ()
dropWhileTrue size start =
    drainTransformationDefault
        (size + start)
        (UF.dropWhileM (\_ -> return True))
        start

#ifdef INSPECTION
inspect $ 'dropWhileTrue `hasNoType` ''S.Step
inspect $ 'dropWhileTrue `hasNoType` ''FL.Step
inspect $ 'dropWhileTrue `hasNoType` ''SPEC
#endif

{-# INLINE dropWhileFalse #-}
dropWhileFalse :: Int -> Int -> IO ()
dropWhileFalse size start =
    drainTransformationDefault
        (size + start)
        (UF.dropWhileM (\_ -> return False))
        start

#ifdef INSPECTION
inspect $ 'dropWhileFalse `hasNoType` ''S.Step
inspect $ 'dropWhileFalse `hasNoType` ''FL.Step
inspect $ 'dropWhileFalse `hasNoType` ''SPEC
#endif

{-# INLINE dropWhileMTrue #-}
dropWhileMTrue :: Int -> Int -> IO ()
dropWhileMTrue size start =
    drainTransformationDefault
        size
        (UF.dropWhileM (\_ -> return True))
        start

#ifdef INSPECTION
inspect $ 'dropWhileMTrue `hasNoType` ''S.Step
inspect $ 'dropWhileMTrue `hasNoType` ''FL.Step
inspect $ 'dropWhileMTrue `hasNoType` ''SPEC
#endif

{-# INLINE dropWhileMFalse #-}
dropWhileMFalse :: Int -> Int -> IO ()
dropWhileMFalse size start =
    drainTransformationDefault
        size
        (UF.dropWhileM (\_ -> return False))
        start

#ifdef INSPECTION
inspect $ 'dropWhileMFalse `hasNoType` ''S.Step
inspect $ 'dropWhileMFalse `hasNoType` ''FL.Step
inspect $ 'dropWhileMFalse `hasNoType` ''SPEC
#endif

{-# INLINE dropWhile #-}
dropWhile :: Int -> Int -> IO ()
dropWhile size start =
    drainTransformationDefault
        (size + start)
        (UF.dropWhile (\_ -> False))
        start

#ifdef INSPECTION
inspect $ 'dropWhile `hasNoType` ''S.Step
inspect $ 'dropWhile `hasNoType` ''FL.Step
inspect $ 'dropWhile `hasNoType` ''SPEC
#endif

{-# INLINE mapMaybe #-}
mapMaybe :: Int -> Int -> IO ()
mapMaybe size start =
    drainTransformationDefault (size + start) (UF.mapMaybe Just) start

#ifdef INSPECTION
inspect $ 'mapMaybe `hasNoType` ''S.Step
inspect $ 'mapMaybe `hasNoType` ''FL.Step
inspect $ 'mapMaybe `hasNoType` ''SPEC
#endif

{-# INLINE mapMaybeM #-}
mapMaybeM :: Int -> Int -> IO ()
mapMaybeM size start =
    drainTransformationDefault (size + start) (UF.mapMaybeM (return . Just)) start

#ifdef INSPECTION
inspect $ 'mapMaybeM `hasNoType` ''S.Step
inspect $ 'mapMaybeM `hasNoType` ''FL.Step
inspect $ 'mapMaybeM `hasNoType` ''SPEC
#endif

{-# INLINE catMaybes #-}
catMaybes :: Int -> Int -> IO ()
catMaybes size start =
    drainTransformationDefault (size + start) (UF.catMaybes . UF.map Just) start

#ifdef INSPECTION
inspect $ 'catMaybes `hasNoType` ''S.Step
inspect $ 'catMaybes `hasNoType` ''FL.Step
inspect $ 'catMaybes `hasNoType` ''SPEC
#endif

-------------------------------------------------------------------------------
-- Stream combination
-------------------------------------------------------------------------------

{-# INLINE eitherLeft #-}
eitherLeft :: Int -> Int -> IO ()
eitherLeft size start =
    drainGeneration
        (UF.either (source (size + start)) (source (size + start)))
        (Left start)

#ifdef INSPECTION
inspect $ 'eitherLeft `hasNoType` ''S.Step
inspect $ 'eitherLeft `hasNoType` ''FL.Step
inspect $ 'eitherLeft `hasNoType` ''SPEC
#endif

{-# INLINE zipRepeat #-}
zipRepeat :: Int -> Int -> IO ()
zipRepeat size start =
    drainGeneration (UF.zipRepeat (source (size + start))) (start, start)

#ifdef INSPECTION
inspect $ 'zipRepeat `hasNoType` ''S.Step
inspect $ 'zipRepeat `hasNoType` ''FL.Step
inspect $ 'zipRepeat `hasNoType` ''SPEC
#endif

-------------------------------------------------------------------------------
-- Applicative
-------------------------------------------------------------------------------

nthRoot :: Double -> Int -> Int
nthRoot n value = round (fromIntegral value**(1/n))

{-# INLINE innerJoin #-}
innerJoin :: Int -> Int -> IO ()
innerJoin value start =
    let end = start + nthRoot 2 value
        s = source end
    in UF.fold FL.drain (UF.innerJoin (==) s s) start

#ifdef INSPECTION
inspect $ 'innerJoin `hasNoType` ''S.Step
inspect $ 'innerJoin `hasNoType` ''FL.Step
inspect $ 'innerJoin `hasNoType` ''SPEC
inspect $ 'innerJoin `hasNoType` ''Producer.CrossState
#endif

-------------------------------------------------------------------------------
-- Resource management
-------------------------------------------------------------------------------

{-# INLINE before #-}
before :: Int -> Int -> IO ()
before size start =
    drainTransformationDefault (size + start) (UF.before (\_ -> return ())) start

#ifdef INSPECTION
inspect $ 'before `hasNoType` ''S.Step
inspect $ 'before `hasNoType` ''FL.Step
inspect $ 'before `hasNoType` ''SPEC
#endif

{-# INLINE after_ #-}
after_ :: Int -> Int -> IO ()
after_ size start =
    drainTransformationDefault (size + start) (UF.after_ (\_ -> return ())) start

#ifdef INSPECTION
inspect $ 'after_ `hasNoType` ''S.Step
inspect $ 'after_ `hasNoType` ''FL.Step
inspect $ 'after_ `hasNoType` ''SPEC
#endif

{-# INLINE afterIO #-}
afterIO :: Int -> Int -> IO ()
afterIO size start =
    UF.fold FL.drain
        (UF.afterIO (\_ -> return ())
            (UF.supplySecond (size + start) UF.enumerateFromToIntegral))
        start

#ifdef INSPECTION
inspect $ 'afterIO `hasNoType` ''S.Step
inspect $ 'afterIO `hasNoType` ''FL.Step
inspect $ 'afterIO `hasNoType` ''SPEC
#endif

{-# INLINE finallyIO #-}
finallyIO :: Int -> Int -> IO ()
finallyIO size start =
    UF.fold FL.drain
        (UF.finallyIO (\_ -> return ())
            (UF.supplySecond (size + start) UF.enumerateFromToIntegral))
        start

-- 'finallyIO' and 'bracketIO' wrap the step function in exception handlers,
-- so 'Step' constructors are not eliminated.
#ifdef INSPECTION
-- inspect $ 'finallyIO `hasNoType` ''S.Step
inspect $ 'finallyIO `hasNoType` ''FL.Step
inspect $ 'finallyIO `hasNoType` ''SPEC
#endif

{-# INLINE bracketIO #-}
bracketIO :: Int -> Int -> IO ()
bracketIO size start =
    UF.fold FL.drain
        (UF.bracketIO return (\_ -> return ())
            (UF.supplySecond (size + start) UF.enumerateFromToIntegral))
        start

#ifdef INSPECTION
-- inspect $ 'bracketIO `hasNoType` ''S.Step
inspect $ 'bracketIO `hasNoType` ''FL.Step
inspect $ 'bracketIO `hasNoType` ''SPEC
#endif

lf :: Word8
lf = fromIntegral (ord '\n')

-- | Split on line feed.
foldManySepBy :: Handle -> IO Int
foldManySepBy =
    let u = UF.foldMany (FL.takeEndBy_ (== lf) FL.drain) FH.reader
     in UF.fold FL.length u

-- Handle-based fold splitting ('FoldMany' Fuse annotation is disabled so
-- 'Step' is not eliminated, but 'FL.Step' and 'SPEC' should still vanish.)
#ifdef INSPECTION
-- inspect $ 'foldManySepBy `hasNoType` ''S.Step
inspect $ 'foldManySepBy `hasNoType` ''FL.Step
inspect $ 'foldManySepBy `hasNoType` ''SPEC
#endif

-------------------------------------------------------------------------------
-- Benchmarks
-------------------------------------------------------------------------------

moduleName :: String
moduleName = "Data.Unfold"

o_1_space_transformation_input :: Int -> [(SpaceComplexity, Benchmark)]
o_1_space_transformation_input size =
    [ (SpaceO_1, bgroup
          "transformation/input"
          [ benchIO "discardFirst" $ discardFirst size
          , benchIO "discardSecond" $ discardSecond size
          ])
    ]

o_1_space_generation :: Int -> [(SpaceComplexity, Benchmark)]
o_1_space_generation size =
    [ (SpaceO_1, bgroup
          "generation"
          [ benchIO "fromStream" $ fromStream size
          , benchIO "fromStreamK" $ fromStreamK size
          , benchIO "fromStreamD" $ fromStreamD size
          , benchIO "nilM" $ nilM size
          , benchIO "nil" $ nil size
          , benchIO "consM" $ consM size
          , benchIO "fromListM" $ fromListM size
          , benchIO "replicateM" $ replicateM size
          , benchIO "repeatM" $ repeatM size
          , benchIO "repeat" $ repeat size
          , benchIO "iterateM" $ iterateM size
          , benchIO "fromIndicesM" $ fromIndicesM size
          ])
    ]

o_1_space_transformation :: Int -> [(SpaceComplexity, Benchmark)]
o_1_space_transformation size =
    [ (SpaceO_1, bgroup
          "transformation"
          [ benchIO "postscan" $ postscan size
          , benchIO "scanl" $ scanl size
          , benchIO "scanlMany" $ scanlMany size
          ])
    ]

o_1_space_filtering :: Int -> [(SpaceComplexity, Benchmark)]
o_1_space_filtering size =
    [ (SpaceO_1, bgroup
          "filtering"
          [ benchIO "take" $ take size
          , benchIO "filter" $ filter size
          , benchIO "filterM" $ filterM size
          , benchIO "dropOne" $ dropOne size
          , benchIO "dropAll" $ dropAll size
          , benchIO "dropWhileTrue" $ dropWhileTrue size
          , benchIO "dropWhileFalse" $ dropWhileFalse size
          , benchIO "dropWhileMTrue" $ dropWhileMTrue size
          , benchIO "dropWhileMFalse" $ dropWhileMFalse size
          , benchIO "dropWhile" $ dropWhile size
          , benchIO "mapMaybe" $ mapMaybe size
          , benchIO "mapMaybeM" $ mapMaybeM size
          , benchIO "catMaybes" $ catMaybes size
          ])
    ]

o_1_space_zip :: Int -> [(SpaceComplexity, Benchmark)]
o_1_space_zip size =
    [ (SpaceO_1, bgroup
          "zip"
          [ benchIO "eitherLeft" $ eitherLeft size
          , benchIO "zipRepeat" $ zipRepeat size
          ])
    ]

o_1_space_nested :: BenchEnv -> Int -> [(SpaceComplexity, Benchmark)]
o_1_space_nested env size =
    [ (SpaceO_1, bgroup
          "nested"
          [ benchIO "innerJoin outer=inner=(sqrt Max)" $ innerJoin size
          , mkBench "foldMany (Fold.takeEndBy_ (== lf) Fold.drain)" env
            $ \inh _ -> foldManySepBy inh
          ])
    ]

o_1_space_resource_management :: Int -> [(SpaceComplexity, Benchmark)]
o_1_space_resource_management size =
    [ (SpaceO_1, bgroup
          "resource-management"
          [ benchIO "before" $ before size
          , benchIO "after_" $ after_ size
          , benchIO "afterIO" $ afterIO size
          , benchIO "finallyIO" $ finallyIO size
          , benchIO "bracketIO" $ bracketIO size
          ])
    ]

-------------------------------------------------------------------------------
-- Unfold Exception Benchmarks
-------------------------------------------------------------------------------
-- | Send the file contents to /dev/null with exception handling
readWriteOnExceptionUnfold :: Handle -> Handle -> IO ()
readWriteOnExceptionUnfold inh devNull =
    let readEx = UF.onException (\_ -> hClose inh) FH.reader
    in S.fold (FH.write devNull) $ S.unfold readEx inh

#ifdef INSPECTION
-- inspect $ hasNoTypeClasses 'readWriteOnExceptionUnfold
-- inspect $ 'readWriteOnExceptionUnfold `hasNoType` ''Step
#endif

-- | Send the file contents to /dev/null with exception handling
readWriteHandleExceptionUnfold :: Handle -> Handle -> IO ()
readWriteHandleExceptionUnfold inh devNull =
    let handler (_e :: SomeException) = hClose inh >> return 10
        readEx = UF.handle (UF.functionM handler) FH.reader
    in S.fold (FH.write devNull) $ S.unfold readEx inh

#ifdef INSPECTION
-- hasNoTypeClasses started failing in GHC 9.4.4
-- inspect $ hasNoTypeClasses 'readWriteHandleExceptionUnfold
-- inspect $ 'readWriteHandleExceptionUnfold `hasNoType` ''Step
#endif

-- | Send the file contents to /dev/null with exception handling
readWriteFinally_Unfold :: Handle -> Handle -> IO ()
readWriteFinally_Unfold inh devNull =
    let readEx = UF.finally_ (\_ -> hClose inh) FH.reader
    in S.fold (FH.write devNull) $ S.unfold readEx inh

#ifdef INSPECTION
#if __GLASGOW_HASKELL__ >= 906
inspect $ hasNoTypeClassesExcept 'readWriteFinally_Unfold [''MonadCatch]
#else
inspect $ hasNoTypeClasses 'readWriteFinally_Unfold
#endif
-- inspect $ 'readWriteFinally_Unfold `hasNoType` ''Step
#endif

-- | Send the file contents to /dev/null with exception handling
readWriteBracket_Unfold :: Handle -> Handle -> IO ()
readWriteBracket_Unfold inh devNull =
    let readEx = UF.bracket_ return (\_ -> hClose inh) FH.reader
    in S.fold (FH.write devNull) $ S.unfold readEx inh

#ifdef INSPECTION
#if __GLASGOW_HASKELL__ >= 906
inspect $ hasNoTypeClassesExcept 'readWriteBracket_Unfold [''MonadCatch]
#else
inspect $ hasNoTypeClasses 'readWriteBracket_Unfold
#endif
-- inspect $ 'readWriteBracket_Unfold `hasNoType` ''S.Step
#endif

o_1_space_copy_read_exceptions :: BenchEnv -> [(SpaceComplexity, Benchmark)]
o_1_space_copy_read_exceptions env =
    [ (SpaceO_1, bgroup "exceptions"
       [ mkBenchSmall "UF.onException" env $ \inh _ ->
           readWriteOnExceptionUnfold inh (nullH env)
       , mkBenchSmall "UF.handle" env $ \inh _ ->
           readWriteHandleExceptionUnfold inh (nullH env)
       , mkBenchSmall "UF.finally_" env $ \inh _ ->
           readWriteFinally_Unfold inh (nullH env)
       , mkBenchSmall "UF.bracket_" env $ \inh _ ->
           readWriteBracket_Unfold inh (nullH env)
        ])
    ]


-------------------------------------------------------------------------------
-- Driver
-------------------------------------------------------------------------------

main :: IO ()
main = do
#ifndef FUSION_CHECK
    env <- mkHandleBenchEnv
    runWithCLIOpts defaultStreamSize (allBenchmarks env)

    where

    allBenchmarks env size =
        let allBenches = Prelude.concat
                  [ Type.benchmarks size
                  , Enumeration.benchmarks size
                  , o_1_space_transformation_input size
                  , o_1_space_generation size
                  , o_1_space_transformation size
                  , o_1_space_filtering size
                  , o_1_space_zip size
                  , o_1_space_nested env size
                  , o_1_space_resource_management size
                  , o_1_space_copy_read_exceptions env
                  ]
            get x = Prelude.map snd $ Prelude.filter ((==) x . fst) allBenches
            o_1_space = get SpaceO_1
            o_n_space = get SpaceO_n
        in
        [ bgroup (o_1_space_prefix moduleName) o_1_space
        , bgroup (o_n_space_prefix moduleName) o_n_space
        ]
#else
    -- Enable FUSION_CHECK macro at the beginning of the file
    -- Enable one benchmark below, and run the benchmark
    -- Check the .dump-simpl output
    let value = 100000
    Type.lmapM value 0
    return ()
#endif
