-- |
-- Module      : Scanl.Type
-- Copyright   : (c) 2024 Composewell
--
-- License     : MIT
-- Maintainer  : streamly@composewell.com

#undef FUSION_CHECK
#ifdef FUSION_CHECK
{-# OPTIONS_GHC -fplugin-opt=Fusion.Plugin:verbose=2 #-}
{-# OPTIONS_GHC -ddump-simpl -ddump-to-file -dsuppress-all #-}
#endif

#ifdef __HADDOCK_VERSION__
#undef INSPECTION
#endif

#ifdef INSPECTION
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fplugin Test.Inspection.Plugin #-}
#endif

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- Benchmarks for operations exported from Streamly.Internal.Data.Scanl.Type.
module Scanl.Type
    ( benchmarks
    , benchIO
    , withStream
    , withPostscanl
    , withPostscanlDesc
    , withPostscanlDouble
    , withPostscanlMap
    , sourceEnumerate
    ) where

import Streamly.Internal.Data.Scanl (Scanl)
import Streamly.Internal.Data.Stream (Stream)
import Streamly.Internal.Data.StreamK (StreamK)
import System.Random (randomRIO)

import qualified Streamly.Internal.Data.Fold as FL
import qualified Streamly.Internal.Data.Scanl as Scanl
import qualified Streamly.Internal.Data.Stream as Stream

import Fusion.Plugin.Types
import Streamly.Benchmark.Common
import Test.Tasty.Bench
import Prelude hiding (length, maximum, minimum, take, filter)

#ifdef INSPECTION
import Streamly.Internal.Data.Stream (Step(..))
import Test.Inspection
import GHC.Types (SPEC(..))
#endif

-------------------------------------------------------------------------------
-- Helpers
-------------------------------------------------------------------------------

{-# INLINE sourceUnfoldrM #-}
sourceUnfoldrM :: Monad m => Int -> Int -> Stream m Int
sourceUnfoldrM value n = Stream.unfoldrM step n
    where
    step cnt =
        if cnt > n + value
        then return Nothing
        else return (Just (cnt, cnt + 1))

-- XXX We can also try other enumeration APIs here for testing those.
{-# INLINE sourceEnumerate #-}
sourceEnumerate :: (Monad m, Num a, Stream.Enumerable a) => Int -> a
    -> Stream m a
sourceEnumerate len from =
    Stream.enumerateFromThenTo from (from + 1) (from + fromIntegral len)

{-# INLINE sourceEnumerateDesc #-}
sourceEnumerateDesc :: (Monad m, Num a, Stream.Enumerable a) =>
    Int -> a -> Stream m a
sourceEnumerateDesc len from =
    Stream.enumerateFromThenTo
        (from + fromIntegral len)
        (from + fromIntegral (len - 1))
        from

-- XXX For testing the fusion of enumerate based stream generation APIs we can
-- switch the source below to enumerate API sources.
{-# INLINE withStream #-}
withStream :: Int -> (Stream IO Int -> IO b) -> Int -> IO b
withStream n f = f . sourceUnfoldrM n

{-# INLINE withDescStream #-}
withDescStream :: (Num a, Stream.Enumerable a) => Int -> (Stream IO a -> IO b)
    -> Int -> IO b
withDescStream n f = f . sourceEnumerateDesc n . fromIntegral

-- | Run a scan over the stream as a postscan and drain the result.
{-# INLINE withPostscanl #-}
withPostscanl :: Int -> Scanl IO Int b -> Int -> IO ()
withPostscanl n s = withStream n $ Stream.fold FL.drain . Stream.postscanl s

-- | Like 'withPostscanl' but over a 'Double' input stream.
{-# INLINE withPostscanlDouble #-}
withPostscanlDouble :: Int -> Scanl IO Double b -> Int -> IO ()
withPostscanlDouble n s =
    Stream.fold FL.drain . Stream.postscanl s . sourceEnumerate n . fromIntegral

{-# INLINE withPostscanlDesc #-}
withPostscanlDesc :: Int -> Scanl IO Int b -> Int -> IO ()
withPostscanlDesc n s =
    withDescStream n $ Stream.fold FL.drain . Stream.postscanl s

-- | Run a scan over a transformed input stream.
{-# INLINE withPostscanlMap #-}
withPostscanlMap :: Int -> (Int -> a) -> Scanl IO a b -> Int -> IO ()
withPostscanlMap n f s =
    withStream n $ Stream.fold FL.drain . Stream.postscanl s . fmap f

{-# INLINE benchIO #-}
benchIO :: String -> (Int -> Int -> IO ()) -> Int -> Benchmark
benchIO name f value = bench name $ nfIO $ randomRIO (1, 1 :: Int) >>= f value

{-# INLINE oddEven #-}
oddEven :: Int -> Either Int Int
oddEven x = if odd x then Left x else Right x

-------------------------------------------------------------------------------
-- Constructors
-------------------------------------------------------------------------------

{-# ANN scanl' (PermitPatternMatches []) #-}
{-# ANN scanl' (PermitConstructions [''()]) #-}
{-# ANN scanl' (PermitTypeClasses []) #-}
{-# NOINLINE scanl' #-}
scanl' :: Int -> Int -> IO ()
scanl' n = withPostscanl n (Scanl.scanl' (+) 0)

#ifdef INSPECTION
inspect $ 'scanl' `hasNoType` ''Step
inspect $ 'scanl' `hasNoType` ''FL.Step
inspect $ 'scanl' `hasNoType` ''SPEC
#endif

{-# ANN scanlM' (PermitPatternMatches []) #-}
{-# ANN scanlM' (PermitConstructions [''()]) #-}
{-# ANN scanlM' (PermitTypeClasses []) #-}
{-# NOINLINE scanlM' #-}
scanlM' :: Int -> Int -> IO ()
scanlM' n = withPostscanl n (Scanl.scanlM' (\b a -> return (b + a)) (return 0))

#ifdef INSPECTION
inspect $ 'scanlM' `hasNoType` ''Step
inspect $ 'scanlM' `hasNoType` ''FL.Step
inspect $ 'scanlM' `hasNoType` ''SPEC
#endif

{-# ANN scanl1' (PermitPatternMatches []) #-}
{-# ANN scanl1' (PermitConstructions [''()]) #-}
{-# ANN scanl1' (PermitTypeClasses []) #-}
{-# NOINLINE scanl1' #-}
scanl1' :: Int -> Int -> IO ()
scanl1' n = withPostscanl n (Scanl.scanl1' (+))

#ifdef INSPECTION
inspect $ 'scanl1' `hasNoType` ''Step
inspect $ 'scanl1' `hasNoType` ''FL.Step
inspect $ 'scanl1' `hasNoType` ''SPEC
#endif

{-# ANN scanl1M' (PermitPatternMatches []) #-}
{-# ANN scanl1M' (PermitConstructions [''()]) #-}
{-# ANN scanl1M' (PermitTypeClasses []) #-}
{-# NOINLINE scanl1M' #-}
scanl1M' :: Int -> Int -> IO ()
scanl1M' n = withPostscanl n (Scanl.scanl1M' (\a b -> return (a + b)))

#ifdef INSPECTION
inspect $ 'scanl1M' `hasNoType` ''Step
inspect $ 'scanl1M' `hasNoType` ''FL.Step
inspect $ 'scanl1M' `hasNoType` ''SPEC
#endif

{-# ANN scant' (PermitPatternMatches []) #-}
{-# ANN scant' (PermitConstructions [''()]) #-}
{-# ANN scant' (PermitTypeClasses []) #-}
{-# NOINLINE scant' #-}
scant' :: Int -> Int -> IO ()
scant' n = withPostscanl n
    (Scanl.scant' (\s a -> Scanl.Partial (s + a)) (FL.Partial 0) id)

#ifdef INSPECTION
inspect $ 'scant' `hasNoType` ''Step
inspect $ 'scant' `hasNoType` ''SPEC
#endif

{-# ANN scantM' (PermitPatternMatches []) #-}
{-# ANN scantM' (PermitConstructions [''()]) #-}
{-# ANN scantM' (PermitTypeClasses []) #-}
{-# NOINLINE scantM' #-}
scantM' :: Int -> Int -> IO ()
scantM' n =
    withPostscanl n
        (Scanl.scantM'
            (\s a -> return (Scanl.Partial (s + a)))
            (return (FL.Partial 0))
            return)

#ifdef INSPECTION
inspect $ 'scantM' `hasNoType` ''Step
inspect $ 'scantM' `hasNoType` ''SPEC
#endif

{-
{-# INLINE mkScanr #-}
mkScanr :: Int -> Int -> IO ()
mkScanr n = withPostscanl n (Scanl.mkScanr (+) 0)

{-# INLINE mkScanrM #-}
mkScanrM :: Int -> Int -> IO ()
mkScanrM n =
    withPostscanl n (Scanl.mkScanrM (\a b -> return (a + b)) (return 0))
-}

-------------------------------------------------------------------------------
-- Reducers
-------------------------------------------------------------------------------

{-# ANN drain (PermitPatternMatches []) #-}
{-# ANN drain (PermitConstructions [''()]) #-}
{-# ANN drain (PermitTypeClasses []) #-}
{-# NOINLINE drain #-}
drain :: Int -> Int -> IO ()
drain n = withPostscanl n Scanl.drain

#ifdef INSPECTION
inspect $ 'drain `hasNoType` ''Step
inspect $ 'drain `hasNoType` ''FL.Step
inspect $ 'drain `hasNoType` ''SPEC
#endif

{-# ANN latest (PermitPatternMatches []) #-}
{-# ANN latest (PermitConstructions [''()]) #-}
{-# ANN latest (PermitTypeClasses []) #-}
{-# NOINLINE latest #-}
latest :: Int -> Int -> IO ()
latest n = withPostscanl n Scanl.latest

#ifdef INSPECTION
inspect $ 'latest `hasNoType` ''Step
inspect $ 'latest `hasNoType` ''FL.Step
inspect $ 'latest `hasNoType` ''SPEC
#endif

{-# ANN functionM (PermitPatternMatches []) #-}
{-# ANN functionM (PermitConstructions [''()]) #-}
{-# ANN functionM (PermitTypeClasses []) #-}
{-# NOINLINE functionM #-}
functionM :: Int -> Int -> IO ()
functionM n = withPostscanl n (Scanl.functionM (return . Just))

#ifdef INSPECTION
inspect $ 'functionM `hasNoType` ''Step
inspect $ 'functionM `hasNoType` ''FL.Step
inspect $ 'functionM `hasNoType` ''SPEC
#endif

{-# ANN genericLength (PermitPatternMatches []) #-}
{-# ANN genericLength (PermitConstructions [''()]) #-}
{-# ANN genericLength (PermitTypeClasses []) #-}
{-# NOINLINE genericLength #-}
genericLength :: Int -> Int -> IO ()
genericLength n = withPostscanl n (Scanl.genericLength :: Scanl IO Int Int)

#ifdef INSPECTION
inspect $ 'genericLength `hasNoType` ''Step
inspect $ 'genericLength `hasNoType` ''FL.Step
inspect $ 'genericLength `hasNoType` ''SPEC
#endif

{-# ANN length (PermitPatternMatches []) #-}
{-# ANN length (PermitConstructions [''()]) #-}
{-# ANN length (PermitTypeClasses []) #-}
{-# NOINLINE length #-}
length :: Int -> Int -> IO ()
length n = withPostscanl n Scanl.length

#ifdef INSPECTION
inspect $ 'length `hasNoType` ''Step
inspect $ 'length `hasNoType` ''FL.Step
inspect $ 'length `hasNoType` ''SPEC
#endif

{-# ANN maximumBy (PermitPatternMatches []) #-}
{-# ANN maximumBy (PermitConstructions [''()]) #-}
{-# ANN maximumBy (PermitTypeClasses []) #-}
{-# NOINLINE maximumBy #-}
maximumBy :: Int -> Int -> IO ()
maximumBy n = withPostscanl n (Scanl.maximumBy compare)

#ifdef INSPECTION
inspect $ 'maximumBy `hasNoType` ''Step
inspect $ 'maximumBy `hasNoType` ''FL.Step
inspect $ 'maximumBy `hasNoType` ''SPEC
#endif

{-# ANN maximum (PermitPatternMatches []) #-}
{-# ANN maximum (PermitConstructions [''()]) #-}
{-# ANN maximum (PermitTypeClasses []) #-}
{-# NOINLINE maximum #-}
maximum :: Int -> Int -> IO ()
maximum n = withPostscanl n Scanl.maximum

#ifdef INSPECTION
inspect $ 'maximum `hasNoType` ''Step
inspect $ 'maximum `hasNoType` ''FL.Step
inspect $ 'maximum `hasNoType` ''SPEC
#endif

{-# ANN minimumBy (PermitPatternMatches []) #-}
{-# ANN minimumBy (PermitConstructions [''()]) #-}
{-# ANN minimumBy (PermitTypeClasses []) #-}
{-# NOINLINE minimumBy #-}
minimumBy :: Int -> Int -> IO ()
minimumBy n = withPostscanl n (Scanl.minimumBy compare)

#ifdef INSPECTION
inspect $ 'minimumBy `hasNoType` ''Step
inspect $ 'minimumBy `hasNoType` ''FL.Step
inspect $ 'minimumBy `hasNoType` ''SPEC
#endif

{-# ANN minimum (PermitPatternMatches []) #-}
{-# ANN minimum (PermitConstructions [''()]) #-}
{-# ANN minimum (PermitTypeClasses []) #-}
{-# NOINLINE minimum #-}
minimum :: Int -> Int -> IO ()
minimum n = withPostscanl n Scanl.minimum

#ifdef INSPECTION
inspect $ 'minimum `hasNoType` ''Step
inspect $ 'minimum `hasNoType` ''FL.Step
inspect $ 'minimum `hasNoType` ''SPEC
#endif

{-# ANN rangeBy (PermitPatternMatches []) #-}
{-# ANN rangeBy (PermitConstructions [''()]) #-}
{-# ANN rangeBy (PermitTypeClasses []) #-}
{-# NOINLINE rangeBy #-}
rangeBy :: Int -> Int -> IO ()
rangeBy n = withPostscanl n (Scanl.rangeBy compare)

#ifdef INSPECTION
inspect $ 'rangeBy `hasNoType` ''Step
inspect $ 'rangeBy `hasNoType` ''FL.Step
inspect $ 'rangeBy `hasNoType` ''SPEC
#endif

{-# ANN range (PermitPatternMatches []) #-}
{-# ANN range (PermitConstructions [''()]) #-}
{-# ANN range (PermitTypeClasses []) #-}
{-# NOINLINE range #-}
range :: Int -> Int -> IO ()
range n = withPostscanl n Scanl.range

#ifdef INSPECTION
inspect $ 'range `hasNoType` ''Step
inspect $ 'range `hasNoType` ''FL.Step
inspect $ 'range `hasNoType` ''SPEC
#endif

-------------------------------------------------------------------------------
-- Mapping
-------------------------------------------------------------------------------

{-# ANN rmapM (PermitPatternMatches []) #-}
{-# ANN rmapM (PermitConstructions [''()]) #-}
{-# ANN rmapM (PermitTypeClasses []) #-}
{-# NOINLINE rmapM #-}
rmapM :: Int -> Int -> IO ()
rmapM n = withPostscanl n (Scanl.rmapM return Scanl.drain)

#ifdef INSPECTION
inspect $ 'rmapM `hasNoType` ''Step
inspect $ 'rmapM `hasNoType` ''FL.Step
inspect $ 'rmapM `hasNoType` ''SPEC
#endif

{-# ANN lmap (PermitPatternMatches []) #-}
{-# ANN lmap (PermitConstructions [''()]) #-}
{-# ANN lmap (PermitTypeClasses []) #-}
{-# NOINLINE lmap #-}
lmap :: Int -> Int -> IO ()
lmap n = withPostscanl n (Scanl.lmap (+ 1) Scanl.drain)

#ifdef INSPECTION
inspect $ 'lmap `hasNoType` ''Step
inspect $ 'lmap `hasNoType` ''FL.Step
inspect $ 'lmap `hasNoType` ''SPEC
#endif

{-# ANN lmapM (PermitPatternMatches []) #-}
{-# ANN lmapM (PermitConstructions [''()]) #-}
{-# ANN lmapM (PermitTypeClasses []) #-}
{-# NOINLINE lmapM #-}
lmapM :: Int -> Int -> IO ()
lmapM n = withPostscanl n (Scanl.lmapM return Scanl.drain)

#ifdef INSPECTION
inspect $ 'lmapM `hasNoType` ''Step
inspect $ 'lmapM `hasNoType` ''FL.Step
inspect $ 'lmapM `hasNoType` ''SPEC
#endif

{-# ANN postscanl (PermitPatternMatches []) #-}
{-# ANN postscanl (PermitConstructions [''()]) #-}
{-# ANN postscanl (PermitTypeClasses []) #-}
{-# NOINLINE postscanl #-}
postscanl :: Int -> Int -> IO ()
postscanl n = withPostscanl n (Scanl.postscanl Scanl.length Scanl.drain)

#ifdef INSPECTION
inspect $ 'postscanl `hasNoType` ''Step
inspect $ 'postscanl `hasNoType` ''SPEC
#endif

-------------------------------------------------------------------------------
-- Filtering
-------------------------------------------------------------------------------

{-# ANN catMaybes (PermitPatternMatches []) #-}
{-# ANN catMaybes (PermitConstructions [''()]) #-}
{-# ANN catMaybes (PermitTypeClasses []) #-}
{-# NOINLINE catMaybes #-}
catMaybes :: Int -> Int -> IO ()
catMaybes n = withPostscanlMap n Just (Scanl.catMaybes Scanl.length)

#ifdef INSPECTION
inspect $ 'catMaybes `hasNoType` ''Step
inspect $ 'catMaybes `hasNoType` ''FL.Step
inspect $ 'catMaybes `hasNoType` ''SPEC
#endif

{-# ANN postscanlMaybe (PermitPatternMatches []) #-}
{-# ANN postscanlMaybe (PermitConstructions [''()]) #-}
{-# ANN postscanlMaybe (PermitTypeClasses []) #-}
{-# NOINLINE postscanlMaybe #-}
postscanlMaybe :: Int -> Int -> IO ()
postscanlMaybe n =
    withPostscanl n (Scanl.postscanlMaybe (Scanl.filtering even) Scanl.drain)

#ifdef INSPECTION
inspect $ 'postscanlMaybe `hasNoType` ''Step
inspect $ 'postscanlMaybe `hasNoType` ''SPEC
#endif

{-# ANN filter (PermitPatternMatches []) #-}
{-# ANN filter (PermitConstructions [''()]) #-}
{-# ANN filter (PermitTypeClasses []) #-}
{-# NOINLINE filter #-}
filter :: Int -> Int -> IO ()
filter n = withPostscanl n (Scanl.filter even Scanl.drain)

#ifdef INSPECTION
inspect $ 'filter `hasNoType` ''Step
inspect $ 'filter `hasNoType` ''FL.Step
inspect $ 'filter `hasNoType` ''SPEC
#endif

{-# ANN filtering (PermitPatternMatches []) #-}
{-# ANN filtering (PermitConstructions [''()]) #-}
{-# ANN filtering (PermitTypeClasses []) #-}
{-# NOINLINE filtering #-}
filtering :: Int -> Int -> IO ()
filtering n = withPostscanl n (Scanl.filtering even)

#ifdef INSPECTION
inspect $ 'filtering `hasNoType` ''Step
inspect $ 'filtering `hasNoType` ''FL.Step
inspect $ 'filtering `hasNoType` ''SPEC
#endif

{-# ANN filterM (PermitPatternMatches []) #-}
{-# ANN filterM (PermitConstructions [''()]) #-}
{-# ANN filterM (PermitTypeClasses []) #-}
{-# NOINLINE filterM #-}
filterM :: Int -> Int -> IO ()
filterM n = withPostscanl n (Scanl.filterM (return . even) Scanl.drain)

#ifdef INSPECTION
inspect $ 'filterM `hasNoType` ''Step
inspect $ 'filterM `hasNoType` ''FL.Step
inspect $ 'filterM `hasNoType` ''SPEC
#endif

{-# ANN catLefts (PermitPatternMatches []) #-}
{-# ANN catLefts (PermitConstructions [''()]) #-}
{-# ANN catLefts (PermitTypeClasses []) #-}
{-# NOINLINE catLefts #-}
catLefts :: Int -> Int -> IO ()
catLefts n = withPostscanlMap n (Left :: Int -> Either Int Int)
    (Scanl.catLefts Scanl.length)

#ifdef INSPECTION
inspect $ 'catLefts `hasNoType` ''Step
inspect $ 'catLefts `hasNoType` ''SPEC
#endif

{-# ANN catRights (PermitPatternMatches []) #-}
{-# ANN catRights (PermitConstructions [''()]) #-}
{-# ANN catRights (PermitTypeClasses []) #-}
{-# NOINLINE catRights #-}
catRights :: Int -> Int -> IO ()
catRights n = withPostscanlMap n (Right :: Int -> Either Int Int)
    (Scanl.catRights Scanl.length)

#ifdef INSPECTION
inspect $ 'catRights `hasNoType` ''Step
inspect $ 'catRights `hasNoType` ''SPEC
#endif

{-# ANN catEithers (PermitPatternMatches []) #-}
{-# ANN catEithers (PermitConstructions [''()]) #-}
{-# ANN catEithers (PermitTypeClasses []) #-}
{-# NOINLINE catEithers #-}
catEithers :: Int -> Int -> IO ()
catEithers n = withPostscanlMap n oddEven (Scanl.catEithers Scanl.length)

#ifdef INSPECTION
inspect $ 'catEithers `hasNoType` ''Step
inspect $ 'catEithers `hasNoType` ''SPEC
#endif

-------------------------------------------------------------------------------
-- Trimming
-------------------------------------------------------------------------------

{-# ANN take (PermitPatternMatches [''Int]) #-}
{-# ANN take (PermitConstructions [''Int,''()]) #-}
{-# ANN take (PermitTypeClasses []) #-}
{-# NOINLINE take #-}
take :: Int -> Int -> IO ()
take n = withPostscanl n (Scanl.take n Scanl.drain)

#ifdef INSPECTION
inspect $ 'take `hasNoType` ''Step
inspect $ 'take `hasNoType` ''FL.Step
inspect $ 'take `hasNoType` ''SPEC
#endif

{-# ANN taking (PermitPatternMatches [''Int]) #-}
{-# ANN taking (PermitConstructions [''Int,''()]) #-}
{-# ANN taking (PermitTypeClasses []) #-}
{-# NOINLINE taking #-}
taking :: Int -> Int -> IO ()
taking n = withPostscanl n (Scanl.taking n)

#ifdef INSPECTION
inspect $ 'taking `hasNoType` ''Step
inspect $ 'taking `hasNoType` ''FL.Step
inspect $ 'taking `hasNoType` ''SPEC
#endif

{-# ANN takeEndBy_ (PermitPatternMatches []) #-}
{-# ANN takeEndBy_ (PermitConstructions [''()]) #-}
{-# ANN takeEndBy_ (PermitTypeClasses []) #-}
{-# NOINLINE takeEndBy_ #-}
takeEndBy_ :: Int -> Int -> IO ()
takeEndBy_ n = withPostscanl n (Scanl.takeEndBy_ (>= n) Scanl.drain)

#ifdef INSPECTION
inspect $ 'takeEndBy_ `hasNoType` ''Step
inspect $ 'takeEndBy_ `hasNoType` ''FL.Step
inspect $ 'takeEndBy_ `hasNoType` ''SPEC
#endif

{-# ANN takeEndBy (PermitPatternMatches []) #-}
{-# ANN takeEndBy (PermitConstructions [''()]) #-}
{-# ANN takeEndBy (PermitTypeClasses []) #-}
{-# NOINLINE takeEndBy #-}
takeEndBy :: Int -> Int -> IO ()
takeEndBy n = withPostscanl n (Scanl.takeEndBy (>= n) Scanl.drain)

#ifdef INSPECTION
inspect $ 'takeEndBy `hasNoType` ''Step
inspect $ 'takeEndBy `hasNoType` ''FL.Step
inspect $ 'takeEndBy `hasNoType` ''SPEC
#endif

{-# ANN dropping (PermitPatternMatches [''Int]) #-}
{-# ANN dropping (PermitConstructions [''Int,''()]) #-}
{-# ANN dropping (PermitTypeClasses []) #-}
{-# NOINLINE dropping #-}
dropping :: Int -> Int -> IO ()
dropping n = withPostscanl n (Scanl.dropping n)

#ifdef INSPECTION
inspect $ 'dropping `hasNoType` ''Step
inspect $ 'dropping `hasNoType` ''FL.Step
inspect $ 'dropping `hasNoType` ''SPEC
#endif

-------------------------------------------------------------------------------
-- Distributing
-------------------------------------------------------------------------------

{-# ANN teeWith (PermitPatternMatches []) #-}
{-# ANN teeWith (PermitConstructions [''()]) #-}
{-# ANN teeWith (PermitTypeClasses []) #-}
{-# NOINLINE teeWith #-}
teeWith :: Int -> Int -> IO ()
teeWith n = withPostscanl n (Scanl.teeWith (,) Scanl.length Scanl.latest)

#ifdef INSPECTION
inspect $ 'teeWith `hasNoType` ''Step
inspect $ 'teeWith `hasNoType` ''FL.Step
inspect $ 'teeWith `hasNoType` ''SPEC
#endif

-------------------------------------------------------------------------------
-- O(n) heap: building structures
-------------------------------------------------------------------------------

{-# ANN toList (PermitPatternMatches [''[]]) #-}
{-# ANN toList (PermitConstructions [''Int,''[],''()]) #-}
{-# ANN toList (PermitTypeClasses []) #-}
{-# NOINLINE toList #-}
toList :: Int -> Int -> IO ()
toList n = withPostscanl n Scanl.toList

{-# ANN toStreamK (PermitPatternMatches []) #-}
{-# ANN toStreamK (PermitConstructions [''Int,''()]) #-}
{-# ANN toStreamK (PermitTypeClasses []) #-}
{-# NOINLINE toStreamK #-}
toStreamK :: Int -> Int -> IO ()
toStreamK n = withPostscanl n (Scanl.toStreamK :: Scanl IO Int (StreamK IO Int))

{-# ANN toStreamKRev (PermitPatternMatches []) #-}
{-# ANN toStreamKRev (PermitConstructions [''Int,''()]) #-}
{-# ANN toStreamKRev (PermitTypeClasses []) #-}
{-# NOINLINE toStreamKRev #-}
toStreamKRev :: Int -> Int -> IO ()
toStreamKRev n = withPostscanl n
    (Scanl.toStreamKRev :: Scanl IO Int (StreamK IO Int))

-------------------------------------------------------------------------------
-- Benchmarks
-------------------------------------------------------------------------------

-- Benchmark naming: name each benchmark (and its IO action) after the exported
-- function it benchmarks, using combinator_dimension1_dimension2..., where the
-- dimensions are optional variants/type specializations (used esp. when more
-- than one specialization is benchmarked). Keep extra info in parenthetical
-- notes in the description; these also disambiguate benchmarks that reuse a
-- single IO action with different arguments. If the name has a trailing
-- underscore, add one more underscore.
benchmarks :: Int -> [(SpaceComplexity, Benchmark)]
benchmarks value =
    fmap (SpaceO_1,)
        [ benchIO "scanl'" scanl' value
        , benchIO "scanlM'" scanlM' value
        , benchIO "scanl1'" scanl1' value
        , benchIO "scanl1M'" scanl1M' value
        , benchIO "scant'" scant' value
        , benchIO "scantM'" scantM' value
        -- XXX these take too much stack and do not finish
        -- , benchIO "mkScanr" mkScanr value
        -- , benchIO "mkScanrM" mkScanrM value
        , benchIO "drain" drain value
        , benchIO "latest" latest value
        , benchIO "functionM" functionM value
        , benchIO "genericLength" genericLength value
        , benchIO "length" length value
        , benchIO "maximumBy" maximumBy value
        , benchIO "maximum" maximum value
        , benchIO "minimumBy" minimumBy value
        , benchIO "minimum" minimum value
        , benchIO "rangeBy" rangeBy value
        , benchIO "range" range value
        , benchIO "rmapM" rmapM value
        , benchIO "lmap" lmap value
        , benchIO "lmapM" lmapM value
        , benchIO "postscanl" postscanl value
        , benchIO "catMaybes" catMaybes value
        , benchIO "postscanlMaybe (filtering even)" postscanlMaybe value
        , benchIO "filter (even)" filter value
        , benchIO "filtering (even)" filtering value
        , benchIO "filterM (even)" filterM value
        , benchIO "catLefts" catLefts value
        , benchIO "catRights" catRights value
        , benchIO "catEithers" catEithers value
        , benchIO "take" take value
        , benchIO "taking" taking value
        , benchIO "takeEndBy_" takeEndBy_ value
        , benchIO "takeEndBy" takeEndBy value
        , benchIO "dropping" dropping value
        , benchIO "teeWith (length, latest)" teeWith value
        ]
    ++ fmap (HeapO_n,)
        [ benchIO "toList (1/1000)" toList (value `div` 1000)
        , benchIO "toStreamK (1/1000)" toStreamK (value `div` 1000)
        , benchIO "toStreamKRev" toStreamKRev value
        ]
