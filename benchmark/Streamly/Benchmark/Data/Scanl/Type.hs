-- |
-- Module      : Scanl.Type
-- Copyright   : (c) 2024 Composewell
--
-- License     : MIT
-- Maintainer  : streamly@composewell.com

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

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- Benchmarks for operations exported from Streamly.Internal.Data.Scanl.Type.
module Scanl.Type (benchmarks) where

import Streamly.Internal.Data.Scanl (Scanl)
import Streamly.Internal.Data.Stream (Stream)
import System.Random (randomRIO)

import qualified Streamly.Internal.Data.Fold as FL
import qualified Streamly.Internal.Data.Scanl as Scanl
import qualified Streamly.Internal.Data.Stream as Stream

import Streamly.Benchmark.Common
import Test.Tasty.Bench
import Prelude hiding (length, maximum, minimum, take, filter)

#ifdef INSPECTION
import GHC.Types (SPEC(..))
import Streamly.Internal.Data.Stream (Step(..))
import Test.Inspection
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

{-# INLINE withStream #-}
withStream :: Int -> (Stream IO Int -> IO b) -> IO b
withStream n f = randomRIO (1, 1) >>= f . sourceUnfoldrM n

-- | Run a scan over the stream as a postscan and drain the result.
{-# INLINE withPostscanl #-}
withPostscanl :: Int -> Scanl IO Int b -> IO ()
withPostscanl n s = withStream n $ Stream.fold FL.drain . Stream.postscanl s

-- | Run a scan over a transformed input stream.
{-# INLINE withPostscanlMap #-}
withPostscanlMap :: Int -> (Int -> a) -> Scanl IO a b -> IO ()
withPostscanlMap n f s = withStream n $ Stream.fold FL.drain . Stream.postscanl s . fmap f

{-# INLINE benchIO #-}
benchIO :: String -> (Int -> IO ()) -> Int -> Benchmark
benchIO name f value = bench name $ nfIO $ f value

{-# INLINE oddEven #-}
oddEven :: Int -> Either Int Int
oddEven x = if odd x then Left x else Right x

-------------------------------------------------------------------------------
-- Constructors
-------------------------------------------------------------------------------

{-# INLINE scanl' #-}
scanl' :: Int -> IO ()
scanl' n = withPostscanl n (Scanl.scanl' (+) 0)

#ifdef INSPECTION
inspect $ 'scanl' `hasNoType` ''Step
inspect $ 'scanl' `hasNoType` ''FL.Step
inspect $ 'scanl' `hasNoType` ''SPEC
#endif

{-# INLINE scanlM' #-}
scanlM' :: Int -> IO ()
scanlM' n = withPostscanl n (Scanl.scanlM' (\b a -> return (b + a)) (return 0))

#ifdef INSPECTION
inspect $ 'scanlM' `hasNoType` ''Step
inspect $ 'scanlM' `hasNoType` ''FL.Step
inspect $ 'scanlM' `hasNoType` ''SPEC
#endif

{-# INLINE scanl1' #-}
scanl1' :: Int -> IO ()
scanl1' n = withPostscanl n (Scanl.scanl1' (+))

#ifdef INSPECTION
inspect $ 'scanl1' `hasNoType` ''Step
inspect $ 'scanl1' `hasNoType` ''FL.Step
inspect $ 'scanl1' `hasNoType` ''SPEC
#endif

{-# INLINE scanl1M' #-}
scanl1M' :: Int -> IO ()
scanl1M' n = withPostscanl n (Scanl.scanl1M' (\a b -> return (a + b)))

#ifdef INSPECTION
inspect $ 'scanl1M' `hasNoType` ''Step
inspect $ 'scanl1M' `hasNoType` ''FL.Step
inspect $ 'scanl1M' `hasNoType` ''SPEC
#endif

{-# INLINE scant' #-}
scant' :: Int -> IO ()
scant' n = withPostscanl n (Scanl.scant' (\s a -> Scanl.Partial (s + a)) (FL.Partial 0) id)

#ifdef INSPECTION
inspect $ 'scant' `hasNoType` ''Step
inspect $ 'scant' `hasNoType` ''SPEC
#endif

{-# INLINE scantM' #-}
scantM' :: Int -> IO ()
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
mkScanr :: Int -> IO ()
mkScanr n = withPostscanl n (Scanl.mkScanr (+) 0)

{-# INLINE mkScanrM #-}
mkScanrM :: Int -> IO ()
mkScanrM n = withPostscanl n (Scanl.mkScanrM (\a b -> return (a + b)) (return 0))
-}

-------------------------------------------------------------------------------
-- Reducers
-------------------------------------------------------------------------------

{-# INLINE drain #-}
drain :: Int -> IO ()
drain n = withPostscanl n Scanl.drain

#ifdef INSPECTION
inspect $ 'drain `hasNoType` ''Step
inspect $ 'drain `hasNoType` ''FL.Step
inspect $ 'drain `hasNoType` ''SPEC
#endif

{-# INLINE latest #-}
latest :: Int -> IO ()
latest n = withPostscanl n Scanl.latest

#ifdef INSPECTION
inspect $ 'latest `hasNoType` ''Step
inspect $ 'latest `hasNoType` ''FL.Step
inspect $ 'latest `hasNoType` ''SPEC
#endif

{-# INLINE functionM #-}
functionM :: Int -> IO ()
functionM n = withPostscanl n (Scanl.functionM (return . Just))

#ifdef INSPECTION
inspect $ 'functionM `hasNoType` ''Step
inspect $ 'functionM `hasNoType` ''FL.Step
inspect $ 'functionM `hasNoType` ''SPEC
#endif

{-# INLINE genericLength #-}
genericLength :: Int -> IO ()
genericLength n = withPostscanl n (Scanl.genericLength :: Scanl IO Int Int)

#ifdef INSPECTION
inspect $ 'genericLength `hasNoType` ''Step
inspect $ 'genericLength `hasNoType` ''FL.Step
inspect $ 'genericLength `hasNoType` ''SPEC
#endif

{-# INLINE length #-}
length :: Int -> IO ()
length n = withPostscanl n Scanl.length

#ifdef INSPECTION
inspect $ 'length `hasNoType` ''Step
inspect $ 'length `hasNoType` ''FL.Step
inspect $ 'length `hasNoType` ''SPEC
#endif

{-# INLINE maximumBy #-}
maximumBy :: Int -> IO ()
maximumBy n = withPostscanl n (Scanl.maximumBy compare)

#ifdef INSPECTION
inspect $ 'maximumBy `hasNoType` ''Step
inspect $ 'maximumBy `hasNoType` ''FL.Step
inspect $ 'maximumBy `hasNoType` ''SPEC
#endif

{-# INLINE maximum #-}
maximum :: Int -> IO ()
maximum n = withPostscanl n Scanl.maximum

#ifdef INSPECTION
inspect $ 'maximum `hasNoType` ''Step
inspect $ 'maximum `hasNoType` ''FL.Step
inspect $ 'maximum `hasNoType` ''SPEC
#endif

{-# INLINE minimumBy #-}
minimumBy :: Int -> IO ()
minimumBy n = withPostscanl n (Scanl.minimumBy compare)

#ifdef INSPECTION
inspect $ 'minimumBy `hasNoType` ''Step
inspect $ 'minimumBy `hasNoType` ''FL.Step
inspect $ 'minimumBy `hasNoType` ''SPEC
#endif

{-# INLINE minimum #-}
minimum :: Int -> IO ()
minimum n = withPostscanl n Scanl.minimum

#ifdef INSPECTION
inspect $ 'minimum `hasNoType` ''Step
inspect $ 'minimum `hasNoType` ''FL.Step
inspect $ 'minimum `hasNoType` ''SPEC
#endif

{-# INLINE rangeBy #-}
rangeBy :: Int -> IO ()
rangeBy n = withPostscanl n (Scanl.rangeBy compare)

#ifdef INSPECTION
inspect $ 'rangeBy `hasNoType` ''Step
inspect $ 'rangeBy `hasNoType` ''FL.Step
inspect $ 'rangeBy `hasNoType` ''SPEC
#endif

{-# INLINE range #-}
range :: Int -> IO ()
range n = withPostscanl n Scanl.range

#ifdef INSPECTION
inspect $ 'range `hasNoType` ''Step
inspect $ 'range `hasNoType` ''FL.Step
inspect $ 'range `hasNoType` ''SPEC
#endif

-------------------------------------------------------------------------------
-- Mapping
-------------------------------------------------------------------------------

{-# INLINE rmapM #-}
rmapM :: Int -> IO ()
rmapM n = withPostscanl n (Scanl.rmapM return Scanl.drain)

#ifdef INSPECTION
inspect $ 'rmapM `hasNoType` ''Step
inspect $ 'rmapM `hasNoType` ''FL.Step
inspect $ 'rmapM `hasNoType` ''SPEC
#endif

{-# INLINE lmap #-}
lmap :: Int -> IO ()
lmap n = withPostscanl n (Scanl.lmap (+ 1) Scanl.drain)

#ifdef INSPECTION
inspect $ 'lmap `hasNoType` ''Step
inspect $ 'lmap `hasNoType` ''FL.Step
inspect $ 'lmap `hasNoType` ''SPEC
#endif

{-# INLINE lmapM #-}
lmapM :: Int -> IO ()
lmapM n = withPostscanl n (Scanl.lmapM return Scanl.drain)

#ifdef INSPECTION
inspect $ 'lmapM `hasNoType` ''Step
inspect $ 'lmapM `hasNoType` ''FL.Step
inspect $ 'lmapM `hasNoType` ''SPEC
#endif

{-# INLINE postscanl #-}
postscanl :: Int -> IO ()
postscanl n = withPostscanl n (Scanl.postscanl Scanl.length Scanl.drain)

#ifdef INSPECTION
inspect $ 'postscanl `hasNoType` ''Step
inspect $ 'postscanl `hasNoType` ''SPEC
#endif

-------------------------------------------------------------------------------
-- Filtering
-------------------------------------------------------------------------------

{-# INLINE catMaybes #-}
catMaybes :: Int -> IO ()
catMaybes n = withPostscanlMap n Just (Scanl.catMaybes Scanl.length)

#ifdef INSPECTION
inspect $ 'catMaybes `hasNoType` ''Step
inspect $ 'catMaybes `hasNoType` ''FL.Step
inspect $ 'catMaybes `hasNoType` ''SPEC
#endif

{-# INLINE postscanlMaybe #-}
postscanlMaybe :: Int -> IO ()
postscanlMaybe n = withPostscanl n (Scanl.postscanlMaybe (Scanl.filtering even) Scanl.drain)

#ifdef INSPECTION
inspect $ 'postscanlMaybe `hasNoType` ''Step
inspect $ 'postscanlMaybe `hasNoType` ''SPEC
#endif

{-# INLINE filter #-}
filter :: Int -> IO ()
filter n = withPostscanl n (Scanl.filter even Scanl.drain)

#ifdef INSPECTION
inspect $ 'filter `hasNoType` ''Step
inspect $ 'filter `hasNoType` ''FL.Step
inspect $ 'filter `hasNoType` ''SPEC
#endif

{-# INLINE filtering #-}
filtering :: Int -> IO ()
filtering n = withPostscanl n (Scanl.filtering even)

#ifdef INSPECTION
inspect $ 'filtering `hasNoType` ''Step
inspect $ 'filtering `hasNoType` ''FL.Step
inspect $ 'filtering `hasNoType` ''SPEC
#endif

{-# INLINE filterM #-}
filterM :: Int -> IO ()
filterM n = withPostscanl n (Scanl.filterM (return . even) Scanl.drain)

#ifdef INSPECTION
inspect $ 'filterM `hasNoType` ''Step
inspect $ 'filterM `hasNoType` ''FL.Step
inspect $ 'filterM `hasNoType` ''SPEC
#endif

{-# INLINE catLefts #-}
catLefts :: Int -> IO ()
catLefts n = withPostscanlMap n (Left :: Int -> Either Int Int) (Scanl.catLefts Scanl.length)

#ifdef INSPECTION
inspect $ 'catLefts `hasNoType` ''Step
inspect $ 'catLefts `hasNoType` ''SPEC
#endif

{-# INLINE catRights #-}
catRights :: Int -> IO ()
catRights n = withPostscanlMap n (Right :: Int -> Either Int Int) (Scanl.catRights Scanl.length)

#ifdef INSPECTION
inspect $ 'catRights `hasNoType` ''Step
inspect $ 'catRights `hasNoType` ''SPEC
#endif

{-# INLINE catEithers #-}
catEithers :: Int -> IO ()
catEithers n = withPostscanlMap n oddEven (Scanl.catEithers Scanl.length)

#ifdef INSPECTION
inspect $ 'catEithers `hasNoType` ''Step
inspect $ 'catEithers `hasNoType` ''SPEC
#endif

-------------------------------------------------------------------------------
-- Trimming
-------------------------------------------------------------------------------

{-# INLINE take #-}
take :: Int -> IO ()
take n = withPostscanl n (Scanl.take n Scanl.drain)

#ifdef INSPECTION
inspect $ 'take `hasNoType` ''Step
inspect $ 'take `hasNoType` ''FL.Step
inspect $ 'take `hasNoType` ''SPEC
#endif

{-# INLINE taking #-}
taking :: Int -> IO ()
taking n = withPostscanl n (Scanl.taking n)

#ifdef INSPECTION
inspect $ 'taking `hasNoType` ''Step
inspect $ 'taking `hasNoType` ''FL.Step
inspect $ 'taking `hasNoType` ''SPEC
#endif

{-# INLINE takeEndBy_ #-}
takeEndBy_ :: Int -> IO ()
takeEndBy_ n = withPostscanl n (Scanl.takeEndBy_ (>= n) Scanl.drain)

#ifdef INSPECTION
inspect $ 'takeEndBy_ `hasNoType` ''Step
inspect $ 'takeEndBy_ `hasNoType` ''FL.Step
inspect $ 'takeEndBy_ `hasNoType` ''SPEC
#endif

{-# INLINE takeEndBy #-}
takeEndBy :: Int -> IO ()
takeEndBy n = withPostscanl n (Scanl.takeEndBy (>= n) Scanl.drain)

#ifdef INSPECTION
inspect $ 'takeEndBy `hasNoType` ''Step
inspect $ 'takeEndBy `hasNoType` ''FL.Step
inspect $ 'takeEndBy `hasNoType` ''SPEC
#endif

{-# INLINE dropping #-}
dropping :: Int -> IO ()
dropping n = withPostscanl n (Scanl.dropping n)

#ifdef INSPECTION
inspect $ 'dropping `hasNoType` ''Step
inspect $ 'dropping `hasNoType` ''FL.Step
inspect $ 'dropping `hasNoType` ''SPEC
#endif

-------------------------------------------------------------------------------
-- Distributing
-------------------------------------------------------------------------------

{-# INLINE teeWith #-}
teeWith :: Int -> IO ()
teeWith n = withPostscanl n (Scanl.teeWith (,) Scanl.length Scanl.latest)

#ifdef INSPECTION
inspect $ 'teeWith `hasNoType` ''Step
inspect $ 'teeWith `hasNoType` ''FL.Step
inspect $ 'teeWith `hasNoType` ''SPEC
#endif

-------------------------------------------------------------------------------
-- O(n) heap: building structures
-------------------------------------------------------------------------------

{-# INLINE toList #-}
toList :: Int -> IO ()
toList n = withPostscanl n Scanl.toList

{-# INLINE toStreamK #-}
toStreamK :: Int -> IO ()
toStreamK n = withPostscanl n Scanl.toStreamK

{-# INLINE toStreamKRev #-}
toStreamKRev :: Int -> IO ()
toStreamKRev n = withPostscanl n Scanl.toStreamKRev

-------------------------------------------------------------------------------
-- Benchmarks
-------------------------------------------------------------------------------

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
        , benchIO "filter even" filter value
        , benchIO "filtering even" filtering value
        , benchIO "filterM even" filterM value
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
