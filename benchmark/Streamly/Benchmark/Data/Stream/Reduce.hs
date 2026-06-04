-- |
-- Module      : Stream.Reduce
-- Copyright   : (c) 2018 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}

#ifdef __HADDOCK_VERSION__
#undef INSPECTION
#endif

#ifdef INSPECTION
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fplugin Test.Inspection.Plugin #-}
#endif

module Stream.Reduce (benchmarks) where

#ifdef INSPECTION
import GHC.Types (SPEC(..))
import Test.Inspection
#endif

import Control.DeepSeq (NFData(..))
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Maybe (isJust)
import Data.Monoid (Sum(..))
import GHC.Generics (Generic)
import Streamly.Internal.Data.Stream (Stream)

import qualified Stream.Common as Common
import qualified Streamly.Internal.Data.Fold as FL
import qualified Streamly.Internal.Data.Refold.Type as Refold
import qualified Streamly.Internal.Data.Stream as S

import Test.Tasty.Bench
import Streamly.Benchmark.Common
import Stream.Common
import Prelude hiding (reverse, tail)

-- Apply transformation g count times on a stream of length len
{-# INLINE iterateSource #-}
iterateSource ::
       MonadAsync m
    => (Stream m Int -> Stream m Int)
    -> Int
    -> Int
    -> Int
    -> Stream m Int
iterateSource g count len n = f count (sourceUnfoldrM len n)

    where

    f (0 :: Int) stream = stream
    f i stream = f (i - 1) (g stream)

-------------------------------------------------------------------------------
-- Grouping transformations
-------------------------------------------------------------------------------

{-# INLINE groups #-}
groups :: MonadIO m => Stream m Int -> m ()
groups = Common.drain . S.groupsWhile (==) FL.drain

{-# INLINE groupsWhileLT #-}
groupsWhileLT :: MonadIO m => Stream m Int -> m ()
groupsWhileLT = Common.drain . S.groupsWhile (<) FL.drain

{-# INLINE groupsWhileEq #-}
groupsWhileEq :: MonadIO m => Stream m Int -> m ()
groupsWhileEq = Common.drain . S.groupsWhile (==) FL.drain

{-# INLINE groupsByRollingLT #-}
groupsByRollingLT :: MonadIO m => Stream m Int -> m ()
groupsByRollingLT = Common.drain . S.groupsRollingBy (<) FL.drain

{-# INLINE groupsByRollingEq #-}
groupsByRollingEq :: MonadIO m => Stream m Int -> m ()
groupsByRollingEq = Common.drain . S.groupsRollingBy (==) FL.drain

{-# INLINE foldMany #-}
foldMany :: Monad m => Stream m Int -> m ()
foldMany =
      Common.drain
    . fmap getSum
    . S.foldMany (FL.take 2 FL.mconcat)
    . fmap Sum

{-# INLINE foldMany1 #-}
foldMany1 :: Monad m => Stream m Int -> m ()
foldMany1 =
      Common.drain
    . fmap getSum
    . S.foldManyPost (FL.take 2 FL.mconcat)
    . fmap Sum

{-# INLINE refoldMany #-}
refoldMany :: Monad m => Stream m Int -> m ()
refoldMany =
      Common.drain
    . fmap getSum
    . S.refoldMany (Refold.take 2 Refold.sconcat) (return mempty)
    . fmap Sum

{-# INLINE foldIterateM #-}
foldIterateM :: Monad m => Stream m Int -> m ()
foldIterateM =
    Common.drain
        . fmap getSum
        . S.foldIterateM
            (return . FL.take 2 . FL.sconcat) (return (Sum 0))
        . fmap Sum

{-# INLINE refoldIterateM #-}
refoldIterateM :: Monad m => Stream m Int -> m ()
refoldIterateM =
    Common.drain
        . fmap getSum
        . S.refoldIterateM
            (Refold.take 2 Refold.sconcat) (return (Sum 0))
        . fmap Sum

o_1_space_grouping :: Int -> [Benchmark]
o_1_space_grouping value =
    -- Buffering operations using heap proportional to group/window sizes.
    [ bgroup "grouping"
        [
          benchIOSink value "groups" groups
        , benchIOSink value "groupsWhileLT" groupsWhileLT
        , benchIOSink value "groupsWhileEq" groupsWhileEq
        , benchIOSink value "groupsByRollingLT" groupsByRollingLT
        , benchIOSink value "groupsByRollingEq" groupsByRollingEq
        ,

        -- XXX parseMany/parseIterate benchmarks are in the Parser/ParserD
        -- modules we can bring those here. chunksOf benchmarks are in
        -- Parser/ParserD/Array.Stream/FileSystem.Handle.
          benchIOSink value "foldMany" foldMany
        , benchIOSink value "foldMany1" foldMany1
        , benchIOSink value "refoldMany" refoldMany
        , benchIOSink value "foldIterateM" foldIterateM
        , benchIOSink value "refoldIterateM" refoldIterateM
        ]
    ]

-------------------------------------------------------------------------------
-- Size conserving transformations (reordering, buffering, etc.)
-------------------------------------------------------------------------------

{-# INLINE reverse #-}
reverse :: MonadIO m => Int -> Stream m Int -> m ()
reverse n = composeN n S.reverse

{-# INLINE reverse' #-}
reverse' :: MonadIO m => Int -> Stream m Int -> m ()
reverse' n = composeN n S.reverseUnbox

o_n_heap_buffering :: Int -> [Benchmark]
o_n_heap_buffering value =
    [ bgroup "buffered"
        [
        -- Reversing a stream
          benchIOSink value "reverse" (reverse 1)
        , benchIOSink value "reverse'" (reverse' 1)
        ]
    ]

-------------------------------------------------------------------------------
-- Mixed Transformation
-------------------------------------------------------------------------------

{-# INLINE scanMap #-}
scanMap :: MonadIO m => Int -> Stream m Int -> m ()
scanMap n = composeN n $ fmap (subtract 1) . Common.scanl' (+) 0

{-# INLINE dropMap #-}
dropMap :: MonadIO m => Int -> Stream m Int -> m ()
dropMap n = composeN n $ fmap (subtract 1) . S.drop 1

{-# INLINE dropScan #-}
dropScan :: MonadIO m => Int -> Stream m Int -> m ()
dropScan n = composeN n $ Common.scanl' (+) 0 . S.drop 1

{-# INLINE takeDrop #-}
takeDrop :: MonadIO m => Int -> Int -> Stream m Int -> m ()
takeDrop value n = composeN n $ S.drop 1 . S.take (value + 1)

{-# INLINE takeScan #-}
takeScan :: MonadIO m => Int -> Int -> Stream m Int -> m ()
takeScan value n = composeN n $ Common.scanl' (+) 0 . S.take (value + 1)

{-# INLINE takeMap #-}
takeMap :: MonadIO m => Int -> Int -> Stream m Int -> m ()
takeMap value n = composeN n $ fmap (subtract 1) . S.take (value + 1)

{-# INLINE filterDrop #-}
filterDrop :: MonadIO m => Int -> Int -> Stream m Int -> m ()
filterDrop value n = composeN n $ S.drop 1 . S.filter (<= (value + 1))

{-# INLINE filterTake #-}
filterTake :: MonadIO m => Int -> Int -> Stream m Int -> m ()
filterTake value n = composeN n $ S.take (value + 1) . S.filter (<= (value + 1))

{-# INLINE filterScan #-}
filterScan :: MonadIO m => Int -> Stream m Int -> m ()
filterScan n = composeN n $ Common.scanl' (+) 0 . S.filter (<= maxBound)

{-# INLINE filterScanl1 #-}
filterScanl1 :: MonadIO m => Int -> Stream m Int -> m ()
filterScanl1 n = composeN n $ S.scanl1' (+) . S.filter (<= maxBound)

{-# INLINE filterMap #-}
filterMap :: MonadIO m => Int -> Int -> Stream m Int -> m ()
filterMap value n = composeN n $ fmap (subtract 1) . S.filter (<= (value + 1))

-------------------------------------------------------------------------------
-- Scan and fold
-------------------------------------------------------------------------------

data Pair a b =
    Pair !a !b
    deriving (Generic, NFData)

{-# INLINE sumProductFold #-}
sumProductFold :: Monad m => Stream m Int -> m (Pair Int Int)
sumProductFold =
    Common.foldl' (\(Pair s p) x -> Pair (s + x) (p * x)) (Pair 0 1)

{-# INLINE sumProductScan #-}
sumProductScan :: Monad m => Stream m Int -> m (Pair Int Int)
sumProductScan =
    Common.foldl' (\(Pair _ p) (s0, x) -> Pair s0 (p * x)) (Pair 0 1) .
    Common.scanl' (\(s, _) x -> (s + x, x)) (0, 0)

{-# INLINE foldl'ReduceMap #-}
foldl'ReduceMap :: Monad m => Stream m Int -> m Int
foldl'ReduceMap = fmap (+ 1) . Common.foldl' (+) 0

o_1_space_transformations_mixed :: Int -> [Benchmark]
o_1_space_transformations_mixed value =
    -- scanl-map and foldl-map are equivalent to the scan and fold in the foldl
    -- library. If scan/fold followed by a map is efficient enough we may not
    -- need monolithic implementations of these.
    [ bgroup "mixed"
        [ benchIOSink value "scanl-map" (scanMap 1)
        , benchIOSink value "drop-map" (dropMap 1)
        , benchIOSink value "drop-scan" (dropScan 1)
        , benchIOSink value "take-drop" (takeDrop value 1)
        , benchIOSink value "take-scan" (takeScan value 1)
        , benchIOSink value "take-map" (takeMap value 1)
        , benchIOSink value "filter-drop" (filterDrop value 1)
        , benchIOSink value "filter-take" (filterTake value 1)
        , benchIOSink value "filter-scan" (filterScan 1)
        , benchIOSink value "filter-map" (filterMap value 1)
        , benchIOSink value "foldl-map" foldl'ReduceMap
        , benchIOSink value "sum-product-fold" sumProductFold
        , benchIOSink value "sum-product-scan" sumProductScan
        ]
    ]

o_1_space_transformations_mixedX2 :: Int -> [Benchmark]
o_1_space_transformations_mixedX2 value =
    [ bgroup "mixedX2"
        [ benchIOSink value "scan-map" (scanMap 2)
        , benchIOSink value "drop-map" (dropMap 2)
        , benchIOSink value "drop-scan" (dropScan 2)
        , benchIOSink value "take-drop" (takeDrop value 2)
        , benchIOSink value "take-scan" (takeScan value 2)
        , benchIOSink value "take-map" (takeMap value 2)
        , benchIOSink value "filter-drop" (filterDrop value 2)
        , benchIOSink value "filter-take" (filterTake value 2)
        , benchIOSink value "filter-scan" (filterScan 2)
        , benchIOSink value "filter-scanl1" (filterScanl1 2)
        , benchIOSink value "filter-map" (filterMap value 2)
        ]
    ]

o_1_space_transformations_mixedX4 :: Int -> [Benchmark]
o_1_space_transformations_mixedX4 value =
    [ bgroup "mixedX4"
        [ benchIOSink value "scan-map" (scanMap 4)
        , benchIOSink value "drop-map" (dropMap 4)
        , benchIOSink value "drop-scan" (dropScan 4)
        , benchIOSink value "take-drop" (takeDrop value 4)
        , benchIOSink value "take-scan" (takeScan value 4)
        , benchIOSink value "take-map" (takeMap value 4)
        , benchIOSink value "filter-drop" (filterDrop value 4)
        , benchIOSink value "filter-take" (filterTake value 4)
        , benchIOSink value "filter-scan" (filterScan 4)
        , benchIOSink value "filter-scanl1" (filterScanl1 4)
        , benchIOSink value "filter-map" (filterMap value 4)
        ]
    ]

-------------------------------------------------------------------------------
-- Iterating a transformation over and over again
-------------------------------------------------------------------------------

-- this is quadratic
{-# INLINE iterateScan #-}
iterateScan :: MonadAsync m => Int -> Int -> Int -> Stream m Int
iterateScan = iterateSource (Common.scanl' (+) 0)

-- this is quadratic
{-# INLINE iterateScanl1 #-}
iterateScanl1 :: MonadAsync m => Int -> Int -> Int -> Stream m Int
iterateScanl1 = iterateSource (S.scanl1' (+))
{-# INLINE iterateMapM #-}
iterateMapM :: MonadAsync m => Int -> Int -> Int -> Stream m Int
iterateMapM = iterateSource (S.mapM return)

{-# INLINE iterateFilterEven #-}
iterateFilterEven :: MonadAsync m => Int -> Int -> Int -> Stream m Int
iterateFilterEven = iterateSource (S.filter even)

{-# INLINE iterateTakeAll #-}
iterateTakeAll :: MonadAsync m => Int -> Int -> Int -> Int -> Stream m Int
iterateTakeAll value = iterateSource (S.take (value + 1))

{-# INLINE iterateDropOne #-}
iterateDropOne :: MonadAsync m => Int -> Int -> Int -> Stream m Int
iterateDropOne = iterateSource (S.drop 1)

{-# INLINE iterateDropWhileTrue #-}
iterateDropWhileTrue :: MonadAsync m
    => Int -> Int -> Int -> Int -> Stream m Int
iterateDropWhileTrue value = iterateSource (S.dropWhile (<= (value + 1)))

{-# INLINE iterateDropWhileFalse #-}
iterateDropWhileFalse :: MonadAsync m
    => Int -> Int -> Int -> Int -> Stream m Int
iterateDropWhileFalse value = iterateSource (S.dropWhile (> (value + 1)))

{-# INLINE tail #-}
tail :: Monad m => Stream m a -> m ()
tail s = S.tail s >>= mapM_ tail

{-# INLINE nullHeadTail #-}
nullHeadTail :: Monad m => Stream m Int -> m ()
nullHeadTail s = do
    r <- S.null s
    when (not r) $ do
        _ <- S.head s
        S.tail s >>= mapM_ nullHeadTail

nullTail :: Monad m => Stream m Int -> m ()
nullTail s = do
    r <- S.null s
    when (not r) $ S.tail s >>= mapM_ nullTail

headTail :: Monad m => Stream m Int -> m ()
headTail s = do
    h <- S.head s
    when (isJust h) $ S.tail s >>= mapM_ headTail

-- Head recursive operations.
o_n_stack_iterated :: Int -> [Benchmark]
o_n_stack_iterated value = by10 `seq` by100 `seq`
    [ bgroup "iterated"
        [ benchIOSrc "mapM (n/10 x 10)" $ iterateMapM by10 10
        , benchIOSrc "scanl' (quadratic) (n/100 x 100)" $
            iterateScan by100 100
        , benchIOSrc "scanl1' (n/10 x 10)" $ iterateScanl1 by10 10
        , benchIOSrc "filterEven (n/10 x 10)" $
            iterateFilterEven by10 10
        , benchIOSrc "takeAll (n/10 x 10)" $
            iterateTakeAll value by10 10
        , benchIOSrc "dropOne (n/10 x 10)" $ iterateDropOne by10 10
        , benchIOSrc "dropWhileTrue (n/10 x 10)" $
            iterateDropWhileTrue value by10 10
        , benchIOSrc "dropWhileFalse (n/10 x 10)" $
            iterateDropWhileFalse value by10 10
        , benchIOSink value "tail" tail
        , benchIOSink value "nullTail" nullTail
        , benchIOSink value "headTail" headTail
        , benchIOSink value "nullHeadTail" nullHeadTail
     ]
    ]

    where

    by10 = value `div` 10
    by100 = value `div` 100

-------------------------------------------------------------------------------
-- Pipes
-------------------------------------------------------------------------------

o_1_space_pipes :: Int -> [Benchmark]
o_1_space_pipes value =
    [ bgroup "pipes"
        [ benchIOSink value "mapM" (transformMapM 1)
        , benchIOSink value "compose" (transformComposeMapM 1)
        , benchIOSink value "tee" (transformTeeMapM 1)
#ifdef DEVBUILD
        -- XXX this take 1 GB memory to compile
        -- , benchIOSink value "zip" (transformZipMapM 1)
#endif
        ]
    ]

o_1_space_pipesX4 :: Int -> [Benchmark]
o_1_space_pipesX4 value =
    [ bgroup "pipesX4"
        [ benchIOSink value "mapM" (transformMapM 4)
        , benchIOSink value "compose" (transformComposeMapM 4)
        -- XXX requires @-fspec-constr-recursive=16@.
        , benchIOSink value "tee" (transformTeeMapM 4)
#ifdef DEVBUILD
        -- XXX this take 1 GB memory to compile
        -- , benchIOSink value "zip" (transformZipMapM 4)
#endif
        ]
    ]

-------------------------------------------------------------------------------
-- Scans
-------------------------------------------------------------------------------

o_1_space_scans :: Int -> [Benchmark]
o_1_space_scans value =
    [ bgroup "scans"
        [
          benchIOSink value "mapM" (scanMapM 1)
        , benchIOSink value "compose" (scanComposeMapM 1)
        , benchIOSink value "tee" (scanTeeMapM 1)
        ]
    ]

o_1_space_scansX4 :: Int -> [Benchmark]
o_1_space_scansX4 value =
    [ bgroup "scansX4"
        [ benchIOSink value "mapM" (scanMapM 4)
        , benchIOSink value "compose" (scanComposeMapM 4)
        , benchIOSink value "tee" (scanTeeMapM 4)
        ]
    ]

-------------------------------------------------------------------------------
-- Inspection
-------------------------------------------------------------------------------

#ifdef INSPECTION
-- These benchmarks take an abstract input 'Stream', so a 'Step'-free core can
-- only be checked on a complete pipeline. We bake in a concrete
-- 'sourceUnfoldrM' source (the same one 'benchIOSink' supplies) and assert the
-- whole generate+transform+drain pipeline fully fuses: no 'Step' constructors
-- remain in the optimized core. Combinators that buffer or recurse through
-- opaque state keep their 'Step' constructors; those checks are kept but
-- commented out.

-- grouping
{-# INLINE inspGroups #-}
inspGroups :: Int -> Int -> IO ()
inspGroups value n = groups (sourceUnfoldrM value n)

{-# INLINE inspGroupsWhileLT #-}
inspGroupsWhileLT :: Int -> Int -> IO ()
inspGroupsWhileLT value n = groupsWhileLT (sourceUnfoldrM value n)

{-# INLINE inspGroupsWhileEq #-}
inspGroupsWhileEq :: Int -> Int -> IO ()
inspGroupsWhileEq value n = groupsWhileEq (sourceUnfoldrM value n)

{-# INLINE inspGroupsByRollingLT #-}
inspGroupsByRollingLT :: Int -> Int -> IO ()
inspGroupsByRollingLT value n = groupsByRollingLT (sourceUnfoldrM value n)

{-# INLINE inspGroupsByRollingEq #-}
inspGroupsByRollingEq :: Int -> Int -> IO ()
inspGroupsByRollingEq value n = groupsByRollingEq (sourceUnfoldrM value n)

{-# INLINE inspFoldMany #-}
inspFoldMany :: Int -> Int -> IO ()
inspFoldMany value n = foldMany (sourceUnfoldrM value n)

{-# INLINE inspFoldMany1 #-}
inspFoldMany1 :: Int -> Int -> IO ()
inspFoldMany1 value n = foldMany1 (sourceUnfoldrM value n)

{-# INLINE inspRefoldMany #-}
inspRefoldMany :: Int -> Int -> IO ()
inspRefoldMany value n = refoldMany (sourceUnfoldrM value n)

{-# INLINE inspFoldIterateM #-}
inspFoldIterateM :: Int -> Int -> IO ()
inspFoldIterateM value n = foldIterateM (sourceUnfoldrM value n)

{-# INLINE inspRefoldIterateM #-}
inspRefoldIterateM :: Int -> Int -> IO ()
inspRefoldIterateM value n = refoldIterateM (sourceUnfoldrM value n)

-- mixed (compose 1)
{-# INLINE inspScanMap #-}
inspScanMap :: Int -> Int -> IO ()
inspScanMap value n = scanMap 1 (sourceUnfoldrM value n)

{-# INLINE inspDropMap #-}
inspDropMap :: Int -> Int -> IO ()
inspDropMap value n = dropMap 1 (sourceUnfoldrM value n)

{-# INLINE inspDropScan #-}
inspDropScan :: Int -> Int -> IO ()
inspDropScan value n = dropScan 1 (sourceUnfoldrM value n)

{-# INLINE inspTakeDrop #-}
inspTakeDrop :: Int -> Int -> IO ()
inspTakeDrop value n = takeDrop value 1 (sourceUnfoldrM value n)

{-# INLINE inspTakeScan #-}
inspTakeScan :: Int -> Int -> IO ()
inspTakeScan value n = takeScan value 1 (sourceUnfoldrM value n)

{-# INLINE inspTakeMap #-}
inspTakeMap :: Int -> Int -> IO ()
inspTakeMap value n = takeMap value 1 (sourceUnfoldrM value n)

{-# INLINE inspFilterDrop #-}
inspFilterDrop :: Int -> Int -> IO ()
inspFilterDrop value n = filterDrop value 1 (sourceUnfoldrM value n)

{-# INLINE inspFilterTake #-}
inspFilterTake :: Int -> Int -> IO ()
inspFilterTake value n = filterTake value 1 (sourceUnfoldrM value n)

{-# INLINE inspFilterScan #-}
inspFilterScan :: Int -> Int -> IO ()
inspFilterScan value n = filterScan 1 (sourceUnfoldrM value n)

{-# INLINE inspFilterMap #-}
inspFilterMap :: Int -> Int -> IO ()
inspFilterMap value n = filterMap value 1 (sourceUnfoldrM value n)

{-# INLINE inspFoldlReduceMap #-}
inspFoldlReduceMap :: Int -> Int -> IO Int
inspFoldlReduceMap value n = foldl'ReduceMap (sourceUnfoldrM value n)

{-# INLINE inspSumProductFold #-}
inspSumProductFold :: Int -> Int -> IO (Pair Int Int)
inspSumProductFold value n = sumProductFold (sourceUnfoldrM value n)

{-# INLINE inspSumProductScan #-}
inspSumProductScan :: Int -> Int -> IO (Pair Int Int)
inspSumProductScan value n = sumProductScan (sourceUnfoldrM value n)

-- mixedX2 (compose 2)
{-# INLINE inspScanMapX2 #-}
inspScanMapX2 :: Int -> Int -> IO ()
inspScanMapX2 value n = scanMap 2 (sourceUnfoldrM value n)

{-# INLINE inspDropMapX2 #-}
inspDropMapX2 :: Int -> Int -> IO ()
inspDropMapX2 value n = dropMap 2 (sourceUnfoldrM value n)

{-# INLINE inspDropScanX2 #-}
inspDropScanX2 :: Int -> Int -> IO ()
inspDropScanX2 value n = dropScan 2 (sourceUnfoldrM value n)

{-# INLINE inspTakeDropX2 #-}
inspTakeDropX2 :: Int -> Int -> IO ()
inspTakeDropX2 value n = takeDrop value 2 (sourceUnfoldrM value n)

{-# INLINE inspTakeScanX2 #-}
inspTakeScanX2 :: Int -> Int -> IO ()
inspTakeScanX2 value n = takeScan value 2 (sourceUnfoldrM value n)

{-# INLINE inspTakeMapX2 #-}
inspTakeMapX2 :: Int -> Int -> IO ()
inspTakeMapX2 value n = takeMap value 2 (sourceUnfoldrM value n)

{-# INLINE inspFilterDropX2 #-}
inspFilterDropX2 :: Int -> Int -> IO ()
inspFilterDropX2 value n = filterDrop value 2 (sourceUnfoldrM value n)

{-# INLINE inspFilterTakeX2 #-}
inspFilterTakeX2 :: Int -> Int -> IO ()
inspFilterTakeX2 value n = filterTake value 2 (sourceUnfoldrM value n)

{-# INLINE inspFilterScanX2 #-}
inspFilterScanX2 :: Int -> Int -> IO ()
inspFilterScanX2 value n = filterScan 2 (sourceUnfoldrM value n)

{-# INLINE inspFilterScanl1X2 #-}
inspFilterScanl1X2 :: Int -> Int -> IO ()
inspFilterScanl1X2 value n = filterScanl1 2 (sourceUnfoldrM value n)

{-# INLINE inspFilterMapX2 #-}
inspFilterMapX2 :: Int -> Int -> IO ()
inspFilterMapX2 value n = filterMap value 2 (sourceUnfoldrM value n)

-- mixedX4 (compose 4)
{-# INLINE inspScanMapX4 #-}
inspScanMapX4 :: Int -> Int -> IO ()
inspScanMapX4 value n = scanMap 4 (sourceUnfoldrM value n)

{-# INLINE inspDropMapX4 #-}
inspDropMapX4 :: Int -> Int -> IO ()
inspDropMapX4 value n = dropMap 4 (sourceUnfoldrM value n)

{-# INLINE inspDropScanX4 #-}
inspDropScanX4 :: Int -> Int -> IO ()
inspDropScanX4 value n = dropScan 4 (sourceUnfoldrM value n)

{-# INLINE inspTakeDropX4 #-}
inspTakeDropX4 :: Int -> Int -> IO ()
inspTakeDropX4 value n = takeDrop value 4 (sourceUnfoldrM value n)

{-# INLINE inspTakeScanX4 #-}
inspTakeScanX4 :: Int -> Int -> IO ()
inspTakeScanX4 value n = takeScan value 4 (sourceUnfoldrM value n)

{-# INLINE inspTakeMapX4 #-}
inspTakeMapX4 :: Int -> Int -> IO ()
inspTakeMapX4 value n = takeMap value 4 (sourceUnfoldrM value n)

{-# INLINE inspFilterDropX4 #-}
inspFilterDropX4 :: Int -> Int -> IO ()
inspFilterDropX4 value n = filterDrop value 4 (sourceUnfoldrM value n)

{-# INLINE inspFilterTakeX4 #-}
inspFilterTakeX4 :: Int -> Int -> IO ()
inspFilterTakeX4 value n = filterTake value 4 (sourceUnfoldrM value n)

{-# INLINE inspFilterScanX4 #-}
inspFilterScanX4 :: Int -> Int -> IO ()
inspFilterScanX4 value n = filterScan 4 (sourceUnfoldrM value n)

{-# INLINE inspFilterScanl1X4 #-}
inspFilterScanl1X4 :: Int -> Int -> IO ()
inspFilterScanl1X4 value n = filterScanl1 4 (sourceUnfoldrM value n)

{-# INLINE inspFilterMapX4 #-}
inspFilterMapX4 :: Int -> Int -> IO ()
inspFilterMapX4 value n = filterMap value 4 (sourceUnfoldrM value n)

-- pipes
{-# INLINE inspPipeMapM #-}
inspPipeMapM :: Int -> Int -> IO ()
inspPipeMapM value n = transformMapM 1 (sourceUnfoldrM value n)

{-# INLINE inspPipeCompose #-}
inspPipeCompose :: Int -> Int -> IO ()
inspPipeCompose value n = transformComposeMapM 1 (sourceUnfoldrM value n)

{-# INLINE inspPipeTee #-}
inspPipeTee :: Int -> Int -> IO ()
inspPipeTee value n = transformTeeMapM 1 (sourceUnfoldrM value n)

{-# INLINE inspPipeMapMX4 #-}
inspPipeMapMX4 :: Int -> Int -> IO ()
inspPipeMapMX4 value n = transformMapM 4 (sourceUnfoldrM value n)

{-# INLINE inspPipeComposeX4 #-}
inspPipeComposeX4 :: Int -> Int -> IO ()
inspPipeComposeX4 value n = transformComposeMapM 4 (sourceUnfoldrM value n)

{-# INLINE inspPipeTeeX4 #-}
inspPipeTeeX4 :: Int -> Int -> IO ()
inspPipeTeeX4 value n = transformTeeMapM 4 (sourceUnfoldrM value n)

-- scans
{-# INLINE inspScanMapM #-}
inspScanMapM :: Int -> Int -> IO ()
inspScanMapM value n = scanMapM 1 (sourceUnfoldrM value n)

{-# INLINE inspScanCompose #-}
inspScanCompose :: Int -> Int -> IO ()
inspScanCompose value n = scanComposeMapM 1 (sourceUnfoldrM value n)

{-# INLINE inspScanTee #-}
inspScanTee :: Int -> Int -> IO ()
inspScanTee value n = scanTeeMapM 1 (sourceUnfoldrM value n)

{-# INLINE inspScanMapMX4 #-}
inspScanMapMX4 :: Int -> Int -> IO ()
inspScanMapMX4 value n = scanMapM 4 (sourceUnfoldrM value n)

{-# INLINE inspScanComposeX4 #-}
inspScanComposeX4 :: Int -> Int -> IO ()
inspScanComposeX4 value n = scanComposeMapM 4 (sourceUnfoldrM value n)

{-# INLINE inspScanTeeX4 #-}
inspScanTeeX4 :: Int -> Int -> IO ()
inspScanTeeX4 value n = scanTeeMapM 4 (sourceUnfoldrM value n)

-- buffered (reverse materializes the whole stream)
{-# INLINE inspReverse #-}
inspReverse :: Int -> Int -> IO ()
inspReverse value n = reverse 1 (sourceUnfoldrM value n)

{-# INLINE inspReverse' #-}
inspReverse' :: Int -> Int -> IO ()
inspReverse' value n = reverse' 1 (sourceUnfoldrM value n)

-- grouping. 'groupsWhile' nests an inner fold over the stream, so a 'Step'
-- constructor survives (the 'groupsRollingBy' variants below do fuse).
inspect $ hasNoTypeClasses 'inspGroups
-- inspect $ 'inspGroups `hasNoType` ''S.Step
inspect $ 'inspGroups `hasNoType` ''FL.Step
inspect $ 'inspGroups `hasNoType` ''SPEC
inspect $ hasNoTypeClasses 'inspGroupsWhileLT
-- inspect $ 'inspGroupsWhileLT `hasNoType` ''S.Step
inspect $ 'inspGroupsWhileLT `hasNoType` ''FL.Step
inspect $ 'inspGroupsWhileLT `hasNoType` ''SPEC
inspect $ hasNoTypeClasses 'inspGroupsWhileEq
-- inspect $ 'inspGroupsWhileEq `hasNoType` ''S.Step
inspect $ 'inspGroupsWhileEq `hasNoType` ''FL.Step
inspect $ 'inspGroupsWhileEq `hasNoType` ''SPEC
inspect $ hasNoTypeClasses 'inspGroupsByRollingLT
inspect $ 'inspGroupsByRollingLT `hasNoType` ''S.Step
inspect $ 'inspGroupsByRollingLT `hasNoType` ''FL.Step
inspect $ 'inspGroupsByRollingLT `hasNoType` ''SPEC
inspect $ hasNoTypeClasses 'inspGroupsByRollingEq
inspect $ 'inspGroupsByRollingEq `hasNoType` ''S.Step
inspect $ 'inspGroupsByRollingEq `hasNoType` ''FL.Step
inspect $ 'inspGroupsByRollingEq `hasNoType` ''SPEC
inspect $ hasNoTypeClasses 'inspFoldMany
inspect $ 'inspFoldMany `hasNoType` ''S.Step
inspect $ 'inspFoldMany `hasNoType` ''FL.Step
inspect $ 'inspFoldMany `hasNoType` ''SPEC
inspect $ hasNoTypeClasses 'inspFoldMany1
inspect $ 'inspFoldMany1 `hasNoType` ''S.Step
inspect $ 'inspFoldMany1 `hasNoType` ''FL.Step
inspect $ 'inspFoldMany1 `hasNoType` ''SPEC
inspect $ hasNoTypeClasses 'inspRefoldMany
inspect $ 'inspRefoldMany `hasNoType` ''S.Step
inspect $ 'inspRefoldMany `hasNoType` ''FL.Step
inspect $ 'inspRefoldMany `hasNoType` ''SPEC
inspect $ hasNoTypeClasses 'inspFoldIterateM
inspect $ 'inspFoldIterateM `hasNoType` ''S.Step
inspect $ 'inspFoldIterateM `hasNoType` ''FL.Step
inspect $ 'inspFoldIterateM `hasNoType` ''SPEC
inspect $ hasNoTypeClasses 'inspRefoldIterateM
inspect $ 'inspRefoldIterateM `hasNoType` ''S.Step
inspect $ 'inspRefoldIterateM `hasNoType` ''FL.Step
inspect $ 'inspRefoldIterateM `hasNoType` ''SPEC

-- mixed
inspect $ hasNoTypeClasses 'inspScanMap
inspect $ 'inspScanMap `hasNoType` ''S.Step
inspect $ 'inspScanMap `hasNoType` ''FL.Step
inspect $ 'inspScanMap `hasNoType` ''SPEC
inspect $ hasNoTypeClasses 'inspDropMap
inspect $ 'inspDropMap `hasNoType` ''S.Step
inspect $ 'inspDropMap `hasNoType` ''FL.Step
inspect $ 'inspDropMap `hasNoType` ''SPEC
inspect $ hasNoTypeClasses 'inspDropScan
inspect $ 'inspDropScan `hasNoType` ''S.Step
inspect $ 'inspDropScan `hasNoType` ''FL.Step
inspect $ 'inspDropScan `hasNoType` ''SPEC
inspect $ hasNoTypeClasses 'inspTakeDrop
inspect $ 'inspTakeDrop `hasNoType` ''S.Step
inspect $ 'inspTakeDrop `hasNoType` ''FL.Step
inspect $ 'inspTakeDrop `hasNoType` ''SPEC
inspect $ hasNoTypeClasses 'inspTakeScan
inspect $ 'inspTakeScan `hasNoType` ''S.Step
inspect $ 'inspTakeScan `hasNoType` ''FL.Step
inspect $ 'inspTakeScan `hasNoType` ''SPEC
inspect $ hasNoTypeClasses 'inspTakeMap
inspect $ 'inspTakeMap `hasNoType` ''S.Step
inspect $ 'inspTakeMap `hasNoType` ''FL.Step
inspect $ 'inspTakeMap `hasNoType` ''SPEC
inspect $ hasNoTypeClasses 'inspFilterDrop
inspect $ 'inspFilterDrop `hasNoType` ''S.Step
inspect $ 'inspFilterDrop `hasNoType` ''FL.Step
inspect $ 'inspFilterDrop `hasNoType` ''SPEC
inspect $ hasNoTypeClasses 'inspFilterTake
inspect $ 'inspFilterTake `hasNoType` ''S.Step
inspect $ 'inspFilterTake `hasNoType` ''FL.Step
inspect $ 'inspFilterTake `hasNoType` ''SPEC
inspect $ hasNoTypeClasses 'inspFilterScan
inspect $ 'inspFilterScan `hasNoType` ''S.Step
inspect $ 'inspFilterScan `hasNoType` ''FL.Step
inspect $ 'inspFilterScan `hasNoType` ''SPEC
inspect $ hasNoTypeClasses 'inspFilterMap
inspect $ 'inspFilterMap `hasNoType` ''S.Step
inspect $ 'inspFilterMap `hasNoType` ''FL.Step
inspect $ 'inspFilterMap `hasNoType` ''SPEC
inspect $ hasNoTypeClasses 'inspFoldlReduceMap
inspect $ 'inspFoldlReduceMap `hasNoType` ''S.Step
inspect $ 'inspFoldlReduceMap `hasNoType` ''FL.Step
inspect $ 'inspFoldlReduceMap `hasNoType` ''SPEC
inspect $ hasNoTypeClasses 'inspSumProductFold
inspect $ 'inspSumProductFold `hasNoType` ''S.Step
inspect $ 'inspSumProductFold `hasNoType` ''FL.Step
inspect $ 'inspSumProductFold `hasNoType` ''SPEC
inspect $ hasNoTypeClasses 'inspSumProductScan
inspect $ 'inspSumProductScan `hasNoType` ''S.Step
inspect $ 'inspSumProductScan `hasNoType` ''FL.Step
inspect $ 'inspSumProductScan `hasNoType` ''SPEC

-- mixedX2
inspect $ hasNoTypeClasses 'inspScanMapX2
inspect $ 'inspScanMapX2 `hasNoType` ''S.Step
inspect $ 'inspScanMapX2 `hasNoType` ''FL.Step
inspect $ 'inspScanMapX2 `hasNoType` ''SPEC
inspect $ hasNoTypeClasses 'inspDropMapX2
inspect $ 'inspDropMapX2 `hasNoType` ''S.Step
inspect $ 'inspDropMapX2 `hasNoType` ''FL.Step
inspect $ 'inspDropMapX2 `hasNoType` ''SPEC
inspect $ hasNoTypeClasses 'inspDropScanX2
inspect $ 'inspDropScanX2 `hasNoType` ''S.Step
inspect $ 'inspDropScanX2 `hasNoType` ''FL.Step
inspect $ 'inspDropScanX2 `hasNoType` ''SPEC
inspect $ hasNoTypeClasses 'inspTakeDropX2
inspect $ 'inspTakeDropX2 `hasNoType` ''S.Step
inspect $ 'inspTakeDropX2 `hasNoType` ''FL.Step
inspect $ 'inspTakeDropX2 `hasNoType` ''SPEC
inspect $ hasNoTypeClasses 'inspTakeScanX2
inspect $ 'inspTakeScanX2 `hasNoType` ''S.Step
inspect $ 'inspTakeScanX2 `hasNoType` ''FL.Step
inspect $ 'inspTakeScanX2 `hasNoType` ''SPEC
inspect $ hasNoTypeClasses 'inspTakeMapX2
inspect $ 'inspTakeMapX2 `hasNoType` ''S.Step
inspect $ 'inspTakeMapX2 `hasNoType` ''FL.Step
inspect $ 'inspTakeMapX2 `hasNoType` ''SPEC
inspect $ hasNoTypeClasses 'inspFilterDropX2
inspect $ 'inspFilterDropX2 `hasNoType` ''S.Step
inspect $ 'inspFilterDropX2 `hasNoType` ''FL.Step
inspect $ 'inspFilterDropX2 `hasNoType` ''SPEC
inspect $ hasNoTypeClasses 'inspFilterTakeX2
inspect $ 'inspFilterTakeX2 `hasNoType` ''S.Step
inspect $ 'inspFilterTakeX2 `hasNoType` ''FL.Step
inspect $ 'inspFilterTakeX2 `hasNoType` ''SPEC
inspect $ hasNoTypeClasses 'inspFilterScanX2
inspect $ 'inspFilterScanX2 `hasNoType` ''S.Step
inspect $ 'inspFilterScanX2 `hasNoType` ''FL.Step
inspect $ 'inspFilterScanX2 `hasNoType` ''SPEC
inspect $ hasNoTypeClasses 'inspFilterScanl1X2
inspect $ 'inspFilterScanl1X2 `hasNoType` ''S.Step
inspect $ 'inspFilterScanl1X2 `hasNoType` ''FL.Step
inspect $ 'inspFilterScanl1X2 `hasNoType` ''SPEC
inspect $ hasNoTypeClasses 'inspFilterMapX2
inspect $ 'inspFilterMapX2 `hasNoType` ''S.Step
inspect $ 'inspFilterMapX2 `hasNoType` ''FL.Step
inspect $ 'inspFilterMapX2 `hasNoType` ''SPEC

-- mixedX4
inspect $ hasNoTypeClasses 'inspScanMapX4
inspect $ 'inspScanMapX4 `hasNoType` ''S.Step
inspect $ 'inspScanMapX4 `hasNoType` ''FL.Step
inspect $ 'inspScanMapX4 `hasNoType` ''SPEC
inspect $ hasNoTypeClasses 'inspDropMapX4
inspect $ 'inspDropMapX4 `hasNoType` ''S.Step
inspect $ 'inspDropMapX4 `hasNoType` ''FL.Step
inspect $ 'inspDropMapX4 `hasNoType` ''SPEC
inspect $ hasNoTypeClasses 'inspDropScanX4
inspect $ 'inspDropScanX4 `hasNoType` ''S.Step
inspect $ 'inspDropScanX4 `hasNoType` ''FL.Step
inspect $ 'inspDropScanX4 `hasNoType` ''SPEC
inspect $ hasNoTypeClasses 'inspTakeDropX4
inspect $ 'inspTakeDropX4 `hasNoType` ''S.Step
inspect $ 'inspTakeDropX4 `hasNoType` ''FL.Step
inspect $ 'inspTakeDropX4 `hasNoType` ''SPEC
inspect $ hasNoTypeClasses 'inspTakeScanX4
inspect $ 'inspTakeScanX4 `hasNoType` ''S.Step
inspect $ 'inspTakeScanX4 `hasNoType` ''FL.Step
inspect $ 'inspTakeScanX4 `hasNoType` ''SPEC
inspect $ hasNoTypeClasses 'inspTakeMapX4
inspect $ 'inspTakeMapX4 `hasNoType` ''S.Step
inspect $ 'inspTakeMapX4 `hasNoType` ''FL.Step
inspect $ 'inspTakeMapX4 `hasNoType` ''SPEC
inspect $ hasNoTypeClasses 'inspFilterDropX4
inspect $ 'inspFilterDropX4 `hasNoType` ''S.Step
inspect $ 'inspFilterDropX4 `hasNoType` ''FL.Step
inspect $ 'inspFilterDropX4 `hasNoType` ''SPEC
inspect $ hasNoTypeClasses 'inspFilterTakeX4
inspect $ 'inspFilterTakeX4 `hasNoType` ''S.Step
inspect $ 'inspFilterTakeX4 `hasNoType` ''FL.Step
inspect $ 'inspFilterTakeX4 `hasNoType` ''SPEC
inspect $ hasNoTypeClasses 'inspFilterScanX4
inspect $ 'inspFilterScanX4 `hasNoType` ''S.Step
inspect $ 'inspFilterScanX4 `hasNoType` ''FL.Step
inspect $ 'inspFilterScanX4 `hasNoType` ''SPEC
inspect $ hasNoTypeClasses 'inspFilterScanl1X4
inspect $ 'inspFilterScanl1X4 `hasNoType` ''S.Step
inspect $ 'inspFilterScanl1X4 `hasNoType` ''FL.Step
inspect $ 'inspFilterScanl1X4 `hasNoType` ''SPEC
inspect $ hasNoTypeClasses 'inspFilterMapX4
inspect $ 'inspFilterMapX4 `hasNoType` ''S.Step
inspect $ 'inspFilterMapX4 `hasNoType` ''FL.Step
inspect $ 'inspFilterMapX4 `hasNoType` ''SPEC

-- pipes
inspect $ hasNoTypeClasses 'inspPipeMapM
inspect $ 'inspPipeMapM `hasNoType` ''S.Step
inspect $ 'inspPipeMapM `hasNoType` ''FL.Step
inspect $ 'inspPipeMapM `hasNoType` ''SPEC
inspect $ hasNoTypeClasses 'inspPipeCompose
inspect $ 'inspPipeCompose `hasNoType` ''S.Step
inspect $ 'inspPipeCompose `hasNoType` ''FL.Step
inspect $ 'inspPipeCompose `hasNoType` ''SPEC
inspect $ hasNoTypeClasses 'inspPipeTee
inspect $ 'inspPipeTee `hasNoType` ''S.Step
inspect $ 'inspPipeTee `hasNoType` ''FL.Step
inspect $ 'inspPipeTee `hasNoType` ''SPEC
inspect $ hasNoTypeClasses 'inspPipeMapMX4
inspect $ 'inspPipeMapMX4 `hasNoType` ''S.Step
inspect $ 'inspPipeMapMX4 `hasNoType` ''FL.Step
inspect $ 'inspPipeMapMX4 `hasNoType` ''SPEC
inspect $ hasNoTypeClasses 'inspPipeComposeX4
inspect $ 'inspPipeComposeX4 `hasNoType` ''S.Step
inspect $ 'inspPipeComposeX4 `hasNoType` ''FL.Step
inspect $ 'inspPipeComposeX4 `hasNoType` ''SPEC
inspect $ hasNoTypeClasses 'inspPipeTeeX4
inspect $ 'inspPipeTeeX4 `hasNoType` ''S.Step
inspect $ 'inspPipeTeeX4 `hasNoType` ''FL.Step
inspect $ 'inspPipeTeeX4 `hasNoType` ''SPEC

-- scans
inspect $ hasNoTypeClasses 'inspScanMapM
inspect $ 'inspScanMapM `hasNoType` ''S.Step
inspect $ 'inspScanMapM `hasNoType` ''FL.Step
inspect $ 'inspScanMapM `hasNoType` ''SPEC
inspect $ hasNoTypeClasses 'inspScanCompose
inspect $ 'inspScanCompose `hasNoType` ''S.Step
inspect $ 'inspScanCompose `hasNoType` ''FL.Step
inspect $ 'inspScanCompose `hasNoType` ''SPEC
inspect $ hasNoTypeClasses 'inspScanTee
inspect $ 'inspScanTee `hasNoType` ''S.Step
inspect $ 'inspScanTee `hasNoType` ''FL.Step
inspect $ 'inspScanTee `hasNoType` ''SPEC
inspect $ hasNoTypeClasses 'inspScanMapMX4
inspect $ 'inspScanMapMX4 `hasNoType` ''S.Step
inspect $ 'inspScanMapMX4 `hasNoType` ''FL.Step
inspect $ 'inspScanMapMX4 `hasNoType` ''SPEC
inspect $ hasNoTypeClasses 'inspScanComposeX4
inspect $ 'inspScanComposeX4 `hasNoType` ''S.Step
inspect $ 'inspScanComposeX4 `hasNoType` ''FL.Step
inspect $ 'inspScanComposeX4 `hasNoType` ''SPEC
inspect $ hasNoTypeClasses 'inspScanTeeX4
inspect $ 'inspScanTeeX4 `hasNoType` ''S.Step
inspect $ 'inspScanTeeX4 `hasNoType` ''FL.Step
inspect $ 'inspScanTeeX4 `hasNoType` ''SPEC

-- buffered. 'reverse' buffers the whole stream; the boxed 'reverse' still
-- fuses generation+drain, but the unboxed 'reverse'' (reverseUnbox) writes to a
-- mutable array, so a 'Step' constructor survives.
inspect $ hasNoTypeClasses 'inspReverse
inspect $ 'inspReverse `hasNoType` ''S.Step
inspect $ 'inspReverse `hasNoType` ''FL.Step
-- inspect $ 'inspReverse `hasNoType` ''SPEC
inspect $ hasNoTypeClasses 'inspReverse'
-- inspect $ 'inspReverse' `hasNoType` ''S.Step

-- Not inspection-tested (not 'Step'-free fusion targets):
--   * The 'o_n_stack' "iterated" benchmarks ('iterateMapM', 'iterateScan',
--     'iterateScanl1', 'iterateFilterEven', 'iterateTakeAll', 'iterateDropOne',
--     'iterateDropWhileTrue', 'iterateDropWhileFalse') apply a transformation a
--     runtime number of times, so the nested 'Step's cannot be eliminated.
--   * 'tail', 'nullTail', 'headTail' and 'nullHeadTail' recurse explicitly,
--     reconstructing the stream on each step, so a 'Step' constructor survives.
#endif

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

-- In addition to gauge options, the number of elements in the stream can be
-- passed using the --stream-size option.
--
benchmarks :: String -> Int -> [Benchmark]
benchmarks moduleName size =
        [ bgroup (o_1_space_prefix moduleName) $ Prelude.concat
            [ o_1_space_grouping size
            , o_1_space_transformations_mixed size
            , o_1_space_transformations_mixedX2 size
            , o_1_space_transformations_mixedX4 size

            -- pipes
            , o_1_space_pipes size
            , o_1_space_pipesX4 size

            -- scans
            , o_1_space_scans size
            , o_1_space_scansX4 size
            ]
        , bgroup (o_n_stack_prefix moduleName) (o_n_stack_iterated size)
        , bgroup (o_n_heap_prefix moduleName) (o_n_heap_buffering size)
        ]
