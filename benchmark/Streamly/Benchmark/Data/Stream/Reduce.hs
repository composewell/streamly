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

module Stream.Reduce (benchmarks) where

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
