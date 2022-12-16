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

#ifdef USE_PRELUDE
{-# OPTIONS_GHC -Wno-deprecations #-}
#endif

module Stream.Reduce (benchmarks) where

import Control.DeepSeq (NFData(..))
import Control.Monad.IO.Class (MonadIO(..))
import Data.Monoid (Sum(..))
import GHC.Generics (Generic)
import Foreign.Ptr (plusPtr, castPtr)
import Foreign.Storable (Storable(..))
import Streamly.Data.Array (Unbox(..))
import Streamly.Internal.Data.Stream (Stream)
import Streamly.Internal.Data.Unboxed (castContents)
import System.Random (randomRIO)

import qualified Streamly.Internal.Data.Refold.Type as Refold
import qualified Streamly.Internal.Data.Fold as FL
import qualified Stream.Common as Common
#ifdef USE_PRELUDE
import Control.Monad (when)
import Data.Proxy (Proxy(..))
import Data.HashMap.Strict (HashMap)
import Streamly.Internal.Data.IsMap.HashMap ()
import qualified Streamly.Internal.Data.Stream.IsStream as S
import Streamly.Prelude (fromSerial)
import Streamly.Benchmark.Prelude hiding
    ( benchIO, benchIOSrc, sourceUnfoldrM, apDiscardFst, apDiscardSnd, apLiftA2
    , toNullAp, monadThen, toNullM, toNullM3, filterAllInM, filterAllOutM
    , filterSome, breakAfterSome, toListM, toListSome, transformMapM
    , transformComposeMapM, transformTeeMapM, transformZipMapM)
#else
import qualified Streamly.Internal.Data.Stream as S
#endif

import Gauge
import Streamly.Benchmark.Common
import Stream.Common
import Prelude hiding (reverse, tail)

-------------------------------------------------------------------------------
-- Iteration/looping utilities
-------------------------------------------------------------------------------

{-# INLINE iterateN #-}
iterateN :: (Int -> a -> a) -> a -> Int -> a
iterateN g initial count = f count initial

    where

    f (0 :: Int) x = x
    f i x = f (i - 1) (g i x)

-- Iterate a transformation over a singleton stream
{-# INLINE iterateSingleton #-}
iterateSingleton ::
       (Int -> Stream m Int -> Stream m Int)
    -> Int
    -> Int
    -> Stream m Int
iterateSingleton g count n = iterateN g (S.fromPure n) count

{-
-- XXX need to check why this is slower than the explicit recursion above, even
-- if the above code is written in a foldr like head recursive way. We also
-- need to try this with foldlM' once #150 is fixed.
-- However, it is perhaps best to keep the iteration benchmarks independent of
-- foldrM and any related fusion issues.
{-# INLINE _iterateSingleton #-}
_iterateSingleton ::
       Monad m
    => (Int -> Stream m Int -> Stream m Int)
    -> Int
    -> Int
    -> Stream m Int
_iterateSingleton g value n = S.foldrM g (return n) $ sourceIntFromTo value n
-}

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
-- Functor
-------------------------------------------------------------------------------

o_n_space_functor :: Int -> [Benchmark]
o_n_space_functor value =
    [ bgroup "Functor"
        [ benchIO "(+) (n times) (baseline)" $ \i0 ->
            iterateN (\i acc -> acc >>= \n -> return $ i + n) (return i0) value
        , benchIOSrc "(<$) (n times)" $
            iterateSingleton (<$) value
        , benchIOSrc "fmap (n times)" $
            iterateSingleton (fmap . (+)) value
        {-
        , benchIOSrc fromSerial "_(<$) (n times)" $
            _iterateSingleton (<$) value
        , benchIOSrc fromSerial "_fmap (n times)" $
            _iterateSingleton (fmap . (+)) value
        -}
        ]
    ]

-------------------------------------------------------------------------------
-- Grouping transformations
-------------------------------------------------------------------------------

#ifdef USE_PRELUDE
{-# INLINE groups #-}
groups :: MonadIO m => Stream m Int -> m ()
groups = Common.drain . S.groups FL.drain

-- XXX Change this test when the order of comparison is later changed
{-# INLINE groupsByGT #-}
groupsByGT :: MonadIO m => Stream m Int -> m ()
groupsByGT = Common.drain . S.groupsBy (>) FL.drain

{-# INLINE groupsByEq #-}
groupsByEq :: MonadIO m => Stream m Int -> m ()
groupsByEq = Common.drain . S.groupsBy (==) FL.drain

-- XXX Change this test when the order of comparison is later changed
{-# INLINE groupsByRollingLT #-}
groupsByRollingLT :: MonadIO m => Stream m Int -> m ()
groupsByRollingLT =
    Common.drain . S.groupsByRolling (<) FL.drain

{-# INLINE groupsByRollingEq #-}
groupsByRollingEq :: MonadIO m => Stream m Int -> m ()
groupsByRollingEq =
    Common.drain . S.groupsByRolling (==) FL.drain
#endif

{-# INLINE foldMany #-}
foldMany :: Monad m => Stream m Int -> m ()
foldMany =
      Common.drain
    . fmap getSum
    . S.foldMany (FL.take 2 FL.mconcat)
    . fmap Sum

{-# INLINE foldManyPost #-}
foldManyPost :: Monad m => Stream m Int -> m ()
foldManyPost =
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
#ifdef USE_PRELUDE
          benchIOSink value "groups" groups
        , benchIOSink value "groupsByGT" groupsByGT
        , benchIOSink value "groupsByEq" groupsByEq
        , benchIOSink value "groupsByRollingLT" groupsByRollingLT
        , benchIOSink value "groupsByRollingEq" groupsByRollingEq
        ,
#endif
        -- XXX parseMany/parseIterate benchmarks are in the Parser/ParserD
        -- modules we can bring those here. arraysOf benchmarks are in
        -- Parser/ParserD/Array.Stream/FileSystem.Handle.
          benchIOSink value "foldMany" foldMany
        , benchIOSink value "foldManyPost" foldManyPost
        , benchIOSink value "refoldMany" refoldMany
        , benchIOSink value "foldIterateM" foldIterateM
        , benchIOSink value "refoldIterateM" refoldIterateM
        ]
    ]

-------------------------------------------------------------------------------
-- Size conserving transformations (reordering, buffering, etc.)
-------------------------------------------------------------------------------

data LargeObject = LargeObject
    { _int0 :: Int
    , _int1 :: Int
    , _int2 :: Int
    , _int3 :: Int
    , _int4 :: Int
    , _int5 :: Int
    , _int6 :: Int
    , _int7 :: Int
    , _int8 :: Int
    , _int9 :: Int
    }

instance Storable LargeObject where
    sizeOf _ = 80
    alignment _ = 8
    peek ptr = do
        let p = castPtr ptr
        x0 <- peek p
        x1 <- peek (p `plusPtr` 8)
        x2 <- peek (p `plusPtr` 16)
        x3 <- peek (p `plusPtr` 24)
        x4 <- peek (p `plusPtr` 32)
        x5 <- peek (p `plusPtr` 40)
        x6 <- peek (p `plusPtr` 48)
        x7 <- peek (p `plusPtr` 56)
        x8 <- peek (p `plusPtr` 64)
        x9 <- peek (p `plusPtr` 72)
        return $ LargeObject x0 x1 x2 x3 x4 x5 x6 x7 x8 x9

    poke ptr (LargeObject x0 x1 x2 x3 x4 x5 x6 x7 x8 x9) = do
        let p = castPtr ptr
        poke p x0
        poke (p `plusPtr` 8)   x1
        poke (p `plusPtr` 16)  x2
        poke (p `plusPtr` 24)  x3
        poke (p `plusPtr` 32)  x4
        poke (p `plusPtr` 40)  x5
        poke (p `plusPtr` 48)  x6
        poke (p `plusPtr` 56)  x7
        poke (p `plusPtr` 64)  x8
        poke (p `plusPtr` 72)  x9
        return ()

instance Unbox LargeObject where
    peekByteIndex arr i = do
        let p = castContents arr
        x0 <- peekByteIndex p i
        x1 <- peekByteIndex p (i + 8)
        x2 <- peekByteIndex p (i + 16)
        x3 <- peekByteIndex p (i + 24)
        x4 <- peekByteIndex p (i + 32)
        x5 <- peekByteIndex p (i + 40)
        x6 <- peekByteIndex p (i + 48)
        x7 <- peekByteIndex p (i + 56)
        x8 <- peekByteIndex p (i + 64)
        x9 <- peekByteIndex p (i + 72)
        return $ LargeObject x0 x1 x2 x3 x4 x5 x6 x7 x8 x9

    pokeByteIndex arr i (LargeObject x0 x1 x2 x3 x4 x5 x6 x7 x8 x9) = do
        let p = castContents arr
        pokeByteIndex p i           x0
        pokeByteIndex p (i + 8)     x1
        pokeByteIndex p (i + 16)    x2
        pokeByteIndex p (i + 24)    x3
        pokeByteIndex p (i + 32)    x4
        pokeByteIndex p (i + 40)    x5
        pokeByteIndex p (i + 48)    x6
        pokeByteIndex p (i + 56)    x7
        pokeByteIndex p (i + 64)    x8
        pokeByteIndex p (i + 72)    x9
        return ()

{-# INLINE sourceUnfoldrMLarge #-}
sourceUnfoldrMLarge :: MonadAsync m => Int -> Int -> Stream m LargeObject
sourceUnfoldrMLarge count start = S.unfoldrM step start

    where

    obj x = LargeObject x 1 2 3 4 5 6 7 8 9

    step cnt =
        if cnt > start + count
        then return Nothing
        else return (Just (obj cnt, cnt + 1))

{-# INLINE benchIOLarge #-}
benchIOLarge
    :: (NFData b)
    => Int -> String -> (Stream IO LargeObject -> IO b) -> Benchmark
benchIOLarge value name f =
    bench name $ nfIO $ randomRIO (1,1) >>= f . sourceUnfoldrMLarge value

{-# INLINE reverse #-}
reverse :: MonadIO m => Int -> Stream m Int -> m ()
reverse n = composeN n S.reverse

{-# INLINE reverseGeneric #-}
reverseGeneric :: MonadIO m => Int -> Stream m Int -> m ()
reverseGeneric n = composeN n S.reverseGeneric

{-# INLINE reverseLarge #-}
reverseLarge :: MonadIO m => Stream m LargeObject -> m ()
reverseLarge = S.fold FL.drain . S.reverse

{-# INLINE reverseGenericLarge #-}
reverseGenericLarge :: MonadIO m => Stream m LargeObject -> m ()
reverseGenericLarge = S.fold FL.drain . S.reverseGeneric

o_n_heap_buffering :: Int -> [Benchmark]
o_n_heap_buffering value =
    [ bgroup "buffered"
        [
        -- Reversing a stream
          benchIOSink value "reverse" (reverse 1)
        , benchIOSink value "reverseGeneric" (reverseGeneric 1)
        , benchIOLarge value "reverse large" reverseLarge
        , benchIOLarge value "reverseGeneric large" reverseGenericLarge

#ifdef USE_PRELUDE
        , benchIOSink value "mkAsync" (mkAsync fromSerial)
#endif
        ]
    ]

-------------------------------------------------------------------------------
-- Grouping/Splitting
-------------------------------------------------------------------------------

#ifdef USE_PRELUDE
{-# INLINE classifySessionsOf #-}
classifySessionsOf :: MonadAsync m => (Int -> Int) -> Stream m Int -> m ()
classifySessionsOf getKey =
      Common.drain
    . S.classifySessionsOf
        (const (return False)) 3 (FL.take 10 FL.sum)
    . S.timestamped
    . fmap (\x -> (getKey x, x))

{-# INLINE classifySessionsOfHash #-}
classifySessionsOfHash :: MonadAsync m =>
    (Int -> Int) -> Stream m Int -> m ()
classifySessionsOfHash getKey =
      Common.drain
    . S.classifySessionsByGeneric
        (Proxy :: Proxy (HashMap k))
        1 False (const (return False)) 3 (FL.take 10 FL.sum)
    . S.timestamped
    . fmap (\x -> (getKey x, x))

o_n_space_grouping :: Int -> [Benchmark]
o_n_space_grouping value =
    -- Buffering operations using heap proportional to group/window sizes.
    [ bgroup "grouping"
        [ benchIOSink value "classifySessionsOf (10000 buckets)"
            (classifySessionsOf (getKey 10000))
        , benchIOSink value "classifySessionsOf (64 buckets)"
            (classifySessionsOf (getKey 64))
        , benchIOSink value "classifySessionsOfHash (10000 buckets)"
            (classifySessionsOfHash (getKey 10000))
        , benchIOSink value "classifySessionsOfHash (64 buckets)"
            (classifySessionsOfHash (getKey 64))
        ]
    ]

    where

    getKey n = (`mod` n)
#endif

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

#ifdef USE_PRELUDE
{-# INLINE filterScanl1 #-}
filterScanl1 :: MonadIO m => Int -> Stream m Int -> m ()
filterScanl1 n = composeN n $ S.scanl1' (+) . S.filter (<= maxBound)
#endif

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
sumProductFold :: Monad m => Stream m Int -> m (Int, Int)
sumProductFold = Common.foldl' (\(s, p) x -> (s + x, p * x)) (0, 1)

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
        , benchIOSink value "foldl-map" foldl'ReduceMap
        , benchIOSink value "sum-product-fold" sumProductFold
        , benchIOSink value "sum-product-scan" sumProductScan
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
#ifdef USE_PRELUDE
        , benchIOSink value "filter-scanl1" (filterScanl1 4)
#endif
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

#ifdef USE_PRELUDE
-- this is quadratic
{-# INLINE iterateScanl1 #-}
iterateScanl1 :: MonadAsync m => Int -> Int -> Int -> Stream m Int
iterateScanl1 = iterateSource (S.scanl1' (+))
#endif

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

{-# INLINE iterateDropWhileFalse #-}
iterateDropWhileFalse :: MonadAsync m
    => Int -> Int -> Int -> Int -> Stream m Int
iterateDropWhileFalse value = iterateSource (S.dropWhile (> (value + 1)))

{-# INLINE iterateDropWhileTrue #-}
iterateDropWhileTrue :: MonadAsync m
    => Int -> Int -> Int -> Int -> Stream m Int
iterateDropWhileTrue value = iterateSource (S.dropWhile (<= (value + 1)))

#ifdef USE_PRELUDE
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
#endif

-- Head recursive operations.
o_n_stack_iterated :: Int -> [Benchmark]
o_n_stack_iterated value = by10 `seq` by100 `seq`
    [ bgroup "iterated"
        [ benchIOSrc "mapM (n/10 x 10)" $ iterateMapM by10 10
        , benchIOSrc "scanl' (quadratic) (n/100 x 100)" $
            iterateScan by100 100
#ifdef USE_PRELUDE
        , benchIOSrc "scanl1' (n/10 x 10)" $ iterateScanl1 by10 10
#endif
        , benchIOSrc "filterEven (n/10 x 10)" $
            iterateFilterEven by10 10
        , benchIOSrc "takeAll (n/10 x 10)" $
            iterateTakeAll value by10 10
        , benchIOSrc "dropOne (n/10 x 10)" $ iterateDropOne by10 10
        , benchIOSrc "dropWhileFalse (n/10 x 10)" $
            iterateDropWhileFalse value by10 10
        , benchIOSrc "dropWhileTrue (n/10 x 10)" $
            iterateDropWhileTrue value by10 10
#ifdef USE_PRELUDE
        , benchIOSink value "tail" tail
        , benchIOSink value "nullHeadTail" nullHeadTail
#endif
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
        , benchIOSink value "zip" (transformZipMapM 1)
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
        , benchIOSink value "zip" (transformZipMapM 4)
#endif
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
            , o_1_space_transformations_mixedX4 size

            -- pipes
            , o_1_space_pipes size
            , o_1_space_pipesX4 size
            ]
        , bgroup (o_n_stack_prefix moduleName) (o_n_stack_iterated size)
        , bgroup (o_n_heap_prefix moduleName) $ Prelude.concat
            [
#ifdef USE_PRELUDE
              o_n_space_grouping size
             ,
#endif
              o_n_space_functor size
            , o_n_heap_buffering size
            ]
        ]
