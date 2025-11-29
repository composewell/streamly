{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      : Main
-- Copyright   : (c) 2018 Composewell Technologies
--
-- License     : BSD3
-- Maintainer  : streamly@composewell.com

module Stream.ConcurrentCommon
    ( allBenchmarks
    , mkParallel
    , unParallel
    , mkFairParallel
    , unFairParallel
    , mkEagerParallel
    , unEagerParallel
    , mkOrderedParallel
    , unOrderedParallel
    )
where

import Stream.Common
    (composeN, benchIO, benchIOSink, benchIOSrc, sourceUnfoldrM)
import Streamly.Data.Stream (Stream)
import Streamly.Internal.Data.Stream.Prelude (MonadAsync, Config)

import qualified Data.List as List
import qualified Streamly.Data.Fold as Fold
import qualified Streamly.Data.Fold.Prelude as Fold
import qualified Streamly.Data.Stream as Stream
import qualified Streamly.Internal.Data.Stream.Prelude as Async
import qualified Streamly.Internal.Data.Stream.Prelude as Stream

import Test.Tasty.Bench
import Prelude hiding (mapM)
import Streamly.Benchmark.Common
import Streamly.Data.Stream.MkType

-- XXX Write inspection tests to make sure no dictionaries are being passed
-- around to find specialization issues. Could be really bad for perf.

-------------------------------------------------------------------------------
-- Mapping
-------------------------------------------------------------------------------

{-# INLINE mapM #-}
mapM ::
       MonadAsync m
    => (Config -> Config)
    -> Int
    -> Stream m Int
    -> m ()
mapM f n = composeN n $ Async.parMapM f return

o_1_space_mapping :: Int -> (Config -> Config) -> [Benchmark]
o_1_space_mapping value f =
    [ bgroup "mapping"
        [ benchIOSink value "mapM" $ mapM f 1
        ]
    ]

-------------------------------------------------------------------------------
-- Size conserving transformations (reordering, buffering, etc.)
-------------------------------------------------------------------------------

o_n_heap_benchmarks :: Int -> (Config -> Config) -> [Benchmark]
o_n_heap_benchmarks value f =
    [ bgroup "buffered"
        [ benchIOSink value "parBuffered"
            (Stream.fold Fold.drain . Async.parBuffered f)
        , benchIOSink value "fmap parBuffered"
            (Stream.fold Fold.drain . fmap (+1) . Async.parBuffered f)
        ]
    ]

-------------------------------------------------------------------------------
-- Joining
-------------------------------------------------------------------------------

{-# INLINE async2 #-}
async2 :: (Config -> Config) -> Int -> Int -> IO ()
async2 f count n =
    Stream.fold Fold.drain
        $ Async.parTwo f
            (sourceUnfoldrM count n) (sourceUnfoldrM count (n + 1))

{-# INLINE concatAsync2 #-}
concatAsync2 :: (Config -> Config) -> Int -> Int -> IO ()
concatAsync2 f count n =
    Stream.fold Fold.drain
        $ Async.parConcat f
        $ Stream.fromList
            [sourceUnfoldrM count n, sourceUnfoldrM count (n + 1)]

{-# INLINE parMergeByM #-}
parMergeByM :: (Config -> Config) -> Int -> Int -> IO ()
parMergeByM f count n =
    Stream.fold Fold.drain
        $ Async.parMergeByM f
        (\a b -> return (a `compare` b))
        (sourceUnfoldrM count n)
        (sourceUnfoldrM count (n + 1))

{-# INLINE parMergeBy #-}
parMergeBy :: (Config -> Config) -> Int -> Int -> IO ()
parMergeBy f count n =
    Stream.fold Fold.drain
        $ Async.parMergeBy f
        compare
        (sourceUnfoldrM count n)
        (sourceUnfoldrM count (n + 1))

{-# INLINE parZipWithM #-}
parZipWithM :: (Config -> Config) -> Int -> Int -> IO ()
parZipWithM f count n =
    Stream.fold Fold.drain
        $ Async.parZipWithM f
        (curry return)
        (sourceUnfoldrM count n)
        (sourceUnfoldrM count (n + 1))

{-# INLINE parZipWith #-}
parZipWith :: (Config -> Config) -> Int -> Int -> IO ()
parZipWith f count n =
    Stream.fold Fold.drain
        $ Async.parZipWith f
        (,)
        (sourceUnfoldrM count n)
        (sourceUnfoldrM count (n + 1))

parZipApply :: MonadAsync m => Stream m (a -> b) -> Stream m a -> Stream m b
parZipApply = Stream.parZipWith id id

$(mkZipType "ParZip" "parZipApply" True)

{-# INLINE zipApplicative #-}
zipApplicative :: Int -> Int -> IO ()
zipApplicative count start =
    Stream.fold Fold.drain $ unParZip $
        (+) <$> mkParZip (sourceUnfoldrM count start)
            <*> mkParZip (sourceUnfoldrM count (start + 1))

{-# INLINE parTap #-}
parTap :: (Fold.Config -> Fold.Config) -> Int -> Int -> IO ()
parTap f count n =
    Stream.fold Fold.drain
        $ Stream.tap (Fold.parBuffered f Fold.sum) (sourceUnfoldrM count n)

o_1_space_joining :: Int -> (Config -> Config) -> [Benchmark]
o_1_space_joining value f =
    [ bgroup "joining (2 of n/2)"
        [ benchIOSrc1 "parTwo" (async2 f (value `div` 2))
        , benchIOSrc1 "parConcat" (concatAsync2 f (value `div` 2))
        , benchIOSrc1 "parMergeByM" (parMergeByM f (value `div` 2))
        , benchIOSrc1 "parMergeBy" (parMergeBy f (value `div` 2))
        , benchIOSrc1 "parZipWithM" (parZipWithM f (value `div` 2))
        , benchIOSrc1 "parZipWith" (parZipWith f (value `div` 2))
        , benchIO "parZipApplicative" $ zipApplicative value
        ]
    -- XXX use configurable modifier, put this in concurrent fold benchmarks
    , benchIOSrc1 "tap (Fold.parBuffered id Fold.sum)" (parTap id value)
    ]

-------------------------------------------------------------------------------
-- Concat
-------------------------------------------------------------------------------

{-# INLINE sourceFoldMapWith #-}
sourceFoldMapWith :: (Config -> Config) -> Int -> Int -> Stream IO Int
sourceFoldMapWith f value n =
    Async.parConcatMap f Stream.fromPure $ Stream.fromList [n..n+value]

{-# INLINE sourceFoldMapWithStream #-}
sourceFoldMapWithStream :: (Config -> Config) -> Int -> Int -> Stream IO Int
sourceFoldMapWithStream f value n =
    Async.parConcatMap f Stream.fromPure
        $ Stream.enumerateFromTo n (n + value)

{-# INLINE concatFoldableWith #-}
concatFoldableWith :: (Config -> Config) -> Int -> Int -> Stream IO Int
concatFoldableWith f value n =
    let step x =
            if x <= n + value
            then Just (Stream.fromPure x, x + 1)
            else Nothing
        list = List.unfoldr step n
     in Async.parConcat f (Stream.fromList  list)

o_1_space_concatFoldable :: Int -> (Config -> Config) -> [Benchmark]
o_1_space_concatFoldable value f =
    [ bgroup "concat-foldable"
        [ benchIOSrc "foldMapWith (<>) (List)"
            (sourceFoldMapWith f value)
        , benchIOSrc "foldMapWith (<>) (Stream)"
            (sourceFoldMapWithStream f value)
        , benchIOSrc "S.concatFoldableWith (<>) (List)"
            (concatFoldableWith f value)
        ]
    ]

{-# INLINE concatMapStreamsWith #-}
concatMapStreamsWith
    :: (Config -> Config)
    -> Int
    -> Int
    -> Int
    -> IO ()
concatMapStreamsWith f outer inner n =
    Stream.fold Fold.drain
        $ Async.parConcatMap f (sourceUnfoldrM inner) (sourceUnfoldrM outer n)

{-# INLINE concatFmapStreamsWith #-}
concatFmapStreamsWith
    :: (Config -> Config)
    -> Int
    -> Int
    -> Int
    -> IO ()
concatFmapStreamsWith f outer inner n =
    Stream.fold Fold.drain
        $ Async.parConcat f
        $ fmap (sourceUnfoldrM inner) (sourceUnfoldrM outer n)

o_1_space_concatMap :: String -> Int -> (Config -> Config) -> [Benchmark]
o_1_space_concatMap label value f =
    value2 `seq`
        [ bgroup ("concat" ++ label)
            [ benchIO "parConcatMap (n of 1)"
                  (concatMapStreamsWith f value 1)
            , benchIO "parConcatMap (sqrt n of sqrt n)"
                  (concatMapStreamsWith f value2 value2)
            , benchIO "parConcatMap (1 of n)"
                  (concatMapStreamsWith f 1 value)
            , benchIO "concat . fmap (n of 1)"
                  (concatFmapStreamsWith f value 1)
            ]
        ]

    where

    value2 = round $ sqrt (fromIntegral value :: Double)

o_1_space_benchmarks :: Int -> (Config -> Config) -> [Benchmark]
o_1_space_benchmarks value modifier =
    concat
        [ o_1_space_mapping value modifier
        , o_1_space_joining value modifier
        , o_1_space_concatFoldable value modifier
        , o_1_space_concatMap "" value modifier
        , o_1_space_concatMap "-maxThreads-1" value (modifier . Async.maxThreads 1)
        , o_1_space_concatMap "-maxBuffer-1 1/10" (value `div` 10) (modifier . Async.maxBuffer 1)
        , o_1_space_concatMap "-rate-Nothing" value (modifier . Async.rate Nothing)
        ]

-------------------------------------------------------------------------------
-- Apply
-------------------------------------------------------------------------------

{-# INLINE parCrossApply #-}
parCrossApply :: (Config -> Config) -> Int -> Int -> IO ()
parCrossApply f linearCount start =
    Stream.fold Fold.drain
        $ Async.parCrossApply f
            (fmap (+) (sourceUnfoldrM nestedCount2 start))
            (sourceUnfoldrM nestedCount2 start)

    where

    nestedCount2 = round (fromIntegral linearCount**(1/2::Double))

-------------------------------------------------------------------------------
-- Monad Types
-------------------------------------------------------------------------------

parallelBind :: MonadAsync m => Stream m a -> (a -> Stream m b) -> Stream m b
parallelBind = flip (Stream.parConcatMap id)
$(mkCrossType "Parallel" "parallelBind" True)

fairParallelBind :: MonadAsync m => Stream m a -> (a -> Stream m b) -> Stream m b
fairParallelBind = flip (Stream.parConcatMap (Stream.interleaved True))
$(mkCrossType "FairParallel" "fairParallelBind" True)

eagerParallelBind :: MonadAsync m => Stream m a -> (a -> Stream m b) -> Stream m b
eagerParallelBind = flip (Stream.parConcatMap (Stream.eager True))
$(mkCrossType "EagerParallel" "eagerParallelBind" True)

orderedBind :: MonadAsync m => Stream m a -> (a -> Stream m b) -> Stream m b
orderedBind = flip (Stream.parConcatMap (Stream.ordered True))
$(mkCrossType "OrderedParallel" "orderedBind" True)

-------------------------------------------------------------------------------
-- Monadic benchmarks
-------------------------------------------------------------------------------

{-# INLINE applicative #-}
applicative :: Monad (t IO) => (Stream IO Int -> t IO Int) -> (t IO Int -> Stream IO Int) -> Int -> Int -> IO ()
applicative mk un linearCount start =
    Stream.fold Fold.drain $ un $
        (+) <$> mk (sourceUnfoldrM nestedCount2 start)
            <*> mk (sourceUnfoldrM nestedCount2 (start + 1))
    where

    nestedCount2 = round (fromIntegral linearCount**(1/2::Double))


{-# INLINE monad2 #-}
monad2 :: Monad (t IO) => (Stream IO Int -> t IO Int) -> (t IO Int -> Stream IO Int) -> Int -> Int -> IO ()
monad2 mk un linearCount start =
    Stream.fold Fold.drain $ un $ do
        x <- mk $ sourceUnfoldrM nestedCount2 start
        y <- mk $ sourceUnfoldrM nestedCount2 start
        return $ x + y

    where

    nestedCount2 = round (fromIntegral linearCount**(1/2::Double))

{-# INLINE monadTakeSome #-}
monadTakeSome :: Monad (t IO) => (Stream IO Int -> t IO Int) -> (t IO Int -> Stream IO Int) -> Int -> Int -> IO ()
monadTakeSome mk un linearCount start =
    Stream.fold Fold.drain $ Stream.take 1000 $ un $ do
        x <- mk $ sourceUnfoldrM nestedCount2 start
        y <- mk $ sourceUnfoldrM nestedCount2 start
        return $ x + y

    where

    nestedCount2 = round (fromIntegral linearCount**(1/2::Double))

{-# INLINE monad3 #-}
monad3 :: Monad (t IO) => (Stream IO Int -> t IO Int) -> (t IO Int -> Stream IO Int) -> Int -> Int -> IO ()
monad3 mk un linearCount start =
    Stream.fold Fold.drain $ un $ do
        x <- mk $ sourceUnfoldrM nestedCount3 start
        y <- mk $ sourceUnfoldrM nestedCount3 start
        z <- mk $ sourceUnfoldrM nestedCount3 start
        return $ x + y + z

    where

    nestedCount3 = round (fromIntegral linearCount**(1/3::Double))

{-# INLINE monadFilterAllOut #-}
monadFilterAllOut :: Monad (t IO) => (Stream IO Int -> t IO Int) -> (t IO Int -> Stream IO Int) -> Int -> Int -> IO ()
monadFilterAllOut mk un linearCount start =
    Stream.fold Fold.drain $ un $ do
        x <- mk $ sourceUnfoldrM nestedCount2 start
        y <- mk $ sourceUnfoldrM nestedCount2 start
        let s = x + y
        if s < 0
        then return s
        else mk Stream.nil

    where

    nestedCount2 = round (fromIntegral linearCount**(1/2::Double))

{-# INLINE monadFilterAllIn #-}
monadFilterAllIn :: Monad (t IO) => (Stream IO Int -> t IO Int) -> (t IO Int -> Stream IO Int) -> Int -> Int -> IO ()
monadFilterAllIn mk un linearCount start =
    Stream.fold Fold.drain $ un $ do
        x <- mk $ sourceUnfoldrM nestedCount2 start
        y <- mk $ sourceUnfoldrM nestedCount2 start
        let s = x + y
        if s > 0
        then return s
        else mk Stream.nil

    where

    nestedCount2 = round (fromIntegral linearCount**(1/2::Double))

{-# INLINE monadFilterSome #-}
monadFilterSome :: Monad (t IO) => (Stream IO Int -> t IO Int) -> (t IO Int -> Stream IO Int) -> Int -> Int -> IO ()
monadFilterSome mk un linearCount start =
    Stream.fold Fold.drain $ un $ do
        x <- mk $ sourceUnfoldrM nestedCount2 start
        y <- mk $ sourceUnfoldrM nestedCount2 start
        let s = x + y
        if odd s
        then return s
        else mk Stream.nil

    where

    nestedCount2 = round (fromIntegral linearCount**(1/2::Double))

{-
{-# INLINE monadBreak #-}
monadBreak :: Monad (t IO) => (Stream IO Int -> t IO Int) -> (t IO Int -> Stream IO Int) -> Int -> Int -> IO ()
monadBreak mk un linearCount start =
    Stream.fold Fold.drain $ un $ do
        x <- mk $ sourceUnfoldrM nestedCount2 start
        y <- mk $ sourceUnfoldrM nestedCount2 start
        let s = x + y
        if s > nestedCount2
        then error "break"
        else return s

    where

    nestedCount2 = round (fromIntegral linearCount**(1/2::Double))
-}

crossBenchmarks :: Monad (t IO) =>
       (Stream IO Int -> t IO Int)
    -> (t IO Int -> Stream IO Int)
    -> Int -> (Stream.Config -> Stream.Config) -> [Benchmark]
crossBenchmarks mk un len f =
    [ bgroup "cross-product"
        [ benchIO "parCrossApply" $ parCrossApply f len
        , benchIO "monadAp" $ applicative mk un len
        , benchIO "monad2Levels" $ monad2 mk un len
        , benchIO "monad3Levels" $ monad3 mk un len
        , benchIO "monad2FilterAllOut" $ monadFilterAllOut mk un len
        , benchIO "monad2FilterAllIn" $ monadFilterAllIn mk un len
        , benchIO "monad2FilterSome" $ monadFilterSome mk un len
        , benchIO "monad2TakeSome" $ monadTakeSome mk un len
        -- , benchIO "monad2Break" $ monadBreak mk un len
        ]
    ]

-------------------------------------------------------------------------------
-- Benchmark sets
-------------------------------------------------------------------------------

allBenchmarks :: Monad (t IO) =>
       (Stream IO Int -> t IO Int)
    -> (t IO Int -> Stream IO Int)
    -> String -> Bool -> (Config -> Config) -> Int -> [Benchmark]
allBenchmarks mk un moduleName wide modifier value =
    [ bgroup (o_1_space_prefix moduleName) $ concat
        [ o_1_space_benchmarks value modifier
        ] ++ if wide then [] else crossBenchmarks mk un value modifier
    , bgroup (o_n_heap_prefix moduleName) $ concat
        [ o_n_heap_benchmarks value modifier
        ] ++ if wide then crossBenchmarks mk un value modifier else []
    ]
