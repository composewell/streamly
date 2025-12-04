-- |
-- Module      : Streamly.Benchmark.Data.Fold
-- Copyright   : (c) 2018 Composewell
--
-- License     : MIT
-- Maintainer  : streamly@composewell.com

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}

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
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Char (ord)
import Streamly.Internal.Data.Array (Array)
import Data.Functor.Identity (Identity(..))
import Data.Map.Strict (Map)
import Data.IntMap.Strict (IntMap)
import Data.Monoid (Last(..), Sum(..))
import Data.Word (Word8)
import System.IO (Handle)
import System.Random (randomRIO)

import Streamly.Internal.Data.Stream (Stream)
import Streamly.Internal.Data.Fold (Fold(..))
import Streamly.Internal.Data.MutArray (MutArray)

import qualified Streamly.Internal.Data.Array as Array
import qualified Streamly.Internal.Data.Fold as FL
import qualified Streamly.Internal.Data.Scanl as Scanl
import qualified Streamly.Internal.Data.Fold as Fold
import qualified Streamly.Internal.Data.Parser as Parser
import qualified Streamly.Internal.Data.Pipe as Pipe
-- import qualified Streamly.Internal.Data.Scan as Scan
import qualified Streamly.Internal.Data.Stream as Stream
import qualified Streamly.Internal.Data.Unfold as Unfold
import qualified Streamly.Internal.FileSystem.Handle as Handle
import qualified Streamly.Internal.Unicode.Stream as Unicode

import Test.Tasty.Bench hiding (env)
import Streamly.Benchmark.Common
import Streamly.Benchmark.Common.Handle
import Prelude hiding (last, length, all, any, take, unzip, sequence_, filter)

#ifdef INSPECTION
import Streamly.Internal.Data.Stream (Step(..))

import qualified Streamly.Internal.Data.MutArray as MutArray
import qualified Streamly.Internal.Data.Unfold as Unfold

import Test.Inspection
#endif

-- We need a monadic bind here to make sure that the function f does not get
-- completely optimized out by the compiler in some cases.

{-# INLINE sourceUnfoldrM #-}
sourceUnfoldrM :: Monad m => Int -> Int -> Stream m Int
sourceUnfoldrM value n = Stream.unfoldrM step n
    where
    step cnt =
        if cnt > n + value
        then return Nothing
        else return (Just (cnt, cnt + 1))

{-# INLINE source #-}
source :: Monad m => Int -> Int -> Stream m Int
source = sourceUnfoldrM

-- | Takes a fold method, and uses it with a default source.
{-# INLINE benchIOSink #-}
benchIOSink
    :: NFData b
    => Int -> String -> (Stream IO Int -> IO b) -> Benchmark
benchIOSink value name f = bench name $ nfIO $ randomRIO (1,1) >>= f . source value

-------------------------------------------------------------------------------
-- Folds
-------------------------------------------------------------------------------

{-# INLINE any #-}
any :: (Monad m, Ord a) => a -> Stream m a -> m Bool
any value = Stream.fold (FL.any (> value))

{-# INLINE all #-}
all :: (Monad m, Ord a) => a -> Stream m a -> m Bool
all value = Stream.fold (FL.all (<= value))

{-# INLINE take #-}
take :: Monad m => Int -> Stream m a -> m ()
take value = Stream.fold (FL.take value FL.drain)

{-# INLINE sequence_ #-}
sequence_ :: Monad m => Int -> Fold m a ()
sequence_ value =
    foldr f (FL.fromPure ()) (Prelude.replicate value (FL.take 1 FL.drain))

    where

    {-# INLINE f #-}
    f m k = FL.concatMap (const k) m

-------------------------------------------------------------------------------
-- Filter
-------------------------------------------------------------------------------

{-# INLINE filter #-}
filter :: Monad m => Int -> Stream m Int -> m ()
filter _ = Stream.fold (FL.filter even FL.drain)

{-# INLINE scanMaybe #-}
scanMaybe :: Monad m => Int -> Stream m Int -> m ()
scanMaybe _ = Stream.fold (FL.postscanlMaybe (Scanl.filtering even) FL.drain)

{-# INLINE scanMaybe2 #-}
scanMaybe2 :: Monad m => Int -> Stream m Int -> m ()
scanMaybe2 _ =
    Stream.fold
        $ FL.postscanlMaybe (Scanl.filtering even)
        $ FL.postscanlMaybe (Scanl.filtering odd) FL.drain

-------------------------------------------------------------------------------
-- Splitting in two
-------------------------------------------------------------------------------

{-# INLINE splitAllAny #-}
splitAllAny :: Monad m => Int -> Stream m Int -> m (Bool, Bool)
splitAllAny value =
    Stream.fold
        (FL.splitWith (,)
            (FL.all (<= (value `div` 2)))
            (FL.any (> value))
        )

{-# INLINE split_ #-}
split_ :: Monad m => Int -> Stream m Int -> m Bool
split_ value =
    Stream.fold
        (FL.split_
            (FL.all (<= (value `div` 2)))
            (FL.any (> value))
        )

{-# INLINE shortest #-}
shortest :: Monad m => Stream m Int -> m (Either Int Int)
shortest = Stream.fold (FL.shortest FL.sum FL.length)

{-# INLINE longest #-}
longest :: Monad m => Stream m Int -> m (Either Int Int)
longest = Stream.fold (FL.longest FL.sum FL.length)

{-# INLINE foldBreak #-}
foldBreak :: Monad m => Stream m Int -> m ()
foldBreak s = do
    (r, s1) <- Stream.foldBreak (FL.take 1 FL.length) s
    when (r /= 0) $ foldBreak s1

-------------------------------------------------------------------------------
-- Split generated streams (not a file)
-------------------------------------------------------------------------------

{-# INLINE many #-}
many :: Monad m => Stream m Int -> m ()
many = Stream.fold (FL.many (FL.take 1 FL.drain) FL.drain)

{-# INLINE takeEndBy_ #-}
takeEndBy_ :: Monad m => Int -> Stream m Int -> m ()
takeEndBy_ value = Stream.fold (FL.takeEndBy_ (>= value) FL.drain)

-------------------------------------------------------------------------------
-- Splitting a file stream into a stream by serial application
-------------------------------------------------------------------------------

lf :: Word8
lf = fromIntegral (ord '\n')

toarr :: String -> Array Word8
toarr = Array.fromList . map (fromIntegral . ord)

-- | Split on line feed.
fileInfixTakeEndBy_ :: Handle -> IO Int
fileInfixTakeEndBy_ inh =
    Stream.fold Fold.length
        $ Stream.foldManyPost (FL.takeEndBy_ (== lf) Fold.drain)
        $ Handle.read inh -- >>= print

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'fileInfixTakeEndBy_
inspect $ 'fileInfixTakeEndBy_ `hasNoType` ''Step
inspect $ 'fileInfixTakeEndBy_ `hasNoType` ''Unfold.ConcatState -- FH.read/UF.many
inspect $ 'fileInfixTakeEndBy_ `hasNoType` ''MutArray.ArrayUnsafe  -- FH.read/A.read
#endif

-- | Split on line feed.
fileSuffixTakeEndBy_ :: Handle -> IO Int
fileSuffixTakeEndBy_ inh =
    Stream.fold Fold.length
        $ Stream.foldMany
            (Fold.takeEndBy_ (== lf) Fold.drain)
            (Handle.read inh)
     -- >>= print

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'fileSuffixTakeEndBy_
inspect $ 'fileSuffixTakeEndBy_ `hasNoType` ''Step
inspect $ 'fileSuffixTakeEndBy_ `hasNoType` ''Unfold.ConcatState -- FH.read/UF.many
inspect $ 'fileSuffixTakeEndBy_ `hasNoType` ''MutArray.ArrayUnsafe  -- FH.read/A.read
#endif

-- | Split on line feed.
parseFileSuffixTakeEndBy_ :: Handle -> IO Int
parseFileSuffixTakeEndBy_ inh =
    Stream.fold Fold.length
        $ Stream.parseMany
            (Parser.fromFold $ Fold.takeEndBy_ (== lf) Fold.drain)
            (Handle.read inh)
     -- >>= print

-- | Split suffix with line feed.
fileSuffixTakeEndBy :: Handle -> IO Int
fileSuffixTakeEndBy inh =
    Stream.fold Fold.length
        $ Stream.foldMany
            (Fold.takeEndBy (== lf) Fold.drain)
            (Handle.read inh)
     -- >>= print

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'fileSuffixTakeEndBy
inspect $ 'fileSuffixTakeEndBy `hasNoType` ''Step
inspect $ 'fileSuffixTakeEndBy `hasNoType` ''Unfold.ConcatState -- FH.read/UF.many
inspect $ 'fileSuffixTakeEndBy `hasNoType` ''MutArray.ArrayUnsafe  -- FH.read/A.read
#endif

-- | Infix split on a word8 sequence.
splitOnSeq :: String -> Handle -> IO Int
splitOnSeq str inh =
    Stream.fold Fold.length
        $ Stream.foldManyPost (Fold.takeEndBySeq_ (toarr str) Fold.drain)
        $ Handle.read inh -- >>= print

#ifdef INSPECTION
-- inspect $ hasNoTypeClasses 'splitOnSeq
-- inspect $ 'splitOnSeq `hasNoType` ''Step
#endif

-- | Infix split on a word8 sequence.
splitOnSeq100k :: Handle -> IO Int
splitOnSeq100k inh = do
    arr <- Stream.fold Array.create $ Stream.replicate 100000 123
    Stream.fold Fold.length
        $ Stream.foldManyPost (Fold.takeEndBySeq_ arr Fold.drain)
        $ Handle.read inh -- >>= print

-- | Split on suffix sequence.
splitOnSuffixSeq :: String -> Handle -> IO Int
splitOnSuffixSeq str inh =
    Stream.fold Fold.length
        $ Stream.foldMany (Fold.takeEndBySeq_ (toarr str) Fold.drain)
        $ Handle.read inh -- >>= print

#ifdef INSPECTION
-- inspect $ hasNoTypeClasses 'splitOnSuffixSeq
-- inspect $ 'splitOnSuffixSeq `hasNoType` ''Step
#endif

-- | Split on suffix sequence.
splitWithSuffixSeq :: String -> Handle -> IO Int
splitWithSuffixSeq str inh =
    Stream.fold Fold.length
        $ Stream.foldMany (Fold.takeEndBySeq (toarr str) Fold.drain)
        $ Handle.read inh -- >>= print

o_1_space_reduce_read_split :: BenchEnv -> [Benchmark]
o_1_space_reduce_read_split env =
    -- NOTE: keep the benchmark names consistent with Data.Stream.split*
    [ bgroup "FileSplitElem"
        -- Splitting on single element
        [
          mkBench "takeEndBy_ infix (splitOn)" env $ \inh _ ->
            fileInfixTakeEndBy_ inh
        , mkBench "takeEndBy_ suffix (splitOnSuffix)" env $ \inh _ ->
            fileSuffixTakeEndBy_ inh
        , mkBench "takeEndBy_ suffix parseMany (splitOnSuffix)" env
            $ \inh _ -> parseFileSuffixTakeEndBy_ inh
        , mkBench "takeEndBy suffix (splitWithSuffix)" env $ \inh _ ->
            fileSuffixTakeEndBy inh
        ]

    -- Splitting on sequence
    , bgroup "FileSplitSeq"
        [
          -- Infix takeEndBySeq_
          mkBench "takeEndBySeq_ infix empty pattern" env $ \inh _ ->
            splitOnSeq "" inh
        , mkBench "takeEndBySeq_ infix lf" env $ \inh _ ->
            splitOnSeq "\n" inh
        , mkBench "takeEndBySeq_ infix a" env $ \inh _ ->
            splitOnSeq "a" inh
        , mkBench "takeEndBySeq_ infix crlf" env $ \inh _ ->
            splitOnSeq "\r\n" inh
        , mkBench "takeEndBySeq_ infix aa" env $ \inh _ ->
            splitOnSeq "aa" inh
        , mkBench "takeEndBySeq_ infix aaaa" env $ \inh _ ->
            splitOnSeq "aaaa" inh
        , mkBench "takeEndBySeq_ infix abcdefgh" env $ \inh _ ->
            splitOnSeq "abcdefgh" inh
        , mkBench "takeEndBySeq_ infix abcdefghi" env $ \inh _ ->
            splitOnSeq "abcdefghi" inh
        , mkBench "takeEndBySeq_ infix catcatcatcatcat" env $ \inh _ ->
            splitOnSeq "catcatcatcatcat" inh
        , mkBench "takeEndBySeq_ infix abcdefghijklmnopqrstuvwxyz"
            env $ \inh _ -> splitOnSeq "abcdefghijklmnopqrstuvwxyz" inh
        , mkBench "takeEndBySeq_ infix 100k long pattern"
            env $ \inh _ -> splitOnSeq100k inh

          -- Suffix takeEndBySeq_
        , mkBench "takeEndBySeq_ suffix empty pattern" env $ \inh _ ->
            splitOnSuffixSeq "" inh
        , mkBench "takeEndBySeq_ suffix lf" env $ \inh _ ->
            splitOnSuffixSeq "\n" inh
        , mkBench "takeEndBySeq_ suffix crlf" env $ \inh _ ->
            splitOnSuffixSeq "\r\n" inh
        , mkBenchSmall "takeEndBySeq_ suffix abcdefghijklmnopqrstuvwxyz"
            env $ \inh _ -> splitOnSuffixSeq "abcdefghijklmnopqrstuvwxyz" inh

          -- Suffix takeEndBySeq
        , mkBench "takeEndBySeq suffix crlf" env $ \inh _ ->
            splitWithSuffixSeq "\r\n" inh
        , mkBenchSmall "takeEndBySeq suffix abcdefghijklmnopqrstuvwxyz"
            env $ \inh _ -> splitWithSuffixSeq "abcdefghijklmnopqrstuvwxyz" inh
        ]
    ]

-- | Infix split on a character sequence.
splitOnSeqUtf8 :: String -> Handle -> IO Int
splitOnSeqUtf8 str inh =
    -- XXX requires @-fspec-constr-recursive=12@. Maybe due to
    -- decodeUtf8.
    Stream.fold Fold.length
        $ Stream.foldManyPost (Fold.takeEndBySeq_ (Array.fromList str) Fold.drain)
        $ Unicode.decodeUtf8Chunks
        $ Handle.readChunks inh -- >>= print

o_1_space_reduce_toChunks_split :: BenchEnv -> [Benchmark]
o_1_space_reduce_toChunks_split env =
    [ bgroup "FileSplitSeqUtf8"
        [ mkBenchSmall "takeEndBySeq_ infix abcdefgh"
            env $ \inh _ -> splitOnSeqUtf8 "abcdefgh" inh
        , mkBenchSmall "takeEndBySeq_ infix abcdefghijklmnopqrstuvwxyz"
            env $ \inh _ -> splitOnSeqUtf8 "abcdefghijklmnopqrstuvwxyz" inh
        ]
    ]

-------------------------------------------------------------------------------
-- Distributing by parallel application
-------------------------------------------------------------------------------

{-# INLINE teeSumLength #-}
teeSumLength :: Monad m => Stream m Int -> m (Int, Int)
teeSumLength = Stream.fold (FL.teeWith (,) FL.sum FL.length)

{-# INLINE teeAllAny #-}
teeAllAny :: (Monad m, Ord a) => a -> Stream m a -> m (Bool, Bool)
teeAllAny value = Stream.fold (FL.teeWith (,) all_ any_)

    where

    all_ = FL.all (<= value)
    any_ = FL.any (> value)

{-# INLINE teeWithFst #-}
teeWithFst :: Monad m => Stream m Int -> m (Int, Int)
teeWithFst = Stream.fold (FL.teeWithFst (,) FL.sum FL.length)

{-# INLINE teeWithMin #-}
teeWithMin :: Monad m => Stream m Int -> m (Int, Int)
teeWithMin = Stream.fold (FL.teeWithMin (,) FL.sum FL.length)

{-# INLINE distribute #-}
distribute :: Monad m => Stream m Int -> m [Int]
distribute = Stream.fold (FL.distribute [FL.sum, FL.length])

-------------------------------------------------------------------------------
-- Partitioning
-------------------------------------------------------------------------------

{-# INLINE oddEven #-}
oddEven :: Int -> Either Int Int
oddEven x = if odd x then Left x else Right x

{-# INLINE partition #-}
partition :: Monad m => Stream m Int -> m (Int, Int)
partition = Stream.fold $ FL.lmap oddEven (FL.partition FL.sum FL.length)

{-# INLINE partitionByFstM #-}
partitionByFstM :: Monad m => Stream m Int -> m (Int, Int)
partitionByFstM =
    Stream.fold (FL.partitionByFstM (return . oddEven) FL.sum FL.length)

{-# INLINE partitionByMinM #-}
partitionByMinM :: Monad m => Stream m Int -> m (Int, Int)
partitionByMinM =
    Stream.fold (FL.partitionByMinM (return . oddEven) FL.sum FL.length)

{-# INLINE demuxToMap  #-}
demuxToMap :: (Monad m, Ord k) =>
    (a -> k) -> (k -> m (Maybe (Fold m a b))) -> Stream m a -> m (Map k b)
demuxToMap f g = Stream.fold (FL.demuxerToContainer f g)

{-# INLINE demuxToIntMap  #-}
demuxToIntMap :: Monad m =>
    (a -> Int) -> (Int -> m (Maybe (Fold m a b))) -> Stream m a -> m (IntMap b)
demuxToIntMap f g = Stream.fold (FL.demuxerToContainer f g)

{-# INLINE demuxToMapIO  #-}
demuxToMapIO :: (MonadIO m, Ord k) =>
    (a -> k) -> (k -> m (Maybe (Fold m a b))) -> Stream m a -> m (Map k b)
demuxToMapIO f g = Stream.fold (FL.demuxerToContainerIO f g)

{-# INLINE toMap #-}
toMap ::
       (Monad m, Ord k, Num a) => (a -> k) -> Stream m a -> m (Map k a)
toMap f = Stream.fold (FL.toContainer f FL.sum)

{-# INLINE toIntMap #-}
toIntMap ::
       (Monad m, Num a) => (a -> Int) -> Stream m a -> m (IntMap a)
toIntMap f = Stream.fold (FL.toContainer f FL.sum)

{-# INLINE toMapIO #-}
toMapIO ::
       (MonadIO m, Ord k, Num a) => (a -> k) -> Stream m a -> m (Map k a)
toMapIO f = Stream.fold (FL.toContainerIO f FL.sum)

{-# INLINE toIntMapIO #-}
toIntMapIO ::
       (MonadIO m, Num a) => (a -> Int) -> Stream m a -> m (IntMap a)
toIntMapIO f = Stream.fold (FL.toContainerIO f FL.sum)

-------------------------------------------------------------------------------
-- unzip
-------------------------------------------------------------------------------

{-# INLINE unzip #-}
unzip :: Monad m => Stream m Int -> m (Int, Int)
unzip = Stream.fold $ FL.lmap (\a -> (a, a)) (FL.unzip FL.sum FL.length)

{-# INLINE unzipWithFstM #-}
unzipWithFstM :: Monad m => Stream m Int -> m (Int, Int)
unzipWithFstM = do
    let f a = return (a + 1, a)
    Stream.fold (FL.unzipWithFstM f FL.sum FL.length)

{-# INLINE unzipWithMinM #-}
unzipWithMinM :: Monad m => Stream m Int -> m (Int, Int)
unzipWithMinM = do
    let f a = return (a + 1, a)
    Stream.fold (FL.unzipWithMinM f FL.sum FL.length)

-------------------------------------------------------------------------------
-- Nested
-------------------------------------------------------------------------------

{-# INLINE unfoldMany #-}
unfoldMany :: Int -> Benchmarkable
unfoldMany val =
    nfIO
        $ Stream.fold (FL.unfoldMany Unfold.replicateM FL.drain)
        $ Stream.fromPure (val, randomRIO (1, 1 :: Int))

-------------------------------------------------------------------------------
-- Benchmarks
-------------------------------------------------------------------------------

moduleName :: String
moduleName = "Data.Fold"

instance NFData (MutArray a) where
    {-# INLINE rnf #-}
    rnf _ = ()

instance NFData a => NFData (Stream Identity a) where
    {-# INLINE rnf #-}
    rnf xs = runIdentity $ Stream.fold (FL.foldl' (\_ x -> rnf x) ()) xs

o_1_space_serial_elimination :: Int -> [Benchmark]
o_1_space_serial_elimination value =
    [ bgroup "elimination"
        [ benchIOSink value "drain" (Stream.fold FL.drain)
        , benchIOSink value "drainBy" (Stream.fold (FL.drainMapM return))
        , benchIOSink value "drainN" (Stream.fold (FL.drainN value))
        , benchIOSink value "last" (Stream.fold FL.latest)
        , benchIOSink value "length" (Stream.fold FL.length)
        , benchIOSink value "top" (Stream.fold $ FL.top 10)
        , benchIOSink value "bottom" (Stream.fold $ FL.bottom 10)
        , benchIOSink value "sum" (Stream.fold FL.sum)
        , benchIOSink value "sum (foldMap)" (Stream.fold (FL.foldMap Sum))
        , benchIOSink value "product" (Stream.fold FL.product)
        , benchIOSink value "maximumBy" (Stream.fold (FL.maximumBy compare))
        , benchIOSink value "maximum" (Stream.fold FL.maximum)
        , benchIOSink value "minimumBy" (Stream.fold (FL.minimumBy compare))
        , benchIOSink value "minimum" (Stream.fold FL.minimum)
        , benchIOSink
              value
              "mean"
              (Stream.fold FL.mean . fmap (fromIntegral :: Int -> Double))
{-
        -- These are already benchmarked in streamly-statistics package. If we
        -- still want to keep these tests here, perhaps we should move them to a
        -- different module so we can remove -fno-warn-warnings-deprecations.

        , benchIOSink
              value
              "variance"
              (Stream.fold FL.variance . fmap (fromIntegral :: Int -> Double))
        , benchIOSink
              value
              "stdDev"
              (Stream.fold FL.stdDev . fmap (fromIntegral :: Int -> Double))
-}
        , benchIOSink
              value
              "mconcat"
              (Stream.fold FL.mconcat . fmap (Last . Just))
        , benchIOSink
              value
              "foldMap"
              (Stream.fold (FL.foldMap (Last . Just)))
        , benchIOSink
              value
              "foldMapM"
              (Stream.fold (FL.foldMapM (return . Last . Just)))
        , benchIOSink value "index" (Stream.fold (FL.index (value + 1)))
        -- , benchIOSink value "head" (Stream.fold FL.head)
        , benchIOSink value "find" (Stream.fold (FL.find (== (value + 1))))
        , benchIOSink
              value
              "lookup"
              (Stream.fold (FL.lmap (\a -> (a, a)) (FL.lookup (value + 1))))
        , benchIOSink
              value
              "findIndex"
              (Stream.fold (FL.findIndex (== (value + 1))))
        , benchIOSink
              value
              "elemIndex"
              (Stream.fold (FL.elemIndex (value + 1)))
        -- , benchIOSink value "null" (Stream.fold FL.null)
        , benchIOSink value "elem" (Stream.fold (FL.elem (value + 1)))
        , benchIOSink value "notElem" (Stream.fold (FL.notElem (value + 1)))
        , benchIOSink value "all" $ all value
        , benchIOSink value "any" $ any value
        , benchIOSink value "take" $ take value
        , benchIOSink value "takeEndBy_" $ takeEndBy_ value
        , benchIOSink value "and" (Stream.fold FL.and . fmap (<= (value + 1)))
        , benchIOSink value "or" (Stream.fold FL.or . fmap (> (value + 1)))
        ]
    ]

o_1_space_serial_transformation :: Int -> [Benchmark]
o_1_space_serial_transformation value =
    [ bgroup "transformation"
        [ benchIOSink value "map" (Stream.fold (FL.lmap (+ 1) FL.drain))
        , let f x = if even x then Just x else Nothing
              fld = FL.mapMaybe f FL.drain
           in benchIOSink value "mapMaybe" (Stream.fold fld)
        , benchIOSink
              value
              "rsequence"
              (Stream.fold (FL.rmapM id (return <$> FL.drain)))
        , benchIOSink value "rmapM" (Stream.fold (FL.rmapM return FL.drain))
        , benchIOSink
              value
              "pipe-mapM"
              (Stream.fold
                   (FL.pipe
                        (Pipe.mapM (\x -> return $ x + 1))
                        FL.drain))
{-
        , benchIOSink
              value
              "fold-runScan"
              (Stream.fold
                   (FL.runScan
                        (Scan.functionM (\x -> return $ x + 1))
                        FL.drain))
-}
        , benchIOSink
            value
            "fold-scan"
            (Stream.fold $ FL.scanl Scanl.sum FL.drain)
        , benchIOSink
            value
            "fold-scanMany"
            (Stream.fold $ FL.scanlMany (Scanl.take 2 Scanl.drain) FL.drain)
        , benchIOSink
            value
            "fold-postscan"
            (Stream.fold $ FL.postscanl Scanl.sum FL.drain)
        ]
    ]

o_1_space_serial_composition :: Int -> [Benchmark]
o_1_space_serial_composition value =
      [ bgroup
            "composition"
            [ benchIOSink value "filter even" $ filter value
            , benchIOSink value "scanMaybe even" $ scanMaybe value
            , benchIOSink value "scanMaybe even, odd" $ scanMaybe2 value
            , benchIOSink value "foldBreak (recursive)" foldBreak
            , benchIOSink value "splitWith (all, any)" $ splitAllAny value
            , benchIOSink value "split_ (all, any)" $ split_ value
            , benchIOSink value "tee (all, any)" $ teeAllAny value
            , benchIOSink value "many drain (take 1)" many
            , bench "unfoldMany" $ unfoldMany value
            , benchIOSink value "shortest (sum, length)" shortest
            , benchIOSink value "longest (sum, length)" longest
            , benchIOSink value "tee (sum, length)" teeSumLength
            , benchIOSink value "teeWithFst (sum, length)" teeWithFst
            , benchIOSink value "teeWithMin (sum, length)" teeWithMin
            , benchIOSink value "distribute [sum, length]" distribute
            , benchIOSink value "partition (sum, length)" partition
            , benchIOSink value "partitionByFstM (sum, length)" partitionByFstM
            , benchIOSink value "partitionByMinM (sum, length)" partitionByMinM
            , benchIOSink value "unzip (sum, length)" unzip
            , benchIOSink value "unzipWithFstM (sum, length)" unzipWithFstM
            , benchIOSink value "unzipWithMinM (sum, length)" unzipWithMinM
            ]
      ]

o_n_space_serial :: Int -> [Benchmark]
o_n_space_serial value =
    [ benchIOSink value "sequence_/100" $ Stream.fold (sequence_ (value `div` 100))
    ]

o_n_heap_serial :: Int -> [Benchmark]
o_n_heap_serial value =
    [ bgroup "elimination"
      -- Left folds for building a structure are inherently non-streaming
      -- as the structure cannot be lazily consumed until fully built.
            [
              benchIOSink value "toList" (Stream.fold FL.toList)
            , benchIOSink value "toListRev" (Stream.fold FL.toListRev)
            , benchIOSink value "toStream"
                (Stream.fold FL.toStream
                    :: Stream IO a -> IO (Stream Identity a))
            , benchIOSink value "toStreamRev"
                (Stream.fold FL.toStreamRev
                    :: Stream IO a -> IO (Stream Identity a))
            , benchIOSink value "nub" (Stream.fold FL.nub)
            ]
    , bgroup "key-value"
            [
              benchIOSink value "demuxToMap (64 buckets) [sum, length]"
                $ demuxToMap (getKey 64) getFold
            , benchIOSink value "demuxToIntMap (64 buckets) [sum, length]"
                $ demuxToIntMap (getKey 64) getFold
            , benchIOSink value "demuxToMapIO (64 buckets) [sum, length]"
                $ demuxToMapIO (getKey 64) getFold

            -- classify: immutable
            , benchIOSink value "toMap (64 buckets) sum"
                $ toMap (getKey 64)
            , benchIOSink value "toIntMap (64 buckets) sum"
                $ toIntMap (getKey 64)

            -- classify: mutable cells
            , benchIOSink value "toMapIO (single bucket) sum"
                $ toMapIO (getKey 1)
            , benchIOSink value "toMapIO (64 buckets) sum"
                $ toMapIO (getKey 64)
            , benchIOSink value "toMapIO (max buckets) sum"
                $ toMapIO (getKey value)
            , benchIOSink value "toIntMapIO (64 buckets) sum"
                $ toIntMapIO (getKey 64)
            ]
    ]

    where

    getKey buckets = (`mod` buckets)

    getFold k =
        return $ Just $ case k of
            0 -> FL.sum
            1 -> FL.length
            _ -> FL.length

-------------------------------------------------------------------------------
-- Driver
-------------------------------------------------------------------------------

main :: IO ()
main = do
#ifndef FUSION_CHECK
    env <- mkHandleBenchEnv
    runWithCLIOpts defaultStreamSize (allBenchmarks env)

    where

    allBenchmarks env value =
        [ bgroup (o_1_space_prefix moduleName) $ concat
            [ o_1_space_serial_elimination value
            , o_1_space_serial_transformation value
            , o_1_space_serial_composition value
            , o_1_space_reduce_read_split env
            , o_1_space_reduce_toChunks_split env
            ]
        , bgroup (o_n_space_prefix moduleName) (o_n_space_serial value)
        , bgroup (o_n_heap_prefix moduleName) (o_n_heap_serial value)
        ]
#else
    -- Enable FUSION_CHECK macro at the beginning of the file
    -- Enable one benchmark below, and run the benchmark
    -- Check the .dump-simpl output
    let value = 100000
    let input = source value 1
    let getKey buckets = (`mod` buckets)
    let getFold k =
            return $ case k of
                0 -> FL.sum
                1 -> FL.length
                _ -> FL.length

    -- demuxToMap (getKey 64) (getFold . getKey 64) input
    toIntMapIO (getKey 64) input
    return ()
#endif
