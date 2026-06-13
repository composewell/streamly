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
import qualified Streamly.Internal.Data.Stream as Stream
import qualified Streamly.Internal.Data.Unfold as Unfold
import qualified Streamly.Internal.FileSystem.Handle as Handle
import qualified Streamly.Internal.Unicode.Stream as Unicode

import Test.Tasty.Bench hiding (env)
import Streamly.Benchmark.Common
import Streamly.Benchmark.Common.Handle
import Prelude hiding
    ( last, length, all, any, take, unzip, sequence_, filter
    , sum, product, maximum, minimum, mconcat, and, or
    , elem, notElem, lookup, map, foldMap
    )

#ifdef INSPECTION
import GHC.Types (SPEC(..))
import Streamly.Internal.Data.Stream (Step(..))

import qualified Streamly.Internal.Data.MutArray as MutArray

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

{-# INLINE withStream #-}
withStream :: Int -> (Stream IO Int -> IO b) -> IO b
withStream n f = randomRIO (1,1) >>= f . sourceUnfoldrM n

{-# INLINE benchIO #-}
benchIO :: NFData b => String -> IO b -> Benchmark
benchIO name = bench name . nfIO

-------------------------------------------------------------------------------
-- Elimination
-------------------------------------------------------------------------------

{-# INLINE drain #-}
drain :: Int -> IO ()
drain n = withStream n $ Stream.fold FL.drain

#ifdef INSPECTION
inspect $ 'drain `hasNoType` ''Step
inspect $ 'drain `hasNoType` ''FL.Step
inspect $ 'drain `hasNoType` ''SPEC
#endif

{-# INLINE drainBy #-}
drainBy :: Int -> IO ()
drainBy n = withStream n $ Stream.fold (FL.drainMapM return)

#ifdef INSPECTION
inspect $ 'drainBy `hasNoType` ''Step
inspect $ 'drainBy `hasNoType` ''FL.Step
inspect $ 'drainBy `hasNoType` ''SPEC
#endif

{-# INLINE drainN #-}
drainN :: Int -> IO ()
drainN n = withStream n $ Stream.fold (FL.drainN n)

#ifdef INSPECTION
inspect $ 'drainN `hasNoType` ''Step
inspect $ 'drainN `hasNoType` ''FL.Step
inspect $ 'drainN `hasNoType` ''SPEC
#endif

{-# INLINE last #-}
last :: Int -> IO (Maybe Int)
last n = withStream n $ Stream.fold FL.latest

#ifdef INSPECTION
inspect $ 'last `hasNoType` ''Step
inspect $ 'last `hasNoType` ''FL.Step
inspect $ 'last `hasNoType` ''SPEC
#endif

{-# INLINE length #-}
length :: Int -> IO Int
length n = withStream n $ Stream.fold FL.length

#ifdef INSPECTION
inspect $ 'length `hasNoType` ''Step
inspect $ 'length `hasNoType` ''FL.Step
inspect $ 'length `hasNoType` ''SPEC
#endif

{-# INLINE top #-}
top :: Int -> IO (MutArray Int)
top n = withStream n $ Stream.fold (FL.top 10)

{-# INLINE bottom #-}
bottom :: Int -> IO (MutArray Int)
bottom n = withStream n $ Stream.fold (FL.bottom 10)

{-# INLINE sum #-}
sum :: Int -> IO Int
sum n = withStream n $ Stream.fold FL.sum

#ifdef INSPECTION
inspect $ 'sum `hasNoType` ''Step
inspect $ 'sum `hasNoType` ''FL.Step
inspect $ 'sum `hasNoType` ''SPEC
#endif

{-# INLINE foldMapSum #-}
foldMapSum :: Int -> IO (Sum Int)
foldMapSum n = withStream n $ Stream.fold (FL.foldMap Sum)

#ifdef INSPECTION
inspect $ 'foldMapSum `hasNoType` ''Step
inspect $ 'foldMapSum `hasNoType` ''FL.Step
inspect $ 'foldMapSum `hasNoType` ''SPEC
#endif

{-# INLINE product #-}
product :: Int -> IO Int
product n = withStream n $ Stream.fold FL.product

#ifdef INSPECTION
inspect $ 'product `hasNoType` ''Step
inspect $ 'product `hasNoType` ''FL.Step
inspect $ 'product `hasNoType` ''SPEC
#endif

{-# INLINE maximumBy #-}
maximumBy :: Int -> IO (Maybe Int)
maximumBy n = withStream n $ Stream.fold (FL.maximumBy compare)

#ifdef INSPECTION
inspect $ 'maximumBy `hasNoType` ''Step
inspect $ 'maximumBy `hasNoType` ''FL.Step
inspect $ 'maximumBy `hasNoType` ''SPEC
#endif

{-# INLINE maximum #-}
maximum :: Int -> IO (Maybe Int)
maximum n = withStream n $ Stream.fold FL.maximum

#ifdef INSPECTION
inspect $ 'maximum `hasNoType` ''Step
inspect $ 'maximum `hasNoType` ''FL.Step
inspect $ 'maximum `hasNoType` ''SPEC
#endif

{-# INLINE minimumBy #-}
minimumBy :: Int -> IO (Maybe Int)
minimumBy n = withStream n $ Stream.fold (FL.minimumBy compare)

#ifdef INSPECTION
inspect $ 'minimumBy `hasNoType` ''Step
inspect $ 'minimumBy `hasNoType` ''FL.Step
inspect $ 'minimumBy `hasNoType` ''SPEC
#endif

{-# INLINE minimum #-}
minimum :: Int -> IO (Maybe Int)
minimum n = withStream n $ Stream.fold FL.minimum

#ifdef INSPECTION
inspect $ 'minimum `hasNoType` ''Step
inspect $ 'minimum `hasNoType` ''FL.Step
inspect $ 'minimum `hasNoType` ''SPEC
#endif

{-# INLINE mean #-}
mean :: Int -> IO Double
mean n = withStream n $ Stream.fold FL.mean . fmap (fromIntegral :: Int -> Double)

#ifdef INSPECTION
inspect $ 'mean `hasNoType` ''Step
inspect $ 'mean `hasNoType` ''FL.Step
inspect $ 'mean `hasNoType` ''SPEC
#endif

{-# INLINE mconcat #-}
mconcat :: Int -> IO (Last Int)
mconcat n = withStream n $ Stream.fold FL.mconcat . fmap (Last . Just)

#ifdef INSPECTION
inspect $ 'mconcat `hasNoType` ''Step
inspect $ 'mconcat `hasNoType` ''FL.Step
inspect $ 'mconcat `hasNoType` ''SPEC
#endif

{-# INLINE foldMap #-}
foldMap :: Int -> IO (Last Int)
foldMap n = withStream n $ Stream.fold (FL.foldMap (Last . Just))

#ifdef INSPECTION
inspect $ 'foldMap `hasNoType` ''Step
inspect $ 'foldMap `hasNoType` ''FL.Step
inspect $ 'foldMap `hasNoType` ''SPEC
#endif

{-# INLINE foldMapM #-}
foldMapM :: Int -> IO (Last Int)
foldMapM n = withStream n $ Stream.fold (FL.foldMapM (return . Last . Just))

#ifdef INSPECTION
inspect $ 'foldMapM `hasNoType` ''Step
inspect $ 'foldMapM `hasNoType` ''FL.Step
inspect $ 'foldMapM `hasNoType` ''SPEC
#endif

{-# INLINE index #-}
index :: Int -> IO (Maybe Int)
index n = withStream n $ Stream.fold (FL.index (n + 1))

#ifdef INSPECTION
inspect $ 'index `hasNoType` ''Step
inspect $ 'index `hasNoType` ''FL.Step
inspect $ 'index `hasNoType` ''SPEC
#endif

{-# INLINE find #-}
find :: Int -> IO (Maybe Int)
find n = withStream n $ Stream.fold (FL.find (== (n + 1)))

#ifdef INSPECTION
inspect $ 'find `hasNoType` ''Step
inspect $ 'find `hasNoType` ''FL.Step
inspect $ 'find `hasNoType` ''SPEC
#endif

{-# INLINE lookup #-}
lookup :: Int -> IO (Maybe Int)
lookup n = withStream n $ Stream.fold (FL.lmap (\a -> (a, a)) (FL.lookup (n + 1)))

#ifdef INSPECTION
inspect $ 'lookup `hasNoType` ''Step
inspect $ 'lookup `hasNoType` ''FL.Step
inspect $ 'lookup `hasNoType` ''SPEC
#endif

{-# INLINE findIndex #-}
findIndex :: Int -> IO (Maybe Int)
findIndex n = withStream n $ Stream.fold (FL.findIndex (== (n + 1)))

#ifdef INSPECTION
inspect $ 'findIndex `hasNoType` ''Step
inspect $ 'findIndex `hasNoType` ''FL.Step
inspect $ 'findIndex `hasNoType` ''SPEC
#endif

{-# INLINE elemIndex #-}
elemIndex :: Int -> IO (Maybe Int)
elemIndex n = withStream n $ Stream.fold (FL.elemIndex (n + 1))

#ifdef INSPECTION
inspect $ 'elemIndex `hasNoType` ''Step
inspect $ 'elemIndex `hasNoType` ''FL.Step
inspect $ 'elemIndex `hasNoType` ''SPEC
#endif

{-# INLINE elem #-}
elem :: Int -> IO Bool
elem n = withStream n $ Stream.fold (FL.elem (n + 1))

#ifdef INSPECTION
inspect $ 'elem `hasNoType` ''Step
inspect $ 'elem `hasNoType` ''FL.Step
inspect $ 'elem `hasNoType` ''SPEC
#endif

{-# INLINE notElem #-}
notElem :: Int -> IO Bool
notElem n = withStream n $ Stream.fold (FL.notElem (n + 1))

#ifdef INSPECTION
inspect $ 'notElem `hasNoType` ''Step
inspect $ 'notElem `hasNoType` ''FL.Step
inspect $ 'notElem `hasNoType` ''SPEC
#endif

{-# INLINE all #-}
all :: Int -> IO Bool
all n = withStream n $ Stream.fold (FL.all (<= n))

#ifdef INSPECTION
inspect $ 'all `hasNoType` ''Step
inspect $ 'all `hasNoType` ''FL.Step
inspect $ 'all `hasNoType` ''SPEC
#endif

{-# INLINE any #-}
any :: Int -> IO Bool
any n = withStream n $ Stream.fold (FL.any (> n))

#ifdef INSPECTION
inspect $ 'any `hasNoType` ''Step
inspect $ 'any `hasNoType` ''FL.Step
inspect $ 'any `hasNoType` ''SPEC
#endif

{-# INLINE take #-}
take :: Int -> IO ()
take n = withStream n $ Stream.fold (FL.take n FL.drain)

#ifdef INSPECTION
inspect $ 'take `hasNoType` ''Step
inspect $ 'take `hasNoType` ''FL.Step
inspect $ 'take `hasNoType` ''SPEC
inspect $ 'take `hasNoType` ''FL.Tuple'Fused
#endif

{-# INLINE and #-}
and :: Int -> IO Bool
and n = withStream n $ Stream.fold FL.and . fmap (<= (n + 1))

#ifdef INSPECTION
inspect $ 'and `hasNoType` ''Step
inspect $ 'and `hasNoType` ''FL.Step
inspect $ 'and `hasNoType` ''SPEC
#endif

{-# INLINE or #-}
or :: Int -> IO Bool
or n = withStream n $ Stream.fold FL.or . fmap (> (n + 1))

#ifdef INSPECTION
inspect $ 'or `hasNoType` ''Step
inspect $ 'or `hasNoType` ''FL.Step
inspect $ 'or `hasNoType` ''SPEC
#endif

-------------------------------------------------------------------------------
-- Filter
-------------------------------------------------------------------------------

{-# INLINE filter #-}
filter :: Int -> IO ()
filter n = withStream n $ Stream.fold (FL.filter even FL.drain)

#ifdef INSPECTION
inspect $ 'filter `hasNoType` ''Step
inspect $ 'filter `hasNoType` ''FL.Step
inspect $ 'filter `hasNoType` ''SPEC
#endif

{-# INLINE scanMaybe #-}
scanMaybe :: Int -> IO ()
scanMaybe n = withStream n $ Stream.fold (FL.postscanlMaybe (Scanl.filtering even) FL.drain)

#ifdef INSPECTION
inspect $ 'scanMaybe `hasNoType` ''Step
inspect $ 'scanMaybe `hasNoType` ''FL.Step
inspect $ 'scanMaybe `hasNoType` ''SPEC
#endif

{-# INLINE scanMaybe2 #-}
scanMaybe2 :: Int -> IO ()
scanMaybe2 n = withStream n $
    Stream.fold
        $ FL.postscanlMaybe (Scanl.filtering even)
        $ FL.postscanlMaybe (Scanl.filtering odd) FL.drain

#ifdef INSPECTION
inspect $ 'scanMaybe2 `hasNoType` ''Step
inspect $ 'scanMaybe2 `hasNoType` ''FL.Step
inspect $ 'scanMaybe2 `hasNoType` ''SPEC
#endif

{-# INLINE sequence_ #-}
sequence_ :: Monad m => Int -> Fold m a ()
sequence_ value =
    foldr f (FL.fromPure ()) (Prelude.replicate value (FL.take 1 FL.drain))

    where

    {-# INLINE f #-}
    f m k = FL.concatMap (const k) m

-------------------------------------------------------------------------------
-- Splitting in two
-------------------------------------------------------------------------------

{-# INLINE splitAllAny #-}
splitAllAny :: Int -> IO (Bool, Bool)
splitAllAny n = withStream n $
    Stream.fold
        (FL.splitWith (,)
            (FL.all (<= (n `div` 2)))
            (FL.any (> n))
        )

#ifdef INSPECTION
inspect $ 'splitAllAny `hasNoType` ''Step
inspect $ 'splitAllAny `hasNoType` ''FL.Step
inspect $ 'splitAllAny `hasNoType` ''SPEC
inspect $ 'splitAllAny `hasNoType` ''FL.SeqFoldState
#endif

{-# INLINE split_ #-}
split_ :: Int -> IO Bool
split_ n = withStream n $
    Stream.fold
        (FL.split_
            (FL.all (<= (n `div` 2)))
            (FL.any (> n))
        )

#ifdef INSPECTION
inspect $ 'split_ `hasNoType` ''Step
inspect $ 'split_ `hasNoType` ''FL.Step
inspect $ 'split_ `hasNoType` ''SPEC
inspect $ 'split_ `hasNoType` ''FL.SeqFoldState_
#endif

{-# INLINE shortest #-}
shortest :: Int -> IO (Either Int Int)
shortest n = withStream n $ Stream.fold (FL.shortest FL.sum FL.length)

#ifdef INSPECTION
-- shortest uses Tuple' (no Fuse annotation), so only check the basics
inspect $ 'shortest `hasNoType` ''Step
inspect $ 'shortest `hasNoType` ''FL.Step
inspect $ 'shortest `hasNoType` ''SPEC
#endif

{-# INLINE longest #-}
longest :: Int -> IO (Either Int Int)
longest n = withStream n $ Stream.fold (FL.longest FL.sum FL.length)

#ifdef INSPECTION
-- longest has LongestState with Fuse annotation
inspect $ 'longest `hasNoType` ''Step
inspect $ 'longest `hasNoType` ''FL.Step
inspect $ 'longest `hasNoType` ''SPEC
inspect $ 'longest `hasNoType` ''FL.LongestState
#endif

{-# INLINE foldBreak #-}
foldBreak :: Int -> IO ()
foldBreak n = withStream n go
    where
    go s = do
        (r, s1) <- Stream.foldBreak (FL.take 1 FL.length) s
        when (r /= 0) $ go s1

-------------------------------------------------------------------------------
-- Split generated streams (not a file)
-------------------------------------------------------------------------------

{-# INLINE many #-}
many :: Int -> IO ()
many n = withStream n $ Stream.fold (FL.many (FL.take 1 FL.drain) FL.drain)

#ifdef INSPECTION
inspect $ 'many `hasNoType` ''Step
inspect $ 'many `hasNoType` ''FL.Step
inspect $ 'many `hasNoType` ''SPEC
inspect $ 'many `hasNoType` ''FL.ManyState
#endif

{-# INLINE takeEndBy_ #-}
takeEndBy_ :: Int -> IO ()
takeEndBy_ n = withStream n $ Stream.fold (FL.takeEndBy_ (>= n) FL.drain)

#ifdef INSPECTION
inspect $ 'takeEndBy_ `hasNoType` ''Step
inspect $ 'takeEndBy_ `hasNoType` ''FL.Step
inspect $ 'takeEndBy_ `hasNoType` ''SPEC
#endif

-------------------------------------------------------------------------------
-- Splitting a file stream into a stream by serial application
-------------------------------------------------------------------------------

lf :: Word8
lf = fromIntegral (ord '\n')

toarr :: String -> Array Word8
toarr = Array.fromList . fmap (fromIntegral . ord)

-- | Split on line feed.
fileInfixTakeEndBy_ :: Handle -> IO Int
fileInfixTakeEndBy_ inh =
    Stream.fold Fold.length
        $ Stream.foldManyPost (FL.takeEndBy_ (== lf) Fold.drain)
        $ Handle.read inh -- >>= print

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'fileInfixTakeEndBy_
inspect $ 'fileInfixTakeEndBy_ `hasNoType` ''Step
inspect $ 'fileInfixTakeEndBy_ `hasNoType` ''FL.Step
inspect $ 'fileInfixTakeEndBy_ `hasNoType` ''SPEC
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
inspect $ 'fileSuffixTakeEndBy_ `hasNoType` ''FL.Step
inspect $ 'fileSuffixTakeEndBy_ `hasNoType` ''SPEC
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
inspect $ 'fileSuffixTakeEndBy `hasNoType` ''FL.Step
inspect $ 'fileSuffixTakeEndBy `hasNoType` ''SPEC
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

o_1_space_reduce_read_split :: BenchEnv -> [(SpaceComplexity, Benchmark)]
o_1_space_reduce_read_split env =
    -- NOTE: keep the benchmark names consistent with Data.Stream.split*
    fmap (SpaceO_1,)
    -- Splitting on single element
    [ mkBench "takeEndBy_ infix (splitOn)" env $ \inh _ ->
        fileInfixTakeEndBy_ inh
    , mkBench "takeEndBy_ suffix (splitOnSuffix)" env $ \inh _ ->
        fileSuffixTakeEndBy_ inh
    , mkBench "takeEndBy_ suffix parseMany (splitOnSuffix)" env
        $ \inh _ -> parseFileSuffixTakeEndBy_ inh
    , mkBench "takeEndBy suffix (splitWithSuffix)" env $ \inh _ ->
        fileSuffixTakeEndBy inh

    -- Splitting on sequence
    -- Infix takeEndBySeq_
    , mkBench "takeEndBySeq_ infix empty pattern" env $ \inh _ ->
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

-- | Infix split on a character sequence.
splitOnSeqUtf8 :: String -> Handle -> IO Int
splitOnSeqUtf8 str inh =
    -- XXX requires @-fspec-constr-recursive=12@. Maybe due to
    -- decodeUtf8.
    Stream.fold Fold.length
        $ Stream.foldManyPost (Fold.takeEndBySeq_ (Array.fromList str) Fold.drain)
        $ Unicode.decodeUtf8Chunks
        $ Handle.readChunks inh -- >>= print

o_1_space_reduce_toChunks_split :: BenchEnv -> [(SpaceComplexity, Benchmark)]
o_1_space_reduce_toChunks_split env =
    fmap (SpaceO_1,)
    [ mkBenchSmall "takeEndBySeq_ infix abcdefgh (Utf8)"
        env $ \inh _ -> splitOnSeqUtf8 "abcdefgh" inh
    , mkBenchSmall "takeEndBySeq_ infix abcdefghijklmnopqrstuvwxyz (Utf8)"
        env $ \inh _ -> splitOnSeqUtf8 "abcdefghijklmnopqrstuvwxyz" inh
    ]

-------------------------------------------------------------------------------
-- Distributing by parallel application
-------------------------------------------------------------------------------

{-# INLINE teeSumLength #-}
teeSumLength :: Int -> IO (Int, Int)
teeSumLength n = withStream n $ Stream.fold (FL.teeWith (,) FL.sum FL.length)

#ifdef INSPECTION
inspect $ 'teeSumLength `hasNoType` ''Step
inspect $ 'teeSumLength `hasNoType` ''FL.Step
inspect $ 'teeSumLength `hasNoType` ''SPEC
inspect $ 'teeSumLength `hasNoType` ''FL.TeeState
#endif

{-# INLINE teeAllAny #-}
teeAllAny :: Int -> IO (Bool, Bool)
teeAllAny n = withStream n $ Stream.fold (FL.teeWith (,) (FL.all (<= n)) (FL.any (> n)))

#ifdef INSPECTION
inspect $ 'teeAllAny `hasNoType` ''Step
inspect $ 'teeAllAny `hasNoType` ''FL.Step
inspect $ 'teeAllAny `hasNoType` ''SPEC
inspect $ 'teeAllAny `hasNoType` ''FL.TeeState
#endif

{-# INLINE teeWithFst #-}
teeWithFst :: Int -> IO (Int, Int)
teeWithFst n = withStream n $ Stream.fold (FL.teeWithFst (,) FL.sum FL.length)

#ifdef INSPECTION
inspect $ 'teeWithFst `hasNoType` ''Step
inspect $ 'teeWithFst `hasNoType` ''FL.Step
inspect $ 'teeWithFst `hasNoType` ''SPEC
inspect $ 'teeWithFst `hasNoType` ''FL.TeeFstState
#endif

{-# INLINE teeWithMin #-}
teeWithMin :: Int -> IO (Int, Int)
teeWithMin n = withStream n $ Stream.fold (FL.teeWithMin (,) FL.sum FL.length)

#ifdef INSPECTION
-- teeWithMin uses Tuple' (no Fuse annotation), so only check the basics
inspect $ 'teeWithMin `hasNoType` ''Step
inspect $ 'teeWithMin `hasNoType` ''FL.Step
inspect $ 'teeWithMin `hasNoType` ''SPEC
#endif

{-# INLINE distribute #-}
distribute :: Int -> IO [Int]
distribute n = withStream n $ Stream.fold (FL.distribute [FL.sum, FL.length])

-------------------------------------------------------------------------------
-- Partitioning
-------------------------------------------------------------------------------

{-# INLINE oddEven #-}
oddEven :: Int -> Either Int Int
oddEven x = if odd x then Left x else Right x

{-# INLINE partition #-}
partition :: Int -> IO (Int, Int)
partition n = withStream n $ Stream.fold $ FL.lmap oddEven (FL.partition FL.sum FL.length)

#ifdef INSPECTION
inspect $ 'partition `hasNoType` ''Step
inspect $ 'partition `hasNoType` ''FL.Step
inspect $ 'partition `hasNoType` ''SPEC
inspect $ 'partition `hasNoType` ''FL.TeeState
#endif

{-# INLINE partitionByFstM #-}
partitionByFstM :: Int -> IO (Int, Int)
partitionByFstM n = withStream n $
    Stream.fold (FL.partitionByFstM (return . oddEven) FL.sum FL.length)

#ifdef INSPECTION
inspect $ 'partitionByFstM `hasNoType` ''Step
inspect $ 'partitionByFstM `hasNoType` ''FL.Step
inspect $ 'partitionByFstM `hasNoType` ''SPEC
inspect $ 'partitionByFstM `hasNoType` ''FL.TeeFstState
#endif

{-# INLINE partitionByMinM #-}
partitionByMinM :: Int -> IO (Int, Int)
partitionByMinM n = withStream n $
    Stream.fold (FL.partitionByMinM (return . oddEven) FL.sum FL.length)

-------------------------------------------------------------------------------
-- Key-value folds (helpers)
-------------------------------------------------------------------------------

{-# INLINE demuxToMap #-}
demuxToMap :: (Monad m, Ord k) =>
    (a -> k) -> (k -> m (Maybe (Fold m a b))) -> Stream m a -> m (Map k b)
demuxToMap f g = Stream.fold (FL.demuxerToContainer f g)

{-# INLINE demuxToIntMap #-}
demuxToIntMap :: Monad m =>
    (a -> Int) -> (Int -> m (Maybe (Fold m a b))) -> Stream m a -> m (IntMap b)
demuxToIntMap f g = Stream.fold (FL.demuxerToContainer f g)

{-# INLINE demuxToMapIO #-}
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
-- Unzip
-------------------------------------------------------------------------------

{-# INLINE unzip #-}
unzip :: Int -> IO (Int, Int)
unzip n = withStream n $ Stream.fold $ FL.lmap (\a -> (a, a)) (FL.unzip FL.sum FL.length)

#ifdef INSPECTION
inspect $ 'unzip `hasNoType` ''Step
inspect $ 'unzip `hasNoType` ''FL.Step
inspect $ 'unzip `hasNoType` ''SPEC
inspect $ 'unzip `hasNoType` ''FL.TeeState
#endif

{-# INLINE unzipWithFstM #-}
unzipWithFstM :: Int -> IO (Int, Int)
unzipWithFstM n = withStream n $ Stream.fold (FL.unzipWithFstM f FL.sum FL.length)
    where f a = return (a + 1, a)

#ifdef INSPECTION
inspect $ 'unzipWithFstM `hasNoType` ''Step
inspect $ 'unzipWithFstM `hasNoType` ''FL.Step
inspect $ 'unzipWithFstM `hasNoType` ''SPEC
inspect $ 'unzipWithFstM `hasNoType` ''FL.TeeFstState
#endif

{-# INLINE unzipWithMinM #-}
unzipWithMinM :: Int -> IO (Int, Int)
unzipWithMinM n = withStream n $ Stream.fold (FL.unzipWithMinM f FL.sum FL.length)
    where f a = return (a + 1, a)

-------------------------------------------------------------------------------
-- Nested
-------------------------------------------------------------------------------

{-# INLINE unfoldMany #-}
unfoldMany :: Int -> IO ()
unfoldMany n =
    Stream.fold (FL.unfoldMany Unfold.replicateM FL.drain)
        $ Stream.fromPure (n, randomRIO (1, 1 :: Int))

-------------------------------------------------------------------------------
-- Transformation
-------------------------------------------------------------------------------

{-# INLINE map #-}
map :: Int -> IO ()
map n = withStream n $ Stream.fold (FL.lmap (+ 1) FL.drain)

#ifdef INSPECTION
inspect $ 'map `hasNoType` ''Step
inspect $ 'map `hasNoType` ''FL.Step
inspect $ 'map `hasNoType` ''SPEC
#endif

{-# INLINE mapMaybe #-}
mapMaybe :: Int -> IO ()
mapMaybe n = withStream n $
    Stream.fold (FL.mapMaybe (\x -> if even x then Just x else Nothing) FL.drain)

#ifdef INSPECTION
inspect $ 'mapMaybe `hasNoType` ''Step
inspect $ 'mapMaybe `hasNoType` ''FL.Step
inspect $ 'mapMaybe `hasNoType` ''SPEC
#endif

{-# INLINE rsequence #-}
rsequence :: Int -> IO ()
rsequence n = withStream n $ Stream.fold (FL.rmapM id (return <$> FL.drain))

{-# INLINE rmapM #-}
rmapM :: Int -> IO ()
rmapM n = withStream n $ Stream.fold (FL.rmapM return FL.drain)

#ifdef INSPECTION
inspect $ 'rmapM `hasNoType` ''Step
inspect $ 'rmapM `hasNoType` ''FL.Step
inspect $ 'rmapM `hasNoType` ''SPEC
#endif

{-# INLINE pipeMapM #-}
pipeMapM :: Int -> IO ()
pipeMapM n = withStream n $
    Stream.fold (FL.pipe (Pipe.mapM (\x -> return $ x + 1)) FL.drain)

#ifdef INSPECTION
inspect $ 'pipeMapM `hasNoType` ''Step
inspect $ 'pipeMapM `hasNoType` ''FL.Step
inspect $ 'pipeMapM `hasNoType` ''SPEC
#endif

{-# INLINE foldScanl #-}
foldScanl :: Int -> IO ()
foldScanl n = withStream n $ Stream.fold $ FL.scanl Scanl.sum FL.drain

#ifdef INSPECTION
inspect $ 'foldScanl `hasNoType` ''Step
inspect $ 'foldScanl `hasNoType` ''FL.Step
inspect $ 'foldScanl `hasNoType` ''SPEC
#endif

{-# INLINE foldScanlMany #-}
foldScanlMany :: Int -> IO ()
foldScanlMany n = withStream n $ Stream.fold $ FL.scanlMany (Scanl.take 2 Scanl.drain) FL.drain

#ifdef INSPECTION
inspect $ 'foldScanlMany `hasNoType` ''Step
-- inspect $ 'foldScanlMany `hasNoType` ''FL.Step
inspect $ 'foldScanlMany `hasNoType` ''SPEC
#endif

{-# INLINE foldPostscanl #-}
foldPostscanl :: Int -> IO ()
foldPostscanl n = withStream n $ Stream.fold $ FL.postscanl Scanl.sum FL.drain

#ifdef INSPECTION
inspect $ 'foldPostscanl `hasNoType` ''Step
inspect $ 'foldPostscanl `hasNoType` ''FL.Step
inspect $ 'foldPostscanl `hasNoType` ''SPEC
#endif

-------------------------------------------------------------------------------
-- O(n)-heap: elimination (building structures)
-------------------------------------------------------------------------------

{-# INLINE toList #-}
toList :: Int -> IO [Int]
toList n = withStream n $ Stream.fold FL.toList

{-# INLINE toListRev #-}
toListRev :: Int -> IO [Int]
toListRev n = withStream n $ Stream.fold FL.toListRev

{-# INLINE toStream #-}
toStream :: Int -> IO (Stream Identity Int)
toStream n = withStream n $ Stream.fold FL.toStream

{-# INLINE toStreamRev #-}
toStreamRev :: Int -> IO (Stream Identity Int)
toStreamRev n = withStream n $ Stream.fold FL.toStreamRev

{-# INLINE nub #-}
nub :: Int -> IO (Maybe Int)
nub n = withStream n $ Stream.fold FL.nub

-------------------------------------------------------------------------------
-- O(n)-heap: key-value (benchmark wrappers)
-------------------------------------------------------------------------------

{-# INLINE getKey #-}
getKey :: Int -> Int -> Int
getKey buckets x = x `mod` buckets

{-# INLINE getFold #-}
getFold :: Int -> IO (Maybe (Fold IO Int Int))
getFold k = return $ Just $ case k of
    0 -> FL.sum
    1 -> FL.length
    _ -> FL.length

{-# INLINE demuxToMap64 #-}
demuxToMap64 :: Int -> IO (Map Int Int)
demuxToMap64 n = withStream n $ demuxToMap (getKey 64) getFold

{-# INLINE demuxToIntMap64 #-}
demuxToIntMap64 :: Int -> IO (IntMap Int)
demuxToIntMap64 n = withStream n $ demuxToIntMap (getKey 64) getFold

{-# INLINE demuxToMapIO64 #-}
demuxToMapIO64 :: Int -> IO (Map Int Int)
demuxToMapIO64 n = withStream n $ demuxToMapIO (getKey 64) getFold

{-# INLINE toMap64 #-}
toMap64 :: Int -> IO (Map Int Int)
toMap64 n = withStream n $ toMap (getKey 64)

{-# INLINE toIntMap64 #-}
toIntMap64 :: Int -> IO (IntMap Int)
toIntMap64 n = withStream n $ toIntMap (getKey 64)

{-# INLINE toMapIO1 #-}
toMapIO1 :: Int -> IO (Map Int Int)
toMapIO1 n = withStream n $ toMapIO (getKey 1)

{-# INLINE toMapIO64 #-}
toMapIO64 :: Int -> IO (Map Int Int)
toMapIO64 n = withStream n $ toMapIO (getKey 64)

{-# INLINE toMapIOMax #-}
toMapIOMax :: Int -> IO (Map Int Int)
toMapIOMax n = withStream n $ toMapIO (getKey n)

{-# INLINE toIntMapIO64 #-}
toIntMapIO64 :: Int -> IO (IntMap Int)
toIntMapIO64 n = withStream n $ toIntMapIO (getKey 64)

-------------------------------------------------------------------------------
-- N-space
-------------------------------------------------------------------------------

{-# INLINE sequenceFolds #-}
sequenceFolds :: Int -> IO ()
sequenceFolds n = withStream n $ Stream.fold (sequence_ n)

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

o_1_space_serial_elimination :: Int -> [(SpaceComplexity, Benchmark)]
o_1_space_serial_elimination value =
    fmap (SpaceO_1,)
    [ benchIO "drain" $ drain value
    , benchIO "drainBy" $ drainBy value
    , benchIO "drainN" $ drainN value
    , benchIO "last" $ last value
    , benchIO "length" $ length value
    , benchIO "top" $ top value
    , benchIO "bottom" $ bottom value
    , benchIO "sum" $ sum value
    , benchIO "sum (foldMap)" $ foldMapSum value
    , benchIO "product" $ product value
    , benchIO "maximumBy" $ maximumBy value
    , benchIO "maximum" $ maximum value
    , benchIO "minimumBy" $ minimumBy value
    , benchIO "minimum" $ minimum value
    , benchIO "mean" $ mean value
{-
    -- These are already benchmarked in streamly-statistics package. If we
    -- still want to keep these tests here, perhaps we should move them to a
    -- different module so we can remove -fno-warn-warnings-deprecations.

    , benchIO "variance" $ variance value
    , benchIO "stdDev" $ stdDev value
-}
    , benchIO "mconcat" $ mconcat value
    , benchIO "foldMap" $ foldMap value
    , benchIO "foldMapM" $ foldMapM value
    , benchIO "index" $ index value
    -- , benchIO "head" $ head value
    , benchIO "find" $ find value
    , benchIO "lookup" $ lookup value
    , benchIO "findIndex" $ findIndex value
    , benchIO "elemIndex" $ elemIndex value
    -- , benchIO "null" $ null value
    , benchIO "elem" $ elem value
    , benchIO "notElem" $ notElem value
    , benchIO "all" $ all value
    , benchIO "any" $ any value
    , benchIO "take" $ take value
    , benchIO "takeEndBy_" $ takeEndBy_ value
    , benchIO "and" $ and value
    , benchIO "or" $ or value
    ]

o_1_space_serial_transformation :: Int -> [(SpaceComplexity, Benchmark)]
o_1_space_serial_transformation value =
    fmap (SpaceO_1,)
    [ benchIO "map" $ map value
    , benchIO "mapMaybe" $ mapMaybe value
    , benchIO "rsequence" $ rsequence value
    , benchIO "rmapM" $ rmapM value
    , benchIO "pipe-mapM" $ pipeMapM value
{-
    , benchIO "fold-runScan" $ foldRunScan value
-}
    , benchIO "fold-scan" $ foldScanl value
    , benchIO "fold-scanMany" $ foldScanlMany value
    , benchIO "fold-postscan" $ foldPostscanl value
    ]

o_1_space_serial_composition :: Int -> [(SpaceComplexity, Benchmark)]
o_1_space_serial_composition value =
    fmap (SpaceO_1,)
    [ benchIO "filter even" $ filter value
    , benchIO "scanMaybe even" $ scanMaybe value
    , benchIO "scanMaybe even, odd" $ scanMaybe2 value
    , benchIO "foldBreak (recursive)" $ foldBreak value
    , benchIO "splitWith (all, any)" $ splitAllAny value
    , benchIO "split_ (all, any)" $ split_ value
    , benchIO "tee (all, any)" $ teeAllAny value
    , benchIO "many drain (take 1)" $ many value
    , benchIO "unfoldMany" $ unfoldMany value
    , benchIO "shortest (sum, length)" $ shortest value
    , benchIO "longest (sum, length)" $ longest value
    , benchIO "tee (sum, length)" $ teeSumLength value
    , benchIO "teeWithFst (sum, length)" $ teeWithFst value
    , benchIO "teeWithMin (sum, length)" $ teeWithMin value
    , benchIO "distribute [sum, length]" $ distribute value
    , benchIO "partition (sum, length)" $ partition value
    , benchIO "partitionByFstM (sum, length)" $ partitionByFstM value
    , benchIO "partitionByMinM (sum, length)" $ partitionByMinM value
    , benchIO "unzip (sum, length)" $ unzip value
    , benchIO "unzipWithFstM (sum, length)" $ unzipWithFstM value
    , benchIO "unzipWithMinM (sum, length)" $ unzipWithMinM value
    ]

o_n_space_serial :: Int -> [(SpaceComplexity, Benchmark)]
o_n_space_serial value =
    [ (SpaceO_n, benchIO "sequence_/100" $ sequenceFolds (value `div` 100))
    ]

o_n_heap_serial :: Int -> [(SpaceComplexity, Benchmark)]
o_n_heap_serial value =
    fmap (HeapO_n,)
    -- Left folds for building a structure are inherently non-streaming
    -- as the structure cannot be lazily consumed until fully built.
    [ benchIO "toList" $ toList value
    , benchIO "toListRev" $ toListRev value
    , benchIO "toStream" $ toStream value
    , benchIO "toStreamRev" $ toStreamRev value
    , benchIO "nub" $ nub value
    , benchIO "demuxToMap (64 buckets) [sum, length]" $ demuxToMap64 value
    , benchIO "demuxToIntMap (64 buckets) [sum, length]" $ demuxToIntMap64 value
    , benchIO "demuxToMapIO (64 buckets) [sum, length]" $ demuxToMapIO64 value
    -- classify: immutable
    , benchIO "toMap (64 buckets) sum" $ toMap64 value
    , benchIO "toIntMap (64 buckets) sum" $ toIntMap64 value
    -- classify: mutable cells
    , benchIO "toMapIO (single bucket) sum" $ toMapIO1 value
    , benchIO "toMapIO (64 buckets) sum" $ toMapIO64 value
    , benchIO "toMapIO (max buckets) sum" $ toMapIOMax value
    , benchIO "toIntMapIO (64 buckets) sum" $ toIntMapIO64 value
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

    allBenchmarks env value =
        let allBenches =
                   o_1_space_serial_elimination value
                ++ o_1_space_serial_transformation value
                ++ o_1_space_serial_composition value
                ++ o_1_space_reduce_read_split env
                ++ o_1_space_reduce_toChunks_split env
                ++ o_n_space_serial value
                ++ o_n_heap_serial value
            get x = [b | (c, b) <- allBenches, c == x]
            o_1_space = get SpaceO_1
            o_n_heap = get HeapO_n
            o_n_space = get SpaceO_n
        in
        [ bgroup (o_1_space_prefix moduleName) o_1_space
        , bgroup (o_n_space_prefix moduleName) o_n_space
        , bgroup (o_n_heap_prefix moduleName) o_n_heap
        ]
#else
    -- Enable FUSION_CHECK macro at the beginning of the file
    -- Enable one benchmark below, and run the benchmark
    -- Check the .dump-simpl output
    let value = 100000
    let input = sourceUnfoldrM value 1
    let getKey' buckets = (`mod` buckets)
    let getFold' k =
            return $ case k of
                0 -> FL.sum
                1 -> FL.length
                _ -> FL.length

    -- demuxToMap (getKey' 64) (getFold' . getKey' 64) input
    toIntMapIO (getKey' 64) input
    return ()
#endif
