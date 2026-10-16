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
import Data.Set (Set)
import Data.STRef (STRef)
import Data.Word (Word8, Word32)
import Foreign.Ptr (Ptr)
import GHC.Classes (IP)
import GHC.Stack (CallStack, SrcLoc)
import GHC.Types (SPEC(..))
import System.IO (Handle)
import System.Random (randomRIO)
import Unsafe.Coerce (UnsafeEquality)

import Streamly.Data.MutByteArray (MutByteArray, Unbox)
import Streamly.Internal.Data.MutByteArray (PinnedState)
import Streamly.Internal.Data.Stream (Stream)
import Streamly.Internal.Data.Fold
    (Fold(..), ConcatMapState, SplitOnSeqState, Tuple'Fused)
import Streamly.Internal.Data.MutArray (MutArray)
import Streamly.Internal.Data.Tuple.Strict (Tuple')

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

import Fusion.Plugin.Types
import Test.Tasty.Bench hiding (env)
import Streamly.Benchmark.Common
import Streamly.Benchmark.Common.Handle
import Streamly.Internal.Data.SVar.Type (State)
import Prelude hiding
    ( length, all, any, take, unzip, sequence_, filter
    , sum, product, maximum, minimum, mconcat, and, or
    , elem, notElem, lookup, concatMap, scanl
    )

#ifdef INSPECTION
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
withStream :: Int -> (Stream IO Int -> IO b) -> Int -> IO b
withStream n f = f . sourceUnfoldrM n

{-# INLINE benchIO #-}
benchIO :: NFData b => String -> (Int -> IO b) -> Benchmark
benchIO name f = bench name $ nfIO $ randomRIO (1, 1 :: Int) >>= f

-------------------------------------------------------------------------------
-- Elimination
-------------------------------------------------------------------------------

{-# ANN drain (PermitPatternMatches []) #-}
{-# ANN drain (PermitConstructions [''()]) #-}
{-# ANN drain (PermitTypeClasses []) #-}
{-# NOINLINE drain #-}
drain :: Int -> Int -> IO ()
drain n = withStream n $ Stream.fold FL.drain

#ifdef INSPECTION
inspect $ 'drain `hasNoType` ''Step
inspect $ 'drain `hasNoType` ''FL.Step
inspect $ 'drain `hasNoType` ''SPEC
#endif

{-# ANN drainMapM (PermitPatternMatches []) #-}
{-# ANN drainMapM (PermitConstructions [''()]) #-}
{-# ANN drainMapM (PermitTypeClasses []) #-}
{-# NOINLINE drainMapM #-}
drainMapM :: Int -> Int -> IO ()
drainMapM n = withStream n $ Stream.fold (FL.drainMapM return)

#ifdef INSPECTION
inspect $ 'drainMapM `hasNoType` ''Step
inspect $ 'drainMapM `hasNoType` ''FL.Step
inspect $ 'drainMapM `hasNoType` ''SPEC
#endif

{-# ANN drainN (PermitPatternMatches []) #-}
{-# ANN drainN (PermitConstructions [''()]) #-}
{-# ANN drainN (PermitTypeClasses []) #-}
{-# NOINLINE drainN #-}
drainN :: Int -> Int -> IO ()
drainN n = withStream n $ Stream.fold (FL.drainN n)

#ifdef INSPECTION
inspect $ 'drainN `hasNoType` ''Step
inspect $ 'drainN `hasNoType` ''FL.Step
inspect $ 'drainN `hasNoType` ''SPEC
#endif

{-# ANN latest (PermitPatternMatches []) #-}
{-# ANN latest (PermitConstructions [''Maybe,''Int]) #-}
{-# ANN latest (PermitTypeClasses []) #-}
{-# NOINLINE latest #-}
latest :: Int -> Int -> IO (Maybe Int)
latest n = withStream n $ Stream.fold FL.latest

#ifdef INSPECTION
inspect $ 'latest `hasNoType` ''Step
inspect $ 'latest `hasNoType` ''FL.Step
inspect $ 'latest `hasNoType` ''SPEC
#endif

{-# ANN length (PermitPatternMatches []) #-}
{-# ANN length (PermitConstructions [''Int]) #-}
{-# ANN length (PermitTypeClasses []) #-}
{-# NOINLINE length #-}
length :: Int -> Int -> IO Int
length n = withStream n $ Stream.fold FL.length

#ifdef INSPECTION
inspect $ 'length `hasNoType` ''Step
inspect $ 'length `hasNoType` ''FL.Step
inspect $ 'length `hasNoType` ''SPEC
#endif

{-# ANN top (PermitPatternMatches [''Int,''MutArray]) #-}
{-# ANN top (PermitConstructions [''Int,''MutArray]) #-}
{-# ANN top (PermitTypeClasses [''MonadIO,''Unbox]) #-}
{-# NOINLINE top #-}
top :: Int -> Int -> IO (MutArray Int)
top n = withStream n $ Stream.fold (FL.top 10)

{-# ANN bottom (PermitPatternMatches [''Int,''MutArray]) #-}
{-# ANN bottom (PermitConstructions [''Int,''MutArray]) #-}
{-# ANN bottom (PermitTypeClasses [''MonadIO,''Unbox]) #-}
{-# NOINLINE bottom #-}
bottom :: Int -> Int -> IO (MutArray Int)
bottom n = withStream n $ Stream.fold (FL.bottom 10)

{-# ANN sum (PermitPatternMatches []) #-}
{-# ANN sum (PermitConstructions [''Int]) #-}
{-# ANN sum (PermitTypeClasses []) #-}
{-# NOINLINE sum #-}
sum :: Int -> Int -> IO Int
sum n = withStream n $ Stream.fold FL.sum

#ifdef INSPECTION
inspect $ 'sum `hasNoType` ''Step
inspect $ 'sum `hasNoType` ''FL.Step
inspect $ 'sum `hasNoType` ''SPEC
#endif

{-# ANN foldMap_Sum (PermitPatternMatches []) #-}
{-# ANN foldMap_Sum (PermitConstructions [''Int]) #-}
{-# ANN foldMap_Sum (PermitTypeClasses []) #-}
{-# NOINLINE foldMap_Sum #-}
foldMap_Sum :: Int -> Int -> IO (Sum Int)
foldMap_Sum n = withStream n $ Stream.fold (FL.foldMap Sum)

#ifdef INSPECTION
inspect $ 'foldMap_Sum `hasNoType` ''Step
inspect $ 'foldMap_Sum `hasNoType` ''FL.Step
inspect $ 'foldMap_Sum `hasNoType` ''SPEC
#endif

{-# ANN product (PermitPatternMatches []) #-}
{-# ANN product (PermitConstructions [''Int]) #-}
{-# ANN product (PermitTypeClasses []) #-}
{-# NOINLINE product #-}
product :: Int -> Int -> IO Int
product n = withStream n $ Stream.fold FL.product

#ifdef INSPECTION
inspect $ 'product `hasNoType` ''Step
inspect $ 'product `hasNoType` ''FL.Step
inspect $ 'product `hasNoType` ''SPEC
#endif

{-# ANN maximumBy (PermitPatternMatches []) #-}
{-# ANN maximumBy (PermitConstructions [''Maybe,''Int]) #-}
{-# ANN maximumBy (PermitTypeClasses []) #-}
{-# NOINLINE maximumBy #-}
maximumBy :: Int -> Int -> IO (Maybe Int)
maximumBy n = withStream n $ Stream.fold (FL.maximumBy compare)

#ifdef INSPECTION
inspect $ 'maximumBy `hasNoType` ''Step
inspect $ 'maximumBy `hasNoType` ''FL.Step
inspect $ 'maximumBy `hasNoType` ''SPEC
#endif

{-# ANN maximum (PermitPatternMatches []) #-}
{-# ANN maximum (PermitConstructions [''Maybe,''Int]) #-}
{-# ANN maximum (PermitTypeClasses []) #-}
{-# NOINLINE maximum #-}
maximum :: Int -> Int -> IO (Maybe Int)
maximum n = withStream n $ Stream.fold FL.maximum

#ifdef INSPECTION
inspect $ 'maximum `hasNoType` ''Step
inspect $ 'maximum `hasNoType` ''FL.Step
inspect $ 'maximum `hasNoType` ''SPEC
#endif

{-# ANN minimumBy (PermitPatternMatches []) #-}
{-# ANN minimumBy (PermitConstructions [''Maybe,''Int]) #-}
{-# ANN minimumBy (PermitTypeClasses []) #-}
{-# NOINLINE minimumBy #-}
minimumBy :: Int -> Int -> IO (Maybe Int)
minimumBy n = withStream n $ Stream.fold (FL.minimumBy compare)

#ifdef INSPECTION
inspect $ 'minimumBy `hasNoType` ''Step
inspect $ 'minimumBy `hasNoType` ''FL.Step
inspect $ 'minimumBy `hasNoType` ''SPEC
#endif

{-# ANN minimum (PermitPatternMatches []) #-}
{-# ANN minimum (PermitConstructions [''Maybe,''Int]) #-}
{-# ANN minimum (PermitTypeClasses []) #-}
{-# NOINLINE minimum #-}
minimum :: Int -> Int -> IO (Maybe Int)
minimum n = withStream n $ Stream.fold FL.minimum

#ifdef INSPECTION
inspect $ 'minimum `hasNoType` ''Step
inspect $ 'minimum `hasNoType` ''FL.Step
inspect $ 'minimum `hasNoType` ''SPEC
#endif

{-# ANN mean (PermitPatternMatches []) #-}
{-# ANN mean (PermitConstructions [''Double]) #-}
{-# ANN mean (PermitTypeClasses []) #-}
{-# NOINLINE mean #-}
mean :: Int -> Int -> IO Double
mean n = withStream n $ Stream.fold FL.mean . fmap (fromIntegral :: Int -> Double)

#ifdef INSPECTION
inspect $ 'mean `hasNoType` ''Step
inspect $ 'mean `hasNoType` ''FL.Step
inspect $ 'mean `hasNoType` ''SPEC
#endif

{-# ANN mconcat (PermitPatternMatches []) #-}
{-# ANN mconcat (PermitConstructions [''Maybe,''Int]) #-}
{-# ANN mconcat (PermitTypeClasses []) #-}
{-# NOINLINE mconcat #-}
mconcat :: Int -> Int -> IO (Last Int)
mconcat n = withStream n $ Stream.fold FL.mconcat . fmap (Last . Just)

#ifdef INSPECTION
inspect $ 'mconcat `hasNoType` ''Step
inspect $ 'mconcat `hasNoType` ''FL.Step
inspect $ 'mconcat `hasNoType` ''SPEC
#endif

{-# ANN foldMap_Last (PermitPatternMatches []) #-}
{-# ANN foldMap_Last (PermitConstructions [''Maybe,''Int]) #-}
{-# ANN foldMap_Last (PermitTypeClasses []) #-}
{-# NOINLINE foldMap_Last #-}
foldMap_Last :: Int -> Int -> IO (Last Int)
foldMap_Last n = withStream n $ Stream.fold (FL.foldMap (Last . Just))

#ifdef INSPECTION
inspect $ 'foldMap_Last `hasNoType` ''Step
inspect $ 'foldMap_Last `hasNoType` ''FL.Step
inspect $ 'foldMap_Last `hasNoType` ''SPEC
#endif

{-# ANN foldMapM (PermitPatternMatches []) #-}
{-# ANN foldMapM (PermitConstructions [''Maybe,''Int]) #-}
{-# ANN foldMapM (PermitTypeClasses []) #-}
{-# NOINLINE foldMapM #-}
foldMapM :: Int -> Int -> IO (Last Int)
foldMapM n = withStream n $ Stream.fold (FL.foldMapM (return . Last . Just))

#ifdef INSPECTION
inspect $ 'foldMapM `hasNoType` ''Step
inspect $ 'foldMapM `hasNoType` ''FL.Step
inspect $ 'foldMapM `hasNoType` ''SPEC
#endif

{-# ANN index (PermitPatternMatches []) #-}
{-# ANN index (PermitConstructions [''Maybe,''Int]) #-}
{-# ANN index (PermitTypeClasses []) #-}
{-# NOINLINE index #-}
index :: Int -> Int -> IO (Maybe Int)
index n = withStream n $ Stream.fold (FL.index (n + 1))

#ifdef INSPECTION
inspect $ 'index `hasNoType` ''Step
inspect $ 'index `hasNoType` ''FL.Step
inspect $ 'index `hasNoType` ''SPEC
#endif

{-# ANN find (PermitPatternMatches []) #-}
{-# ANN find (PermitConstructions [''Maybe,''Int]) #-}
{-# ANN find (PermitTypeClasses []) #-}
{-# NOINLINE find #-}
find :: Int -> Int -> IO (Maybe Int)
find n = withStream n $ Stream.fold (FL.find (== (n + 1)))

#ifdef INSPECTION
inspect $ 'find `hasNoType` ''Step
inspect $ 'find `hasNoType` ''FL.Step
inspect $ 'find `hasNoType` ''SPEC
#endif

{-# ANN lookup (PermitPatternMatches []) #-}
{-# ANN lookup (PermitConstructions [''Maybe,''Int]) #-}
{-# ANN lookup (PermitTypeClasses []) #-}
{-# NOINLINE lookup #-}
lookup :: Int -> Int -> IO (Maybe Int)
lookup n = withStream n $ Stream.fold (FL.lmap (\a -> (a, a)) (FL.lookup (n + 1)))

#ifdef INSPECTION
inspect $ 'lookup `hasNoType` ''Step
inspect $ 'lookup `hasNoType` ''FL.Step
inspect $ 'lookup `hasNoType` ''SPEC
#endif

{-# ANN findIndex (PermitPatternMatches []) #-}
{-# ANN findIndex (PermitConstructions [''Maybe,''Int]) #-}
{-# ANN findIndex (PermitTypeClasses []) #-}
{-# NOINLINE findIndex #-}
findIndex :: Int -> Int -> IO (Maybe Int)
findIndex n = withStream n $ Stream.fold (FL.findIndex (== (n + 1)))

#ifdef INSPECTION
inspect $ 'findIndex `hasNoType` ''Step
inspect $ 'findIndex `hasNoType` ''FL.Step
inspect $ 'findIndex `hasNoType` ''SPEC
#endif

{-# ANN elemIndex (PermitPatternMatches []) #-}
{-# ANN elemIndex (PermitConstructions [''Maybe,''Int]) #-}
{-# ANN elemIndex (PermitTypeClasses []) #-}
{-# NOINLINE elemIndex #-}
elemIndex :: Int -> Int -> IO (Maybe Int)
elemIndex n = withStream n $ Stream.fold (FL.elemIndex (n + 1))

#ifdef INSPECTION
inspect $ 'elemIndex `hasNoType` ''Step
inspect $ 'elemIndex `hasNoType` ''FL.Step
inspect $ 'elemIndex `hasNoType` ''SPEC
#endif

{-# ANN elem (PermitPatternMatches []) #-}
{-# ANN elem (PermitConstructions [''Bool]) #-}
{-# ANN elem (PermitTypeClasses []) #-}
{-# NOINLINE elem #-}
elem :: Int -> Int -> IO Bool
elem n = withStream n $ Stream.fold (FL.elem (n + 1))

#ifdef INSPECTION
inspect $ 'elem `hasNoType` ''Step
inspect $ 'elem `hasNoType` ''FL.Step
inspect $ 'elem `hasNoType` ''SPEC
#endif

{-# ANN notElem (PermitPatternMatches []) #-}
{-# ANN notElem (PermitConstructions [''Bool]) #-}
{-# ANN notElem (PermitTypeClasses []) #-}
{-# NOINLINE notElem #-}
notElem :: Int -> Int -> IO Bool
notElem n = withStream n $ Stream.fold (FL.notElem (n + 1))

#ifdef INSPECTION
inspect $ 'notElem `hasNoType` ''Step
inspect $ 'notElem `hasNoType` ''FL.Step
inspect $ 'notElem `hasNoType` ''SPEC
#endif

{-# ANN all (PermitPatternMatches []) #-}
{-# ANN all (PermitConstructions [''Bool]) #-}
{-# ANN all (PermitTypeClasses []) #-}
{-# NOINLINE all #-}
all :: Int -> Int -> IO Bool
all n = withStream n $ Stream.fold (FL.all (<= n))

#ifdef INSPECTION
inspect $ 'all `hasNoType` ''Step
inspect $ 'all `hasNoType` ''FL.Step
inspect $ 'all `hasNoType` ''SPEC
#endif

{-# ANN any (PermitPatternMatches []) #-}
{-# ANN any (PermitConstructions [''Bool]) #-}
{-# ANN any (PermitTypeClasses []) #-}
{-# NOINLINE any #-}
any :: Int -> Int -> IO Bool
any n = withStream n $ Stream.fold (FL.any (> n))

#ifdef INSPECTION
inspect $ 'any `hasNoType` ''Step
inspect $ 'any `hasNoType` ''FL.Step
inspect $ 'any `hasNoType` ''SPEC
#endif

-- XXX this is not fusing
{-# ANN take (PermitPatternMatches []) #-}
{-# ANN take (PermitConstructions [''()]) #-}
{-# ANN take (PermitTypeClasses []) #-}
{-# NOINLINE take #-}
take :: Int -> Int -> IO ()
take n x = (withStream n $ Stream.fold (FL.take n FL.drain)) x

#ifdef INSPECTION
inspect $ 'take `hasNoType` ''Step
inspect $ 'take `hasNoType` ''FL.Step
inspect $ 'take `hasNoType` ''SPEC
inspect $ 'take `hasNoType` ''FL.Tuple'Fused
#endif

{-# ANN and (PermitPatternMatches []) #-}
{-# ANN and (PermitConstructions [''Bool]) #-}
{-# ANN and (PermitTypeClasses []) #-}
{-# NOINLINE and #-}
and :: Int -> Int -> IO Bool
and n = withStream n $ Stream.fold FL.and . fmap (<= (n + 1))

#ifdef INSPECTION
inspect $ 'and `hasNoType` ''Step
inspect $ 'and `hasNoType` ''FL.Step
inspect $ 'and `hasNoType` ''SPEC
#endif

{-# ANN or (PermitPatternMatches []) #-}
{-# ANN or (PermitConstructions [''Bool]) #-}
{-# ANN or (PermitTypeClasses []) #-}
{-# NOINLINE or #-}
or :: Int -> Int -> IO Bool
or n = withStream n $ Stream.fold FL.or . fmap (> (n + 1))

#ifdef INSPECTION
inspect $ 'or `hasNoType` ''Step
inspect $ 'or `hasNoType` ''FL.Step
inspect $ 'or `hasNoType` ''SPEC
#endif

-------------------------------------------------------------------------------
-- Filter
-------------------------------------------------------------------------------

{-# ANN filter (PermitPatternMatches []) #-}
{-# ANN filter (PermitConstructions [''()]) #-}
{-# ANN filter (PermitTypeClasses []) #-}
{-# NOINLINE filter #-}
filter :: Int -> Int -> IO ()
filter n = withStream n $ Stream.fold (FL.filter even FL.drain)

#ifdef INSPECTION
inspect $ 'filter `hasNoType` ''Step
inspect $ 'filter `hasNoType` ''FL.Step
inspect $ 'filter `hasNoType` ''SPEC
#endif

{-# ANN postscanlMaybe (PermitPatternMatches []) #-}
{-# ANN postscanlMaybe (PermitConstructions [''()]) #-}
{-# ANN postscanlMaybe (PermitTypeClasses []) #-}
{-# NOINLINE postscanlMaybe #-}
postscanlMaybe :: Int -> Int -> IO ()
postscanlMaybe n = withStream n $
    Stream.fold (FL.postscanlMaybe (Scanl.filtering even) FL.drain)

#ifdef INSPECTION
inspect $ 'postscanlMaybe `hasNoType` ''Step
inspect $ 'postscanlMaybe `hasNoType` ''FL.Step
inspect $ 'postscanlMaybe `hasNoType` ''SPEC
#endif

{-# ANN postscanlMaybe_x2 (PermitPatternMatches []) #-}
{-# ANN postscanlMaybe_x2 (PermitConstructions [''()]) #-}
{-# ANN postscanlMaybe_x2 (PermitTypeClasses []) #-}
{-# NOINLINE postscanlMaybe_x2 #-}
postscanlMaybe_x2 :: Int -> Int -> IO ()
postscanlMaybe_x2 n = withStream n $
    Stream.fold
        $ FL.postscanlMaybe (Scanl.filtering even)
        $ FL.postscanlMaybe (Scanl.filtering odd) FL.drain

#ifdef INSPECTION
inspect $ 'postscanlMaybe_x2 `hasNoType` ''Step
inspect $ 'postscanlMaybe_x2 `hasNoType` ''FL.Step
inspect $ 'postscanlMaybe_x2 `hasNoType` ''SPEC
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

{-# ANN splitWith (PermitPatternMatches []) #-}
{-# ANN splitWith (PermitConstructions [''(,),''Bool]) #-}
{-# ANN splitWith (PermitTypeClasses []) #-}
{-# NOINLINE splitWith #-}
splitWith :: Int -> Int -> IO (Bool, Bool)
splitWith n = withStream n $
    Stream.fold
        (FL.splitWith (,)
            (FL.all (<= (n `div` 2)))
            (FL.any (> n))
        )

#ifdef INSPECTION
inspect $ 'splitWith `hasNoType` ''Step
inspect $ 'splitWith `hasNoType` ''FL.Step
inspect $ 'splitWith `hasNoType` ''SPEC
inspect $ 'splitWith `hasNoType` ''FL.SeqFoldState
#endif

{-# ANN split_ (PermitPatternMatches []) #-}
{-# ANN split_ (PermitConstructions [''Bool]) #-}
{-# ANN split_ (PermitTypeClasses []) #-}
{-# NOINLINE split_ #-}
split_ :: Int -> Int -> IO Bool
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

{-# ANN shortest (PermitPatternMatches []) #-}
{-# ANN shortest (PermitConstructions [''Either,''Int]) #-}
{-# ANN shortest (PermitTypeClasses []) #-}
{-# NOINLINE shortest #-}
shortest :: Int -> Int -> IO (Either Int Int)
shortest n = withStream n $ Stream.fold (FL.shortest FL.sum FL.length)

#ifdef INSPECTION
-- shortest uses Tuple' (no Fuse annotation), so only check the basics
inspect $ 'shortest `hasNoType` ''Step
inspect $ 'shortest `hasNoType` ''FL.Step
inspect $ 'shortest `hasNoType` ''SPEC
#endif

{-# ANN longest (PermitPatternMatches []) #-}
{-# ANN longest (PermitConstructions [''Either,''Int]) #-}
{-# ANN longest (PermitTypeClasses []) #-}
{-# NOINLINE longest #-}
longest :: Int -> Int -> IO (Either Int Int)
longest n = withStream n $ Stream.fold (FL.longest FL.sum FL.length)

#ifdef INSPECTION
-- longest has LongestState with Fuse annotation
inspect $ 'longest `hasNoType` ''Step
inspect $ 'longest `hasNoType` ''FL.Step
inspect $ 'longest `hasNoType` ''SPEC
inspect $ 'longest `hasNoType` ''FL.LongestState
#endif

{-# ANN foldBreak (PermitPatternMatches [''Stream.Step,''Int]) #-}
{-# ANN foldBreak (PermitConstructions [''Int,''Stream.Step]) #-}
{-# ANN foldBreak (PermitTypeClasses []) #-}
{-# NOINLINE foldBreak #-}
foldBreak :: Int -> Int -> IO ()
foldBreak n = withStream n go
    where
    go s = do
        (r, s1) <- Stream.foldBreak (FL.take 1 FL.length) s
        when (r /= 0) $ go s1

-------------------------------------------------------------------------------
-- Split generated streams (not a file)
-------------------------------------------------------------------------------

{-# ANN many (PermitPatternMatches []) #-}
{-# ANN many (PermitConstructions [''()]) #-}
{-# ANN many (PermitTypeClasses []) #-}
{-# NOINLINE many #-}
many :: Int -> Int -> IO ()
many n = withStream n $ Stream.fold (FL.many (FL.take 1 FL.drain) FL.drain)

#ifdef INSPECTION
inspect $ 'many `hasNoType` ''Step
inspect $ 'many `hasNoType` ''FL.Step
inspect $ 'many `hasNoType` ''SPEC
inspect $ 'many `hasNoType` ''FL.ManyState
#endif

{-# ANN takeEndBy_ (PermitPatternMatches []) #-}
{-# ANN takeEndBy_ (PermitConstructions [''()]) #-}
{-# ANN takeEndBy_ (PermitTypeClasses []) #-}
{-# NOINLINE takeEndBy_ #-}
takeEndBy_ :: Int -> Int -> IO ()
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
{-# ANN takeEndBy__Infix_FileRead (PermitPatternMatches
    [''[],''Int,''UnsafeEquality,''IO,''Array]) #-}
{-# ANN takeEndBy__Infix_FileRead (PermitConstructions
    [''Int,''SrcLoc,''CallStack,''[],''Array,''Ptr,''PinnedState]) #-}
{-# ANN takeEndBy__Infix_FileRead (PermitTypeClasses [''IP]) #-}
{-# NOINLINE takeEndBy__Infix_FileRead #-}
takeEndBy__Infix_FileRead :: Handle -> IO Int
takeEndBy__Infix_FileRead inh =
    Stream.fold Fold.length
        $ Stream.foldManyPost (FL.takeEndBy_ (== lf) Fold.drain)
        $ Handle.read inh -- >>= print

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'takeEndBy__Infix_FileRead
inspect $ 'takeEndBy__Infix_FileRead `hasNoType` ''Step
inspect $ 'takeEndBy__Infix_FileRead `hasNoType` ''FL.Step
inspect $ 'takeEndBy__Infix_FileRead `hasNoType` ''SPEC
-- FH.read/A.read
inspect $ 'takeEndBy__Infix_FileRead `hasNoType` ''MutArray.ArrayUnsafe
#endif

-- | Split on line feed.
{-# ANN takeEndBy__Suffix_FileRead (PermitPatternMatches
    [''[],''Int,''UnsafeEquality,''IO,''Array]) #-}
{-# ANN takeEndBy__Suffix_FileRead (PermitConstructions
    [''Int,''SrcLoc,''CallStack,''[],''Array,''Ptr,''PinnedState]) #-}
{-# ANN takeEndBy__Suffix_FileRead (PermitTypeClasses [''IP]) #-}
{-# NOINLINE takeEndBy__Suffix_FileRead #-}
takeEndBy__Suffix_FileRead :: Handle -> IO Int
takeEndBy__Suffix_FileRead inh =
    Stream.fold Fold.length
        $ Stream.foldMany
            (Fold.takeEndBy_ (== lf) Fold.drain)
            (Handle.read inh)
     -- >>= print

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'takeEndBy__Suffix_FileRead
inspect $ 'takeEndBy__Suffix_FileRead `hasNoType` ''Step
inspect $ 'takeEndBy__Suffix_FileRead `hasNoType` ''FL.Step
inspect $ 'takeEndBy__Suffix_FileRead `hasNoType` ''SPEC
-- FH.read/A.read
inspect $ 'takeEndBy__Suffix_FileRead `hasNoType` ''MutArray.ArrayUnsafe
#endif

-- | Split on line feed.
{-# ANN takeEndBy__Suffix_ParseMany_FileRead (PermitPatternMatches
    [''[],''Int,''UnsafeEquality,''IO,''Array]) #-}
{-# ANN takeEndBy__Suffix_ParseMany_FileRead (PermitConstructions
    [''Int,''SrcLoc,''CallStack,''[],''Array,''Ptr,''PinnedState]) #-}
{-# ANN takeEndBy__Suffix_ParseMany_FileRead (PermitTypeClasses [''IP]) #-}
{-# NOINLINE takeEndBy__Suffix_ParseMany_FileRead #-}
takeEndBy__Suffix_ParseMany_FileRead :: Handle -> IO Int
takeEndBy__Suffix_ParseMany_FileRead inh =
    Stream.fold Fold.length
        $ Stream.parseMany
            (Parser.fromFold $ Fold.takeEndBy_ (== lf) Fold.drain)
            (Handle.read inh)
     -- >>= print

-- | Split suffix with line feed.
{-# ANN takeEndBy_Suffix_FileRead (PermitPatternMatches
    [''[],''Int,''UnsafeEquality,''IO,''Array]) #-}
{-# ANN takeEndBy_Suffix_FileRead (PermitConstructions
    [''Int,''SrcLoc,''CallStack,''[],''Array,''Ptr,''PinnedState]) #-}
{-# ANN takeEndBy_Suffix_FileRead (PermitTypeClasses [''IP]) #-}
{-# NOINLINE takeEndBy_Suffix_FileRead #-}
takeEndBy_Suffix_FileRead :: Handle -> IO Int
takeEndBy_Suffix_FileRead inh =
    Stream.fold Fold.length
        $ Stream.foldMany
            (Fold.takeEndBy (== lf) Fold.drain)
            (Handle.read inh)
     -- >>= print

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'takeEndBy_Suffix_FileRead
inspect $ 'takeEndBy_Suffix_FileRead `hasNoType` ''Step
inspect $ 'takeEndBy_Suffix_FileRead `hasNoType` ''FL.Step
inspect $ 'takeEndBy_Suffix_FileRead `hasNoType` ''SPEC
-- FH.read/A.read
inspect $ 'takeEndBy_Suffix_FileRead `hasNoType` ''MutArray.ArrayUnsafe
#endif

-- | Infix split on a word8 sequence.
{-# ANN takeEndBySeq__Infix_FileRead (PermitPatternMatches
    [''[],''Char,''Int,''UnsafeEquality,''IO,''MutArray,''Word8,''State
    ,''Array,''Word,''Word32]) #-}
{-# ANN takeEndBySeq__Infix_FileRead (PermitConstructions
    [''[],''Word8,''Int,''SrcLoc,''CallStack,''Array,''Word32,''MutArray
    ,''State,''Maybe,''Bool,''Word,''(),''Ptr,''PinnedState]) #-}
{-# ANN takeEndBySeq__Infix_FileRead (PermitTypeClasses [''IP]) #-}
{-# NOINLINE takeEndBySeq__Infix_FileRead #-}
takeEndBySeq__Infix_FileRead :: String -> Handle -> IO Int
takeEndBySeq__Infix_FileRead str inh =
    Stream.fold Fold.length
        $ Stream.foldManyPost (Fold.takeEndBySeq_ (toarr str) Fold.drain)
        $ Handle.read inh -- >>= print

#ifdef INSPECTION
-- inspect $ hasNoTypeClasses 'takeEndBySeq__Infix_FileRead
-- inspect $ 'takeEndBySeq__Infix_FileRead `hasNoType` ''Step
#endif

-- | Infix split on a word8 sequence.
{-# ANN takeEndBySeq__Infix_Long_FileRead (PermitPatternMatches
    [''[],''Int,''UnsafeEquality,''IO,''Word,''Array,''MutByteArray
    ,''Word32]) #-}
{-# ANN takeEndBySeq__Infix_Long_FileRead (PermitConstructions
    [''Int,''SrcLoc,''CallStack,''[],''Array,''Word32,''Word,''()
    ,''MutByteArray,''Ptr,''PinnedState]) #-}
{-# ANN takeEndBySeq__Infix_Long_FileRead (PermitTypeClasses [''IP]) #-}
{-# NOINLINE takeEndBySeq__Infix_Long_FileRead #-}
takeEndBySeq__Infix_Long_FileRead :: Handle -> IO Int
takeEndBySeq__Infix_Long_FileRead inh = do
    arr <- Stream.fold Array.create $ Stream.replicate 100000 123
    Stream.fold Fold.length
        $ Stream.foldManyPost (Fold.takeEndBySeq_ arr Fold.drain)
        $ Handle.read inh -- >>= print

-- | Split on suffix sequence.
{-# ANN takeEndBySeq__Suffix_FileRead (PermitPatternMatches
    [''[],''Char,''Int,''UnsafeEquality,''IO,''MutArray,''Word8,''State
    ,''Array,''(),''Word,''SplitOnSeqState,''MutByteArray,''Word32]) #-}
{-# ANN takeEndBySeq__Suffix_FileRead (PermitConstructions
    [''[],''Word8,''Int,''SrcLoc,''CallStack,''Array,''Word32,''MutArray
    ,''State,''Maybe,''Bool,''(),''Word,''Ptr,''PinnedState
    ,''SplitOnSeqState,''MutByteArray]) #-}
{-# ANN takeEndBySeq__Suffix_FileRead (PermitTypeClasses [''IP]) #-}
{-# NOINLINE takeEndBySeq__Suffix_FileRead #-}
takeEndBySeq__Suffix_FileRead :: String -> Handle -> IO Int
takeEndBySeq__Suffix_FileRead str inh =
    Stream.fold Fold.length
        $ Stream.foldMany (Fold.takeEndBySeq_ (toarr str) Fold.drain)
        $ Handle.read inh -- >>= print

#ifdef INSPECTION
-- inspect $ hasNoTypeClasses 'takeEndBySeq__Suffix_FileRead
-- inspect $ 'takeEndBySeq__Suffix_FileRead `hasNoType` ''Step
#endif

-- | Split on suffix sequence.
{-# ANN takeEndBySeq_Suffix_FileRead (PermitPatternMatches
    [''[],''Char,''Int,''UnsafeEquality,''IO,''MutArray,''Word8,''State
    ,''Array,''Word,''Word32]) #-}
{-# ANN takeEndBySeq_Suffix_FileRead (PermitConstructions
    [''[],''Word8,''Int,''SrcLoc,''CallStack,''Array,''Word32,''MutArray
    ,''State,''Maybe,''Bool,''Word,''Ptr,''PinnedState]) #-}
{-# ANN takeEndBySeq_Suffix_FileRead (PermitTypeClasses [''IP]) #-}
{-# NOINLINE takeEndBySeq_Suffix_FileRead #-}
takeEndBySeq_Suffix_FileRead :: String -> Handle -> IO Int
takeEndBySeq_Suffix_FileRead str inh =
    Stream.fold Fold.length
        $ Stream.foldMany (Fold.takeEndBySeq (toarr str) Fold.drain)
        $ Handle.read inh -- >>= print


-- | Infix split on a character sequence.
{-# ANN takeEndBySeq__Infix_Utf8_FileRead (PermitPatternMatches
    [''MutArray,''[],''Char,''Int,''State,''UnsafeEquality,''IO,''Array
    ,''Word,''Word32,''Ptr,''Stream.Step]) #-}
{-# ANN takeEndBySeq__Infix_Utf8_FileRead (PermitConstructions
    [''MutArray,''State,''Maybe,''Bool,''Int,''SrcLoc,''CallStack,''[]
    ,''Array,''Word32,''Word,''(),''Ptr,''PinnedState]) #-}
{-# ANN takeEndBySeq__Infix_Utf8_FileRead (PermitTypeClasses [''IP]) #-}
{-# NOINLINE takeEndBySeq__Infix_Utf8_FileRead #-}
takeEndBySeq__Infix_Utf8_FileRead :: String -> Handle -> IO Int
takeEndBySeq__Infix_Utf8_FileRead str inh =
    -- XXX requires @-fspec-constr-recursive=12@. Maybe due to
    -- decodeUtf8.
    Stream.fold Fold.length
        $ Stream.foldManyPost (Fold.takeEndBySeq_ (Array.fromList str) Fold.drain)
        $ Unicode.decodeUtf8Chunks
        $ Handle.readChunks inh -- >>= print


-------------------------------------------------------------------------------
-- Distributing by parallel application
-------------------------------------------------------------------------------

{-# ANN teeWith_SumLength (PermitPatternMatches []) #-}
{-# ANN teeWith_SumLength (PermitConstructions [''(,),''Int]) #-}
{-# ANN teeWith_SumLength (PermitTypeClasses []) #-}
{-# NOINLINE teeWith_SumLength #-}
teeWith_SumLength :: Int -> Int -> IO (Int, Int)
teeWith_SumLength n =
    withStream n $ Stream.fold (FL.teeWith (,) FL.sum FL.length)

#ifdef INSPECTION
inspect $ 'teeWith_SumLength `hasNoType` ''Step
inspect $ 'teeWith_SumLength `hasNoType` ''FL.Step
inspect $ 'teeWith_SumLength `hasNoType` ''SPEC
inspect $ 'teeWith_SumLength `hasNoType` ''FL.TeeState
#endif

{-# ANN teeWith_AllAny (PermitPatternMatches []) #-}
{-# ANN teeWith_AllAny (PermitConstructions [''(,),''Bool]) #-}
{-# ANN teeWith_AllAny (PermitTypeClasses []) #-}
{-# NOINLINE teeWith_AllAny #-}
teeWith_AllAny :: Int -> Int -> IO (Bool, Bool)
teeWith_AllAny n = withStream n $
    Stream.fold (FL.teeWith (,) (FL.all (<= n)) (FL.any (> n)))

#ifdef INSPECTION
inspect $ 'teeWith_AllAny `hasNoType` ''Step
inspect $ 'teeWith_AllAny `hasNoType` ''FL.Step
inspect $ 'teeWith_AllAny `hasNoType` ''SPEC
inspect $ 'teeWith_AllAny `hasNoType` ''FL.TeeState
#endif

{-# ANN teeWithFst (PermitPatternMatches []) #-}
{-# ANN teeWithFst (PermitConstructions [''(,),''Int]) #-}
{-# ANN teeWithFst (PermitTypeClasses []) #-}
{-# NOINLINE teeWithFst #-}
teeWithFst :: Int -> Int -> IO (Int, Int)
teeWithFst n = withStream n $ Stream.fold (FL.teeWithFst (,) FL.sum FL.length)

#ifdef INSPECTION
inspect $ 'teeWithFst `hasNoType` ''Step
inspect $ 'teeWithFst `hasNoType` ''FL.Step
inspect $ 'teeWithFst `hasNoType` ''SPEC
inspect $ 'teeWithFst `hasNoType` ''FL.TeeFstState
#endif

{-# ANN teeWithMin (PermitPatternMatches []) #-}
{-# ANN teeWithMin (PermitConstructions [''(,),''Int]) #-}
{-# ANN teeWithMin (PermitTypeClasses []) #-}
{-# NOINLINE teeWithMin #-}
teeWithMin :: Int -> Int -> IO (Int, Int)
teeWithMin n = withStream n $ Stream.fold (FL.teeWithMin (,) FL.sum FL.length)

#ifdef INSPECTION
-- teeWithMin uses Tuple' (no Fuse annotation), so only check the basics
inspect $ 'teeWithMin `hasNoType` ''Step
inspect $ 'teeWithMin `hasNoType` ''FL.Step
inspect $ 'teeWithMin `hasNoType` ''SPEC
#endif

{-# ANN distribute (PermitPatternMatches []) #-}
{-# ANN distribute (PermitConstructions [''[],''Int]) #-}
{-# ANN distribute (PermitTypeClasses []) #-}
{-# NOINLINE distribute #-}
distribute :: Int -> Int -> IO [Int]
distribute n = withStream n $ Stream.fold (FL.distribute [FL.sum, FL.length])

-------------------------------------------------------------------------------
-- Partitioning
-------------------------------------------------------------------------------

{-# INLINE oddEven #-}
oddEven :: Int -> Either Int Int
oddEven x = if odd x then Left x else Right x

{-# ANN partition (PermitPatternMatches []) #-}
{-# ANN partition (PermitConstructions [''(,),''Int]) #-}
{-# ANN partition (PermitTypeClasses []) #-}
{-# NOINLINE partition #-}
partition :: Int -> Int -> IO (Int, Int)
partition n = withStream n $ Stream.fold $ FL.lmap oddEven (FL.partition FL.sum FL.length)

#ifdef INSPECTION
inspect $ 'partition `hasNoType` ''Step
inspect $ 'partition `hasNoType` ''FL.Step
inspect $ 'partition `hasNoType` ''SPEC
inspect $ 'partition `hasNoType` ''FL.TeeState
#endif

{-# ANN partitionByFstM (PermitPatternMatches []) #-}
{-# ANN partitionByFstM (PermitConstructions [''(,),''Int]) #-}
{-# ANN partitionByFstM (PermitTypeClasses []) #-}
{-# NOINLINE partitionByFstM #-}
partitionByFstM :: Int -> Int -> IO (Int, Int)
partitionByFstM n = withStream n $
    Stream.fold (FL.partitionByFstM (return . oddEven) FL.sum FL.length)

#ifdef INSPECTION
inspect $ 'partitionByFstM `hasNoType` ''Step
inspect $ 'partitionByFstM `hasNoType` ''FL.Step
inspect $ 'partitionByFstM `hasNoType` ''SPEC
inspect $ 'partitionByFstM `hasNoType` ''FL.TeeFstState
#endif

{-# ANN partitionByMinM (PermitPatternMatches []) #-}
{-# ANN partitionByMinM (PermitConstructions [''(,),''Int]) #-}
{-# ANN partitionByMinM (PermitTypeClasses []) #-}
{-# NOINLINE partitionByMinM #-}
partitionByMinM :: Int -> Int -> IO (Int, Int)
partitionByMinM n = withStream n $
    Stream.fold (FL.partitionByMinM (return . oddEven) FL.sum FL.length)

-------------------------------------------------------------------------------
-- Unzip
-------------------------------------------------------------------------------

{-# ANN unzip (PermitPatternMatches []) #-}
{-# ANN unzip (PermitConstructions [''(,),''Int]) #-}
{-# ANN unzip (PermitTypeClasses []) #-}
{-# NOINLINE unzip #-}
unzip :: Int -> Int -> IO (Int, Int)
unzip n = withStream n $ Stream.fold $ FL.lmap (\a -> (a, a)) (FL.unzip FL.sum FL.length)

#ifdef INSPECTION
inspect $ 'unzip `hasNoType` ''Step
inspect $ 'unzip `hasNoType` ''FL.Step
inspect $ 'unzip `hasNoType` ''SPEC
inspect $ 'unzip `hasNoType` ''FL.TeeState
#endif

{-# ANN unzipWithFstM (PermitPatternMatches []) #-}
{-# ANN unzipWithFstM (PermitConstructions [''(,),''Int]) #-}
{-# ANN unzipWithFstM (PermitTypeClasses []) #-}
{-# NOINLINE unzipWithFstM #-}
unzipWithFstM :: Int -> Int -> IO (Int, Int)
unzipWithFstM n = withStream n $ Stream.fold (FL.unzipWithFstM f FL.sum FL.length)
    where f a = return (a + 1, a)

#ifdef INSPECTION
inspect $ 'unzipWithFstM `hasNoType` ''Step
inspect $ 'unzipWithFstM `hasNoType` ''FL.Step
inspect $ 'unzipWithFstM `hasNoType` ''SPEC
inspect $ 'unzipWithFstM `hasNoType` ''FL.TeeFstState
#endif

{-# ANN unzipWithMinM (PermitPatternMatches []) #-}
{-# ANN unzipWithMinM (PermitConstructions [''(,),''Int]) #-}
{-# ANN unzipWithMinM (PermitTypeClasses []) #-}
{-# NOINLINE unzipWithMinM #-}
unzipWithMinM :: Int -> Int -> IO (Int, Int)
unzipWithMinM n = withStream n $ Stream.fold (FL.unzipWithMinM f FL.sum FL.length)
    where f a = return (a + 1, a)

-------------------------------------------------------------------------------
-- Nested
-------------------------------------------------------------------------------

{-# ANN unfoldEach (PermitPatternMatches [''()]) #-}
{-# ANN unfoldEach (PermitConstructions [''()]) #-}
{-# ANN unfoldEach (PermitTypeClasses []) #-}
{-# NOINLINE unfoldEach #-}
unfoldEach :: Int -> Int -> IO ()
unfoldEach n start =
    Stream.fold (FL.unfoldEach Unfold.replicateM FL.drain)
        $ Stream.fromPure (n, return start)

-------------------------------------------------------------------------------
-- Transformation
-------------------------------------------------------------------------------

{-# ANN lmap (PermitPatternMatches []) #-}
{-# ANN lmap (PermitConstructions [''()]) #-}
{-# ANN lmap (PermitTypeClasses []) #-}
{-# NOINLINE lmap #-}
lmap :: Int -> Int -> IO ()
lmap n = withStream n $ Stream.fold (FL.lmap (+ 1) FL.drain)

#ifdef INSPECTION
inspect $ 'lmap `hasNoType` ''Step
inspect $ 'lmap `hasNoType` ''FL.Step
inspect $ 'lmap `hasNoType` ''SPEC
#endif

{-# ANN mapMaybe (PermitPatternMatches []) #-}
{-# ANN mapMaybe (PermitConstructions [''()]) #-}
{-# ANN mapMaybe (PermitTypeClasses []) #-}
{-# NOINLINE mapMaybe #-}
mapMaybe :: Int -> Int -> IO ()
mapMaybe n = withStream n $
    Stream.fold (FL.mapMaybe (\x -> if even x then Just x else Nothing) FL.drain)

#ifdef INSPECTION
inspect $ 'mapMaybe `hasNoType` ''Step
inspect $ 'mapMaybe `hasNoType` ''FL.Step
inspect $ 'mapMaybe `hasNoType` ''SPEC
#endif

{-# ANN rmapM_Sequence (PermitPatternMatches []) #-}
{-# ANN rmapM_Sequence (PermitConstructions [''()]) #-}
{-# ANN rmapM_Sequence (PermitTypeClasses []) #-}
{-# NOINLINE rmapM_Sequence #-}
rmapM_Sequence :: Int -> Int -> IO ()
rmapM_Sequence n =
    withStream n $ Stream.fold (FL.rmapM id (return <$> FL.drain))

{-# ANN rmapM (PermitPatternMatches []) #-}
{-# ANN rmapM (PermitConstructions [''()]) #-}
{-# ANN rmapM (PermitTypeClasses []) #-}
{-# NOINLINE rmapM #-}
rmapM :: Int -> Int -> IO ()
rmapM n = withStream n $ Stream.fold (FL.rmapM return FL.drain)

#ifdef INSPECTION
inspect $ 'rmapM `hasNoType` ''Step
inspect $ 'rmapM `hasNoType` ''FL.Step
inspect $ 'rmapM `hasNoType` ''SPEC
#endif

{-# ANN pipe (PermitPatternMatches []) #-}
{-# ANN pipe (PermitConstructions [''()]) #-}
{-# ANN pipe (PermitTypeClasses []) #-}
{-# NOINLINE pipe #-}
pipe :: Int -> Int -> IO ()
pipe n = withStream n $
    Stream.fold (FL.pipe (Pipe.mapM (\x -> return $ x + 1)) FL.drain)

#ifdef INSPECTION
inspect $ 'pipe `hasNoType` ''Step
inspect $ 'pipe `hasNoType` ''FL.Step
inspect $ 'pipe `hasNoType` ''SPEC
#endif

{-# ANN scanl (PermitPatternMatches []) #-}
{-# ANN scanl (PermitConstructions [''()]) #-}
{-# ANN scanl (PermitTypeClasses []) #-}
{-# NOINLINE scanl #-}
scanl :: Int -> Int -> IO ()
scanl n = withStream n $ Stream.fold $ FL.scanl Scanl.sum FL.drain

#ifdef INSPECTION
inspect $ 'scanl `hasNoType` ''Step
inspect $ 'scanl `hasNoType` ''FL.Step
inspect $ 'scanl `hasNoType` ''SPEC
#endif

{-# ANN scanlMany (PermitPatternMatches []) #-}
{-# ANN scanlMany (PermitConstructions [''()]) #-}
{-# ANN scanlMany (PermitTypeClasses []) #-}
{-# NOINLINE scanlMany #-}
scanlMany :: Int -> Int -> IO ()
scanlMany n = withStream n $
    Stream.fold $ FL.scanlMany (Scanl.take 2 Scanl.drain) FL.drain

#ifdef INSPECTION
inspect $ 'scanlMany `hasNoType` ''Step
-- inspect $ 'scanlMany `hasNoType` ''FL.Step
inspect $ 'scanlMany `hasNoType` ''SPEC
#endif

{-# ANN postscanl (PermitPatternMatches []) #-}
{-# ANN postscanl (PermitConstructions [''()]) #-}
{-# ANN postscanl (PermitTypeClasses []) #-}
{-# NOINLINE postscanl #-}
postscanl :: Int -> Int -> IO ()
postscanl n = withStream n $ Stream.fold $ FL.postscanl Scanl.sum FL.drain

#ifdef INSPECTION
inspect $ 'postscanl `hasNoType` ''Step
inspect $ 'postscanl `hasNoType` ''FL.Step
inspect $ 'postscanl `hasNoType` ''SPEC
#endif

-------------------------------------------------------------------------------
-- O(n)-heap: elimination (building structures)
-------------------------------------------------------------------------------

{-# ANN toList (PermitPatternMatches []) #-}
{-# ANN toList (PermitConstructions [''[],''Int]) #-}
{-# ANN toList (PermitTypeClasses []) #-}
{-# NOINLINE toList #-}
toList :: Int -> Int -> IO [Int]
toList n = withStream n $ Stream.fold FL.toList

{-# ANN toListRev (PermitPatternMatches [''Int,''SPEC,''[]]) #-}
{-# ANN toListRev (PermitConstructions [''[],''Int,''SPEC]) #-}
{-# ANN toListRev (PermitTypeClasses []) #-}
{-# NOINLINE toListRev #-}
toListRev :: Int -> Int -> IO [Int]
toListRev n = withStream n $ Stream.fold FL.toListRev

{-# ANN toStream (PermitPatternMatches [''[]]) #-}
{-# ANN toStream (PermitConstructions [''Stream.Step,''[],''Int,''Stream]) #-}
{-# ANN toStream (PermitTypeClasses []) #-}
{-# NOINLINE toStream #-}
toStream :: Int -> Int -> IO (Stream Identity Int)
toStream n = withStream n $ Stream.fold FL.toStream

{-# ANN toStreamRev (PermitPatternMatches [''[],''Int,''SPEC]) #-}
{-# ANN toStreamRev (PermitConstructions
    [''Stream.Step,''Stream,''[],''Int,''SPEC]) #-}
{-# ANN toStreamRev (PermitTypeClasses []) #-}
{-# NOINLINE toStreamRev #-}
toStreamRev :: Int -> Int -> IO (Stream Identity Int)
toStreamRev n = withStream n $ Stream.fold FL.toStreamRev

{-# ANN nub (PermitPatternMatches [''Set,''Int,''Tuple']) #-}
{-# ANN nub (PermitConstructions [''Set,''Tuple',''Maybe,''Int]) #-}
{-# ANN nub (PermitTypeClasses []) #-}
{-# NOINLINE nub #-}
nub :: Int -> Int -> IO (Maybe Int)
nub n = withStream n $ Stream.fold FL.nub

-------------------------------------------------------------------------------
-- O(n)-heap: key-value
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

{-# ANN demuxerToContainer_Map (PermitPatternMatches
    [''Map,''Int,''Tuple',''Fold,''FL.Step,''IO,''Maybe]) #-}
{-# ANN demuxerToContainer_Map (PermitConstructions
    [''Int,''FL.Step,''Fold,''Map,''Maybe,''Tuple',''SrcLoc,''CallStack]) #-}
{-# ANN demuxerToContainer_Map (PermitTypeClasses [''IP]) #-}
{-# NOINLINE demuxerToContainer_Map #-}
demuxerToContainer_Map :: Int -> Int -> Int -> IO (Map Int Int)
demuxerToContainer_Map buckets n =
    withStream n $ Stream.fold (FL.demuxerToContainer (getKey buckets) getFold)

{-# ANN demuxerToContainer_IntMap (PermitPatternMatches
    [''Int,''Tuple',''IntMap,''Fold,''FL.Step,''IO]) #-}
{-# ANN demuxerToContainer_IntMap (PermitConstructions
    [''Int,''FL.Step,''Fold,''Tuple',''SrcLoc,''CallStack,''IntMap]) #-}
{-# ANN demuxerToContainer_IntMap (PermitTypeClasses [''IP]) #-}
{-# NOINLINE demuxerToContainer_IntMap #-}
demuxerToContainer_IntMap :: Int -> Int -> Int -> IO (IntMap Int)
demuxerToContainer_IntMap buckets n =
    withStream n $ Stream.fold (FL.demuxerToContainer (getKey buckets) getFold)

{-# ANN demuxerToContainerIO_Map (PermitPatternMatches
    [''STRef,''Map,''Int,''Tuple',''Fold,''FL.Step,''IO,''Maybe]) #-}
{-# ANN demuxerToContainerIO_Map (PermitConstructions
    [''Map,''Int,''FL.Step,''Maybe,''Tuple',''SrcLoc,''CallStack,''Fold
    ,''STRef]) #-}
{-# ANN demuxerToContainerIO_Map (PermitTypeClasses [''IP]) #-}
{-# NOINLINE demuxerToContainerIO_Map #-}
demuxerToContainerIO_Map :: Int -> Int -> Int -> IO (Map Int Int)
demuxerToContainerIO_Map buckets n =
    withStream n
        $ Stream.fold (FL.demuxerToContainerIO (getKey buckets) getFold)

{-# ANN toContainer_Map (PermitPatternMatches
    [''Tuple',''Map,''Int,''Maybe]) #-}
{-# ANN toContainer_Map (PermitConstructions [''Map,''Int,''Maybe,''Tuple']) #-}
{-# ANN toContainer_Map (PermitTypeClasses []) #-}
{-# NOINLINE toContainer_Map #-}
toContainer_Map :: Int -> Int -> Int -> IO (Map Int Int)
toContainer_Map buckets n =
    withStream n $ Stream.fold (FL.toContainer (getKey buckets) FL.sum)

{-# ANN toContainer_IntMap (PermitPatternMatches
    [''IntMap,''Tuple',''Int,''FL.Step]) #-}
{-# ANN toContainer_IntMap (PermitConstructions
    [''Int,''IntMap,''FL.Step,''Tuple']) #-}
{-# ANN toContainer_IntMap (PermitTypeClasses []) #-}
{-# NOINLINE toContainer_IntMap #-}
toContainer_IntMap :: Int -> Int -> Int -> IO (IntMap Int)
toContainer_IntMap buckets n =
    withStream n $ Stream.fold (FL.toContainer (getKey buckets) FL.sum)

{-# ANN toContainerIO_Map (PermitPatternMatches
    [''STRef,''Map,''Int,''Tuple',''Maybe]) #-}
{-# ANN toContainerIO_Map (PermitConstructions
    [''Map,''Int,''Maybe,''Tuple',''STRef]) #-}
{-# ANN toContainerIO_Map (PermitTypeClasses []) #-}
{-# NOINLINE toContainerIO_Map #-}
toContainerIO_Map :: Int -> Int -> Int -> IO (Map Int Int)
toContainerIO_Map buckets n =
    withStream n $ Stream.fold (FL.toContainerIO (getKey buckets) FL.sum)

{-# ANN toContainerIO_IntMap (PermitPatternMatches
    [''IntMap,''STRef,''Tuple',''Int,''FL.Step]) #-}
{-# ANN toContainerIO_IntMap (PermitConstructions
    [''Int,''IntMap,''FL.Step,''Tuple',''STRef]) #-}
{-# ANN toContainerIO_IntMap (PermitTypeClasses []) #-}
{-# NOINLINE toContainerIO_IntMap #-}
toContainerIO_IntMap :: Int -> Int -> Int -> IO (IntMap Int)
toContainerIO_IntMap buckets n =
    withStream n $ Stream.fold (FL.toContainerIO (getKey buckets) FL.sum)

-------------------------------------------------------------------------------
-- N-space
-------------------------------------------------------------------------------

{-# ANN concatMap (PermitPatternMatches
    [''ConcatMapState,''Tuple'Fused,''Int,''FL.Step,''Fold]) #-}
{-# ANN concatMap (PermitConstructions
    [''(),''FL.Step,''ConcatMapState,''Tuple'Fused,''Int,''Fold
    ,''SrcLoc,''CallStack]) #-}
{-# ANN concatMap (PermitTypeClasses [''IP]) #-}
{-# NOINLINE concatMap #-}
concatMap :: Int -> Int -> IO ()
concatMap n = withStream n $ Stream.fold (sequence_ n)

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

-- Note: Name each benchmark (and its IO action) after the exported function it
-- benchmarks, using the format functionName_dimension1_dimension2..., where
-- the dimensions are optional variants/type specializations. Keep extra info
-- in parenthetical notes in the description.
benchmarks :: BenchEnv -> Int -> [(SpaceComplexity, Benchmark)]
benchmarks env value =
    fmap (SpaceO_1,)
    [ mkBench "takeEndBy__Infix_FileRead" env $ \inh _ ->
        takeEndBy__Infix_FileRead inh
    , mkBench "takeEndBy__Suffix_FileRead" env $ \inh _ ->
        takeEndBy__Suffix_FileRead inh
    , mkBench "takeEndBy__Suffix_ParseMany_FileRead" env
        $ \inh _ -> takeEndBy__Suffix_ParseMany_FileRead inh
    , mkBench "takeEndBy_Suffix_FileRead" env $ \inh _ ->
        takeEndBy_Suffix_FileRead inh

    -- Splitting on sequence
    -- Infix takeEndBySeq_
    , mkBench "takeEndBySeq__Infix_FileRead (empty pattern)" env $ \inh _ ->
        takeEndBySeq__Infix_FileRead "" inh
    , mkBench "takeEndBySeq__Infix_FileRead (lf)" env $ \inh _ ->
        takeEndBySeq__Infix_FileRead "\n" inh
    , mkBench "takeEndBySeq__Infix_FileRead (a)" env $ \inh _ ->
        takeEndBySeq__Infix_FileRead "a" inh
    , mkBench "takeEndBySeq__Infix_FileRead (crlf)" env $ \inh _ ->
        takeEndBySeq__Infix_FileRead "\r\n" inh
    , mkBench "takeEndBySeq__Infix_FileRead (aa)" env $ \inh _ ->
        takeEndBySeq__Infix_FileRead "aa" inh
    , mkBench "takeEndBySeq__Infix_FileRead (aaaa)" env $ \inh _ ->
        takeEndBySeq__Infix_FileRead "aaaa" inh
    , mkBench "takeEndBySeq__Infix_FileRead (abcdefgh)" env $ \inh _ ->
        takeEndBySeq__Infix_FileRead "abcdefgh" inh
    , mkBench "takeEndBySeq__Infix_FileRead (abcdefghi)" env $ \inh _ ->
        takeEndBySeq__Infix_FileRead "abcdefghi" inh
    , mkBench "takeEndBySeq__Infix_FileRead (catcatcatcatcat)" env $ \inh _ ->
        takeEndBySeq__Infix_FileRead "catcatcatcatcat" inh
    , mkBench "takeEndBySeq__Infix_FileRead (abcdefghijklmnopqrstuvwxyz)"
        env $ \inh _ ->
            takeEndBySeq__Infix_FileRead "abcdefghijklmnopqrstuvwxyz" inh
    , mkBench "takeEndBySeq__Infix_Long_FileRead (100k pattern)"
        env $ \inh _ -> takeEndBySeq__Infix_Long_FileRead inh

    -- Suffix takeEndBySeq_
    , mkBench "takeEndBySeq__Suffix_FileRead (empty pattern)" env $ \inh _ ->
        takeEndBySeq__Suffix_FileRead "" inh
    , mkBench "takeEndBySeq__Suffix_FileRead (lf)" env $ \inh _ ->
        takeEndBySeq__Suffix_FileRead "\n" inh
    , mkBench "takeEndBySeq__Suffix_FileRead (crlf)" env $ \inh _ ->
        takeEndBySeq__Suffix_FileRead "\r\n" inh
    , mkBenchSmall "takeEndBySeq__Suffix_FileRead (abcdefghijklmnopqrstuvwxyz)"
        env $ \inh _ ->
            takeEndBySeq__Suffix_FileRead "abcdefghijklmnopqrstuvwxyz" inh

    -- Suffix takeEndBySeq
    , mkBench "takeEndBySeq_Suffix_FileRead (crlf)" env $ \inh _ ->
        takeEndBySeq_Suffix_FileRead "\r\n" inh
    , mkBenchSmall "takeEndBySeq_Suffix_FileRead (abcdefghijklmnopqrstuvwxyz)"
        env $ \inh _ ->
            takeEndBySeq_Suffix_FileRead "abcdefghijklmnopqrstuvwxyz" inh

    , mkBenchSmall "takeEndBySeq__Infix_Utf8_FileRead (abcdefgh)"
        env $ \inh _ -> takeEndBySeq__Infix_Utf8_FileRead "abcdefgh" inh
    , mkBenchSmall
        "takeEndBySeq__Infix_Utf8_FileRead (abcdefghijklmnopqrstuvwxyz)"
        env $ \inh _ ->
            takeEndBySeq__Infix_Utf8_FileRead "abcdefghijklmnopqrstuvwxyz" inh

    , benchIO "drain" $ drain value
    , benchIO "drainMapM" $ drainMapM value
    , benchIO "drainN" $ drainN value
    , benchIO "latest" $ latest value
    , benchIO "length" $ length value
    , benchIO "top" $ top value
    , benchIO "bottom" $ bottom value
    , benchIO "sum" $ sum value
    , benchIO "foldMap_Sum" $ foldMap_Sum value
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
    , benchIO "foldMap_Last" $ foldMap_Last value
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

    , benchIO "lmap" $ lmap value
    , benchIO "mapMaybe" $ mapMaybe value
    , benchIO "rmapM_Sequence" $ rmapM_Sequence value
    , benchIO "rmapM" $ rmapM value
    , benchIO "pipe (mapM)" $ pipe value
    , benchIO "scanl (sum)" $ scanl value
    , benchIO "scanlMany (take 2, drain)" $ scanlMany value
    , benchIO "postscanl (sum)" $ postscanl value

    , benchIO "filter (even)" $ filter value
    , benchIO "postscanlMaybe (filtering even)" $ postscanlMaybe value
    , benchIO "postscanlMaybe_x2 (filtering even, odd)" $
        postscanlMaybe_x2 value
    , benchIO "foldBreak (recursive)" $ foldBreak value
    , benchIO "splitWith (all, any)" $ splitWith value
    , benchIO "split_ (all, any)" $ split_ value
    , benchIO "teeWith_AllAny" $ teeWith_AllAny value
    , benchIO "many (take 1, drain)" $ many value
    , benchIO "unfoldEach" $ unfoldEach value
    , benchIO "shortest (sum, length)" $ shortest value
    , benchIO "longest (sum, length)" $ longest value
    , benchIO "teeWith_SumLength" $ teeWith_SumLength value
    , benchIO "teeWithFst (sum, length)" $ teeWithFst value
    , benchIO "teeWithMin (sum, length)" $ teeWithMin value
    , benchIO "distribute (sum, length)" $ distribute value
    , benchIO "partition (sum, length)" $ partition value
    , benchIO "partitionByFstM (sum, length)" $ partitionByFstM value
    , benchIO "partitionByMinM (sum, length)" $ partitionByMinM value
    , benchIO "unzip (sum, length)" $ unzip value
    , benchIO "unzipWithFstM (sum, length)" $ unzipWithFstM value
    , benchIO "unzipWithMinM (sum, length)" $ unzipWithMinM value
    ]
    ++ [ ( SpaceO_n
         , benchIO "concatMap (folds sequenced, value div 100)"
             $ concatMap (value `div` 100)
         )
       ]
    ++ fmap (HeapO_n,)
    -- Left folds for building a structure are inherently non-streaming
    -- as the structure cannot be lazily consumed until fully built.
    [ benchIO "toList" $ toList value
    , benchIO "toListRev" $ toListRev value
    , benchIO "toStream" $ toStream value
    , benchIO "toStreamRev" $ toStreamRev value
    , benchIO "nub" $ nub value
    , benchIO "demuxerToContainer_Map (64 buckets, sum & length)"
        $ demuxerToContainer_Map 64 value
    , benchIO "demuxerToContainer_IntMap (64 buckets, sum & length)"
        $ demuxerToContainer_IntMap 64 value
    , benchIO "demuxerToContainerIO_Map (64 buckets, sum & length)"
        $ demuxerToContainerIO_Map 64 value
    -- classify: immutable
    , benchIO "toContainer_Map (64 buckets, sum)"
        $ toContainer_Map 64 value
    , benchIO "toContainer_IntMap (64 buckets, sum)"
        $ toContainer_IntMap 64 value
    -- classify: mutable cells
    , benchIO "toContainerIO_Map (single bucket, sum)"
        $ toContainerIO_Map 1 value
    , benchIO "toContainerIO_Map (64 buckets, sum)"
        $ toContainerIO_Map 64 value
    , benchIO "toContainerIO_Map (max buckets, sum)"
        $ toContainerIO_Map value value
    , benchIO "toContainerIO_IntMap (64 buckets, sum)"
        $ toContainerIO_IntMap 64 value
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
        let allBenches = benchmarks env value
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

    -- demuxerToContainer_Map 64 value 1
    _ <- toContainerIO_IntMap 64 value 1
    return ()
#endif
