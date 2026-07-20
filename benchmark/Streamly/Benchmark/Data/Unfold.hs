-- |
-- Module      : Streamly.Benchmark.Data.Fold
-- Copyright   : (c) 2018 Composewell
-- License     : MIT
-- Maintainer  : streamly@composewell.com

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Control.DeepSeq (NFData(..))
import Control.Exception (Exception, SomeException)
import Control.Monad.Catch (MonadCatch)
import Data.Char (ord)
import Data.STRef (STRef)
import Data.Word (Word8)
import GHC.Classes (IP)
import Unsafe.Coerce (UnsafeEquality)
import GHC.Stack (CallStack, SrcLoc)
import Streamly.Internal.Data.Array (Array)
import Streamly.Internal.Data.MutArray (ArrayUnsafe)
import Streamly.Internal.Data.Producer (ConcatState, EnumToState)
import Streamly.Internal.Data.Stream (UnfoldState)
import Streamly.Internal.Data.Unfold (Unfold)
import System.IO (Handle, hClose)
import System.Random (randomRIO)

import qualified Prelude
import qualified Streamly.FileSystem.Handle as FH
import qualified Streamly.Internal.FileSystem.Handle as IFH
import qualified Streamly.Internal.Data.Fold as FL
import qualified Streamly.Internal.Data.Scanl as Scanl
import qualified Streamly.Internal.Data.Unfold as UF
import qualified Streamly.Internal.Data.Stream as S
import qualified Streamly.Internal.Data.StreamK as K
import qualified Streamly.Internal.Data.SVar.Type as SVar

import qualified Unfold.Enumeration as Enumeration
import qualified Unfold.Type as Type

import Fusion.Plugin.Types
import Test.Tasty.Bench hiding (env)
import Prelude hiding
    (take, filter, zipWith, map, mapM, takeWhile, scanl, repeat, dropWhile)
import Streamly.Benchmark.Common
import Streamly.Benchmark.Common.Handle

{-# INLINE benchIO #-}
benchIO :: (NFData b) => String -> (Int -> IO b) -> Benchmark
benchIO name f = bench name $ nfIO $ randomRIO (1,1) >>= f

-------------------------------------------------------------------------------
-- Stream generation and elimination
-------------------------------------------------------------------------------

-- generate numbers up to the argument value
{-# INLINE source #-}
source :: Monad m => Int -> Unfold m Int Int
source n = UF.supplySecond n UF.enumerateFromToNum

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
    drainTransformation (UF.supplySecond to UF.enumerateFromToNum)

-------------------------------------------------------------------------------
-- Operations on input
-------------------------------------------------------------------------------

{-# ANN discardFirst (PermitPatternMatches [''Int]) #-}
{-# ANN discardFirst (PermitConstructions []) #-}
{-# ANN discardFirst (PermitTypeClasses []) #-}
{-# NOINLINE discardFirst #-}
discardFirst :: Int -> Int -> IO ()
discardFirst size start =
    drainTransformationDefault (size + start) UF.discardFirst (start, start)

{-# ANN discardSecond (PermitPatternMatches [''Int]) #-}
{-# ANN discardSecond (PermitConstructions []) #-}
{-# ANN discardSecond (PermitTypeClasses []) #-}
{-# NOINLINE discardSecond #-}
discardSecond :: Int -> Int -> IO ()
discardSecond size start =
    drainTransformationDefault (size + start) UF.discardSecond (start, start)

-------------------------------------------------------------------------------
-- Stream generation
-------------------------------------------------------------------------------

{-# ANN fromStream (PermitPatternMatches [''Int,''S.Step]) #-}
{-# ANN fromStream (PermitConstructions [''Int,''S.Step]) #-}
{-# ANN fromStream (PermitTypeClasses []) #-}
{-# NOINLINE fromStream #-}
fromStream :: Int -> Int -> IO ()
fromStream size start =
    drainGeneration UF.fromStream (S.replicate size start :: S.Stream IO Int)

-- XXX INVESTIGATE: Although the performance of this should be equivalant to
-- fromStream, this is considerably worse. More than 4x worse.
{-# ANN fromStreamK (PermitPatternMatches [''Maybe,''(,)]) #-}
{-# ANN fromStreamK (PermitConstructions
    [''Maybe,''(,),''SVar.State,''Bool]) #-}
{-# ANN fromStreamK (PermitTypeClasses []) #-}
{-# NOINLINE fromStreamK #-}
fromStreamK :: Int -> Int -> IO ()
fromStreamK size start = drainGeneration UF.fromStreamK (K.replicate size start)

{-# ANN fromStreamD (PermitPatternMatches [''Int,''S.Step]) #-}
{-# ANN fromStreamD (PermitConstructions [''Int,''S.Step]) #-}
{-# ANN fromStreamD (PermitTypeClasses []) #-}
{-# NOINLINE fromStreamD #-}
fromStreamD :: Int -> Int -> IO ()
fromStreamD size start =
    drainGeneration UF.fromStream (S.replicate size start)

-- 'nilM' runs its action on the seed but yields no output, so unfold it over an
-- outer source of value seeds to run it ~value times.
{-# ANN nilM (PermitPatternMatches [''Int]) #-}
{-# ANN nilM (PermitConstructions []) #-}
{-# ANN nilM (PermitTypeClasses []) #-}
{-# NOINLINE nilM #-}
nilM :: Int -> Int -> IO ()
nilM value start =
    drainGeneration (UF.unfoldEach (UF.nilM return) (source (start + value)))
        start

{-# ANN nil (PermitPatternMatches [''Int]) #-}
{-# ANN nil (PermitConstructions []) #-}
{-# ANN nil (PermitTypeClasses []) #-}
{-# NOINLINE nil #-}
nil :: Int -> Int -> IO ()
nil value start =
    drainGeneration (UF.unfoldEach UF.nil (source (start + value))) start

{-# ANN consM (PermitPatternMatches
    [''Int,''EnumToState,''S.Step,''UnfoldState]) #-}
{-# ANN consM (PermitConstructions
    [''UnfoldState,''EnumToState,''Int,''S.Step]) #-}
{-# ANN consM (PermitTypeClasses []) #-}
{-# NOINLINE consM #-}
consM :: Int -> Int -> IO ()
consM size start =
    drainTransformationDefault (size + start) (UF.consM return) start

{-# INLINE _fromSVar #-}
_fromSVar :: Int -> Int -> m ()
_fromSVar = undefined

{-# INLINE _fromProducer #-}
_fromProducer :: Int -> Int -> m ()
_fromProducer = undefined

{-# ANN fromListM (PermitPatternMatches [''[]]) #-}
{-# ANN fromListM (PermitConstructions [''Int,''[]]) #-}
{-# ANN fromListM (PermitTypeClasses []) #-}
{-# NOINLINE fromListM #-}
fromListM :: Int -> Int -> IO ()
fromListM size start =
    drainGeneration UF.fromListM (Prelude.map return [start .. start + size])

{-# ANN replicateM (PermitPatternMatches []) #-}
{-# ANN replicateM (PermitConstructions []) #-}
{-# ANN replicateM (PermitTypeClasses []) #-}
{-# NOINLINE replicateM #-}
replicateM :: Int -> Int -> IO ()
replicateM size start = drainGeneration UF.replicateM (size, return start)

{-# ANN repeatM (PermitPatternMatches []) #-}
{-# ANN repeatM (PermitConstructions []) #-}
{-# ANN repeatM (PermitTypeClasses []) #-}
{-# NOINLINE repeatM #-}
repeatM :: Int -> Int -> IO ()
repeatM size start = drainGeneration (UF.take size UF.repeatM) (return start)

{-# ANN repeat (PermitPatternMatches []) #-}
{-# ANN repeat (PermitConstructions []) #-}
{-# ANN repeat (PermitTypeClasses []) #-}
{-# NOINLINE repeat #-}
repeat :: Int -> Int -> IO ()
repeat size start = drainGeneration (UF.take size UF.repeat) start

{-# ANN iterateM (PermitPatternMatches []) #-}
{-# ANN iterateM (PermitConstructions []) #-}
{-# ANN iterateM (PermitTypeClasses []) #-}
{-# NOINLINE iterateM #-}
iterateM :: Int -> Int -> IO ()
iterateM size start =
    drainGeneration (UF.take size (UF.iterateM return)) (return start)

{-# ANN fromIndicesM (PermitPatternMatches []) #-}
{-# ANN fromIndicesM (PermitConstructions []) #-}
{-# ANN fromIndicesM (PermitTypeClasses []) #-}
{-# NOINLINE fromIndicesM #-}
fromIndicesM :: Int -> Int -> IO ()
fromIndicesM size start =
    drainGeneration (UF.take size (UF.fromIndicesM return)) start

-------------------------------------------------------------------------------
-- Stream transformation
-------------------------------------------------------------------------------

{-# ANN postscanl (PermitPatternMatches [''Int]) #-}
{-# ANN postscanl (PermitConstructions []) #-}
{-# ANN postscanl (PermitTypeClasses []) #-}
{-# NOINLINE postscanl #-}
postscanl :: Int -> Int -> IO ()
postscanl size start =
    drainTransformationDefault (size + start) (UF.postscanl Scanl.sum) start

{-# ANN scanl (PermitPatternMatches [''Int]) #-}
{-# ANN scanl (PermitConstructions []) #-}
{-# ANN scanl (PermitTypeClasses []) #-}
{-# NOINLINE scanl #-}
scanl :: Int -> Int -> IO ()
scanl size start =
    drainTransformationDefault (size + start) (UF.scanl Scanl.sum) start

{-# ANN scanlMany (PermitPatternMatches [''Int]) #-}
{-# ANN scanlMany (PermitConstructions []) #-}
{-# ANN scanlMany (PermitTypeClasses []) #-}
{-# NOINLINE scanlMany #-}
scanlMany :: Int -> Int -> IO ()
scanlMany size start =
    drainTransformationDefault (size + start)
        (UF.scanlMany (Scanl.take 2 Scanl.sum)) start

-------------------------------------------------------------------------------
-- Stream filtering
-------------------------------------------------------------------------------

{-# ANN take (PermitPatternMatches [''Int]) #-}
{-# ANN take (PermitConstructions []) #-}
{-# ANN take (PermitTypeClasses []) #-}
{-# NOINLINE take #-}
take :: Int -> Int -> IO ()
take size start = drainTransformationDefault (size + start) (UF.take size) start

{-# ANN filter (PermitPatternMatches [''Int]) #-}
{-# ANN filter (PermitConstructions []) #-}
{-# ANN filter (PermitTypeClasses []) #-}
{-# NOINLINE filter #-}
filter :: Int -> Int -> IO ()
filter size start =
    drainTransformationDefault (size + start) (UF.filter (\_ -> True)) start

{-# ANN filterM (PermitPatternMatches [''Int]) #-}
{-# ANN filterM (PermitConstructions []) #-}
{-# ANN filterM (PermitTypeClasses []) #-}
{-# NOINLINE filterM #-}
filterM :: Int -> Int -> IO ()
filterM size start =
    drainTransformationDefault
        (size + start)
        (UF.filterM (\_ -> (return True)))
        start

-- Dropping one element from a large stream is dominated by generation, so
-- instead exercise 'drop' ~value/2 times: generate value/2 two-element streams
-- with 'fromTuple', 'drop' the first element of each, and flatten the rest.
{-# ANN drop_One (PermitPatternMatches [''Int]) #-}
{-# ANN drop_One (PermitConstructions []) #-}
{-# ANN drop_One (PermitTypeClasses []) #-}
{-# NOINLINE drop_One #-}
drop_One :: Int -> Int -> IO ()
drop_One value start =
    let outer = UF.map (\i -> (i, i)) (source (start + value `div` 2))
     in drainGeneration (UF.unfoldEach (UF.drop 1 UF.fromTuple) outer) start

{-# ANN drop_All (PermitPatternMatches [''Int]) #-}
{-# ANN drop_All (PermitConstructions []) #-}
{-# ANN drop_All (PermitTypeClasses []) #-}
{-# NOINLINE drop_All #-}
drop_All :: Int -> Int -> IO ()
drop_All size start =
    drainTransformationDefault (size + start) (UF.drop (size + 1)) start

{-# ANN dropWhile_True (PermitPatternMatches [''Int]) #-}
{-# ANN dropWhile_True (PermitConstructions []) #-}
{-# ANN dropWhile_True (PermitTypeClasses []) #-}
{-# NOINLINE dropWhile_True #-}
dropWhile_True :: Int -> Int -> IO ()
dropWhile_True size start =
    drainTransformationDefault
        (size + start)
        (UF.dropWhile (\_ -> True))
        start

{-# ANN dropWhile_False (PermitPatternMatches [''Int]) #-}
{-# ANN dropWhile_False (PermitConstructions []) #-}
{-# ANN dropWhile_False (PermitTypeClasses []) #-}
{-# NOINLINE dropWhile_False #-}
dropWhile_False :: Int -> Int -> IO ()
dropWhile_False size start =
    drainTransformationDefault
        (size + start)
        (UF.dropWhile (\_ -> False))
        start

{-# ANN dropWhileM_True (PermitPatternMatches [''Int]) #-}
{-# ANN dropWhileM_True (PermitConstructions []) #-}
{-# ANN dropWhileM_True (PermitTypeClasses []) #-}
{-# NOINLINE dropWhileM_True #-}
dropWhileM_True :: Int -> Int -> IO ()
dropWhileM_True size start =
    drainTransformationDefault
        (size + start)
        (UF.dropWhileM (\_ -> return True))
        start

{-# ANN dropWhileM_False (PermitPatternMatches [''Int]) #-}
{-# ANN dropWhileM_False (PermitConstructions []) #-}
{-# ANN dropWhileM_False (PermitTypeClasses []) #-}
{-# NOINLINE dropWhileM_False #-}
dropWhileM_False :: Int -> Int -> IO ()
dropWhileM_False size start =
    drainTransformationDefault
        (size + start)
        (UF.dropWhileM (\_ -> return False))
        start

{-# ANN mapMaybe (PermitPatternMatches [''Int]) #-}
{-# ANN mapMaybe (PermitConstructions []) #-}
{-# ANN mapMaybe (PermitTypeClasses []) #-}
{-# NOINLINE mapMaybe #-}
mapMaybe :: Int -> Int -> IO ()
mapMaybe size start =
    drainTransformationDefault (size + start) (UF.mapMaybe Just) start

{-# ANN mapMaybeM (PermitPatternMatches [''Int]) #-}
{-# ANN mapMaybeM (PermitConstructions []) #-}
{-# ANN mapMaybeM (PermitTypeClasses []) #-}
{-# NOINLINE mapMaybeM #-}
mapMaybeM :: Int -> Int -> IO ()
mapMaybeM size start =
    drainTransformationDefault (size + start) (UF.mapMaybeM (return . Just))
        start

{-# ANN catMaybes (PermitPatternMatches [''Int]) #-}
{-# ANN catMaybes (PermitConstructions []) #-}
{-# ANN catMaybes (PermitTypeClasses []) #-}
{-# NOINLINE catMaybes #-}
catMaybes :: Int -> Int -> IO ()
catMaybes size start =
    drainTransformationDefault (size + start) (UF.catMaybes . UF.map Just) start

-------------------------------------------------------------------------------
-- Stream combination
-------------------------------------------------------------------------------

{-# ANN either_Left (PermitPatternMatches [''Int]) #-}
{-# ANN either_Left (PermitConstructions []) #-}
{-# ANN either_Left (PermitTypeClasses []) #-}
{-# NOINLINE either_Left #-}
either_Left :: Int -> Int -> IO ()
either_Left size start =
    drainGeneration
        (UF.either (source (size + start)) (source (size + start)))
        (Left start)

{-# ANN zipRepeat (PermitPatternMatches [''Int]) #-}
{-# ANN zipRepeat (PermitConstructions []) #-}
{-# ANN zipRepeat (PermitTypeClasses []) #-}
{-# NOINLINE zipRepeat #-}
zipRepeat :: Int -> Int -> IO ()
zipRepeat size start =
    drainGeneration (UF.zipRepeat (source (size + start))) (start, start)

-------------------------------------------------------------------------------
-- Applicative
-------------------------------------------------------------------------------

nthRoot :: Double -> Int -> Int
nthRoot n value = round (fromIntegral value**(1/n))

{-# ANN innerJoin (PermitPatternMatches [''Int]) #-}
{-# ANN innerJoin (PermitConstructions [''Int]) #-}
{-# ANN innerJoin (PermitTypeClasses []) #-}
{-# NOINLINE innerJoin #-}
innerJoin :: Int -> Int -> IO ()
innerJoin value start =
    let end = start + nthRoot 2 value
        s = source end
    in UF.fold FL.drain (UF.innerJoin (==) s s) start

-------------------------------------------------------------------------------
-- Resource management
-------------------------------------------------------------------------------

{-# ANN before (PermitPatternMatches [''Int]) #-}
{-# ANN before (PermitConstructions []) #-}
{-# ANN before (PermitTypeClasses []) #-}
{-# NOINLINE before #-}
before :: Int -> Int -> IO ()
before size start =
    drainTransformationDefault (size + start) (UF.before (\_ -> return ()))
        start

{-# ANN after_ (PermitPatternMatches [''Int]) #-}
{-# ANN after_ (PermitConstructions []) #-}
{-# ANN after_ (PermitTypeClasses []) #-}
{-# NOINLINE after_ #-}
after_ :: Int -> Int -> IO ()
after_ size start =
    drainTransformationDefault (size + start) (UF.after_ (\_ -> return ()))
        start

{-# ANN afterIO (PermitPatternMatches [''Maybe,''Int]) #-}
{-# ANN afterIO (PermitConstructions [''Maybe,''()]) #-}
{-# ANN afterIO (PermitTypeClasses []) #-}
{-# NOINLINE afterIO #-}
afterIO :: Int -> Int -> IO ()
afterIO size start =
    UF.fold FL.drain
        (UF.afterIO (\_ -> return ())
            (UF.supplySecond (size + start) UF.enumerateFromToNum))
        start

{-# ANN finallyIO (PermitPatternMatches [''Maybe,''S.Step]) #-}
{-# ANN finallyIO (PermitConstructions
    [''Int,''SrcLoc,''CallStack,''Maybe,''()]) #-}
{-# ANN finallyIO (PermitTypeClasses [''IP,''MonadCatch]) #-}
{-# NOINLINE finallyIO #-}
finallyIO :: Int -> Int -> IO ()
finallyIO size start =
    UF.fold FL.drain
        (UF.finallyIO (\_ -> return ())
            (UF.supplySecond (size + start) UF.enumerateFromToNum))
        start

{-# ANN bracketIO (PermitPatternMatches [''Maybe,''STRef,''(,),''S.Step]) #-}
{-# ANN bracketIO (PermitConstructions
    [''Int,''SrcLoc,''CallStack,''Maybe,''()]) #-}
{-# ANN bracketIO (PermitTypeClasses [''IP,''MonadCatch]) #-}
{-# NOINLINE bracketIO #-}
bracketIO :: Int -> Int -> IO ()
bracketIO size start =
    UF.fold FL.drain
        (UF.bracketIO return (\_ -> return ())
            (UF.supplySecond (size + start) UF.enumerateFromToNum))
        start

lf :: Word8
lf = fromIntegral (ord '\n')

-- | Split on line feed.
{-# ANN foldMany (PermitPatternMatches
    [''UnsafeEquality,''IO,''Int,''[],''Array]) #-}
{-# ANN foldMany (PermitConstructions
    [''Int,''SrcLoc,''[],''CallStack,''Array]) #-}
{-# ANN foldMany (PermitTypeClasses [''IP]) #-}
{-# NOINLINE foldMany #-}
foldMany :: Handle -> IO Int
foldMany =
    let u = UF.foldMany (FL.takeEndBy_ (== lf) FL.drain) FH.reader
     in UF.fold FL.length u

-- Handle-based fold splitting ('FoldMany' Fuse annotation is disabled so
-- 'Step' is not eliminated, but 'FL.Step' and 'SPEC' should still vanish.)

-------------------------------------------------------------------------------
-- Benchmarks
-------------------------------------------------------------------------------

moduleName :: String
moduleName = "Data.Unfold"

-------------------------------------------------------------------------------
-- Unfold Exception Benchmarks
-------------------------------------------------------------------------------

-- | Send the file contents to /dev/null with exception handling
{-# ANN onException_CopyFileChunks (PermitPatternMatches
    [''UnsafeEquality,''IO,''Int,''[],''Array,''S.Step]) #-}
{-# ANN onException_CopyFileChunks (PermitConstructions
    [''Int,''SrcLoc,''[],''CallStack,''Array]) #-}
{-# ANN onException_CopyFileChunks (PermitTypeClasses [''IP,''MonadCatch]) #-}
{-# NOINLINE onException_CopyFileChunks #-}
onException_CopyFileChunks :: Handle -> Handle -> IO ()
onException_CopyFileChunks inh devNull =
    let readEx = UF.onException (\_ -> hClose inh) FH.chunkReader
    in UF.fold (IFH.writeChunks devNull) readEx inh

-- | Send the file contents to /dev/null with exception handling
{-# ANN bracket__CopyFileChunks (PermitPatternMatches
    [''UnsafeEquality,''IO,''Int,''[],''Array,''S.Step]) #-}
{-# ANN bracket__CopyFileChunks (PermitConstructions
    [''Int,''SrcLoc,''[],''CallStack,''Array]) #-}
{-# ANN bracket__CopyFileChunks (PermitTypeClasses [''IP,''MonadCatch]) #-}
{-# NOINLINE bracket__CopyFileChunks #-}
bracket__CopyFileChunks :: Handle -> Handle -> IO ()
bracket__CopyFileChunks inh devNull =
    let readEx = UF.bracket_ return (\_ -> hClose inh) FH.chunkReader
    in UF.fold (IFH.writeChunks devNull) readEx inh

-- | Send the file contents to /dev/null with exception handling
{-# ANN onException_CopyFileBytes (PermitPatternMatches
    [''UnsafeEquality,''Word8,''IO,''Int,''[],''Array,''ArrayUnsafe
    ,''ConcatState,''S.Step]) #-}
{-# ANN onException_CopyFileBytes (PermitConstructions
    [''Int,''SrcLoc,''[],''CallStack,''Array,''S.Step,''ConcatState
    ,''ArrayUnsafe,''Word8,''()]) #-}
{-# ANN onException_CopyFileBytes (PermitTypeClasses [''IP,''MonadCatch]) #-}
{-# NOINLINE onException_CopyFileBytes #-}
onException_CopyFileBytes :: Handle -> Handle -> IO ()
onException_CopyFileBytes inh devNull =
    let readEx = UF.onException (\_ -> hClose inh) FH.reader
    in S.fold (FH.write devNull) $ S.unfold readEx inh

-- | Send the file contents to /dev/null with exception handling
{-# ANN handle_CopyFileBytes (PermitPatternMatches
    [''Either,''UnsafeEquality,''Word8,''IO,''Int,''[],''Array
    ,''ArrayUnsafe,''ConcatState,''S.Step]) #-}
{-# ANN handle_CopyFileBytes (PermitConstructions
    [''Int,''SrcLoc,''[],''CallStack,''Array,''S.Step,''ConcatState
    ,''ArrayUnsafe,''Word8,''()]) #-}
{-# ANN handle_CopyFileBytes (PermitTypeClasses
    [''IP,''MonadCatch,''Exception]) #-}
{-# NOINLINE handle_CopyFileBytes #-}
handle_CopyFileBytes :: Handle -> Handle -> IO ()
handle_CopyFileBytes inh devNull =
    let handler (_e :: SomeException) = hClose inh >> return 10
        readEx = UF.handle (UF.functionM handler) FH.reader
    in S.fold (FH.write devNull) $ S.unfold readEx inh

-- | Send the file contents to /dev/null with exception handling
{-# ANN finally__CopyFileBytes (PermitPatternMatches
    [''UnsafeEquality,''Word8,''IO,''Int,''[],''Array,''ArrayUnsafe
    ,''ConcatState,''S.Step]) #-}
{-# ANN finally__CopyFileBytes (PermitConstructions
    [''Int,''SrcLoc,''[],''CallStack,''Array,''S.Step,''ConcatState
    ,''ArrayUnsafe,''Word8,''()]) #-}
{-# ANN finally__CopyFileBytes (PermitTypeClasses [''IP,''MonadCatch]) #-}
{-# NOINLINE finally__CopyFileBytes #-}
finally__CopyFileBytes :: Handle -> Handle -> IO ()
finally__CopyFileBytes inh devNull =
    let readEx = UF.finally_ (\_ -> hClose inh) FH.reader
    in S.fold (FH.write devNull) $ S.unfold readEx inh

-- | Send the file contents to /dev/null with exception handling
{-# ANN bracket__CopyFileBytes (PermitPatternMatches
    [''UnsafeEquality,''Word8,''IO,''Int,''[],''Array,''ArrayUnsafe
    ,''ConcatState,''S.Step]) #-}
{-# ANN bracket__CopyFileBytes (PermitConstructions
    [''Int,''SrcLoc,''[],''CallStack,''Array,''S.Step,''ConcatState
    ,''ArrayUnsafe,''Word8,''()]) #-}
{-# ANN bracket__CopyFileBytes (PermitTypeClasses [''IP,''MonadCatch]) #-}
{-# NOINLINE bracket__CopyFileBytes #-}
bracket__CopyFileBytes :: Handle -> Handle -> IO ()
bracket__CopyFileBytes inh devNull =
    let readEx = UF.bracket_ return (\_ -> hClose inh) FH.reader
    in S.fold (FH.write devNull) $ S.unfold readEx inh

benchmarks :: BenchEnv -> Int -> [(SpaceComplexity, Benchmark)]
benchmarks env size =
    -- Basic Constructors
    [ (SpaceO_1, benchIO "nilM" $ nilM size)
    , (SpaceO_1, benchIO "nil" $ nil size)
    , (SpaceO_1, benchIO "consM" $ consM size)
    -- Generators
    , (SpaceO_1, benchIO "repeatM" $ repeatM size)
    , (SpaceO_1, benchIO "repeat" $ repeat size)
    , (SpaceO_1, benchIO "replicateM" $ replicateM size)
    , (SpaceO_1, benchIO "fromIndicesM" $ fromIndicesM size)
    , (SpaceO_1, benchIO "iterateM" $ iterateM size)
    -- From Containers
    , (SpaceO_1, benchIO "fromListM" $ fromListM size)
    -- From Stream
    , (SpaceO_1, benchIO "fromStreamK" $ fromStreamK size)
    , (SpaceO_1, benchIO "fromStreamD" $ fromStreamD size)
    , (SpaceO_1, benchIO "fromStream" $ fromStream size)
    -- Mapping on Input
    , (SpaceO_1, benchIO "discardFirst" $ discardFirst size)
    , (SpaceO_1, benchIO "discardSecond" $ discardSecond size)
    -- Mapping on Output
    , (SpaceO_1, benchIO "postscanl" $ postscanl size)
    , (SpaceO_1, benchIO "scanl" $ scanl size)
    , (SpaceO_1, benchIO "scanlMany" $ scanlMany size)
    , (SpaceO_1, mkBench "foldMany" env
        $ \inh _ -> foldMany inh)
    -- Either Wrapped Input
    , (SpaceO_1, benchIO "either_Left" $ either_Left size)
    -- Filtering
    , (SpaceO_1, benchIO "take" $ take size)
    , (SpaceO_1, benchIO "filter" $ filter size)
    , (SpaceO_1, benchIO "filterM" $ filterM size)
    , (SpaceO_1, benchIO "drop_One" $ drop_One size)
    , (SpaceO_1, benchIO "drop_All" $ drop_All size)
    , (SpaceO_1, benchIO "dropWhile_True" $ dropWhile_True size)
    , (SpaceO_1, benchIO "dropWhile_False" $ dropWhile_False size)
    , (SpaceO_1, benchIO "dropWhileM_True" $ dropWhileM_True size)
    , (SpaceO_1, benchIO "dropWhileM_False" $ dropWhileM_False size)
    , (SpaceO_1, benchIO "mapMaybe" $ mapMaybe size)
    , (SpaceO_1, benchIO "mapMaybeM" $ mapMaybeM size)
    , (SpaceO_1, benchIO "catMaybes" $ catMaybes size)
    -- Cross product
    , (SpaceO_1, benchIO "innerJoin" $ innerJoin size)
    -- Zip
    , (SpaceO_1, benchIO "zipRepeat" $ zipRepeat size)
    -- Resource Management
    , (SpaceO_1, benchIO "before" $ before size)
    , (SpaceO_1, benchIO "afterIO" $ afterIO size)
    , (SpaceO_1, benchIO "after_" $ after_ size)
    , (SpaceO_1, benchIO "finallyIO" $ finallyIO size)
    , (SpaceO_1, mkBenchSmall "finally__CopyFileBytes" env $ \inh _ ->
        finally__CopyFileBytes inh (nullH env))
    , (SpaceO_1, benchIO "bracketIO" $ bracketIO size)
    , (SpaceO_1, mkBenchSmall "bracket__CopyFileBytes" env $ \inh _ ->
        bracket__CopyFileBytes inh (nullH env))
    -- Exceptions
    , (SpaceO_1, mkBenchSmall "onException_CopyFileBytes" env $ \inh _ ->
        onException_CopyFileBytes inh (nullH env))
    , (SpaceO_1, mkBench "onException_CopyFileChunks" env $ \inh _ ->
        onException_CopyFileChunks inh (nullH env))
    , (SpaceO_1, mkBench "bracket__CopyFileChunks" env $ \inh _ ->
        bracket__CopyFileChunks inh (nullH env))
    , (SpaceO_1, mkBenchSmall "handle_CopyFileBytes" env $ \inh _ ->
        handle_CopyFileBytes inh (nullH env))
    ]

-------------------------------------------------------------------------------
-- Driver
-------------------------------------------------------------------------------

main :: IO ()
main = do
    env <- mkHandleBenchEnv
    runWithCLIOpts defaultStreamSize (allBenchmarks env)

    where

    allBenchmarks env size =
        let allBenches =
                  Type.benchmarks size
                  ++ Enumeration.benchmarks size
                  ++ benchmarks env size
            get x = Prelude.map snd $ Prelude.filter ((==) x . fst) allBenches
            o_1_space = get SpaceO_1
            o_n_space = get SpaceO_n
        in
        [ bgroup (o_1_space_prefix moduleName) o_1_space
        , bgroup (o_n_space_prefix moduleName) o_n_space
        ]
