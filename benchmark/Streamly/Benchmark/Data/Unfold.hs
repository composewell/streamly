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
import Prelude hiding (take, filter, zipWith, map, mapM, takeWhile, scanl, repeat, dropWhile)
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
    drainGeneration (UF.unfoldEach (UF.nilM return) (source (start + value))) start

{-# ANN nil (PermitPatternMatches [''Int]) #-}
{-# ANN nil (PermitConstructions []) #-}
{-# ANN nil (PermitTypeClasses []) #-}
{-# NOINLINE nil #-}
nil :: Int -> Int -> IO ()
nil value start =
    drainGeneration (UF.unfoldEach UF.nil (source (start + value))) start

{-# ANN consM (PermitPatternMatches [''Int,''EnumToState,''S.Step,''UnfoldState]) #-}
{-# ANN consM (PermitConstructions [''UnfoldState,''EnumToState,''Int,''S.Step]) #-}
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

{-# ANN postscan (PermitPatternMatches [''Int]) #-}
{-# ANN postscan (PermitConstructions []) #-}
{-# ANN postscan (PermitTypeClasses []) #-}
{-# NOINLINE postscan #-}
postscan :: Int -> Int -> IO ()
postscan size start =
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
    drainTransformationDefault (size + start) (UF.scanlMany (Scanl.take 2 Scanl.sum)) start

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
{-# ANN dropOne (PermitPatternMatches [''Int]) #-}
{-# ANN dropOne (PermitConstructions []) #-}
{-# ANN dropOne (PermitTypeClasses []) #-}
{-# NOINLINE dropOne #-}
dropOne :: Int -> Int -> IO ()
dropOne value start =
    let outer = UF.map (\i -> (i, i)) (source (start + value `div` 2))
     in drainGeneration (UF.unfoldEach (UF.drop 1 UF.fromTuple) outer) start

{-# ANN dropAll (PermitPatternMatches [''Int]) #-}
{-# ANN dropAll (PermitConstructions []) #-}
{-# ANN dropAll (PermitTypeClasses []) #-}
{-# NOINLINE dropAll #-}
dropAll :: Int -> Int -> IO ()
dropAll size start =
    drainTransformationDefault (size + start) (UF.drop (size + 1)) start

{-# ANN dropWhileTrue (PermitPatternMatches [''Int]) #-}
{-# ANN dropWhileTrue (PermitConstructions []) #-}
{-# ANN dropWhileTrue (PermitTypeClasses []) #-}
{-# NOINLINE dropWhileTrue #-}
dropWhileTrue :: Int -> Int -> IO ()
dropWhileTrue size start =
    drainTransformationDefault
        (size + start)
        (UF.dropWhileM (\_ -> return True))
        start

{-# ANN dropWhileFalse (PermitPatternMatches [''Int]) #-}
{-# ANN dropWhileFalse (PermitConstructions []) #-}
{-# ANN dropWhileFalse (PermitTypeClasses []) #-}
{-# NOINLINE dropWhileFalse #-}
dropWhileFalse :: Int -> Int -> IO ()
dropWhileFalse size start =
    drainTransformationDefault
        (size + start)
        (UF.dropWhileM (\_ -> return False))
        start

{-# ANN dropWhileMTrue (PermitPatternMatches [''Int]) #-}
{-# ANN dropWhileMTrue (PermitConstructions []) #-}
{-# ANN dropWhileMTrue (PermitTypeClasses []) #-}
{-# NOINLINE dropWhileMTrue #-}
dropWhileMTrue :: Int -> Int -> IO ()
dropWhileMTrue size start =
    drainTransformationDefault
        size
        (UF.dropWhileM (\_ -> return True))
        start

{-# ANN dropWhileMFalse (PermitPatternMatches [''Int]) #-}
{-# ANN dropWhileMFalse (PermitConstructions []) #-}
{-# ANN dropWhileMFalse (PermitTypeClasses []) #-}
{-# NOINLINE dropWhileMFalse #-}
dropWhileMFalse :: Int -> Int -> IO ()
dropWhileMFalse size start =
    drainTransformationDefault
        size
        (UF.dropWhileM (\_ -> return False))
        start

{-# ANN dropWhile (PermitPatternMatches [''Int]) #-}
{-# ANN dropWhile (PermitConstructions []) #-}
{-# ANN dropWhile (PermitTypeClasses []) #-}
{-# NOINLINE dropWhile #-}
dropWhile :: Int -> Int -> IO ()
dropWhile size start =
    drainTransformationDefault
        (size + start)
        (UF.dropWhile (\_ -> False))
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
    drainTransformationDefault (size + start) (UF.mapMaybeM (return . Just)) start

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

{-# ANN eitherLeft (PermitPatternMatches [''Int]) #-}
{-# ANN eitherLeft (PermitConstructions []) #-}
{-# ANN eitherLeft (PermitTypeClasses []) #-}
{-# NOINLINE eitherLeft #-}
eitherLeft :: Int -> Int -> IO ()
eitherLeft size start =
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
    drainTransformationDefault (size + start) (UF.before (\_ -> return ())) start

{-# ANN after_ (PermitPatternMatches [''Int]) #-}
{-# ANN after_ (PermitConstructions []) #-}
{-# ANN after_ (PermitTypeClasses []) #-}
{-# NOINLINE after_ #-}
after_ :: Int -> Int -> IO ()
after_ size start =
    drainTransformationDefault (size + start) (UF.after_ (\_ -> return ())) start

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
{-# ANN finallyIO (PermitConstructions [''Int,''SrcLoc,''CallStack,''Maybe,''()]) #-}
{-# ANN finallyIO (PermitTypeClasses [''IP,''MonadCatch]) #-}
{-# NOINLINE finallyIO #-}
finallyIO :: Int -> Int -> IO ()
finallyIO size start =
    UF.fold FL.drain
        (UF.finallyIO (\_ -> return ())
            (UF.supplySecond (size + start) UF.enumerateFromToNum))
        start

{-# ANN bracketIO (PermitPatternMatches [''Maybe,''STRef,''(,),''S.Step]) #-}
{-# ANN bracketIO (PermitConstructions [''Int,''SrcLoc,''CallStack,''Maybe,''()]) #-}
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
{-# ANN foldManySepBy (PermitPatternMatches [''UnsafeEquality,''IO,''Int,''[],''Array]) #-}
{-# ANN foldManySepBy (PermitConstructions [''Int,''SrcLoc,''[],''CallStack,''Array]) #-}
{-# ANN foldManySepBy (PermitTypeClasses [''IP]) #-}
{-# NOINLINE foldManySepBy #-}
foldManySepBy :: Handle -> IO Int
foldManySepBy =
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
{-# ANN readChunksOnException (PermitPatternMatches [''UnsafeEquality,''IO,''Int,''[],''Array,''S.Step]) #-}
{-# ANN readChunksOnException (PermitConstructions
   [''Int,''SrcLoc,''[],''CallStack,''Array]) #-}
{-# ANN readChunksOnException (PermitTypeClasses [''IP,''MonadCatch]) #-}
{-# NOINLINE readChunksOnException #-}
readChunksOnException :: Handle -> Handle -> IO ()
readChunksOnException inh devNull =
    let readEx = UF.onException (\_ -> hClose inh) FH.chunkReader
    in UF.fold (IFH.writeChunks devNull) readEx inh

-- | Send the file contents to /dev/null with exception handling
{-# ANN readChunksBracket_ (PermitPatternMatches [''UnsafeEquality,''IO,''Int,''[],''Array,''S.Step]) #-}
{-# ANN readChunksBracket_ (PermitConstructions
   [''Int,''SrcLoc,''[],''CallStack,''Array]) #-}
{-# ANN readChunksBracket_ (PermitTypeClasses [''IP,''MonadCatch]) #-}
{-# NOINLINE readChunksBracket_ #-}
readChunksBracket_ :: Handle -> Handle -> IO ()
readChunksBracket_ inh devNull =
    let readEx = UF.bracket_ return (\_ -> hClose inh) FH.chunkReader
    in UF.fold (IFH.writeChunks devNull) readEx inh

-- | Send the file contents to /dev/null with exception handling
{-# ANN readWriteOnExceptionUnfold (PermitPatternMatches [''UnsafeEquality,''Word8,''IO,''Int,''[],''Array,''ArrayUnsafe,''ConcatState,''S.Step]) #-}
{-# ANN readWriteOnExceptionUnfold (PermitConstructions
   [''Int,''SrcLoc,''[],''CallStack,''Array,''S.Step,''ConcatState,''ArrayUnsafe,''Word8,''()]) #-}
{-# ANN readWriteOnExceptionUnfold
   (PermitTypeClasses [''IP,''MonadCatch]) #-}
{-# NOINLINE readWriteOnExceptionUnfold #-}
readWriteOnExceptionUnfold :: Handle -> Handle -> IO ()
readWriteOnExceptionUnfold inh devNull =
    let readEx = UF.onException (\_ -> hClose inh) FH.reader
    in S.fold (FH.write devNull) $ S.unfold readEx inh

-- | Send the file contents to /dev/null with exception handling
{-# ANN readWriteHandleExceptionUnfold (PermitPatternMatches [''Either,''UnsafeEquality,''Word8,''IO,''Int,''[],''Array,''ArrayUnsafe,''ConcatState,''S.Step]) #-}
{-# ANN readWriteHandleExceptionUnfold (PermitConstructions
   [''Int,''SrcLoc,''[],''CallStack,''Array,''S.Step,''ConcatState,''ArrayUnsafe,''Word8,''()]) #-}
{-# ANN readWriteHandleExceptionUnfold
   (PermitTypeClasses [''IP,''MonadCatch,''Exception]) #-}
{-# NOINLINE readWriteHandleExceptionUnfold #-}
readWriteHandleExceptionUnfold :: Handle -> Handle -> IO ()
readWriteHandleExceptionUnfold inh devNull =
    let handler (_e :: SomeException) = hClose inh >> return 10
        readEx = UF.handle (UF.functionM handler) FH.reader
    in S.fold (FH.write devNull) $ S.unfold readEx inh

-- | Send the file contents to /dev/null with exception handling
{-# ANN readWriteFinally_Unfold (PermitPatternMatches [''UnsafeEquality,''Word8,''IO,''Int,''[],''Array,''ArrayUnsafe,''ConcatState,''S.Step]) #-}
{-# ANN readWriteFinally_Unfold (PermitConstructions
   [''Int,''SrcLoc,''[],''CallStack,''Array,''S.Step,''ConcatState,''ArrayUnsafe,''Word8,''()]) #-}
{-# ANN readWriteFinally_Unfold (PermitTypeClasses [''IP,''MonadCatch]) #-}
{-# NOINLINE readWriteFinally_Unfold #-}
readWriteFinally_Unfold :: Handle -> Handle -> IO ()
readWriteFinally_Unfold inh devNull =
    let readEx = UF.finally_ (\_ -> hClose inh) FH.reader
    in S.fold (FH.write devNull) $ S.unfold readEx inh

-- | Send the file contents to /dev/null with exception handling
{-# ANN readWriteBracket_Unfold (PermitPatternMatches [''UnsafeEquality,''Word8,''IO,''Int,''[],''Array,''ArrayUnsafe,''ConcatState,''S.Step]) #-}
{-# ANN readWriteBracket_Unfold (PermitConstructions
   [''Int,''SrcLoc,''[],''CallStack,''Array,''S.Step,''ConcatState,''ArrayUnsafe,''Word8,''()]) #-}
{-# ANN readWriteBracket_Unfold (PermitTypeClasses [''IP,''MonadCatch]) #-}
{-# NOINLINE readWriteBracket_Unfold #-}
readWriteBracket_Unfold :: Handle -> Handle -> IO ()
readWriteBracket_Unfold inh devNull =
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
    , (SpaceO_1, benchIO "postscan" $ postscan size)
    , (SpaceO_1, benchIO "scanl" $ scanl size)
    , (SpaceO_1, benchIO "scanlMany" $ scanlMany size)
    , (SpaceO_1, mkBench "foldMany (Fold.takeEndBy_ (== lf) Fold.drain)" env
        $ \inh _ -> foldManySepBy inh)
    -- Either Wrapped Input
    , (SpaceO_1, benchIO "eitherLeft" $ eitherLeft size)
    -- Filtering
    , (SpaceO_1, benchIO "take" $ take size)
    , (SpaceO_1, benchIO "filter" $ filter size)
    , (SpaceO_1, benchIO "filterM" $ filterM size)
    , (SpaceO_1, benchIO "dropOne" $ dropOne size)
    , (SpaceO_1, benchIO "dropAll" $ dropAll size)
    , (SpaceO_1, benchIO "dropWhile" $ dropWhile size)
    , (SpaceO_1, benchIO "dropWhileTrue" $ dropWhileTrue size)
    , (SpaceO_1, benchIO "dropWhileFalse" $ dropWhileFalse size)
    , (SpaceO_1, benchIO "dropWhileMTrue" $ dropWhileMTrue size)
    , (SpaceO_1, benchIO "dropWhileMFalse" $ dropWhileMFalse size)
    , (SpaceO_1, benchIO "mapMaybe" $ mapMaybe size)
    , (SpaceO_1, benchIO "mapMaybeM" $ mapMaybeM size)
    , (SpaceO_1, benchIO "catMaybes" $ catMaybes size)
    -- Cross product
    , (SpaceO_1, benchIO "innerJoin outer=inner=(sqrt Max)" $ innerJoin size)
    -- Zip
    , (SpaceO_1, benchIO "zipRepeat" $ zipRepeat size)
    -- Resource Management
    , (SpaceO_1, benchIO "before" $ before size)
    , (SpaceO_1, benchIO "afterIO" $ afterIO size)
    , (SpaceO_1, benchIO "after_" $ after_ size)
    , (SpaceO_1, benchIO "finallyIO" $ finallyIO size)
    , (SpaceO_1, mkBenchSmall "UF.finally_" env $ \inh _ ->
        readWriteFinally_Unfold inh (nullH env))
    , (SpaceO_1, benchIO "bracketIO" $ bracketIO size)
    , (SpaceO_1, mkBenchSmall "UF.bracket_" env $ \inh _ ->
        readWriteBracket_Unfold inh (nullH env))
    -- Exceptions
    , (SpaceO_1, mkBenchSmall "UF.onException" env $ \inh _ ->
        readWriteOnExceptionUnfold inh (nullH env))
    , (SpaceO_1, mkBench "UF.onException (chunk)" env $ \inh _ ->
        readChunksOnException inh (nullH env))
    , (SpaceO_1, mkBench "UF.bracket_ (chunk)" env $ \inh _ ->
        readChunksBracket_ inh (nullH env))
    , (SpaceO_1, mkBenchSmall "UF.handle" env $ \inh _ ->
        readWriteHandleExceptionUnfold inh (nullH env))
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
