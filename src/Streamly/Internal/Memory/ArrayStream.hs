#include "inline.hs"

-- |
-- Module      : Streamly.Internal.Memory.ArrayStream
-- Copyright   : (c) 2020 Composewell Technologies
--
-- License     : BSD3-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Combinators to efficiently manipulate streams of arrays.
--
module Streamly.Internal.Memory.ArrayStream
    (
    -- * Creation
      arraysOf

    -- * Flattening to elements
    , concat
    , concatRev
    , interpose
    , interposeSuffix
    , intercalateSuffix

    -- * Transformation
    , splitOn
    , splitOnSuffix
    , compact -- compact

    -- * Elimination
    , toArray
    )
where

import Control.Monad.IO.Class (MonadIO(..))
-- import Data.Functor.Identity (Identity)
import Data.Word (Word8)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Ptr (minusPtr, plusPtr, castPtr)
import Foreign.Storable (Storable(..))
import Prelude hiding (length, null, last, map, (!!), read, concat)

import Streamly.Internal.Memory.Array.Types (Array(..), length)
import Streamly.Internal.Data.Stream.Serial (SerialT)
import Streamly.Internal.Data.Stream.StreamK.Type (IsStream)

import qualified Streamly.Internal.Memory.Array as A
import qualified Streamly.Internal.Memory.Array.Types as A
import qualified Streamly.Internal.Data.Array.Storable.Foreign.Mut.Types as MA
import qualified Streamly.Internal.Data.Stream.IsStream as S
import qualified Streamly.Internal.Data.Stream.StreamD as D
import qualified Streamly.Internal.Data.Stream.Prelude as P

-- XXX efficiently compare two streams of arrays. Two streams can have chunks
-- of different sizes, we can handle that in the stream comparison abstraction.
-- This could be useful e.g. to fast compare whether two files differ.

-- | Convert a stream of arrays into a stream of their elements.
--
-- Same as the following but more efficient:
--
-- > concat = S.concatMap A.read
--
-- @since 0.7.0
{-# INLINE concat #-}
concat :: (IsStream t, MonadIO m, Storable a) => t m (Array a) -> t m a
-- concat m = D.fromStreamD $ A.flattenArrays (D.toStreamD m)
-- concat m = D.fromStreamD $ D.concatMap A.toStreamD (D.toStreamD m)
concat m = D.fromStreamD $ D.concatMapU A.read (D.toStreamD m)

-- XXX should we have a reverseArrays API to reverse the stream of arrays
-- instead?
--
-- | Convert a stream of arrays into a stream of their elements reversing the
-- contents of each array before flattening.
--
-- @since 0.7.0
{-# INLINE concatRev #-}
concatRev :: (IsStream t, MonadIO m, Storable a) => t m (Array a) -> t m a
concatRev m = D.fromStreamD $ A.flattenArraysRev (D.toStreamD m)

-- | Flatten a stream of arrays after inserting the given element between
-- arrays.
--
-- /Internal/
{-# INLINE interpose #-}
interpose :: (MonadIO m, IsStream t, Storable a) => a -> t m (Array a) -> t m a
interpose x = S.interpose x A.read

{-# INLINE intercalateSuffix #-}
intercalateSuffix :: (MonadIO m, IsStream t, Storable a)
    => Array a -> t m (Array a) -> t m a
intercalateSuffix arr = S.intercalateSuffix arr A.read

-- | Flatten a stream of arrays appending the given element after each
-- array.
--
-- @since 0.7.0
{-# INLINE interposeSuffix #-}
interposeSuffix :: (MonadIO m, IsStream t, Storable a)
    => a -> t m (Array a) -> t m a
-- interposeSuffix x = D.fromStreamD . A.unlines x . D.toStreamD
interposeSuffix x = S.interposeSuffix x A.read

-- | Split a stream of arrays on a given separator byte, dropping the separator
-- and coalescing all the arrays between two separators into a single array.
--
-- @since 0.7.0
{-# INLINE splitOn #-}
splitOn
    :: (IsStream t, MonadIO m)
    => Word8
    -> t m (Array Word8)
    -> t m (Array Word8)
splitOn byte s =
    D.fromStreamD $ D.splitInnerBy (A.breakOn byte) A.spliceTwo $ D.toStreamD s

{-# INLINE splitOnSuffix #-}
splitOnSuffix
    :: (IsStream t, MonadIO m)
    => Word8
    -> t m (Array Word8)
    -> t m (Array Word8)
-- splitOn byte s = D.fromStreamD $ A.splitOn byte $ D.toStreamD s
splitOnSuffix byte s =
    D.fromStreamD $ D.splitInnerBySuffix (A.breakOn byte) A.spliceTwo $ D.toStreamD s

-- | Coalesce adjacent arrays in incoming stream to form bigger arrays of a
-- maximum specified size in bytes.
--
-- @since 0.7.0
{-# INLINE compact #-}
compact :: (MonadIO m, Storable a)
    => Int -> SerialT m (Array a) -> SerialT m (Array a)
compact n xs = D.fromStreamD $ A.packArraysChunksOf n (D.toStreamD xs)

-- | @arraysOf n stream@ groups the elements in the input stream into arrays of
-- @n@ elements each.
--
-- Same as the following but more efficient:
--
-- > arraysOf n = S.chunksOf n (A.writeN n)
--
-- @since 0.7.0
{-# INLINE arraysOf #-}
arraysOf :: (IsStream t, MonadIO m, Storable a)
    => Int -> t m a -> t m (Array a)
arraysOf n str =
    D.fromStreamD $ A.fromStreamDArraysOf n (D.toStreamD str)

-- XXX Both of these implementations of splicing seem to perform equally well.
-- We need to perform benchmarks over a range of sizes though.

-- CAUTION! length must more than equal to lengths of all the arrays in the
-- stream.
{-# INLINE spliceArraysLenUnsafe #-}
spliceArraysLenUnsafe :: (MonadIO m, Storable a)
    => Int -> SerialT m (MA.Array a) -> m (MA.Array a)
spliceArraysLenUnsafe len buffered = do
    arr <- liftIO $ MA.newArray len
    end <- S.foldlM' writeArr (return $ MA.aEnd arr) buffered
    return $ arr {MA.aEnd = end}

    where

    writeArr dst (MA.Array as ae _) =
        liftIO $ withForeignPtr as $ \src -> do
                        let count = ae `minusPtr` src
                        A.memcpy (castPtr dst) (castPtr src) count
                        return $ dst `plusPtr` count

{-# INLINE _spliceArrays #-}
_spliceArrays :: (MonadIO m, Storable a)
    => SerialT m (Array a) -> m (Array a)
_spliceArrays s = do
    buffered <- P.foldr S.cons S.nil s
    len <- S.sum (S.map length buffered)
    arr <- liftIO $ MA.newArray len
    end <- S.foldlM' writeArr (return $ MA.aEnd arr) s
    return $ A.unsafeFreeze $ arr {MA.aEnd = end}

    where

    writeArr dst (Array as ae) =
        liftIO $ withForeignPtr as $ \src -> do
                        let count = ae `minusPtr` src
                        A.memcpy (castPtr dst) (castPtr src) count
                        return $ dst `plusPtr` count

{-# INLINE _spliceArraysBuffered #-}
_spliceArraysBuffered :: (MonadIO m, Storable a)
    => SerialT m (Array a) -> m (Array a)
_spliceArraysBuffered s = do
    buffered <- P.foldr S.cons S.nil s
    len <- S.sum (S.map length buffered)
    A.unsafeFreeze <$> spliceArraysLenUnsafe len (S.map A.unsafeThaw s)

{-# INLINE spliceArraysRealloced #-}
spliceArraysRealloced :: forall m a. (MonadIO m, Storable a)
    => SerialT m (Array a) -> m (Array a)
spliceArraysRealloced s = do
    let idst = liftIO $ MA.newArray (A.bytesToElemCount (undefined :: a)
                                  (A.mkChunkSizeKB 4))

    arr <- S.foldlM' MA.spliceWithDoubling idst (S.map A.unsafeThaw s)
    liftIO $ A.unsafeFreeze <$> MA.shrinkToFit arr

-- | Given a stream of arrays, splice them all together to generate a single
-- array. The stream must be /finite/.
--
-- @since 0.7.0
{-# INLINE toArray #-}
toArray :: (MonadIO m, Storable a) => SerialT m (Array a) -> m (Array a)
toArray = spliceArraysRealloced
-- spliceArrays = _spliceArraysBuffered

-- exponentially increasing sizes of the chunks upto the max limit.
-- XXX this will be easier to implement with parsers/terminating folds
-- With this we should be able to reduce the number of chunks/allocations.
-- The reallocation/copy based toArray can also be implemented using this.
--
{-
{-# INLINE toArraysInRange #-}
toArraysInRange :: (IsStream t, MonadIO m, Storable a)
    => Int -> Int -> Fold m (Array a) b -> Fold m a b
toArraysInRange low high (Fold step initial extract) =
-}

{-
-- | Fold the input to a pure buffered stream (List) of arrays.
{-# INLINE _toArraysOf #-}
_toArraysOf :: (MonadIO m, Storable a)
    => Int -> Fold m a (SerialT Identity (Array a))
_toArraysOf n = FL.lchunksOf n (A.writeNF n) FL.toStream
-}
