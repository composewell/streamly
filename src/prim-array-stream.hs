import Control.Monad.Primitive (PrimMonad(..))
import Data.Primitive.Types (Prim(..))
import Data.Word (Word8)
import Prelude hiding (length, null, last, map, (!!), read, concat)

import Streamly.Internal.Data.Stream.Serial (SerialT)
import Streamly.Internal.Data.Stream.StreamK.Type (IsStream)

import qualified Streamly.Internal.Prelude as S
import qualified Streamly.Internal.Data.Stream.StreamD as D

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
concat :: (IsStream t, PrimMonad m, Prim a) => t m (Array a) -> t m a
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
concatRev :: (IsStream t, PrimMonad m, Prim a) => t m (Array a) -> t m a
concatRev m = D.fromStreamD $ A.flattenArraysRev (D.toStreamD m)

-- | Flatten a stream of arrays after inserting the given element between
-- arrays.
--
-- /Internal/
{-# INLINE interpose #-}
interpose :: (PrimMonad m, IsStream t, Prim a) => a -> t m (Array a) -> t m a
interpose x = S.interpose x A.read

{-# INLINE intercalateSuffix #-}
intercalateSuffix :: (PrimMonad m, IsStream t, Prim a)
    => Array a -> t m (Array a) -> t m a
intercalateSuffix arr = S.intercalateSuffix arr A.read

-- | Flatten a stream of arrays appending the given element after each
-- array.
--
-- @since 0.7.0
{-# INLINE interposeSuffix #-}
interposeSuffix :: (PrimMonad m, IsStream t, Prim a)
    => a -> t m (Array a) -> t m a
-- interposeSuffix x = D.fromStreamD . A.unlines x . D.toStreamD
interposeSuffix x = S.interposeSuffix x A.read

-- | Split a stream of arrays on a given separator byte, dropping the separator
-- and coalescing all the arrays between two separators into a single array.
--
-- @since 0.7.0
{-# INLINE splitOn #-}
splitOn
    :: (IsStream t, PrimMonad m)
    => Word8
    -> t m (Array Word8)
    -> t m (Array Word8)
splitOn byte s =
    D.fromStreamD $ D.splitInnerBy (A.breakOn byte) A.spliceTwo $ D.toStreamD s

{-# INLINE splitOnSuffix #-}
splitOnSuffix
    :: (IsStream t, PrimMonad m)
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
compact :: (PrimMonad m, Prim a)
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
arraysOf :: (IsStream t, PrimMonad m, Prim a)
    => Int -> t m a -> t m (Array a)
arraysOf n str =
    D.fromStreamD $ A.fromStreamDArraysOf n (D.toStreamD str)

{-# INLINE spliceArraysRealloced #-}
spliceArraysRealloced :: forall m a. (PrimMonad m, Prim a)
    => SerialT m (Array a) -> m (Array a)
spliceArraysRealloced s = S.foldlM' A.spliceTwo A.nil s

-- | Given a stream of arrays, splice them all together to generate a single
-- array. The stream must be /finite/.
--
-- @since 0.7.0
{-# INLINE toArray #-}
toArray :: (PrimMonad m, Prim a) => SerialT m (Array a) -> m (Array a)
toArray = spliceArraysRealloced
-- spliceArrays = _spliceArraysBuffered

-- exponentially increasing sizes of the chunks upto the max limit.
-- XXX this will be easier to implement with parsers/terminating folds
-- With this we should be able to reduce the number of chunks/allocations.
-- The reallocation/copy based toArray can also be implemented using this.
--
{-
{-# INLINE toArraysInRange #-}
toArraysInRange :: (IsStream t, PrimMonad m, Prim a)
    => Int -> Int -> Fold m (Array a) b -> Fold m a b
toArraysInRange low high (Fold step initial extract) =
-}

{-
-- | Fold the input to a pure buffered stream (List) of arrays.
{-# INLINE _toArraysOf #-}
_toArraysOf :: (PrimMonad m, Prim a)
    => Int -> Fold m a (SerialT Identity (Array a))
_toArraysOf n = FL.lchunksOf n (A.writeNF n) FL.toStream
-}
