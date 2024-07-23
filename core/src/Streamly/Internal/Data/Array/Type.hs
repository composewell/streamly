{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}
-- Must come after TypeFamilies, otherwise it is re-enabled.
-- MonoLocalBinds enabled by TypeFamilies causes perf regressions in general.
{-# LANGUAGE NoMonoLocalBinds #-}
-- |
-- Module      : Streamly.Internal.Data.Array.Type
-- Copyright   : (c) 2020 Composewell Technologies
-- License     : BSD3-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- See notes in "Streamly.Internal.Data.MutArray.Type"
--

module Streamly.Internal.Data.Array.Type
    (
    -- ** Type
    -- $arrayNotes
      Array (..)

    -- ** Conversion
    -- *** Mutable and Immutable
    , unsafeFreeze
    , unsafeFreezeWithShrink
    , unsafeThaw

    -- *** Pinned and Unpinned
    , pin
    , unpin
    , isPinned

    -- *** Casting
    , unsafePinnedAsPtr
    , unsafeAsForeignPtr

    -- ** Construction
    , empty

    -- *** Slicing
    -- | Get a subarray without copying
    , splitAt
    , breakOn -- XXX requires MonadIO

    -- *** Stream Folds
    , unsafeMakePure
    , createOf
    , pinnedCreateOf
    , unsafeCreateOf
    , unsafePinnedCreateOf
    , create
    , pinnedCreate
    , createWith

    -- *** From containers
    , fromListN
    , pinnedFromListN
    , fromList
    , pinnedFromList
    , fromListRevN
    , fromListRev
    , fromStreamN
    , fromStream
    , fromPureStreamN
    , fromPureStream
    , fromCString#
    , fromCString
    , fromW16CString#
    , fromW16CString
    , fromPtrN
    , fromChunks
    , fromChunksK

    -- ** Reading

    -- *** Indexing
    , unsafeIndexIO -- XXX unsafeGetIndexIO
    , getIndexUnsafe -- XXX unsafeGetIndex

    -- *** To Streams
    , read
    , readRev
    , toStreamK
    , toStreamKRev

    -- *** To Containers
    , toList

    -- *** Unfolds
    , producer -- experimental
    , readerUnsafe
    , reader
    , readerRev

    -- *** Size
    , length
    , byteLength

    -- ** Folding
    , foldl'
    , foldr
    , byteCmp
    , byteEq

    -- ** Appending
    , splice -- XXX requires MonadIO

    -- ** Streams of arrays
    -- *** Chunk
    -- | Group a stream into arrays.
    , chunksOf
    , pinnedChunksOf
    , buildChunks

    -- *** Split
    -- | Split an array into slices.

    -- *** Concat
    -- | Append the arrays in a stream to form a stream of elements.
    , concat
    , concatRev

    -- *** Compact
    -- | Append the arrays in a stream to form a stream of larger arrays.
    , fCompactGE
    , fPinnedCompactGE
    , lCompactGE
    , lPinnedCompactGE
    , compactGE

    -- ** Deprecated
    , asPtrUnsafe
    , unsafeIndex
    , bufferChunks
    , flattenArrays
    , flattenArraysRev
    , fromArrayStreamK
    , fromStreamDN
    , fromStreamD
    , toStreamD
    , toStreamDRev
    , toStream
    , toStreamRev
    , nil
    , writeWith
    , writeN
    , pinnedWriteN
    , writeNUnsafe
    , pinnedWriteNUnsafe
    , pinnedWriteNAligned
    , write
    , pinnedWrite
    , fromByteStr#
    , fromByteStr
    )
where

#include "ArrayMacros.h"
#include "inline.hs"

import Control.Exception (assert)
import Control.Monad (replicateM, when)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Functor.Identity (Identity(..))
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Proxy (Proxy(..))
import Data.Word (Word8, Word16, Word32, Word64)
import GHC.Base (build)
import GHC.Exts (IsList, IsString(..), Addr#)
import GHC.ForeignPtr (ForeignPtr(..), ForeignPtrContents(..))

import GHC.IO (unsafePerformIO)
import GHC.Ptr (Ptr(..))
import Streamly.Internal.Data.Producer.Type (Producer(..))
import Streamly.Internal.Data.MutArray.Type (MutArray(..))
import Streamly.Internal.Data.MutByteArray.Type (MutByteArray)
import Streamly.Internal.Data.Fold.Type (Fold(..))
import Streamly.Internal.Data.Stream.Type (Stream)
import Streamly.Internal.Data.StreamK.Type (StreamK)
import Streamly.Internal.Data.Unbox (Unbox(..))
import Streamly.Internal.Data.Unfold.Type (Unfold(..))
import Text.Read (readPrec)

import Prelude hiding (Foldable(..), concat, read, unlines, splitAt)

import qualified GHC.Exts as Exts
import qualified Streamly.Internal.Data.Fold.Type as Fold
import qualified Streamly.Internal.Data.MutArray.Type as MA
import qualified Streamly.Internal.Data.Stream.Type as D
import qualified Streamly.Internal.Data.StreamK.Type as K
import qualified Streamly.Internal.Data.MutByteArray.Type as Unboxed
import qualified Streamly.Internal.Data.Producer as Producer
import qualified Streamly.Internal.Data.Unfold.Type as Unfold
import qualified Text.ParserCombinators.ReadPrec as ReadPrec

import Streamly.Internal.System.IO (unsafeInlineIO, defaultChunkSize)

#include "DocTestDataArray.hs"

-------------------------------------------------------------------------------
-- Notes
-------------------------------------------------------------------------------

-- IMPORTANT:

-- We need to be careful while using unsafePerformIO when array creation is
-- involved.
--
-- * We need to make sure the unsafe IO line does not float out of the binding.
-- * The order of the IO actions should be sane. For example, `touch` after `f`.
--
-- Assume the unsafe IO action floats up. If it makes sense given this
-- assumption, it's probably OK to use usafe IO.
--
-- A general approach should be never to use unsafe IO where Array creation is
-- involved or touch is involved.

-------------------------------------------------------------------------------
-- Array Data Type
-------------------------------------------------------------------------------

-- $arrayNotes
--
-- We can use an 'Unbox' constraint in the Array type and the constraint can
-- be automatically provided to a function that pattern matches on the Array
-- type. However, it has huge performance cost, so we do not use it.
-- Investigate a GHC improvement possiblity.
--
data Array a =
#ifdef DEVBUILD
    Unbox a =>
#endif
    -- All offsets are in terms of bytes from the start of arraycontents
    Array
    { arrContents :: {-# UNPACK #-} !MutByteArray
    , arrStart :: {-# UNPACK #-} !Int -- offset
    , arrEnd   :: {-# UNPACK #-} !Int   -- offset + len
    }

-------------------------------------------------------------------------------
-- Utility functions
-------------------------------------------------------------------------------

-- | Use an @Array a@ as @Ptr a@.
--
-- See 'MA.unsafePinnedAsPtr' in the Mutable array module for more details.
--
-- /Unsafe/
--
-- /Pre-release/
--
{-# INLINE unsafePinnedAsPtr #-}
unsafePinnedAsPtr :: MonadIO m => Array a -> (Ptr a -> Int -> m b) -> m b
unsafePinnedAsPtr arr = MA.unsafePinnedAsPtr (unsafeThaw arr)

-- | Use an @Array a@ as @ForeignPtr a@.
--
-- /Unsafe/ because of direct pointer operations. The user must ensure that they
-- are writing within the legal bounds of the array.
--
-- /Pre-release/
--
{-# INLINE unsafeAsForeignPtr #-}
unsafeAsForeignPtr
    :: MonadIO m => Array a -> (ForeignPtr a -> Int -> m b) -> m b
unsafeAsForeignPtr arr@Array{..} f =
    unsafePinnedAsPtr arr finner
    where
    finner (Ptr addr#) i =
        let fptrContents =
                PlainPtr (Unboxed.getMutByteArray# arrContents)
            fptr = ForeignPtr addr# fptrContents
         in f fptr i

{-# DEPRECATED asPtrUnsafe "Please use unsafePinnedAsPtr instead." #-}
{-# INLINE asPtrUnsafe #-}
asPtrUnsafe :: MonadIO m => Array a -> (Ptr a -> m b) -> m b
asPtrUnsafe arr f = unsafePinnedAsPtr arr (\p _ -> f p)

-------------------------------------------------------------------------------
-- Freezing and Thawing
-------------------------------------------------------------------------------

-- XXX For debugging we can track slices/references through a weak IORef.  Then
-- trigger a GC after freeze/thaw and assert that there are no references
-- remaining.

-- | Makes an immutable array using the underlying memory of the mutable
-- array.
--
-- Please make sure that there are no other references to the mutable array
-- lying around, so that it is never used after freezing it using
-- /unsafeFreeze/.  If the underlying array is mutated, the immutable promise
-- is lost.
--
-- /Pre-release/
{-# INLINE unsafeFreeze #-}
unsafeFreeze :: MutArray a -> Array a
unsafeFreeze (MutArray ac as ae _) = Array ac as ae

-- | Similar to 'unsafeFreeze' but uses 'MA.rightSize' on the mutable array
-- first.
{-# INLINE unsafeFreezeWithShrink #-}
unsafeFreezeWithShrink :: Unbox a => MutArray a -> Array a
unsafeFreezeWithShrink arr = unsafePerformIO $ do
  MutArray ac as ae _ <- MA.rightSize arr
  return $ Array ac as ae

-- | Makes a mutable array using the underlying memory of the immutable array.
--
-- Please make sure that there are no other references to the immutable array
-- lying around, so that it is never used after thawing it using /unsafeThaw/.
-- If the resulting array is mutated, any references to the older immutable
-- array are mutated as well.
--
-- /Pre-release/
{-# INLINE unsafeThaw #-}
unsafeThaw :: Array a -> MutArray a
unsafeThaw (Array ac as ae) = MutArray ac as ae ae

-------------------------------------------------------------------------------
-- Pinning & Unpinning
-------------------------------------------------------------------------------

-- | Return a copy of the 'Array' in pinned memory if unpinned, else return the
-- original array.
{-# INLINE pin #-}
pin :: Array a -> IO (Array a)
pin = fmap unsafeFreeze . MA.pin . unsafeThaw

-- | Return a copy of the 'Array' in unpinned memory if pinned, else return the
-- original array.
{-# INLINE unpin #-}
unpin :: Array a -> IO (Array a)
unpin = fmap unsafeFreeze . MA.unpin . unsafeThaw

-- | Return 'True' if the array is allocated in pinned memory.
{-# INLINE isPinned #-}
isPinned :: Array a -> Bool
isPinned = MA.isPinned . unsafeThaw

-------------------------------------------------------------------------------
-- Construction
-------------------------------------------------------------------------------

-- | Copy two immutable arrays into a new array. If you want to splice more
-- than two arrays then this operation would be highly inefficient because it
-- would make a copy on every splice operation, instead use the
-- 'fromChunksK' operation to combine n immutable arrays.
{-# INLINE splice #-}
splice :: (MonadIO m
#ifdef DEVBUILD
    , Unbox a
#endif
    )
    => Array a -> Array a -> m (Array a)
splice arr1 arr2 =
    unsafeFreeze <$> MA.spliceCopy (unsafeThaw arr1) (unsafeThaw arr2)

-- | Create an 'Array' from the first N elements of a list. The array is
-- allocated to size N, if the list terminates before N elements then the
-- array may hold less than N elements.
--
{-# INLINABLE fromListN #-}
fromListN :: Unbox a => Int -> [a] -> Array a
fromListN n xs = unsafePerformIO $ unsafeFreeze <$> MA.fromListN n xs

-- | Like 'fromListN' but creates a pinned array.
{-# INLINABLE pinnedFromListN #-}
pinnedFromListN :: Unbox a => Int -> [a] -> Array a
pinnedFromListN n xs =
    unsafePerformIO $ unsafeFreeze <$> MA.pinnedFromListN n xs

-- | Create an 'Array' from the first N elements of a list in reverse order.
-- The array is allocated to size N, if the list terminates before N elements
-- then the array may hold less than N elements.
--
-- /Pre-release/
{-# INLINABLE fromListRevN #-}
fromListRevN :: Unbox a => Int -> [a] -> Array a
fromListRevN n xs = unsafePerformIO $ unsafeFreeze <$> MA.fromListRevN n xs

-- | Create an 'Array' from a list. The list must be of finite size.
--
{-# INLINE fromList #-}
fromList :: Unbox a => [a] -> Array a
fromList xs = unsafePerformIO $ unsafeFreeze <$> MA.fromList xs

-- | Like 'fromList' but creates a pinned array.
{-# INLINE pinnedFromList #-}
pinnedFromList :: Unbox a => [a] -> Array a
pinnedFromList xs = unsafePerformIO $ unsafeFreeze <$> MA.pinnedFromList xs

-- | Create an 'Array' from a list in reverse order. The list must be of finite
-- size.
--
-- /Pre-release/
{-# INLINABLE fromListRev #-}
fromListRev :: Unbox a => [a] -> Array a
fromListRev xs = unsafePerformIO $ unsafeFreeze <$> MA.fromListRev xs

-- | Create an 'Array' from the first N elements of a stream. The array is
-- allocated to size N, if the stream terminates before N elements then the
-- array may hold less than N elements.
--
-- >>> fromStreamN n = Stream.fold (Array.writeN n)
--
-- /Pre-release/
{-# INLINE_NORMAL fromStreamN #-}
fromStreamN :: (MonadIO m, Unbox a) => Int -> Stream m a -> m (Array a)
fromStreamN n m = do
    when (n < 0) $ error "writeN: negative write count specified"
    unsafeFreeze <$> MA.fromStreamN n m
-- fromStreamN n = D.fold (writeN n)

{-# DEPRECATED fromStreamDN "Please use fromStreamN instead." #-}
fromStreamDN :: forall m a. (MonadIO m, Unbox a)
    => Int -> D.Stream m a -> m (Array a)
fromStreamDN = fromStreamN

-- | Create an 'Array' from a stream. This is useful when we want to create a
-- single array from a stream of unknown size. 'writeN' is at least twice
-- as efficient when the size is already known.
--
-- >>> fromStream = Stream.fold Array.write
--
-- Note that if the input stream is too large memory allocation for the array
-- may fail.  When the stream size is not known, `chunksOf` followed by
-- processing of indvidual arrays in the resulting stream should be preferred.
--
-- /Pre-release/
{-# INLINE_NORMAL fromStreamD #-}
fromStream :: (MonadIO m, Unbox a) => Stream m a -> m (Array a)
fromStream = D.fold write
-- fromStreamD str = unsafeFreeze <$> MA.fromStream str

{-# DEPRECATED fromStreamD "Please use fromStream instead." #-}
fromStreamD :: forall m a. (MonadIO m, Unbox a)
    => D.Stream m a -> m (Array a)
fromStreamD = fromStream

-------------------------------------------------------------------------------
-- Streams of arrays
-------------------------------------------------------------------------------

{-# INLINE buildChunks #-}
buildChunks :: (MonadIO m, Unbox a) =>
    D.Stream m a -> m (K.StreamK m (Array a))
buildChunks m = D.foldr K.cons K.nil $ chunksOf defaultChunkSize m

{-# DEPRECATED bufferChunks "Please use buildChunks instead." #-}
bufferChunks :: (MonadIO m, Unbox a) =>
    D.Stream m a -> m (K.StreamK m (Array a))
bufferChunks = buildChunks

-- | @chunksOf n stream@ groups the elements in the input stream into arrays of
-- @n@ elements each.
--
-- Same as the following but may be more efficient:
--
-- >>> chunksOf n = Stream.foldMany (Array.writeN n)
--
-- /Pre-release/
{-# INLINE_NORMAL chunksOf #-}
chunksOf :: forall m a. (MonadIO m, Unbox a)
    => Int -> D.Stream m a -> D.Stream m (Array a)
chunksOf n str = D.map unsafeFreeze $ MA.chunksOf n str

-- | Like 'chunksOf' but creates pinned arrays.
{-# INLINE_NORMAL pinnedChunksOf #-}
pinnedChunksOf :: forall m a. (MonadIO m, Unbox a)
    => Int -> D.Stream m a -> D.Stream m (Array a)
pinnedChunksOf n str = D.map unsafeFreeze $ MA.pinnedChunksOf n str

-- | Convert a stream of arrays into a stream of their elements.
--
-- >>> concat = Stream.unfoldMany Array.reader
--
{-# INLINE_NORMAL concat #-}
concat :: (Monad m, Unbox a) => Stream m (Array a) -> Stream m a
-- XXX this requires MonadIO whereas the unfoldMany version does not
concat = MA.concatWith (pure . unsafeInlineIO) . D.map unsafeThaw
-- concat = D.unfoldMany reader

{-# DEPRECATED flattenArrays "Please use \"unfoldMany reader\" instead." #-}
{-# INLINE flattenArrays #-}
flattenArrays :: forall m a. (MonadIO m, Unbox a)
    => D.Stream m (Array a) -> D.Stream m a
flattenArrays = concat

-- | Convert a stream of arrays into a stream of their elements reversing the
-- contents of each array before flattening.
--
-- >>> concatRev = Stream.unfoldMany Array.readerRev
--
{-# INLINE_NORMAL concatRev #-}
concatRev :: forall m a. (Monad m, Unbox a)
    => D.Stream m (Array a) -> D.Stream m a
-- XXX this requires MonadIO whereas the unfoldMany version does not
concatRev = MA.concatRevWith (pure . unsafeInlineIO) . D.map unsafeThaw
-- concatRev = D.unfoldMany readerRev

{-# DEPRECATED flattenArraysRev "Please use \"unfoldMany readerRev\" instead." #-}
{-# INLINE flattenArraysRev #-}
flattenArraysRev :: forall m a. (MonadIO m, Unbox a)
    => D.Stream m (Array a) -> D.Stream m a
flattenArraysRev = concatRev

-------------------------------------------------------------------------------
-- Compact
-------------------------------------------------------------------------------

-- XXX Note that this thaws immutable arrays for appending, that may be
-- problematic if multiple users do the same thing, however, thawed immutable
-- arrays would have no capacity to append, therefore, a copy will be forced
-- anyway.

-- | Fold @fCompactGE n@ coalesces adjacent arrays in the input stream
-- until the size becomes greater than or equal to n.
--
-- Generates unpinned arrays irrespective of the pinning status of input
-- arrays.
{-# INLINE_NORMAL fCompactGE #-}
fCompactGE :: (MonadIO m, Unbox a) => Int -> Fold m (Array a) (Array a)
fCompactGE n = fmap unsafeFreeze $ Fold.lmap unsafeThaw $ MA.fCompactGE n

-- | PInned version of 'fCompactGE'.
{-# INLINE_NORMAL fPinnedCompactGE #-}
fPinnedCompactGE :: (MonadIO m, Unbox a) => Int -> Fold m (Array a) (Array a)
fPinnedCompactGE n =
    fmap unsafeFreeze $ Fold.lmap unsafeThaw $ MA.fPinnedCompactGE n

-- | @compactGE n stream@ coalesces adjacent arrays in the @stream@ until
-- the size becomes greater than or equal to @n@.
--
-- >>> compactGE n = Stream.foldMany (Array.fCompactGE n)
--
-- Generates unpinned arrays irrespective of the pinning status of input
-- arrays.
{-# INLINE compactGE #-}
compactGE ::
       (MonadIO m, Unbox a)
    => Int -> Stream m (Array a) -> Stream m (Array a)
compactGE n stream =
    D.map unsafeFreeze $ MA.compactGE n $ D.map unsafeThaw stream

-- | Like 'compactGE' but for transforming folds instead of stream.
--
-- >>> lCompactGE n = Fold.many (Array.fCompactGE n)
--
-- Generates unpinned arrays irrespective of the pinning status of input
-- arrays.
{-# INLINE_NORMAL lCompactGE #-}
lCompactGE :: (MonadIO m, Unbox a)
    => Int -> Fold m (Array a) () -> Fold m (Array a) ()
lCompactGE n fld =
    Fold.lmap unsafeThaw $ MA.lCompactGE n (Fold.lmap unsafeFreeze fld)

-- | Pinned version of 'lCompactGE'.
{-# INLINE_NORMAL lPinnedCompactGE #-}
lPinnedCompactGE :: (MonadIO m, Unbox a)
    => Int -> Fold m (Array a) () -> Fold m (Array a) ()
lPinnedCompactGE n fld =
    Fold.lmap unsafeThaw $ MA.lPinnedCompactGE n (Fold.lmap unsafeFreeze fld)

-------------------------------------------------------------------------------
-- Splitting
-------------------------------------------------------------------------------

-- Drops the separator byte
{-# INLINE breakOn #-}
breakOn :: MonadIO m
    => Word8 -> Array Word8 -> m (Array Word8, Maybe (Array Word8))
breakOn sep arr = do
  (a, b) <- MA.breakOn sep (unsafeThaw arr)
  return (unsafeFreeze a, unsafeFreeze <$> b)

-------------------------------------------------------------------------------
-- Elimination
-------------------------------------------------------------------------------

-- | Return element at the specified index without checking the bounds.
--
-- Unsafe because it does not check the bounds of the array.
{-# INLINE_NORMAL unsafeIndexIO #-}
unsafeIndexIO :: forall a. Unbox a => Int -> Array a -> IO a
unsafeIndexIO i arr = MA.unsafeGetIndex i (unsafeThaw arr)

-- | Return element at the specified index without checking the bounds.
{-# INLINE_NORMAL getIndexUnsafe #-}
getIndexUnsafe :: forall a. Unbox a => Int -> Array a -> a
getIndexUnsafe i arr = let !r = unsafeInlineIO $ unsafeIndexIO i arr in r

{-# DEPRECATED unsafeIndex "Please use 'getIndexUnsafe' instead" #-}
{-# INLINE_NORMAL unsafeIndex #-}
unsafeIndex :: forall a. Unbox a => Int -> Array a -> a
unsafeIndex = getIndexUnsafe

-- | /O(1)/ Get the byte length of the array.
--
{-# INLINE byteLength #-}
byteLength :: Array a -> Int
byteLength = MA.byteLength . unsafeThaw

-- | /O(1)/ Get the length of the array i.e. the number of elements in the
-- array.
--
{-# INLINE length #-}
length :: Unbox a => Array a -> Int
length arr = MA.length (unsafeThaw arr)

{-# INLINE_NORMAL producer #-}
producer :: forall m a. (Monad m, Unbox a) => Producer m (Array a) a
producer =
    Producer.translate unsafeThaw unsafeFreeze
        $ MA.producerWith (return . unsafeInlineIO)

-- | Unfold an array into a stream.
--
{-# INLINE_NORMAL reader #-}
reader :: forall m a. (Monad m, Unbox a) => Unfold m (Array a) a
reader = Producer.simplify producer

-- | Unfold an array into a stream, does not check the end of the array, the
-- user is responsible for terminating the stream within the array bounds. For
-- high performance application where the end condition can be determined by
-- a terminating fold.
--
-- Written in the hope that it may be faster than "read", however, in the case
-- for which this was written, "read" proves to be faster even though the core
-- generated with unsafeRead looks simpler.
--
-- /Pre-release/
--
{-# INLINE_NORMAL readerUnsafe #-}
readerUnsafe :: forall m a. (Monad m, Unbox a) => Unfold m (Array a) a
readerUnsafe = Unfold step inject
    where

    inject (Array contents start end) =
        return (MA.ArrayUnsafe contents end start)

    {-# INLINE_LATE step #-}
    step (MA.ArrayUnsafe contents end p) = do
            -- unsafeInlineIO allows us to run this in Identity monad for pure
            -- toList/foldr case which makes them much faster due to not
            -- accumulating the list and fusing better with the pure consumers.
            --
            -- This should be safe as the array contents are guaranteed to be
            -- evaluated/written to before we peek at them.
            let !x = unsafeInlineIO $ peekAt p contents
            let !p1 = INDEX_NEXT(p,a)
            return $ D.Yield x (MA.ArrayUnsafe contents end p1)

-- | Unfold an array into a stream in reverse order.
--
{-# INLINE_NORMAL readerRev #-}
readerRev :: forall m a. (Monad m, Unbox a) => Unfold m (Array a) a
readerRev = Unfold.lmap unsafeThaw $ MA.readerRevWith (return . unsafeInlineIO)

{-# DEPRECATED toStreamD "Please use 'read' instead." #-}
{-# INLINE_NORMAL toStreamD #-}
toStreamD :: forall m a. (Monad m, Unbox a) => Array a -> D.Stream m a
toStreamD = read

{-# INLINE toStreamK #-}
toStreamK :: forall m a. (Monad m, Unbox a) => Array a -> K.StreamK m a
toStreamK arr = MA.toStreamKWith (return . unsafeInlineIO) (unsafeThaw arr)

{-# DEPRECATED toStreamDRev "Please use 'readRev' instead." #-}
{-# INLINE_NORMAL toStreamDRev #-}
toStreamDRev :: forall m a. (Monad m, Unbox a) => Array a -> D.Stream m a
toStreamDRev = readRev

{-# INLINE toStreamKRev #-}
toStreamKRev :: forall m a. (Monad m, Unbox a) => Array a -> K.StreamK m a
toStreamKRev arr =
    MA.toStreamKRevWith (return . unsafeInlineIO) (unsafeThaw arr)

-- | Convert an 'Array' into a stream.
--
-- /Pre-release/
{-# INLINE_EARLY read #-}
read :: (Monad m, Unbox a) => Array a -> Stream m a
read arr = MA.toStreamWith (return . unsafeInlineIO) (unsafeThaw arr)

-- | Same as 'read'
--
{-# DEPRECATED toStream "Please use 'read' instead." #-}
{-# INLINE_EARLY toStream #-}
toStream :: (Monad m, Unbox a) => Array a -> Stream m a
toStream = read
-- XXX add fallback to StreamK rule
-- {-# RULES "Streamly.Array.read fallback to StreamK" [1]
--     forall a. S.readK (read a) = K.fromArray a #-}

-- | Convert an 'Array' into a stream in reverse order.
--
-- /Pre-release/
{-# INLINE_EARLY readRev #-}
readRev :: (Monad m, Unbox a) => Array a -> Stream m a
readRev arr = MA.toStreamRevWith (return . unsafeInlineIO) (unsafeThaw arr)

-- | Same as 'readRev'
--
{-# DEPRECATED toStreamRev "Please use 'readRev' instead." #-}
{-# INLINE_EARLY toStreamRev #-}
toStreamRev :: (Monad m, Unbox a) => Array a -> Stream m a
toStreamRev = readRev

-- XXX add fallback to StreamK rule
-- {-# RULES "Streamly.Array.readRev fallback to StreamK" [1]
--     forall a. S.toStreamK (readRev a) = K.revFromArray a #-}

{-# INLINE_NORMAL foldl' #-}
foldl' :: forall a b. Unbox a => (b -> a -> b) -> b -> Array a -> b
foldl' f z arr = runIdentity $ D.foldl' f z $ toStreamD arr

{-# INLINE_NORMAL foldr #-}
foldr :: Unbox a => (a -> b -> b) -> b -> Array a -> b
foldr f z arr = runIdentity $ D.foldr f z $ toStreamD arr

-- | Create two slices of an array without copying the original array. The
-- specified index @i@ is the first index of the second slice.
--
{-# INLINE splitAt #-}
splitAt :: Unbox a => Int -> Array a -> (Array a, Array a)
splitAt i arr = (unsafeFreeze a, unsafeFreeze b)
  where
    (a, b) = MA.splitAt i (unsafeThaw arr)

-- Use foldr/build fusion to fuse with list consumers
-- This can be useful when using the IsList instance
{-# INLINE_LATE toListFB #-}
toListFB :: forall a b. Unbox a => (a -> b -> b) -> b -> Array a -> b
toListFB c n Array{..} = go arrStart
    where

    go p | assert (p <= arrEnd) (p == arrEnd) = n
    go p =
        -- unsafeInlineIO allows us to run this in Identity monad for pure
        -- toList/foldr case which makes them much faster due to not
        -- accumulating the list and fusing better with the pure consumers.
        --
        -- This should be safe as the array contents are guaranteed to be
        -- evaluated/written to before we peekAt at them.
        let !x = unsafeInlineIO $ peekAt p arrContents
        in c x (go (INDEX_NEXT(p,a)))

-- | Convert an 'Array' into a list.
--
{-# INLINE toList #-}
toList :: Unbox a => Array a -> [a]
toList s = build (\c n -> toListFB c n s)

-------------------------------------------------------------------------------
-- Folds
-------------------------------------------------------------------------------

-- | @createOf n@ folds a maximum of @n@ elements from the input stream to an
-- 'Array'.
--
{-# INLINE_NORMAL createOf #-}
createOf :: forall m a. (MonadIO m, Unbox a) => Int -> Fold m a (Array a)
createOf = fmap unsafeFreeze . MA.createOf

{-# DEPRECATED writeN  "Please use createOf instead." #-}
{-# INLINE writeN #-}
writeN :: forall m a. (MonadIO m, Unbox a) => Int -> Fold m a (Array a)
writeN = createOf

-- | Like 'createOf' but creates a pinned array.
{-# INLINE_NORMAL pinnedCreateOf #-}
pinnedCreateOf :: forall m a. (MonadIO m, Unbox a) => Int -> Fold m a (Array a)
pinnedCreateOf = fmap unsafeFreeze . MA.pinnedCreateOf

{-# DEPRECATED pinnedWriteN  "Please use pinnedCreateOf instead." #-}
{-# INLINE pinnedWriteN #-}
pinnedWriteN :: forall m a. (MonadIO m, Unbox a) => Int -> Fold m a (Array a)
pinnedWriteN = pinnedCreateOf

-- | @pinnedWriteNAligned alignment n@ folds a maximum of @n@ elements from the input
-- stream to an 'Array' aligned to the given size.
--
-- /Pre-release/
--
{-# INLINE_NORMAL pinnedWriteNAligned #-}
{-# DEPRECATED pinnedWriteNAligned  "To be removed." #-}
pinnedWriteNAligned :: forall m a. (MonadIO m, Unbox a)
    => Int -> Int -> Fold m a (Array a)
pinnedWriteNAligned alignSize = fmap unsafeFreeze . MA.pinnedWriteNAligned alignSize

-- | Like 'createOf' but does not check the array bounds when writing. The fold
-- driver must not call the step function more than 'n' times otherwise it will
-- corrupt the memory and crash. This function exists mainly because any
-- conditional in the step function blocks fusion causing 10x performance
-- slowdown.
--
{-# INLINE_NORMAL unsafeCreateOf #-}
unsafeCreateOf :: forall m a. (MonadIO m, Unbox a)
    => Int -> Fold m a (Array a)
unsafeCreateOf n = unsafeFreeze <$> MA.unsafeCreateOf n

{-# DEPRECATED writeNUnsafe  "Please use unsafeCreateOf instead." #-}
{-# INLINE writeNUnsafe #-}
writeNUnsafe :: forall m a. (MonadIO m, Unbox a)
    => Int -> Fold m a (Array a)
writeNUnsafe = unsafeCreateOf

{-# INLINE_NORMAL unsafePinnedCreateOf #-}
unsafePinnedCreateOf :: forall m a. (MonadIO m, Unbox a)
    => Int -> Fold m a (Array a)
unsafePinnedCreateOf n = unsafeFreeze <$> MA.unsafePinnedCreateOf n

{-# DEPRECATED pinnedWriteNUnsafe  "Please use unsafePinnedCreateOf instead." #-}
{-# INLINE pinnedWriteNUnsafe #-}
pinnedWriteNUnsafe :: forall m a. (MonadIO m, Unbox a)
    => Int -> Fold m a (Array a)
pinnedWriteNUnsafe = unsafePinnedCreateOf

-- | A version of "create" that let's you pass in the initial capacity of the
-- array in terms of the number of elements.
--
-- Semantically @createWith 10@ and @createWith 100@ will behave in the same
-- way. @createWith 100@ will be more performant though.
--
-- > create = createWith elementCount
--
-- /Pre-release/
{-# INLINE_NORMAL createWith #-}
createWith :: forall m a. (MonadIO m, Unbox a)
    => Int -> Fold m a (Array a)
-- createWith n = FL.rmapM spliceArrays $ toArraysOf n
createWith elemCount = unsafeFreeze <$> MA.createWith elemCount

{-# DEPRECATED writeWith "Please use createWith instead." #-}
{-# INLINE writeWith #-}
writeWith :: forall m a. (MonadIO m, Unbox a)
    => Int -> Fold m a (Array a)
writeWith = createWith

-- | Fold the whole input to a single array.
--
-- /Caution! Do not use this on infinite streams./
--
{-# INLINE create #-}
create :: forall m a. (MonadIO m, Unbox a) => Fold m a (Array a)
create = fmap unsafeFreeze MA.create

{-# DEPRECATED write  "Please use create instead." #-}
{-# INLINE write #-}
write :: forall m a. (MonadIO m, Unbox a) => Fold m a (Array a)
write = create

-- | Like 'create' but creates a pinned array.
{-# INLINE pinnedCreate #-}
pinnedCreate :: forall m a. (MonadIO m, Unbox a) => Fold m a (Array a)
pinnedCreate = fmap unsafeFreeze MA.pinnedCreate

{-# DEPRECATED pinnedWrite  "Please use pinnedCreate instead." #-}
{-# INLINE pinnedWrite #-}
pinnedWrite :: forall m a. (MonadIO m, Unbox a) => Fold m a (Array a)
pinnedWrite = pinnedCreate

-- | Fold "step" has a dependency on "initial", and each step is dependent on
-- the previous invocation of step due to state passing, finally extract
-- depends on the result of step, therefore, as long as the fold is driven in
-- the correct order the operations would be correctly ordered. We need to
-- ensure that we strictly evaluate the previous step completely before the
-- next step.
--
-- To not share the same array we need to make sure that the result of
-- "initial" is not shared. Existential type ensures that it does not get
-- shared across different folds. However, if we invoke "initial" multiple
-- times for the same fold, there is a possiblity of sharing the two because
-- the compiler would consider it as a pure value. One such example is the
-- chunksOf combinator, or using an array creation fold with foldMany
-- combinator. Is there a proper way in GHC to tell it to not share a pure
-- expression in a particular case?
--
-- For this reason array creation folds have a MonadIO constraint. Pure folds
-- could be unsafe and dangerous. This is dangerous especially when used with
-- foldMany like operations.
--
-- >>> unsafePureWrite = Array.unsafeMakePure Array.write
--
{-# INLINE unsafeMakePure #-}
unsafeMakePure :: Monad m => Fold IO a b -> Fold m a b
unsafeMakePure (Fold step initial extract final) =
    Fold (\x a -> return $! unsafeInlineIO (step x a))
         (return $! unsafePerformIO initial)
         (\s -> return $! unsafeInlineIO $ extract s)
         (\s -> return $! unsafeInlineIO $ final s)

{-# INLINE fromPureStreamN #-}
fromPureStreamN :: Unbox a => Int -> Stream Identity a -> Array a
fromPureStreamN n x =
    unsafePerformIO $ fmap unsafeFreeze (MA.fromPureStreamN n x)

-- | Convert a pure stream in Identity monad to an immutable array.
--
-- Same as the following but with better performance:
--
-- >>> fromPureStream = Array.fromList . runIdentity . Stream.toList
--
fromPureStream :: Unbox a => Stream Identity a -> Array a
fromPureStream x = unsafePerformIO $ fmap unsafeFreeze (MA.fromPureStream x)
-- fromPureStream = runIdentity . D.fold (unsafeMakePure write)
-- fromPureStream = fromList . runIdentity . D.toList

-- XXX This should be monadic.

-- | Copy an immutable 'Ptr Word8' sequence into an array.
--
-- /Unsafe:/ The caller is responsible for safe addressing.
--
-- Note that this should be evaluated strictly to ensure that we do not hold
-- the reference to the pointer in a lazy thunk.
fromPtrN :: Int -> Ptr Word8 -> Array Word8
fromPtrN n addr = unsafePerformIO $ fmap unsafeFreeze (MA.fromPtrN n addr)

-- | Copy a null terminated immutable 'Addr#' Word8 sequence into an array.
--
-- /Unsafe:/ The caller is responsible for safe addressing.
--
-- Note that this is completely safe when reading from Haskell string
-- literals because they are guaranteed to be NULL terminated:
--
-- Note, you can use lazy unsafePerformIO _only if_ the pointer is immutable.
--
-- >>> Array.toList $ unsafePerformIO $ Array.fromCString# "\1\2\3\0"#
-- [1,2,3]
--
fromCString# :: MonadIO m => Addr# -> m (Array Word8)
fromCString# addr = fmap unsafeFreeze (MA.fromCString# addr)

{-# DEPRECATED fromByteStr# "Please use fromCString# instead." #-}
fromByteStr# :: Addr# -> Array Word8
fromByteStr# addr = unsafePerformIO $ fromCString# addr

-- | Copy a C string consisting of 16-bit wide chars and terminated by a 16-bit
-- null char, into a Word16 array. The null character is not copied.
--
-- Useful for copying UTF16 strings on Windows.
--
fromW16CString# :: MonadIO m => Addr# -> m (Array Word16)
fromW16CString# addr = fmap unsafeFreeze (MA.fromW16CString# addr)

fromCString :: MonadIO m => Ptr Word8 -> m (Array Word8)
fromCString (Ptr addr#) = fromCString# addr#

{-# DEPRECATED fromByteStr "Please use fromCString instead." #-}
fromByteStr :: Ptr Word8 -> Array Word8
fromByteStr = unsafePerformIO . fromCString

-- | Copy a C string consisting of 16-bit wide chars and terminated by a 16-bit
-- null char, into a Word16 array. The null character is not copied.
--
-- Useful for copying UTF16 strings on Windows.
--
fromW16CString :: MonadIO m => Ptr Word8 -> m (Array Word16)
fromW16CString (Ptr addr#) = fromW16CString# addr#

-- XXX implement fromChunks/fromChunkList instead?

-- | Convert an array stream to an array. Note that this requires peak memory
-- that is double the size of the array stream.
--
{-# INLINE fromChunksK #-}
fromChunksK :: (MonadIO m, Unbox a) => StreamK m (Array a) -> m (Array a)
fromChunksK stream =
    -- We buffer the entire stream and then allocate the target array of the
    -- same size, thus requiring double the memory.
    fmap unsafeFreeze $ MA.fromChunksK $ fmap unsafeThaw stream

{-# DEPRECATED fromArrayStreamK "Please use fromChunksK instead." #-}
fromArrayStreamK :: (Unbox a, MonadIO m) => StreamK m (Array a) -> m (Array a)
fromArrayStreamK = fromChunksK

-- | Given a stream of arrays, splice them all together to generate a single
-- array. The stream must be /finite/.
--
{-# INLINE fromChunks #-}
fromChunks :: (MonadIO m, Unbox a) => Stream m (Array a) -> m (Array a)
fromChunks s =
    -- XXX Check which implementation is better
    -- This may also require double the memory as we double the space every
    -- time, when copying the last array we may have reallocated almost double
    -- the space required before we right size it.
    fmap unsafeFreeze $ MA.fromChunksRealloced (fmap unsafeThaw s)
    -- fromChunkStreamK $ D.toStreamK s

-------------------------------------------------------------------------------
-- Instances
-------------------------------------------------------------------------------

instance (Show a, Unbox a) => Show (Array a) where
    {-# INLINE show #-}
    show arr = "fromList " ++ show (toList arr)

instance (Unbox a, Read a, Show a) => Read (Array a) where
    {-# INLINE readPrec #-}
    readPrec = do
        fromListWord <- replicateM 9 ReadPrec.get
        if fromListWord == "fromList "
        then fromList <$> readPrec
        else ReadPrec.pfail

instance (a ~ Char) => IsString (Array a) where
    {-# INLINE fromString #-}
    fromString = fromList

-- GHC versions 8.0 and below cannot derive IsList
instance Unbox a => IsList (Array a) where
    type (Item (Array a)) = a
    {-# INLINE fromList #-}
    fromList = fromList
    {-# INLINE fromListN #-}
    fromListN = fromListN
    {-# INLINE toList #-}
    toList = toList

-- | Byte compare two arrays. Compare the length of the arrays. If the length
-- is equal, compare the lexicographical ordering of two underlying byte arrays
-- otherwise return the result of length comparison.
--
-- /Unsafe/: Note that the 'Unbox' instance of sum types with constructors of
-- different sizes may leave some memory uninitialized which can make byte
-- comparison unreliable.
--
-- /Pre-release/
{-# INLINE byteCmp #-}
byteCmp :: Array a -> Array a -> Ordering
byteCmp arr1 arr2 =
    -- unsafePerformIO?
    unsafeInlineIO $! unsafeThaw arr1 `MA.byteCmp` unsafeThaw arr2

-- | Byte equality of two arrays.
--
-- >>> byteEq arr1 arr2 = (==) EQ $ Array.byteCmp arr1 arr2
--
-- /Unsafe/: See 'byteCmp'.
{-# INLINE byteEq #-}
byteEq :: Array a -> Array a -> Bool
byteEq arr1 arr2 = (==) EQ $ byteCmp arr1 arr2

#define MK_EQ_INSTANCE(typ)                              \
instance {-# OVERLAPPING #-} Eq (Array typ) where {      \
;    {-# INLINE (==) #-}                                 \
;    (==) = byteEq \
}

MK_EQ_INSTANCE(Char)
MK_EQ_INSTANCE(Word8)
MK_EQ_INSTANCE(Word16)
MK_EQ_INSTANCE(Word32)

-- XXX The Word64 default instance should be as fast because we are comparing
-- 64-bit at a time.
MK_EQ_INSTANCE(Word64)
MK_EQ_INSTANCE(Int)
MK_EQ_INSTANCE(Int8)
MK_EQ_INSTANCE(Int16)
MK_EQ_INSTANCE(Int32)

-- XXX The Int64 default instance should be as fast.
MK_EQ_INSTANCE(Int64)

-- | If the type allows a byte-by-byte comparison this instance can be
-- overlapped by a more specific instance that uses 'byteCmp'. Byte comparison
-- can be significantly faster.
--
instance {-# OVERLAPPABLE #-} (Unbox a, Eq a) => Eq (Array a) where
    {-# INLINE (==) #-}
    arr1 == arr2 =
        -- Does unboxed byte equality mean element equality?
        -- XXX This is incorrect for sum types, as we may have some
        -- uninitialized memory in that case. If we always initialize the
        -- unused memory to zero we can use this.
        -- Byte comparison is 40% faster compared to stream equality.
        -- (==) EQ $ unsafeInlineIO $! unsafeThaw arr1 `MA.cmp` unsafeThaw arr2
           (toStreamD arr1 :: Stream Identity a) == toStreamD arr2

instance (Unbox a, Ord a) => Ord (Array a) where
    {-# INLINE compare #-}
    compare arr1 arr2 = runIdentity $
        D.cmpBy compare (toStreamD arr1) (toStreamD arr2)

    -- Default definitions defined in base do not have an INLINE on them, so we
    -- replicate them here with an INLINE.
    {-# INLINE (<) #-}
    x <  y = case compare x y of { LT -> True;  _ -> False }

    {-# INLINE (<=) #-}
    x <= y = case compare x y of { GT -> False; _ -> True }

    {-# INLINE (>) #-}
    x >  y = case compare x y of { GT -> True;  _ -> False }

    {-# INLINE (>=) #-}
    x >= y = case compare x y of { LT -> False; _ -> True }

    -- These two default methods use '<=' rather than 'compare'
    -- because the latter is often more expensive
    {-# INLINE max #-}
    max x y = if x <= y then y else x

    {-# INLINE min #-}
    min x y = if x <= y then x else y

#ifdef DEVBUILD
-- Definitions using the Unboxed constraint from the Array type. These are to
-- make the Foldable instance possible though it is much slower (7x slower).
--
{-# INLINE_NORMAL _toStreamD_ #-}
_toStreamD_ :: forall m a. MonadIO m => Int -> Array a -> D.Stream m a
_toStreamD_ size Array{..} = D.Stream step arrStart

    where

    {-# INLINE_LATE step #-}
    step _ p | p == arrEnd = return D.Stop
    step _ p = liftIO $ do
        x <- peekAt p arrContents
        return $ D.Yield x (p + size)

{-
XXX Why isn't Unboxed implicit? This does not compile unless I use the Unboxed
contraint.
{-# INLINE_NORMAL _foldr #-}
_foldr :: forall a b. (a -> b -> b) -> b -> Array a -> b
_foldr f z arr =
    let !n = SIZE_OF(a)
    in unsafePerformIO $ D.foldr f z $ toStreamD_ n arr
-- | Note that the 'Foldable' instance is 7x slower than the direct
-- operations.
instance Foldable Array where
  foldr = _foldr
-}

#endif

-------------------------------------------------------------------------------
-- Semigroup and Monoid
-------------------------------------------------------------------------------

-- XXX Deprecate and remove the Semigroup and Monoid instances because of
-- potential misuse chances.

-- | This should not be used for combining many or N arrays as it would copy
-- the two arrays everytime to a new array. For coalescing multiple arrays use
-- 'fromChunksK' instead.
instance Unbox a => Semigroup (Array a) where
    arr1 <> arr2 = unsafePerformIO $ splice arr1 arr2

empty ::
#ifdef DEVBUILD
    Unbox a =>
#endif
    Array a
empty = Array Unboxed.empty 0 0

{-# DEPRECATED nil "Please use empty instead." #-}
nil ::
#ifdef DEVBUILD
    Unbox a =>
#endif
    Array a
nil = empty

instance Unbox a => Monoid (Array a) where
    mempty = nil
    mappend = (<>)
