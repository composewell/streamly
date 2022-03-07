-- |
-- Module      : Streamly.Internal.Data.Array.Foreign.Type
-- Copyright   : (c) 2020 Composewell Technologies
--
-- License     : BSD3-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- See notes in "Streamly.Internal.Data.Array.Foreign.Mut.Type"
--
module Streamly.Internal.Data.Array.Foreign.Type
    (
    -- $arrayNotes
      Array (..)
    , asPtrUnsafe

    -- * Freezing and Thawing
    , unsafeFreeze
    , unsafeFreezeWithShrink
    , unsafeThaw

    -- * Construction
    , splice

    , fromPtr
    , fromForeignPtrUnsafe
    , fromAddr#
    , fromCString#
    , fromList
    , fromListN
    , fromListRev
    , fromListRevN
    , fromStreamDN
    , fromStreamD

    -- * Split
    , breakOn

    -- * Elimination
    , unsafeIndexIO
    , unsafeIndex
    , byteLength
    , length

    , foldl'
    , foldr
    , splitAt

    , readRev
    , toStreamD
    , toStreamDRev
    , toStreamK
    , toStreamKRev
    , toStream
    , toStreamRev
    , toList

    -- * Folds
    , writeWith
    , writeN
    , writeNUnsafe
    , MA.ArrayUnsafe (..)
    , writeNAligned
    , writeNAlignedUnmanaged
    , write

    -- * Streams of arrays
    , arraysOf
    , bufferChunks
    , flattenArrays
    , flattenArraysRev
    )
where

#include "ArrayMacros.h"
#include "inline.hs"

import Control.Exception (assert)
import Control.DeepSeq (NFData(..))
#if MIN_VERSION_deepseq(1,4,3)
import Control.DeepSeq (NFData1(..))
#endif
import Control.Monad.IO.Class (MonadIO(..))
import Data.Functor.Identity (Identity(..))
import Data.Word (Word8)
import Foreign.C.String (CString)
import Foreign.C.Types (CSize(..))
import Foreign.Ptr (plusPtr, castPtr)
import Foreign.Storable (Storable(..))
import GHC.Base (Addr#, nullAddr#, build)
import GHC.Exts (IsList, IsString(..))
import GHC.ForeignPtr (ForeignPtr)

import GHC.IO (unsafePerformIO)
import GHC.Ptr (Ptr(..))
import Streamly.Internal.Data.Array.Foreign.Mut.Type
    (ArrayContents, ReadUState(..), touch)
import Streamly.Internal.Data.Fold.Type (Fold(..))
import Streamly.Internal.Data.Stream.Serial (SerialT(..))
import Streamly.Internal.Data.Unfold.Type (Unfold(..))
import Text.Read (readPrec, readListPrec, readListPrecDefault)

import Prelude hiding (length, foldr, read, unlines, splitAt)

import qualified Streamly.Internal.Data.Array.Foreign.Mut.Type as MA
import qualified Streamly.Internal.Data.Stream.StreamD.Type as D
import qualified Streamly.Internal.Data.Stream.StreamK.Type as K
import qualified GHC.Exts as Exts

import Streamly.Internal.System.IO (unsafeInlineIO, defaultChunkSize)

#if __GLASGOW_HASKELL__ < 808
import Data.Semigroup (Semigroup(..))
#endif

#ifdef DEVBUILD
import qualified Data.Foldable as F
#endif

--
-- $setup
-- >>> :m
-- >>> :set -XMagicHash
-- >>> import Prelude hiding (length, foldr, read, unlines, splitAt)
-- >>> import Streamly.Internal.Data.Array.Foreign as Array

-- XXX Since these are immutable arrays MonadIO constraint can be removed from
-- most places.

-------------------------------------------------------------------------------
-- Array Data Type
-------------------------------------------------------------------------------

-- $arrayNotes
--
-- We can use a 'Storable' constraint in the Array type and the constraint can
-- be automatically provided to a function that pattern matches on the Array
-- type. However, it has huge performance cost, so we do not use it.
-- Investigate a GHC improvement possiblity.
--
-- XXX Rename the fields to better names.
--
data Array a =
#ifdef DEVBUILD
    Storable a =>
#endif
    Array
    { arrContents ::
#ifndef USE_FOREIGN_PTR
        {-# UNPACK #-}
#endif
            !ArrayContents -- ^ first address
    , arrStart :: {-# UNPACK #-} !(Ptr a) -- start address
    , aEnd   :: {-# UNPACK #-} !(Ptr a)        -- first unused addres
    }

-------------------------------------------------------------------------------
-- Utility functions
-------------------------------------------------------------------------------

foreign import ccall unsafe "string.h strlen" c_strlen
    :: CString -> IO CSize

-- | Use an @Array a@ as @Ptr a@.
--
-- /Unsafe/
--
-- /Pre-release/
--
asPtrUnsafe :: MonadIO m => Array a -> (Ptr a -> m b) -> m b
asPtrUnsafe Array{..} f = do
  r <- f arrStart
  liftIO $ touch arrContents
  return r

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
unsafeFreeze :: MA.Array a -> Array a
unsafeFreeze (MA.Array ac as ae _) = Array ac as ae

-- | Similar to 'unsafeFreeze' but uses 'MA.rightSize' on the mutable array
-- first.
{-# INLINE unsafeFreezeWithShrink #-}
unsafeFreezeWithShrink :: Storable a => MA.Array a -> Array a
unsafeFreezeWithShrink arr = unsafePerformIO $ do
  MA.Array ac as ae _ <- MA.rightSize arr
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
unsafeThaw :: Array a -> MA.Array a
unsafeThaw (Array ac as ae) = MA.Array ac as ae ae

-------------------------------------------------------------------------------
-- Construction
-------------------------------------------------------------------------------

-- Splice two immutable arrays creating a new array.
{-# INLINE splice #-}
splice :: (MonadIO m, Storable a) => Array a -> Array a -> m (Array a)
splice arr1 arr2 =
    unsafeFreeze <$> MA.splice (unsafeThaw arr1) (unsafeThaw arr2)

-- | Create an 'Array' of the given number of elements of type @a@ from a read
-- only pointer @Ptr a@.  The pointer is not freed when the array is garbage
-- collected. This API is unsafe for the following reasons:
--
-- 1. The pointer must point to static pinned memory or foreign memory that
-- does not require freeing..
-- 2. The pointer must be legally accessible upto the given length.
-- 3. To guarantee that the array is immutable, the contents of the address
-- must be guaranteed to not change.
--
-- /Unsafe/
--
-- /Pre-release/
--
{-# INLINE fromPtr #-}
fromPtr ::
#ifdef DEVBUILD
    Storable a =>
#endif
    Int -> Ptr a -> Array a
fromPtr n ptr = unsafeInlineIO $ do
    let end = ptr `plusPtr` n
    return $ Array
        { arrContents = MA.nilArrayContents
        , arrStart = ptr
        , aEnd = end
        }

-- | @fromForeignPtrUnsafe foreignPtr end bound@ creates an 'Array' that starts
-- at the memory pointed by the @foreignPtr@, @end@ is the first unused
-- address.
--
-- Unsafe: Make sure that foreignPtr <= end and (end - start) is an
-- integral multiple of the element size. Only PlainPtr type ForeignPtr is
-- supported.
--
-- /Pre-release/
--
{-# INLINE fromForeignPtrUnsafe #-}
fromForeignPtrUnsafe ::
#ifdef DEVBUILD
    Storable a =>
#endif
    ForeignPtr a -> Ptr a -> Array a
fromForeignPtrUnsafe fp end = unsafeFreeze $ MA.fromForeignPtrUnsafe fp end end

-- XXX when converting an array of Word8 from a literal string we can simply
-- refer to the literal string. Is it possible to write rules such that
-- fromList Word8 can be rewritten so that GHC does not first convert the
-- literal to [Char] and then we convert it back to an Array Word8?
--
-- TBD: We can also add template haskell quasiquotes to specify arrays of other
-- literal types. TH will encode them into a string literal and we read that as
-- an array of the required type. With template Haskell we can provide a safe
-- version of fromString#.
--
-- | Create an @Array Word8@ of the given length from a static, read only
-- machine address 'Addr#'. See 'fromPtr' for safety caveats.
--
-- A common use case for this API is to create an array from a static unboxed
-- string literal. GHC string literals are of type 'Addr#', and must contain
-- characters that can be encoded in a byte i.e. characters or literal bytes in
-- the range from 0-255.
--
-- >>> import Data.Word (Word8)
-- >>> Array.fromAddr# 5 "hello world!"# :: Array Word8
-- [104,101,108,108,111]
--
-- >>> Array.fromAddr# 3 "\255\NUL\255"# :: Array Word8
-- [255,0,255]
--
-- /See also: 'fromString#'/
--
-- /Unsafe/
--
-- /Time complexity: O(1)/
--
-- /Pre-release/
--
{-# INLINE fromAddr# #-}
fromAddr# ::
#ifdef DEVBUILD
    Storable a =>
#endif
    Int -> Addr# -> Array a
fromAddr# n addr# = fromPtr n (castPtr $ Ptr addr#)

-- | Generate a byte array from an 'Addr#' that contains a sequence of NUL
-- (@0@) terminated bytes. The array would not include the NUL byte. The
-- address must be in static read-only memory and must be legally accessible up
-- to and including the first NUL byte.
--
-- An unboxed string literal (e.g. @"hello"#@) is a common example of an
-- 'Addr#' in static read only memory. It represents the UTF8 encoded sequence
-- of bytes terminated by a NUL byte (a 'CString') corresponding to the
-- given unicode string.
--
-- >>> Array.fromCString# "hello world!"#
-- [104,101,108,108,111,32,119,111,114,108,100,33]
--
-- >>> Array.fromCString# "\255\NUL\255"#
-- [255]
--
-- /See also: 'fromAddr#'/
--
-- /Unsafe/
--
-- /Time complexity: O(n) (computes the length of the string)/
--
-- /Pre-release/
--
{-# INLINE fromCString# #-}
fromCString# :: Addr# -> Array Word8
fromCString# addr# = do
    let cstr = Ptr addr#
        len = unsafeInlineIO $ c_strlen cstr
    fromPtr (fromIntegral len) (castPtr cstr)

-- | Create an 'Array' from the first N elements of a list. The array is
-- allocated to size N, if the list terminates before N elements then the
-- array may hold less than N elements.
--
-- /Since 0.7.0 (Streamly.Memory.Array)/
--
-- @since 0.8.0
{-# INLINABLE fromListN #-}
fromListN :: Storable a => Int -> [a] -> Array a
fromListN n xs = unsafePerformIO $ unsafeFreeze <$> MA.fromListN n xs

-- XXX We can possibly have a direction flag in the array to reverse it without
-- actually doing anything. With that we can just do "reverse . fromList". But
-- it may complicate all the APIs as all reads of the array will have to handle
-- the flag.
--
-- | Create an 'Array' from the first N elements of a list in reverse order.
-- The array is allocated to size N, if the list terminates before N elements
-- then the array may hold less than N elements.
--
-- /Unimplemented/
{-# INLINABLE fromListRevN #-}
fromListRevN :: {- Storable a => -} Int -> [a] -> Array a
fromListRevN _n _xs = undefined

-- | Create an 'Array' from a list. The list must be of finite size.
--
-- /Since 0.7.0 (Streamly.Memory.Array)/
--
-- @since 0.8.0
{-# INLINE fromList #-}
fromList :: Storable a => [a] -> Array a
fromList xs = unsafePerformIO $ unsafeFreeze <$> MA.fromList xs

-- | Create an 'Array' from a list in reverse order. The list must be of finite
-- size.
--
-- /Unimplemented/
{-# INLINABLE fromListRev #-}
fromListRev :: {- Storable a => -} [a] -> Array a
fromListRev _xs = undefined

{-# INLINE_NORMAL fromStreamDN #-}
fromStreamDN :: forall m a. (MonadIO m, Storable a)
    => Int -> D.Stream m a -> m (Array a)
fromStreamDN limit str = unsafeFreeze <$> MA.fromStreamDN limit str

{-# INLINE_NORMAL fromStreamD #-}
fromStreamD :: forall m a. (MonadIO m, Storable a)
    => D.Stream m a -> m (Array a)
fromStreamD str = unsafeFreeze <$> MA.fromStreamD str

-------------------------------------------------------------------------------
-- Streams of arrays
-------------------------------------------------------------------------------

{-# INLINE bufferChunks #-}
bufferChunks :: (MonadIO m, Storable a) =>
    D.Stream m a -> m (K.Stream m (Array a))
bufferChunks m = D.foldr K.cons K.nil $ arraysOf defaultChunkSize m

-- | @arraysOf n stream@ groups the input stream into a stream of
-- arrays of size n.
{-# INLINE_NORMAL arraysOf #-}
arraysOf :: forall m a. (MonadIO m, Storable a)
    => Int -> D.Stream m a -> D.Stream m (Array a)
arraysOf n str = D.map unsafeFreeze $ MA.arraysOf n str

-- | Use the "read" unfold instead.
--
-- @flattenArrays = unfoldMany read@
--
-- We can try this if there are any fusion issues in the unfold.
--
{-# INLINE_NORMAL flattenArrays #-}
flattenArrays :: forall m a. (MonadIO m, Storable a)
    => D.Stream m (Array a) -> D.Stream m a
flattenArrays = MA.flattenArrays . D.map unsafeThaw

-- | Use the "readRev" unfold instead.
--
-- @flattenArrays = unfoldMany readRev@
--
-- We can try this if there are any fusion issues in the unfold.
--
{-# INLINE_NORMAL flattenArraysRev #-}
flattenArraysRev :: forall m a. (MonadIO m, Storable a)
    => D.Stream m (Array a) -> D.Stream m a
flattenArraysRev = MA.flattenArraysRev . D.map unsafeThaw

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
unsafeIndexIO :: forall a. Storable a => Int -> Array a -> IO a
unsafeIndexIO i arr = MA.getIndexUnsafe i (unsafeThaw arr)

-- | Return element at the specified index without checking the bounds.
{-# INLINE_NORMAL unsafeIndex #-}
unsafeIndex :: forall a. Storable a => Int -> Array a -> a
unsafeIndex i arr = let !r = unsafeInlineIO $ unsafeIndexIO i arr in r

-- | /O(1)/ Get the byte length of the array.
--
-- @since 0.7.0
{-# INLINE byteLength #-}
byteLength :: Array a -> Int
byteLength = MA.byteLength . unsafeThaw

-- | /O(1)/ Get the length of the array i.e. the number of elements in the
-- array.
--
-- /Since 0.7.0 (Streamly.Memory.Array)/
--
-- @since 0.8.0
{-# INLINE length #-}
length :: forall a. Storable a => Array a -> Int
length arr = MA.length (unsafeThaw arr)

-- | Unfold an array into a stream in reverse order.
--
-- @since 0.8.0
{-# INLINE_NORMAL readRev #-}
readRev :: forall m a. (Monad m, Storable a) => Unfold m (Array a) a
readRev = Unfold step inject
    where

    inject (Array contents start end) =
        let p = PTR_PREV(end,a)
         in return $ ReadUState contents start p

    {-# INLINE_LATE step #-}
    step (ReadUState contents start p) | p < start =
        let x = unsafeInlineIO $ touch contents
        in x `seq` return D.Stop
    step (ReadUState contents start p) = do
            -- unsafeInlineIO allows us to run this in Identity monad for pure
            -- toList/foldr case which makes them much faster due to not
            -- accumulating the list and fusing better with the pure consumers.
            --
            -- This should be safe as the array contents are guaranteed to be
            -- evaluated/written to before we peek at them.
            let !x = unsafeInlineIO $ peek p
            return $ D.Yield x (ReadUState contents start (PTR_PREV(p,a)))


{-# INLINE_NORMAL toStreamD #-}
toStreamD :: forall m a. (Monad m, Storable a) => Array a -> D.Stream m a
toStreamD Array{..} = D.Stream step arrStart

    where

    {-# INLINE_LATE step #-}
    step _ p | assert (p <= aEnd) (p == aEnd) = return D.Stop
    step _ p = do
        -- unsafeInlineIO allows us to run this in Identity monad for pure
        -- toList/foldr case which makes them much faster due to not
        -- accumulating the list and fusing better with the pure consumers.
        --
        -- This should be safe as the array contents are guaranteed to be
        -- evaluated/written to before we peek at them.
        --
        let !x = unsafeInlineIO $ do
                    r <- peek p
                    touch arrContents
                    return r
        return $ D.Yield x (PTR_NEXT(p,a))

{-# INLINE toStreamK #-}
toStreamK :: forall m a. Storable a => Array a -> K.Stream m a
toStreamK Array{..} = go arrStart

    where

    go p | assert (p <= aEnd) (p == aEnd) = K.nil
         | otherwise =
        -- See Note in toStreamD.
        let !x = unsafeInlineIO $ do
                    r <- peek p
                    touch arrContents
                    return r
        in x `K.cons` go (PTR_NEXT(p,a))

{-# INLINE_NORMAL toStreamDRev #-}
toStreamDRev :: forall m a. (Monad m, Storable a) => Array a -> D.Stream m a
toStreamDRev Array{..} = D.Stream step (PTR_PREV(aEnd,a))

    where

    {-# INLINE_LATE step #-}
    step _ p | p < arrStart = return D.Stop
    step _ p = do
        -- See comments in toStreamD for why we use unsafeInlineIO
        let !x = unsafeInlineIO $ do
                    r <- peek p
                    touch arrContents
                    return r
        return $ D.Yield x (PTR_PREV(p,a))

{-# INLINE toStreamKRev #-}
toStreamKRev :: forall m a. Storable a => Array a -> K.Stream m a
toStreamKRev Array {..} = go (PTR_PREV(aEnd,a))

    where

    go p | p < arrStart = K.nil
         | otherwise =
        let !x = unsafeInlineIO $ do
                    r <- peek p
                    touch arrContents
                    return r
        in x `K.cons` go (PTR_PREV(p,a))

-- | Convert an 'Array' into a stream.
--
-- /Pre-release/
{-# INLINE_EARLY toStream #-}
toStream :: (Monad m, Storable a) => Array a -> SerialT m a
toStream = SerialT . D.toStreamK . toStreamD
-- XXX add fallback to StreamK rule
-- {-# RULES "Streamly.Array.read fallback to StreamK" [1]
--     forall a. S.readK (read a) = K.fromArray a #-}

-- | Convert an 'Array' into a stream in reverse order.
--
-- /Pre-release/
{-# INLINE_EARLY toStreamRev #-}
toStreamRev :: (Monad m, Storable a) => Array a -> SerialT m a
toStreamRev = SerialT . D.toStreamK . toStreamDRev

-- XXX add fallback to StreamK rule
-- {-# RULES "Streamly.Array.readRev fallback to StreamK" [1]
--     forall a. S.toStreamK (readRev a) = K.revFromArray a #-}

{-# INLINE_NORMAL foldl' #-}
foldl' :: forall a b. Storable a => (b -> a -> b) -> b -> Array a -> b
foldl' f z arr = runIdentity $ D.foldl' f z $ toStreamD arr

{-# INLINE_NORMAL foldr #-}
foldr :: Storable a => (a -> b -> b) -> b -> Array a -> b
foldr f z arr = runIdentity $ D.foldr f z $ toStreamD arr

-- | Create two slices of an array without copying the original array. The
-- specified index @i@ is the first index of the second slice.
--
-- @since 0.7.0
splitAt :: forall a. Storable a => Int -> Array a -> (Array a, Array a)
splitAt i arr = (unsafeFreeze a, unsafeFreeze b)
  where
    (a, b) = MA.splitAt i (unsafeThaw arr)

-- Use foldr/build fusion to fuse with list consumers
-- This can be useful when using the IsList instance
{-# INLINE_LATE toListFB #-}
toListFB :: forall a b. Storable a => (a -> b -> b) -> b -> Array a -> b
toListFB c n Array{..} = go arrStart
    where

    go p | assert (p <= aEnd) (p == aEnd) = n
    go p =
        -- unsafeInlineIO allows us to run this in Identity monad for pure
        -- toList/foldr case which makes them much faster due to not
        -- accumulating the list and fusing better with the pure consumers.
        --
        -- This should be safe as the array contents are guaranteed to be
        -- evaluated/written to before we peek at them.
        let !x = unsafeInlineIO $ do
                    r <- peek p
                    touch arrContents
                    return r
        in c x (go (PTR_NEXT(p,a)))

-- | Convert an 'Array' into a list.
--
-- /Since 0.7.0 (Streamly.Memory.Array)/
--
-- @since 0.8.0
{-# INLINE toList #-}
toList :: Storable a => Array a -> [a]
toList s = build (\c n -> toListFB c n s)

-------------------------------------------------------------------------------
-- Folds
-------------------------------------------------------------------------------

-- | @writeN n@ folds a maximum of @n@ elements from the input stream to an
-- 'Array'.
--
-- /Since 0.7.0 (Streamly.Memory.Array)/
--
-- @since 0.8.0
{-# INLINE_NORMAL writeN #-}
writeN :: forall m a. (MonadIO m, Storable a) => Int -> Fold m a (Array a)
writeN = fmap unsafeFreeze . MA.writeN

-- | @writeNAligned alignment n@ folds a maximum of @n@ elements from the input
-- stream to an 'Array' aligned to the given size.
--
-- /Pre-release/
--
{-# INLINE_NORMAL writeNAligned #-}
writeNAligned :: forall m a. (MonadIO m, Storable a)
    => Int -> Int -> Fold m a (Array a)
writeNAligned alignSize = fmap unsafeFreeze . MA.writeNAligned alignSize

-- | @writeNAlignedUnmanaged n@ folds a maximum of @n@ elements from the input
-- stream to an 'Array' aligned to the given size and using unmanaged memory.
-- This could be useful to allocate memory that we need to allocate only once
-- in the lifetime of the program.
--
-- /Pre-release/
--
{-# INLINE_NORMAL writeNAlignedUnmanaged #-}
writeNAlignedUnmanaged :: forall m a. (MonadIO m, Storable a)
    => Int -> Int -> Fold m a (Array a)
writeNAlignedUnmanaged alignSize =
    fmap unsafeFreeze . MA.writeNAlignedUnmanaged alignSize

-- | Like 'writeN' but does not check the array bounds when writing. The fold
-- driver must not call the step function more than 'n' times otherwise it will
-- corrupt the memory and crash. This function exists mainly because any
-- conditional in the step function blocks fusion causing 10x performance
-- slowdown.
--
-- @since 0.7.0
{-# INLINE_NORMAL writeNUnsafe #-}
writeNUnsafe :: forall m a. (MonadIO m, Storable a)
    => Int -> Fold m a (Array a)
writeNUnsafe n = unsafeFreeze <$> MA.writeNUnsafe n

{-# INLINE_NORMAL writeWith #-}
writeWith :: forall m a. (MonadIO m, Storable a)
    => Int -> Fold m a (Array a)
-- writeWith n = FL.rmapM spliceArrays $ toArraysOf n
writeWith elemCount = unsafeFreeze <$> MA.writeWith elemCount

-- | Fold the whole input to a single array.
--
-- /Caution! Do not use this on infinite streams./
--
-- /Since 0.7.0 (Streamly.Memory.Array)/
--
-- @since 0.8.0
{-# INLINE write #-}
write :: forall m a. (MonadIO m, Storable a) => Fold m a (Array a)
write = fmap unsafeFreeze MA.write

-------------------------------------------------------------------------------
-- Instances
-------------------------------------------------------------------------------

instance (Show a, Storable a) => Show (Array a) where
    {-# INLINE showsPrec #-}
    showsPrec _ = shows . toList

instance (Storable a, Read a, Show a) => Read (Array a) where
    {-# INLINE readPrec #-}
    readPrec = fromList <$> readPrec
    readListPrec = readListPrecDefault

instance (a ~ Char) => IsString (Array a) where
    {-# INLINE fromString #-}
    fromString = fromList

-- GHC versions 8.0 and below cannot derive IsList
instance Storable a => IsList (Array a) where
    type (Item (Array a)) = a
    {-# INLINE fromList #-}
    fromList = fromList
    {-# INLINE fromListN #-}
    fromListN = fromListN
    {-# INLINE toList #-}
    toList = toList

-- XXX we are assuming that Storable equality means element equality. This may
-- or may not be correct? arrcmp is 40% faster compared to stream equality.
instance (Storable a, Eq a) => Eq (Array a) where
    {-# INLINE (==) #-}
    arr1 == arr2 = unsafeInlineIO $! unsafeThaw arr1 `MA.cmp` unsafeThaw arr2

-- Since this is a Storable array we cannot have unevaluated data in it so
-- this is just a no op.
instance NFData (Array a) where
    {-# INLINE rnf #-}
    rnf Array {} = ()

#if MIN_VERSION_deepseq(1,4,3)
instance NFData1 Array where
    liftRnf _ Array{} = ()
#endif

instance (Storable a, Ord a) => Ord (Array a) where
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
-- Definitions using the Storable constraint from the Array type. These are to
-- make the Foldable instance possible though it is much slower (7x slower).
--
{-# INLINE_NORMAL toStreamD_ #-}
toStreamD_ :: forall m a. MonadIO m => Int -> Array a -> D.Stream m a
toStreamD_ size Array{..} = D.Stream step arrStart

    where

    {-# INLINE_LATE step #-}
    step _ p | p == aEnd = return D.Stop
    step _ p = liftIO $ do
        x <- peek p
        touch arrContents
        return $ D.Yield x (p `plusPtr` size)

{-

XXX Why isn't Storable implicit? This does not compile unless I use the Storable
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

instance Storable a => Semigroup (Array a) where
    arr1 <> arr2 = unsafePerformIO $ splice arr1 arr2

nil ::
#ifdef DEVBUILD
    Storable a =>
#endif
    Array a
nil = Array MA.nilArrayContents (Ptr nullAddr#) (Ptr nullAddr#)

instance Storable a => Monoid (Array a) where
    mempty = nil
    mappend = (<>)
