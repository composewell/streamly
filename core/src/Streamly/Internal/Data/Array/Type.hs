-- |
-- Module      : Streamly.Internal.Data.Array.Type
-- Copyright   : (c) 2020 Composewell Technologies
--
-- License     : BSD3-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- See notes in "Streamly.Internal.Data.Array.Mut.Type"
--
module Streamly.Internal.Data.Array.Type
    (
    -- $arrayNotes
      Array (..)
    , asPtrUnsafe

    -- * Freezing and Thawing
    , unsafeFreeze
    , unsafeFreezeWithShrink
    , unsafeThaw

    -- * Pinning and Unpinning
    , pin
    , unpin

    -- * Construction
    , splice

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
    , unsafeIndex -- getIndexUnsafe
    , byteLength
    , length

    , foldl'
    , foldr
    , splitAt

    , toStreamD
    , toStreamDRev
    , toStreamK
    , toStreamKRev
    , toStream
    , toStreamRev
    , read
    , readRev
    , readerRev
    , toList

    -- * Folds
    , writeWith
    , writeN
    , writeNUnsafe
    , MA.ArrayUnsafe (..)
    , writeNAligned
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
import Control.Monad (replicateM)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Functor.Identity (Identity(..))
import Data.Proxy (Proxy(..))
import Data.Word (Word8)
import GHC.Base (build)
import GHC.Exts (IsList, IsString(..))

import GHC.IO (unsafePerformIO)
import GHC.Ptr (Ptr(..))
import Streamly.Internal.Data.Array.Mut.Type (MutArray(..), MutableByteArray)
import Streamly.Internal.Data.Fold.Type (Fold(..))
import Streamly.Internal.Data.Stream.StreamD.Type (Stream)
import Streamly.Internal.Data.Unboxed (Unbox, peekWith, sizeOf)
import Streamly.Internal.Data.Unfold.Type (Unfold(..))
import Text.Read (readPrec)

import Prelude hiding (length, foldr, read, unlines, splitAt)

import qualified GHC.Exts as Exts
import qualified Streamly.Internal.Data.Array.Mut.Type as MA
import qualified Streamly.Internal.Data.Stream.StreamD.Type as D
import qualified Streamly.Internal.Data.Stream.StreamK.Type as K
import qualified Streamly.Internal.Data.Unboxed as Unboxed
import qualified Streamly.Internal.Data.Unfold.Type as Unfold
import qualified Text.ParserCombinators.ReadPrec as ReadPrec

import Streamly.Internal.System.IO (unsafeInlineIO, defaultChunkSize)

--
-- $setup
-- >>> :m
-- >>> :set -XMagicHash
-- >>> import Prelude hiding (length, foldr, read, unlines, splitAt)
-- >>> import Streamly.Data.Stream as Stream
-- >>> import Streamly.Internal.Data.Array as Array

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
    { arrContents :: {-# UNPACK #-} !MutableByteArray
    , arrStart :: {-# UNPACK #-} !Int -- offset
    , arrEnd   :: {-# UNPACK #-} !Int   -- offset + len
    }

-------------------------------------------------------------------------------
-- Utility functions
-------------------------------------------------------------------------------

-- | Use an @Array a@ as @Ptr a@.
--
-- See 'MA.asPtrUnsafe' in the Mutable array module for more details.
--
-- /Unsafe/
--
-- /Pre-release/
--
asPtrUnsafe :: MonadIO m => Array a -> (Ptr a -> m b) -> m b
asPtrUnsafe arr = MA.asPtrUnsafe (unsafeThaw arr)

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

{-# INLINE pin #-}
pin :: Array a -> IO (Array a)
pin = fmap unsafeFreeze . MA.pin . unsafeThaw

{-# INLINE unpin #-}
unpin :: Array a -> IO (Array a)
unpin = fmap unsafeFreeze . MA.unpin . unsafeThaw

-------------------------------------------------------------------------------
-- Construction
-------------------------------------------------------------------------------

-- Splice two immutable arrays creating a new array.
{-# INLINE splice #-}
splice :: (MonadIO m, Unbox a) => Array a -> Array a -> m (Array a)
splice arr1 arr2 =
    unsafeFreeze <$> MA.splice (unsafeThaw arr1) (unsafeThaw arr2)

-- | Create an 'Array' from the first N elements of a list. The array is
-- allocated to size N, if the list terminates before N elements then the
-- array may hold less than N elements.
--
{-# INLINABLE fromListN #-}
fromListN :: Unbox a => Int -> [a] -> Array a
fromListN n xs = unsafePerformIO $ unsafeFreeze <$> MA.fromListN n xs

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

-- | Create an 'Array' from a list in reverse order. The list must be of finite
-- size.
--
-- /Pre-release/
{-# INLINABLE fromListRev #-}
fromListRev :: Unbox a => [a] -> Array a
fromListRev xs = unsafePerformIO $ unsafeFreeze <$> MA.fromListRev xs

{-# INLINE_NORMAL fromStreamDN #-}
fromStreamDN :: forall m a. (MonadIO m, Unbox a)
    => Int -> D.Stream m a -> m (Array a)
fromStreamDN limit str = unsafeFreeze <$> MA.fromStreamDN limit str

{-# INLINE_NORMAL fromStreamD #-}
fromStreamD :: forall m a. (MonadIO m, Unbox a)
    => D.Stream m a -> m (Array a)
fromStreamD str = unsafeFreeze <$> MA.fromStreamD str

-------------------------------------------------------------------------------
-- Streams of arrays
-------------------------------------------------------------------------------

{-# INLINE bufferChunks #-}
bufferChunks :: (MonadIO m, Unbox a) =>
    D.Stream m a -> m (K.StreamK m (Array a))
bufferChunks m = D.foldr K.cons K.nil $ arraysOf defaultChunkSize m

-- | @arraysOf n stream@ groups the elements in the input stream into arrays of
-- @n@ elements each.
--
-- Same as the following but may be more efficient:
--
-- >>> arraysOf n = Stream.foldMany (Array.writeN n)
--
-- /Pre-release/
{-# INLINE_NORMAL arraysOf #-}
arraysOf :: forall m a. (MonadIO m, Unbox a)
    => Int -> D.Stream m a -> D.Stream m (Array a)
arraysOf n str = D.map unsafeFreeze $ MA.arraysOf n str

-- | Use the "read" unfold instead.
--
-- @flattenArrays = unfoldMany read@
--
-- We can try this if there are any fusion issues in the unfold.
--
{-# INLINE_NORMAL flattenArrays #-}
flattenArrays :: forall m a. (MonadIO m, Unbox a)
    => D.Stream m (Array a) -> D.Stream m a
flattenArrays = MA.flattenArrays . D.map unsafeThaw

-- | Use the "readRev" unfold instead.
--
-- @flattenArrays = unfoldMany readRev@
--
-- We can try this if there are any fusion issues in the unfold.
--
{-# INLINE_NORMAL flattenArraysRev #-}
flattenArraysRev :: forall m a. (MonadIO m, Unbox a)
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
unsafeIndexIO :: forall a. Unbox a => Int -> Array a -> IO a
unsafeIndexIO i arr = MA.getIndexUnsafe i (unsafeThaw arr)

-- | Return element at the specified index without checking the bounds.
{-# INLINE_NORMAL unsafeIndex #-}
unsafeIndex :: forall a. Unbox a => Int -> Array a -> a
unsafeIndex i arr = let !r = unsafeInlineIO $ unsafeIndexIO i arr in r

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

-- | Unfold an array into a stream in reverse order.
--
{-# INLINE_NORMAL readerRev #-}
readerRev :: forall m a. (Monad m, Unbox a) => Unfold m (Array a) a
readerRev = Unfold.lmap unsafeThaw $ MA.readerRevWith (return . unsafeInlineIO)

{-# INLINE_NORMAL toStreamD #-}
toStreamD :: forall m a. (Monad m, Unbox a) => Array a -> D.Stream m a
toStreamD arr = MA.toStreamDWith (return . unsafeInlineIO) (unsafeThaw arr)

{-# INLINE toStreamK #-}
toStreamK :: forall m a. (Monad m, Unbox a) => Array a -> K.StreamK m a
toStreamK arr = MA.toStreamKWith (return . unsafeInlineIO) (unsafeThaw arr)

{-# INLINE_NORMAL toStreamDRev #-}
toStreamDRev :: forall m a. (Monad m, Unbox a) => Array a -> D.Stream m a
toStreamDRev arr =
    MA.toStreamDRevWith (return . unsafeInlineIO) (unsafeThaw arr)

{-# INLINE toStreamKRev #-}
toStreamKRev :: forall m a. (Monad m, Unbox a) => Array a -> K.StreamK m a
toStreamKRev arr =
    MA.toStreamKRevWith (return . unsafeInlineIO) (unsafeThaw arr)

-- | Convert an 'Array' into a stream.
--
-- /Pre-release/
{-# INLINE_EARLY read #-}
read :: (Monad m, Unbox a) => Array a -> Stream m a
read = toStreamD

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
readRev = toStreamDRev

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
        -- evaluated/written to before we peekWith at them.
        let !x = unsafeInlineIO $ peekWith arrContents p
        in c x (go (INDEX_NEXT(p,a)))

-- | Convert an 'Array' into a list.
--
{-# INLINE toList #-}
toList :: Unbox a => Array a -> [a]
toList s = build (\c n -> toListFB c n s)

-------------------------------------------------------------------------------
-- Folds
-------------------------------------------------------------------------------

-- | @writeN n@ folds a maximum of @n@ elements from the input stream to an
-- 'Array'.
--
{-# INLINE_NORMAL writeN #-}
writeN :: forall m a. (MonadIO m, Unbox a) => Int -> Fold m a (Array a)
writeN = fmap unsafeFreeze . MA.writeN

-- | @writeNAligned alignment n@ folds a maximum of @n@ elements from the input
-- stream to an 'Array' aligned to the given size.
--
-- /Pre-release/
--
{-# INLINE_NORMAL writeNAligned #-}
writeNAligned :: forall m a. (MonadIO m, Unbox a)
    => Int -> Int -> Fold m a (Array a)
writeNAligned alignSize = fmap unsafeFreeze . MA.writeNAligned alignSize

-- | Like 'writeN' but does not check the array bounds when writing. The fold
-- driver must not call the step function more than 'n' times otherwise it will
-- corrupt the memory and crash. This function exists mainly because any
-- conditional in the step function blocks fusion causing 10x performance
-- slowdown.
--
{-# INLINE_NORMAL writeNUnsafe #-}
writeNUnsafe :: forall m a. (MonadIO m, Unbox a)
    => Int -> Fold m a (Array a)
writeNUnsafe n = unsafeFreeze <$> MA.writeNUnsafe n

{-# INLINE_NORMAL writeWith #-}
writeWith :: forall m a. (MonadIO m, Unbox a)
    => Int -> Fold m a (Array a)
-- writeWith n = FL.rmapM spliceArrays $ toArraysOf n
writeWith elemCount = unsafeFreeze <$> MA.writeWith elemCount

-- | Fold the whole input to a single array.
--
-- /Caution! Do not use this on infinite streams./
--
{-# INLINE write #-}
write :: forall m a. (MonadIO m, Unbox a) => Fold m a (Array a)
write = fmap unsafeFreeze MA.write

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

-- XXX we are assuming that Unboxed equality means element equality. This may
-- or may not be correct? arrcmp is 40% faster compared to stream equality.
instance (Unbox a, Eq a) => Eq (Array a) where
    {-# INLINE (==) #-}
    arr1 == arr2 =
        (==) EQ $ unsafeInlineIO $! unsafeThaw arr1 `MA.cmp` unsafeThaw arr2

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
        x <- peekWith arrContents p
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

instance Unbox a => Semigroup (Array a) where
    arr1 <> arr2 = unsafePerformIO $ splice arr1 arr2

nil ::
#ifdef DEVBUILD
    Unbox a =>
#endif
    Array a
nil = Array Unboxed.nil 0 0

instance Unbox a => Monoid (Array a) where
    mempty = nil
    mappend = (<>)
