{-# LANGUAGE CPP #-}
{-# LANGUAGE UnliftedFFITypes #-}
-- |
-- Module      : Streamly.Internal.Data.MutArray.Type
-- Copyright   : (c) 2020 Composewell Technologies
-- License     : BSD3-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Pinned and unpinned mutable array for 'Unboxed' types. Fulfils the following
-- goals:
--
-- * Random access (array)
-- * Efficient storage (unboxed)
-- * Performance (unboxed access)
-- * Performance - in-place operations (mutable)
-- * Performance - GC (pinned, mutable)
-- * interfacing with OS (pinned)
--
-- Stream and Fold APIs allow easy, efficient and convenient operations on
-- arrays.
--
-- Mutable arrays and file system files are quite similar, they can grow and
-- their content is mutable. Therefore, both have similar APIs as well. We
-- strive to keep the API consistent for both. Ideally, you should be able to
-- replace one with another with little changes to the code.

module Streamly.Internal.Data.MutArray.Type
    (
    -- ** Type
    -- $arrayNotes
      MutArray (..)
    , fromMutByteArray
    , toMutByteArray

    -- ** Conversion
    -- *** Pinned and Unpinned
    , pin
    , unpin
    , isPinned

    -- ** Casting
    , cast
    , unsafeCast
    , asBytes
    , unsafeAsPtr -- XXX asPtr
    , asCString
    , asCWString

    -- ** Construction
    , empty
 -- , singleton

    -- *** New
    -- | New arrays are always empty arrays with some reserve capacity to
    -- extend the length without reallocating.
    , emptyOf
    , emptyWithAligned -- XXX emptyAlignAtWith
    , emptyOf'

    -- *** Slicing
    -- | Get a subarray without copying

    -- Element agnostic.
    , unsafeSliceOffLen
    , sliceOffLen

    -- Counting from the beginning
    -- We use the name "break" for splitting into two parts. And the word
    -- "split" for splitting into possibly more than two.
    , unsafeBreakAt
    , breakAt -- called splitAt in lists
 -- , take
 -- , drop
 -- , uncons
 -- , tail

    -- Counting from the end
 -- , revBreakAt
 -- , takeEnd
 -- , dropEnd
 -- , unsnoc
 -- , init

    -- Element aware
    -- search from the beginning
    , breakEndByWord8_
    , breakEndBy
    , breakEndBy_
 -- , breakBeginBy -- called break in lists
 -- , breakSpan -- called span in lists
 -- , breakBeginBySeq -- called breakOn in text
 -- , breakSepBy_
    , dropWhile
 -- , takeWhile
 -- , stripPrefix

    -- search from the end
    , revBreakEndBy
    , revBreakEndBy_
 -- , revBreakBeginBy -- called breakOnEnd in text
    , revDropWhile -- dropWhileEnd
 -- , takeWhileEnd
 -- , stripSuffix

    , dropAround

    -- *** Stream Folds
    -- | Note: create is just appending to an empty array. So keep the names
    -- consistent with append operations.
    , ArrayUnsafe (..)

    -- With allocator, of capacity
    , unsafeCreateWithOf
    , createWithOf -- create alloc with

    , unsafeCreateOf
    , createOf
    , createMinOf
    , create -- XXX should we change the min to one elem or one Word?
 -- , createGrowBy

    -- Reverse variants
    , revCreateOf
 -- , revCreate

    -- Pinned variants

    , unsafeCreateOf'
    , createOf'
    , create'

    -- *** From containers
    -- | These can be implemented by appending a stream to an empty array.
    , clone -- XXX fromMutArray or copyMutArray
    , clone'
    , fromListN
    , fromListN'
    , fromList
    , fromList'
    , fromListRevN
    , fromListRev
    , fromStreamN
    , fromStream
    , fromPureStreamN
    , fromPureStream
    , fromCString#
    , fromW16CString#
    , fromPtrN
    , fromChunksK
    , fromChunksRealloced -- fromSmallChunks

    , unsafeCreateWithPtr'

    -- ** Random writes
    , putIndex
 -- , putIndexRev -- or revPutIndex
    , unsafePutIndex
    , putIndices
    -- , putFromThenTo
    -- , putFrom -- start writing at the given position
    -- , putUpto -- write from beginning up to the given position
    -- , putFromTo
    -- , putFromRev
    -- , putUptoRev
    , unsafeModifyIndex
    , modifyIndex
    , modifyIndices
    , modify
    , swapIndices
    , unsafeSwapIndices

    -- ** Reading

    -- *** Indexing
 -- , head
    , getIndex
    , unsafeGetIndex
    , unsafeGetIndexRev
    -- , getFromThenTo
 -- , last
    , getIndexRev -- getRevIndex?
    , indexReader
    , indexReaderWith

    -- -- *** Searching
    -- See the Data.Array module as well
    -- , binarySearch
    -- , findIndicesOf
    -- , getIndicesOf
    -- , indexFinder
    -- , findIndexOf
    -- , find
    -- , elem

    -- *** To Streams
    , read
    , readRev
    , toStreamWith
    , toStreamRevWith
    , toStreamK
    , toStreamKWith
    , toStreamKRev
    , toStreamKRevWith

    -- *** To Containers
    , toList

    -- *** Unfolds
    -- experimental
    , producerWith
    , producer

    , reader
    , readerRevWith
    , readerRev

    -- ** Size and Capacity
    -- *** Size
 -- , null
 -- , compareLength
    , length
    , byteLength

    -- *** Capacity Reporting
    , capacity
    , free
    , byteCapacity
    , bytesFree

    -- *** Capacity Management
    -- There are two ways of growing an array:
    --
    -- * grow: double, align to next power of 2 if large, never shrink
    -- * growBy: align to block size if large, never shrink

    , blockSize
    , arrayChunkBytes
    , allocBytesToElemCount
    , reallocBytes
    , reallocBytesWith

 -- , grow -- double the used capacity and align to power of 2
    , growTo
    , growBy
    , growExp
    , rightSize
    , vacate

    -- ** Folding
    , foldl'
    , foldr
    , fold
    , foldRev -- XXX revFold
    , byteCmp
    , byteEq

    -- ** In-place Mutation Algorithms
    , reverse
    , permute
    , partitionBy
    , shuffleBy
    , divideBy
    , mergeBy
    , bubble
    , rangeBy
 -- , filter

    -- ** Growing and Shrinking
    -- | Arrays grow only at the end, though technically it is possible to
    -- grow on both sides and therefore we can have a cons as well as snoc. But
    -- cons is not implemented yet.

    -- *** Appending elements
    -- | snoc is the fundamental operation for growing arrays. Streaming folds,
    -- appending streams can be implemented in terms of snoc.

    -- XXX snoc 128/256/512 bit data using SIMD.
    , snocWith -- XXX snocGrowWith
    , snoc
    , snocGrowBy
    , snocMay
    , unsafeSnoc

 -- , revSnoc -- cons
 -- , revSnocGrowBy  -- consGrowBy

    -- *** Folds for appending streams
    -- | Fundamentally these are a sequence of snoc operations.
    -- Folds are named "append" whereas joining two arrays is named as "splice".

    , appendWith -- XXX replace by pure appendGrowWith

    , unsafeAppendMax -- can be renamed to unsafeAppendN later
    , appendMax -- can be renamed to appendN later
 -- , appendMin -- like createMinOf, supplies a min hint to reduce allocs
 -- , appendGrowWith
    , append2   -- to be renamed to append later
    , appendGrowBy

 -- , revAppend
 -- , revAppendN
 -- , revAppendGrowBy

    -- *** Appending streams
    -- | Fundamentally these are a sequence of snoc operations. These are
    -- convenience operations implemented in terms of folds.
    , unsafeAppendPtrN
    , appendPtrN
    , appendCString
    , appendCString#
 -- , appendStreamGrowWith
    , appendStream
    , appendStreamN
 -- , appendStreamGrowBy

    -- *** Splicing arrays
    -- | TODO: We can replace memcpy with stream copy using Word64. Arrays are
    -- aligned on 64-bit boundaries on 64-bit CPUs. A fast way to copy an
    -- array is to unsafeCast it to Word64, read it as a stream, write the
    -- stream to Word64 array and unsafeCast it again. We can use SIMD
    -- read/write as well.

    , spliceCopy -- XXX freeze and splice instead?
    , splice
    , spliceWith -- XXX spliceGrowWith
    , spliceExp -- XXX spliceGrowExp
 -- , spliceN
 -- , spliceGrowBy
    , unsafeSplice
    -- , putSlice
    -- , appendSlice
    -- , appendSliceFrom

    -- XXX Do not expose these yet, we should perhaps expose only the Peek/Poke
    -- monads instead? Decide after implementing the monads.

    -- ** Serialization using Unbox
    -- | Fixed length serialization.
    -- Serialization operations are essentially a combination of serialization
    -- using Unbox/Serialize type class, followed by snoc. TODO: use SIMD for
    -- snoc.
    , poke
    , pokeMay
 -- , pokeGrowBy
    , unsafePokeSkip -- XXX unsafePoke_
 -- , revPoke

    -- ** Deserialization using Unbox
    -- Fixed length deserialization.
    , peek
    , unsafePeek
    , unsafePeekSkip -- XXX unsafePeek_
 -- , revPeek

    -- Arrays of arrays
    --  We can add dimensionality parameter to the array type to get
    --  multidimensional arrays. Multidimensional arrays would just be a
    --  convenience wrapper on top of single dimensional arrays.

    -- ** Streams of Arrays
    -- *** Chunk
    -- | Group a stream into arrays.
    , chunksOf
    , chunksOf' -- chunksOf'
    -- , timedChunksOf -- see the Streamly.Data.Stream.Prelude module
    , buildChunks
    , chunksEndBy
    , chunksEndBy'
    , chunksEndByLn
    , chunksEndByLn'
    -- , chunksBeginBySeq -- for parsing streams with headers

    -- *** Split
    -- | Split an array into a stream of slices.

    -- Note: some splitting APIs are in MutArray.hs
    , splitEndBy_
    , splitEndBy
 -- , splitSepBy_
 -- , splitSepBySeq
 -- , splitGroupBy
 -- , splitWordsBy

    -- *** Concat
    -- | Append the arrays in a stream to form a stream of elements.
    , concat
    -- , concatSepBy
    -- , concatEndBy
    -- , concatEndByLn -- unlines - concat a byte chunk stream using newline byte separator
    -- , concatWordsBy
    , concatWith -- internal
    , concatRev
    , concatRevWith -- internal

    -- *** Compact
    -- | Coalesce arrays together in a stream of arrays to form a stream of
    -- larger arrays.
    , SpliceState (..)
    , compactLeAs -- internal

    -- Creation folds/parsers
    , createCompactMax
    , createCompactMax'
    , createCompactMin
    , createCompactMin'

    -- Stream compaction
    , compactMin
    -- , compactMin'
    , compactExact
    -- , compactExact'

    -- Scans
    , scanCompactMin
    , scanCompactMin'

    -- ** Utilities
    , isPower2
    , roundUpToPower2

    -- * Deprecated
    , unsafeGetSlice
    , getSlice
    , sliceEndBy_
    , strip
    , stripStart
    , stripEnd
    , breakOn
    , splitAt
    , unsafeSplitAt
    , realloc
    , createOfWith
    , peekUncons
    , peekUnconsUnsafe
    , pokeAppend
    , pokeAppendMay
    , castUnsafe
    , newArrayWith
    , getSliceUnsafe
    , putIndexUnsafe
    , modifyIndexUnsafe
    , getIndexUnsafe
    , snocUnsafe
    , spliceUnsafe
    , pokeSkipUnsafe
    , peekSkipUnsafe
    , asPtrUnsafe
    , writeChunks
    , flattenArrays
    , flattenArraysRev
    , fromArrayStreamK
    , fromStreamDN
    , fromStreamD
    , cmp
    , getIndices
    , getIndicesWith
    , resize
    , resizeExp
    , nil
    , new
    , pinnedNew
    , pinnedNewBytes
    , writeAppendNUnsafe
    , writeAppendN
    , writeAppendWith
    , writeAppend
    , writeNWithUnsafe
    , writeNWith
    , writeNUnsafe
    , pinnedWriteNUnsafe
    , writeN
    , pinnedWriteN
    , pinnedWriteNAligned -- XXX not required
    , writeWith
    , write
    , pinnedWrite
    , writeRevN
    , fromByteStr#
    , pCompactLE
    , pPinnedCompactLE
    , fCompactGE
    , fPinnedCompactGE
    , lPinnedCompactGE
    , lCompactGE
    , compactGE
    , pinnedEmptyOf
    , pinnedChunksOf
    , pinnedCreateOf
    , pinnedCreate
    , pinnedFromListN
    , pinnedFromList
    , pinnedClone
    , unsafePinnedCreateOf
    , splitOn
    , pinnedNewAligned
    , unsafePinnedAsPtr
    , grow
    , createWith
    , snocLinear
    , unsafeAppendN
    , appendN
    , append
    )
where

#include "assert.hs"
#include "deprecation.h"
#include "inline.hs"
#include "ArrayMacros.h"
#include "MachDeps.h"

import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Bifunctor (first)
import Data.Bits (shiftR, (.|.), (.&.))
import Data.Char (ord)
import Data.Functor.Identity (Identity(..))
import Data.Proxy (Proxy(..))
import Data.Word (Word8, Word16)
import Foreign.C.String (CString, CWString)
import Foreign.C.Types (CSize(..), CChar, CWchar)
import Foreign.Ptr (plusPtr, castPtr)
import Streamly.Internal.Data.MutByteArray.Type
    ( MutByteArray(..)
    , PinnedState(..)
    , getMutByteArray#
    , unsafePutSlice
    , blockSize
    , largeObjectThreshold
    , unsafeByteCmp
    )
import Streamly.Internal.Data.Unbox (Unbox(..))
import GHC.Base (noinline)
import GHC.Exts (Addr#, MutableByteArray#, RealWorld)
import GHC.Ptr (Ptr(..))
import GHC.Exts (byteArrayContents#, unsafeCoerce#)

import Streamly.Internal.Data.Fold.Type (Fold(..))
import Streamly.Internal.Data.Producer.Type (Producer (..))
import Streamly.Internal.Data.Scanl.Type (Scanl (..))
import Streamly.Internal.Data.Stream.Type (Stream)
import Streamly.Internal.Data.Parser.Type (Parser (..))
import Streamly.Internal.Data.StreamK.Type (StreamK)
import Streamly.Internal.Data.SVar.Type (adaptState, defState)
import Streamly.Internal.Data.Tuple.Strict (Tuple'(..))
import Streamly.Internal.Data.Unfold.Type (Unfold(..))
import Streamly.Internal.System.IO (arrayPayloadSize, defaultChunkSize)

import qualified Streamly.Internal.Data.Fold.Type as FL
import qualified Streamly.Internal.Data.MutByteArray.Type as Unboxed
import qualified Streamly.Internal.Data.Parser.Type as Parser
-- import qualified Streamly.Internal.Data.Fold.Type as Fold
import qualified Streamly.Internal.Data.Producer as Producer
import qualified Streamly.Internal.Data.Stream.Type as D
import qualified Streamly.Internal.Data.Stream.Lift as D
import qualified Streamly.Internal.Data.Stream.Generate as D
import qualified Streamly.Internal.Data.StreamK.Type as K
import qualified Prelude

import Prelude hiding
    (Foldable(..), concat, read, unlines, splitAt, reverse, truncate, dropWhile)

#include "DocTestDataMutArray.hs"

-------------------------------------------------------------------------------
-- Foreign helpers
-------------------------------------------------------------------------------

-- NOTE: Have to be "ccall unsafe" so that we can pass unpinned memory to
-- these. For passing unpinned memory safely we have to pass unlifted byte
-- array pointers in FFI so that neither the constructor nor the array can
-- become stale if a GC kicks in at any point before the call.

foreign import ccall unsafe "string.h memcpy" c_memcpy_pinned_src
    :: MutableByteArray# RealWorld -> Ptr Word8 -> CSize -> IO (Ptr Word8)

foreign import ccall unsafe "memchr_index" c_memchr_index
    :: MutableByteArray# RealWorld -> CSize -> Word8 -> CSize -> IO CSize

-- XXX Use cstringLength# from GHC.CString in ghc-prim
foreign import ccall unsafe "string.h strlen" c_strlen_pinned
    :: Addr# -> IO CSize

-- | Given an 'Unboxed' type (unused first arg) and a number of bytes, return
-- how many elements of that type will completely fit in those bytes.
--
{-# INLINE bytesToElemCount #-}
bytesToElemCount :: forall a. Unbox a => a -> Int -> Int
bytesToElemCount _ n = n `div` SIZE_OF(a)

-------------------------------------------------------------------------------
-- MutArray Data Type
-------------------------------------------------------------------------------

-- Note on using "IO" callbacks:
--
-- The Array APIs should use "IO" callbacks instead of lifted callbacks as the
-- lifted callbacks aren't optimized properly.
--
-- See:
-- https://github.com/composewell/streamly/issues/2820
-- https://github.com/composewell/streamly/issues/2589


-- $arrayNotes
--
-- We can use an 'Unboxed' constraint in the MutArray type and the constraint
-- can be automatically provided to a function that pattern matches on the
-- MutArray type. However, it has huge performance cost, so we do not use it.
-- Investigate a GHC improvement possiblity.

-- | An unboxed mutable array. An array is created with a given length
-- and capacity. Length is the number of valid elements in the array.  Capacity
-- is the maximum number of elements that the array can be expanded to without
-- having to reallocate the memory.
--
-- The elements in the array can be mutated in-place without changing the
-- reference (constructor). However, the length of the array cannot be mutated
-- in-place.  A new array reference is generated when the length changes.  When
-- the length is increased (upto the maximum reserved capacity of the array),
-- the array is not reallocated and the new reference uses the same underlying
-- memory as the old one.
--
-- Several routines in this module allow the programmer to control the capacity
-- of the array. The programmer can control the trade-off between memory usage
-- and performance impact due to reallocations when growing or shrinking the
-- array.
--
data MutArray a =
#ifdef DEVBUILD
    Unbox a =>
#endif
    -- The array is a range into arrContents. arrContents may be a superset of
    -- the slice represented by the array. All offsets are in bytes.
    MutArray
    { arrContents :: {-# UNPACK #-} !MutByteArray
    , arrStart :: {-# UNPACK #-} !Int  -- ^ index into arrContents
    , arrEnd   :: {-# UNPACK #-} !Int  -- ^ index into arrContents
                                       -- Represents the first invalid index of
                                       -- the array.
    -- XXX rename to arrCapacity to be consistent with ring.
    , arrBound :: {-# UNPACK #-} !Int  -- ^ first invalid index of arrContents.
    }

-------------------------------------------------------------------------------
-- Construction and destructuring
-------------------------------------------------------------------------------

{-# INLINE fromMutByteArray #-}
fromMutByteArray :: MonadIO m => MutByteArray -> Int -> Int -> m (MutArray a)
fromMutByteArray arr start end = do
    len <- liftIO $ Unboxed.length arr
    return $ MutArray
        { arrContents = arr
        , arrStart = start
        , arrEnd = end
        , arrBound = len
        }

{-# INLINE toMutByteArray #-}
toMutByteArray :: MutArray a -> (MutByteArray, Int, Int)
toMutByteArray MutArray{..} = (arrContents, arrStart, arrEnd)

-------------------------------------------------------------------------------
-- Pinning & Unpinning
-------------------------------------------------------------------------------

-- | Return a copy of the array in pinned memory if unpinned, else return the
-- original array.
{-# INLINE pin #-}
pin :: MutArray a -> IO (MutArray a)
pin arr@MutArray{..} =
    if Unboxed.isPinned arrContents
    then pure arr
    else clone' arr

-- | Return a copy of the array in unpinned memory if pinned, else return the
-- original array.
{-# INLINE unpin #-}
unpin :: MutArray a -> IO (MutArray a)
unpin arr@MutArray{..} =
    if Unboxed.isPinned arrContents
    then clone arr
    else pure arr

-- | Return 'True' if the array is allocated in pinned memory.
{-# INLINE isPinned #-}
isPinned :: MutArray a -> Bool
isPinned MutArray{..} = Unboxed.isPinned arrContents

-------------------------------------------------------------------------------
-- Construction
-------------------------------------------------------------------------------

-- XXX Change the names to use "new" instead of "newArray". That way we can use
-- the same names for managed file system objects as well. For unmanaged ones
-- we can use open/create etc as usual.
--
-- A new array is similar to "touch" creating a zero length file. An mmapped
-- array would be similar to a sparse file with holes. TBD: support mmapped
-- files and arrays.

-- GHC always guarantees word-aligned memory, alignment is important only when
-- we need more than that.  See stg_pinnedNewAlignedByteArrayzh and
-- allocatePinned in GHC source.

-- XXX Rename to emptyAlignedWith, alignSize should be first arg.

-- | @emptyWithAligned allocator alignment count@ allocates a new array of zero
-- length and with a capacity to hold @count@ elements, using @allocator
-- size alignment@ as the memory allocator function.
--
-- Alignment must be greater than or equal to machine word size and a power of
-- 2.
--
-- Alignment is ignored if the allocator allocates unpinned memory.
--
-- /Pre-release/
{-# INLINE emptyWithAligned #-}
newArrayWith, emptyWithAligned :: forall m a. (MonadIO m, Unbox a)
    => (Int -> Int -> IO MutByteArray) -> Int -> Int -> m (MutArray a)
emptyWithAligned alloc alignSize count = liftIO $ do
    let size = max (count * SIZE_OF(a)) 0
    contents <- alloc size alignSize
    return $ MutArray
        { arrContents = contents
        , arrStart = 0
        , arrEnd   = 0
        , arrBound = size
        }

-- For arrays "nil" sounds a bit odd. empty is better. The only problem with
-- empty is that it is also used by the Alternative type class. But assuming we
-- will mostly import the Array module qualified this should be fine.

-- | Create an empty array.
empty ::
#ifdef DEVBUILD
    Unbox a =>
#endif
    MutArray a
empty = MutArray Unboxed.empty 0 0 0

{-# DEPRECATED nil "Please use empty instead." #-}
nil ::
#ifdef DEVBUILD
    Unbox a =>
#endif
    MutArray a
nil = empty

{-# INLINE newBytesAs #-}
newBytesAs :: MonadIO m =>
#ifdef DEVBUILD
    Unbox a =>
#endif
    PinnedState -> Int -> m (MutArray a)
newBytesAs ps bytes = do
    contents <- liftIO $ Unboxed.newAs ps bytes
    return $ MutArray
        { arrContents = contents
        , arrStart = 0
        , arrEnd   = 0
        , arrBound = bytes
        }

-- | Allocates a pinned empty array that with a reserved capacity of bytes.
-- The memory of the array is uninitialized and the allocation is aligned as
-- per the 'Unboxed' instance of the type.
--
-- > pinnedNewBytes = (unsafeCast :: Array Word8 -> a) . emptyOf'
--
-- /Pre-release/
{-# INLINE pinnedNewBytes #-}
{-# DEPRECATED pinnedNewBytes "Please use emptyOf' to create a Word8 array and cast it accordingly." #-}
pinnedNewBytes :: MonadIO m =>
#ifdef DEVBUILD
    Unbox a =>
#endif
    Int -> m (MutArray a)
pinnedNewBytes = newBytesAs Pinned

-- | Like 'emptyWithAligned' but using an allocator is a pinned memory allocator and
-- the alignment is dictated by the 'Unboxed' instance of the type.
--
-- /Internal/
{-# DEPRECATED pinnedNewAligned "Please use emptyOf' to create a Word8 array and cast it accordingly." #-}
{-# INLINE pinnedNewAligned #-}
pinnedNewAligned :: (MonadIO m, Unbox a) => Int -> Int -> m (MutArray a)
pinnedNewAligned = emptyWithAligned (\s _ -> liftIO $ Unboxed.new' s)

{-# INLINE newAs #-}
newAs :: (MonadIO m, Unbox a) => PinnedState -> Int -> m (MutArray a)
newAs ps =
    emptyWithAligned
        (\s _ -> liftIO $ Unboxed.newAs ps s)
        (error "new: alignment is not used in unpinned arrays.")

-- XXX can unaligned allocation be more efficient when alignment is not needed?

-- | Allocates a pinned array of zero length but growable to the specified
-- capacity without reallocation.
{-# INLINE emptyOf' #-}
pinnedEmptyOf, emptyOf' :: (MonadIO m, Unbox a) => Int -> m (MutArray a)
emptyOf' = newAs Pinned
RENAME_PRIME(pinnedEmptyOf,emptyOf)

{-# DEPRECATED pinnedNew "Please use emptyOf' instead." #-}
{-# INLINE pinnedNew #-}
pinnedNew :: forall m a. (MonadIO m, Unbox a) => Int -> m (MutArray a)
pinnedNew = emptyOf'

-- | Allocates an unpinned array of zero length but growable to the specified
-- capacity without reallocation.
--
{-# INLINE emptyOf #-}
emptyOf :: (MonadIO m, Unbox a) => Int -> m (MutArray a)
emptyOf = newAs Unpinned

{-# DEPRECATED new "Please use emptyOf instead." #-}
{-# INLINE new #-}
new :: (MonadIO m, Unbox a) => Int -> m (MutArray a)
new = emptyOf

-------------------------------------------------------------------------------
-- Random writes
-------------------------------------------------------------------------------

-- | Write the given element to the given index of the array. Does not check if
-- the index is out of bounds of the array.
--
-- /Pre-release/
{-# INLINE unsafePutIndex #-}
putIndexUnsafe, unsafePutIndex :: forall m a. (MonadIO m, Unbox a)
    => Int -> MutArray a -> a -> m ()
unsafePutIndex i MutArray{..} x = do
    let index = INDEX_OF(arrStart, i, a)
    assert (i >= 0 && INDEX_VALID(index, arrEnd, a)) (return ())
    liftIO $ pokeAt index arrContents  x

invalidIndex :: String -> Int -> a
invalidIndex label i =
    error $ label ++ ": invalid array index " ++ show i

-- | /O(1)/ Write the given element at the given index in the array.
-- Performs in-place mutation of the array.
--
-- >>> putIndex ix arr val = MutArray.modifyIndex ix arr (const (val, ()))
-- >>> f = MutArray.putIndices
-- >>> putIndex ix arr val = Stream.fold (f arr) (Stream.fromPure (ix, val))
--
{-# INLINE putIndex #-}
putIndex :: forall m a. (MonadIO m, Unbox a) => Int -> MutArray a -> a -> m ()
putIndex i MutArray{..} x = do
    let index = INDEX_OF(arrStart,i,a)
    if i >= 0 && INDEX_VALID(index,arrEnd,a)
    then liftIO $ pokeAt index arrContents  x
    else invalidIndex "putIndex" i

-- | Write an input stream of (index, value) pairs to an array. Throws an
-- error if any index is out of bounds.
--
-- /Pre-release/
{-# INLINE putIndices #-}
putIndices :: forall m a. (MonadIO m, Unbox a)
    => MutArray a -> Fold m (Int, a) ()
putIndices arr = FL.foldlM' step (return ())

    where

    step () (i, x) = putIndex i arr x

-- | Modify a given index of an array using a modifier function.
--
-- Unsafe because it does not check the bounds of the array.
--
-- /Pre-release/
modifyIndexUnsafe, unsafeModifyIndex :: forall m a b. (MonadIO m, Unbox a) =>
    Int -> MutArray a -> (a -> (a, b)) -> m b
unsafeModifyIndex i MutArray{..} f = liftIO $ do
        let index = INDEX_OF(arrStart,i,a)
        assert (i >= 0 && INDEX_NEXT(index,a) <= arrEnd) (return ())
        r <- peekAt index arrContents
        let (x, res) = f r
        pokeAt index arrContents  x
        return res

-- | Modify a given index of an array using a modifier function.
--
-- /Pre-release/
modifyIndex :: forall m a b. (MonadIO m, Unbox a) =>
    Int -> MutArray a -> (a -> (a, b)) -> m b
modifyIndex i MutArray{..} f = do
    let index = INDEX_OF(arrStart,i,a)
    if i >= 0 && INDEX_VALID(index,arrEnd,a)
    then liftIO $ do
        r <- peekAt index arrContents
        let (x, res) = f r
        pokeAt index arrContents  x
        return res
    else invalidIndex "modifyIndex" i

-- | Modify the array indices generated by the supplied stream.
--
-- /Pre-release/
{-# INLINE modifyIndices #-}
modifyIndices :: forall m a . (MonadIO m, Unbox a)
    => MutArray a -> (Int -> a -> a) -> Fold m Int ()
modifyIndices arr f = FL.foldlM' step initial

    where

    initial = return ()

    step () i =
        let f1 x = (f i x, ())
         in modifyIndex i arr f1

-- | Modify each element of an array using the supplied modifier function.
--
-- This is an in-place equivalent of an immutable map operation.
--
-- /Pre-release/
modify :: forall m a. (MonadIO m, Unbox a)
    => MutArray a -> (a -> a) -> m ()
modify MutArray{..} f = liftIO $
    go arrStart

    where

    go i =
        when (INDEX_VALID(i,arrEnd,a)) $ do
            r <- peekAt i arrContents
            pokeAt i arrContents (f r)
            go (INDEX_NEXT(i,a))

-- XXX We could specify the number of bytes to swap instead of Proxy. Need
-- to ensure that the memory does not overlap.
{-# INLINE swapArrayByteIndices #-}
swapArrayByteIndices ::
       forall a. Unbox a
    => Proxy a
    -> MutByteArray
    -> Int
    -> Int
    -> IO ()
swapArrayByteIndices _ arrContents i1 i2 = do
    r1 <- peekAt i1 arrContents
    r2 <- peekAt i2 arrContents
    pokeAt i1 arrContents (r2 :: a)
    pokeAt i2 arrContents (r1 :: a)

-- | Swap the elements at two indices without validating the indices.
--
-- /Unsafe/: This could result in memory corruption if indices are not valid.
--
-- /Pre-release/
{-# INLINE unsafeSwapIndices #-}
unsafeSwapIndices :: forall m a. (MonadIO m, Unbox a)
    => Int -> Int -> MutArray a -> m ()
unsafeSwapIndices i1 i2 MutArray{..} = liftIO $ do
        let t1 = INDEX_OF(arrStart,i1,a)
            t2 = INDEX_OF(arrStart,i2,a)
        swapArrayByteIndices (Proxy :: Proxy a) arrContents t1 t2

-- | Swap the elements at two indices.
--
-- /Pre-release/
swapIndices :: forall m a. (MonadIO m, Unbox a)
    => Int -> Int -> MutArray a -> m ()
swapIndices i1 i2 MutArray{..} = liftIO $ do
        let t1 = INDEX_OF(arrStart,i1,a)
            t2 = INDEX_OF(arrStart,i2,a)
        when (i1 < 0 || INDEX_INVALID(t1,arrEnd,a))
            $ invalidIndex "swapIndices" i1
        when (i2 < 0 || INDEX_INVALID(t2,arrEnd,a))
            $ invalidIndex "swapIndices" i2
        swapArrayByteIndices (Proxy :: Proxy a) arrContents t1 t2

-------------------------------------------------------------------------------
-- Rounding
-------------------------------------------------------------------------------

-- XXX Should be done only when we are using the GHC allocator.
-- | Round up an array larger than 'largeObjectThreshold' to use the whole
-- block.
{-# INLINE roundUpLargeArray #-}
roundUpLargeArray :: Int -> Int
roundUpLargeArray size =
    if size >= largeObjectThreshold
    then
        assert
            (blockSize /= 0 && ((blockSize .&. (blockSize - 1)) == 0))
            ((size + blockSize - 1) .&. negate blockSize)
    else size

{-# INLINE isPower2 #-}
isPower2 :: Int -> Bool
isPower2 n = n .&. (n - 1) == 0

{-# INLINE roundUpToPower2 #-}
roundUpToPower2 :: Int -> Int
roundUpToPower2 n =
#if WORD_SIZE_IN_BITS == 64
    1 + z6
#else
    1 + z5
#endif

    where

    z0 = n - 1
    z1 = z0 .|. z0 `shiftR` 1
    z2 = z1 .|. z1 `shiftR` 2
    z3 = z2 .|. z2 `shiftR` 4
    z4 = z3 .|. z3 `shiftR` 8
    z5 = z4 .|. z4 `shiftR` 16
    z6 = z5 .|. z5 `shiftR` 32

-- | @allocBytesToBytes elem allocatedBytes@ returns the array size in bytes
-- such that the real allocation is less than or equal to @allocatedBytes@,
-- unless @allocatedBytes@ is less than the size of one array element in which
-- case it returns one element's size.
--
{-# INLINE allocBytesToBytes #-}
allocBytesToBytes :: forall a. Unbox a => a -> Int -> Int
allocBytesToBytes _ n = max (arrayPayloadSize n) (SIZE_OF(a))

-- | Given an 'Unboxed' type (unused first arg) and real allocation size
-- (including overhead), return how many elements of that type will completely
-- fit in it, returns at least 1.
--
{-# INLINE allocBytesToElemCount #-}
allocBytesToElemCount :: Unbox a => a -> Int -> Int
allocBytesToElemCount x bytes =
    let n = bytesToElemCount x (allocBytesToBytes x bytes)
     in assert (n >= 1) n

-- | The default chunk size by which the array creation routines increase the
-- size of the array when the array is grown linearly.
arrayChunkBytes :: Int
arrayChunkBytes = 1024

-------------------------------------------------------------------------------
-- Resizing
-------------------------------------------------------------------------------

-- | Round the second argument down to multiples of the first argument.
{-# INLINE roundDownTo #-}
roundDownTo :: Int -> Int -> Int
roundDownTo elemSize size = size - (size `mod` elemSize)

-- NOTE: we are passing elemSize explicitly to avoid an Unboxed constraint.
-- Since this is not inlined, Unboxed constraint leads to dictionary passing
-- which complicates some inspection tests.
--
{-# NOINLINE reallocExplicitAs #-}
reallocExplicitAs :: PinnedState -> Int -> Int -> MutArray a -> IO (MutArray a)
reallocExplicitAs ps elemSize newCapacityInBytes MutArray{..} = do
    assertM(arrEnd <= arrBound)

    let newCapMaxInBytes = roundUpLargeArray newCapacityInBytes
        oldSizeInBytes = arrEnd - arrStart
        -- XXX Should we round up instead?
        newCapInBytes = roundDownTo elemSize newCapMaxInBytes
        newLenInBytes = min oldSizeInBytes newCapInBytes

    assert (oldSizeInBytes `mod` elemSize == 0) (return ())
    assert (newLenInBytes >= 0) (return ())
    assert (newLenInBytes `mod` elemSize == 0) (return ())

    contents <-
        Unboxed.reallocSliceAs
            ps newCapInBytes arrContents arrStart newLenInBytes

    return $ MutArray
        { arrStart = 0
        , arrContents = contents
        , arrEnd   = newLenInBytes
        , arrBound = newCapInBytes
        }

-- XXX We may also need reallocAs to allocate as pinned/unpinned explicitly. In
-- fact clone/clone' can be implemented using reallocAs.

-- | @realloc newCapacity array@ reallocates the array to the specified
-- capacity in bytes.
--
-- If the new size is less than the original array the array gets truncated.
-- If the new size is not a multiple of array element size then it is rounded
-- down to multiples of array size.  If the new size is more than
-- 'largeObjectThreshold' then it is rounded up to the block size (4K).
--
-- If the original array is pinned, the newly allocated array is also pinned.
{-# INLINABLE reallocBytes #-}
realloc, reallocBytes :: forall m a. (MonadIO m, Unbox a) => Int -> MutArray a -> m (MutArray a)
reallocBytes bytes arr =
    let ps =
            if isPinned arr
            then Pinned
            else Unpinned
     in liftIO $ reallocExplicitAs ps (SIZE_OF(a)) bytes arr

-- | @reallocBytesWith label capSizer minIncrBytes array@. The label is used
-- in error messages and the capSizer is used to determine the capacity of the
-- new array in bytes given the current byte length of the array.
reallocBytesWith :: forall m a. (MonadIO m , Unbox a) =>
       String
    -> (Int -> Int)
    -> Int
    -> MutArray a
    -> m (MutArray a)
reallocBytesWith label capSizer minIncrBytes arr = do
    let oldSizeBytes = arrEnd arr - arrStart arr
        newCapBytes = capSizer oldSizeBytes
        newSizeBytes = oldSizeBytes + minIncrBytes
        safeCapBytes = max newCapBytes newSizeBytes
    assertM(safeCapBytes >= newSizeBytes || error (badSize newSizeBytes))

    realloc safeCapBytes arr

    where

    badSize newSize =
        Prelude.concat
            [ label
            , ": new array size (in bytes) is less than required size "
            , show newSize
            , ". Please check the sizing function passed."
            ]

-- | @growTo newCapacity array@ changes the total capacity of the array so that
-- it is enough to hold the specified number of elements.  Nothing is done if
-- the specified capacity is less than the length of the array.
--
-- If the capacity is more than 'largeObjectThreshold' then it is rounded up to
-- the block size (4K).
--
-- Nothing is done if the requested capacity is <= 0.
--
-- /Pre-release/
{-# INLINE growTo #-}
growTo, grow :: forall m a. (MonadIO m, Unbox a) =>
    Int -> MutArray a -> m (MutArray a)
growTo nElems arr@MutArray{..} = do
    let req = SIZE_OF(a) * nElems
        cap = arrBound - arrStart
    if req < cap
    then return arr
    else realloc req arr

RENAME(grow,growTo)

-- | Like 'growTo' but specifies the required reserve (unused) capacity rather
-- than the total capacity. Increases the reserve capacity, if required, to at
-- least the given amount.
--
-- Nothing is done if the requested capacity is <= 0.
--
{-# INLINE growBy #-}
growBy :: forall m a. (MonadIO m, Unbox a) =>
    Int -> MutArray a -> m (MutArray a)
growBy nElems arr@MutArray{..} = do
    let req = arrEnd - arrStart + SIZE_OF(a) * nElems
        cap = arrBound - arrStart
    if req < cap
    then return arr
    else realloc req arr

{-# DEPRECATED resize "Please use growTo instead." #-}
{-# INLINE resize #-}
resize :: forall m a. (MonadIO m, Unbox a) =>
    Int -> MutArray a -> m (MutArray a)
resize = grow

-- | Like 'growTo' but if the requested byte capacity is more than
-- 'largeObjectThreshold' then it is rounded up to the closest power of 2.
--
-- Nothing is done if the requested capacity is <= 0.
--
-- /Pre-release/
{-# INLINE growExp #-}
growExp :: forall m a. (MonadIO m, Unbox a) =>
    Int -> MutArray a -> m (MutArray a)
growExp nElems arr@MutArray{..} = do
    let req = roundUpLargeArray (SIZE_OF(a) * nElems)
        req1 =
            if req > largeObjectThreshold
            then roundUpToPower2 req
            else req
        cap = arrBound - arrStart
    if req1 < cap
    then return arr
    else realloc req1 arr

{-# DEPRECATED resizeExp "Please use growExp instead." #-}
{-# INLINE resizeExp #-}
resizeExp :: forall m a. (MonadIO m, Unbox a) =>
    Int -> MutArray a -> m (MutArray a)
resizeExp = growExp

-- | Resize the allocated memory to drop any reserved free space at the end of
-- the array and reallocate it to reduce wastage.
--
-- Up to 25% wastage is allowed to avoid reallocations.  If the capacity is
-- more than 'largeObjectThreshold' then free space up to the 'blockSize' is
-- retained.
--
-- /Pre-release/
{-# INLINE rightSize #-}
rightSize :: forall m a. (MonadIO m, Unbox a) => MutArray a -> m (MutArray a)
rightSize arr@MutArray{..} = do
    assert (arrEnd <= arrBound) (return ())
    let start = arrStart
        len = arrEnd - start
        cap = arrBound - start
        target = roundUpLargeArray len
        waste = arrBound - arrEnd
    assert (target >= len) (return ())
    assert (len `mod` SIZE_OF(a) == 0) (return ())
    -- We trade off some wastage (25%) to avoid reallocations and copying.
    if target < cap && len < 3 * waste
    then realloc target arr
    else return arr

-- | Reset the array end position to start, thus truncating the array to 0
-- length, making it empty. The capacity of the array remains unchanged. The
-- array refers to the same memory as before.
{-# INLINE vacate #-}
vacate :: MutArray a -> MutArray a
vacate MutArray{..} = MutArray arrContents arrStart arrStart arrBound

-------------------------------------------------------------------------------
-- Snoc
-------------------------------------------------------------------------------

-- XXX We can possibly use a smallMutableByteArray to hold the start, end,
-- bound pointers.  Using fully mutable handle will ensure that we do not have
-- multiple references to the same array of different lengths lying around and
-- potentially misused. In that case "snoc" need not return a new array (snoc
-- :: MutArray a -> a -> m ()), it will just modify the old reference.  The array
-- length will be mutable.  This means the length function would also be
-- monadic.  Mutable arrays would behave more like files that grow in that
-- case.

-- | Snoc using a 'Ptr'. Low level reusable function.
--
-- /Internal/
{-# INLINE snocNewEnd #-}
snocNewEnd :: (MonadIO m, Unbox a) => Int -> MutArray a -> a -> m (MutArray a)
snocNewEnd newEnd arr@MutArray{..} x = liftIO $ do
    assert (newEnd <= arrBound) (return ())
    pokeAt arrEnd arrContents x
    return $ arr {arrEnd = newEnd}

-- | Really really unsafe, appends the element into the first array, may
-- cause silent data corruption or if you are lucky a segfault if the first
-- array does not have enough space to append the element.
--
-- /Internal/
{-# INLINE unsafeSnoc #-}
snocUnsafe, unsafeSnoc :: forall m a. (MonadIO m, Unbox a) =>
    MutArray a -> a -> m (MutArray a)
unsafeSnoc arr@MutArray{..} = snocNewEnd (INDEX_NEXT(arrEnd,a)) arr

-- | Like 'snoc' but does not reallocate when pre-allocated array capacity
-- becomes full.
--
-- /Internal/
{-# INLINE snocMay #-}
snocMay :: forall m a. (MonadIO m, Unbox a) =>
    MutArray a -> a -> m (Maybe (MutArray a))
snocMay arr@MutArray{..} x = do
    let newEnd = INDEX_NEXT(arrEnd,a)
    if newEnd <= arrBound
    then Just <$> snocNewEnd newEnd arr x
    else return Nothing

-- | Increments the capacity such that there is at least one unused slot even
-- if the sizer returns a size less than or equal to current size.

-- NOINLINE to move it out of the way and not pollute the instruction cache.
{-# NOINLINE snocWithRealloc #-}
snocWithRealloc :: forall m a. (MonadIO m, Unbox a) =>
       (Int -> Int)
    -> MutArray a
    -> a
    -> m (MutArray a)
snocWithRealloc sizer arr x = do
    arr1 <- reallocBytesWith "snocWith" sizer (SIZE_OF(a)) arr
    unsafeSnoc arr1 x

-- XXX sizer should use elements instead of bytes? That may increase the cost
-- but sizing is not a frequent operation.

-- | @snocWith sizer arr elem@ mutates @arr@ to append @elem@. The used length
-- of the array increases by 1.
--
-- If there is no reserved space available in @arr@ it is reallocated to a size
-- in bytes determined by the @sizer oldSizeBytes@ function, where
-- @oldSizeBytes@ is the original size of the array in bytes. The sizer
-- function should return a capacity more than or equal to the current used
-- size. If the capacity returned is less than or equal to the current used
-- size, the array is still grown by one element.
--
-- If the new array size is more than 'largeObjectThreshold' then it is rounded
-- up to 'blockSize'.
--
-- Note that the returned array may be a mutated version of the original array.
--
-- /Pre-release/
{-# INLINE snocWith #-}
snocWith :: forall m a. (MonadIO m, Unbox a) =>
       (Int -> Int)
    -> MutArray a
    -> a
    -> m (MutArray a)
snocWith sizer arr x = do
    let newEnd = INDEX_NEXT(arrEnd arr,a)
    if newEnd <= arrBound arr
    then snocNewEnd newEnd arr x
    else snocWithRealloc sizer arr x

-- | The array is mutated to append an additional element to it. If there
-- is no reserved space available in the array then it is reallocated to grow
-- it by 'arrayChunkBytes' rounded up to 'blockSize' when the size becomes more
-- than 'largeObjectThreshold'.
--
-- Note that the returned array may be a mutated version of the original array.
--
-- Performs O(n^2) copies to grow but is thrifty on memory.
--
-- /Pre-release/
{-# DEPRECATED snocLinear "Please use snocGrowBy instead. snocLinear ~ snocGrowBy (1024 / sizeOf (Proxy :: Proxy a) + 1)" #-}
{-# INLINE snocLinear #-}
snocLinear :: forall m a. (MonadIO m, Unbox a) => MutArray a -> a -> m (MutArray a)
snocLinear = snocWith (+ allocBytesToBytes (undefined :: a) arrayChunkBytes)

-- | The array is mutated to append an additional element to it.
--
-- If there is no reserved space available in the array then it is reallocated
-- to grow it by adding space for the requested number of elements, the new
-- size is rounded up to 'blockSize' when the size becomes more than
-- 'largeObjectThreshold'. If the size specified is <= 0 then the array is
-- grown by one element.
--
-- Note that the returned array may be a mutated version of the original array.
--
-- Performs O(n^2) copies to grow but is thrifty on memory compared to 'snoc'.
--
-- /Pre-release/
{-# INLINE snocGrowBy #-}
snocGrowBy :: forall m a. (MonadIO m, Unbox a) =>
    Int -> MutArray a -> a -> m (MutArray a)
snocGrowBy n = snocWith (+ (n * SIZE_OF(a)))

-- | The array is mutated to append an additional element to it. If there is no
-- reserved space available in the array then it is reallocated to double the
-- original size and aligned to a power of 2.
--
-- This is useful to reduce allocations when appending unknown number of
-- elements.
--
-- Note that the returned array may be a mutated version of the original array.
--
-- Performs only O(n * log n) copies to grow, but is liberal with memory
-- allocation compared to 'snocGrowBy'.
--
{-# INLINE snoc #-}
snoc :: forall m a. (MonadIO m, Unbox a) => MutArray a -> a -> m (MutArray a)
snoc = snocWith f

    where

    f oldSize =
        if isPower2 oldSize
        then oldSize * 2
        else roundUpToPower2 oldSize * 2

-------------------------------------------------------------------------------
-- Serialization/Deserialization using Unbox
-------------------------------------------------------------------------------

{-# INLINE pokeNewEnd #-}
pokeNewEnd :: (MonadIO m, Unbox a) =>
    Int -> MutArray Word8 -> a -> m (MutArray Word8)
pokeNewEnd newEnd arr@MutArray{..} x = liftIO $ do
    assert (newEnd <= arrBound) (return ())
    liftIO $ pokeAt arrEnd arrContents x
    return $ arr {arrEnd = newEnd}

-- | Really really unsafe, unboxes a Haskell type and appends the resulting
-- bytes to the byte array, may cause silent data corruption or if you are
-- lucky a segfault if the array does not have enough space to append the
-- element.
--
-- /Internal/
{-# INLINE unsafePoke #-}
unsafePoke :: forall m a. (MonadIO m, Unbox a) =>
    MutArray Word8 -> a -> m (MutArray Word8)
unsafePoke arr@MutArray{..} = pokeNewEnd (arrEnd + SIZE_OF(a)) arr

-- | Skip the specified number of bytes in the array. The data in the skipped
-- region remains uninitialzed.
{-# INLINE unsafePokeSkip #-}
pokeSkipUnsafe, unsafePokeSkip :: Int -> MutArray Word8 -> MutArray Word8
unsafePokeSkip n arr@MutArray{..} =  do
    let newEnd = arrEnd + n
     in assert (newEnd <= arrBound) (arr {arrEnd = newEnd})

-- | Like 'poke' but does not grow the array when pre-allocated array
-- capacity becomes full.
--
-- /Internal/
{-# INLINE pokeMay #-}
pokeAppendMay, pokeMay :: forall m a. (MonadIO m, Unbox a) =>
    MutArray Word8 -> a -> m (Maybe (MutArray Word8))
pokeMay arr@MutArray{..} x = liftIO $ do
    let newEnd = arrEnd + SIZE_OF(a)
    if newEnd <= arrBound
    then Just <$> pokeNewEnd newEnd arr x
    else return Nothing

{-# NOINLINE pokeWithRealloc #-}
pokeWithRealloc :: forall m a. (MonadIO m, Unbox a) =>
       (Int -> Int)
    -> MutArray Word8
    -> a
    -> m (MutArray Word8)
pokeWithRealloc sizer arr x = do
    arr1 <- liftIO $ reallocBytesWith "pokeWithRealloc" sizer (SIZE_OF(a)) arr
    unsafePoke arr1 x

{-# INLINE pokeWith #-}
pokeWith :: forall m a. (MonadIO m, Unbox a) =>
       (Int -> Int)
    -> MutArray Word8
    -> a
    -> m (MutArray Word8)
pokeWith allocSize arr x = liftIO $ do
    let newEnd = arrEnd arr + SIZE_OF(a)
    if newEnd <= arrBound arr
    then pokeNewEnd newEnd arr x
    else pokeWithRealloc allocSize arr x

-- | Unbox a Haskell type and append the resulting bytes to a mutable byte
-- array. The array is grown exponentially when more space is needed.
--
-- Like 'snoc' except that the value is unboxed to the byte array.
--
-- Note: If you are serializing a large number of small fields, and the types
-- are statically known, then it may be more efficient to declare a record of
-- those fields and derive an 'Unbox' instance of the entire record.
--
{-# INLINE poke #-}
pokeAppend, poke :: forall m a. (MonadIO m, Unbox a) =>
    MutArray Word8 -> a -> m (MutArray Word8)
poke = pokeWith f

    where

    f oldSize =
        if isPower2 oldSize
        then oldSize * 2
        else roundUpToPower2 oldSize * 2

-- | Really really unsafe, create a Haskell value from an unboxed byte array,
-- does not check if the array is big enough, may return garbage or if you are
-- lucky may cause a segfault.
--
-- /Internal/
{-# INLINE unsafePeek #-}
peekUnconsUnsafe, unsafePeek :: forall m a. (MonadIO m, Unbox a) =>
    MutArray Word8 -> m (a, MutArray Word8)
unsafePeek MutArray{..} = do
    let start1 = arrStart + SIZE_OF(a)
    assert (start1 <= arrEnd) (return ())
    liftIO $ do
        r <- peekAt arrStart arrContents
        return (r, MutArray arrContents start1 arrEnd arrBound)

-- | Discard the specified number of bytes at the beginning of the array.
{-# INLINE unsafePeekSkip #-}
peekSkipUnsafe, unsafePeekSkip :: Int -> MutArray Word8 -> MutArray Word8
unsafePeekSkip n MutArray{..} =
    let start1 = arrStart + n
     in assert (start1 <= arrEnd) (MutArray arrContents start1 arrEnd arrBound)

-- | Create a Haskell value from its unboxed representation from the head of a
-- byte array, return the value and the remaining array.
--
-- Like 'uncons' except that the value is deserialized from the byte array.
--
-- Note: If you are deserializing a large number of small fields, and the types
-- are statically known, then it may be more efficient to declare a record of
-- those fields and derive an 'Unbox' instance of the entire record.
{-# INLINE peek #-}
peekUncons, peek :: forall m a. (MonadIO m, Unbox a) =>
    MutArray Word8 -> m (Maybe a, MutArray Word8)
peek arr@MutArray{..} = do
    let start1 = arrStart + SIZE_OF(a)
    if start1 > arrEnd
    then return (Nothing, arr)
    else liftIO $ do
        r <- peekAt arrStart arrContents
        return (Just r, MutArray arrContents start1 arrEnd arrBound)

-------------------------------------------------------------------------------
-- Random reads
-------------------------------------------------------------------------------

-- XXX Can this be deduplicated with array/foreign

-- | Return the element at the specified index without checking the bounds.
--
-- Unsafe because it does not check the bounds of the array.
{-# INLINE_NORMAL unsafeGetIndex #-}
getIndexUnsafe, unsafeGetIndex :: forall m a. (MonadIO m, Unbox a) => Int -> MutArray a -> m a
unsafeGetIndex i MutArray{..} = do
    let index = INDEX_OF(arrStart,i,a)
    assert (i >= 0 && INDEX_VALID(index,arrEnd,a)) (return ())
    liftIO $ peekAt index arrContents

-- | /O(1)/ Lookup the element at the given index. Index starts from 0.
--
{-# INLINE getIndex #-}
getIndex :: forall m a. (MonadIO m, Unbox a) => Int -> MutArray a -> m (Maybe a)
getIndex i MutArray{..} = do
    let index = INDEX_OF(arrStart,i,a)
    if i >= 0 && INDEX_VALID(index,arrEnd,a)
    then liftIO $ Just <$> peekAt index arrContents
    else return Nothing

{-# INLINE_NORMAL unsafeGetIndexRev #-}
unsafeGetIndexRev :: forall m a. (MonadIO m, Unbox a) =>
    Int -> MutArray a -> m a
unsafeGetIndexRev i MutArray{..} = do
    let index = RINDEX_OF(arrEnd,i,a)
    assert (i >= 0 && INDEX_VALID(index,arrEnd,a)) (return ())
    liftIO $ peekAt index arrContents

-- | /O(1)/ Lookup the element at the given index from the end of the array.
-- Index starts from 0.
--
-- Slightly faster than computing the forward index and using getIndex.
--
{-# INLINE getIndexRev #-}
getIndexRev :: forall m a. (MonadIO m, Unbox a) => Int -> MutArray a -> m a
getIndexRev i MutArray{..} = do
    let index = RINDEX_OF(arrEnd,i,a)
    if i >= 0 && index >= arrStart
    then liftIO $ peekAt index arrContents
    else invalidIndex "getIndexRev" i

data GetIndicesState contents start end st =
    GetIndicesState contents start end st

{-# INLINE indexReaderWith #-}
indexReaderWith :: (Monad m, Unbox a) =>
    (forall b. IO b -> m b) -> D.Stream m Int -> Unfold m (MutArray a) a
indexReaderWith liftio (D.Stream stepi sti) = Unfold step inject

    where

    inject (MutArray contents start end _) =
        return $ GetIndicesState contents start end sti

    {-# INLINE_LATE step #-}
    step (GetIndicesState contents start end st) = do
        r <- stepi defState st
        case r of
            D.Yield i s -> do
                x <- liftio $ getIndex i (MutArray contents start end undefined)
                case x of
                    Just v -> return $ D.Yield v (GetIndicesState contents start end s)
                    Nothing -> error "Invalid Index"
            D.Skip s -> return $ D.Skip (GetIndicesState contents start end s)
            D.Stop -> return D.Stop

{-# DEPRECATED getIndicesWith "Please use indexReaderWith instead." #-}
{-# INLINE getIndicesWith #-}
getIndicesWith :: (Monad m, Unbox a) =>
    (forall b. IO b -> m b) -> D.Stream m Int -> Unfold m (MutArray a) a
getIndicesWith = indexReaderWith

-- | Given an unfold that generates array indices, read the elements on those
-- indices from the supplied MutArray. An error is thrown if an index is out of
-- bounds.
--
-- /Pre-release/
{-# INLINE indexReader #-}
indexReader :: (MonadIO m, Unbox a) => Stream m Int -> Unfold m (MutArray a) a
indexReader = indexReaderWith liftIO

-- XXX DO NOT REMOVE, change the signature to use Stream instead of unfold
{-# DEPRECATED getIndices "Please use indexReader instead." #-}
{-# INLINE getIndices #-}
getIndices :: (MonadIO m, Unbox a) => Stream m Int -> Unfold m (MutArray a) a
getIndices = indexReader

-------------------------------------------------------------------------------
-- Subarrays
-------------------------------------------------------------------------------

-- XXX We can also get immutable slices.
-- XXX sliceFromLen for a stream of slices starting from a given index

-- | /O(1)/ Slice an array in constant time.
--
-- Unsafe: The bounds of the slice are not checked.
--
-- /Unsafe/
--
-- /Pre-release/
{-# INLINE unsafeSliceOffLen #-}
unsafeSliceOffLen, getSliceUnsafe, unsafeGetSlice :: forall a. Unbox a
    => Int -- ^ from index
    -> Int -- ^ length of the slice
    -> MutArray a
    -> MutArray a
unsafeSliceOffLen index len (MutArray contents start e _) =
    let fp1 = INDEX_OF(start,index,a)
        end = fp1 + (len * SIZE_OF(a))
     in assert
            (index >= 0 && len >= 0 && end <= e)
            -- Note: In a slice we always use bound = end so that the slice
            -- user cannot overwrite elements beyond the end of the slice.
            (MutArray contents fp1 end end)

-- | /O(1)/ Get a reference to a slice from a mutable array. Throws an error if
-- the slice extends out of the array bounds.
--
-- The capacity of the slice is the same as its length i.e. it does not have
-- any unused or reserved space at the end.
--
-- The slice shares the same underlying mutable array when created. However, if
-- the slice or the original array is reallocated by growing or shrinking then
-- it will be copied to new memory and they will no longer share the same
-- memory.
--
-- /Pre-release/
{-# INLINE sliceOffLen #-}
sliceOffLen, getSlice :: forall a. Unbox a =>
       Int -- ^ from index
    -> Int -- ^ length of the slice
    -> MutArray a
    -> MutArray a
sliceOffLen index len (MutArray contents start e _) =
    let fp1 = INDEX_OF(start,index,a)
        end = fp1 + (len * SIZE_OF(a))
     in if index >= 0 && len >= 0 && end <= e
        -- Note: In a slice we always use bound = end so that the slice user
        -- cannot overwrite elements beyond the end of the slice.
        then MutArray contents fp1 end end
        else error
                $ "sliceOffLen: invalid slice, index "
                ++ show index ++ " length " ++ show len

-------------------------------------------------------------------------------
-- In-place mutation algorithms
-------------------------------------------------------------------------------

-- XXX consider the bulk update/accumulation/permutation APIs from vector.

-- | You may not need to reverse an array because you can consume it in reverse
-- using 'readerRev'. To reverse large arrays you can read in reverse and write
-- to another array. However, in-place reverse can be useful to take adavantage
-- of cache locality and when you do not want to allocate additional memory.
--
{-# INLINE reverse #-}
reverse :: forall m a. (MonadIO m, Unbox a) => MutArray a -> m ()
reverse MutArray{..} = liftIO $ do
    let l = arrStart
        h = INDEX_PREV(arrEnd,a)
     in swap l h

    where

    swap l h = do
        when (l < h) $ do
            swapArrayByteIndices (Proxy :: Proxy a) arrContents l h
            swap (INDEX_NEXT(l,a)) (INDEX_PREV(h,a))

-- | Generate the next permutation of the sequence, returns False if this is
-- the last permutation.
--
-- /Unimplemented/
{-# INLINE permute #-}
permute :: MutArray a -> m Bool
permute = undefined

-- | Partition an array into two halves using a partitioning predicate. The
-- first half retains values where the predicate is 'False' and the second half
-- retains values where the predicate is 'True'.
--
-- /Pre-release/
{-# INLINE partitionBy #-}
partitionBy :: forall m a. (MonadIO m, Unbox a)
    => (a -> Bool) -> MutArray a -> m (MutArray a, MutArray a)
partitionBy f arr@MutArray{..} = liftIO $ do
    if arrStart >= arrEnd
    then return (arr, arr)
    else do
        ptr <- go arrStart (INDEX_PREV(arrEnd,a))
        let pl = MutArray arrContents arrStart ptr ptr
            pr = MutArray arrContents ptr arrEnd arrEnd
        return (pl, pr)

    where

    -- Invariant low < high on entry, and on return as well
    moveHigh low high = do
        h <- peekAt high arrContents
        if f h
        then
            -- Correctly classified, continue the loop
            let high1 = INDEX_PREV(high,a)
             in if low == high1
                then return Nothing
                else moveHigh low high1
        else return (Just (high, h)) -- incorrectly classified

    -- Keep a low pointer starting at the start of the array (first partition)
    -- and a high pointer starting at the end of the array (second partition).
    -- Keep incrementing the low ptr and decrementing the high ptr until both
    -- are wrongly classified, at that point swap the two and continue until
    -- the two pointer cross each other.
    --
    -- Invariants when entering this loop:
    -- low <= high
    -- Both low and high are valid locations within the array
    go low high = do
        l <- peekAt low arrContents
        if f l
        then
            -- low is wrongly classified
            if low == high
            then return low
            else do -- low < high
                r <- moveHigh low high
                case r of
                    Nothing -> return low
                    Just (high1, h) -> do -- low < high1
                        pokeAt low arrContents h
                        pokeAt high1 arrContents l
                        let low1 = INDEX_NEXT(low,a)
                            high2 = INDEX_PREV(high1,a)
                        if low1 <= high2
                        then go low1 high2
                        else return low1 -- low1 > high2

        else do
            -- low is correctly classified
            let low1 = INDEX_NEXT(low,a)
            if low == high
            then return low1
            else go low1 high

-- | Shuffle corresponding elements from two arrays using a shuffle function.
-- If the shuffle function returns 'False' then do nothing otherwise swap the
-- elements. This can be used in a bottom up fold to shuffle or reorder the
-- elements.
--
-- /Unimplemented/
{-# INLINE shuffleBy #-}
shuffleBy :: (a -> a -> m Bool) -> MutArray a -> MutArray a -> m ()
shuffleBy = undefined

-- XXX we can also make the folds partial by stopping at a certain level.
--
-- | @divideBy level partition array@  performs a top down hierarchical
-- recursive partitioning fold of items in the container using the given
-- function as the partition function.  Level indicates the level in the tree
-- where the fold would stop.
--
-- This performs a quick sort if the partition function is
-- 'partitionBy (< pivot)'.
--
-- /Unimplemented/
{-# INLINABLE divideBy #-}
divideBy ::
    Int -> (MutArray a -> m (MutArray a, MutArray a)) -> MutArray a -> m ()
divideBy = undefined

-- | @mergeBy level merge array@ performs a pairwise bottom up fold recursively
-- merging the pairs using the supplied merge function. Level indicates the
-- level in the tree where the fold would stop.
--
-- This performs a random shuffle if the merge function is random.  If we
-- stop at level 0 and repeatedly apply the function then we can do a bubble
-- sort.
--
-- /Unimplemented/
mergeBy :: Int -> (MutArray a -> MutArray a -> m ()) -> MutArray a -> m ()
mergeBy = undefined

-- XXX Use vector instructions in arrays to find min/max/range faster

-- XXX If we can mutate the array then we can do pairwise processing to keep
-- min in the first slot and max in the second. Then compare adjacent mins and
-- keep the min of those in the first slot, and similarly for max. Thus
-- reducing the comparisons in binary fashion.
--
-- Or we can use mergeBy as defined above.
--
-- If we cannot mutate the array then we can (1) copy it and use the above
-- algo, or (2) stream the array and use pairwise concat.

-- | Find the minimum and maximum elements in the array using the provided
-- comparison function.
rangeBy :: (a -> a -> Ordering) -> MutArray a -> IO (Maybe (a, a))
rangeBy = undefined

-------------------------------------------------------------------------------
-- Size
-------------------------------------------------------------------------------

-- | /O(1)/ Get the byte length of the array.
--
{-# INLINE byteLength #-}
byteLength :: MutArray a -> Int
byteLength MutArray{..} =
    let len = arrEnd - arrStart
    in assert (len >= 0) len

-- Note: try to avoid the use of length in performance sensitive internal
-- routines as it involves a costly 'div' operation. Instead use the end ptr
-- in the array to check the bounds etc.

-- | /O(1)/ Get the used length of the array i.e. the number of elements in the
-- array.
--
-- Note that 'byteLength' is less expensive than this operation, as 'length'
-- involves a costly division operation.
--
{-# INLINE length #-}
length :: forall a. Unbox a => MutArray a -> Int
length arr =
    let elemSize = SIZE_OF(a)
        blen = byteLength arr
     in assert (blen `mod` elemSize == 0) (blen `div` elemSize)

-- | Get the total capacity of an array. An array may have space reserved
-- beyond the current used length of the array.
--
-- /Pre-release/
{-# INLINE byteCapacity #-}
byteCapacity :: MutArray a -> Int
byteCapacity MutArray{..} =
    let len = arrBound - arrStart
    in assert (len >= 0) len

-- | The remaining capacity in the array for appending more elements without
-- reallocation.
--
-- /Pre-release/
{-# INLINE bytesFree #-}
bytesFree :: MutArray a -> Int
bytesFree MutArray{..} =
    let n = arrBound - arrEnd
    in assert (n >= 0) n

{-# INLINE capacity #-}
capacity :: forall a. Unbox a => MutArray a -> Int
capacity arr =
    let elemSize = SIZE_OF(a)
        bcap = byteCapacity arr
     in assert (bcap `mod` elemSize == 0) (bcap `div` elemSize)

{-# INLINE free #-}
free :: forall a. Unbox a => MutArray a -> Int
free arr =
    let elemSize = SIZE_OF(a)
        bfree = bytesFree arr
     in assert (bfree `mod` elemSize == 0) (bfree `div` elemSize)

-------------------------------------------------------------------------------
-- Streams of arrays - Creation
-------------------------------------------------------------------------------

data GroupState s contents start end bound
    = GroupStart s
    | GroupBuffer s contents start end bound
    | GroupYield
        contents start end bound (GroupState s contents start end bound)
    | GroupFinish

{-# INLINE_NORMAL chunksOfAs #-}
chunksOfAs :: forall m a. (MonadIO m, Unbox a)
    => PinnedState -> Int -> D.Stream m a -> D.Stream m (MutArray a)
chunksOfAs ps n (D.Stream step state) =
    D.Stream step' (GroupStart state)

    where

    {-# INLINE_LATE step' #-}
    step' _ (GroupStart st) = do
        when (n <= 0) $
            -- XXX we can pass the module string from the higher level API
            error $ "Streamly.Internal.Data.MutArray.Mut.Type.chunksOf: "
                    ++ "the size of arrays [" ++ show n
                    ++ "] must be a natural number"
        (MutArray contents start end bound :: MutArray a) <- newAs ps n
        return $ D.Skip (GroupBuffer st contents start end bound)

    step' gst (GroupBuffer st contents start end bound) = do
        r <- step (adaptState gst) st
        case r of
            D.Yield x s -> do
                liftIO $ pokeAt end contents  x
                let end1 = INDEX_NEXT(end,a)
                return $
                    if end1 >= bound
                    then D.Skip
                            (GroupYield
                                contents start end1 bound (GroupStart s))
                    else D.Skip (GroupBuffer s contents start end1 bound)
            D.Skip s ->
                return $ D.Skip (GroupBuffer s contents start end bound)
            D.Stop ->
                return
                    $ D.Skip (GroupYield contents start end bound GroupFinish)

    step' _ (GroupYield contents start end bound next) =
        return $ D.Yield (MutArray contents start end bound) next

    step' _ GroupFinish = return D.Stop

-- | @chunksOf n stream@ groups the elements in the input stream into arrays of
-- @n@ elements each.
--
-- Same as the following but may be more efficient:
--
-- >>> chunksOf n = Stream.foldMany (MutArray.createOf n)
--
-- /Pre-release/
{-# INLINE_NORMAL chunksOf #-}
chunksOf :: forall m a. (MonadIO m, Unbox a)
    => Int -> D.Stream m a -> D.Stream m (MutArray a)
-- XXX the idiomatic implementation leads to large regression in the D.reverse'
-- benchmark. It seems it has difficulty producing optimized code when
-- converting to StreamK. Investigate GHC optimizations.
-- chunksOf n = D.foldMany (createOf n)
chunksOf = chunksOfAs Unpinned

-- | Like 'chunksOf' but creates pinned arrays.
{-# INLINE_NORMAL chunksOf' #-}
pinnedChunksOf, chunksOf' :: forall m a. (MonadIO m, Unbox a)
    => Int -> D.Stream m a -> D.Stream m (MutArray a)
-- chunksOf' n = D.foldMany (createOf' n)
chunksOf' = chunksOfAs Pinned
RENAME_PRIME(pinnedChunksOf,chunksOf)

-- | Create arrays from the input stream using a predicate to find the end of
-- the chunk. When the predicate matches, the chunk ends, the matching element
-- is included in the chunk.
--
--  Definition:
--
-- >>> chunksEndBy p = Stream.foldMany (Fold.takeEndBy p MutArray.create)
--
{-# INLINE chunksEndBy #-}
chunksEndBy :: forall m a. (MonadIO m, Unbox a)
    => (a -> Bool) -> D.Stream m a -> D.Stream m (MutArray a)
chunksEndBy p = D.foldMany (FL.takeEndBy p create)

-- | Like 'chunksEndBy' but creates pinned arrays.
--
{-# INLINE chunksEndBy' #-}
chunksEndBy' :: forall m a. (MonadIO m, Unbox a)
    => (a -> Bool) -> D.Stream m a -> D.Stream m (MutArray a)
chunksEndBy' p = D.foldMany (FL.takeEndBy p create')

-- | Create chunks using newline as the separator, including it.
{-# INLINE chunksEndByLn #-}
chunksEndByLn :: (MonadIO m)
    => D.Stream m Word8 -> D.Stream m (MutArray Word8)
chunksEndByLn = chunksEndBy (== fromIntegral (ord '\n'))

-- | Like 'chunksEndByLn' but creates pinned arrays.
{-# INLINE chunksEndByLn' #-}
chunksEndByLn' :: (MonadIO m)
    => D.Stream m Word8 -> D.Stream m (MutArray Word8)
chunksEndByLn' = chunksEndBy' (== fromIntegral (ord '\n'))

-- | When we are buffering a stream of unknown size into an array we do not
-- know how much space to pre-allocate. So we start with the min size and emit
-- the array then keep on doubling the size every time. Thus we do not need to
-- guess the optimum chunk size.
--
-- We can incorporate this in chunksOfAs if the additional size parameter does
-- not impact perf.
--
{-# INLINE _chunksOfRange #-}
_chunksOfRange :: -- (MonadIO m, Unbox a) =>
    PinnedState -> Int -> Int -> D.Stream m a -> D.Stream m (MutArray a)
_chunksOfRange _ps _low _hi = undefined

-- XXX buffer to a list instead?
-- | Buffer the stream into arrays in memory.
{-# INLINE arrayStreamKFromStreamDAs #-}
arrayStreamKFromStreamDAs :: forall m a. (MonadIO m, Unbox a) =>
    PinnedState -> D.Stream m a -> m (StreamK m (MutArray a))
arrayStreamKFromStreamDAs ps =
    let n = allocBytesToElemCount (undefined :: a) defaultChunkSize
     in D.foldr K.cons K.nil . chunksOfAs ps n

-------------------------------------------------------------------------------
-- Streams of arrays - Flattening
-------------------------------------------------------------------------------

data FlattenState s contents a =
      OuterLoop s
    | InnerLoop s contents !Int !Int

{-# INLINE_NORMAL concatWith #-}
concatWith :: forall m a. (Monad m, Unbox a)
    => (forall b. IO b -> m b) -> D.Stream m (MutArray a) -> D.Stream m a
concatWith liftio (D.Stream step state) = D.Stream step' (OuterLoop state)

    where

    {-# INLINE_LATE step' #-}
    step' gst (OuterLoop st) = do
        r <- step (adaptState gst) st
        return $ case r of
            D.Yield MutArray{..} s ->
                D.Skip (InnerLoop s arrContents arrStart arrEnd)
            D.Skip s -> D.Skip (OuterLoop s)
            D.Stop -> D.Stop

    step' _ (InnerLoop st _ p end) | assert (p <= end) (p == end) =
        return $ D.Skip $ OuterLoop st

    step' _ (InnerLoop st contents p end) = do
        !x <- liftio $ peekAt p contents
        return $ D.Yield x (InnerLoop st contents (INDEX_NEXT(p,a)) end)

-- | Same as the following but may be more efficient due to better fusion:
--
-- >>> concat = Stream.unfoldEach MutArray.reader
--
{-# INLINE_NORMAL concat #-}
concat :: forall m a. (MonadIO m, Unbox a)
    => D.Stream m (MutArray a) -> D.Stream m a
concat = concatWith liftIO

{-# DEPRECATED flattenArrays "Please use \"unfoldMany reader\" instead." #-}
{-# INLINE flattenArrays #-}
flattenArrays :: forall m a. (MonadIO m, Unbox a)
    => D.Stream m (MutArray a) -> D.Stream m a
flattenArrays = concat

{-# INLINE_NORMAL concatRevWith #-}
concatRevWith :: forall m a. (Monad m, Unbox a)
    => (forall b. IO b -> m b) -> D.Stream m (MutArray a) -> D.Stream m a
concatRevWith liftio (D.Stream step state) = D.Stream step' (OuterLoop state)

    where

    {-# INLINE_LATE step' #-}
    step' gst (OuterLoop st) = do
        r <- step (adaptState gst) st
        return $ case r of
            D.Yield MutArray{..} s ->
                let p = INDEX_PREV(arrEnd,a)
                 in D.Skip (InnerLoop s arrContents p arrStart)
            D.Skip s -> D.Skip (OuterLoop s)
            D.Stop -> D.Stop

    step' _ (InnerLoop st _ p start) | p < start =
        return $ D.Skip $ OuterLoop st

    step' _ (InnerLoop st contents p start) = do
        !x <- liftio $ peekAt p contents
        let cur = INDEX_PREV(p,a)
        return $ D.Yield x (InnerLoop st contents cur start)

-- | Use the "readerRev" unfold instead.
--
-- @concat = unfoldMany readerRev@
--
-- We can try this if there are any fusion issues in the unfold.
--
{-# INLINE_NORMAL concatRev #-}
concatRev :: forall m a. (MonadIO m, Unbox a)
    => D.Stream m (MutArray a) -> D.Stream m a
concatRev = concatRevWith liftIO

{-# DEPRECATED flattenArraysRev "Please use \"unfoldMany readerRev\" instead." #-}
{-# INLINE flattenArraysRev #-}
flattenArraysRev :: forall m a. (MonadIO m, Unbox a)
    => D.Stream m (MutArray a) -> D.Stream m a
flattenArraysRev = concatRev

-------------------------------------------------------------------------------
-- Unfolds
-------------------------------------------------------------------------------

data ArrayUnsafe a = ArrayUnsafe
    {-# UNPACK #-} !MutByteArray   -- contents
    {-# UNPACK #-} !Int                -- index 1
    {-# UNPACK #-} !Int                -- index 2

toArrayUnsafe :: MutArray a -> ArrayUnsafe a
toArrayUnsafe (MutArray contents start end _) = ArrayUnsafe contents start end

fromArrayUnsafe ::
#ifdef DEVBUILD
    Unbox a =>
#endif
    ArrayUnsafe a -> MutArray a
fromArrayUnsafe (ArrayUnsafe contents start end) =
         MutArray contents start end end

{-# INLINE_NORMAL producerWith #-}
producerWith ::
       forall m a. (Monad m, Unbox a)
    => (forall b. IO b -> m b) -> Producer m (MutArray a) a
producerWith liftio = Producer step (return . toArrayUnsafe) extract
    where

    {-# INLINE_LATE step #-}
    step (ArrayUnsafe _ cur end)
        | assert (cur <= end) (cur == end) = return D.Stop
    step (ArrayUnsafe contents cur end) = do
            -- When we use a purely lazy Monad like Identity, we need to force a
            -- few actions for correctness and execution order sanity. We want
            -- the peek to occur right here and not lazily at some later point
            -- because we want the peek to be ordered with respect to the touch.
            !x <- liftio $ peekAt cur contents
            return $ D.Yield x (ArrayUnsafe contents (INDEX_NEXT(cur,a)) end)

    extract = return . fromArrayUnsafe

-- | Resumable unfold of an array.
--
{-# INLINE_NORMAL producer #-}
producer :: forall m a. (MonadIO m, Unbox a) => Producer m (MutArray a) a
producer = producerWith liftIO

-- | Unfold an array into a stream.
--
{-# INLINE_NORMAL reader #-}
reader :: forall m a. (MonadIO m, Unbox a) => Unfold m (MutArray a) a
reader = Producer.simplify producer

{-# INLINE_NORMAL readerRevWith #-}
readerRevWith ::
       forall m a. (Monad m, Unbox a)
    => (forall b. IO b -> m b) -> Unfold m (MutArray a) a
readerRevWith liftio = Unfold step inject
    where

    inject (MutArray contents start end _) =
        let p = INDEX_PREV(end,a)
         in return $ ArrayUnsafe contents start p

    {-# INLINE_LATE step #-}
    step (ArrayUnsafe _ start p) | p < start = return D.Stop
    step (ArrayUnsafe contents start p) = do
        !x <- liftio $ peekAt p contents
        return $ D.Yield x (ArrayUnsafe contents start (INDEX_PREV(p,a)))

-- | Unfold an array into a stream in reverse order.
--
{-# INLINE_NORMAL readerRev #-}
readerRev :: forall m a. (MonadIO m, Unbox a) => Unfold m (MutArray a) a
readerRev = readerRevWith liftIO

-------------------------------------------------------------------------------
-- to Lists and streams
-------------------------------------------------------------------------------

{-
-- Use foldr/build fusion to fuse with list consumers
-- This can be useful when using the IsList instance
{-# INLINE_LATE toListFB #-}
toListFB :: forall a b. Unbox a => (a -> b -> b) -> b -> MutArray a -> b
toListFB c n MutArray{..} = go arrStart
    where

    go p | assert (p <= arrEnd) (p == arrEnd) = n
    go p =
        -- unsafeInlineIO allows us to run this in Identity monad for pure
        -- toList/foldr case which makes them much faster due to not
        -- accumulating the list and fusing better with the pure consumers.
        --
        -- This should be safe as the array contents are guaranteed to be
        -- evaluated/written to before we peek at them.
        -- XXX
        let !x = unsafeInlineIO $ do
                    r <- peekAt arrContents p
                    return r
        in c x (go (PTR_NEXT(p,a)))
-}

-- XXX Monadic foldr/build fusion?
-- Reference: https://www.researchgate.net/publication/220676509_Monadic_augment_and_generalised_short_cut_fusion

-- | Convert a 'MutArray' into a list.
--
{-# INLINE toList #-}
toList :: forall m a. (MonadIO m, Unbox a) => MutArray a -> m [a]
toList MutArray{..} = liftIO $ go arrStart
    where

    go p | assert (p <= arrEnd) (p == arrEnd) = return []
    go p = do
        x <- peekAt p arrContents
        (:) x <$> go (INDEX_NEXT(p,a))

{-# INLINE_NORMAL toStreamWith #-}
toStreamWith ::
       forall m a. (Monad m, Unbox a)
    => (forall b. IO b -> m b) -> MutArray a -> D.Stream m a
toStreamWith liftio MutArray{..} = D.Stream step arrStart

    where

    {-# INLINE_LATE step #-}
    step _ p | assert (p <= arrEnd) (p == arrEnd) = return D.Stop
    step _ p = liftio $ do
        r <- peekAt p arrContents
        return $ D.Yield r (INDEX_NEXT(p,a))

-- | Convert a 'MutArray' into a stream.
--
-- >>> read = Stream.unfold MutArray.reader
--
{-# INLINE_NORMAL read #-}
read :: forall m a. (MonadIO m, Unbox a) => MutArray a -> D.Stream m a
read = toStreamWith liftIO

{-# INLINE toStreamKWith #-}
toStreamKWith ::
       forall m a. (Monad m, Unbox a)
    => (forall b. IO b -> m b) -> MutArray a -> StreamK m a
toStreamKWith liftio MutArray{..} = go arrStart

    where

    go p | assert (p <= arrEnd) (p == arrEnd) = K.nil
         | otherwise =
        let elemM = peekAt p arrContents
        in liftio elemM `K.consM` go (INDEX_NEXT(p,a))

{-# INLINE toStreamK #-}
toStreamK :: forall m a. (MonadIO m, Unbox a) => MutArray a -> StreamK m a
toStreamK = toStreamKWith liftIO

{-# INLINE_NORMAL toStreamRevWith #-}
toStreamRevWith ::
       forall m a. (Monad m, Unbox a)
    => (forall b. IO b -> m b) -> MutArray a -> D.Stream m a
toStreamRevWith liftio MutArray{..} =
    let p = INDEX_PREV(arrEnd,a)
    in D.Stream step p

    where

    {-# INLINE_LATE step #-}
    step _ p | p < arrStart = return D.Stop
    step _ p = liftio $ do
        r <- peekAt p arrContents
        return $ D.Yield r (INDEX_PREV(p,a))

-- | Convert a 'MutArray' into a stream in reverse order.
--
-- >>> readRev = Stream.unfold MutArray.readerRev
--
{-# INLINE_NORMAL readRev #-}
readRev :: forall m a. (MonadIO m, Unbox a) => MutArray a -> D.Stream m a
readRev = toStreamRevWith liftIO

{-# INLINE toStreamKRevWith #-}
toStreamKRevWith ::
       forall m a. (Monad m, Unbox a)
    => (forall b. IO b -> m b) -> MutArray a -> StreamK m a
toStreamKRevWith liftio MutArray {..} =
    let p = INDEX_PREV(arrEnd,a)
    in go p

    where

    go p | p < arrStart = K.nil
         | otherwise =
        let elemM = peekAt p arrContents
        in liftio elemM `K.consM` go (INDEX_PREV(p,a))

{-# INLINE toStreamKRev #-}
toStreamKRev :: forall m a. (MonadIO m, Unbox a) => MutArray a -> StreamK m a
toStreamKRev = toStreamKRevWith liftIO

-------------------------------------------------------------------------------
-- Folding
-------------------------------------------------------------------------------

-- XXX Need something like "MutArray m a" enforcing monadic action to avoid the
-- possibility of such APIs.
--
-- | Strict left fold of an array.
{-# INLINE_NORMAL foldl' #-}
foldl' :: (MonadIO m, Unbox a) => (b -> a -> b) -> b -> MutArray a -> m b
foldl' f z arr = D.foldl' f z $ read arr

-- | Right fold of an array.
{-# INLINE_NORMAL foldr #-}
foldr :: (MonadIO m, Unbox a) => (a -> b -> b) -> b -> MutArray a -> m b
foldr f z arr = D.foldr f z $ read arr

-- | Fold an array using a 'Fold'.
--
-- For example:
--
-- >>> findIndex eq = MutArray.fold (Fold.findIndex eq)
--
-- /Pre-release/
{-# INLINE fold #-}
fold :: (MonadIO m, Unbox a) => Fold m a b -> MutArray a -> m b
fold f arr = D.fold f (read arr)

-- | Fold an arary starting from end up to beginning.
--
-- For example:
--
-- >>> findIndexRev eq = MutArray.foldRev (Fold.findIndex eq)
--
foldRev :: (MonadIO m, Unbox a) => Fold m a b -> MutArray a -> m b
foldRev f arr = D.fold f (readRev arr)

-------------------------------------------------------------------------------
-- Folds for appending
-------------------------------------------------------------------------------

-- Note: Arrays may be allocated with a specific alignment at the beginning of
-- the array. If you need to maintain that alignment on reallocations then you
-- can resize the array manually before append, using an aligned resize
-- operation.

-- XXX Keep the bound intact to not lose any free space? Perf impact?

-- | @unsafeAppendN n arr@ appends up to @n@ input items to the supplied
-- array.
--
-- Unsafe: Do not drive the fold beyond @n@ elements, it will lead to memory
-- corruption or segfault.
--
-- Any free space left in the array after appending @n@ elements is lost.
--
-- /Internal/
{-# DEPRECATED unsafeAppendN "Please use unsafeAppendMax instead." #-}
{-# INLINE_NORMAL unsafeAppendN #-}
unsafeAppendN :: forall m a. (MonadIO m, Unbox a) =>
       Int
    -> m (MutArray a)
    -> Fold m a (MutArray a)
unsafeAppendN n action = fmap fromArrayUnsafe $ FL.foldlM' step initial

    where

    initial = do
        assert (n >= 0) (return ())
        arr@(MutArray _ _ end bound) <- action
        let free_ = bound - end
            needed = n * SIZE_OF(a)
        -- XXX We can also reallocate if the array has too much free space,
        -- otherwise we lose that space.
        arr1 <-
            if free_ < needed
            then noinline reallocBytesWith "unsafeAppendN" (+ needed) needed arr
            else return arr
        return $ toArrayUnsafe arr1

    step (ArrayUnsafe contents start end) x = do
        liftIO $ pokeAt end contents x
        -- We are using end as the bound, so no reserved space left.
        return $ ArrayUnsafe contents start (INDEX_NEXT(end,a))

-- | @unsafeAppendMax n arr@ appends up to @n@ input items to the supplied
-- array.
--
-- Unsafe: Do not drive the fold beyond @n@ elements, it will lead to memory
-- corruption or segfault.
--
-- /Internal/
{-# INLINE_NORMAL unsafeAppendMax #-}
unsafeAppendMax :: forall m a. (MonadIO m, Unbox a) =>
       Int
    -> MutArray a
    -> Fold m a (MutArray a)
unsafeAppendMax n arr@MutArray{..} =
    fmap final $ FL.foldlM' step initial

    where

    free_ = arrBound - arrEnd
    needed = n * SIZE_OF(a)
    bound = arrBound + needed - free_

    initial = do
        assert (n >= 0) (return ())
        arr1 <-
            if free_ < needed
            then noinline
                    reallocBytesWith "unsafeAppendMax" (+ needed) needed arr
            else return arr
        return $ toArrayUnsafe arr1

    step (ArrayUnsafe contents start end) x = do
        liftIO $ pokeAt end contents x
        return $ ArrayUnsafe contents start (INDEX_NEXT(end,a))

    final (ArrayUnsafe contents start end) =
        MutArray contents start end bound

{-# DEPRECATED writeAppendNUnsafe "Please use unsafeAppendN instead." #-}
{-# INLINE writeAppendNUnsafe #-}
writeAppendNUnsafe :: forall m a. (MonadIO m, Unbox a) =>
       Int
    -> m (MutArray a)
    -> Fold m a (MutArray a)
writeAppendNUnsafe = unsafeAppendN

-- | Append @n@ elements to an existing array. Any free space left in the array
-- after appending @n@ elements is lost.
--
-- >>> appendN n initial = Fold.take n (MutArray.unsafeAppendN n initial)
--
{-# DEPRECATED appendN "Please use appendMax instead." #-}
{-# INLINE_NORMAL appendN #-}
appendN :: forall m a. (MonadIO m, Unbox a) =>
    Int -> m (MutArray a) -> Fold m a (MutArray a)
appendN n initial = FL.take n (unsafeAppendN n initial)

-- | Allocates space for n additional elements. The fold terminates after
-- appending n elements. If less than n elements are supplied then the space
-- for the remaining elements is guaranteed to be reserved.
--
-- >>> appendMax n arr = Fold.take n (MutArray.unsafeAppendMax n arr)
--
{-# INLINE_NORMAL appendMax #-}
appendMax :: forall m a. (MonadIO m, Unbox a) =>
    Int -> MutArray a -> Fold m a (MutArray a)
appendMax n initial = FL.take n (unsafeAppendMax n initial)

{-# DEPRECATED writeAppendN "Please use appendN instead." #-}
{-# INLINE writeAppendN #-}
writeAppendN :: forall m a. (MonadIO m, Unbox a) =>
    Int -> m (MutArray a) -> Fold m a (MutArray a)
writeAppendN = appendN

-- | @appendWith sizer action@ mutates the array generated by @action@ to
-- append the input stream. If there is no reserved space available in the
-- array it is reallocated to a size in bytes determined by @sizer oldSize@,
-- where @oldSize@ is the current size of the array in bytes. If the sizer
-- returns less than or equal to the current size then the size is incremented
-- by one element.
--
-- Note that the returned array may be a mutated version of original array.
--
-- >>> appendWith sizer = Fold.foldlM' (MutArray.snocWith sizer)
--
-- /Pre-release/
{-# INLINE appendWith #-}
appendWith :: forall m a. (MonadIO m, Unbox a) =>
    (Int -> Int) -> m (MutArray a) -> Fold m a (MutArray a)
appendWith sizer = FL.foldlM' (snocWith sizer)

{-# DEPRECATED writeAppendWith "Please use appendWith instead." #-}
{-# INLINE writeAppendWith #-}
writeAppendWith :: forall m a. (MonadIO m, Unbox a) =>
    (Int -> Int) -> m (MutArray a) -> Fold m a (MutArray a)
writeAppendWith = appendWith

-- | @append action@ mutates the array generated by @action@ to append the
-- input stream. If there is no reserved space available in the array it is
-- reallocated to double the size and aligned to power of 2.
--
-- Note that the returned array may be a mutated version of original array.
--
-- >>> append = Fold.foldlM' MutArray.snoc
--
{-# DEPRECATED append "Please use append2 instead." #-}
{-# INLINE append #-}
append :: forall m a. (MonadIO m, Unbox a) =>
    m (MutArray a) -> Fold m a (MutArray a)
-- append = appendWith (* 2)
append = FL.foldlM' snoc

-- | Fold @append2 arr@ mutates the array arr to append the input stream. If
-- there is no reserved space available in the array it is reallocated to
-- double the size and aligned to power of 2.
--
-- Note that the returned array may be a mutated version of original array.
--
-- >>> append2 arr = Fold.foldlM' MutArray.snoc (pure arr)
--
{-# INLINE append2 #-}
append2 :: (MonadIO m, Unbox a) => MutArray a -> Fold m a (MutArray a)
append2 arr = FL.foldlM' snoc (pure arr)

{-# DEPRECATED writeAppend "Please use append instead." #-}
{-# INLINE writeAppend #-}
writeAppend :: forall m a. (MonadIO m, Unbox a) =>
    m (MutArray a) -> Fold m a (MutArray a)
writeAppend = append

-- | @appendGrowBy arr@ mutates the array arr to append the input stream. If
-- there is no reserved space available in the array it is reallocated to add
-- space for the min number of elements supplied and align to block size if the
-- array becomes larger than 'largeObjectThreshold'.
--
-- Note that the returned array may be a mutated version of original array.
--
-- >>> appendGrowBy n arr = Fold.foldlM' (MutArray.snocGrowBy n) (pure arr)
--
{-# INLINE appendGrowBy #-}
appendGrowBy :: (MonadIO m, Unbox a) =>
    Int -> MutArray a -> Fold m a (MutArray a)
appendGrowBy n arr = FL.foldlM' (snocGrowBy n) (pure arr)

-------------------------------------------------------------------------------
-- Actions for Appending streams
-------------------------------------------------------------------------------

-- |
-- >>> appendStream arr = Stream.fold (MutArray.append (pure arr))
--
{-# INLINE appendStream #-}
appendStream :: (MonadIO m, Unbox a) =>
    MutArray a -> Stream m a -> m (MutArray a)
appendStream arr = D.fold (append (pure arr))

-- |
-- >>> appendStreamN n arr = Stream.fold (MutArray.appendMax n arr)
--
{-# INLINE appendStreamN #-}
appendStreamN :: (MonadIO m, Unbox a) =>
    Int -> MutArray a -> Stream m a -> m (MutArray a)
appendStreamN n arr = D.fold (appendMax n arr)

-- | The array is grown only by the required amount of space.
{-# INLINE appendCString# #-}
appendCString# :: MonadIO m => MutArray Word8 -> Addr# -> m (MutArray Word8)
appendCString# arr addr = do
    len <- liftIO $ c_strlen_pinned addr
    appendPtrN arr (Ptr addr) (fromIntegral len)

-- Note: in hsc code # is treated in a special way, so it is difficult to use
-- appendCString#
{-# INLINE appendCString #-}
appendCString :: MonadIO m => MutArray Word8 -> Ptr a -> m (MutArray Word8)
appendCString arr (Ptr addr) = appendCString# arr addr

-------------------------------------------------------------------------------
-- Folds for creating
-------------------------------------------------------------------------------

-- XXX Use "IO" instead of "m" in the alloc function

-- XXX We can carry bound as well in the state to make sure we do not lose the
-- remaining capacity. Need to check perf impact.

-- | Like 'unsafeCreateOf' but takes a new array allocator @alloc size@
-- function as argument.
--
-- >>> unsafeCreateWithOf alloc n = MutArray.unsafeAppendN (alloc n) n
--
-- /Pre-release/
{-# INLINE_NORMAL unsafeCreateWithOf #-}
unsafeCreateWithOf :: forall m a. (MonadIO m, Unbox a)
    => (Int -> m (MutArray a)) -> Int -> Fold m a (MutArray a)
unsafeCreateWithOf alloc n = fromArrayUnsafe <$> FL.foldlM' step initial

    where

    initial = toArrayUnsafe <$> alloc (max n 0)

    step (ArrayUnsafe contents start end) x = do
        liftIO $ pokeAt end contents x
        return
          $ ArrayUnsafe contents start (INDEX_NEXT(end,a))

{-# DEPRECATED writeNWithUnsafe "Please use unsafeCreateWithOf instead." #-}
{-# INLINE writeNWithUnsafe #-}
writeNWithUnsafe :: forall m a. (MonadIO m, Unbox a)
    => (Int -> m (MutArray a)) -> Int -> Fold m a (MutArray a)
writeNWithUnsafe = unsafeCreateWithOf

{-# INLINE_NORMAL writeNUnsafeAs #-}
writeNUnsafeAs :: forall m a. (MonadIO m, Unbox a)
    => PinnedState -> Int -> Fold m a (MutArray a)
writeNUnsafeAs ps = unsafeCreateWithOf (newAs ps)

-- | Like 'createOf' but does not check the array bounds when writing. The fold
-- driver must not call the step function more than 'n' times otherwise it will
-- corrupt the memory and crash. This function exists mainly because any
-- conditional in the step function blocks fusion causing 10x performance
-- slowdown.
--
-- >>> unsafeCreateOf = MutArray.unsafeCreateWithOf MutArray.emptyOf
--
{-# INLINE_NORMAL unsafeCreateOf #-}
unsafeCreateOf :: forall m a. (MonadIO m, Unbox a)
    => Int -> Fold m a (MutArray a)
unsafeCreateOf = writeNUnsafeAs Unpinned

{-# DEPRECATED writeNUnsafe "Please use unsafeCreateOf instead." #-}
{-# INLINE writeNUnsafe #-}
writeNUnsafe :: forall m a. (MonadIO m, Unbox a)
    => Int -> Fold m a (MutArray a)
writeNUnsafe = unsafeCreateOf

-- | Like 'unsafeCreateOf' but creates a pinned array.
{-# INLINE_NORMAL unsafeCreateOf' #-}
unsafePinnedCreateOf, unsafeCreateOf' :: forall m a. (MonadIO m, Unbox a)
    => Int -> Fold m a (MutArray a)
unsafeCreateOf' = writeNUnsafeAs Pinned
RENAME_PRIME(unsafePinnedCreateOf,unsafeCreateOf)

{-# DEPRECATED pinnedWriteNUnsafe "Please use unsafeCreateOf' instead." #-}
{-# INLINE pinnedWriteNUnsafe #-}
pinnedWriteNUnsafe :: forall m a. (MonadIO m, Unbox a)
    => Int -> Fold m a (MutArray a)
pinnedWriteNUnsafe = unsafeCreateOf'

-- XXX Use "IO" instead of "m" in the alloc function

-- | @createWithOf alloc n@ folds a maximum of @n@ elements into an array
-- allocated using the @alloc@ function.
--
-- The array capacity is guranteed to be at least @n@.
--
-- >>> createWithOf alloc n = Fold.take n (MutArray.unsafeCreateWithOf alloc n)
-- >>> createWithOf alloc n = MutArray.appendN (alloc n) n
--
{-# INLINE_NORMAL createWithOf #-}
createOfWith, createWithOf :: forall m a. (MonadIO m, Unbox a)
    => (Int -> m (MutArray a)) -> Int -> Fold m a (MutArray a)
createWithOf alloc n = FL.take n (unsafeCreateWithOf alloc n)

{-# DEPRECATED writeNWith "Please use createWithOf instead." #-}
{-# INLINE writeNWith #-}
writeNWith :: forall m a. (MonadIO m, Unbox a)
    => (Int -> m (MutArray a)) -> Int -> Fold m a (MutArray a)
writeNWith = createWithOf

{-# INLINE_NORMAL writeNAs #-}
writeNAs ::
       forall m a. (MonadIO m, Unbox a)
    => PinnedState
    -> Int
    -> Fold m a (MutArray a)
writeNAs ps = createWithOf (newAs ps)

-- | @createOf n@ folds a maximum of @n@ elements from the input stream to an
-- 'MutArray'.
--
-- The array capacity is guranteed to be at least @n@.
--
-- >>> createOf = MutArray.createWithOf MutArray.emptyOf
-- >>> createOf n = Fold.take n (MutArray.unsafeCreateOf n)
-- >>> createOf n = MutArray.appendMax n MutArray.empty
--
{-# INLINE_NORMAL createOf #-}
createOf :: forall m a. (MonadIO m, Unbox a) => Int -> Fold m a (MutArray a)
createOf = writeNAs Unpinned

{-# DEPRECATED writeN "Please use createOf instead." #-}
{-# INLINE writeN #-}
writeN :: forall m a. (MonadIO m, Unbox a) => Int -> Fold m a (MutArray a)
writeN = createOf

-- | Like 'createOf' but creates a pinned array.
{-# INLINE_NORMAL createOf' #-}
pinnedCreateOf, createOf' ::
       forall m a. (MonadIO m, Unbox a)
    => Int
    -> Fold m a (MutArray a)
createOf' = writeNAs Pinned
RENAME_PRIME(pinnedCreateOf,createOf)

{-# DEPRECATED pinnedWriteN "Please use createOf' instead." #-}
{-# INLINE pinnedWriteN #-}
pinnedWriteN ::
       forall m a. (MonadIO m, Unbox a)
    => Int
    -> Fold m a (MutArray a)
pinnedWriteN = createOf'

-- | Like unsafeCreateWithOf but writes the array in reverse order.
--
-- /Internal/
{-# INLINE_NORMAL writeRevNWithUnsafe #-}
writeRevNWithUnsafe :: forall m a. (MonadIO m, Unbox a)
    => (Int -> m (MutArray a)) -> Int -> Fold m a (MutArray a)
writeRevNWithUnsafe alloc n = fromArrayUnsafe <$> FL.foldlM' step initial

    where

    toArrayUnsafeRev (MutArray contents _ _ bound) =
         ArrayUnsafe contents bound bound

    initial = toArrayUnsafeRev <$> alloc (max n 0)

    step (ArrayUnsafe contents start end) x = do
        let ptr = INDEX_PREV(start,a)
        liftIO $ pokeAt ptr contents x
        return
          $ ArrayUnsafe contents ptr end

-- | Like createWithOf but writes the array in reverse order.
--
-- /Internal/
{-# INLINE_NORMAL writeRevNWith #-}
writeRevNWith :: forall m a. (MonadIO m, Unbox a)
    => (Int -> m (MutArray a)) -> Int -> Fold m a (MutArray a)
writeRevNWith alloc n = FL.take n (writeRevNWithUnsafe alloc n)

-- | Like 'createOf' but writes the array in reverse order.
--
-- /Pre-release/
{-# INLINE_NORMAL revCreateOf #-}
revCreateOf :: forall m a. (MonadIO m, Unbox a) => Int -> Fold m a (MutArray a)
revCreateOf = writeRevNWith new

{-# DEPRECATED writeRevN "Please use revCreateOf instead." #-}
{-# INLINE writeRevN #-}
writeRevN :: forall m a. (MonadIO m, Unbox a) => Int -> Fold m a (MutArray a)
writeRevN = revCreateOf

-- | @pinnedWriteNAligned align n@ folds a maximum of @n@ elements from the
-- input stream to a 'MutArray' aligned to the given size.
--
-- /Pre-release/
--
{-# INLINE_NORMAL pinnedWriteNAligned #-}
pinnedWriteNAligned :: forall m a. (MonadIO m, Unbox a)
    => Int -> Int -> Fold m a (MutArray a)
pinnedWriteNAligned align = createWithOf (pinnedNewAligned align)

-- XXX Buffer to a list instead?

-- | Buffer a stream into a stream of arrays.
--
-- >>> buildChunks n = Fold.many (MutArray.createOf n) Fold.toStreamK
--
-- Breaking an array into an array stream  can be useful to consume a large
-- array sequentially such that memory of the array is released incrementatlly.
--
-- See also: 'arrayStreamKFromStreamD'.
--
-- /Unimplemented/
--
{-# INLINE_NORMAL buildChunks #-}
buildChunks :: (MonadIO m, Unbox a) =>
    Int -> Fold m a (StreamK n (MutArray a))
buildChunks n = FL.many (createOf n) FL.toStreamK

{-# DEPRECATED writeChunks "Please use buildChunks instead." #-}
{-# INLINE writeChunks #-}
writeChunks :: (MonadIO m, Unbox a) =>
    Int -> Fold m a (StreamK n (MutArray a))
writeChunks = buildChunks

-- | Grows by doubling
{-# INLINE_NORMAL writeWithAs #-}
writeWithAs :: forall m a. (MonadIO m, Unbox a)
    => PinnedState -> Int -> Fold m a (MutArray a)
-- writeWithAs ps n = FL.rmapM rightSize $ appendWith (* 2) (newAs ps n)
writeWithAs ps elemCount =
    FL.rmapM extract $ FL.foldlM' step initial

    where

    -- XXX create an empty Array if the count is <= 0?
    initial = do
        when (elemCount < 0) $ error "createWith: elemCount is negative"
        newAs ps elemCount

    step arr@(MutArray _ start end bound) x
        | INDEX_NEXT(end,a) > bound = do
        let oldSize = end - start
            newSize = max (oldSize * 2) 1
        arr1 <- liftIO $ reallocExplicitAs ps (SIZE_OF(a)) newSize arr
        unsafeSnoc arr1 x
    step arr x = unsafeSnoc arr x

    extract = liftIO . rightSize

-- XXX Compare createWith with fromStreamD which uses an array of streams
-- implementation. We can write this using buildChunks above if that is faster.
-- If createWith is faster then we should use that to implement
-- fromStreamD.
--
-- XXX The realloc based implementation needs to make one extra copy if we use
-- shrinkToFit.  On the other hand, the stream of arrays implementation may
-- buffer the array chunk pointers in memory but it does not have to shrink as
-- we know the exact size in the end. However, memory copying does not seem to
-- be as expensive as the allocations. Therefore, we need to reduce the number
-- of allocations instead. Also, the size of allocations matters, right sizing
-- an allocation even at the cost of copying seems to help.  Should be measured
-- on a big stream with heavy calls to toArray to see the effect.
--
-- XXX check if GHC's memory allocator is efficient enough. We can try the C
-- malloc to compare against.

-- | @createMinOf count@ folds the whole input to a single array. The array
-- starts at a size big enough to hold minCount elements, the size is doubled
-- every time the array needs to be grown.
--
-- The array capacity is guaranteed to be at least count.
--
-- /Caution! Do not use this on infinite streams./
--
-- >>> f n = MutArray.appendWith (* 2) (MutArray.emptyOf n)
-- >>> createWith n = Fold.rmapM MutArray.rightSize (f n)
-- >>> createWith n = Fold.rmapM MutArray.fromChunksK (MutArray.buildChunks n)
--
-- /Pre-release/
{-# INLINE_NORMAL createMinOf #-}
createMinOf, createWith :: forall m a. (MonadIO m, Unbox a)
    => Int -> Fold m a (MutArray a)
-- createWith n = FL.rmapM rightSize $ appendWith (* 2) (emptyOf n)
createMinOf = writeWithAs Unpinned

RENAME(createWith,createMinOf)

{-# DEPRECATED writeWith "Please use createMinOf instead." #-}
{-# INLINE writeWith #-}
writeWith :: forall m a. (MonadIO m, Unbox a)
    => Int -> Fold m a (MutArray a)
writeWith = createMinOf

-- | Fold the whole input to a single array.
--
-- Same as 'createMinOf using an initial array size of 'arrayChunkBytes' bytes
-- rounded up to the element size. If the array is expected to be smaller than
-- 'arrayChunkBytes' then use 'createMinOf' to avoid wasting memory.
--
-- /Caution! Do not use this on infinite streams./
--
{-# INLINE create #-}
create :: forall m a. (MonadIO m, Unbox a) => Fold m a (MutArray a)
create = createMinOf (allocBytesToElemCount (undefined :: a) arrayChunkBytes)

{-# DEPRECATED write "Please use create instead." #-}
{-# INLINE write #-}
write :: forall m a. (MonadIO m, Unbox a) => Fold m a (MutArray a)
write = create

-- | Like 'create' but creates a pinned array.
{-# INLINE create' #-}
pinnedCreate, create' :: forall m a. (MonadIO m, Unbox a) => Fold m a (MutArray a)
create' =
    writeWithAs Pinned (allocBytesToElemCount (undefined :: a) arrayChunkBytes)
RENAME_PRIME(pinnedCreate,create)

{-# DEPRECATED pinnedWrite "Please use create' instead." #-}
{-# INLINE pinnedWrite #-}
pinnedWrite :: forall m a. (MonadIO m, Unbox a) => Fold m a (MutArray a)
pinnedWrite = create'

-------------------------------------------------------------------------------
-- construct from streams, known size
-------------------------------------------------------------------------------

{-# INLINE_NORMAL fromStreamDNAs #-}
fromStreamDNAs :: forall m a. (MonadIO m, Unbox a)
    => PinnedState -> Int -> D.Stream m a -> m (MutArray a)
fromStreamDNAs ps limit str = do
    (arr :: MutArray a) <- newAs ps limit
    end <- D.foldlM'
            (fwrite (arrContents arr))
            (return $ arrEnd arr)
            $ D.take limit str
    return $ arr {arrEnd = end}

    where

    fwrite arrContents ptr x = do
        liftIO $ pokeAt ptr arrContents  x
        return $ INDEX_NEXT(ptr,a)

-- | Create a MutArray of given size from a stream.
--
-- >>> fromStreamN n = Stream.fold (MutArray.createOf n)
--
{-# INLINE_NORMAL fromStreamN #-}
fromStreamN :: forall m a. (MonadIO m, Unbox a)
    => Int -> D.Stream m a -> m (MutArray a)
-- fromStreamDN n = D.fold (createOf n)
fromStreamN = fromStreamDNAs Unpinned

{-# DEPRECATED fromStreamDN "Please use fromStreamN instead." #-}
{-# INLINE fromStreamDN #-}
fromStreamDN :: forall m a. (MonadIO m, Unbox a)
    => Int -> D.Stream m a -> m (MutArray a)
fromStreamDN = fromStreamN

-- | Create a 'MutArray' from the first N elements of a list. The array is
-- allocated to size N, if the list terminates before N elements then the
-- array may hold less than N elements.
--
{-# INLINABLE fromListN #-}
fromListN :: (MonadIO m, Unbox a) => Int -> [a] -> m (MutArray a)
fromListN n xs = fromStreamN n $ D.fromList xs

-- | Like 'fromListN' but creates a pinned array.
{-# INLINABLE fromListN' #-}
pinnedFromListN, fromListN' :: (MonadIO m, Unbox a) => Int -> [a] -> m (MutArray a)
fromListN' n xs = fromStreamDNAs Pinned n $ D.fromList xs
RENAME_PRIME(pinnedFromListN,fromListN)

-- | Like fromListN but writes the array in reverse order.
--
-- /Pre-release/
{-# INLINE fromListRevN #-}
fromListRevN :: (MonadIO m, Unbox a) => Int -> [a] -> m (MutArray a)
fromListRevN n xs = D.fold (revCreateOf n) $ D.fromList xs

-- | Convert a pure stream in Identity monad to a mutable array.
{-# INLINABLE fromPureStreamN #-}
fromPureStreamN :: (MonadIO m, Unbox a) =>
    Int -> Stream Identity a -> m (MutArray a)
fromPureStreamN n = D.fold (createOf n) . D.generalizeInner

-- | Convert a pure stream in Identity monad to a mutable array.
{-# INLINABLE fromPureStream #-}
fromPureStream :: (MonadIO m, Unbox a) => Stream Identity a -> m (MutArray a)
fromPureStream = D.fold create . D.generalizeInner

-- | @fromPtrN len addr@ copies @len@ bytes from @addr@ into an array.
--
-- /Unsafe:/
--
-- The caller has to ensure that:
--
-- 1. the pointer is pinned and alive during the call.
-- 2. the pointer passed is valid up to the given length.
--
{-# INLINABLE fromPtrN #-}
fromPtrN :: MonadIO m => Int -> Ptr Word8 -> m (MutArray Word8)
fromPtrN len addr = do
    -- memcpy is better than stream copy when the size is known.
    -- XXX We can implement a stream copy in a similar way by streaming Word64
    -- first and then remaining Word8.
    (arr :: MutArray Word8) <- emptyOf len
    let mbarr = getMutByteArray# (arrContents arr)
    _ <- liftIO $ c_memcpy_pinned_src mbarr addr (fromIntegral len)
    pure (arr { arrEnd = len })

-- | @fromCString# addr@ copies a C string consisting of bytes and
-- terminated by a null byte, into a Word8 array. The null byte is not copied.
--
-- >>> MutArray.fromCString# "hello"#
--
-- /Unsafe:/
--
-- The caller has to ensure that:
--
-- 1. the @addr@ is pinned and alive during the call.
-- 2. the pointer passed is valid up to the point where null byte is found.
--
{-# INLINABLE fromCString# #-}
fromCString# :: MonadIO m => Addr# -> m (MutArray Word8)
fromCString# addr = do
    -- It is better to count the size first and allocate exact space.
    -- Also, memcpy is better than stream copy when the size is known.
    -- C strlen compares 4 bytes at a time, so is better than the stream
    -- version. https://github.com/bminor/glibc/blob/master/string/strlen.c
    -- XXX We can possibly use a stream of Word64 to do the same.
    -- fromByteStr# addr = fromPureStream (D.fromByteStr# addr)
    len <- liftIO $ c_strlen_pinned addr
    fromPtrN (fromIntegral len) (Ptr addr)

{-# DEPRECATED fromByteStr# "Please fromCString# instead." #-}
{-# INLINABLE fromByteStr# #-}
fromByteStr# :: MonadIO m => Addr# -> m (MutArray Word8)
fromByteStr# = fromCString#

-- | @fromW16CString# addr@ copies a C string consisting of 16-bit wide chars
-- and terminated by a 16-bit null char, into a Word16 array. The null
-- character is not copied.
--
-- Useful for copying UTF16 strings on Windows.
--
-- /Unsafe:/
--
-- The caller has to ensure that:
--
-- 1. the @addr@ is pinned and alive during the call.
-- 2. the pointer passed is valid up to the point where null Word16 is found.
--
{-# INLINABLE fromW16CString# #-}
fromW16CString# :: MonadIO m => Addr# -> m (MutArray Word16)
fromW16CString# addr = do
    -- XXX this can be done faster if we process one Word64 at a time
    w16len <- D.fold FL.length $ D.fromW16CString# addr
    let bytes = w16len * 2
    arr <- fromPtrN bytes (Ptr addr)
    pure $ unsafeCast arr

-------------------------------------------------------------------------------
-- convert a stream of arrays to a single array by reallocating and copying
-------------------------------------------------------------------------------

-- XXX Both of these implementations of splicing seem to perform equally well.
-- We need to perform benchmarks over a range of sizes though.

-- | Also see 'fromChunksK'.
{-# INLINE fromChunksRealloced #-}
fromChunksRealloced :: forall m a. (MonadIO m, Unbox a)
    => Stream m (MutArray a) -> m (MutArray a)
fromChunksRealloced s = do
    res <- D.uncons s
    case res of
        Just (a, strm) -> do
            arr <- D.foldlM' spliceExp (pure a) strm
            -- Reallocation is exponential so there may be 50% empty space in
            -- worst case. One more reallocation to reclaim the space.
            rightSize arr
        Nothing -> pure nil

-------------------------------------------------------------------------------
-- convert a stream of arrays to a single array by buffering arrays first
-------------------------------------------------------------------------------

{-# INLINE arrayStreamKLength #-}
arrayStreamKLength :: (Monad m, Unbox a) => StreamK m (MutArray a) -> m Int
arrayStreamKLength as = K.foldl' (+) 0 (K.map length as)

-- | Convert an array stream to an array. Note that this requires peak memory
-- that is double the size of the array stream.
--
{-# INLINE fromChunkskAs #-}
fromChunkskAs :: (Unbox a, MonadIO m) =>
    PinnedState -> StreamK m (MutArray a) -> m (MutArray a)
fromChunkskAs ps as = do
    len <- arrayStreamKLength as
    arr <- newAs ps len
    -- XXX is StreamK fold faster or StreamD fold?
    K.foldlM' unsafeSplice (pure arr) as
    -- fromStreamDN len $ D.unfoldMany reader $ D.fromStreamK as

-- XXX Need to compare this with fromChunks and fromChunkList and keep the
-- fastest or simplest one if all are equally fast.

-- | Convert an array stream to an array. Note that this requires peak memory
-- that is double the size of the array stream.
--
-- Also see 'fromChunksRealloced'.
--
{-# INLINE fromChunksK #-}
fromChunksK :: (Unbox a, MonadIO m) =>
    StreamK m (MutArray a) -> m (MutArray a)
fromChunksK = fromChunkskAs Unpinned

{-# DEPRECATED fromArrayStreamK "Please use fromChunksK instead." #-}
{-# INLINE fromArrayStreamK #-}
fromArrayStreamK :: (Unbox a, MonadIO m) =>
    StreamK m (MutArray a) -> m (MutArray a)
fromArrayStreamK = fromChunksK

{-# INLINE fromStreamDAs #-}
fromStreamDAs ::
       (MonadIO m, Unbox a) => PinnedState -> D.Stream m a -> m (MutArray a)
fromStreamDAs ps m =
    arrayStreamKFromStreamDAs Unpinned m >>= fromChunkskAs ps

-- | Create an 'Array' from a stream. This is useful when we want to create a
-- single array from a stream of unknown size. 'createOf' is at least twice
-- as efficient when the size is already known.
--
-- Note that if the input stream is too large memory allocation for the array
-- may fail.  When the stream size is not known, `chunksOf` followed by
-- processing of indvidual arrays in the resulting stream should be preferred.
--
-- /Pre-release/
{-# INLINE fromStream #-}
fromStream :: (MonadIO m, Unbox a) => Stream m a -> m (MutArray a)
fromStream = fromStreamDAs Unpinned

-- fromStream (Stream m) = P.fold create m
-- CAUTION: a very large number (millions) of arrays can degrade performance
-- due to GC overhead because we need to buffer the arrays before we flatten
-- all the arrays.
--
-- XXX Compare if this is faster or "fold create".
--
-- | We could take the approach of doubling the memory allocation on each
-- overflow. This would result in more or less the same amount of copying as in
-- the chunking approach. However, if we have to shrink in the end then it may
-- result in an extra copy of the entire data.
--
-- >>> fromStreamD = StreamD.fold MutArray.create
--
{-# INLINE fromStreamD #-}
{-# DEPRECATED fromStreamD "Please use fromStream instead." #-}
fromStreamD :: (MonadIO m, Unbox a) => D.Stream m a -> m (MutArray a)
fromStreamD = fromStream

-- | Create a 'MutArray' from a list. The list must be of finite size.
--
{-# INLINE fromList #-}
fromList :: (MonadIO m, Unbox a) => [a] -> m (MutArray a)
fromList xs = fromStreamD $ D.fromList xs

-- | Like 'fromList' but creates a pinned array.
{-# INLINE fromList' #-}
pinnedFromList, fromList' :: (MonadIO m, Unbox a) => [a] -> m (MutArray a)
fromList' xs = fromStreamDAs Pinned $ D.fromList xs
RENAME_PRIME(pinnedFromList,fromList)

-- XXX We are materializing the whole list first for getting the length. Check
-- if the 'fromList' like chunked implementation would fare better.

-- | Like 'fromList' but writes the contents of the list in reverse order.
{-# INLINE fromListRev #-}
fromListRev :: (MonadIO m, Unbox a) => [a] -> m (MutArray a)
fromListRev xs = fromListRevN (Prelude.length xs) xs

-------------------------------------------------------------------------------
-- Cloning
-------------------------------------------------------------------------------

-- Arrays are aligned on 64-bit boundaries. The fastest way to copy an array is
-- to unsafeCast it to Word64, read it, write it to Word64 array and unsafeCast
-- it again. We can use SIMD read/write as well.

{-# INLINE cloneAs #-}
cloneAs ::
    ( MonadIO m
#ifdef DEVBUILD
    , Unbox a
#endif
    )
    => PinnedState -> MutArray a -> m (MutArray a)
cloneAs ps src =
    do
        let startSrc = arrStart src
            srcLen = arrEnd src - startSrc
        newArrContents <-
            Unboxed.unsafeCloneSliceAs ps startSrc srcLen (arrContents src)
        return $ MutArray newArrContents 0 srcLen srcLen

-- | Clone the elements of a MutArray. Does not clone the reserve capacity.
--
-- To clone a slice of "MutArray" you can create a slice with "unsafeSliceOffLen"
-- and then use "clone".
--
-- The new "MutArray" is unpinned in nature. Use "clone'" to clone the
-- MutArray in pinned memory.
{-# INLINE clone #-}
clone ::
    ( MonadIO m
#ifdef DEVBUILD
    , Unbox a
#endif
    )
    => MutArray a -> m (MutArray a)
clone = cloneAs Unpinned

-- Similar to "clone" but uses pinned memory.
{-# INLINE clone' #-}
pinnedClone, clone' ::
    ( MonadIO m
#ifdef DEVBUILD
    , Unbox a
#endif
    )
    => MutArray a -> m (MutArray a)
clone' = cloneAs Pinned
RENAME_PRIME(pinnedClone,clone)

-------------------------------------------------------------------------------
-- Combining
-------------------------------------------------------------------------------

-- | Copy two arrays into a newly allocated array. If the first array is pinned
-- the spliced array is also pinned.
--
-- Note: If you freeze and splice it will create a new array.
{-# INLINE spliceCopy #-}
spliceCopy :: forall m a. MonadIO m =>
#ifdef DEVBUILD
    Unbox a =>
#endif
    MutArray a -> MutArray a -> m (MutArray a)
spliceCopy arr1 arr2 = do
    let start1 = arrStart arr1
        start2 = arrStart arr2
        len1 = arrEnd arr1 - start1
        len2 = arrEnd arr2 - start2
    let len = len1 + len2
    newArrContents <-
        if Unboxed.isPinned (arrContents arr1)
        then liftIO $ Unboxed.new' len
        else liftIO $ Unboxed.new len
    unsafePutSlice (arrContents arr1) start1 newArrContents 0 len1
    unsafePutSlice (arrContents arr2) start2 newArrContents len1 len2
    return $ MutArray newArrContents 0 len len

-- | Really really unsafe, appends the second array into the first array. If
-- the first array does not have enough space it may cause silent data
-- corruption or if you are lucky a segfault.
{-# INLINE unsafeSplice #-}
spliceUnsafe, unsafeSplice :: MonadIO m =>
    MutArray a -> MutArray a -> m (MutArray a)
unsafeSplice dst src = do
     let startSrc = arrStart src
         srcLen = arrEnd src - startSrc
         endDst = arrEnd dst
     assertM(endDst + srcLen <= arrBound dst)
     unsafePutSlice
         (arrContents src) startSrc (arrContents dst) endDst srcLen
     return $ dst {arrEnd = endDst + srcLen}

-- | Append specified number of bytes from a given pointer to the MutArray.
--
-- /Unsafe:/
--
-- The caller has to ensure that:
--
-- 1. the MutArray is valid up to the given length.
-- 2. the source pointer is pinned and alive during the call.
-- 3. the pointer passed is valid up to the given length.
--
{-# INLINE unsafeAppendPtrN #-}
unsafeAppendPtrN :: MonadIO m =>
    MutArray Word8 -> Ptr Word8 -> Int -> m (MutArray Word8)
unsafeAppendPtrN arr ptr ptrLen = do
    let newEnd = arrEnd arr + ptrLen
    assertM(newEnd <= arrBound arr)
    Unboxed.unsafePutPtrN ptr (arrContents arr) (arrEnd arr) ptrLen
    return $ arr {arrEnd = newEnd}

{-# INLINE appendPtrN #-}
appendPtrN :: MonadIO m =>
    MutArray Word8 -> Ptr Word8 -> Int -> m (MutArray Word8)
appendPtrN arr ptr ptrLen = do
    arr1 <- growBy ptrLen arr
    unsafeAppendPtrN arr1 ptr ptrLen

-- | @spliceWith sizer dst src@ mutates @dst@ to append @src@. If there is no
-- reserved space available in @dst@ it is reallocated to a size determined by
-- the @sizer dstBytes srcBytes@ function, where @dstBytes@ is the size of the
-- first array and @srcBytes@ is the size of the second array, in bytes.
--
-- Note that the returned array may be a mutated version of first array.
--
-- /Pre-release/
{-# INLINE spliceWith #-}
spliceWith :: forall m a. (MonadIO m, Unbox a) =>
    (Int -> Int -> Int) -> MutArray a -> MutArray a -> m (MutArray a)
spliceWith sizer dst@(MutArray _ start end bound) src = do
{-
    let f = appendWith (`sizer` byteLength src) (return dst)
     in D.fold f (toStreamD src)
-}
    assert (end <= bound) (return ())
    let srcBytes = arrEnd src - arrStart src

    dst1 <-
        if end + srcBytes >= bound
        then do
            let dstBytes = end - start
                newSizeInBytes = sizer dstBytes srcBytes
            when (newSizeInBytes < dstBytes + srcBytes)
                $ error
                    $ "splice: newSize is less than the total size "
                    ++ "of arrays being appended. Please check the "
                    ++ "sizer function passed."
            realloc newSizeInBytes dst
        else return dst
    unsafeSplice dst1 src

-- | The first array is extended in-place to append the second array. If there is no
-- reserved space available in the first array then a new allocation of exact
-- required size is done.
--
-- Note that the returned array may be an extended version of first array,
-- referring to the same memory as the original array.
--
-- >>> splice = MutArray.spliceWith (+)
--
-- If the original array is pinned the spliced array is also pinned.
--
-- /Pre-release/
{-# INLINE splice #-}
splice :: (MonadIO m, Unbox a) => MutArray a -> MutArray a -> m (MutArray a)
splice = spliceWith (+)

-- | Like 'append' but the growth of the array is exponential. Whenever a new
-- allocation is required the previous array size is at least doubled.
--
-- This is useful to reduce allocations when folding many arrays together.
--
-- Note that the returned array may be a mutated version of first array.
--
-- >>> spliceExp = MutArray.spliceWith (\l1 l2 -> max (l1 * 2) (l1 + l2))
--
-- /Pre-release/
{-# INLINE spliceExp #-}
spliceExp :: (MonadIO m, Unbox a) => MutArray a -> MutArray a -> m (MutArray a)
spliceExp = spliceWith (\l1 l2 -> max (l1 * 2) (l1 + l2))

-------------------------------------------------------------------------------
-- Splitting
-------------------------------------------------------------------------------

{-# INLINE splitUsing #-}
splitUsing :: (MonadIO m, Unbox a) =>
    ((a -> Bool) -> Stream m a -> Stream m (Int, Int))
    -> (a -> Bool) -> MutArray a -> Stream m (MutArray a)
splitUsing f predicate arr =
    fmap (\(i, len) -> unsafeSliceOffLen i len arr)
        $ f predicate (read arr)

-- | Generate a stream of array slices using a predicate. The array element
-- matching the predicate is dropped.
--
-- /Pre-release/
{-# INLINE splitEndBy_ #-}
splitEndBy_, sliceEndBy_, splitOn :: (MonadIO m, Unbox a) =>
    (a -> Bool) -> MutArray a -> Stream m (MutArray a)
splitEndBy_ = splitUsing D.indexEndBy_

RENAME(splitOn,splitEndBy_)
RENAME(sliceEndBy_,splitEndBy_)

-- | Generate a stream of array slices using a predicate. The array element
-- matching the predicate is included.
--
-- /Pre-release/
{-# INLINE splitEndBy #-}
splitEndBy :: (MonadIO m, Unbox a) =>
    (a -> Bool) -> MutArray a -> Stream m (MutArray a)
splitEndBy = splitUsing D.indexEndBy

-- XXX See advanceStartTill for a potential performance issue with this type of
-- code which needed to be investigated. Measure the perf of this and use
-- advanceStartTill if that turns out to be better.

{-# INLINE breakUsing #-}
breakUsing :: (MonadIO m, Unbox a) =>
    Int -> ((a -> Bool) -> Stream m a -> Stream m (Int, Int))
    -> (a -> Bool) -> MutArray a -> m (MutArray a, MutArray a)
breakUsing adj indexer predicate arr = do
    -- XXX Use MutArray.fold Fold.findIndex instead.
    r <- D.head $ indexer predicate (read arr)
    case r of
        Just (i, len) ->
            -- assert (i == 0)
            -- XXX avoid using length (div operation)
            let arrLen = length arr
                i1 = len + adj
                arr1 =
                    if i1 >= arrLen
                    then empty
                    else unsafeSliceOffLen i1 (arrLen - i1) arr
             in return (unsafeSliceOffLen i len arr, arr1)
        Nothing -> return (arr, empty)

{-# INLINE revBreakUsing #-}
revBreakUsing :: (MonadIO m, Unbox a) =>
    Bool -> (a -> Bool) -> MutArray a -> m (MutArray a, MutArray a)
revBreakUsing withSep predicate arr = do
    let indexer = if withSep then D.indexEndBy else D.indexEndBy_
        adj = if withSep then 0 else 1
    -- XXX Use MutArray.foldRev Fold.findIndex instead.
    r <- D.head $ indexer predicate (readRev arr)
    case r of
        Just (_, len) ->
            -- assert (i == 0)
            -- XXX avoid using length (div operation)
            let arrLen = length arr
                len1 = len + adj
                arr0 =
                    if len1 >= arrLen
                    then empty
                    else unsafeSliceOffLen 0 (arrLen - len1) arr
                arr1 = unsafeSliceOffLen (arrLen - len) len arr
             in return (arr0, arr1)
        Nothing -> return (arr, empty)

-- |
-- >>> arr <- MutArray.fromList "hello world"
-- >>> (a,b) <- MutArray.breakEndBy (== ' ') arr
-- >>> MutArray.toList a
-- "hello "
-- >>> MutArray.toList b
-- "world"
--
{-# INLINE breakEndBy #-}
breakEndBy :: (MonadIO m, Unbox a) =>
    (a -> Bool) -> MutArray a -> m (MutArray a, MutArray a)
breakEndBy = breakUsing 0 D.indexEndBy

-- | Break the array into two slices when the predicate succeeds. The array
-- element matching the predicate is dropped. If the predicate never succeeds
-- the second array is empty.
--
-- >>> arr <- MutArray.fromList "hello world"
-- >>> (a,b) <- MutArray.breakEndBy_ (== ' ') arr
-- >>> MutArray.toList a
-- "hello"
-- >>> MutArray.toList b
-- "world"
--
-- /Pre-release/
{-# INLINE breakEndBy_ #-}
breakEndBy_ :: (MonadIO m, Unbox a) =>
    (a -> Bool) -> MutArray a -> m (MutArray a, MutArray a)
breakEndBy_ = breakUsing 1 D.indexEndBy_

-- |
--
-- >>> arr <- MutArray.fromList "hello world"
-- >>> (a,b) <- MutArray.revBreakEndBy (== ' ') arr
-- >>> MutArray.toList a
-- "hello"
-- >>> MutArray.toList b
-- " world"
--
{-# INLINE revBreakEndBy #-}
revBreakEndBy :: (MonadIO m, Unbox a) =>
    (a -> Bool) -> MutArray a -> m (MutArray a, MutArray a)
revBreakEndBy = revBreakUsing True

-- |
--
-- >>> arr <- MutArray.fromList "hello world"
-- >>> (a,b) <- MutArray.revBreakEndBy_ (== ' ') arr
-- >>> MutArray.toList a
-- "hello"
-- >>> MutArray.toList b
-- "world"
--
{-# INLINE revBreakEndBy_ #-}
revBreakEndBy_ :: (MonadIO m, Unbox a) =>
    (a -> Bool) -> MutArray a -> m (MutArray a, MutArray a)
revBreakEndBy_ = revBreakUsing False

-- Note: We could return empty array instead of Nothing. But then we cannot
-- distinguish if the separator was found in the end or was not found at all.
-- XXX Do we need to distinguish that?

-- | Drops the separator byte
{-# INLINE breakEndByWord8_ #-}
breakEndByWord8_, breakOn :: MonadIO m
    => Word8 -> MutArray Word8 -> m (MutArray Word8, Maybe (MutArray Word8))
breakEndByWord8_ sep arr@MutArray{..} = liftIO $ do
    -- XXX We do not need memchr here, we can use a Haskell equivalent.
    -- Need efficient stream based primitives that work on Word64.
    let marr = getMutByteArray# arrContents
        len = fromIntegral (arrEnd - arrStart)
    sepIndex <- c_memchr_index marr (fromIntegral arrStart) sep len
    let intIndex = fromIntegral sepIndex
    return $
        if sepIndex >= len
        then (arr, Nothing)
        else
            ( MutArray
                { arrContents = arrContents
                , arrStart = arrStart
                , arrEnd = arrStart + intIndex -- exclude the separator
                , arrBound = arrStart + intIndex
                }
            , Just $ MutArray
                    { arrContents = arrContents
                    , arrStart = arrStart + (intIndex + 1)
                    , arrEnd = arrEnd
                    , arrBound = arrBound
                    }
            )
RENAME(breakOn,breakEndByWord8_)

-- | Like 'breakAt' but does not check whether the index is valid.
--
-- >>> unsafeBreakAt i arr = (MutArray.unsafeSliceOffLen 0 i arr, MutArray.unsafeSliceOffLen i (MutArray.length arr - i) arr)
--
{-# INLINE unsafeBreakAt #-}
unsafeBreakAt, unsafeSplitAt :: forall a. Unbox a =>
    Int -> MutArray a -> (MutArray a, MutArray a)
unsafeBreakAt i MutArray{..} =
    -- (unsafeSliceOffLen 0 i arr, unsafeSliceOffLen i (length arr - i) arr)
    let off = i * SIZE_OF(a)
        p = arrStart + off
     in ( MutArray
         { arrContents = arrContents
         , arrStart = arrStart
         , arrEnd = p
         , arrBound = p
         }
        , MutArray
          { arrContents = arrContents
          , arrStart = p
          , arrEnd = arrEnd
          , arrBound = arrBound
          }
        )
RENAME(unsafeSplitAt,unsafeBreakAt)

-- | Create two slices of an array without copying the original array. The
-- specified index @i@ is the first index of the second slice.
--
{-# INLINE breakAt #-}
breakAt, splitAt
    :: forall a. Unbox a => Int -> MutArray a -> (MutArray a, MutArray a)
breakAt i arr =
    let maxIndex = length arr - 1
    in  if i < 0
        then error "sliceAt: negative array index"
        else if i > maxIndex
             then error $ "sliceAt: specified array index " ++ show i
                        ++ " is beyond the maximum index " ++ show maxIndex
             else unsafeBreakAt i arr
RENAME(splitAt,breakAt)

-------------------------------------------------------------------------------
-- Casting
-------------------------------------------------------------------------------

-- | Cast an array having elements of type @a@ into an array having elements of
-- type @b@. The array size must be a multiple of the size of type @b@
-- otherwise accessing the last element of the array may result into a crash or
-- a random value.
--
-- /Pre-release/
--
castUnsafe, unsafeCast ::
#ifdef DEVBUILD
    Unbox b =>
#endif
    MutArray a -> MutArray b
unsafeCast (MutArray contents start end bound) =
    MutArray contents start end bound

-- | Cast an @MutArray a@ into an @MutArray Word8@.
--
asBytes :: MutArray a -> MutArray Word8
asBytes = unsafeCast

-- | Cast an array having elements of type @a@ into an array having elements of
-- type @b@. The length of the array should be a multiple of the size of the
-- target element otherwise 'Nothing' is returned.
--
cast :: forall a b. Unbox b => MutArray a -> Maybe (MutArray b)
cast arr =
    let len = byteLength arr
        r = len `mod` SIZE_OF(b)
     in if r /= 0
        then Nothing
        else Just $ unsafeCast arr

-- XXX Should we just name it asPtr, the unsafety is implicit for any pointer
-- operations. And we are safe from Haskell perspective because we will be
-- pinning the memory.

-- | NOTE: this is deprecated because it can lead to accidental problems if the
-- user tries to use it to mutate the array because it does not return the new
-- array after pinning.
{-# DEPRECATED unsafePinnedAsPtr "Pin the array and then use unsafeAsPtr." #-}
{-# INLINE unsafePinnedAsPtr #-}
unsafePinnedAsPtr :: MonadIO m => MutArray a -> (Ptr a -> Int -> m b) -> m b
unsafePinnedAsPtr mutarr f = do
    let arr0 = arrContents mutarr
    arr <- liftIO $ Unboxed.pin arr0
    let !ptr = Ptr (byteArrayContents#
                     (unsafeCoerce# (getMutByteArray# arr)))
    r <- f (ptr `plusPtr` arrStart mutarr) (byteLength mutarr)
    liftIO $ Unboxed.touch arr
    return r

{-# DEPRECATED asPtrUnsafe "Pin the array and then use unsafeAsPtr." #-}
{-# INLINE asPtrUnsafe #-}
asPtrUnsafe :: MonadIO m => MutArray a -> (Ptr a -> m b) -> m b
asPtrUnsafe a f = unsafePinnedAsPtr a (\p _ -> f p)

-- | @unsafeAsPtr arr f@, f is a function used as @f ptr len@ where @ptr@ is a
-- pointer to the beginning of array and @len@ is the byte-length of the array.
--
-- /Unsafe/ WARNING:
--
-- 1. The array must be pinned, otherwise it will lead to memory corruption.
-- 2. The user must not use the pointer beyond the supplied length.
--
-- /Pre-release/
--
{-# INLINE unsafeAsPtr #-}
unsafeAsPtr :: MonadIO m => MutArray a -> (Ptr a -> Int -> IO b) -> m b
unsafeAsPtr arr f =
    Unboxed.unsafeAsPtr
        (arrContents arr)
        (\ptr -> f (ptr `plusPtr` arrStart arr) (byteLength arr))

-- | @unsafeCreateWithPtr' capacity populator@ creates a pinned array of
-- @capacity@ bytes and invokes the @populator@ function to populate it.
-- @populator ptr len@ gets the pointer to the array and MUST return the amount
-- of the capacity populated in bytes.
--
-- /Unsafe/ because the populator is allowed to use the pointer only up to
-- specified length. In other words, bytes populated MUST be less than or equal
-- to the total capacity.
{-# INLINE unsafeCreateWithPtr' #-}
unsafeCreateWithPtr'
    :: MonadIO m => Int -> (Ptr Word8 -> IO Int) -> m (MutArray Word8)
unsafeCreateWithPtr' cap pop = do
    (arr :: MutArray Word8) <- emptyOf' cap
    len <- Unboxed.unsafeAsPtr (arrContents arr) pop
    when (len > cap) (error (errMsg len))
    -- arrStart == 0
    pure (arr { arrEnd = len })


    where

    errMsg len =
        "unsafeCreateWithPtr': length > capacity, "
             ++ "length = " ++ show len ++ ", "
             ++ "capacity = " ++ show cap

asCString :: MutArray a -> (CString -> IO b) -> IO b
asCString arr act = do
    let pinned = isPinned arr
        req = byteLength arr + SIZE_OF(CChar)
    arr1 <-
        if byteCapacity arr < req || not pinned
        then reallocExplicitAs Pinned 1 req arr
        else return arr
    arr2 :: MutArray CChar <- snocUnsafe (unsafeCast arr1) (0 :: CChar)
    unsafeAsPtr arr2 $ \ptr _ -> act (castPtr ptr)

asCWString :: MutArray a -> (CWString -> IO b) -> IO b
asCWString arr act = do
    let pinned = isPinned arr
        req = byteLength arr + SIZE_OF(CWchar)
    arr1 <-
        if byteCapacity arr < req || not pinned
        then reallocExplicitAs Pinned 1 req arr
        else return arr
    arr2 :: MutArray CWchar <- snocUnsafe (unsafeCast arr1) (0 :: CWchar)
    unsafeAsPtr arr2 $ \ptr _ -> act (castPtr ptr)

-------------------------------------------------------------------------------
-- Equality
-------------------------------------------------------------------------------

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
byteCmp :: MonadIO m => MutArray a -> MutArray a -> m Ordering
byteCmp arr1 arr2 = do
    let !marr1 = arrContents arr1
        !marr2 = arrContents arr2
        !len1 = byteLength arr1
        !len2 = byteLength arr2
        !st1 = arrStart arr1
        !st2 = arrStart arr2
    case compare len1 len2 of
        EQ -> do
            r <- liftIO $ unsafeByteCmp marr1 st1 marr2 st2 len1
            return $ compare r 0
        x -> return x

{-# INLINE cmp #-}
{-# DEPRECATED cmp "Please use byteCmp instead." #-}
cmp :: MonadIO m => MutArray a -> MutArray a -> m Ordering
cmp = byteCmp

-- | Byte equality of two arrays.
--
-- >>> byteEq arr1 arr2 = (==) EQ <$> MutArray.byteCmp arr1 arr2
--
-- /Unsafe/: See 'byteCmp'.
{-# INLINE byteEq #-}
byteEq :: MonadIO m => MutArray a -> MutArray a -> m Bool
byteEq arr1 arr2 = fmap (EQ ==) $ byteCmp arr1 arr2

-------------------------------------------------------------------------------
-- Compact
-------------------------------------------------------------------------------

-- Note: LE versions avoid an extra copy compared to GE. LE parser trades
-- backtracking one array in lieu of avoiding a copy. However, LE and GE both
-- can leave some memory unused. They may split the last array to fit it
-- exactly in the space.

{-# INLINE_NORMAL pCompactLeAs #-}
pCompactLeAs ::
       forall m a. (MonadIO m, Unbox a)
    => PinnedState -> Int -> Parser (MutArray a) m (MutArray a)
pCompactLeAs ps maxElems = Parser step initial extract

    where

    maxBytes = maxElems * SIZE_OF(a)

    functionName = "Streamly.Internal.Data.MutArray.pCompactLE"

    initial =
        return
            $ if maxElems <= 0
              then error
                       $ functionName
                       ++ ": the size of arrays ["
                       ++ show maxElems ++ "] must be a natural number"
              else Parser.IPartial Nothing

    step Nothing arr =
        return
            $ let len = byteLength arr
               in if len >= maxBytes
                  then Parser.SDone 1 arr
                  else Parser.SPartial 1 (Just arr)
    -- XXX Split the last array to use the space more compactly.
    step (Just buf) arr =
        let len = byteLength buf + byteLength arr
         in if len > maxBytes
            then return $ Parser.SDone 0 buf
            else do
                buf1 <-
                    if byteCapacity buf < maxBytes
                    then liftIO $ reallocExplicitAs
                            ps (SIZE_OF(a)) maxBytes buf
                    else return buf
                buf2 <- unsafeSplice buf1 arr
                return $ Parser.SPartial 1 (Just buf2)

    extract Nothing = return $ Parser.FDone 0 nil
    extract (Just buf) = return $ Parser.FDone 0 buf

-- | Parser @createCompactMax maxElems@ coalesces adjacent arrays in the
-- input stream only if the combined size would be less than or equal to
-- @maxElems@ elements. Note that it won't split an array if the original array
-- is already larger than maxElems.
--
-- @maxElems@ must be greater than 0.
--
-- Generates unpinned arrays irrespective of the pinning status of input
-- arrays.
--
-- Note that a fold compacting to less than or equal to a given size is not
-- possible, as folds cannot backtrack.
--
-- /Internal/
{-# INLINE createCompactMax #-}
createCompactMax, pCompactLE ::
       forall m a. (MonadIO m, Unbox a)
    => Int -> Parser (MutArray a) m (MutArray a)
createCompactMax = pCompactLeAs Unpinned

RENAME(pCompactLE,createCompactMax)

-- | Pinned version of 'createCompactMax'.
{-# INLINE createCompactMax' #-}
createCompactMax', pPinnedCompactLE ::
       forall m a. (MonadIO m, Unbox a)
    => Int -> Parser (MutArray a) m (MutArray a)
createCompactMax' = pCompactLeAs Pinned

{-# DEPRECATED pPinnedCompactLE "Please use createCompactMax' instead." #-}
{-# INLINE pPinnedCompactLE #-}
pPinnedCompactLE = createCompactMax'

data SpliceState s arr
    = SpliceInitial s
    | SpliceBuffering s arr
    | SpliceYielding arr (SpliceState s arr)
    | SpliceFinish

-- | This mutates the first array (if it has space) to append values from the
-- second one. This would work for immutable arrays as well because an
-- immutable array never has additional space so a new array is allocated
-- instead of mutating it.
{-# INLINE_NORMAL compactLeAs #-}
compactLeAs :: forall m a. (MonadIO m, Unbox a)
    => PinnedState -> Int -> D.Stream m (MutArray a) -> D.Stream m (MutArray a)
compactLeAs ps maxElems (D.Stream step state) =
    D.Stream step' (SpliceInitial state)

    where

    maxBytes = maxElems * SIZE_OF(a)

    functionName = "Streamly.Internal.Data.MutArray.rCompactLE"

    {-# INLINE_LATE step' #-}
    step' gst (SpliceInitial st) = do
        when (maxElems <= 0) $
            -- XXX we can pass the module string from the higher level API
            error $ functionName ++ ": the size of arrays [" ++ show maxElems
                ++ "] must be a natural number"
        r <- step gst st
        case r of
            D.Yield arr s -> return $
                let len = byteLength arr
                 in if len >= maxBytes
                    then D.Skip (SpliceYielding arr (SpliceInitial s))
                    else D.Skip (SpliceBuffering s arr)
            D.Skip s -> return $ D.Skip (SpliceInitial s)
            D.Stop -> return D.Stop

    -- XXX Split the last array to use the space more compactly.
    step' gst (SpliceBuffering st buf) = do
        r <- step gst st
        case r of
            D.Yield arr s -> do
                let len = byteLength buf + byteLength arr
                if len > maxBytes
                then return $
                    D.Skip (SpliceYielding buf (SpliceBuffering s arr))
                else do
                    buf1 <- if byteCapacity buf < maxBytes
                            then liftIO $ reallocExplicitAs
                                    ps (SIZE_OF(a)) maxBytes buf
                            else return buf
                    buf2 <- unsafeSplice buf1 arr
                    return $ D.Skip (SpliceBuffering s buf2)
            D.Skip s -> return $ D.Skip (SpliceBuffering s buf)
            D.Stop -> return $ D.Skip (SpliceYielding buf SpliceFinish)

    step' _ SpliceFinish = return D.Stop

    step' _ (SpliceYielding arr next) = return $ D.Yield arr next


{-# INLINE_NORMAL fCompactGeAs #-}
fCompactGeAs ::
       forall m a. (MonadIO m, Unbox a)
    => PinnedState -> Int -> FL.Fold m (MutArray a) (MutArray a)
fCompactGeAs ps minElems = Fold step initial extract extract

    where

    minBytes = minElems * SIZE_OF(a)

    functionName = "Streamly.Internal.Data.MutArray.fCompactGE"

    initial =
        return
            $ if minElems < 0
              then error
                       $ functionName
                       ++ ": the size of arrays ["
                       ++ show minElems ++ "] must be a natural number"
              else FL.Partial Nothing

    step Nothing arr =
        return
            $ let len = byteLength arr
               in if len >= minBytes
                  then FL.Done arr
                  else FL.Partial (Just arr)
    -- XXX Buffer arrays as a list to avoid copy and reallocations
    step (Just buf) arr = do
        let len = byteLength buf + byteLength arr
        buf1 <-
            if byteCapacity buf < len
            then liftIO $ reallocExplicitAs
                    ps (SIZE_OF(a)) (max minBytes len) buf
            else return buf
        buf2 <- unsafeSplice buf1 arr
        if len >= minBytes
        then return $ FL.Done buf2
        else return $ FL.Partial (Just buf2)

    extract Nothing = return nil
    extract (Just buf) = return buf

-- | Fold @createCompactMin minElems@ coalesces adjacent arrays in the
-- input stream until the size becomes greater than or equal to @minElems@.
--
-- Generates unpinned arrays irrespective of the pinning status of input
-- arrays.
{-# INLINE createCompactMin #-}
createCompactMin, fCompactGE ::
       forall m a. (MonadIO m, Unbox a)
    => Int -> FL.Fold m (MutArray a) (MutArray a)
createCompactMin = fCompactGeAs Unpinned

RENAME(fCompactGE,createCompactMin)

-- | Pinned version of 'createCompactMin'.
{-# INLINE createCompactMin' #-}
createCompactMin', fPinnedCompactGE ::
       forall m a. (MonadIO m, Unbox a)
    => Int -> FL.Fold m (MutArray a) (MutArray a)
createCompactMin' = fCompactGeAs Pinned

{-# DEPRECATED fPinnedCompactGE "Please use createCompactMin' instead." #-}
{-# INLINE fPinnedCompactGE #-}
fPinnedCompactGE = createCompactMin'

{-# INLINE_NORMAL lCompactGeAs #-}
lCompactGeAs :: forall m a. (MonadIO m, Unbox a)
    => PinnedState -> Int -> Fold m (MutArray a) () -> Fold m (MutArray a) ()
-- The fold version turns out to be a little bit slower.
-- lCompactGeAs ps n = FL.many (fCompactGeAs ps n)
lCompactGeAs ps minElems (Fold step1 initial1 _ final1) =
    Fold step initial extract final

    where

    minBytes = minElems * SIZE_OF(a)

    functionName = "Streamly.Internal.Data.MutArray.lCompactGE"

    initial = do
        when (minElems <= 0) $
            -- XXX we can pass the module string from the higher level API
            error $ functionName ++ ": the size of arrays ["
                ++ show minElems ++ "] must be a natural number"

        r <- initial1
        return $ first (Tuple' Nothing) r

    {-# INLINE runInner #-}
    runInner len acc buf =
            if len >= minBytes
            then do
                r <- step1 acc buf
                case r of
                    FL.Done _ -> return $ FL.Done ()
                    FL.Partial s -> do
                        _ <- final1 s
                        res <- initial1
                        return $ first (Tuple' Nothing) res
            else return $ FL.Partial $ Tuple' (Just buf) acc

    step (Tuple' Nothing r1) arr =
         runInner (byteLength arr) r1 arr

    -- XXX Buffer arrays as a list to avoid copy and reallocations
    step (Tuple' (Just buf) r1) arr = do
        let len = byteLength buf + byteLength arr
        buf1 <- if byteCapacity buf < len
                then liftIO $ reallocExplicitAs
                        ps (SIZE_OF(a)) (max minBytes len) buf
                else return buf
        buf2 <- unsafeSplice buf1 arr
        runInner len r1 buf2

    -- XXX Several folds do extract >=> final, therefore, we need to make final
    -- return "m b" rather than using extract post it if we want extract to be
    -- partial.
    --
    -- extract forces the pending buffer to be sent to the fold which is not
    -- what we want.
    extract _ = error "lCompactGE: not designed for scanning"

    final (Tuple' Nothing r1) = final1 r1
    final (Tuple' (Just buf) r1) = do
        r <- step1 r1 buf
        case r of
            FL.Partial rr -> final1 rr
            FL.Done _ -> return ()

-- | Like 'compactGE' but for transforming folds instead of stream.
--
-- >> lCompactGE n = Fold.many (MutArray.fCompactGE n)
--
-- Generates unpinned arrays irrespective of the pinning status of input
-- arrays.
{-# DEPRECATED lCompactGE "Please use scanCompactMin instead." #-}
{-# INLINE lCompactGE #-}
lCompactGE :: forall m a. (MonadIO m, Unbox a)
    => Int -> Fold m (MutArray a) () -> Fold m (MutArray a) ()
lCompactGE = lCompactGeAs Unpinned

-- | Pinned version of 'lCompactGE'.
{-# DEPRECATED lPinnedCompactGE "Please use scanCompactMin' instead." #-}
{-# INLINE lPinnedCompactGE #-}
lPinnedCompactGE :: forall m a. (MonadIO m, Unbox a)
    => Int -> Fold m (MutArray a) () -> Fold m (MutArray a) ()
lPinnedCompactGE = lCompactGeAs Pinned

data CompactMinState arr =
    CompactMinInit | CompactMinIncomplete arr | CompactMinComplete arr

{-# INLINE_NORMAL scanCompactMinAs #-}
scanCompactMinAs :: forall m a. (MonadIO m, Unbox a)
    => PinnedState -> Int -> Scanl m (MutArray a) (Maybe (MutArray a))
scanCompactMinAs ps minElems =
    Scanl step initial extract final

    where

    minBytes = minElems * SIZE_OF(a)

    functionName = "Streamly.Internal.Data.MutArray.scanCompactMin"

    initial = do
        when (minElems <= 0) $
            -- XXX we can pass the module string from the higher level API
            error $ functionName ++ ": the size of arrays ["
                ++ show minElems ++ "] must be a natural number"

        return $ FL.Partial CompactMinInit

    {-# INLINE runInner #-}
    runInner len buf =
            if len >= minBytes
            then do
                return $ FL.Partial $ CompactMinComplete buf
            else return $ FL.Partial $ CompactMinIncomplete buf

    step CompactMinInit arr =
         runInner (byteLength arr) arr

    step (CompactMinComplete _) arr =
         runInner (byteLength arr) arr

    -- XXX Buffer arrays as a list to avoid copy and reallocations
    step (CompactMinIncomplete buf) arr = do
        let len = byteLength buf + byteLength arr
        buf1 <- if byteCapacity buf < len
                then liftIO $ reallocExplicitAs
                        ps (SIZE_OF(a)) (max minBytes len) buf
                else return buf
        buf2 <- unsafeSplice buf1 arr
        runInner len buf2

    extract CompactMinInit = return Nothing
    extract (CompactMinComplete arr) = return (Just arr)
    extract (CompactMinIncomplete _) = return Nothing

    final CompactMinInit = return Nothing
    final (CompactMinComplete arr) = return (Just arr)
    final (CompactMinIncomplete arr) = return (Just arr)

-- | Like 'compactMin' but a scan.
{-# INLINE scanCompactMin #-}
scanCompactMin :: forall m a. (MonadIO m, Unbox a)
    => Int -> Scanl m (MutArray a) (Maybe (MutArray a))
scanCompactMin = scanCompactMinAs Unpinned

-- | Like 'compactMin'' but a scan.
{-# INLINE scanCompactMin' #-}
scanCompactMin' :: forall m a. (MonadIO m, Unbox a)
    => Int -> Scanl m (MutArray a) (Maybe (MutArray a))
scanCompactMin' = scanCompactMinAs Pinned

-- | @compactMin n stream@ coalesces adjacent arrays in the @stream@ until
-- the compacted array size becomes greater than or equal to @n@.
--
-- >>> compactMin n = Stream.foldMany (MutArray.createCompactMin n)
--
{-# INLINE compactMin #-}
compactMin, compactGE ::
       (MonadIO m, Unbox a)
    => Int -> Stream m (MutArray a) -> Stream m (MutArray a)
compactMin n = D.foldMany (createCompactMin n)

RENAME(compactGE,compactMin)

-- | 'compactExact n' coalesces adajacent arrays in the input stream to
-- arrays of exact size @n@.
--
-- /Unimplemented/
{-# INLINE compactExact #-}
compactExact :: -- (MonadIO m, Unbox a) =>
    Int -> Stream m (MutArray a) -> Stream m (MutArray a)
compactExact _n = undefined -- D.parseManyD (pCompactEQ n)

-------------------------------------------------------------------------------
-- In-place mutation algorithms
-------------------------------------------------------------------------------

-- XXX Can use SIMD
-- XXX findIndex can be implemented using this if fold perf is not good enough.

{-# INLINE advanceStartTill #-}
advanceStartTill :: forall a. (Unbox a) => (a -> Bool) -> MutArray a -> IO Int
advanceStartTill eq MutArray{..} = go arrStart

    where

    {-
    -- XXX This should have the same perf but it does not, investigate.
    getStart = do
        r <- liftIO $ D.head $ D.findIndices (not . eq) $ toStreamD arr
        pure $
            case r of
                Nothing -> arrEnd
                Just i -> PTR_INDEX(arrStart,i,a)
    -}

    go cur =
        if cur < arrEnd
        then do
            r <- peekAt cur arrContents
            if eq r
            then go (INDEX_NEXT(cur,a))
            else return cur
        else return cur

{-# INLINE retractEndTill #-}
retractEndTill :: forall a. (Unbox a) => (a -> Bool) -> MutArray a -> IO Int
retractEndTill eq MutArray{..} = go arrEnd

    where

    go cur = do
        if cur > arrStart
        then do
            let prev = INDEX_PREV(cur,a)
            r <- peekAt prev arrContents
            if eq r
            then go prev
            else return cur
        else return cur

-- | Strip elements which match the predicate, from the start of the array.
--
-- >>> arr <- MutArray.fromList "    hello world"
-- >>> a <- MutArray.dropWhile (== ' ') arr
-- >>> MutArray.toList a
-- "hello world"
--
-- /Pre-release/
{-# INLINE dropWhile #-}
dropWhile, stripStart :: forall a m. (Unbox a, MonadIO m) =>
    (a -> Bool) -> MutArray a -> m (MutArray a)
dropWhile eq arr@MutArray{..} = liftIO $ do
    st <- advanceStartTill eq arr
    -- return arr{arrStart = st}
    return $
        if st >= arrEnd
        then empty
        else arr{arrStart = st}
RENAME(stripStart,dropWhile)

-- | Strip elements which match the predicate, from the end of the array.
--
-- >>> arr <- MutArray.fromList "hello world    "
-- >>> a <- MutArray.revDropWhile (== ' ') arr
-- >>> MutArray.toList a
-- "hello world"
--
-- /Pre-release/
{-# INLINE revDropWhile #-}
revDropWhile, stripEnd :: forall a m. (Unbox a, MonadIO m) =>
    (a -> Bool) -> MutArray a -> m (MutArray a)
revDropWhile eq arr@MutArray{..} = liftIO $ do
    end <- retractEndTill eq arr
    -- return arr {arrEnd = end}
    return $
        if end <= arrStart
        then empty
        else arr{arrEnd = end}
RENAME(stripEnd,revDropWhile)

-- | Strip elements which match the predicate, from both ends.
--
-- >>> arr <- MutArray.fromList "   hello world    "
-- >>> a <- MutArray.dropAround (== ' ') arr
-- >>> MutArray.toList a
-- "hello world"
--
-- /Pre-release/
{-# INLINE dropAround #-}
dropAround, strip :: forall a m. (Unbox a, MonadIO m) =>
    (a -> Bool) -> MutArray a -> m (MutArray a)
dropAround eq arr = liftIO $ dropWhile eq arr >>= revDropWhile eq
RENAME(strip,dropAround)

-- | Given an array sorted in ascending order except the last element being out
-- of order, use bubble sort to place the last element at the right place such
-- that the array remains sorted in ascending order.
--
-- /Pre-release/
{-# INLINE bubble #-}
bubble :: (MonadIO m, Unbox a) => (a -> a -> Ordering) -> MutArray a -> m ()
bubble cmp0 arr =
    when (l > 1) $ do
        x <- unsafeGetIndex (l - 1) arr
        go x (l - 2)

        where

        l = length arr

        go x i =
            if i >= 0
            then do
                x1 <- unsafeGetIndex i arr
                case x `cmp0` x1 of
                    LT -> do
                        unsafePutIndex (i + 1) arr x1
                        go x (i - 1)
                    _ -> unsafePutIndex (i + 1) arr x
            else unsafePutIndex (i + 1) arr x

--------------------------------------------------------------------------------
-- Renaming
--------------------------------------------------------------------------------

RENAME(realloc,reallocBytes)
RENAME(castUnsafe,unsafeCast)
RENAME(newArrayWith,emptyWithAligned)
RENAME(getSliceUnsafe,unsafeSliceOffLen)
RENAME(unsafeGetSlice,unsafeSliceOffLen)
RENAME(getSlice,sliceOffLen)
RENAME(putIndexUnsafe,unsafePutIndex)
RENAME(modifyIndexUnsafe,unsafeModifyIndex)
RENAME(getIndexUnsafe,unsafeGetIndex)
RENAME(snocUnsafe,unsafeSnoc)
RENAME(spliceUnsafe,unsafeSplice)
RENAME(pokeSkipUnsafe,unsafePokeSkip)
RENAME(peekSkipUnsafe,unsafePeekSkip)
RENAME(peekUncons,peek)
RENAME(peekUnconsUnsafe,unsafePeek)
RENAME(pokeAppend,poke)
RENAME(pokeAppendMay,pokeMay)

-- This renaming can be done directly without deprecations. But I'm keeping this
-- intentionally. Packdiff should be able to point out such APIs that we can
-- just remove.
RENAME(createOfWith,createWithOf)
