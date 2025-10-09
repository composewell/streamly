{-# LANGUAGE CPP #-}
{-# LANGUAGE UnboxedTuples #-}
-- |
-- Module      : Streamly.Internal.Data.MutArray.Generic
-- Copyright   : (c) 2020 Composewell Technologies
-- License     : BSD3-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
module Streamly.Internal.Data.MutArray.Generic
(
    -- * Type
    -- $arrayNotes
      MutArray (..)

    -- * Constructing and Writing
    -- ** Construction
    , nil

    -- ** Utils
    , initializeOfFilledUpto

    -- *** Uninitialized Arrays
    , emptyOf
    -- , newArrayWith

    -- *** From streams
    , unsafeCreateOf
    , createOf
    , createWith -- createOfMin/createMin/createGE?
    , create
    , fromStreamN
    , fromStream
    , fromPureStream

    -- , writeRevN
    -- , writeRev

    -- ** From containers
    , fromListN
    , fromList

    -- * Random writes
    , putIndex
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
    -- , modifyIndices
    -- , modify
    -- , swapIndices

    -- * Growing and Shrinking
    -- Arrays grow only at the end, though it is possible to grow on both sides
    -- and therefore have a cons as well as snoc. But that will require two
    -- bounds in the array representation.

    -- ** Reallocation
    , realloc
    , uninit

    -- ** Appending elements
    , snocWith
    , snoc
    -- , snocLinear
    -- , snocMay
    , unsafeSnoc

    -- ** Appending streams
    -- , writeAppendNUnsafe
    -- , writeAppendN
    -- , writeAppendWith
    -- , writeAppend

    -- ** Truncation
    -- These are not the same as slicing the array at the beginning, they may
    -- reduce the length as well as the capacity of the array.
    -- , truncateWith
    -- , truncate
    -- , truncateExp

    -- * Eliminating and Reading

    -- ** Unfolds
    , reader
    -- , readerRev
    , producerWith -- experimental
    , producer -- experimental

    -- ** To containers
    , read
    , readRev
    , toStreamK
    -- , toStreamKRev
    , toList

    -- ** Random reads
    , getIndex
    , unsafeGetIndex
    , unsafeGetIndexWith
    -- , getIndices
    -- , getFromThenTo
    -- , getIndexRev

    -- * Size
    , length

    -- * In-place Mutation Algorithms
    , dropAround
    -- , reverse
    -- , permute
    -- , partitionBy
    -- , shuffleBy
    -- , divideBy
    -- , mergeBy

    -- * Folding
    -- , foldl'
    -- , foldr
    , cmp
    , eq

    -- * Arrays of arrays
    --  We can add dimensionality parameter to the array type to get
    --  multidimensional arrays. Multidimensional arrays would just be a
    --  convenience wrapper on top of single dimensional arrays.

    -- | Operations dealing with multiple arrays, streams of arrays or
    -- multidimensional array representations.

    -- ** Construct from streams
    , chunksOf
    -- , arrayStreamKFromStreamD
    -- , writeChunks

    -- ** Eliminate to streams
    -- , flattenArrays
    -- , flattenArraysRev
    -- , fromArrayStreamK

    -- ** Construct from arrays
    -- get chunks without copying
    , unsafeSliceOffLen
    , sliceOffLen
    -- , getSlicesFromLenN
    -- , splitAt -- XXX should be able to express using sliceOffLen
    -- , breakOn

    -- ** Appending arrays
    -- , spliceCopy
    -- , spliceWith
    -- , splice
    -- , spliceExp
    , unsafePutSlice
    -- , appendSlice
    -- , appendSliceFrom

    , clone

    -- * Deprecated
    , getSlice
    , strip
    , new
    , writeNUnsafe
    , writeN
    , writeWith
    , write
    , getIndexUnsafe
    , getIndexUnsafeWith
    , putIndexUnsafe
    , modifyIndexUnsafe
    , snocUnsafe
    , getSliceUnsafe
    , putSliceUnsafe
    )
where

#include "inline.hs"
#include "deprecation.h"
#include "assert.hs"

import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Functor.Identity (Identity(..))
import GHC.Base
    ( MutableArray#
    , RealWorld
    , copyMutableArray#
    , newArray#
    , readArray#
    , writeArray#
    )
import GHC.IO (IO(..))
import GHC.Int (Int(..))
import Streamly.Internal.Data.Fold.Type (Fold(..))
import Streamly.Internal.Data.Producer.Type (Producer (..))
import Streamly.Internal.Data.Unfold.Type (Unfold(..))
import Streamly.Internal.Data.Stream.Type (Stream)
import Streamly.Internal.Data.SVar.Type (adaptState)

import qualified Streamly.Internal.Data.Fold.Type as FL
import qualified Streamly.Internal.Data.Producer as Producer
import qualified Streamly.Internal.Data.Stream.Type as D
import qualified Streamly.Internal.Data.Stream.Generate as D
import qualified Streamly.Internal.Data.Stream.Lift as D
import qualified Streamly.Internal.Data.StreamK.Type as K

import Prelude hiding (read, length, replicate)

#include "DocTestDataMutArrayGeneric.hs"

-------------------------------------------------------------------------------
-- MutArray Data Type
-------------------------------------------------------------------------------

data MutArray a =
    MutArray
        { arrContents# :: MutableArray# RealWorld a
          -- ^ The internal contents of the array representing the entire array.

        , arrStart :: {-# UNPACK #-}!Int
          -- ^ The starting index of this slice.

        , arrEnd :: {-# UNPACK #-}!Int
          -- ^ The index after the last initialized index.

        , arrBound :: {-# UNPACK #-}!Int
          -- ^ The first invalid index.
        }

{-# INLINE bottomElement #-}
bottomElement :: a
bottomElement =
    error
        $ unwords
              [ funcName
              , "This is the bottom element of the array."
              , "This is a place holder and should never be reached!"
              ]

    where

    funcName = "Streamly.Internal.Data.MutArray.Generic.bottomElement:"

-- XXX Would be nice if GHC can provide something like newUninitializedArray# so
-- that we do not have to write undefined or error in the whole array.

{-# INLINE initializeOfFilledUpto #-}
initializeOfFilledUpto :: MonadIO m => Int -> Int -> a -> m (MutArray a)
initializeOfFilledUpto n@(I# n#) end val =
    liftIO
        $ IO
        $ \s# ->
              case newArray# n# val s# of
                  (# s1#, arr# #) ->
                      let ma = MutArray arr# 0 end n
                       in (# s1#, ma #)

-- | @emptyOf count@ allocates a zero length array that can be extended to hold
-- up to 'count' items without reallocating.
--
-- /Pre-release/
{-# INLINE emptyOf #-}
emptyOf :: MonadIO m => Int -> m (MutArray a)
emptyOf n = initializeOfFilledUpto n 0 bottomElement

{-# DEPRECATED new "Please use emptyOf instead." #-}
{-# INLINE new #-}
new :: MonadIO m => Int -> m (MutArray a)
new = emptyOf

-- XXX This could be pure?

-- |
-- Definition:
--
-- >>> nil = MutArray.emptyOf 0
{-# INLINE nil #-}
nil :: MonadIO m => m (MutArray a)
nil = emptyOf 0

-------------------------------------------------------------------------------
-- Random writes
-------------------------------------------------------------------------------

-- | Write the given element to the given index of the 'MutableArray#'. Does not
-- check if the index is out of bounds of the array.
--
-- /Pre-release/
{-# INLINE putIndexUnderlying #-}
putIndexUnderlying :: MonadIO m => Int -> MutableArray# RealWorld a -> a -> m ()
putIndexUnderlying n _arrContents# x =
    liftIO
        $ IO
        $ \s# ->
              case n of
                  I# n# ->
                      let s1# = writeArray# _arrContents# n# x s#
                       in (# s1#, () #)

-- | Write the given element to the given index of the array. Does not check if
-- the index is out of bounds of the array.
--
-- /Pre-release/
{-# INLINE unsafePutIndex #-}
unsafePutIndex, putIndexUnsafe :: forall m a. MonadIO m => Int -> MutArray a -> a -> m ()
unsafePutIndex i arr@(MutArray {..}) x =
    assert (i >= 0 && i < length arr)
        (putIndexUnderlying (i + arrStart) arrContents# x)

invalidIndex :: String -> Int -> a
invalidIndex label i =
    error $ label ++ ": invalid array index " ++ show i

-- | /O(1)/ Write the given element at the given index in the array.
-- Performs in-place mutation of the array.
--
-- >>> putIndex ix arr val = MutArray.modifyIndex ix arr (const (val, ()))
--
-- /Pre-release/
{-# INLINE putIndex #-}
putIndex :: MonadIO m => Int -> MutArray a -> a -> m ()
putIndex i arr x =
    if i >= 0 && i < length arr
    then unsafePutIndex i arr x
    else invalidIndex "putIndex" i

-- | Write an input stream of (index, value) pairs to an array. Throws an
-- error if any index is out of bounds.
--
-- /Pre-release/
{-# INLINE putIndices #-}
putIndices :: MonadIO m
    => MutArray a -> Fold m (Int, a) ()
putIndices arr = FL.foldlM' step (return ())

    where

    step () (i, x) = putIndex i arr x

-- | Modify a given index of an array using a modifier function without checking
-- the bounds.
--
-- Unsafe because it does not check the bounds of the array.
--
-- /Pre-release/
unsafeModifyIndex, modifyIndexUnsafe :: MonadIO m => Int -> MutArray a -> (a -> (a, b)) -> m b
unsafeModifyIndex i MutArray {..} f = do
    liftIO
        $ IO
        $ \s# ->
              case i + arrStart of
                  I# n# ->
                      case readArray# arrContents# n# s# of
                          (# s1#, a #) ->
                              let (a1, b) = f a
                                  s2# = writeArray# arrContents# n# a1 s1#
                               in (# s2#, b #)

-- | Modify a given index of an array using a modifier function.
--
-- /Pre-release/
modifyIndex :: MonadIO m => Int -> MutArray a -> (a -> (a, b)) -> m b
modifyIndex i arr f = do
    if i >= 0 && i < length arr
    then unsafeModifyIndex i arr f
    else invalidIndex "modifyIndex" i

-------------------------------------------------------------------------------
-- Resizing
-------------------------------------------------------------------------------

-- | Reallocates the array according to the new size. This is a safe function
-- that always creates a new array and copies the old array into the new one.
-- If the reallocated size is less than the original array it results in a
-- truncated version of the original array.
--
realloc :: MonadIO m => Int -> MutArray a -> m (MutArray a)
realloc n arr = do
    arr1 <- emptyOf n
    let !newLen@(I# newLen#) = min n (length arr)
        !(I# arrS#) = arrStart arr
        !(I# arr1S#) = arrStart arr1
        arrC# = arrContents# arr
        arr1C# = arrContents# arr1
        !newEnd = arrStart arr1 + newLen
        !newBound = arrStart arr1 + n
    liftIO
        $ IO
        $ \s# ->
              let s1# = copyMutableArray# arrC# arrS# arr1C# arr1S# newLen# s#
               in (# s1#, arr1 {arrEnd = newEnd, arrBound = newBound} #)

reallocWith ::
       MonadIO m => String -> (Int -> Int) -> Int -> MutArray a -> m (MutArray a)
reallocWith label sizer reqSize arr = do
    let oldSize = length arr
        newSize = sizer oldSize
        safeSize = max newSize (oldSize + reqSize)
    assert (newSize >= oldSize + reqSize || error badSize) (return ())
    realloc safeSize arr

    where

    badSize = concat
        [ label
        , ": new array size is less than required size "
        , show reqSize
        , ". Please check the sizing function passed."
        ]

-------------------------------------------------------------------------------
-- Snoc
-------------------------------------------------------------------------------

-- XXX Not sure of the behavior of writeArray# if we specify an index which is
-- out of bounds. This comment should be rewritten based on that.
-- | Really really unsafe, appends the element into the first array, may
-- cause silent data corruption or if you are lucky a segfault if the index
-- is out of bounds.
--
-- /Internal/
{-# INLINE unsafeSnoc #-}
snocUnsafe, unsafeSnoc :: MonadIO m => MutArray a -> a -> m (MutArray a)
unsafeSnoc arr@(MutArray{..}) x = do
    let newEnd = arrEnd + 1
    putIndexUnderlying arrEnd arrContents# x
    return $ arr {arrEnd = newEnd}

-- NOINLINE to move it out of the way and not pollute the instruction cache.
{-# NOINLINE snocWithRealloc #-}
snocWithRealloc :: MonadIO m => (Int -> Int) -> MutArray a -> a -> m (MutArray a)
snocWithRealloc sizer arr x = do
    arr1 <- reallocWith "snocWithRealloc" sizer 1 arr
    unsafeSnoc arr1 x

-- | @snocWith sizer arr elem@ mutates @arr@ to append @elem@. The length of
-- the array increases by 1.
--
-- If there is no reserved space available in @arr@ it is reallocated to a size
-- in bytes determined by the @sizer oldSize@ function, where @oldSize@ is the
-- original size of the array.
--
-- Note that the returned array may be a mutated version of the original array.
--
-- /Pre-release/
{-# INLINE snocWith #-}
snocWith :: MonadIO m => (Int -> Int) -> MutArray a -> a -> m (MutArray a)
snocWith sizer arr@MutArray {..} x = do
    if arrEnd < arrBound
    then unsafeSnoc arr x
    else snocWithRealloc sizer arr x

-- XXX round it to next power of 2.

-- | The array is mutated to append an additional element to it. If there is no
-- reserved space available in the array then it is reallocated to double the
-- original size.
--
-- This is useful to reduce allocations when appending unknown number of
-- elements.
--
-- Note that the returned array may be a mutated version of the original array.
--
-- >>> snoc = MutArray.snocWith (* 2)
--
-- Performs O(n * log n) copies to grow, but is liberal with memory allocation.
--
-- /Pre-release/
{-# INLINE snoc #-}
snoc :: MonadIO m => MutArray a -> a -> m (MutArray a)
snoc = snocWith (* 2)

-- | Make the uninitialized memory in the array available for use extending it
-- by the supplied length beyond the current length of the array. The array may
-- be reallocated.
--
{-# INLINE uninit #-}
uninit :: MonadIO m => MutArray a -> Int -> m (MutArray a)
uninit arr@MutArray{..} len =
    if arrEnd + len <= arrBound
    then return $ arr {arrEnd = arrEnd + len}
    else realloc (length arr + len) arr

-------------------------------------------------------------------------------
-- Random reads
-------------------------------------------------------------------------------

-- | Return the element at the specified index without checking the bounds from
-- a @MutableArray# RealWorld@.
--
-- Unsafe because it does not check the bounds of the array.
{-# INLINE unsafeGetIndexWith #-}
unsafeGetIndexWith, getIndexUnsafeWith :: MonadIO m => MutableArray# RealWorld a -> Int -> m a
unsafeGetIndexWith _arrContents# n =
    liftIO
        $ IO
        $ \s# ->
              let !(I# i#) = n
               in readArray# _arrContents# i# s#

-- | Return the element at the specified index without checking the bounds.
--
-- Unsafe because it does not check the bounds of the array.
{-# INLINE_NORMAL unsafeGetIndex #-}
unsafeGetIndex, getIndexUnsafe :: MonadIO m => Int -> MutArray a -> m a
unsafeGetIndex n MutArray {..} = unsafeGetIndexWith arrContents# (n + arrStart)

-- | /O(1)/ Lookup the element at the given index. Index starts from 0.
--
{-# INLINE getIndex #-}
getIndex :: MonadIO m => Int -> MutArray a -> m (Maybe a)
getIndex i arr =
    if i >= 0 && i < length arr
    then Just <$> unsafeGetIndex i arr
    else return Nothing

-------------------------------------------------------------------------------
-- Subarrays
-------------------------------------------------------------------------------

-- XXX We can also get immutable slices.

-- | /O(1)/ Slice an array in constant time.
--
-- Unsafe: The bounds of the slice are not checked.
--
-- /Unsafe/
--
-- /Pre-release/
{-# INLINE unsafeSliceOffLen #-}
unsafeSliceOffLen, getSliceUnsafe
    :: Int -- ^ from index
    -> Int -- ^ length of the slice
    -> MutArray a
    -> MutArray a
unsafeSliceOffLen index len arr@MutArray {..} =
    assert (index >= 0 && len >= 0 && index + len <= length arr)
        $ arr {arrStart = newStart, arrEnd = newEnd}
    where
    newStart = arrStart + index
    newEnd = newStart + len

-- | /O(1)/ Slice an array in constant time. Throws an error if the slice
-- extends out of the array bounds.
--
-- /Pre-release/
{-# INLINE sliceOffLen #-}
sliceOffLen, getSlice
    :: Int -- ^ from index
    -> Int -- ^ length of the slice
    -> MutArray a
    -> MutArray a
sliceOffLen index len arr@MutArray{..} =
    if index >= 0 && len >= 0 && index + len <= length arr
    then arr {arrStart = newStart, arrEnd = newEnd}
    else error
             $ "sliceOffLen: invalid slice, index "
             ++ show index ++ " length " ++ show len
    where
    newStart = arrStart + index
    newEnd = newStart + len

-------------------------------------------------------------------------------
-- to Lists and streams
-------------------------------------------------------------------------------

-- XXX Maybe faster to create a list explicitly instead of mapM, if list fusion
-- does not work well.

-- | Convert an 'Array' into a list.
--
-- /Pre-release/
{-# INLINE toList #-}
toList :: MonadIO m => MutArray a -> m [a]
toList arr = mapM (`unsafeGetIndex` arr) [0 .. (length arr - 1)]

-- | Generates a stream from the elements of a @MutArray@.
--
-- >>> read = Stream.unfold MutArray.reader
--
{-# INLINE_NORMAL read #-}
read :: MonadIO m => MutArray a -> D.Stream m a
read arr =
    D.mapM (`unsafeGetIndex` arr) $ D.enumerateFromToIntegral 0 (length arr - 1)

-- Check equivalence with StreamK.fromStream . toStreamD and remove
{-# INLINE toStreamK #-}
toStreamK :: MonadIO m => MutArray a -> K.StreamK m a
toStreamK arr = K.unfoldrM step 0

    where

    arrLen = length arr
    step i
        | i == arrLen = return Nothing
        | otherwise = do
            x <- unsafeGetIndex i arr
            return $ Just (x, i + 1)

{-# INLINE_NORMAL readRev #-}
readRev :: MonadIO m => MutArray a -> D.Stream m a
readRev arr =
    D.mapM (`unsafeGetIndex` arr)
        $ D.enumerateFromThenToIntegral (arrLen - 1) (arrLen - 2) 0
    where
    arrLen = length arr

-------------------------------------------------------------------------------
-- Folds
-------------------------------------------------------------------------------

-- XXX deduplicate this across unboxed array and this module?

-- | The default chunk size by which the array creation routines increase the
-- size of the array when the array is grown linearly.
arrayChunkSize :: Int
arrayChunkSize = 1024

-- | Like 'createOf' but does not check the array bounds when writing. The fold
-- driver must not call the step function more than 'n' times otherwise it will
-- corrupt the memory and crash. This function exists mainly because any
-- conditional in the step function blocks fusion causing 10x performance
-- slowdown.
--
-- /Pre-release/
{-# INLINE_NORMAL unsafeCreateOf #-}
unsafeCreateOf :: MonadIO m => Int -> Fold m a (MutArray a)
unsafeCreateOf n = Fold step initial return return

    where

    initial = FL.Partial <$> emptyOf (max n 0)

    step arr x = FL.Partial <$> unsafeSnoc arr x

{-# DEPRECATED writeNUnsafe "Please use unsafeCreateOf instead." #-}
{-# INLINE writeNUnsafe #-}
writeNUnsafe :: MonadIO m => Int -> Fold m a (MutArray a)
writeNUnsafe = unsafeCreateOf

-- | @createOf n@ folds a maximum of @n@ elements from the input stream to an
-- 'Array'.
--
-- >>> createOf n = Fold.take n (MutArray.unsafeCreateOf n)
--
-- /Pre-release/
{-# INLINE_NORMAL createOf #-}
createOf :: MonadIO m => Int -> Fold m a (MutArray a)
createOf n = FL.take n $ unsafeCreateOf n

{-# DEPRECATED writeN "Please use createOf instead." #-}
{-# INLINE writeN #-}
writeN :: MonadIO m => Int -> Fold m a (MutArray a)
writeN = createOf

-- >>> f n = MutArray.writeAppendWith (* 2) (MutArray.pinnedNew n)
-- >>> writeWith n = Fold.rmapM MutArray.rightSize (f n)
-- >>> writeWith n = Fold.rmapM MutArray.fromArrayStreamK (MutArray.writeChunks n)

-- | @createWith minCount@ folds the whole input to a single array. The array
-- starts at a size big enough to hold minCount elements, the size is doubled
-- every time the array needs to be grown.
--
-- /Caution! Do not use this on infinite streams./
--
-- /Pre-release/
{-# INLINE_NORMAL createWith #-}
createWith :: MonadIO m => Int -> Fold m a (MutArray a)
-- writeWith n = FL.rmapM rightSize $ writeAppendWith (* 2) (pinnedNew n)
createWith elemCount = FL.rmapM extract $ FL.foldlM' step initial

    where

    initial = do
        when (elemCount < 0) $ error "createWith: elemCount is negative"
        emptyOf elemCount

    step arr@(MutArray _ start end bound) x
        | end == bound = do
        let oldSize = end - start
            newSize = max (oldSize * 2) 1
        arr1 <- realloc newSize arr
        unsafeSnoc arr1 x
    step arr x = unsafeSnoc arr x

    -- extract = rightSize
    extract = return

{-# DEPRECATED writeWith "Please use createWith instead." #-}
{-# INLINE writeWith #-}
writeWith :: MonadIO m => Int -> Fold m a (MutArray a)
writeWith = createWith

-- | Fold the whole input to a single array.
--
-- Same as 'createWith' using an initial array size of 'arrayChunkSize' bytes
-- rounded up to the element size.
--
-- /Caution! Do not use this on infinite streams./
--
{-# INLINE create #-}
create :: MonadIO m => Fold m a (MutArray a)
create = writeWith arrayChunkSize

{-# DEPRECATED write "Please use create instead." #-}
{-# INLINE write #-}
write :: MonadIO m => Fold m a (MutArray a)
write = create

-- | Create a 'MutArray' from the first @n@ elements of a stream. The
-- array is allocated to size @n@, if the stream terminates before @n@
-- elements then the array may hold less than @n@ elements.
--
{-# INLINE fromStreamN #-}
fromStreamN :: MonadIO m => Int -> Stream m a -> m (MutArray a)
fromStreamN n = D.fold (writeN n)

{-# INLINE fromStream #-}
fromStream :: MonadIO m => Stream m a -> m (MutArray a)
fromStream = D.fold write

{-# INLINABLE fromListN #-}
fromListN :: MonadIO m => Int -> [a] -> m (MutArray a)
fromListN n xs = fromStreamN n $ D.fromList xs

{-# INLINABLE fromList #-}
fromList :: MonadIO m => [a] -> m (MutArray a)
fromList xs = fromStream $ D.fromList xs

{-# INLINABLE fromPureStream #-}
fromPureStream :: MonadIO m => Stream Identity a -> m (MutArray a)
fromPureStream xs =
    D.fold write $ D.morphInner (return . runIdentity) xs

-------------------------------------------------------------------------------
-- Chunking
-------------------------------------------------------------------------------

data GroupState s a start end bound
    = GroupStart s
    | GroupBuffer s (MutableArray# RealWorld a) start end bound
    | GroupYield
          (MutableArray# RealWorld a)
          start
          end
          bound
          (GroupState s a start end bound)
    | GroupFinish

-- | @chunksOf n stream@ groups the input stream into a stream of
-- arrays of size n.
--
-- @chunksOf n = foldMany (MutArray.writeN n)@
--
-- /Pre-release/
{-# INLINE_NORMAL chunksOf #-}
chunksOf :: forall m a. MonadIO m
    => Int -> D.Stream m a -> D.Stream m (MutArray a)
-- XXX the idiomatic implementation leads to large regression in the D.reverse'
-- benchmark. It seems it has difficulty producing optimized code when
-- converting to StreamK. Investigate GHC optimizations.
-- chunksOf n = D.foldMany (writeN n)
chunksOf n (D.Stream step state) =
    D.Stream step' (GroupStart state)

    where

    -- start is always 0
    -- end and len are always equal

    {-# INLINE_LATE step' #-}
    step' _ (GroupStart st) = do
        when (n <= 0) $
            -- XXX we can pass the module string from the higher level API
            error $ "Streamly.Internal.Data.Array.Generic.Mut.Type.chunksOf: "
                    ++ "the size of arrays [" ++ show n
                    ++ "] must be a natural number"
        (MutArray contents start end bound :: MutArray a) <- emptyOf n
        return $ D.Skip (GroupBuffer st contents start end bound)

    step' gst (GroupBuffer st contents start end bound) = do
        r <- step (adaptState gst) st
        case r of
            D.Yield x s -> do
                putIndexUnderlying end contents x
                let end1 = end + 1
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

-------------------------------------------------------------------------------
-- Unfolds
-------------------------------------------------------------------------------

-- | Resumable unfold of an array.
--
{-# INLINE_NORMAL producerWith #-}
producerWith :: Monad m => (forall b. IO b -> m b) -> Producer m (MutArray a) a
producerWith liftio = Producer step inject extract

    where

    {-# INLINE inject #-}
    inject arr = return (arr, 0)

    {-# INLINE extract #-}
    extract (arr, i) =
        return $ arr {arrStart = arrStart arr + i}

    {-# INLINE_LATE step #-}
    step (arr, i)
        | i == length arr = return D.Stop
    step (arr, i) = do
        x <- liftio $ unsafeGetIndex i arr
        return $ D.Yield x (arr, i + 1)

-- | Resumable unfold of an array.
--
{-# INLINE_NORMAL producer #-}
producer :: MonadIO m => Producer m (MutArray a) a
producer = producerWith liftIO

-- | Unfold an array into a stream.
--
{-# INLINE_NORMAL reader #-}
reader :: MonadIO m => Unfold m (MutArray a) a
reader = Producer.simplify producer

--------------------------------------------------------------------------------
-- Appending arrays
--------------------------------------------------------------------------------

-- | Put a sub range of a source array into a subrange of a destination array.
-- This is not safe as it does not check the bounds.
{-# INLINE unsafePutSlice #-}
unsafePutSlice, putSliceUnsafe :: MonadIO m =>
    MutArray a -> Int -> MutArray a -> Int -> Int -> m ()
unsafePutSlice src srcStart dst dstStart len = liftIO $ do
    assertM(len <= length dst)
    assertM(len <= length src)
    let !(I# srcStart#) = srcStart + arrStart src
        !(I# dstStart#) = dstStart + arrStart dst
        !(I# len#) = len
    let arrS# = arrContents# src
        arrD# = arrContents# dst
    IO $ \s# -> (# copyMutableArray#
                    arrS# srcStart# arrD# dstStart# len# s#
                , () #)

{-# INLINE clone #-}
clone :: MonadIO m => MutArray a -> m (MutArray a)
clone src = do
    let len = length src
    dst <- emptyOf len
    unsafePutSlice src 0 dst 0 len
    return dst

-------------------------------------------------------------------------------
-- Size
-------------------------------------------------------------------------------

{-# INLINE length #-}
length :: MutArray a -> Int
length arr = arrEnd arr - arrStart arr

-------------------------------------------------------------------------------
-- Equality
-------------------------------------------------------------------------------

-- | Compare the length of the arrays. If the length is equal, compare the
-- lexicographical ordering of two underlying byte arrays otherwise return the
-- result of length comparison.
--
-- /Pre-release/
{-# INLINE cmp #-}
cmp :: (MonadIO m, Ord a) => MutArray a -> MutArray a -> m Ordering
cmp a1 a2 =
    case compare lenA1 lenA2 of
        EQ -> loop (lenA1 - 1)
        x -> return x

    where

    lenA1 = length a1
    lenA2 = length a2

    loop i
        | i < 0 = return EQ
        | otherwise = do
            v1 <- unsafeGetIndex i a1
            v2 <- unsafeGetIndex i a2
            case compare v1 v2 of
                EQ -> loop (i - 1)
                x -> return x

{-# INLINE eq #-}
eq :: (MonadIO m, Eq a) => MutArray a -> MutArray a -> m Bool
eq a1 a2 =
    if lenA1 == lenA2
    then loop (lenA1 - 1)
    else return False

    where

    lenA1 = length a1
    lenA2 = length a2

    loop i
        | i < 0 = return True
        | otherwise = do
            v1 <- unsafeGetIndex i a1
            v2 <- unsafeGetIndex i a2
            if v1 == v2
            then loop (i - 1)
            else return False

{-# INLINE dropAround #-}
dropAround, strip :: MonadIO m => (a -> Bool) -> MutArray a -> m (MutArray a)
dropAround p arr = liftIO $ do
    let lastIndex = length arr - 1
    indexR <- getIndexR lastIndex -- last predicate failing index
    if indexR < 0
    then nil
    else do
        indexL <- getIndexL 0 -- first predicate failing index
        if indexL == 0 && indexR == lastIndex
        then return arr
        else
           let newLen = indexR - indexL + 1
            in return $ unsafeSliceOffLen indexL newLen arr

    where

    getIndexR idx
        | idx < 0 = return idx
        | otherwise = do
            r <- unsafeGetIndex idx arr
            if p r
            then getIndexR (idx - 1)
            else return idx

    getIndexL idx = do
        r <- unsafeGetIndex idx arr
        if p r
        then getIndexL (idx + 1)
        else return idx

--------------------------------------------------------------------------------
-- Renaming
--------------------------------------------------------------------------------

RENAME(strip,dropAround)
RENAME(putIndexUnsafe, unsafePutIndex)
RENAME(modifyIndexUnsafe, unsafeModifyIndex)
RENAME(getIndexUnsafe, unsafeGetIndex)
RENAME(getIndexUnsafeWith, unsafeGetIndexWith)
RENAME(getSliceUnsafe,unsafeSliceOffLen)
RENAME(putSliceUnsafe, unsafePutSlice)
RENAME(getSlice,sliceOffLen)
RENAME(snocUnsafe, unsafeSnoc)
