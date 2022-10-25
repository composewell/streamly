{-# LANGUAGE UnboxedTuples #-}

-- |
-- Module      : Streamly.Internal.Data.Array.Mut.Type
-- Copyright   : (c) 2020 Composewell Technologies
-- License     : BSD3-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
module Streamly.Internal.Data.Array.Mut.Type
(
    -- * Type
    -- $arrayNotes
      Array (..)

    -- * Constructing and Writing
    -- ** Construction
    -- , nil

    -- *** Uninitialized Arrays
    , newArray
    -- , newArrayWith

    -- *** From streams
    , writeNUnsafe
    , writeN

    -- , writeWith
    -- , write

    -- , writeRevN
    -- , writeRev

    -- ** From containers
    -- , fromListN
    -- , fromList
    -- , fromStreamDN
    -- , fromStreamD

    -- * Random writes
    , putIndex
    , putIndexUnsafe
    -- , putIndices
    -- , putFromThenTo
    -- , putFrom -- start writing at the given position
    -- , putUpto -- write from beginning up to the given position
    -- , putFromTo
    -- , putFromRev
    -- , putUptoRev
    , modifyIndexUnsafe
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

    -- ** Appending elements
    , snocWith
    , snoc
    -- , snocLinear
    -- , snocMay
    , snocUnsafe

    -- ** Appending streams
    -- , appendNUnsafe
    -- , appendN
    -- , appendWith
    -- , append

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
    , producer -- experimental

    -- ** To containers
    , toStreamD
    -- , toStreamDRev
    , toStreamK
    -- , toStreamKRev
    , toList

    -- ** Random reads
    , getIndex
    , getIndexUnsafe
    -- , getIndices
    -- , getFromThenTo
    -- , getIndexRev

    -- * In-place Mutation Algorithms
    -- , reverse
    -- , permute
    -- , partitionBy
    -- , shuffleBy
    -- , divideBy
    -- , mergeBy

    -- * Folding
    -- , foldl'
    -- , foldr
    -- , cmp

    -- * Arrays of arrays
    --  We can add dimensionality parameter to the array type to get
    --  multidimensional arrays. Multidimensional arrays would just be a
    --  convenience wrapper on top of single dimensional arrays.

    -- | Operations dealing with multiple arrays, streams of arrays or
    -- multidimensional array representations.

    -- ** Construct from streams
    -- , arraysOf
    -- , arrayStreamKFromStreamD
    -- , writeChunks

    -- ** Eliminate to streams
    -- , flattenArrays
    -- , flattenArraysRev
    -- , fromArrayStreamK

    -- ** Construct from arrays
    -- get chunks without copying
    , getSliceUnsafe
    , getSlice
    -- , getSlicesFromLenN
    -- , splitAt -- XXX should be able to express using getSlice
    -- , breakOn

    -- ** Appending arrays
    -- , spliceCopy
    -- , spliceWith
    -- , splice
    -- , spliceExp
    , putSliceUnsafe
    -- , appendSlice
    -- , appendSliceFrom

    , clone
    )
where

#include "inline.hs"
#include "assert.hs"

import Control.Monad.IO.Class (MonadIO(..))
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

import qualified Streamly.Internal.Data.Fold.Type as FL
import qualified Streamly.Internal.Data.Producer as Producer
import qualified Streamly.Internal.Data.Stream.StreamD as D
import qualified Streamly.Internal.Data.Stream.StreamK as K

import Prelude hiding (read)

-- $setup
-- >>> :m
-- >>> import qualified Streamly.Internal.Data.Array.Mut.Type as Array
-- >>> import qualified Streamly.Internal.Data.Stream as Stream
-- >>> import qualified Streamly.Internal.Data.Stream.StreamD as StreamD
-- >>> import qualified Streamly.Internal.Data.Fold as Fold
-- >>> import qualified Streamly.Internal.Data.Fold.Type as Fold

-------------------------------------------------------------------------------
-- Array Data Type
-------------------------------------------------------------------------------

data Array a =
    Array
        { arrContents# :: MutableArray# RealWorld a
          -- ^ The internal contents of the array representing the entire array.

        , arrStart :: {-# UNPACK #-}!Int
          -- ^ The starting index of this slice.

        , arrLen :: {-# UNPACK #-}!Int
          -- ^ The length of this slice.

        , arrTrueLen :: {-# UNPACK #-}!Int
          -- ^ This is the true length of the array. Coincidentally, this also
          -- represents the first index beyond the maximum acceptable index of
          -- the array. This is specific to the array contents itself and not
          -- dependent on the slice. This value should not change and is shared
          -- across all the slices.
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

    funcName = "Streamly.Internal.Data.Array.Mut.Type.bottomElement:"

-- XXX Would be nice if GHC can provide something like newUninitializedArray# so
-- that we do not have to write undefined or error in the whole array.
-- | @newArray count@ allocates an empty array that can hold 'count' items.
--
-- /Pre-release/
{-# INLINE newArray #-}
newArray :: forall m a. MonadIO m => Int -> m (Array a)
newArray n@(I# n#) =
    liftIO
        $ IO
        $ \s# ->
              case newArray# n# bottomElement s# of
                  (# s1#, arr# #) ->
                      let ma = Array arr# 0 0 n
                       in (# s1#, ma #)

-------------------------------------------------------------------------------
-- Random writes
-------------------------------------------------------------------------------

-- | Write the given element to the given index of the array. Does not check if
-- the index is out of bounds of the array.
--
-- /Pre-release/
{-# INLINE putIndexUnsafe #-}
putIndexUnsafe :: forall m a. MonadIO m => Array a -> Int -> a -> m ()
putIndexUnsafe Array {..} i x =
    liftIO
        $ IO
        $ \s# ->
              case i + arrStart of
                  I# n# ->
                      let s1# = writeArray# arrContents# n# x s#
                       in (# s1#, () #)

invalidIndex :: String -> Int -> a
invalidIndex label i =
    error $ label ++ ": invalid array index " ++ show i

-- | /O(1)/ Write the given element at the given index in the array.
-- Performs in-place mutation of the array.
--
-- >>> putIndex arr ix val = Array.modifyIndex arr ix (const (val, ()))
--
-- /Pre-release/
{-# INLINE putIndex #-}
putIndex :: MonadIO m => Array a -> Int -> a -> m ()
putIndex arr@Array {..} i x =
    if i >= 0 && i < arrLen
    then putIndexUnsafe arr i x
    else invalidIndex "putIndex" i

-- | Modify a given index of an array using a modifier function without checking
-- the bounds.
--
-- Unsafe because it does not check the bounds of the array.
--
-- /Pre-release/
modifyIndexUnsafe :: MonadIO m => Array a -> Int -> (a -> (a, b)) -> m b
modifyIndexUnsafe Array {..} i f = do
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
modifyIndex :: MonadIO m => Array a -> Int -> (a -> (a, b)) -> m b
modifyIndex arr@Array {..} i f = do
    if i >= 0 && i < arrLen
    then modifyIndexUnsafe arr i f
    else invalidIndex "modifyIndex" i

-------------------------------------------------------------------------------
-- Resizing
-------------------------------------------------------------------------------

-- | Reallocates the array according to the new size. This is a safe function
-- that always creates a new array and copies the old array into the new one. If
-- the reallocated size is less than the original array it results in a
-- truncated version of the original array.
--
realloc :: MonadIO m => Int -> Array a -> m (Array a)
realloc n arr = do
    arr1 <- newArray n
    let !newLen@(I# newLen#) = min n (arrLen arr)
        !(I# arrS#) = arrStart arr
        !(I# arr1S#) = arrStart arr1
        arrC# = arrContents# arr
        arr1C# = arrContents# arr1
    liftIO
        $ IO
        $ \s# ->
              let s1# = copyMutableArray# arrC# arrS# arr1C# arr1S# newLen# s#
               in (# s1#, arr1 {arrLen = newLen, arrTrueLen = n} #)

reallocWith ::
       MonadIO m => String -> (Int -> Int) -> Int -> Array a -> m (Array a)
reallocWith label sizer reqSize arr = do
    let oldSize = arrLen arr
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
{-# INLINE snocUnsafe #-}
snocUnsafe :: MonadIO m => Array a -> a -> m (Array a)
snocUnsafe arr@Array {..} a = do
    assert (arrStart + arrLen < arrTrueLen) (return ())
    putIndexUnsafe arr arrLen a
    return $ arr {arrLen = arrLen + 1}

-- NOINLINE to move it out of the way and not pollute the instruction cache.
{-# NOINLINE snocWithRealloc #-}
snocWithRealloc :: MonadIO m => (Int -> Int) -> Array a -> a -> m (Array a)
snocWithRealloc sizer arr x = do
    arr1 <- reallocWith "snocWithRealloc" sizer 1 arr
    snocUnsafe arr1 x

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
snocWith :: MonadIO m => (Int -> Int) -> Array a -> a -> m (Array a)
snocWith sizer arr@Array {..} x = do
    if arrStart + arrLen < arrTrueLen
    then snocUnsafe arr x
    else snocWithRealloc sizer arr x

-- XXX round it to next power of 2.
--
-- | The array is mutated to append an additional element to it. If there is no
-- reserved space available in the array then it is reallocated to double the
-- original size.
--
-- This is useful to reduce allocations when appending unknown number of
-- elements.
--
-- Note that the returned array may be a mutated version of the original array.
--
-- >>> snoc = Array.snocWith (* 2)
--
-- Performs O(n * log n) copies to grow, but is liberal with memory allocation.
--
-- /Pre-release/
{-# INLINE snoc #-}
snoc :: MonadIO m => Array a -> a -> m (Array a)
snoc = snocWith (* 2)

-------------------------------------------------------------------------------
-- Random reads
-------------------------------------------------------------------------------

-- | Return the element at the specified index without checking the bounds.
--
-- Unsafe because it does not check the bounds of the array.
{-# INLINE_NORMAL getIndexUnsafe #-}
getIndexUnsafe :: MonadIO m => Array a -> Int -> m a
getIndexUnsafe Array {..} n =
    liftIO
        $ IO
        $ \s# ->
              let !(I# i#) = arrStart + n
               in readArray# arrContents# i# s#

-- | /O(1)/ Lookup the element at the given index. Index starts from 0.
--
{-# INLINE getIndex #-}
getIndex :: MonadIO m => Array a -> Int -> m a
getIndex arr@Array {..} i =
    if i >= 0 && i < arrLen
    then getIndexUnsafe arr i
    else invalidIndex "getIndex" i

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
{-# INLINE getSliceUnsafe #-}
getSliceUnsafe
    :: Int -- ^ from index
    -> Int -- ^ length of the slice
    -> Array a
    -> Array a
getSliceUnsafe index len arr@Array {..} =
    assert (index >= 0 && len >= 0 && index + len <= arrLen)
        $ arr {arrStart = arrStart + index, arrLen = len}


-- | /O(1)/ Slice an array in constant time. Throws an error if the slice
-- extends out of the array bounds.
--
-- /Pre-release/
{-# INLINE getSlice #-}
getSlice
    :: Int -- ^ from index
    -> Int -- ^ length of the slice
    -> Array a
    -> Array a
getSlice index len arr@Array{..} =
    if index >= 0 && len >= 0 && index + len <= arrLen
    then arr {arrStart = arrStart + index, arrLen = len}
    else error
             $ "getSlice: invalid slice, index "
             ++ show index ++ " length " ++ show len

-------------------------------------------------------------------------------
-- to Lists and streams
-------------------------------------------------------------------------------

-- | Convert an 'Array' into a list.
--
-- /Pre-release/
{-# INLINE toList #-}
toList :: MonadIO m => Array a -> m [a]
toList arr@Array{..} = mapM (getIndexUnsafe arr) [0 .. (arrLen - 1)]

-- | Use the 'read' unfold instead.
--
-- @toStreamD = D.unfold read@
--
-- We can try this if the unfold has any performance issues.
{-# INLINE_NORMAL toStreamD #-}
toStreamD :: MonadIO m => Array a -> D.Stream m a
toStreamD arr@Array{..} =
    D.mapM (getIndexUnsafe arr) $ D.enumerateFromToIntegral 0 (arrLen - 1)

{-# INLINE toStreamK #-}
toStreamK :: MonadIO m => Array a -> K.Stream m a
toStreamK arr@Array{..} = K.unfoldrM step 0

    where

    step i
        | i == arrLen = return Nothing
        | otherwise = do
            x <- getIndexUnsafe arr i
            return $ Just (x, i + 1)


-------------------------------------------------------------------------------
-- Folds
-------------------------------------------------------------------------------

-- | Like 'writeN' but does not check the array bounds when writing. The fold
-- driver must not call the step function more than 'n' times otherwise it will
-- corrupt the memory and crash. This function exists mainly because any
-- conditional in the step function blocks fusion causing 10x performance
-- slowdown.
--
-- /Pre-release/
{-# INLINE_NORMAL writeNUnsafe #-}
writeNUnsafe :: MonadIO m => Int -> Fold m a (Array a)
writeNUnsafe n = Fold step initial return

    where

    initial = FL.Partial <$> newArray (max n 0)

    step arr x = FL.Partial <$> snocUnsafe arr x

-- | @writeN n@ folds a maximum of @n@ elements from the input stream to an
-- 'Array'.
--
-- >>> writeN n = Fold.take n (Array.writeNUnsafe n)
--
-- /Pre-release/
{-# INLINE_NORMAL writeN #-}
writeN :: MonadIO m => Int -> Fold m a (Array a)
writeN n = FL.take n $ writeNUnsafe n

-------------------------------------------------------------------------------
-- Unfolds
-------------------------------------------------------------------------------

-- | Resumable unfold of an array.
--
{-# INLINE_NORMAL producer #-}
producer :: MonadIO m => Producer m (Array a) a
producer = Producer step inject extract

    where

    {-# INLINE inject #-}
    inject arr = return (arr, 0)

    {-# INLINE extract #-}
    extract (arr, i) =
        return $ arr {arrStart = arrStart arr + i, arrLen = arrLen arr - i}

    {-# INLINE_LATE step #-}
    step (arr, i)
        | assert (arrLen arr >= 0) (i == arrLen arr) = return D.Stop
    step (arr, i) = do
        x <- getIndexUnsafe arr i
        return $ D.Yield x (arr, i + 1)

-- | Unfold an array into a stream.
--
{-# INLINE_NORMAL reader #-}
reader :: MonadIO m => Unfold m (Array a) a
reader = Producer.simplify producer

--------------------------------------------------------------------------------
-- Appending arrays
--------------------------------------------------------------------------------

-- | Put a sub range of a source array into a subrange of a destination array.
-- This is not safe as it does not check the bounds.
{-# INLINE putSliceUnsafe #-}
putSliceUnsafe :: MonadIO m => Array a -> Int -> Array a -> Int -> Int -> m ()
putSliceUnsafe src srcStart dst dstStart len = liftIO $ do
    assertM(len <= arrLen dst)
    assertM(len <= arrLen src)
    let !(I# srcStart#) = srcStart + arrStart src
        !(I# dstStart#) = dstStart + arrStart dst
        !(I# len#) = len
    let arrS# = arrContents# src
        arrD# = arrContents# dst
    IO $ \s# -> (# copyMutableArray#
                    arrS# srcStart# arrD# dstStart# len# s#
                , () #)

{-# INLINE clone #-}
clone :: MonadIO m => Array a -> m (Array a)
clone src = liftIO $ do
    let len = arrLen src
    dst <- newArray len
    putSliceUnsafe src 0 dst 0 len
    return dst
