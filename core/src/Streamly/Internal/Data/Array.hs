{-# LANGUAGE CPP #-}
-- |
-- Module      : Streamly.Internal.Data.Array
-- Copyright   : (c) 2019 Composewell Technologies
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
module Streamly.Internal.Data.Array
    (
    -- * Setup
    -- $setup

    -- * Design Notes
    -- $design

    -- * The Array Type
      module Streamly.Internal.Data.Array.Type

    -- * Construction
    -- Monadic Folds
    , createOfLast

    -- * Random Access
    -- , (!!)
    , getIndex
    , getIndexRev
    , last           -- XXX getLastIndex?
    -- , getIndicesFrom    -- read from a given position to the end of file
    -- , getIndicesUpto    -- read from beginning up to the given position
    -- , getIndicesFromTo
    -- , getIndicesFromRev  -- read from a given position to the beginning of file
    -- , getIndicesUptoRev  -- read from end to the given position in file
    , indexReader
    , indexReaderFromThenTo

    -- * Size
    , null

    -- * Search
    , binarySearch
    , findIndicesOf
    -- getIndicesOf
    , indexFinder -- see splitOn
    -- , findIndexOf
    -- , find

    -- * Casting
    , cast
    , asBytes
    , castUnsafe
    , asCStringUnsafe

    -- * Subarrays
    , getSliceUnsafe
    -- , getSlice
    , sliceIndexerFromLen
    , slicerFromLen
    , splitOn

    -- * Streaming Operations
    , streamTransform

    -- * Folding
    , streamFold
    , fold

    -- * Stream of Arrays

    -- XXX these are probably not very useful to have in this module as we can
    -- express these idiomatically using streams.
    , interpose
    , interposeSuffix
    , intercalateSuffix

    , compactLE
    , pinnedCompactLE
    , compactOnByte
    , compactOnByteSuffix

    , foldBreakChunks
    , foldChunks
    , foldBreakChunksK
    , parseBreakChunksK

    -- * Serialization
    , encodeAs
    , serialize
    , pinnedSerialize
    , deserialize

    -- * Deprecated
    , genSlicesFromLen
    , getSlicesFromLen
    , getIndices
    , writeLastN
    )
where

#include "assert.hs"
#include "inline.hs"
#include "ArrayMacros.h"

import Control.Monad.IO.Class (MonadIO(..))
-- import Data.Bifunctor (first)
-- import Data.Either (fromRight)
import Data.Functor.Identity (Identity)
import Data.Proxy (Proxy(..))
import Data.Word (Word8)
import Foreign.C.String (CString)
import Foreign.Ptr (castPtr)
import GHC.Types (SPEC(..))
import Streamly.Internal.Data.Unbox (Unbox(..))
import Prelude hiding (length, null, last, map, (!!), read, concat)

import Streamly.Internal.Data.MutByteArray.Type (PinnedState(..), MutByteArray)
import Streamly.Internal.Data.Serialize.Type (Serialize)
import Streamly.Internal.Data.Fold.Type (Fold(..))
import Streamly.Internal.Data.Parser (Parser(..), Initial(..), ParseError(..))
import Streamly.Internal.Data.Stream (Stream(..))
import Streamly.Internal.Data.StreamK (StreamK)
import Streamly.Internal.Data.SVar.Type (adaptState, defState)
import Streamly.Internal.Data.Tuple.Strict (Tuple'(..), Tuple3Fused'(..))
import Streamly.Internal.Data.Unfold.Type (Unfold(..))
import Streamly.Internal.System.IO (unsafeInlineIO)

import qualified Streamly.Internal.Data.Fold.Type as Fold
import qualified Streamly.Internal.Data.Serialize.Type as Serialize
import qualified Streamly.Internal.Data.MutByteArray.Type as MBA
import qualified Streamly.Internal.Data.MutArray as MA
import qualified Streamly.Internal.Data.Fold as FL
import qualified Streamly.Internal.Data.Ring as RB
import qualified Streamly.Internal.Data.Parser as Parser
-- import qualified Streamly.Internal.Data.ParserK as ParserK
import qualified Streamly.Internal.Data.Stream as D
import qualified Streamly.Internal.Data.Stream as Stream
import qualified Streamly.Internal.Data.StreamK as StreamK
import qualified Streamly.Internal.Data.Unfold as Unfold
import qualified Prelude

import Streamly.Internal.Data.Array.Type

#include "DocTestDataArray.hs"

-- $design
--
-- To summarize:
--
--  * Arrays are finite and fixed in size
--  * provide /O(1)/ access to elements
--  * store only data and not functions
--  * provide efficient IO interfacing
--
-- 'Foldable' instance is not provided because the implementation would be much
-- less efficient compared to folding via streams.  'Semigroup' and 'Monoid'
-- instances should be used with care; concatenating arrays using binary
-- operations can be highly inefficient.  Instead, use
-- 'Streamly.Internal.Data.Stream.Chunked.toArray' to concatenate N
-- arrays at once.
--
-- Each array is one pointer visible to the GC.  Too many small arrays (e.g.
-- single byte) are only as good as holding those elements in a Haskell list.
-- However, small arrays can be compacted into large ones to reduce the
-- overhead. To hold 32GB memory in 32k sized buffers we need 1 million arrays
-- if we use one array for each chunk. This is still significant to add
-- pressure to GC.

-------------------------------------------------------------------------------
-- Elimination
-------------------------------------------------------------------------------

-- |
--
-- >>> null arr = Array.byteLength arr == 0
--
-- /Pre-release/
{-# INLINE null #-}
null :: Array a -> Bool
null arr = byteLength arr == 0

-- | Like 'getIndex' but indexes the array in reverse from the end.
--
-- /Pre-release/
{-# INLINE getIndexRev #-}
getIndexRev :: forall a. Unbox a => Int -> Array a -> Maybe a
getIndexRev i arr =
    unsafeInlineIO
        $ do
                let elemPtr = RINDEX_OF(arrEnd arr, i, a)
                if i >= 0 && elemPtr >= arrStart arr
                then Just <$> peekAt elemPtr (arrContents arr)
                else return Nothing

-- |
--
-- >>> last arr = Array.getIndexRev arr 0
--
-- /Pre-release/
{-# INLINE last #-}
last :: Unbox a => Array a -> Maybe a
last = getIndexRev 0

-------------------------------------------------------------------------------
-- Folds with Array as the container
-------------------------------------------------------------------------------

-- XXX We should generate this from Ring.

-- | @createOfLast n@ folds a maximum of @n@ elements from the end of the input
-- stream to an 'Array'.
--
{-# INLINE createOfLast #-}
createOfLast ::
       (Unbox a, MonadIO m) => Int -> Fold m a (Array a)
createOfLast n
    | n <= 0 = fmap (const mempty) FL.drain
    | otherwise = unsafeFreeze <$> Fold step initial done done

    where

    step (Tuple3Fused' rb rh i) a = do
        rh1 <- liftIO $ RB.unsafeInsert rb rh a
        return $ FL.Partial $ Tuple3Fused' rb rh1 (i + 1)

    initial =
        let f a = FL.Partial $ Tuple3Fused' a 0 (0 :: Int)
         in fmap f $ liftIO $ RB.emptyOf n

    done (Tuple3Fused' rb rh i) = do
        arr <- MA.emptyOf n
        -- XXX We should write a read unfold for ring.
        foldFunc i rh MA.snocUnsafe arr rb

    foldFunc i
        | i < n = RB.unsafeFoldRingM
        | otherwise = RB.unsafeFoldRingFullM

{-# DEPRECATED writeLastN "Please use createOfLast instead." #-}
{-# INLINE writeLastN #-}
writeLastN :: (Unbox a, MonadIO m) => Int -> Fold m a (Array a)
writeLastN = createOfLast

-------------------------------------------------------------------------------
-- Random Access
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- Searching
-------------------------------------------------------------------------------

-- | Given a sorted array, perform a binary search to find the given element.
-- Returns the index of the element if found.
--
-- /Unimplemented/
{-# INLINE binarySearch #-}
binarySearch :: a -> Array a -> Maybe Int
binarySearch = undefined

-- find/findIndex etc can potentially be implemented more efficiently on arrays
-- compared to streams by using SIMD instructions.
-- We can also return a bit array instead.

-- Can use SIMD.

-- | Perform a linear search to find all the indices where a given element is
-- present in an array.
--
-- /Unimplemented/
indexFinder :: (a -> Bool) -> Unfold Identity (Array a) Int
indexFinder = undefined

-- |
-- /Unimplemented/
findIndicesOf :: (a -> Bool) -> Array a -> Stream Identity Int
findIndicesOf p = Stream.unfold (indexFinder p)

{-
findIndexOf :: (a -> Bool) -> Array a -> Maybe Int
findIndexOf p = Unfold.fold Fold.one . Stream.unfold (indexFinder p)

find :: (a -> Bool) -> Array a -> Bool
find = Unfold.fold Fold.null . Stream.unfold (indexFinder p)
-}

-------------------------------------------------------------------------------
-- Folds
-------------------------------------------------------------------------------

-- XXX We can potentially use SIMD instructions on arrays to fold faster.

-------------------------------------------------------------------------------
-- Slice
-------------------------------------------------------------------------------

-- | /O(1)/ Slice an array in constant time.
--
-- Caution: The bounds of the slice are not checked.
--
-- /Unsafe/
--
-- /Pre-release/
{-# INLINE getSliceUnsafe #-}
getSliceUnsafe ::
       forall a. Unbox a
    => Int -- ^ starting index
    -> Int -- ^ length of the slice
    -> Array a
    -> Array a
getSliceUnsafe index len (Array contents start e) =
    let size = SIZE_OF(a)
        start1 = start + (index * size)
        end1 = start1 + (len * size)
     in assert (end1 <= e) (Array contents start1 end1)

-- | Split the array into a stream of slices using a predicate. The element
-- matching the predicate is dropped.
--
-- /Pre-release/
{-# INLINE splitOn #-}
splitOn :: (Monad m, Unbox a) =>
    (a -> Bool) -> Array a -> Stream m (Array a)
splitOn predicate arr =
    fmap (\(i, len) -> getSliceUnsafe i len arr)
        $ D.indexOnSuffix predicate (read arr)

{-# INLINE sliceIndexerFromLen #-}
sliceIndexerFromLen :: forall m a. (Monad m, Unbox a)
    => Int -- ^ from index
    -> Int -- ^ length of the slice
    -> Unfold m (Array a) (Int, Int)
sliceIndexerFromLen from len =
    Unfold.lmap unsafeThaw (MA.sliceIndexerFromLen from len)

{-# DEPRECATED genSlicesFromLen "Please use sliceIndexerFromLen instead." #-}
{-# INLINE genSlicesFromLen #-}
genSlicesFromLen :: forall m a. (Monad m, Unbox a)
    => Int -- ^ from index
    -> Int -- ^ length of the slice
    -> Unfold m (Array a) (Int, Int)
genSlicesFromLen = sliceIndexerFromLen

-- | Generate a stream of slices of specified length from an array, starting
-- from the supplied array index. The last slice may be shorter than the
-- requested length.
--
-- /Pre-release//
{-# INLINE slicerFromLen #-}
slicerFromLen :: forall m a. (Monad m, Unbox a)
    => Int -- ^ from index
    -> Int -- ^ length of the slice
    -> Unfold m (Array a) (Array a)
slicerFromLen from len =
    fmap unsafeFreeze
        $ Unfold.lmap unsafeThaw (MA.slicerFromLen from len)

{-# DEPRECATED getSlicesFromLen "Please use slicerFromLen instead." #-}
{-# INLINE getSlicesFromLen #-}
getSlicesFromLen :: forall m a. (Monad m, Unbox a)
    => Int -- ^ from index
    -> Int -- ^ length of the slice
    -> Unfold m (Array a) (Array a)
getSlicesFromLen = slicerFromLen

-------------------------------------------------------------------------------
-- Random reads and writes
-------------------------------------------------------------------------------

-- XXX Change this to a partial function instead of a Maybe type? And use
-- MA.getIndex instead.
--
-- | /O(1)/ Lookup the element at the given index. Index starts from 0.
--
{-# INLINE getIndex #-}
getIndex :: forall a. Unbox a => Int -> Array a -> Maybe a
getIndex i arr =
    unsafeInlineIO
        $ do
                let elemPtr = INDEX_OF(arrStart arr, i, a)
                if i >= 0 && INDEX_VALID(elemPtr, arrEnd arr, a)
                then Just <$> peekAt elemPtr (arrContents arr)
                else return Nothing

-- | Given a stream of array indices, read the elements on those indices from
-- the supplied Array. An exception is thrown if an index is out of bounds.
--
-- This is the most general operation. We can implement other operations in
-- terms of this:
--
-- @
-- read =
--      let u = lmap (\arr -> (0, length arr - 1)) Unfold.enumerateFromTo
--       in Unfold.lmap f (indexReader arr)
--
-- readRev =
--      let i = length arr - 1
--       in Unfold.lmap f (indexReaderFromThenTo i (i - 1) 0)
-- @
--
-- /Pre-release/
{-# INLINE indexReader #-}
indexReader :: (Monad m, Unbox a) => Stream m Int -> Unfold m (Array a) a
indexReader m =
    let unf = MA.indexReaderWith (return . unsafeInlineIO) m
     in Unfold.lmap unsafeThaw unf

-- XXX DO NOT REMOVE, change the signature to use Stream instead of unfold
{-# DEPRECATED getIndices "Please use getIndices instead." #-}
{-# INLINE getIndices #-}
getIndices :: (Monad m, Unbox a) => Stream m Int -> Unfold m (Array a) a
getIndices = indexReader

-- | Unfolds @(from, then, to, array)@ generating a finite stream whose first
-- element is the array value from the index @from@ and the successive elements
-- are from the indices in increments of @then@ up to @to@. Index enumeration
-- can occur downwards or upwards depending on whether @then@ comes before or
-- after @from@.
--
-- @
-- getIndicesFromThenTo =
--     let f (from, next, to, arr) =
--             (Stream.enumerateFromThenTo from next to, arr)
--      in Unfold.lmap f getIndices
-- @
--
-- /Unimplemented/
{-# INLINE indexReaderFromThenTo #-}
indexReaderFromThenTo :: Unfold m (Int, Int, Int, Array a) a
indexReaderFromThenTo = undefined

-------------------------------------------------------------------------------
-- Transform via stream operations
-------------------------------------------------------------------------------

-- for non-length changing operations we can use the original length for
-- allocation. If we can predict the length then we can use the prediction for
-- new allocation. Otherwise we can use a hint and adjust dynamically.

{-
-- | Transform an array into another array using a pipe transformation
-- operation.
--
{-# INLINE runPipe #-}
runPipe :: (MonadIO m, Unbox a, Unbox b)
    => Pipe m a b -> Array a -> m (Array b)
runPipe f arr = P.runPipe (toArrayMinChunk (length arr)) $ f (read arr)
-}

-- XXX For transformations that cannot change the number of elements e.g. "map"
-- we can use a predetermined array length.
--
-- | Transform an array into another array using a stream transformation
-- operation.
--
-- /Pre-release/
{-# INLINE streamTransform #-}
streamTransform :: forall m a b. (MonadIO m, Unbox a, Unbox b)
    => (Stream m a -> Stream m b) -> Array a -> m (Array b)
streamTransform f arr =
    Stream.fold (createWith (length arr)) $ f (read arr)

-------------------------------------------------------------------------------
-- Casts
-------------------------------------------------------------------------------

-- | Cast an array having elements of type @a@ into an array having elements of
-- type @b@. The array size must be a multiple of the size of type @b@
-- otherwise accessing the last element of the array may result into a crash or
-- a random value.
--
-- /Pre-release/
--
castUnsafe ::
#ifdef DEVBUILD
    Unbox b =>
#endif
    Array a -> Array b
castUnsafe (Array contents start end) =
    Array contents start end

-- | Cast an @Array a@ into an @Array Word8@.
--
--
asBytes :: Array a -> Array Word8
asBytes = castUnsafe

-- | Cast an array having elements of type @a@ into an array having elements of
-- type @b@. The length of the array should be a multiple of the size of the
-- target element otherwise 'Nothing' is returned.
--
--
cast :: forall a b. (Unbox b) => Array a -> Maybe (Array b)
cast arr =
    let len = byteLength arr
        r = len `mod` SIZE_OF(b)
     in if r /= 0
        then Nothing
        else Just $ castUnsafe arr

-- | Convert an array of any type into a null terminated CString Ptr.  If the
-- array is unpinned it is first converted to a pinned array which requires a
-- copy.
--
-- /Unsafe/
--
-- /O(n) Time: (creates a copy of the array)/
--
-- /Pre-release/
--
asCStringUnsafe :: Array a -> (CString -> IO b) -> IO b
asCStringUnsafe arr act = do
    let arr1 = asBytes arr <> fromList [0]
    -- unsafePinnedAsPtr makes sure the array is pinned
    unsafePinnedAsPtr arr1 $ \ptr -> act (castPtr ptr)

-------------------------------------------------------------------------------
-- Folds
-------------------------------------------------------------------------------

-- XXX We can directly use toStreamD and D.fold here.

-- | Fold an array using a 'Fold'.
--
-- /Pre-release/
{-# INLINE fold #-}
fold :: forall m a b. (Monad m, Unbox a) => Fold m a b -> Array a -> m b
fold f arr = Stream.fold f (read arr)

-- | Fold an array using a stream fold operation.
--
-- /Pre-release/
{-# INLINE streamFold #-}
streamFold :: (Monad m, Unbox a) => (Stream m a -> m b) -> Array a -> m b
streamFold f arr = f (read arr)

--------------------------------------------------------------------------------
-- Serialization
--------------------------------------------------------------------------------

{-# INLINE encodeAs #-}
encodeAs :: forall a. Serialize a => PinnedState -> a -> Array Word8
encodeAs ps a =
    unsafeInlineIO $ do
        let len = Serialize.addSizeTo 0 a
        mbarr <- MBA.newBytesAs ps len
        off <- Serialize.serializeAt 0 mbarr a
        assertM(len == off)
        pure $ Array mbarr 0 off

-- |
-- Properties:
-- 1. Identity: @deserialize . serialize == id@
-- 2. Encoded equivalence: @serialize a == serialize a@
{-# INLINE serialize #-}
serialize :: Serialize a => a -> Array Word8
serialize = encodeAs Unpinned

-- | Serialize a Haskell type to a pinned byte array. The array is allocated
-- using pinned memory so that it can be used directly in OS APIs for writing
-- to file or sending over the network.
--
-- Properties:
--
-- 1. Identity: @deserialize . pinnedSerialize == id@
-- 2. Encoded equivalence: @pinnedSerialize a == pinnedSerialize a@
{-# INLINE pinnedSerialize #-}
pinnedSerialize :: Serialize a => a -> Array Word8
pinnedSerialize = encodeAs Pinned

-- XXX We can deserialize it like MutArray, returning the remaining slice.

-- | Decode a Haskell type from a byte array containing its serialized
-- representation.
{-# INLINE deserialize #-}
deserialize :: Serialize a => Array Word8 -> a
deserialize arr@(Array {..}) = unsafeInlineIO $ do
    let lenArr = length arr
    (off, val) <-
        Serialize.deserializeAt arrStart arrContents (arrStart + lenArr)
    assertM(off == arrStart + lenArr)
    pure val

-------------------------------------------------------------------------------
-- Streams of Arrays
-------------------------------------------------------------------------------

-- TODO: efficiently compare two streams of arrays. Two streams can have chunks
-- of different sizes, we can handle that in the stream comparison abstraction.
-- This could be useful e.g. to fast compare whether two files differ.

-- | Insert the given element between arrays and flatten.
--
-- >>> interpose x = Stream.interpose x Array.reader
--
{-# INLINE interpose #-}
interpose :: (Monad m, Unbox a) => a -> Stream m (Array a) -> Stream m a
interpose x = D.interpose x reader

data FlattenState s =
      OuterLoop s
    | InnerLoop s !MutByteArray !Int !Int

-- | Insert the given element after each array and flatten. This is similar to
-- unlines.
--
-- >>> interposeSuffix x = Stream.interposeSuffix x Array.reader
--
{-# INLINE_NORMAL interposeSuffix #-}
interposeSuffix :: forall m a. (Monad m, Unbox a)
    => a -> Stream m (Array a) -> Stream m a
-- interposeSuffix x = D.interposeSuffix x reader
interposeSuffix sep (D.Stream step state) = D.Stream step' (OuterLoop state)

    where

    {-# INLINE_LATE step' #-}
    step' gst (OuterLoop st) = do
        r <- step (adaptState gst) st
        return $ case r of
            D.Yield Array{..} s ->
                D.Skip (InnerLoop s arrContents arrStart arrEnd)
            D.Skip s -> D.Skip (OuterLoop s)
            D.Stop -> D.Stop

    step' _ (InnerLoop st _ p end) | p == end =
        return $ D.Yield sep $ OuterLoop st

    step' _ (InnerLoop st contents p end) = do
        let !x = unsafeInlineIO $ peekAt p contents
        return $ D.Yield x (InnerLoop st contents (INDEX_NEXT(p,a)) end)

-- | Insert the given array after each array and flatten.
--
-- >>> intercalateSuffix = Stream.intercalateSuffix Array.reader
--
{-# INLINE intercalateSuffix #-}
intercalateSuffix :: (Monad m, Unbox a)
    => Array a -> Stream m (Array a) -> Stream m a
intercalateSuffix = D.intercalateSuffix reader

-- | @compactLE n@ coalesces adjacent arrays in the input stream
-- only if the combined size would be less than or equal to n.
--
-- Generates unpinned arrays irrespective of the pinning status of input
-- arrays.
{-# INLINE_NORMAL compactLE #-}
compactLE :: (MonadIO m, Unbox a)
    => Int -> Stream m (Array a) -> Stream m (Array a)
compactLE n stream =
    D.map unsafeFreeze $ MA.compactLE n $ D.map unsafeThaw stream

-- | Pinned version of 'compactLE'.
{-# INLINE_NORMAL pinnedCompactLE #-}
pinnedCompactLE :: (MonadIO m, Unbox a)
    => Int -> Stream m (Array a) -> Stream m (Array a)
pinnedCompactLE n stream =
    D.map unsafeFreeze $ MA.pinnedCompactLE n $ D.map unsafeThaw stream

-- | Split a stream of arrays on a given separator byte, dropping the separator
-- and coalescing all the arrays between two separators into a single array.
--
{-# INLINE compactOnByte #-}
compactOnByte
    :: (MonadIO m)
    => Word8
    -> Stream m (Array Word8)
    -> Stream m (Array Word8)
compactOnByte byte =
    fmap unsafeFreeze . MA.compactOnByte byte . fmap unsafeThaw

-- | Like 'compactOnByte' considers the separator in suffix position instead of
-- infix position.
{-# INLINE compactOnByteSuffix #-}
compactOnByteSuffix
    :: (MonadIO m)
    => Word8
    -> Stream m (Array Word8)
    -> Stream m (Array Word8)
compactOnByteSuffix byte =
    fmap unsafeFreeze . MA.compactOnByteSuffix byte . fmap unsafeThaw

-------------------------------------------------------------------------------
-- Folding Streams of Arrays
-------------------------------------------------------------------------------

-- XXX This should not be used for breaking a stream as the D.cons used in
-- reconstructing the stream could be very bad for performance. This can only
-- be useful in folding without breaking.
{-# INLINE_NORMAL foldBreakChunks #-}
foldBreakChunks :: forall m a b. (MonadIO m, Unbox a) =>
    Fold m a b -> Stream m (Array a) -> m (b, Stream m (Array a))
foldBreakChunks (Fold fstep initial _ final) stream@(Stream step state) = do
    res <- initial
    case res of
        Fold.Partial fs -> go SPEC state fs
        Fold.Done fb -> return $! (fb, stream)

    where

    {-# INLINE go #-}
    go !_ st !fs = do
        r <- step defState st
        case r of
            Stream.Yield (Array contents start end) s ->
                let fp = Tuple' end contents
                 in goArray SPEC s fp start fs
            Stream.Skip s -> go SPEC s fs
            Stream.Stop -> do
                b <- final fs
                return (b, D.nil)

    goArray !_ s (Tuple' end _) !cur !fs
        | cur == end = do
            go SPEC s fs
    goArray !_ st fp@(Tuple' end contents) !cur !fs = do
        x <- liftIO $ peekAt cur contents
        res <- fstep fs x
        let next = INDEX_NEXT(cur,a)
        case res of
            Fold.Done b -> do
                let arr = Array contents next end
                return $! (b, D.cons arr (D.Stream step st))
            Fold.Partial fs1 -> goArray SPEC st fp next fs1

-- This may be more robust wrt fusion compared to unfoldMany?

-- | Fold a stream of arrays using a 'Fold'. This is equivalent to the
-- following:
--
-- >>> foldChunks f = Stream.fold f . Stream.unfoldMany Array.reader
--
foldChunks :: (MonadIO m, Unbox a) => Fold m a b -> Stream m (Array a) -> m b
foldChunks f s = fmap fst (foldBreakChunks f s)
-- foldStream f = Stream.fold f . Stream.unfoldMany reader

-- | Fold a stream of arrays using a 'Fold' and return the remaining stream.
--
-- The following alternative to this function allows composing the fold using
-- the parser Monad:
--
-- @
-- foldBreakStreamK f s =
--       fmap (first (fromRight undefined))
--     $ StreamK.parseBreakChunks (ParserK.adaptC (Parser.fromFold f)) s
-- @
--
-- We can compare perf and remove this one or define it in terms of that.
--
foldBreakChunksK :: forall m a b. (MonadIO m, Unbox a) =>
    Fold m a b -> StreamK m (Array a) -> m (b, StreamK m (Array a))
{-
foldBreakChunksK f s =
      fmap (first (fromRight undefined))
    $ StreamK.parseBreakChunks (ParserK.adaptC (Parser.fromFold f)) s
-}
foldBreakChunksK (Fold fstep initial _ final) stream = do
    res <- initial
    case res of
        Fold.Partial fs -> go fs stream
        Fold.Done fb -> return (fb, stream)

    where

    {-# INLINE go #-}
    go !fs st = do
        let stop = (, StreamK.nil) <$> final fs
            single a = yieldk a StreamK.nil
            yieldk (Array contents start end) r =
                let fp = Tuple' end contents
                 in goArray fs r fp start
         in StreamK.foldStream defState yieldk single stop st

    goArray !fs st (Tuple' end _) !cur
        | cur == end = do
            go fs st
    goArray !fs st fp@(Tuple' end contents) !cur = do
        x <- liftIO $ peekAt cur contents
        res <- fstep fs x
        let next = INDEX_NEXT(cur,a)
        case res of
            Fold.Done b -> do
                let arr = Array contents next end
                return $! (b, StreamK.cons arr st)
            Fold.Partial fs1 -> goArray fs1 st fp next

{-
-- This can be generalized to any type provided it can be unfolded to a stream
-- and it can be combined using a semigroup operation.
--
{-# INLINE_NORMAL parseBreakD #-}
parseBreakD ::
       forall m a b. (MonadIO m, MonadThrow m, Unbox a)
    => PRD.Parser a m b
    -> D.Stream m (Array.Array a)
    -> m (b, D.Stream m (Array.Array a))
parseBreakD
    (PRD.Parser pstep initial extract) stream@(D.Stream step state) = do

    res <- initial
    case res of
        PRD.IPartial s -> go SPEC state (List []) s
        PRD.IDone b -> return (b, stream)
        PRD.IError err -> throwM $ ParseError err

    where

    -- "backBuf" contains last few items in the stream that we may have to
    -- backtrack to.
    --
    -- XXX currently we are using a dumb list based approach for backtracking
    -- buffer. This can be replaced by a sliding/ring buffer using Data.Array.
    -- That will allow us more efficient random back and forth movement.
    go !_ st backBuf !pst = do
        r <- step defState st
        case r of
            D.Yield (Array contents start end) s ->
                gobuf SPEC s backBuf
                    (Tuple' end contents) start pst
            D.Skip s -> go SPEC s backBuf pst
            D.Stop -> do
                b <- extract pst
                return (b, D.nil)

    -- Use strictness on "cur" to keep it unboxed
    gobuf !_ s backBuf (Tuple' end _) !cur !pst
        | cur == end = do
            go SPEC s backBuf pst
    gobuf !_ s backBuf fp@(Tuple' end contents) !cur !pst = do
        x <- liftIO $ peekByteIndex contents cur
        pRes <- pstep pst x
        let next = INDEX_NEXT(cur,a)
        case pRes of
            PR.Partial 0 pst1 ->
                 gobuf SPEC s (List []) fp next pst1
            PR.Partial n pst1 -> do
                assert (n <= Prelude.length (x:getList backBuf)) (return ())
                let src0 = Prelude.take n (x:getList backBuf)
                    arr0 = A.fromListN n (Prelude.reverse src0)
                    arr1 = Array contents next end
                    src = arr0 <> arr1
                let !(Array cont1 start end1) = src
                    fp1 = Tuple' end1 cont1
                gobuf SPEC s (List []) fp1 start pst1
            PR.Continue 0 pst1 ->
                gobuf SPEC s (List (x:getList backBuf)) fp next pst1
            PR.Continue n pst1 -> do
                assert (n <= Prelude.length (x:getList backBuf)) (return ())
                let (src0, buf1) = splitAt n (x:getList backBuf)
                    arr0 = A.fromListN n (Prelude.reverse src0)
                    arr1 = Array contents next end
                    src = arr0 <> arr1
                let !(Array cont1 start end1) = src
                    fp1 = Tuple' end1 cont1
                gobuf SPEC s (List buf1) fp1 start pst1
            PR.Done 0 b -> do
                let arr = Array contents next end
                return (b, D.cons arr (D.Stream step s))
            PR.Done n b -> do
                assert (n <= Prelude.length (x:getList backBuf)) (return ())
                let src0 = Prelude.take n (x:getList backBuf)
                    -- XXX create the array in reverse instead
                    arr0 = A.fromListN n (Prelude.reverse src0)
                    arr1 = Array contents next end
                    -- XXX Use StreamK to avoid adding arbitrary layers of
                    -- constructors every time.
                    str = D.cons arr0 (D.cons arr1 (D.Stream step s))
                return (b, str)
            PR.Error err -> throwM $ ParseError err
-}

-- | Parse an array stream using the supplied 'Parser'.  Returns the parse
-- result and the unconsumed stream. Throws 'ParseError' if the parse fails.
--
-- The following alternative to this function allows composing the parser using
-- the parser Monad:
--
-- >>> parseBreakStreamK p = StreamK.parseBreakChunks (ParserK.adaptC p)
--
-- We can compare perf and remove this one or define it in terms of that.
--
-- /Internal/
--
{-# INLINE_NORMAL parseBreakChunksK #-}
parseBreakChunksK ::
       forall m a b. (MonadIO m, Unbox a)
    => Parser a m b
    -> StreamK m (Array a)
    -> m (Either ParseError b, StreamK m (Array a))
-- parseBreakStreamK p = StreamK.parseBreakChunks (ParserK.adaptC p)
parseBreakChunksK (Parser pstep initial extract) stream = do
    res <- initial
    case res of
        IPartial s -> go s stream []
        IDone b -> return (Right b, stream)
        IError err -> return (Left (ParseError err), stream)

    where

    -- "backBuf" contains last few items in the stream that we may have to
    -- backtrack to.
    --
    -- XXX currently we are using a dumb list based approach for backtracking
    -- buffer. This can be replaced by a sliding/ring buffer using Data.Array.
    -- That will allow us more efficient random back and forth movement.
    go !pst st backBuf = do
        let stop = goStop pst backBuf -- (, K.nil) <$> extract pst
            single a = yieldk a StreamK.nil
            yieldk arr r = goArray pst backBuf r arr
         in StreamK.foldStream defState yieldk single stop st

    -- Use strictness on "cur" to keep it unboxed
    goArray !pst backBuf st (Array _ cur end) | cur == end = go pst st backBuf
    goArray !pst backBuf st (Array contents cur end) = do
        x <- liftIO $ peekAt cur contents
        pRes <- pstep pst x
        let next = INDEX_NEXT(cur,a)
        case pRes of
            Parser.Partial 0 s ->
                 goArray s [] st (Array contents next end)
            Parser.Partial n s -> do
                assert (n <= Prelude.length (x:backBuf)) (return ())
                let src0 = Prelude.take n (x:backBuf)
                    arr0 = fromListN n (Prelude.reverse src0)
                    arr1 = Array contents next end
                    src = arr0 <> arr1
                goArray s [] st src
            Parser.Continue 0 s ->
                goArray s (x:backBuf) st (Array contents next end)
            Parser.Continue n s -> do
                assert (n <= Prelude.length (x:backBuf)) (return ())
                let (src0, buf1) = Prelude.splitAt n (x:backBuf)
                    arr0 = fromListN n (Prelude.reverse src0)
                    arr1 = Array contents next end
                    src = arr0 <> arr1
                goArray s buf1 st src
            Parser.Done 0 b -> do
                let arr = Array contents next end
                return (Right b, StreamK.cons arr st)
            Parser.Done n b -> do
                assert (n <= Prelude.length (x:backBuf)) (return ())
                let src0 = Prelude.take n (x:backBuf)
                    -- XXX Use fromListRevN once implemented
                    -- arr0 = A.fromListRevN n src0
                    arr0 = fromListN n (Prelude.reverse src0)
                    arr1 = Array contents next end
                    str = StreamK.cons arr0 (StreamK.cons arr1 st)
                return (Right b, str)
            Parser.Error err -> do
                let n = Prelude.length backBuf
                    arr0 = fromListN n (Prelude.reverse backBuf)
                    arr1 = Array contents cur end
                    str = StreamK.cons arr0 (StreamK.cons arr1 stream)
                return (Left (ParseError err), str)

    -- This is a simplified goArray
    goExtract !pst backBuf (Array _ cur end)
        | cur == end = goStop pst backBuf
    goExtract !pst backBuf (Array contents cur end) = do
        x <- liftIO $ peekAt cur contents
        pRes <- pstep pst x
        let next = INDEX_NEXT(cur,a)
        case pRes of
            Parser.Partial 0 s ->
                 goExtract s [] (Array contents next end)
            Parser.Partial n s -> do
                assert (n <= Prelude.length (x:backBuf)) (return ())
                let src0 = Prelude.take n (x:backBuf)
                    arr0 = fromListN n (Prelude.reverse src0)
                    arr1 = Array contents next end
                    src = arr0 <> arr1
                goExtract s [] src
            Parser.Continue 0 s ->
                goExtract s backBuf (Array contents next end)
            Parser.Continue n s -> do
                assert (n <= Prelude.length (x:backBuf)) (return ())
                let (src0, buf1) = Prelude.splitAt n (x:backBuf)
                    arr0 = fromListN n (Prelude.reverse src0)
                    arr1 = Array contents next end
                    src = arr0 <> arr1
                goExtract s buf1 src
            Parser.Done 0 b -> do
                let arr = Array contents next end
                return (Right b, StreamK.fromPure arr)
            Parser.Done n b -> do
                assert (n <= Prelude.length backBuf) (return ())
                let src0 = Prelude.take n backBuf
                    -- XXX Use fromListRevN once implemented
                    -- arr0 = A.fromListRevN n src0
                    arr0 = fromListN n (Prelude.reverse src0)
                    arr1 = Array contents next end
                    str = StreamK.cons arr0 (StreamK.fromPure arr1)
                return (Right b, str)
            Parser.Error err -> do
                let n = Prelude.length backBuf
                    arr0 = fromListN n (Prelude.reverse backBuf)
                    arr1 = Array contents cur end
                    str = StreamK.cons arr0 (StreamK.cons arr1 stream)
                return (Left (ParseError err), str)

    -- This is a simplified goExtract
    {-# INLINE goStop #-}
    goStop !pst backBuf = do
        pRes <- extract pst
        case pRes of
            Parser.Partial _ _ -> error "Bug: parseBreak: Partial in extract"
            Parser.Continue 0 s ->
                goStop s backBuf
            Parser.Continue n s -> do
                assert (n <= Prelude.length backBuf) (return ())
                let (src0, buf1) = Prelude.splitAt n backBuf
                    arr = fromListN n (Prelude.reverse src0)
                goExtract s buf1 arr
            Parser.Done 0 b ->
                return (Right b, StreamK.nil)
            Parser.Done n b -> do
                assert (n <= Prelude.length backBuf) (return ())
                let src0 = Prelude.take n backBuf
                    -- XXX Use fromListRevN once implemented
                    -- arr0 = A.fromListRevN n src0
                    arr0 = fromListN n (Prelude.reverse src0)
                return (Right b, StreamK.fromPure arr0)
            Parser.Error err -> do
                let n = Prelude.length backBuf
                    arr0 = fromListN n (Prelude.reverse backBuf)
                return (Left (ParseError err), StreamK.fromPure arr0)
