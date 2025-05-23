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
    -- , getIndicesFrom    -- read from a given position to the end of file
    -- , getIndicesUpto    -- read from beginning up to the given position
    -- , getIndicesFromTo
    -- , getIndicesFromRev  -- read from a given position to the beginning of file
    -- , getIndicesUptoRev  -- read from end to the given position in file
    , indexReader
    , indexReaderFromThenTo

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
    , unsafeCast
    , asCStringUnsafe -- XXX asCString
    , asCWString

    -- * Subarrays
    -- , sliceOffLen
    , indexerFromLen
    , splitterFromLen

    -- * Streaming Operations
    , streamTransform

    -- * Folding
    , streamFold
    , foldM
    , foldRev

    -- * Stream of Arrays
    , concatSepBy
    , concatEndBy
    , concatEndBySeq

    , compactMax
    , compactMax'
    , compactSepByByte_
    , compactEndByByte_
    , compactEndByLn_

    -- * Parsing Stream of Arrays
    , foldBreakChunks -- Uses Stream, bad perf on break
    , foldChunks
    , foldBreak
    , parseBreakChunksK -- XXX uses Parser. parseBreak is better?
    , parserK
    , parseBreak
    , parse

    -- * Serialization
    , encodeAs
    , serialize
    , serialize'
    , deserialize

    -- * Deprecated
    , sliceEndBy_
    , slicerFromLen
    , sliceIndexerFromLen
    , castUnsafe
    , getSliceUnsafe
    , pinnedSerialize
    , genSlicesFromLen
    , getSlicesFromLen
    , getIndices
    , writeLastN
    , interpose
    , interposeSuffix
    , intercalateSuffix
    , compactLE
    , pinnedCompactLE
    , compactOnByte
    , compactOnByteSuffix
    , splitOn
    , fold
    , foldBreakChunksK
    )
where

#include "assert.hs"
#include "deprecation.h"
#include "inline.hs"
#include "ArrayMacros.h"

import Control.Monad.IO.Class (MonadIO(..))
-- import Data.Bifunctor (first)
-- import Data.Either (fromRight)
import Data.Functor.Identity (Identity(..))
import Data.Proxy (Proxy(..))
import Data.Word (Word8)
import Foreign.C.String (CString, CWString)
import GHC.Types (SPEC(..))
import Streamly.Internal.Data.Unbox (Unbox(..))
import Prelude hiding (length, null, last, map, (!!), read, concat)

import Streamly.Internal.Data.MutByteArray.Type (PinnedState(..), MutByteArray)
import Streamly.Internal.Data.Serialize.Type (Serialize)
import Streamly.Internal.Data.Fold.Type (Fold(..))
import Streamly.Internal.Data.Parser (Parser(..), Initial(..), ParseError(..))
import Streamly.Internal.Data.ParserK.Type
    (ParserK, ParseResult(..), Input(..), Step(..))
import Streamly.Internal.Data.Stream (Stream(..))
import Streamly.Internal.Data.StreamK.Type (StreamK)
import Streamly.Internal.Data.SVar.Type (adaptState, defState)
import Streamly.Internal.Data.Tuple.Strict (Tuple'(..))
import Streamly.Internal.Data.Unfold.Type (Unfold(..))
import Streamly.Internal.System.IO (unsafeInlineIO)

import qualified Streamly.Internal.Data.Fold.Type as Fold
import qualified Streamly.Internal.Data.Serialize.Type as Serialize
import qualified Streamly.Internal.Data.MutByteArray.Type as MBA
import qualified Streamly.Internal.Data.MutArray as MA
import qualified Streamly.Internal.Data.RingArray as RB
import qualified Streamly.Internal.Data.Parser as Parser
import qualified Streamly.Internal.Data.Parser.Type as ParserD
import qualified Streamly.Internal.Data.ParserK.Type as ParserK
import qualified Streamly.Internal.Data.Stream as D
import qualified Streamly.Internal.Data.Stream as Stream
import qualified Streamly.Internal.Data.StreamK.Type as StreamK
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
-- Folds with Array as the container
-------------------------------------------------------------------------------

-- NOTE: We could possible write this in terms of "MutArray.createOfLast" but
-- this causes regression. This is probably because mapping inside "Fold.ifThen"
-- is more efficient than mapping over "Fold.ifTen".
--
-- | @createOfLast n@ folds a maximum of @n@ elements from the end of the input
-- stream to an 'Array'.
--
{-# INLINE createOfLast #-}
createOfLast ::
       (Unbox a, MonadIO m) => Int -> Fold m a (Array a)
createOfLast n = Fold.ifThen (pure (n <= 0)) (Fold.fromPure empty) lst

    where

    lst =
        let f = fmap unsafeFreeze . RB.toMutArray
         in Fold.rmapM f $ RB.createOfLast n

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

getSliceUnsafe ::
       forall a. Unbox a
    => Int -- ^ starting index
    -> Int -- ^ length of the slice
    -> Array a
    -> Array a
RENAME(getSliceUnsafe,unsafeSliceOffLen)

sliceEndBy_, splitOn :: (Monad m, Unbox a) =>
    (a -> Bool) -> Array a -> Stream m (Array a)
RENAME(splitOn,splitEndBy_)
RENAME(sliceEndBy_,splitEndBy_)

{-# INLINE indexerFromLen #-}
indexerFromLen, sliceIndexerFromLen :: forall m a. (Monad m, Unbox a)
    => Int -- ^ from index
    -> Int -- ^ length of the slice
    -> Unfold m (Array a) (Int, Int)
indexerFromLen from len =
    Unfold.lmap unsafeThaw (MA.indexerFromLen from len)
RENAME(sliceIndexerFromLen,indexerFromLen)

{-# DEPRECATED genSlicesFromLen "Please use indexerFromLen instead." #-}
{-# INLINE genSlicesFromLen #-}
genSlicesFromLen :: forall m a. (Monad m, Unbox a)
    => Int -- ^ from index
    -> Int -- ^ length of the slice
    -> Unfold m (Array a) (Int, Int)
genSlicesFromLen = indexerFromLen

-- | Generate a stream of slices of specified length from an array, starting
-- from the supplied array index. The last slice may be shorter than the
-- requested length.
--
-- /Pre-release//
{-# INLINE splitterFromLen #-}
splitterFromLen, slicerFromLen :: forall m a. (Monad m, Unbox a)
    => Int -- ^ from index
    -> Int -- ^ length of the slice
    -> Unfold m (Array a) (Array a)
splitterFromLen from len =
    fmap unsafeFreeze
        $ Unfold.lmap unsafeThaw (MA.splitterFromLen from len)
RENAME(slicerFromLen,splitterFromLen)

{-# DEPRECATED getSlicesFromLen "Please use splitterFromLen instead." #-}
{-# INLINE getSlicesFromLen #-}
getSlicesFromLen :: forall m a. (Monad m, Unbox a)
    => Int -- ^ from index
    -> Int -- ^ length of the slice
    -> Unfold m (Array a) (Array a)
getSlicesFromLen = splitterFromLen

-------------------------------------------------------------------------------
-- Random reads and writes
-------------------------------------------------------------------------------

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
unsafeCast, castUnsafe ::
#ifdef DEVBUILD
    Unbox b =>
#endif
    Array a -> Array b
unsafeCast (Array contents start end) =
    Array contents start end
RENAME(castUnsafe,unsafeCast)

-- | Cast an @Array a@ into an @Array Word8@.
--
--
asBytes :: Array a -> Array Word8
asBytes = unsafeCast

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
        else Just $ unsafeCast arr

-- | Convert an array of any element type into a null terminated CString Ptr.
-- The array is copied to pinned memory.
--
-- /Unsafe/
--
-- /O(n) Time: (creates a copy of the array)/
--
-- /Pre-release/
--
asCStringUnsafe :: Array a -> (CString -> IO b) -> IO b
asCStringUnsafe arr = MA.asCString (unsafeThaw arr)

-- | Convert an array of any element type into a null terminated CWString Ptr.
-- The array is copied to pinned memory.
--
-- /Unsafe/
--
-- /O(n) Time: (creates a copy of the array)/
--
-- /Pre-release/
--
asCWString :: Array a -> (CWString -> IO b) -> IO b
asCWString arr = MA.asCWString (unsafeThaw arr)

-------------------------------------------------------------------------------
-- Folds
-------------------------------------------------------------------------------

-- XXX Use runIdentity for pure fold
-- XXX Rename fold to foldM, we can then use "fold" for pure folds.
-- XXX We do not need an INLINE on fold?

-- | Fold an array using a 'Fold'.
--
-- /Pre-release/
{-# INLINE foldM #-}
fold, foldM :: (Monad m, Unbox a) => Fold m a b -> Array a -> m b
foldM f arr = Stream.fold f (read arr)
RENAME(fold,foldM)

foldRev :: Unbox a => Fold.Fold Identity a b -> Array a -> b
foldRev f arr = runIdentity $ Stream.fold f (readRev arr)

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
        mbarr <- MBA.newAs ps len
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
-- 1. Identity: @deserialize . serialize' == id@
-- 2. Encoded equivalence: @serialize' a == serialize' a@
{-# INLINE serialize' #-}
pinnedSerialize, serialize' :: Serialize a => a -> Array Word8
serialize' = encodeAs Pinned
RENAME_PRIME(pinnedSerialize,serialize)

-- XXX We can deserialize it like MutArray, returning the remaining slice.

-- | Decode a Haskell type from a byte array containing its serialized
-- representation.
{-# INLINE deserialize #-}
deserialize :: Serialize a => Array Word8 -> (a, Array Word8)
deserialize arr =
    let (a, b) = unsafeInlineIO $ MA.deserialize (unsafeThaw arr)
     in (a, unsafeFreeze b)

-------------------------------------------------------------------------------
-- Streams of Arrays
-------------------------------------------------------------------------------

-- TODO: efficiently compare two streams of arrays. Two streams can have chunks
-- of different sizes, we can handle that in the stream comparison abstraction.
-- This could be useful e.g. to fast compare whether two files differ.

-- | Insert the given element between arrays and flatten.
--
-- >>> concatSepBy x = Stream.unfoldEachSepBy x Array.reader
--
{-# INLINE concatSepBy #-}
concatSepBy, interpose :: (Monad m, Unbox a) =>
    a -> Stream m (Array a) -> Stream m a
concatSepBy x = D.unfoldEachSepBy x reader

RENAME(interpose,concatSepBy)

data FlattenState s =
      OuterLoop s
    | InnerLoop s !MutByteArray !Int !Int

-- | Insert the given element after each array and flatten. This is similar to
-- unlines.
--
-- >>> concatEndBy x = Stream.unfoldEachEndBy x Array.reader
--
{-# INLINE_NORMAL concatEndBy #-}
concatEndBy, interposeSuffix :: forall m a. (Monad m, Unbox a)
    => a -> Stream m (Array a) -> Stream m a
-- concatEndBy x = D.unfoldEachEndBy x reader
concatEndBy sep (D.Stream step state) = D.Stream step' (OuterLoop state)

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

RENAME(interposeSuffix,concatEndBy)

-- | Insert the given array after each array and flatten.
--
-- >>> concatEndBySeq x = Stream.unfoldEachEndBySeq x Array.reader
--
{-# INLINE concatEndBySeq #-}
concatEndBySeq, intercalateSuffix :: (Monad m, Unbox a)
    => Array a -> Stream m (Array a) -> Stream m a
concatEndBySeq x = D.unfoldEachEndBySeq x reader

RENAME(intercalateSuffix,concatEndBySeq)

-- | @compactMax n@ coalesces adjacent arrays in the input stream
-- only if the combined size would be less than or equal to n.
--
-- Generates unpinned arrays irrespective of the pinning status of input
-- arrays.
{-# INLINE_NORMAL compactMax #-}
compactMax, compactLE :: (MonadIO m, Unbox a)
    => Int -> Stream m (Array a) -> Stream m (Array a)
compactMax n stream =
    D.map unsafeFreeze $ MA.compactMax n $ D.map unsafeThaw stream

RENAME(compactLE,compactMax)

-- | Like 'compactMax' but generates pinned arrays.
{-# INLINE_NORMAL compactMax' #-}
compactMax', pinnedCompactLE :: (MonadIO m, Unbox a)
    => Int -> Stream m (Array a) -> Stream m (Array a)
compactMax' n stream =
    D.map unsafeFreeze $ MA.compactMax' n $ D.map unsafeThaw stream

{-# DEPRECATED pinnedCompactLE "Please use compactMax' instead." #-}
{-# INLINE pinnedCompactLE #-}
pinnedCompactLE = compactMax'

-- | Split a stream of byte arrays on a given separator byte, dropping the
-- separator and coalescing all the arrays between two separators into a single
-- array.
--
{-# INLINE compactSepByByte_ #-}
compactSepByByte_, compactOnByte
    :: (MonadIO m)
    => Word8
    -> Stream m (Array Word8)
    -> Stream m (Array Word8)
compactSepByByte_ byte =
    fmap unsafeFreeze . MA.compactSepByByte_ byte . fmap unsafeThaw

RENAME(compactOnByte,compactSepByByte_)

-- | Like 'compactSepByByte_', but considers the separator in suffix position
-- instead of infix position.
{-# INLINE compactEndByByte_ #-}
compactEndByByte_, compactOnByteSuffix
    :: (MonadIO m)
    => Word8
    -> Stream m (Array Word8)
    -> Stream m (Array Word8)
compactEndByByte_ byte =
    fmap unsafeFreeze . MA.compactEndByByte_ byte . fmap unsafeThaw
-- compactEndByByte_ byte = chunksEndBy_ (== byte) . concat

RENAME(compactOnByteSuffix,compactEndByByte_)

-- XXX On windows we should compact on "\r\n". We can just compact on '\n' and
-- drop the last byte in each array if it is '\r'.

-- | Compact byte arrays on newline character, dropping the newline char.
{-# INLINE compactEndByLn_ #-}
compactEndByLn_ :: MonadIO m
    => Stream m (Array Word8)
    -> Stream m (Array Word8)
compactEndByLn_ = compactEndByByte_ 10

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
-- >>> foldChunks f = Stream.fold f . Stream.unfoldEach Array.reader
--
foldChunks :: (MonadIO m, Unbox a) => Fold m a b -> Stream m (Array a) -> m b
foldChunks f s = fmap fst (foldBreakChunks f s)
-- foldStream f = Stream.fold f . Stream.unfoldEach reader

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
foldBreak, foldBreakChunksK :: forall m a b. (MonadIO m, Unbox a) =>
    Fold m a b -> StreamK m (Array a) -> m (b, StreamK m (Array a))
{-
foldBreakChunksK f s =
      fmap (first (fromRight undefined))
    $ StreamK.parseBreakChunks (ParserK.adaptC (Parser.fromFold f)) s
-}
foldBreak (Fold fstep initial _ final) stream = do
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

RENAME(foldBreakChunksK,foldBreak)

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
-- 'parseBreak' is an alternative to this function which allows composing the
-- parser using the parser Monad.
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
            Parser.SPartial 1 s ->
                 goArray s [] st (Array contents next end)
            Parser.SPartial m s -> do
                let n = 1 - m
                assert (n <= Prelude.length (x:backBuf)) (return ())
                let src0 = Prelude.take n (x:backBuf)
                    arr0 = fromListN n (Prelude.reverse src0)
                    arr1 = Array contents next end
                    src = arr0 <> arr1
                goArray s [] st src
            Parser.SContinue 1 s ->
                goArray s (x:backBuf) st (Array contents next end)
            Parser.SContinue m s -> do
                let n = 1 - m
                assert (n <= Prelude.length (x:backBuf)) (return ())
                let (src0, buf1) = Prelude.splitAt n (x:backBuf)
                    arr0 = fromListN n (Prelude.reverse src0)
                    arr1 = Array contents next end
                    src = arr0 <> arr1
                goArray s buf1 st src
            Parser.SDone 1 b -> do
                let arr = Array contents next end
                return (Right b, StreamK.cons arr st)
            Parser.SDone m b -> do
                let n = 1 - m
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
                    str = StreamK.cons arr0 (StreamK.cons arr1 st)
                return (Left (ParseError err), str)

    -- This is a simplified goArray
    goExtract !pst backBuf (Array _ cur end)
        | cur == end = goStop pst backBuf
    goExtract !pst backBuf (Array contents cur end) = do
        x <- liftIO $ peekAt cur contents
        pRes <- pstep pst x
        let next = INDEX_NEXT(cur,a)
        case pRes of
            Parser.SPartial 1 s ->
                 goExtract s [] (Array contents next end)
            Parser.SPartial m s -> do
                let n = 1 - m
                assert (n <= Prelude.length (x:backBuf)) (return ())
                let src0 = Prelude.take n (x:backBuf)
                    arr0 = fromListN n (Prelude.reverse src0)
                    arr1 = Array contents next end
                    src = arr0 <> arr1
                goExtract s [] src
            Parser.SContinue 1 s ->
                goExtract s backBuf (Array contents next end)
            Parser.SContinue m s -> do
                let n = 1 - m
                assert (n <= Prelude.length (x:backBuf)) (return ())
                let (src0, buf1) = Prelude.splitAt n (x:backBuf)
                    arr0 = fromListN n (Prelude.reverse src0)
                    arr1 = Array contents next end
                    src = arr0 <> arr1
                goExtract s buf1 src
            Parser.SDone 1 b -> do
                let arr = Array contents next end
                return (Right b, StreamK.fromPure arr)
            Parser.SDone m b -> do
                let n = 1 - m
                assert (n <= Prelude.length backBuf) (return ())
                let src0 = Prelude.take n (x:backBuf)
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
                    str = StreamK.cons arr0 (StreamK.fromPure arr1)
                return (Left (ParseError err), str)

    -- This is a simplified goExtract
    {-# INLINE goStop #-}
    goStop !pst backBuf = do
        pRes <- extract pst
        case pRes of
            Parser.SPartial _ _ -> error "Bug: parseBreak: Partial in extract"
            Parser.SContinue 1 s ->
                goStop s backBuf
            Parser.SContinue m s -> do
                let n = 1 - m
                assert (n <= Prelude.length backBuf) (return ())
                let (src0, buf1) = Prelude.splitAt n backBuf
                    arr = fromListN n (Prelude.reverse src0)
                goExtract s buf1 arr
            Parser.SDone 1 b ->
                return (Right b, StreamK.nil)
            Parser.SDone m b -> do
                let n = 1 - m
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

-- The backracking buffer consists of arrays in the most-recent-first order. We
-- want to take a total of n array elements from this buffer. Note: when we
-- have to take an array partially, we must take the last part of the array.
{-# INLINE backTrack #-}
backTrack :: forall m a. Unbox a =>
       Int
    -> [Array a]
    -> StreamK m (Array a)
    -> (StreamK m (Array a), [Array a])
backTrack = go

    where

    go _ [] stream = (stream, [])
    go n xs stream | n <= 0 = (stream, xs)
    go n (x:xs) stream =
        let len = length x
        in if n > len
           then go (n - len) xs (StreamK.cons x stream)
           else if n == len
           then (StreamK.cons x stream, xs)
           else let !(Array contents start end) = x
                    !start1 = end - (n * SIZE_OF(a))
                    arr1 = Array contents start1 end
                    arr2 = Array contents start start1
                 in (StreamK.cons arr1 stream, arr2:xs)

-- | Run a 'ParserK' over a 'StreamK' of Arrays and return the parse result and
-- the remaining Stream.
{-# INLINE_NORMAL parseBreak #-}
parseBreak
    :: (Monad m, Unbox a)
    => ParserK (Array a) m b
    -> StreamK m (Array a)
    -> m (Either ParseError b, StreamK m (Array a))
parseBreak parser input = do
    let parserk = ParserK.runParser parser ParserK.parserDone 0 0
     in go [] parserk input

    where

    {-# INLINE goStop #-}
    goStop backBuf parserk = do
        pRes <- parserk ParserK.None
        case pRes of
            -- If we stop in an alternative, it will try calling the next
            -- parser, the next parser may call initial returning Partial and
            -- then immediately we have to call extract on it.
            ParserK.Partial 0 cont1 ->
                 go [] cont1 StreamK.nil
            ParserK.Partial n cont1 -> do
                let n1 = negate n
                assertM(n1 >= 0 && n1 <= sum (Prelude.map length backBuf))
                let (s1, backBuf1) = backTrack n1 backBuf StreamK.nil
                 in go backBuf1 cont1 s1
            ParserK.Continue 0 cont1 ->
                go backBuf cont1 StreamK.nil
            ParserK.Continue n cont1 -> do
                let n1 = negate n
                assertM(n1 >= 0 && n1 <= sum (Prelude.map length backBuf))
                let (s1, backBuf1) = backTrack n1 backBuf StreamK.nil
                 in go backBuf1 cont1 s1
            ParserK.Done 0 b ->
                return (Right b, StreamK.nil)
            ParserK.Done n b -> do
                let n1 = negate n
                assertM(n1 >= 0 && n1 <= sum (Prelude.map length backBuf))
                let (s1, _) = backTrack n1 backBuf StreamK.nil
                 in return (Right b, s1)
            ParserK.Error _ err -> do
                let (s1, _) = backTrack maxBound backBuf StreamK.nil
                return (Left (ParseError err), s1)

    seekErr n len =
        error $ "parseBreak: Partial: forward seek not implemented n = "
            ++ show n ++ " len = " ++ show len

    yieldk backBuf parserk arr stream = do
        pRes <- parserk (ParserK.Chunk arr)
        let len = length arr
        case pRes of
            ParserK.Partial n cont1 ->
                case compare n len of
                    EQ -> go [] cont1 stream
                    LT -> do
                        if n >= 0
                        then yieldk [] cont1 arr stream
                        else do
                            let n1 = negate n
                                bufLen = sum (Prelude.map length backBuf)
                                s = StreamK.cons arr stream
                            assertM(n1 >= 0 && n1 <= bufLen)
                            let (s1, _) = backTrack n1 backBuf s
                            go [] cont1 s1
                    GT -> seekErr n len
            ParserK.Continue n cont1 ->
                case compare n len of
                    EQ -> go (arr:backBuf) cont1 stream
                    LT -> do
                        if n >= 0
                        then yieldk backBuf cont1 arr stream
                        else do
                            let n1 = negate n
                                bufLen = sum (Prelude.map length backBuf)
                                s = StreamK.cons arr stream
                            assertM(n1 >= 0 && n1 <= bufLen)
                            let (s1, backBuf1) = backTrack n1 backBuf s
                            go backBuf1 cont1 s1
                    GT -> seekErr n len
            ParserK.Done n b -> do
                let n1 = len - n
                assertM(n1 <= sum (Prelude.map length (arr:backBuf)))
                let (s1, _) = backTrack n1 (arr:backBuf) stream
                 in return (Right b, s1)
            ParserK.Error _ err -> do
                let (s1, _) = backTrack maxBound (arr:backBuf) stream
                return (Left (ParseError err), s1)

    go backBuf parserk stream = do
        let stop = goStop backBuf parserk
            single a = yieldk backBuf parserk a StreamK.nil
         in StreamK.foldStream
                defState (yieldk backBuf parserk) single stop stream

{-# INLINE parse #-}
parse :: (Monad m, Unbox a) =>
    ParserK (Array a) m b -> StreamK m (Array a) -> m (Either ParseError b)
parse f = fmap fst . parseBreak f

-------------------------------------------------------------------------------
-- Convert ParserD to ParserK
-------------------------------------------------------------------------------

{-# INLINE adaptCWith #-}
adaptCWith
    :: forall m a s b r. (Monad m, Unbox a)
    => (s -> a -> m (ParserD.Step s b))
    -> m (ParserD.Initial s b)
    -> (s -> m (ParserD.Step s b))
    -> (ParseResult b -> Int -> Input (Array a) -> m (Step (Array a) m r))
    -> Int
    -> Int
    -> Input (Array a)
    -> m (Step (Array a) m r)
adaptCWith pstep initial extract cont !offset0 !usedCount !input = do
    res <- initial
    case res of
        ParserD.IPartial pst -> do
            case input of
                Chunk arr -> parseContChunk usedCount offset0 pst arr
                None -> parseContNothing usedCount pst
        ParserD.IDone b -> cont (Success offset0 b) usedCount input
        ParserD.IError err -> cont (Failure offset0 err) usedCount input

    where

    -- XXX We can maintain an absolute position instead of relative that will
    -- help in reporting of error location in the stream.
    {-# NOINLINE parseContChunk #-}
    parseContChunk !count !offset !state arr@(Array contents start end) = do
         if offset >= 0
         then go SPEC (start + offset * SIZE_OF(a)) state
         else return $ Continue offset (parseCont count state)

        where

        {-# INLINE onDone #-}
        onDone n b =
            assert (n <= length arr)
                (cont (Success n b) (count + n - offset) (Chunk arr))

        {-# INLINE callParseCont #-}
        callParseCont constr n pst1 =
            assert (n < 0 || n >= length arr)
                (return $ constr n (parseCont (count + n - offset) pst1))

        {-# INLINE onPartial #-}
        onPartial = callParseCont Partial

        {-# INLINE onContinue #-}
        onContinue = callParseCont Continue

        {-# INLINE onError #-}
        onError n err =
            cont (Failure n err) (count + n - offset) (Chunk arr)

        {-# INLINE onBack #-}
        onBack offset1 elemSize constr pst = do
            let pos = offset1 - start
             in if pos >= 0
                then go SPEC offset1 pst
                else constr (pos `div` elemSize) pst

        -- Note: div may be expensive but the alternative is to maintain an element
        -- offset in addition to a byte offset or just the element offset and use
        -- multiplication to get the byte offset every time, both these options
        -- turned out to be more expensive than using div.
        go !_ !cur !pst | cur >= end =
            onContinue ((end - start) `div` SIZE_OF(a))  pst
        go !_ !cur !pst = do
            let !x = unsafeInlineIO $ peekAt cur contents
            pRes <- pstep pst x
            let elemSize = SIZE_OF(a)
                next = INDEX_NEXT(cur,a)
                back n = next - n * elemSize
                curOff = (cur - start) `div` elemSize
                nextOff = (next - start) `div` elemSize
            -- The "n" here is stream position index wrt the array start, and
            -- not the backtrack count as returned by byte stream parsers.
            case pRes of
                ParserD.SDone 1 b ->
                    onDone nextOff b
                ParserD.SDone 0 b ->
                    onDone curOff b
                ParserD.SDone m b ->
                    let n = 1 - m
                     in onDone ((back n - start) `div` elemSize) b
                ParserD.SPartial 1 pst1 ->
                    go SPEC next pst1
                ParserD.SPartial 0 pst1 ->
                    go SPEC cur pst1
                ParserD.SPartial m pst1 ->
                    let n = 1 - m
                     in onBack (back n) elemSize onPartial pst1
                ParserD.SContinue 1 pst1 ->
                    go SPEC next pst1
                ParserD.SContinue 0 pst1 ->
                    go SPEC cur pst1
                ParserD.SContinue m pst1 ->
                    let n = 1 - m
                     in onBack (back n) elemSize onContinue pst1
                ParserD.Error err ->
                    onError curOff err

    {-# NOINLINE parseContNothing #-}
    parseContNothing !count !pst = do
        r <- extract pst
        case r of
            -- IMPORTANT: the n here is from the byte stream parser, that means
            -- it is the backtrack element count and not the stream position
            -- index into the current input array.
            ParserD.SDone m b ->
                let n = 1 - m
                 in assert (n >= 0)
                        (cont (Success (- n) b) (count - n) None)
            ParserD.SContinue m pst1 ->
                let n = 1 - m
                 in assert (n >= 0)
                        (return $ Continue (- n) (parseCont (count - n) pst1))
            ParserD.Error err ->
                -- XXX It is called only when there is no input arr. So using 0
                -- as the position is correct?
                cont (Failure 0 err) count None
            ParserD.SPartial _ _ -> error "Bug: adaptCWith Partial unreachable"

    -- XXX Maybe we can use two separate continuations instead of using
    -- Just/Nothing cases here. That may help in avoiding the parseContJust
    -- function call.
    {-# INLINE parseCont #-}
    parseCont !cnt !pst (Chunk arr) = parseContChunk cnt 0 pst arr
    parseCont !cnt !pst None = parseContNothing cnt pst

-- | Convert a 'Parser' to 'ParserK' working on an Array stream.
--
-- /Pre-release/
--
{-# INLINE_LATE parserK #-}
parserK :: (Monad m, Unbox a) => ParserD.Parser a m b -> ParserK (Array a) m b
parserK (ParserD.Parser step initial extract) =
    ParserK.MkParser $ adaptCWith step initial extract
