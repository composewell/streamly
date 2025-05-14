-- |
-- Module      : Streamly.Internal.Data.Array.Generic
-- Copyright   : (c) 2019 Composewell Technologies
--
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Portability : GHC
--
module Streamly.Internal.Data.Array.Generic
    ( Array(..)

    -- * Construction
    , nil
    , createOf
    , create
    , createWith
    , createOfLast

    , fromStreamN
    , fromStream
    , fromPureStream
    , fromCString#

    , fromListN
    , fromList

    , chunksOf

    -- * Elimination
    , length
    , reader

    , toList
    , read
    , readRev

    , foldl'
    , foldr
    , streamFold
    , fold

    -- * Random Access
    , unsafeGetIndex
    , getIndex
    , unsafeSliceOffLen
    , dropAround

    -- * Parsing Stream of Arrays
    , parserK
    , parse
    , parseBreak

    -- * Deprecated
    , strip
    , getIndexUnsafe
    , getSliceUnsafe
    , unsafeGetSlice
    , writeN
    , write
    , fromByteStr#
    )
where

#include "inline.hs"
#include "assert.hs"
#include "deprecation.h"

import Control.Monad (replicateM)
import Control.Monad.IO.Class (MonadIO)
import Data.Foldable (sum)
import Data.Functor.Identity (Identity(..))
import Data.Word (Word8)
import GHC.Base (MutableArray#, RealWorld)
import GHC.Exts (Addr#)
import GHC.Types (SPEC(..))
import GHC.IO (unsafePerformIO)
import Text.Read (readPrec)

import Streamly.Internal.Data.Fold.Type (Fold(..))
import Streamly.Internal.Data.Parser (ParseError(..))
import Streamly.Internal.Data.ParserK.Type
    (ParserK, ParseResult(..), Input(..), Step(..))
import Streamly.Internal.Data.Stream.Type (Stream)
import Streamly.Internal.Data.StreamK.Type (StreamK)
import Streamly.Internal.Data.SVar.Type (defState)
import Streamly.Internal.Data.Unfold.Type (Unfold(..))
import Streamly.Internal.System.IO (unsafeInlineIO)

import qualified Streamly.Internal.Data.Fold.Type as FL
import qualified Streamly.Internal.Data.MutArray.Generic as MArray
import qualified Streamly.Internal.Data.Parser.Type as ParserD
import qualified Streamly.Internal.Data.ParserK.Type as ParserK
import qualified Streamly.Internal.Data.Producer.Type as Producer
import qualified Streamly.Internal.Data.Producer as Producer
import qualified Streamly.Internal.Data.RingArray.Generic as RB
import qualified Streamly.Internal.Data.Stream.Type as D
import qualified Streamly.Internal.Data.Stream.Generate as D
import qualified Streamly.Internal.Data.StreamK.Type as StreamK
import qualified Text.ParserCombinators.ReadPrec as ReadPrec

import Prelude hiding (Foldable(..), read)

-------------------------------------------------------------------------------
-- Array Data Type
-------------------------------------------------------------------------------

data Array a =
    Array
        { arrContents# :: MutableArray# RealWorld a
          -- ^ The internal contents of the array representing the entire array.

        , arrStart :: {-# UNPACK #-}!Int
          -- ^ The starting index of this slice.

        , arrEnd :: {-# UNPACK #-}!Int
          -- ^ First invalid index of the array.
        }

unsafeFreeze :: MArray.MutArray a -> Array a
unsafeFreeze (MArray.MutArray cont# arrS arrE _) = Array cont# arrS arrE

unsafeThaw :: Array a -> MArray.MutArray a
unsafeThaw (Array cont# arrS arrE) = MArray.MutArray cont# arrS arrE arrE

{-# NOINLINE nil #-}
nil :: Array a
nil = unsafePerformIO $ unsafeFreeze <$> MArray.nil

-------------------------------------------------------------------------------
-- Construction - Folds
-------------------------------------------------------------------------------

{-# INLINE_NORMAL createOf #-}
createOf :: MonadIO m => Int -> Fold m a (Array a)
createOf = fmap unsafeFreeze <$> MArray.createOf

{-# DEPRECATED writeN "Please use createOf instead." #-}
{-# INLINE writeN #-}
writeN :: MonadIO m => Int -> Fold m a (Array a)
writeN = createOf

{-# INLINE_NORMAL createWith #-}
createWith :: MonadIO m => Int -> Fold m a (Array a)
createWith elemCount = unsafeFreeze <$> MArray.createWith elemCount

-- | Fold the whole input to a single array.
--
-- /Caution! Do not use this on infinite streams./
--
{-# INLINE create #-}
create :: MonadIO m => Fold m a (Array a)
create = fmap unsafeFreeze MArray.create

{-# DEPRECATED write "Please use create instead." #-}
{-# INLINE write #-}
write :: MonadIO m => Fold m a (Array a)
write = create

fromPureStream :: Stream Identity a -> Array a
fromPureStream x =
    unsafePerformIO $ fmap unsafeFreeze (MArray.fromPureStream x)
-- fromPureStream = runIdentity . D.fold (unsafeMakePure write)
-- fromPureStream = fromList . runIdentity . D.toList

fromCString# :: MonadIO m => Addr# -> m (Array Word8)
fromCString# addr = fromStream $ D.fromCString# addr

{-# DEPRECATED fromByteStr# "Please use 'unsafePerformIO . fromCString#' instead" #-}
fromByteStr# :: Addr# -> Array Word8
fromByteStr# addr = fromPureStream (D.fromCString# addr)

-------------------------------------------------------------------------------
-- Stream Ops
-------------------------------------------------------------------------------

{-# INLINE_NORMAL chunksOf #-}
chunksOf :: forall m a. MonadIO m
    => Int -> Stream m a -> Stream m (Array a)
chunksOf n strm = fmap unsafeFreeze $ MArray.chunksOf n strm

-------------------------------------------------------------------------------
-- Construction - from streams
-------------------------------------------------------------------------------

{-# INLINE fromStreamN #-}
fromStreamN :: MonadIO m => Int -> Stream m a -> m (Array a)
fromStreamN n = D.fold (writeN n)

{-# INLINE fromStream #-}
fromStream :: MonadIO m => Stream m a -> m (Array a)
fromStream = D.fold write

-- XXX Consider foldr/build fusion in toList/fromList

{-# INLINABLE fromListN #-}
fromListN :: Int -> [a] -> Array a
fromListN n xs = unsafePerformIO $ fromStreamN n $ D.fromList xs

{-# INLINABLE fromList #-}
fromList :: [a] -> Array a
fromList xs = unsafePerformIO $ fromStream $ D.fromList xs

-------------------------------------------------------------------------------
-- Elimination - Unfolds
-------------------------------------------------------------------------------

{-# INLINE length #-}
length :: Array a -> Int
length arr = arrEnd arr - arrStart arr

{-# INLINE_NORMAL reader #-}
reader :: Monad m => Unfold m (Array a) a
reader =
    Producer.simplify
        $ Producer.translate unsafeThaw unsafeFreeze
        $ MArray.producerWith (return . unsafeInlineIO)

-------------------------------------------------------------------------------
-- Elimination - to streams
-------------------------------------------------------------------------------

{-# INLINE_NORMAL toList #-}
toList :: Array a -> [a]
toList arr = loop 0

    where

    len = length arr
    loop i | i == len = []
    loop i = unsafeGetIndex i arr : loop (i + 1)

{-# INLINE_NORMAL read #-}
read :: Monad m => Array a -> Stream m a
read arr =
    D.map (`unsafeGetIndex` arr) $ D.enumerateFromToIntegral 0 (length arr - 1)

{-# INLINE_NORMAL readRev #-}
readRev :: Monad m => Array a -> Stream m a
readRev arr =
    D.map (`unsafeGetIndex` arr)
        $ D.enumerateFromThenToIntegral (arrLen - 1) (arrLen - 2) 0
    where
    arrLen = length arr

-------------------------------------------------------------------------------
-- Elimination - using Folds
-------------------------------------------------------------------------------

{-# INLINE_NORMAL foldl' #-}
foldl' :: (b -> a -> b) -> b -> Array a -> b
foldl' f z arr = unsafePerformIO $ D.foldl' f z $ read arr

{-# INLINE_NORMAL foldr #-}
foldr :: (a -> b -> b) -> b -> Array a -> b
foldr f z arr = unsafePerformIO $ D.foldr f z $ read arr

{-# INLINE fold #-}
fold :: Monad m => Fold m a b -> Array a -> m b
fold f arr = D.fold f (read arr)

{-# INLINE streamFold #-}
streamFold :: Monad m => (Stream m a -> m b) -> Array a -> m b
streamFold f arr = f (read arr)

-------------------------------------------------------------------------------
-- Random reads and writes
-------------------------------------------------------------------------------

-- | /O(1)/ Lookup the element at the given index. Index starts from 0. Does
-- not check the bounds.
--
-- @since 0.8.0
{-# INLINE unsafeGetIndex #-}
unsafeGetIndex, getIndexUnsafe :: Int -> Array a -> a
unsafeGetIndex i arr =
    unsafePerformIO $ MArray.unsafeGetIndex i (unsafeThaw arr)

-- | Lookup the element at the given index. Index starts from 0.
--
{-# INLINE getIndex #-}
getIndex :: Int -> Array a -> Maybe a
getIndex i arr =
    if i >= 0 && i < length arr
    then Just $ unsafeGetIndex i arr
    else Nothing

-- >>> import qualified Streamly.Data.Stream as Stream
-- >>> import qualified Streamly.Data.Fold as Fold
-- >>> import qualified Streamly.Internal.Data.Array.Generic as Array
-- >>> import Data.Function ((&))
-- >>> :{
--  Stream.fromList [1,2,3,4,5::Int]
--      & Stream.scan (Array.createOfLast 2)
--      & Stream.fold Fold.toList
--  :}
-- [fromList [],fromList [1],fromList [1,2],fromList [2,3],fromList [3,4],fromList [4,5]]
--
{-# INLINE createOfLast #-}
createOfLast :: MonadIO m => Int -> Fold m a (Array a)
createOfLast n = FL.rmapM f (RB.createOf n)

    where

    f rb = do
        arr <- RB.copyToMutArray 0 n rb
        return $ unsafeFreeze arr

{-# INLINE unsafeSliceOffLen #-}
unsafeSliceOffLen, getSliceUnsafe, unsafeGetSlice
    :: Int -> Int -> Array a -> Array a
unsafeSliceOffLen offset len =
    unsafeFreeze . MArray.unsafeSliceOffLen offset len . unsafeThaw

-- XXX This is not efficient as it copies the array. We should support array
-- slicing so that we can just refer to the underlying array memory instead of
-- copying.

-- | Truncate the array at the beginning and end as long as the predicate
-- holds true. Returns a slice of the original array.
{-# INLINE dropAround #-}
dropAround, strip :: (a -> Bool) -> Array a -> Array a
dropAround p arr =
    unsafeFreeze $ unsafePerformIO $ MArray.dropAround p (unsafeThaw arr)

-------------------------------------------------------------------------------
-- Instances
-------------------------------------------------------------------------------

instance Eq a => Eq (Array a) where
    {-# INLINE (==) #-}
    arr1 == arr2 =
        unsafeInlineIO $! unsafeThaw arr1 `MArray.eq` unsafeThaw arr2

instance Ord a => Ord (Array a) where
    {-# INLINE compare #-}
    compare arr1 arr2 =
        unsafeInlineIO $! unsafeThaw arr1 `MArray.cmp` unsafeThaw arr2

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

instance Show a => Show (Array a) where
    {-# INLINE show #-}
    show arr = "fromList " ++ show (toList arr)

instance Read a => Read (Array a) where
    {-# INLINE readPrec #-}
    readPrec = do
        fromListWord <- replicateM 9 ReadPrec.get
        if fromListWord == "fromList "
        then fromList <$> readPrec
        else ReadPrec.pfail

-------------------------------------------------------------------------------
-- Backward Compatibility
-------------------------------------------------------------------------------

RENAME(strip,dropAround)
RENAME(getSliceUnsafe,unsafeSliceOffLen)
RENAME(unsafeGetSlice,unsafeSliceOffLen)
RENAME(getIndexUnsafe,unsafeGetIndex)

-------------------------------------------------------------------------------
-- ParserK Chunked Generic
-------------------------------------------------------------------------------

{-# INLINE backTrackGenericChunks #-}
backTrackGenericChunks ::
       Int
    -> [Array a]
    -> StreamK m (Array a)
    -> (StreamK m (Array a), [Array a])
backTrackGenericChunks = go

    where

    go _ [] stream = (stream, [])
    go n xs stream | n <= 0 = (stream, xs)
    go n (x:xs) stream =
        let len = length x
        in if n > len
           then go (n - len) xs (StreamK.cons x stream)
           else if n == len
           then (StreamK.cons x stream, xs)
           else let arr1 = unsafeSliceOffLen (len - n) n x
                    arr2 = unsafeSliceOffLen 0 (len - n) x
                 in (StreamK.cons arr1 stream, arr2:xs)

{-# INLINE_NORMAL parseBreak #-}
parseBreak
    :: forall m a b. Monad m
    => ParserK.ParserK (Array a) m b
    -> StreamK m (Array a)
    -> m (Either ParseError b, StreamK m (Array a))
parseBreak parser input = do
    let parserk = ParserK.runParser parser ParserK.parserDone 0 0
     in go [] parserk input

    where

    {-# INLINE goStop #-}
    goStop
        :: [Array a]
        -> (ParserK.Input (Array a)
                -> m (ParserK.Step (Array a) m b))
        -> m (Either ParseError b, StreamK m (Array a))
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
                let (s1, backBuf1) = backTrackGenericChunks n1 backBuf StreamK.nil
                 in go backBuf1 cont1 s1
            ParserK.Continue 0 cont1 ->
                go backBuf cont1 StreamK.nil
            ParserK.Continue n cont1 -> do
                let n1 = negate n
                assertM(n1 >= 0 && n1 <= sum (Prelude.map length backBuf))
                let (s1, backBuf1) = backTrackGenericChunks n1 backBuf StreamK.nil
                 in go backBuf1 cont1 s1
            ParserK.Done 0 b ->
                return (Right b, StreamK.nil)
            ParserK.Done n b -> do
                let n1 = negate n
                assertM(n1 >= 0 && n1 <= sum (Prelude.map length backBuf))
                let (s1, _) = backTrackGenericChunks n1 backBuf StreamK.nil
                 in return (Right b, s1)
            ParserK.Error _ err ->
                let strm = StreamK.fromList (Prelude.reverse backBuf)
                 in return (Left (ParseError err), strm)

    seekErr n len =
        error $ "parseBreak: Partial: forward seek not implemented n = "
            ++ show n ++ " len = " ++ show len

    yieldk
        :: [Array a]
        -> (ParserK.Input (Array a)
                -> m (ParserK.Step (Array a) m b))
        -> Array a
        -> StreamK m (Array a)
        -> m (Either ParseError b, StreamK m (Array a))
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
                            let (s1, _) = backTrackGenericChunks n1 backBuf s
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
                            let (s1, backBuf1) = backTrackGenericChunks n1 backBuf s
                            go backBuf1 cont1 s1
                    GT -> seekErr n len
            ParserK.Done n b -> do
                let n1 = len - n
                assertM(n1 <= sum (Prelude.map length (arr:backBuf)))
                let (s1, _) = backTrackGenericChunks n1 (arr:backBuf) stream
                 in return (Right b, s1)
            ParserK.Error _ err ->
                let strm =
                        StreamK.append
                            (StreamK.fromList (Prelude.reverse backBuf))
                            (StreamK.cons arr stream)
                 in return (Left (ParseError err), strm)

    go
        :: [Array a]
        -> (ParserK.Input (Array a)
                -> m (ParserK.Step (Array a) m b))
        -> StreamK m (Array a)
        -> m (Either ParseError b, StreamK m (Array a))
    go backBuf parserk stream = do
        let stop = goStop backBuf parserk
            single a = yieldk backBuf parserk a StreamK.nil
         in StreamK.foldStream
                defState (yieldk backBuf parserk) single stop stream

{-# INLINE parse #-}
parse ::
       (Monad m)
    => ParserK.ParserK (Array a) m b
    -> StreamK m (Array a)
    -> m (Either ParseError b)
parse f = fmap fst . parseBreak f

--------------------------------------------------------------------------------
-- Convert Parser to Parserk on Generic Arrays
--------------------------------------------------------------------------------

{-# INLINE adaptCGWith #-}
adaptCGWith
    :: forall m a s b r. (Monad m)
    => (s -> a -> m (ParserD.Step s b))
    -> m (ParserD.Initial s b)
    -> (s -> m (ParserD.Step s b))
    -> (ParseResult b -> Int -> Input (Array a) -> m (Step (Array a) m r))
    -> Int
    -> Int
    -> Input (Array a)
    -> m (Step (Array a) m r)
adaptCGWith pstep initial extract cont !offset0 !usedCount !input = do
    res <- initial
    case res of
        ParserD.IPartial pst -> do
            case input of
                Chunk arr -> parseContChunk usedCount offset0 pst arr
                None -> parseContNothing usedCount pst
        ParserD.IDone b -> cont (Success offset0 b) usedCount input
        ParserD.IError err -> cont (Failure offset0 err) usedCount input

    where

    {-# NOINLINE parseContChunk #-}
    parseContChunk !count !offset !state arr@(Array contents start end) = do
         if offset >= 0
         then go SPEC (start + offset) state
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
        onBack offset1 constr pst = do
            let pos = offset1 - start
             in if pos >= 0
                then go SPEC offset1 pst
                else constr pos pst

        go !_ !cur !pst | cur >= end =
            onContinue (end - start)  pst
        go !_ !cur !pst = do
            let !x = unsafeInlineIO $ MArray.unsafeGetIndexWith contents cur
            pRes <- pstep pst x
            let next = cur + 1
                back n = next - n
                curOff = cur - start
                nextOff = next - start
            -- The "n" here is stream position index wrt the array start, and
            -- not the backtrack count as returned by byte stream parsers.
            case pRes of
                ParserD.Done 0 b ->
                    onDone nextOff b
                ParserD.Done 1 b ->
                    onDone curOff b
                ParserD.Done n b ->
                    onDone (back n - start) b
                ParserD.Partial 0 pst1 ->
                    go SPEC next pst1
                ParserD.Partial 1 pst1 ->
                    go SPEC cur pst1
                ParserD.Partial n pst1 ->
                    onBack (back n) onPartial pst1
                ParserD.Continue 0 pst1 ->
                    go SPEC next pst1
                ParserD.Continue 1 pst1 ->
                    go SPEC cur pst1
                ParserD.Continue n pst1 ->
                    onBack (back n) onContinue pst1
                ParserD.Error err ->
                    onError curOff err

    {-# NOINLINE parseContNothing #-}
    parseContNothing !count !pst = do
        r <- extract pst
        case r of
            -- IMPORTANT: the n here is from the byte stream parser, that means
            -- it is the backtrack element count and not the stream position
            -- index into the current input array.
            ParserD.Done n b ->
                assert (n >= 0)
                    (cont (Success (- n) b) (count - n) None)
            ParserD.Continue n pst1 ->
                assert (n >= 0)
                    (return $ Continue (- n) (parseCont (count - n) pst1))
            ParserD.Error err ->
                -- XXX It is called only when there is no input arr. So using 0
                -- as the position is correct?
                cont (Failure 0 err) count None
            ParserD.Partial _ _ -> error "Bug: adaptCGWith Partial unreachable"

    {-# INLINE parseCont #-}
    parseCont !cnt !pst (Chunk arr) = parseContChunk cnt 0 pst arr
    parseCont !cnt !pst None = parseContNothing cnt pst

-- | Convert a 'Parser' to 'ParserK' working on generic Array stream.
--
-- /Pre-release/
--
{-# INLINE_LATE parserK #-}
parserK :: Monad m => ParserD.Parser a m b -> ParserK (Array a) m b
parserK (ParserD.Parser step initial extract) =
    ParserK.MkParser $ adaptCGWith step initial extract
