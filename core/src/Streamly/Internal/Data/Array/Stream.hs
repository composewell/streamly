{-# OPTIONS_GHC -Wno-deprecations #-}
-- |
-- Module      : Streamly.Internal.Data.Array.Stream
-- Copyright   : (c) 2019 Composewell Technologies
-- License     : BSD3-3-Clause
-- Maintainer  : streamly@composewell.com
-- Portability : GHC
--
-- Combinators to efficiently manipulate streams of immutable arrays.
--
-- We can either push these in the MutArray module with a "chunks" prefix or
-- keep this as a separate module and release it.
--
module Streamly.Internal.Data.Array.Stream
    (
    -- * Creation
      Array.chunksOf
    , Array.pinnedChunksOf
    , Array.bufferChunks

    -- * Flattening to elements
    , concat
    , Array.flattenArrays
    , concatRev
    , Array.flattenArraysRev
    , interpose
    , interposeSuffix
    , intercalateSuffix
    , unlines

    -- * Elimination
    -- ** Element Folds
    -- The byte level foldBreak can work as efficiently as the chunk level. We
    -- can flatten the stream to byte stream and use that. But if we want the
    -- remaining stream to be a chunk stream then this could be handy. But it
    -- could also be implemented using parseBreak.
    , foldBreak
    , foldBreakD
    -- The byte level parseBreak cannot work efficiently. Because the stream
    -- will have to be a StreamK for backtracking, StreamK at byte level would
    -- not be efficient.
    , parseBreak -- StreamK.parseBreakChunks
    -- , parseBreakD
    -- , foldManyChunks
    -- , parseManyChunks
    , K.parseBreakChunks
    , K.parseChunks

    -- ** Array Folds
    -- XXX Use parseBreakChunks/parseChunks instead
    -- foldBreak can be implemented using parseBreak. Use StreamK.
    , runArrayFold
    , runArrayFoldBreak
    -- , parseArr
    , runArrayParserDBreak -- StreamK.parseBreakChunks
    , runArrayFoldMany

    , toArray

    -- * Compaction
    -- We can use something like foldManyChunks, parseManyChunks with a take
    -- fold.
    , lpackArraysChunksOf -- Fold.compactChunks
    , compact -- rechunk, compactChunks

    -- * Splitting
    -- We can use something like foldManyChunks, parseManyChunks with an
    -- appropriate splitting fold.
    , splitOn       -- Stream.rechunkOn
    , splitOnSuffix -- Stream.rechunkOnSuffix
    )
where

#include "ArrayMacros.h"
#include "inline.hs"

import Data.Bifunctor (second)
import Control.Exception (assert)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Proxy (Proxy(..))
import Data.Word (Word8)
import Streamly.Internal.Data.Unbox (Unbox(..))
import Fusion.Plugin.Types (Fuse(..))
import GHC.Exts (SpecConstrAnnotation(..))
import GHC.Types (SPEC(..))
import Prelude hiding (null, last, (!!), read, concat, unlines)

import Streamly.Data.Fold (Fold)
import Streamly.Internal.Data.Array.Type (Array(..))
import Streamly.Internal.Data.Fold.Chunked (ChunkFold(..))
import Streamly.Internal.Data.Parser (ParseError(..))
import Streamly.Internal.Data.Stream (Stream)
import Streamly.Internal.Data.StreamK (StreamK, fromStream, toStream)
import Streamly.Internal.Data.SVar.Type (adaptState, defState)
import Streamly.Internal.Data.Tuple.Strict (Tuple'(..))

import qualified Streamly.Data.Fold as FL
import qualified Streamly.Internal.Data.Array as A
import qualified Streamly.Internal.Data.Array as Array
import qualified Streamly.Internal.Data.MutArray.Type as MA
import qualified Streamly.Internal.Data.MutArray.Stream as AS
import qualified Streamly.Internal.Data.Fold.Type as FL (Fold(..), Step(..))
import qualified Streamly.Internal.Data.Parser as PR
import qualified Streamly.Internal.Data.Parser as PRD
    (Parser(..), Initial(..))
import qualified Streamly.Internal.Data.Stream as D
import qualified Streamly.Internal.Data.StreamK as K

-- XXX Since these are immutable arrays MonadIO constraint can be removed from
-- most places.

-------------------------------------------------------------------------------
-- Append
-------------------------------------------------------------------------------

-- XXX efficiently compare two streams of arrays. Two streams can have chunks
-- of different sizes, we can handle that in the stream comparison abstraction.
-- This could be useful e.g. to fast compare whether two files differ.

-- | Convert a stream of arrays into a stream of their elements.
--
-- Same as the following:
--
-- > concat = Stream.unfoldMany Array.read
--
-- @since 0.7.0
{-# INLINE concat #-}
concat :: (Monad m, Unbox a) => Stream m (Array a) -> Stream m a
-- concat m = fromStreamD $ A.flattenArrays (toStreamD m)
-- concat m = fromStreamD $ D.concatMap A.toStreamD (toStreamD m)
concat = D.unfoldMany A.reader

-- | Convert a stream of arrays into a stream of their elements reversing the
-- contents of each array before flattening.
--
-- > concatRev = Stream.unfoldMany Array.readerRev
--
-- @since 0.7.0
{-# INLINE concatRev #-}
concatRev :: (Monad m, Unbox a) => Stream m (Array a) -> Stream m a
-- concatRev m = fromStreamD $ A.flattenArraysRev (toStreamD m)
concatRev = D.unfoldMany A.readerRev

-------------------------------------------------------------------------------
-- Intersperse and append
-------------------------------------------------------------------------------

-- | Flatten a stream of arrays after inserting the given element between
-- arrays.
--
-- /Pre-release/
{-# INLINE interpose #-}
interpose :: (Monad m, Unbox a) => a -> Stream m (Array a) -> Stream m a
interpose x = D.interpose x A.reader

{-# INLINE intercalateSuffix #-}
intercalateSuffix :: (Monad m, Unbox a)
    => Array a -> Stream m (Array a) -> Stream m a
intercalateSuffix = D.intercalateSuffix A.reader

-- | Flatten a stream of arrays appending the given element after each
-- array.
--
-- @since 0.7.0
{-# INLINE interposeSuffix #-}
interposeSuffix :: (Monad m, Unbox a)
    => a -> Stream m (Array a) -> Stream m a
-- interposeSuffix x = fromStreamD . A.unlines x . toStreamD
interposeSuffix x = D.interposeSuffix x A.reader

data FlattenState s =
      OuterLoop s
    | InnerLoop s !MA.MutableByteArray !Int !Int

-- XXX This is a special case of interposeSuffix, can be removed.
-- XXX Remove monadIO constraint
{-# INLINE_NORMAL unlines #-}
unlines :: forall m a. (MonadIO m, Unbox a)
    => a -> D.Stream m (Array a) -> D.Stream m a
unlines sep (D.Stream step state) = D.Stream step' (OuterLoop state)
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
        x <- liftIO $ peekByteIndex p contents
        return $ D.Yield x (InnerLoop st contents (INDEX_NEXT(p,a)) end)

-------------------------------------------------------------------------------
-- Compact
-------------------------------------------------------------------------------

-- XXX These would not be needed once we implement compactLEFold, see
-- module Streamly.Internal.Data.Stream.MutChunked
--
-- XXX Note that this thaws immutable arrays for appending, that may be
-- problematic if multiple users do the same thing, however, immutable arrays
-- would usually have no capacity to append, therefore, a copy will be forced
-- anyway. Confirm this. We can forcefully trim the array capacity before thaw
-- to ensure this.
{-# INLINE_NORMAL packArraysChunksOf #-}
packArraysChunksOf :: (MonadIO m, Unbox a)
    => Int -> D.Stream m (Array a) -> D.Stream m (Array a)
packArraysChunksOf n str =
    D.map A.unsafeFreeze $ AS.packArraysChunksOf n $ D.map A.unsafeThaw str

-- XXX instead of writing two different versions of this operation, we should
-- write it as a pipe.
--
-- XXX Confirm that immutable arrays won't be modified.
{-# INLINE_NORMAL lpackArraysChunksOf #-}
lpackArraysChunksOf :: (MonadIO m, Unbox a)
    => Int -> Fold m (Array a) () -> Fold m (Array a) ()
lpackArraysChunksOf n fld =
    FL.lmap A.unsafeThaw $ AS.lpackArraysChunksOf n (FL.lmap A.unsafeFreeze fld)

-- | Coalesce adjacent arrays in incoming stream to form bigger arrays of a
-- maximum specified size in bytes.
--
-- @since 0.7.0
{-# INLINE compact #-}
compact :: (MonadIO m, Unbox a)
    => Int -> Stream m (Array a) -> Stream m (Array a)
compact = packArraysChunksOf

-------------------------------------------------------------------------------
-- Split
-------------------------------------------------------------------------------

data SplitState s arr
    = Initial s
    | Buffering s arr
    | Splitting s arr
    | Yielding arr (SplitState s arr)
    | Finishing

-- | Split a stream of arrays on a given separator byte, dropping the separator
-- and coalescing all the arrays between two separators into a single array.
--
-- @since 0.7.0
{-# INLINE_NORMAL _splitOn #-}
_splitOn
    :: MonadIO m
    => Word8
    -> D.Stream m (Array Word8)
    -> D.Stream m (Array Word8)
_splitOn byte (D.Stream step state) = D.Stream step' (Initial state)

    where

    {-# INLINE_LATE step' #-}
    step' gst (Initial st) = do
        r <- step gst st
        case r of
            D.Yield arr s -> do
                (arr1, marr2) <- A.breakOn byte arr
                return $ case marr2 of
                    Nothing   -> D.Skip (Buffering s arr1)
                    Just arr2 -> D.Skip (Yielding arr1 (Splitting s arr2))
            D.Skip s -> return $ D.Skip (Initial s)
            D.Stop -> return D.Stop

    step' gst (Buffering st buf) = do
        r <- step gst st
        case r of
            D.Yield arr s -> do
                (arr1, marr2) <- A.breakOn byte arr
                buf' <- A.splice buf arr1
                return $ case marr2 of
                    Nothing -> D.Skip (Buffering s buf')
                    Just x -> D.Skip (Yielding buf' (Splitting s x))
            D.Skip s -> return $ D.Skip (Buffering s buf)
            D.Stop -> return $
                if A.byteLength buf == 0
                then D.Stop
                else D.Skip (Yielding buf Finishing)

    step' _ (Splitting st buf) = do
        (arr1, marr2) <- A.breakOn byte buf
        return $ case marr2 of
                Nothing -> D.Skip $ Buffering st arr1
                Just arr2 -> D.Skip $ Yielding arr1 (Splitting st arr2)

    step' _ (Yielding arr next) = return $ D.Yield arr next
    step' _ Finishing = return D.Stop

-- XXX Remove MonadIO constraint.
-- | Split a stream of arrays on a given separator byte, dropping the separator
-- and coalescing all the arrays between two separators into a single array.
--
-- @since 0.7.0
{-# INLINE splitOn #-}
splitOn
    :: (MonadIO m)
    => Word8
    -> Stream m (Array Word8)
    -> Stream m (Array Word8)
splitOn byte = D.splitInnerBy (A.breakOn byte) A.splice

{-# INLINE splitOnSuffix #-}
splitOnSuffix
    :: (MonadIO m)
    => Word8
    -> Stream m (Array Word8)
    -> Stream m (Array Word8)
-- splitOn byte s = fromStreamD $ A.splitOn byte $ toStreamD s
splitOnSuffix byte = D.splitInnerBySuffix (A.breakOn byte) A.splice

-------------------------------------------------------------------------------
-- Elimination - Running folds
-------------------------------------------------------------------------------

{-# INLINE_NORMAL foldBreakD #-}
foldBreakD :: forall m a b. (MonadIO m, Unbox a) =>
    Fold m a b -> D.Stream m (Array a) -> m (b, D.Stream m (Array a))
foldBreakD (FL.Fold fstep initial _ final) stream@(D.Stream step state) = do
    res <- initial
    case res of
        FL.Partial fs -> go SPEC state fs
        FL.Done fb -> return $! (fb, stream)

    where

    {-# INLINE go #-}
    go !_ st !fs = do
        r <- step defState st
        case r of
            D.Yield (Array contents start end) s ->
                let fp = Tuple' end contents
                 in goArray SPEC s fp start fs
            D.Skip s -> go SPEC s fs
            D.Stop -> do
                b <- final fs
                return (b, D.nil)

    goArray !_ s (Tuple' end _) !cur !fs
        | cur == end = do
            go SPEC s fs
    goArray !_ st fp@(Tuple' end contents) !cur !fs = do
        x <- liftIO $ peekByteIndex cur contents
        res <- fstep fs x
        let next = INDEX_NEXT(cur,a)
        case res of
            FL.Done b -> do
                let arr = Array contents next end
                return $! (b, D.cons arr (D.Stream step st))
            FL.Partial fs1 -> goArray SPEC st fp next fs1

{-# INLINE_NORMAL foldBreakK #-}
foldBreakK :: forall m a b. (MonadIO m, Unbox a) =>
    Fold m a b -> K.StreamK m (Array a) -> m (b, K.StreamK m (Array a))
foldBreakK (FL.Fold fstep initial _ final) stream = do
    res <- initial
    case res of
        FL.Partial fs -> go fs stream
        FL.Done fb -> return (fb, stream)

    where

    {-# INLINE go #-}
    go !fs st = do
        let stop = (, K.nil) <$> final fs
            single a = yieldk a K.nil
            yieldk (Array contents start end) r =
                let fp = Tuple' end contents
                 in goArray fs r fp start
         in K.foldStream defState yieldk single stop st

    goArray !fs st (Tuple' end _) !cur
        | cur == end = do
            go fs st
    goArray !fs st fp@(Tuple' end contents) !cur = do
        x <- liftIO $ peekByteIndex cur contents
        res <- fstep fs x
        let next = INDEX_NEXT(cur,a)
        case res of
            FL.Done b -> do
                let arr = Array contents next end
                return $! (b, K.cons arr st)
            FL.Partial fs1 -> goArray fs1 st fp next

-- | Fold an array stream using the supplied 'Fold'. Returns the fold result
-- and the unconsumed stream.
--
-- > foldBreak f = runArrayFoldBreak (ChunkFold.fromFold f)
--
-- /Internal/
--
{-# INLINE_NORMAL foldBreak #-}
foldBreak ::
       (MonadIO m, Unbox a)
    => Fold m a b
    -> StreamK m (A.Array a)
    -> m (b, StreamK m (A.Array a))
-- foldBreak f s = fmap fromStreamD <$> foldBreakD f (toStreamD s)
foldBreak = foldBreakK
-- If foldBreak performs better than runArrayFoldBreak we can use a rewrite
-- rule to rewrite runArrayFoldBreak to fold.
-- foldBreak f = runArrayFoldBreak (ChunkFold.fromFold f)

-------------------------------------------------------------------------------
-- Fold to a single Array
-------------------------------------------------------------------------------

-- When we have to take an array partially, take the last part of the array.
{-# INLINE takeArrayListRev #-}
takeArrayListRev :: forall a. Unbox a => Int -> [Array a] -> [Array a]
takeArrayListRev = go

    where

    go _ [] = []
    go n _ | n <= 0 = []
    go n (x:xs) =
        let len = Array.length x
        in if n > len
           then x : go (n - len) xs
           else if n == len
           then [x]
           else let !(Array contents _ end) = x
                    !start = end - (n * SIZE_OF(a))
                 in [Array contents start end]

-- When we have to take an array partially, take the last part of the array in
-- the first split.
{-# INLINE splitAtArrayListRev #-}
splitAtArrayListRev ::
    forall a. Unbox a => Int -> [Array a] -> ([Array a],[Array a])
splitAtArrayListRev n ls
  | n <= 0 = ([], ls)
  | otherwise = go n ls
    where
        go :: Int -> [Array a] -> ([Array a], [Array a])
        go _  []     = ([], [])
        go m (x:xs) =
            let len = Array.length x
                (xs', xs'') = go (m - len) xs
             in if m > len
                then (x:xs', xs'')
                else if m == len
                then ([x],xs)
                else let !(Array contents start end) = x
                         end1 = end - (m * SIZE_OF(a))
                         arr2 = Array contents start end1
                         arr1 = Array contents end1 end
                      in ([arr1], arr2:xs)

-------------------------------------------------------------------------------
-- Fold to a single Array
-------------------------------------------------------------------------------

{-

-- XXX Both of these implementations of splicing seem to perform equally well.
-- We need to perform benchmarks over a range of sizes though.

-- CAUTION! length must more than equal to lengths of all the arrays in the
-- stream.
{-# INLINE spliceArraysLenUnsafe #-}
spliceArraysLenUnsafe :: (MonadIO m, Unbox a)
    => Int -> Stream m (MutArray a) -> m (MutArray a)
spliceArraysLenUnsafe len buffered = do
    -- XXX The new array's pinned state should depend on the first element
    -- of the stream. We should uncons the stream and do the required.
    arr <- undefined
    D.foldlM' MA.spliceUnsafe (return arr) buffered

{-# INLINE _spliceArrays #-}
_spliceArrays :: (MonadIO m, Unbox a)
    => Stream m (Array a) -> m (Array a)
_spliceArrays s = do
    buffered <- D.foldr K.cons K.nil s
    len <- K.fold FL.sum (fmap Array.length buffered)
    -- XXX The new array's pinned state should depend on the first element
    -- of the stream. We should uncons the stream and do the required.
    arr <- undefined
    final <- D.foldlM' writeArr (return arr) (toStream buffered)
    return $ A.unsafeFreeze final

    where

    writeArr dst arr = MA.spliceUnsafe dst (A.unsafeThaw arr)

{-# INLINE _spliceArraysBuffered #-}
_spliceArraysBuffered :: (MonadIO m, Unbox a)
    => Stream m (Array a) -> m (Array a)
_spliceArraysBuffered s = do
    buffered <- D.foldr K.cons K.nil s
    len <- K.fold FL.sum (fmap Array.length buffered)
    A.unsafeFreeze <$>
        spliceArraysLenUnsafe len (fmap A.unsafeThaw (toStream buffered))

-}

{-# INLINE spliceArraysRealloced #-}
spliceArraysRealloced :: forall m a. (MonadIO m, Unbox a)
    => Stream m (Array a) -> m (Array a)
spliceArraysRealloced s = do
    res <- D.uncons s
    case res of
        Just (a, strm) -> do
            arr <-
                D.foldlM'
                    MA.spliceExp
                    (pure (A.unsafeThaw a))
                    (fmap A.unsafeThaw strm)
            liftIO $ A.unsafeFreeze <$> MA.rightSize arr
        Nothing -> pure A.nil

-- XXX This should just be "fold A.write"
--
-- | Given a stream of arrays, splice them all together to generate a single
-- array. The stream must be /finite/.
--
-- @since 0.7.0
{-# INLINE toArray #-}
toArray :: (MonadIO m, Unbox a) => Stream m (Array a) -> m (Array a)
toArray = spliceArraysRealloced
-- spliceArrays = _spliceArraysBuffered

-- exponentially increasing sizes of the chunks upto the max limit.
-- XXX this will be easier to implement with parsers/terminating folds
-- With this we should be able to reduce the number of chunks/allocations.
-- The reallocation/copy based toArray can also be implemented using this.
--
{-
{-# INLINE toArraysInRange #-}
toArraysInRange :: (MonadIO m, Unbox a)
    => Int -> Int -> Fold m (Array a) b -> Fold m a b
toArraysInRange low high (Fold step initial extract) =
-}

{-
-- | Fold the input to a pure buffered stream (List) of arrays.
{-# INLINE _toArraysOf #-}
_toArraysOf :: (MonadIO m, Unbox a)
    => Int -> Fold m a (Stream Identity (Array a))
_toArraysOf n = FL.groupsOf n (A.writeNF n) FL.toStream
-}

-------------------------------------------------------------------------------
-- Elimination - running element parsers
-------------------------------------------------------------------------------

-- GHC parser does not accept {-# ANN type [] NoSpecConstr #-}, so we need
-- to make a newtype.
{-# ANN type List NoSpecConstr #-}
newtype List a = List {getList :: [a]}

{-
-- This can be generalized to any type provided it can be unfolded to a stream
-- and it can be combined using a semigroup operation.
--
-- XXX This should be written using CPS (as parseK) if we want it to scale wrt
-- to the number of times it can be called on the same stream.
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

{-# INLINE_NORMAL parseBreakK #-}
parseBreakK ::
       forall m a b. (MonadIO m, Unbox a)
    => PRD.Parser a m b
    -> K.StreamK m (Array.Array a)
    -> m (Either ParseError b, K.StreamK m (Array.Array a))
parseBreakK (PRD.Parser pstep initial extract) stream = do
    res <- initial
    case res of
        PRD.IPartial s -> go s stream []
        PRD.IDone b -> return (Right b, stream)
        PRD.IError err -> return (Left (ParseError err), stream)

    where

    -- "backBuf" contains last few items in the stream that we may have to
    -- backtrack to.
    --
    -- XXX currently we are using a dumb list based approach for backtracking
    -- buffer. This can be replaced by a sliding/ring buffer using Data.Array.
    -- That will allow us more efficient random back and forth movement.
    go !pst st backBuf = do
        let stop = goStop pst backBuf -- (, K.nil) <$> extract pst
            single a = yieldk a K.nil
            yieldk arr r = goArray pst backBuf r arr
         in K.foldStream defState yieldk single stop st

    -- Use strictness on "cur" to keep it unboxed
    goArray !pst backBuf st (Array _ cur end) | cur == end = go pst st backBuf
    goArray !pst backBuf st (Array contents cur end) = do
        x <- liftIO $ peekByteIndex cur contents
        pRes <- pstep pst x
        let next = INDEX_NEXT(cur,a)
        case pRes of
            PR.Partial 0 s ->
                 goArray s [] st (Array contents next end)
            PR.Partial n s -> do
                assert (n <= Prelude.length (x:backBuf)) (return ())
                let src0 = Prelude.take n (x:backBuf)
                    arr0 = A.fromListN n (Prelude.reverse src0)
                    arr1 = Array contents next end
                    src = arr0 <> arr1
                goArray s [] st src
            PR.Continue 0 s ->
                goArray s (x:backBuf) st (Array contents next end)
            PR.Continue n s -> do
                assert (n <= Prelude.length (x:backBuf)) (return ())
                let (src0, buf1) = splitAt n (x:backBuf)
                    arr0 = A.fromListN n (Prelude.reverse src0)
                    arr1 = Array contents next end
                    src = arr0 <> arr1
                goArray s buf1 st src
            PR.Done 0 b -> do
                let arr = Array contents next end
                return (Right b, K.cons arr st)
            PR.Done n b -> do
                assert (n <= Prelude.length (x:backBuf)) (return ())
                let src0 = Prelude.take n (x:backBuf)
                    -- XXX Use fromListRevN once implemented
                    -- arr0 = A.fromListRevN n src0
                    arr0 = A.fromListN n (Prelude.reverse src0)
                    arr1 = Array contents next end
                    str = K.cons arr0 (K.cons arr1 st)
                return (Right b, str)
            PR.Error err -> do
                let n = Prelude.length backBuf
                    arr0 = A.fromListN n (Prelude.reverse backBuf)
                    arr1 = Array contents cur end
                    str = K.cons arr0 (K.cons arr1 stream)
                return (Left (ParseError err), str)

    -- This is a simplified goArray
    goExtract !pst backBuf (Array _ cur end)
        | cur == end = goStop pst backBuf
    goExtract !pst backBuf (Array contents cur end) = do
        x <- liftIO $ peekByteIndex cur contents
        pRes <- pstep pst x
        let next = INDEX_NEXT(cur,a)
        case pRes of
            PR.Partial 0 s ->
                 goExtract s [] (Array contents next end)
            PR.Partial n s -> do
                assert (n <= Prelude.length (x:backBuf)) (return ())
                let src0 = Prelude.take n (x:backBuf)
                    arr0 = A.fromListN n (Prelude.reverse src0)
                    arr1 = Array contents next end
                    src = arr0 <> arr1
                goExtract s [] src
            PR.Continue 0 s ->
                goExtract s backBuf (Array contents next end)
            PR.Continue n s -> do
                assert (n <= Prelude.length (x:backBuf)) (return ())
                let (src0, buf1) = splitAt n (x:backBuf)
                    arr0 = A.fromListN n (Prelude.reverse src0)
                    arr1 = Array contents next end
                    src = arr0 <> arr1
                goExtract s buf1 src
            PR.Done 0 b -> do
                let arr = Array contents next end
                return (Right b, K.fromPure arr)
            PR.Done n b -> do
                assert (n <= Prelude.length backBuf) (return ())
                let src0 = Prelude.take n backBuf
                    -- XXX Use fromListRevN once implemented
                    -- arr0 = A.fromListRevN n src0
                    arr0 = A.fromListN n (Prelude.reverse src0)
                    arr1 = Array contents next end
                    str = K.cons arr0 (K.fromPure arr1)
                return (Right b, str)
            PR.Error err -> do
                let n = Prelude.length backBuf
                    arr0 = A.fromListN n (Prelude.reverse backBuf)
                    arr1 = Array contents cur end
                    str = K.cons arr0 (K.cons arr1 stream)
                return (Left (ParseError err), str)

    -- This is a simplified goExtract
    {-# INLINE goStop #-}
    goStop !pst backBuf = do
        pRes <- extract pst
        case pRes of
            PR.Partial _ _ -> error "Bug: parseBreak: Partial in extract"
            PR.Continue 0 s ->
                goStop s backBuf
            PR.Continue n s -> do
                assert (n <= Prelude.length backBuf) (return ())
                let (src0, buf1) = splitAt n backBuf
                    arr = A.fromListN n (Prelude.reverse src0)
                goExtract s buf1 arr
            PR.Done 0 b ->
                return (Right b, K.nil)
            PR.Done n b -> do
                assert (n <= Prelude.length backBuf) (return ())
                let src0 = Prelude.take n backBuf
                    -- XXX Use fromListRevN once implemented
                    -- arr0 = A.fromListRevN n src0
                    arr0 = A.fromListN n (Prelude.reverse src0)
                return (Right b, K.fromPure arr0)
            PR.Error err -> do
                let n = Prelude.length backBuf
                    arr0 = A.fromListN n (Prelude.reverse backBuf)
                return (Left (ParseError err), K.fromPure arr0)

-- | Parse an array stream using the supplied 'Parser'.  Returns the parse
-- result and the unconsumed stream. Throws 'ParseError' if the parse fails.
--
-- /Internal/
--
{-# INLINE_NORMAL parseBreak #-}
parseBreak ::
       (MonadIO m, Unbox a)
    => PR.Parser a m b
    -> StreamK m (A.Array a)
    -> m (Either ParseError b, StreamK m (A.Array a))
{-
parseBreak p s =
    fmap fromStreamD <$> parseBreakD (PRD.fromParserK p) (toStreamD s)
-}
parseBreak = parseBreakK

-------------------------------------------------------------------------------
-- Elimination - Running Array Folds and parsers
-------------------------------------------------------------------------------

-- | Note that this is not the same as using a @Parser (Array a) m b@ with the
-- regular "Streamly.Internal.Data.IsStream.parse" function. The regular parse
-- would consume the input arrays as single unit. This parser parses in the way
-- as described in the ChunkFold module. The input arrays are treated as @n@
-- element units and can be consumed partially. The remaining elements are
-- inserted in the source stream as an array.
--
{-# INLINE_NORMAL runArrayParserDBreak #-}
runArrayParserDBreak ::
       forall m a b. (MonadIO m, Unbox a)
    => PRD.Parser (Array a) m b
    -> D.Stream m (Array.Array a)
    -> m (Either ParseError b, D.Stream m (Array.Array a))
runArrayParserDBreak
    (PRD.Parser pstep initial extract)
    stream@(D.Stream step state) = do

    res <- initial
    case res of
        PRD.IPartial s -> go SPEC state (List []) s
        PRD.IDone b -> return (Right b, stream)
        PRD.IError err -> return (Left (ParseError err), stream)

    where

    -- "backBuf" contains last few items in the stream that we may have to
    -- backtrack to.
    --
    -- XXX currently we are using a dumb list based approach for backtracking
    -- buffer. This can be replaced by a sliding/ring buffer using Data.Array.
    -- That will allow us more efficient random back and forth movement.
    go _ st backBuf !pst = do
        r <- step defState st
        case r of
            D.Yield x s -> gobuf SPEC [x] s backBuf pst
            D.Skip s -> go SPEC s backBuf pst
            D.Stop -> goStop backBuf pst

    gobuf !_ [] s backBuf !pst = go SPEC s backBuf pst
    gobuf !_ (x:xs) s backBuf !pst = do
        pRes <- pstep pst x
        case pRes of
            PR.Partial 0 pst1 ->
                 gobuf SPEC xs s (List []) pst1
            PR.Partial n pst1 -> do
                assert
                    (n <= sum (map Array.length (x:getList backBuf)))
                    (return ())
                let src0 = takeArrayListRev n (x:getList backBuf)
                    src  = Prelude.reverse src0 ++ xs
                gobuf SPEC src s (List []) pst1
            PR.Continue 0 pst1 ->
                gobuf SPEC xs s (List (x:getList backBuf)) pst1
            PR.Continue n pst1 -> do
                assert
                    (n <= sum (map Array.length (x:getList backBuf)))
                    (return ())
                let (src0, buf1) = splitAtArrayListRev n (x:getList backBuf)
                    src  = Prelude.reverse src0 ++ xs
                gobuf SPEC src s (List buf1) pst1
            PR.Done 0 b -> do
                let str = D.append (D.fromList xs) (D.Stream step s)
                return (Right b, str)
            PR.Done n b -> do
                assert
                    (n <= sum (map Array.length (x:getList backBuf)))
                    (return ())
                let src0 = takeArrayListRev n (x:getList backBuf)
                    src = Prelude.reverse src0 ++ xs
                return (Right b, D.append (D.fromList src) (D.Stream step s))
            PR.Error err -> do
                let src0 = x:getList backBuf
                    src = Prelude.reverse src0 ++ x:xs
                    strm = D.append (D.fromList src) (D.Stream step s)
                return (Left (ParseError err), strm)

    -- This is a simplified gobuf
    goExtract _ [] backBuf !pst = goStop backBuf pst
    goExtract _ (x:xs) backBuf !pst = do
        pRes <- pstep pst x
        case pRes of
            PR.Partial 0 pst1 ->
                 goExtract SPEC xs (List []) pst1
            PR.Partial n pst1 -> do
                assert
                    (n <= sum (map Array.length (x:getList backBuf)))
                    (return ())
                let src0 = takeArrayListRev n (x:getList backBuf)
                    src  = Prelude.reverse src0 ++ xs
                goExtract SPEC src (List []) pst1
            PR.Continue 0 pst1 ->
                goExtract SPEC xs (List (x:getList backBuf)) pst1
            PR.Continue n pst1 -> do
                assert
                    (n <= sum (map Array.length (x:getList backBuf)))
                    (return ())
                let (src0, buf1) = splitAtArrayListRev n (x:getList backBuf)
                    src  = Prelude.reverse src0 ++ xs
                goExtract SPEC src (List buf1) pst1
            PR.Done 0 b ->
                return (Right b, D.fromList xs)
            PR.Done n b -> do
                assert
                    (n <= sum (map Array.length (x:getList backBuf)))
                    (return ())
                let src0 = takeArrayListRev n (x:getList backBuf)
                    src = Prelude.reverse src0 ++ xs
                return (Right b, D.fromList src)
            PR.Error err -> do
                let src0 = getList backBuf
                    src = Prelude.reverse src0 ++ x:xs
                return (Left (ParseError err), D.fromList src)

    -- This is a simplified goExtract
    {-# INLINE goStop #-}
    goStop backBuf pst = do
        pRes <- extract pst
        case pRes of
            PR.Partial _ _ -> error "Bug: runArrayParserDBreak: Partial in extract"
            PR.Continue 0 pst1 ->
                goStop backBuf pst1
            PR.Continue n pst1 -> do
                assert
                    (n <= sum (map Array.length (getList backBuf)))
                    (return ())
                let (src0, buf1) = splitAtArrayListRev n (getList backBuf)
                    src = Prelude.reverse src0
                goExtract SPEC src (List buf1) pst1
            PR.Done 0 b -> return (Right b, D.nil)
            PR.Done n b -> do
                assert
                    (n <= sum (map Array.length (getList backBuf)))
                    (return ())
                let src0 = takeArrayListRev n (getList backBuf)
                    src = Prelude.reverse src0
                return (Right b, D.fromList src)
            PR.Error err -> do
                let src0 = getList backBuf
                    src = Prelude.reverse src0
                return (Left (ParseError err), D.fromList src)

{-
-- | Parse an array stream using the supplied 'Parser'.  Returns the parse
-- result and the unconsumed stream. Throws 'ParseError' if the parse fails.
--
-- /Internal/
--
{-# INLINE parseArr #-}
parseArr ::
       (MonadIO m, MonadThrow m, Unbox a)
    => ASF.Parser a m b
    -> Stream m (A.Array a)
    -> m (b, Stream m (A.Array a))
parseArr p s = fmap fromStreamD <$> parseBreakD p (toStreamD s)
-}

-- | Fold an array stream using the supplied array stream 'Fold'.
--
-- /Pre-release/
--
{-# INLINE runArrayFold #-}
runArrayFold :: (MonadIO m, Unbox a) =>
    ChunkFold m a b -> StreamK m (A.Array a) -> m (Either ParseError b)
runArrayFold (ChunkFold p) s = fst <$> runArrayParserDBreak p (toStream s)

-- | Like 'fold' but also returns the remaining stream.
--
-- /Pre-release/
--
{-# INLINE runArrayFoldBreak #-}
runArrayFoldBreak :: (MonadIO m, Unbox a) =>
    ChunkFold m a b -> StreamK m (A.Array a) -> m (Either ParseError b, StreamK m (A.Array a))
runArrayFoldBreak (ChunkFold p) s =
    second fromStream <$> runArrayParserDBreak p (toStream s)

{-# ANN type ParseChunksState Fuse #-}
data ParseChunksState x inpBuf st pst =
      ParseChunksInit inpBuf st
    | ParseChunksInitBuf inpBuf
    | ParseChunksInitLeftOver inpBuf
    | ParseChunksStream st inpBuf !pst
    | ParseChunksStop inpBuf !pst
    | ParseChunksBuf inpBuf st inpBuf !pst
    | ParseChunksExtract inpBuf inpBuf !pst
    | ParseChunksYield x (ParseChunksState x inpBuf st pst)

{-# INLINE_NORMAL runArrayFoldManyD #-}
runArrayFoldManyD
    :: (Monad m, Unbox a)
    => ChunkFold m a b
    -> D.Stream m (Array a)
    -> D.Stream m (Either ParseError b)
runArrayFoldManyD
    (ChunkFold (PRD.Parser pstep initial extract)) (D.Stream step state) =

    D.Stream stepOuter (ParseChunksInit [] state)

    where

    {-# INLINE_LATE stepOuter #-}
    -- Buffer is empty, get the first element from the stream, initialize the
    -- fold and then go to stream processing loop.
    stepOuter gst (ParseChunksInit [] st) = do
        r <- step (adaptState gst) st
        case r of
            D.Yield x s -> do
                res <- initial
                case res of
                    PRD.IPartial ps ->
                        return $ D.Skip $ ParseChunksBuf [x] s [] ps
                    PRD.IDone pb -> do
                        let next = ParseChunksInit [x] s
                        return $ D.Skip $ ParseChunksYield (Right pb) next
                    PRD.IError err -> do
                        let next = ParseChunksInitLeftOver []
                        return
                            $ D.Skip
                            $ ParseChunksYield (Left (ParseError err)) next
            D.Skip s -> return $ D.Skip $ ParseChunksInit [] s
            D.Stop   -> return D.Stop

    -- Buffer is not empty, go to buffered processing loop
    stepOuter _ (ParseChunksInit src st) = do
        res <- initial
        case res of
            PRD.IPartial ps ->
                return $ D.Skip $ ParseChunksBuf src st [] ps
            PRD.IDone pb ->
                let next = ParseChunksInit src st
                 in return $ D.Skip $ ParseChunksYield (Right pb) next
            PRD.IError err -> do
                let next = ParseChunksInitLeftOver []
                return
                    $ D.Skip
                    $ ParseChunksYield (Left (ParseError err)) next

    -- This is a simplified ParseChunksInit
    stepOuter _ (ParseChunksInitBuf src) = do
        res <- initial
        case res of
            PRD.IPartial ps ->
                return $ D.Skip $ ParseChunksExtract src [] ps
            PRD.IDone pb ->
                let next = ParseChunksInitBuf src
                 in return $ D.Skip $ ParseChunksYield (Right pb) next
            PRD.IError err -> do
                let next = ParseChunksInitLeftOver []
                return
                    $ D.Skip
                    $ ParseChunksYield (Left (ParseError err)) next

    -- XXX we just discard any leftover input at the end
    stepOuter _ (ParseChunksInitLeftOver _) = return D.Stop

    -- Buffer is empty, process elements from the stream
    stepOuter gst (ParseChunksStream st backBuf pst) = do
        r <- step (adaptState gst) st
        case r of
            D.Yield x s -> do
                pRes <- pstep pst x
                case pRes of
                    PR.Partial 0 pst1 ->
                        return $ D.Skip $ ParseChunksStream s [] pst1
                    PR.Partial n pst1 -> do
                        assert
                            (n <= sum (map Array.length (x:backBuf)))
                            (return ())
                        let src0 = takeArrayListRev n (x:backBuf)
                            src  = Prelude.reverse src0
                        return $ D.Skip $ ParseChunksBuf src s [] pst1
                    PR.Continue 0 pst1 ->
                        return $ D.Skip $ ParseChunksStream s (x:backBuf) pst1
                    PR.Continue n pst1 -> do
                        assert
                            (n <= sum (map Array.length (x:backBuf)))
                            (return ())
                        let (src0, buf1) = splitAtArrayListRev n (x:backBuf)
                            src  = Prelude.reverse src0
                        return $ D.Skip $ ParseChunksBuf src s buf1 pst1
                    PR.Done 0 b -> do
                        return $ D.Skip $
                            ParseChunksYield (Right b) (ParseChunksInit [] s)
                    PR.Done n b -> do
                        assert
                            (n <= sum (map Array.length (x:backBuf)))
                            (return ())
                        let src0 = takeArrayListRev n (x:backBuf)
                            src = Prelude.reverse src0
                            next = ParseChunksInit src s
                        return
                            $ D.Skip
                            $ ParseChunksYield (Right b) next
                    PR.Error err -> do
                        let next = ParseChunksInitLeftOver []
                        return
                            $ D.Skip
                            $ ParseChunksYield (Left (ParseError err)) next

            D.Skip s -> return $ D.Skip $ ParseChunksStream s backBuf pst
            D.Stop -> return $ D.Skip $ ParseChunksStop backBuf pst

    -- go back to stream processing mode
    stepOuter _ (ParseChunksBuf [] s buf pst) =
        return $ D.Skip $ ParseChunksStream s buf pst

    -- buffered processing loop
    stepOuter _ (ParseChunksBuf (x:xs) s backBuf pst) = do
        pRes <- pstep pst x
        case pRes of
            PR.Partial 0 pst1 ->
                return $ D.Skip $ ParseChunksBuf xs s [] pst1
            PR.Partial n pst1 -> do
                assert (n <= sum (map Array.length (x:backBuf))) (return ())
                let src0 = takeArrayListRev n (x:backBuf)
                    src  = Prelude.reverse src0 ++ xs
                return $ D.Skip $ ParseChunksBuf src s [] pst1
            PR.Continue 0 pst1 ->
                return $ D.Skip $ ParseChunksBuf xs s (x:backBuf) pst1
            PR.Continue n pst1 -> do
                assert (n <= sum (map Array.length (x:backBuf))) (return ())
                let (src0, buf1) = splitAtArrayListRev n (x:backBuf)
                    src  = Prelude.reverse src0 ++ xs
                return $ D.Skip $ ParseChunksBuf src s buf1 pst1
            PR.Done 0 b ->
                return
                    $ D.Skip
                    $ ParseChunksYield (Right b) (ParseChunksInit xs s)
            PR.Done n b -> do
                assert (n <= sum (map Array.length (x:backBuf))) (return ())
                let src0 = takeArrayListRev n (x:backBuf)
                    src = Prelude.reverse src0 ++ xs
                return
                    $ D.Skip
                    $ ParseChunksYield (Right b) (ParseChunksInit src s)
            PR.Error err -> do
                let next = ParseChunksInitLeftOver []
                return
                    $ D.Skip
                    $ ParseChunksYield (Left (ParseError err)) next

    -- This is a simplified ParseChunksBuf
    stepOuter _ (ParseChunksExtract [] buf pst) =
        return $ D.Skip $ ParseChunksStop buf pst

    stepOuter _ (ParseChunksExtract (x:xs) backBuf pst) = do
        pRes <- pstep pst x
        case pRes of
            PR.Partial 0 pst1 ->
                return $ D.Skip $ ParseChunksExtract xs [] pst1
            PR.Partial n pst1 -> do
                assert (n <= sum (map Array.length (x:backBuf))) (return ())
                let src0 = takeArrayListRev n (x:backBuf)
                    src  = Prelude.reverse src0 ++ xs
                return $ D.Skip $ ParseChunksExtract src [] pst1
            PR.Continue 0 pst1 ->
                return $ D.Skip $ ParseChunksExtract xs (x:backBuf) pst1
            PR.Continue n pst1 -> do
                assert (n <= sum (map Array.length (x:backBuf))) (return ())
                let (src0, buf1) = splitAtArrayListRev n (x:backBuf)
                    src  = Prelude.reverse src0 ++ xs
                return $ D.Skip $ ParseChunksExtract src buf1 pst1
            PR.Done 0 b ->
                return
                    $ D.Skip
                    $ ParseChunksYield (Right b) (ParseChunksInitBuf xs)
            PR.Done n b -> do
                assert (n <= sum (map Array.length (x:backBuf))) (return ())
                let src0 = takeArrayListRev n (x:backBuf)
                    src = Prelude.reverse src0 ++ xs
                return
                    $ D.Skip
                    $ ParseChunksYield (Right b) (ParseChunksInitBuf src)
            PR.Error err -> do
                let next = ParseChunksInitLeftOver []
                return
                    $ D.Skip
                    $ ParseChunksYield (Left (ParseError err)) next


    -- This is a simplified ParseChunksExtract
    stepOuter _ (ParseChunksStop backBuf pst) = do
        pRes <- extract pst
        case pRes of
            PR.Partial _ _ -> error "runArrayFoldManyD: Partial in extract"
            PR.Continue 0 pst1 ->
                return $ D.Skip $ ParseChunksStop backBuf pst1
            PR.Continue n pst1 -> do
                assert (n <= sum (map Array.length backBuf)) (return ())
                let (src0, buf1) = splitAtArrayListRev n backBuf
                    src  = Prelude.reverse src0
                return $ D.Skip $ ParseChunksExtract src buf1 pst1
            PR.Done 0 b ->
                return
                    $ D.Skip
                    $ ParseChunksYield (Right b) (ParseChunksInitLeftOver [])
            PR.Done n b -> do
                assert (n <= sum (map Array.length backBuf)) (return ())
                let src0 = takeArrayListRev n backBuf
                    src = Prelude.reverse src0
                return
                    $ D.Skip
                    $ ParseChunksYield (Right b) (ParseChunksInitBuf src)
            PR.Error err -> do
                let next = ParseChunksInitLeftOver []
                return
                    $ D.Skip
                    $ ParseChunksYield (Left (ParseError err)) next

    stepOuter _ (ParseChunksYield a next) = return $ D.Yield a next

-- | Apply an 'ChunkFold' repeatedly on an array stream and emit the
-- fold outputs in the output stream.
--
-- See "Streamly.Data.Stream.foldMany" for more details.
--
-- /Pre-release/
{-# INLINE runArrayFoldMany #-}
runArrayFoldMany
    :: (Monad m, Unbox a)
    => ChunkFold m a b
    -> StreamK m (Array a)
    -> StreamK m (Either ParseError b)
runArrayFoldMany p m = fromStream $ runArrayFoldManyD p (toStream m)
