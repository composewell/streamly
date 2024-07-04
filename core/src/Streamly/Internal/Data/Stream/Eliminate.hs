{-# LANGUAGE CPP #-}
-- |
-- Module      : Streamly.Internal.Data.Stream.Eliminate
-- Copyright   : (c) 2018 Composewell Technologies
--               (c) Roman Leshchinskiy 2008-2010
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

-- A few functions in this module have been adapted from the vector package
-- (c) Roman Leshchinskiy.
--
module Streamly.Internal.Data.Stream.Eliminate
    (
    -- * Running a Parser
      parse
    , parseD
    , parseBreak
    , parseBreakD

    -- * Deconstruction
    , uncons

    -- * Right Folds
    , foldr1

    -- * Specific Fold Functions
    , mapM_ -- Map and Fold
    , null
    , head
    , headElse
    , init
    , tail
    , last
    , elem
    , notElem
    , all
    , any
    , maximum
    , maximumBy
    , minimum
    , minimumBy
    , lookup
    , findM
    , find
    , (!!)
    , the

    -- * To containers
    , toListRev

    -- * Multi-Stream Folds
    -- | These should probably be expressed using parsers.
    , isPrefixOf
    , isInfixOf
    , isSuffixOf
    , isSuffixOfUnbox
    , isSubsequenceOf
    , stripPrefix
    , stripSuffix
    , stripSuffixUnbox
    )
where

#include "inline.hs"

import Control.Exception (assert)
import Control.Monad.IO.Class (MonadIO(..))
import GHC.Exts (SpecConstrAnnotation(..))
import GHC.Types (SPEC(..))
import Streamly.Internal.Data.Parser (ParseError(..))
import Streamly.Internal.Data.SVar.Type (adaptState, defState)
import Streamly.Internal.Data.Unbox (Unbox)

import Streamly.Internal.Data.Maybe.Strict (Maybe'(..))

import qualified Streamly.Internal.Data.Array.Type as Array
import qualified Streamly.Internal.Data.Fold as Fold
import qualified Streamly.Internal.Data.Parser as PR
import qualified Streamly.Internal.Data.Parser as PRD
import qualified Streamly.Internal.Data.Stream.Generate as StreamD
import qualified Streamly.Internal.Data.Stream.Nesting as Nesting
import qualified Streamly.Internal.Data.Stream.Transform as StreamD

import Prelude hiding
       ( Foldable(..), all, any, head, last, lookup, mapM, mapM_
       , notElem, splitAt, init, tail, (!!))
import Data.Foldable (length)
import Streamly.Internal.Data.Stream.Type

#include "DocTestDataStream.hs"

------------------------------------------------------------------------------
-- Elimination by Folds
------------------------------------------------------------------------------

------------------------------------------------------------------------------
-- Right Folds
------------------------------------------------------------------------------

{-# INLINE_NORMAL foldr1 #-}
foldr1 :: Monad m => (a -> a -> a) -> Stream m a -> m (Maybe a)
foldr1 f m = do
     r <- uncons m
     case r of
         Nothing   -> return Nothing
         Just (h, t) -> fmap Just (foldr f h t)

------------------------------------------------------------------------------
-- Parsers
------------------------------------------------------------------------------

-- Inlined definition. Without the inline "serially/parser/take" benchmark
-- degrades and parseMany does not fuse. Even using "inline" at the callsite
-- does not help.
{-# INLINE splitAt #-}
splitAt :: Int -> [a] -> ([a],[a])
splitAt n ls
  | n <= 0 = ([], ls)
  | otherwise          = splitAt' n ls
    where
        splitAt' :: Int -> [a] -> ([a], [a])
        splitAt' _  []     = ([], [])
        splitAt' 1  (x:xs) = ([x], xs)
        splitAt' m  (x:xs) = (x:xs', xs'')
          where
            (xs', xs'') = splitAt' (m - 1) xs

-- GHC parser does not accept {-# ANN type [] NoSpecConstr #-}, so we need
-- to make a newtype.
{-# ANN type List NoSpecConstr #-}
newtype List a = List {getList :: [a]}

-- | Run a 'Parse' over a stream.
{-# INLINE_NORMAL parseD #-}
parseD
    :: Monad m
    => PRD.Parser a m b
    -> Stream m a
    -> m (Either ParseError b)
parseD parser strm = do
    (b, _) <- parseBreakD parser strm
    return b

-- | Parse a stream using the supplied 'Parser'.
--
-- Parsers (See "Streamly.Internal.Data.Parser") are more powerful folds that
-- add backtracking and error functionality to terminating folds. Unlike folds,
-- parsers may not always result in a valid output, they may result in an
-- error.  For example:
--
-- >>> Stream.parse (Parser.takeEQ 1 Fold.drain) Stream.nil
-- Left (ParseError "takeEQ: Expecting exactly 1 elements, input terminated on 0")
--
-- Note: @parse p@ is not the same as  @head . parseMany p@ on an empty stream.
--
{-# INLINE [3] parse #-}
parse :: Monad m => PR.Parser a m b -> Stream m a -> m (Either ParseError b)
parse = parseD

-- XXX It may be a good idea to use constant sized chunks for backtracking. We
-- can take a byte stream but when we have to backtrack we create constant
-- sized chunks. We maintain one forward list and one backward list of constant
-- sized chunks, and a last backtracking offset. That way we just need lists of
-- contents and no need to maintain start/end pointers for individual arrays,
-- reducing bookkeeping work.

-- | Run a 'Parse' over a stream and return rest of the Stream.
{-# INLINE_NORMAL parseBreakD #-}
parseBreakD
    :: Monad m
    => PRD.Parser a m b
    -> Stream m a
    -> m (Either ParseError b, Stream m a)
parseBreakD (PRD.Parser pstep initial extract) stream@(Stream step state) = do
    res <- initial
    case res of
        PRD.IPartial s -> go SPEC state (List []) s
        PRD.IDone b -> return (Right b, stream)
        PRD.IError err -> return (Left (ParseError err), stream)

    where

    -- "buf" contains last few items in the stream that we may have to
    -- backtrack to.
    --
    -- XXX currently we are using a dumb list based approach for backtracking
    -- buffer. This can be replaced by a sliding/ring buffer using Data.Array.
    -- That will allow us more efficient random back and forth movement.
    go !_ st buf !pst = do
        r <- step defState st
        case r of
            Yield x s -> do
                pRes <- pstep pst x
                case pRes of
                    PR.Partial 0 pst1 -> go SPEC s (List []) pst1
                    PR.Partial 1 pst1 -> go1 SPEC s x pst1
                    PR.Partial n pst1 -> do
                        assert (n <= length (x:getList buf)) (return ())
                        let src0 = Prelude.take n (x:getList buf)
                            src  = Prelude.reverse src0
                        gobuf SPEC s (List []) (List src) pst1
                    PR.Continue 0 pst1 -> go SPEC s (List (x:getList buf)) pst1
                    PR.Continue 1 pst1 -> gobuf SPEC s buf (List [x]) pst1
                    PR.Continue n pst1 -> do
                        assert (n <= length (x:getList buf)) (return ())
                        let (src0, buf1) = splitAt n (x:getList buf)
                            src  = Prelude.reverse src0
                        gobuf SPEC s (List buf1) (List src) pst1
                    PR.Done 0 b -> return (Right b, Stream step s)
                    PR.Done n b -> do
                        assert (n <= length (x:getList buf)) (return ())
                        let src0 = Prelude.take n (x:getList buf)
                            src  = Prelude.reverse src0
                        -- XXX This would make it quadratic. We should probably
                        -- use StreamK if we have to append many times.
                        return
                            ( Right b,
                              Nesting.append (fromList src) (Stream step s))
                    PR.Error err -> do
                        let src = Prelude.reverse $ x:getList buf
                        return
                            ( Left (ParseError err)
                            , Nesting.append (fromList src) (Stream step s)
                            )

            Skip s -> go SPEC s buf pst
            Stop -> goStop SPEC buf pst

    go1 _ s x !pst = do
        pRes <- pstep pst x
        case pRes of
            PR.Partial 0 pst1 ->
                go SPEC s (List []) pst1
            PR.Partial 1 pst1 -> do
                go1 SPEC s x pst1
            PR.Partial n _ ->
                error $ "parseBreak: parser bug, go1: Partial n = " ++ show n
            PR.Continue 0 pst1 ->
                go SPEC s (List [x]) pst1
            PR.Continue 1 pst1 ->
                go1 SPEC s x pst1
            PR.Continue n _ -> do
                error $ "parseBreak: parser bug, go1: Continue n = " ++ show n
            PR.Done 0 b -> do
                return (Right b, Stream step s)
            PR.Done 1 b -> do
                return (Right b, StreamD.cons x (Stream step s))
            PR.Done n _ -> do
                error $ "parseBreak: parser bug, go1: Done n = " ++ show n
            PR.Error err ->
                return
                    ( Left (ParseError err)
                    , Nesting.append (fromPure x) (Stream step s)
                    )

    gobuf !_ s buf (List []) !pst = go SPEC s buf pst
    gobuf !_ s buf (List (x:xs)) !pst = do
        pRes <- pstep pst x
        case pRes of
            PR.Partial 0 pst1 ->
                gobuf SPEC s (List []) (List xs) pst1
            PR.Partial n pst1 -> do
                assert (n <= length (x:getList buf)) (return ())
                let src0 = Prelude.take n (x:getList buf)
                    src  = Prelude.reverse src0 ++ xs
                gobuf SPEC s (List []) (List src) pst1
            PR.Continue 0 pst1 ->
                gobuf SPEC s (List (x:getList buf)) (List xs) pst1
            PR.Continue 1 pst1 ->
                gobuf SPEC s buf (List (x:xs)) pst1
            PR.Continue n pst1 -> do
                assert (n <= length (x:getList buf)) (return ())
                let (src0, buf1) = splitAt n (x:getList buf)
                    src  = Prelude.reverse src0 ++ xs
                gobuf SPEC s (List buf1) (List src) pst1
            PR.Done n b -> do
                assert (n <= length (x:getList buf)) (return ())
                let src0 = Prelude.take n (x:getList buf)
                    src  = Prelude.reverse src0
                return (Right b, Nesting.append (fromList src) (Stream step s))
            PR.Error err -> do
                let src = Prelude.reverse (getList buf) ++ x:xs
                return
                    ( Left (ParseError err)
                    , Nesting.append (fromList src) (Stream step s)
                    )

    -- This is simplified gobuf
    goExtract !_ buf (List []) !pst = goStop SPEC buf pst
    goExtract !_ buf (List (x:xs)) !pst = do
        pRes <- pstep pst x
        case pRes of
            PR.Partial 0 pst1 ->
                goExtract SPEC (List []) (List xs) pst1
            PR.Partial n pst1 -> do
                assert (n <= length (x:getList buf)) (return ())
                let src0 = Prelude.take n (x:getList buf)
                    src  = Prelude.reverse src0 ++ xs
                goExtract SPEC (List []) (List src) pst1
            PR.Continue 0 pst1 ->
                goExtract SPEC (List (x:getList buf)) (List xs) pst1
            PR.Continue 1 pst1 ->
                goExtract SPEC buf (List (x:xs)) pst1
            PR.Continue n pst1 -> do
                assert (n <= length (x:getList buf)) (return ())
                let (src0, buf1) = splitAt n (x:getList buf)
                    src  = Prelude.reverse src0 ++ xs
                goExtract SPEC (List buf1) (List src) pst1
            PR.Done n b -> do
                assert (n <= length (x:getList buf)) (return ())
                let src0 = Prelude.take n (x:getList buf)
                    src  = Prelude.reverse src0
                return (Right b, fromList src)
            PR.Error err -> do
                let src = Prelude.reverse (getList buf) ++ x:xs
                return (Left (ParseError err), fromList src)

    -- This is simplified goExtract
    -- XXX Use SPEC?
    {-# INLINE goStop #-}
    goStop _ buf pst = do
        pRes <- extract pst
        case pRes of
            PR.Partial _ _ -> error "Bug: parseBreak: Partial in extract"
            PR.Continue 0 pst1 -> goStop SPEC buf pst1
            PR.Continue n pst1 -> do
                assert (n <= length (getList buf)) (return ())
                let (src0, buf1) = splitAt n (getList buf)
                    src = Prelude.reverse src0
                goExtract SPEC (List buf1) (List src) pst1
            PR.Done 0 b -> return (Right b, StreamD.nil)
            PR.Done n b -> do
                assert (n <= length (getList buf)) (return ())
                let src0 = Prelude.take n (getList buf)
                    src  = Prelude.reverse src0
                return (Right b, fromList src)
            PR.Error err -> do
                let src  = Prelude.reverse $ getList buf
                return (Left (ParseError err), fromList src)

-- | Parse a stream using the supplied 'Parser'.
--
{-# INLINE parseBreak #-}
parseBreak :: Monad m => PR.Parser a m b -> Stream m a -> m (Either ParseError b, Stream m a)
parseBreak = parseBreakD

------------------------------------------------------------------------------
-- Specialized Folds
------------------------------------------------------------------------------

-- benchmark after dropping 1 item from stream or using unfolds
{-# INLINE_NORMAL null #-}
null :: Monad m => Stream m a -> m Bool
#ifdef USE_FOLDS_EVERYWHERE
null = fold Fold.null
#else
null = foldrM (\_ _ -> return False) (return True)
#endif

{-# INLINE_NORMAL head #-}
head :: Monad m => Stream m a -> m (Maybe a)
#ifdef USE_FOLDS_EVERYWHERE
head = fold Fold.one
#else
head = foldrM (\x _ -> return (Just x)) (return Nothing)
#endif

{-# INLINE_NORMAL headElse #-}
headElse :: Monad m => a -> Stream m a -> m a
headElse a = foldrM (\x _ -> return x) (return a)

{-# INLINE_NORMAL init #-}
init :: Monad m => Stream m a -> m (Maybe (Stream m a))
init stream = do
    r <- uncons stream
    case r of
        Nothing -> return Nothing
        Just (h, Stream step1 state1) ->
            return $ Just $ Stream step (h, state1)

            where

            step gst (a, s1) = do
                res <- step1 (adaptState gst) s1
                return $
                    case res of
                        Yield x s -> Yield a (x, s)
                        Skip s -> Skip (a, s)
                        Stop -> Stop

-- | Same as:
--
-- >>> tail = fmap (fmap snd) . Stream.uncons
--
-- Does not fuse, has the same performance as the StreamK version.
--
{-# INLINE_NORMAL tail #-}
tail :: Monad m => Stream m a -> m (Maybe (Stream m a))
tail = fmap (fmap snd) . uncons
{-
tail (UnStream step state) = go SPEC state
  where
    go !_ st = do
        r <- step defState st
        case r of
            Yield _ s -> return (Just $ Stream step s)
            Skip  s   -> go SPEC s
            Stop      -> return Nothing
-}

-- XXX will it fuse? need custom impl?
{-# INLINE_NORMAL last #-}
last :: Monad m => Stream m a -> m (Maybe a)
#ifdef USE_FOLDS_EVERYWHERE
last = fold Fold.last
#else
last = foldl' (\_ y -> Just y) Nothing
#endif

-- XXX Use the foldrM based impl instead
{-# INLINE_NORMAL elem #-}
elem :: (Monad m, Eq a) => a -> Stream m a -> m Bool
#ifdef USE_FOLDS_EVERYWHERE
elem e = fold (Fold.elem e)
#else
-- elem e m = foldrM (\x xs -> if x == e then return True else xs) (return False) m
elem e (Stream step state) = go SPEC state
  where
    go !_ st = do
        r <- step defState st
        case r of
            Yield x s
              | x == e -> return True
              | otherwise -> go SPEC s
            Skip s -> go SPEC s
            Stop   -> return False
#endif

{-# INLINE_NORMAL notElem #-}
notElem :: (Monad m, Eq a) => a -> Stream m a -> m Bool
notElem e s = fmap not (e `elem` s)

{-# INLINE_NORMAL all #-}
all :: Monad m => (a -> Bool) -> Stream m a -> m Bool
#ifdef USE_FOLDS_EVERYWHERE
all p = fold (Fold.all p)
#else
-- all p m = foldrM (\x xs -> if p x then xs else return False) (return True) m
all p (Stream step state) = go SPEC state
  where
    go !_ st = do
        r <- step defState st
        case r of
            Yield x s
              | p x -> go SPEC s
              | otherwise -> return False
            Skip s -> go SPEC s
            Stop   -> return True
#endif

{-# INLINE_NORMAL any #-}
any :: Monad m => (a -> Bool) -> Stream m a -> m Bool
#ifdef USE_FOLDS_EVERYWHERE
any p = fold (Fold.any p)
#else
-- any p m = foldrM (\x xs -> if p x then return True else xs) (return False) m
any p (Stream step state) = go SPEC state
  where
    go !_ st = do
        r <- step defState st
        case r of
            Yield x s
              | p x -> return True
              | otherwise -> go SPEC s
            Skip s -> go SPEC s
            Stop   -> return False
#endif

{-# INLINE_NORMAL maximum #-}
maximum :: (Monad m, Ord a) => Stream m a -> m (Maybe a)
#ifdef USE_FOLDS_EVERYWHERE
maximum = fold Fold.maximum
#else
maximum (Stream step state) = go SPEC Nothing' state
  where
    go !_ Nothing' st = do
        r <- step defState st
        case r of
            Yield x s -> go SPEC (Just' x) s
            Skip  s   -> go SPEC Nothing' s
            Stop      -> return Nothing
    go !_ (Just' acc) st = do
        r <- step defState st
        case r of
            Yield x s
              | acc <= x  -> go SPEC (Just' x) s
              | otherwise -> go SPEC (Just' acc) s
            Skip s -> go SPEC (Just' acc) s
            Stop   -> return (Just acc)
#endif

{-# INLINE_NORMAL maximumBy #-}
maximumBy :: Monad m => (a -> a -> Ordering) -> Stream m a -> m (Maybe a)
#ifdef USE_FOLDS_EVERYWHERE
maximumBy cmp = fold (Fold.maximumBy cmp)
#else
maximumBy cmp (Stream step state) = go SPEC Nothing' state
  where
    go !_ Nothing' st = do
        r <- step defState st
        case r of
            Yield x s -> go SPEC (Just' x) s
            Skip  s   -> go SPEC Nothing' s
            Stop      -> return Nothing
    go !_ (Just' acc) st = do
        r <- step defState st
        case r of
            Yield x s -> case cmp acc x of
                GT -> go SPEC (Just' acc) s
                _  -> go SPEC (Just' x) s
            Skip s -> go SPEC (Just' acc) s
            Stop   -> return (Just acc)
#endif

{-# INLINE_NORMAL minimum #-}
minimum :: (Monad m, Ord a) => Stream m a -> m (Maybe a)
#ifdef USE_FOLDS_EVERYWHERE
minimum = fold Fold.minimum
#else
minimum (Stream step state) = go SPEC Nothing' state

    where

    go !_ Nothing' st = do
        r <- step defState st
        case r of
            Yield x s -> go SPEC (Just' x) s
            Skip  s   -> go SPEC Nothing' s
            Stop      -> return Nothing
    go !_ (Just' acc) st = do
        r <- step defState st
        case r of
            Yield x s
              | acc <= x  -> go SPEC (Just' acc) s
              | otherwise -> go SPEC (Just' x) s
            Skip s -> go SPEC (Just' acc) s
            Stop   -> return (Just acc)
#endif

{-# INLINE_NORMAL minimumBy #-}
minimumBy :: Monad m => (a -> a -> Ordering) -> Stream m a -> m (Maybe a)
#ifdef USE_FOLDS_EVERYWHERE
minimumBy cmp = fold (Fold.minimumBy cmp)
#else
minimumBy cmp (Stream step state) = go SPEC Nothing' state

    where

    go !_ Nothing' st = do
        r <- step defState st
        case r of
            Yield x s -> go SPEC (Just' x) s
            Skip  s   -> go SPEC Nothing' s
            Stop      -> return Nothing
    go !_ (Just' acc) st = do
        r <- step defState st
        case r of
            Yield x s -> case cmp acc x of
                GT -> go SPEC (Just' x) s
                _  -> go SPEC (Just' acc) s
            Skip s -> go SPEC (Just' acc) s
            Stop   -> return (Just acc)
#endif

{-# INLINE_NORMAL (!!) #-}
(!!) :: (Monad m) => Stream m a -> Int -> m (Maybe a)
#ifdef USE_FOLDS_EVERYWHERE
stream !! i = fold (Fold.index i) stream
#else
(Stream step state) !! i = go SPEC i state

    where

    go !_ !n st = do
        r <- step defState st
        case r of
            Yield x s | n < 0 -> return Nothing
                      | n == 0 -> return $ Just x
                      | otherwise -> go SPEC (n - 1) s
            Skip s -> go SPEC n s
            Stop   -> return Nothing
#endif

{-# INLINE_NORMAL lookup #-}
lookup :: (Monad m, Eq a) => a -> Stream m (a, b) -> m (Maybe b)
#ifdef USE_FOLDS_EVERYWHERE
lookup e = fold (Fold.lookup e)
#else
lookup e = foldrM (\(a, b) xs -> if e == a then return (Just b) else xs)
                   (return Nothing)
#endif

{-# INLINE_NORMAL findM #-}
findM :: Monad m => (a -> m Bool) -> Stream m a -> m (Maybe a)
#ifdef USE_FOLDS_EVERYWHERE
findM p = fold (Fold.findM p)
#else
findM p = foldrM (\x xs -> p x >>= \r -> if r then return (Just x) else xs)
                   (return Nothing)
#endif

{-# INLINE find #-}
find :: Monad m => (a -> Bool) -> Stream m a -> m (Maybe a)
find p = findM (return . p)

{-# INLINE toListRev #-}
toListRev :: Monad m => Stream m a -> m [a]
#ifdef USE_FOLDS_EVERYWHERE
toListRev = fold Fold.toListRev
#else
toListRev = foldl' (flip (:)) []
#endif

------------------------------------------------------------------------------
-- Transformation comprehensions
------------------------------------------------------------------------------

{-# INLINE_NORMAL the #-}
the :: (Eq a, Monad m) => Stream m a -> m (Maybe a)
#ifdef USE_FOLDS_EVERYWHERE
the = fold Fold.the
#else
the (Stream step state) = go SPEC state
  where
    go !_ st = do
        r <- step defState st
        case r of
            Yield x s -> go' SPEC x s
            Skip s    -> go SPEC s
            Stop      -> return Nothing
    go' !_ n st = do
        r <- step defState st
        case r of
            Yield x s | x == n -> go' SPEC n s
                      | otherwise -> return Nothing
            Skip s -> go' SPEC n s
            Stop   -> return (Just n)
#endif

------------------------------------------------------------------------------
-- Map and Fold
------------------------------------------------------------------------------

-- | Execute a monadic action for each element of the 'Stream'
{-# INLINE_NORMAL mapM_ #-}
mapM_ :: Monad m => (a -> m b) -> Stream m a -> m ()
#ifdef USE_FOLDS_EVERYWHERE
mapM_ f = fold (Fold.drainBy f)
#else
mapM_ m = drain . mapM m
#endif

------------------------------------------------------------------------------
-- Multi-stream folds
------------------------------------------------------------------------------

-- | Returns 'True' if the first stream is the same as or a prefix of the
-- second. A stream is a prefix of itself.
--
-- >>> Stream.isPrefixOf (Stream.fromList "hello") (Stream.fromList "hello" :: Stream IO Char)
-- True
--
{-# INLINE_NORMAL isPrefixOf #-}
isPrefixOf :: (Monad m, Eq a) => Stream m a -> Stream m a -> m Bool
isPrefixOf (Stream stepa ta) (Stream stepb tb) = go SPEC Nothing' ta tb

    where

    go !_ Nothing' sa sb = do
        r <- stepa defState sa
        case r of
            Yield x sa' -> go SPEC (Just' x) sa' sb
            Skip sa'    -> go SPEC Nothing' sa' sb
            Stop        -> return True

    go !_ (Just' x) sa sb = do
        r <- stepb defState sb
        case r of
            Yield y sb' ->
                if x == y
                    then go SPEC Nothing' sa sb'
                    else return False
            Skip sb' -> go SPEC (Just' x) sa sb'
            Stop     -> return False

-- | Returns 'True' if all the elements of the first stream occur, in order, in
-- the second stream. The elements do not have to occur consecutively. A stream
-- is a subsequence of itself.
--
-- >>> Stream.isSubsequenceOf (Stream.fromList "hlo") (Stream.fromList "hello" :: Stream IO Char)
-- True
--
{-# INLINE_NORMAL isSubsequenceOf #-}
isSubsequenceOf :: (Monad m, Eq a) => Stream m a -> Stream m a -> m Bool
isSubsequenceOf (Stream stepa ta) (Stream stepb tb) = go SPEC Nothing' ta tb

    where

    go !_ Nothing' sa sb = do
        r <- stepa defState sa
        case r of
            Yield x sa' -> go SPEC (Just' x) sa' sb
            Skip sa' -> go SPEC Nothing' sa' sb
            Stop -> return True

    go !_ (Just' x) sa sb = do
        r <- stepb defState sb
        case r of
            Yield y sb' ->
                if x == y
                    then go SPEC Nothing' sa sb'
                    else go SPEC (Just' x) sa sb'
            Skip sb' -> go SPEC (Just' x) sa sb'
            Stop -> return False

-- | @stripPrefix prefix input@ strips the @prefix@ stream from the @input@
-- stream if it is a prefix of input. Returns 'Nothing' if the input does not
-- start with the given prefix, stripped input otherwise. Returns @Just nil@
-- when the prefix is the same as the input stream.
--
-- Space: @O(1)@
--
{-# INLINE_NORMAL stripPrefix #-}
stripPrefix
    :: (Monad m, Eq a)
    => Stream m a -> Stream m a -> m (Maybe (Stream m a))
stripPrefix (Stream stepa ta) (Stream stepb tb) = go SPEC Nothing' ta tb

    where

    go !_ Nothing' sa sb = do
        r <- stepa defState sa
        case r of
            Yield x sa' -> go SPEC (Just' x) sa' sb
            Skip sa'    -> go SPEC Nothing' sa' sb
            Stop        -> return $ Just (Stream stepb sb)

    go !_ (Just' x) sa sb = do
        r <- stepb defState sb
        case r of
            Yield y sb' ->
                if x == y
                    then go SPEC Nothing' sa sb'
                    else return Nothing
            Skip sb' -> go SPEC (Just' x) sa sb'
            Stop     -> return Nothing

-- | Returns 'True' if the first stream is an infix of the second. A stream is
-- considered an infix of itself.
--
-- >>> s = Stream.fromList "hello" :: Stream IO Char
-- >>> Stream.isInfixOf s s
-- True
--
-- Space: @O(n)@ worst case where @n@ is the length of the infix.
--
-- /Pre-release/
--
-- /Requires 'Storable' constraint/
--
{-# INLINE isInfixOf #-}
isInfixOf :: (MonadIO m, Eq a, Enum a, Unbox a)
    => Stream m a -> Stream m a -> m Bool
isInfixOf infx stream = do
    arr <- fold Array.write infx
    -- XXX can use breakOnSeq instead (when available)
    r <- null $ StreamD.drop 1 $ Nesting.splitOnSeq arr Fold.drain stream
    return (not r)

-- Note: isPrefixOf uses the prefix stream only once. In contrast, isSuffixOf
-- may use the suffix stream many times. To run in optimal memory we do not
-- want to buffer the suffix stream in memory therefore  we need an ability to
-- clone (or consume it multiple times) the suffix stream without any side
-- effects so that multiple potential suffix matches can proceed in parallel
-- without buffering the suffix stream. For example, we may create the suffix
-- stream from a file handle, however, if we evaluate the stream multiple
-- times, once for each match, we will need a different file handle each time
-- which may exhaust the file descriptors. Instead, we want to share the same
-- underlying file descriptor, use pread on it to generate the stream and clone
-- the stream for each match. Therefore the suffix stream should be built in
-- such a way that it can be consumed multiple times without any problems.

-- XXX Can be implemented with better space/time complexity.
-- Space: @O(n)@ worst case where @n@ is the length of the suffix.

-- | Returns 'True' if the first stream is a suffix of the second. A stream is
-- considered a suffix of itself.
--
-- >>> Stream.isSuffixOf (Stream.fromList "hello") (Stream.fromList "hello" :: Stream IO Char)
-- True
--
-- Space: @O(n)@, buffers entire input stream and the suffix.
--
-- /Pre-release/
--
-- /Suboptimal/ - Help wanted.
--
{-# INLINE isSuffixOf #-}
isSuffixOf :: (Monad m, Eq a) => Stream m a -> Stream m a -> m Bool
isSuffixOf suffix stream =
    StreamD.reverse suffix `isPrefixOf` StreamD.reverse stream

-- | Much faster than 'isSuffixOf'.
{-# INLINE isSuffixOfUnbox #-}
isSuffixOfUnbox :: (MonadIO m, Eq a, Unbox a) =>
    Stream m a -> Stream m a -> m Bool
isSuffixOfUnbox suffix stream =
    StreamD.reverseUnbox suffix `isPrefixOf` StreamD.reverseUnbox stream

-- | Drops the given suffix from a stream. Returns 'Nothing' if the stream does
-- not end with the given suffix. Returns @Just nil@ when the suffix is the
-- same as the stream.
--
-- It may be more efficient to convert the stream to an Array and use
-- stripSuffix on that especially if the elements have a Storable or Prim
-- instance.
--
-- Space: @O(n)@, buffers the entire input stream as well as the suffix
--
-- /Pre-release/
{-# INLINE stripSuffix #-}
stripSuffix
    :: (Monad m, Eq a)
    => Stream m a -> Stream m a -> m (Maybe (Stream m a))
stripSuffix m1 m2 =
    fmap StreamD.reverse
        <$> stripPrefix (StreamD.reverse m1) (StreamD.reverse m2)

-- | Much faster than 'stripSuffix'.
{-# INLINE stripSuffixUnbox #-}
stripSuffixUnbox
    :: (MonadIO m, Eq a, Unbox a)
    => Stream m a -> Stream m a -> m (Maybe (Stream m a))
stripSuffixUnbox m1 m2 =
    fmap StreamD.reverseUnbox
        <$> stripPrefix (StreamD.reverseUnbox m1) (StreamD.reverseUnbox m2)
