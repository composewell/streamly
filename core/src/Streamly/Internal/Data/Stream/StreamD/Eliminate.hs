-- |
-- Module      : Streamly.Internal.Data.Stream.StreamD.Eliminate
-- Copyright   : (c) 2018 Composewell Technologies
--               (c) Roman Leshchinskiy 2008-2010
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

-- A few functions in this module have been adapted from the vector package
-- (c) Roman Leshchinskiy.
--
module Streamly.Internal.Data.Stream.StreamD.Eliminate
    (
    -- * Running a 'Fold'
      fold

    -- -- * Running a 'Parser'
    , parse
    , parseBreak

    -- * Stream Deconstruction
    , uncons

    -- * Right Folds
    , foldrM
    , foldr
    , foldrMx
    , foldr1

    -- * Left Folds
    , foldlM'
    , foldl'
    , foldlMx'
    , foldlx'

    -- * Specific Fold Functions
    , drain
    , mapM_ -- Map and Fold
    , null
    , head
    , headElse
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
    , toList
    , toListRev

    -- * Multi-Stream Folds
    -- ** Comparisons
    -- | These should probably be expressed using zipping operations.
    , eqBy
    , cmpBy

    -- ** Substreams
    -- | These should probably be expressed using parsers.
    , isPrefixOf
    , isSubsequenceOf
    , stripPrefix
    )
where

#include "inline.hs"

import Control.Exception (assert)
import Control.Monad.Catch (MonadThrow, throwM)
import GHC.Exts (SpecConstrAnnotation(..))
import GHC.Types (SPEC(..))
import Streamly.Internal.Data.Parser (ParseError(..))
import Streamly.Internal.Data.SVar.Type (defState)

import Streamly.Internal.Data.Maybe.Strict (Maybe'(..))
#ifdef USE_FOLDS_EVERYWHERE
import qualified Streamly.Internal.Data.Fold as Fold
#endif
import qualified Streamly.Internal.Data.Parser as PR
import qualified Streamly.Internal.Data.Parser.ParserD as PRD
import qualified Streamly.Internal.Data.Stream.StreamD.Generate as StreamD
import qualified Streamly.Internal.Data.Stream.StreamD.Nesting as Nesting

import Prelude hiding
       ( all, any, elem, foldr, foldr1, head, last, lookup, mapM, mapM_
       , maximum, minimum, notElem, null, splitAt, tail, (!!))
import Streamly.Internal.Data.Stream.StreamD.Type

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
{-# INLINE_NORMAL parse #-}
parse
    :: MonadThrow m
    => PRD.Parser m a b
    -> Stream m a
    -> m b
parse parser strm = do
    (b, _) <- parseBreak parser strm
    return b

-- | Run a 'Parse' over a stream and return rest of the Stream.
{-# INLINE_NORMAL parseBreak #-}
parseBreak
    :: MonadThrow m
    => PRD.Parser m a b
    -> Stream m a
    -> m (b, Stream m a)
parseBreak (PRD.Parser pstep initial extract) stream@(Stream step state) = do
    res <- initial
    case res of
        PRD.IPartial s -> go SPEC state (List []) s
        PRD.IDone b -> return (b, stream)
        PRD.IError err -> throwM $ ParseError err

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
                    PR.Partial n pst1 -> do
                        assert (n <= length (x:getList buf)) (return ())
                        let src0 = Prelude.take n (x:getList buf)
                            src  = Prelude.reverse src0
                        gobuf SPEC s (List []) (List src) pst1
                    PR.Continue 0 pst1 -> go SPEC s (List (x:getList buf)) pst1
                    PR.Continue n pst1 -> do
                        assert (n <= length (x:getList buf)) (return ())
                        let (src0, buf1) = splitAt n (x:getList buf)
                            src  = Prelude.reverse src0
                        gobuf SPEC s (List buf1) (List src) pst1
                    PR.Done 0 b -> return (b, Stream step st)
                    PR.Done n b -> do
                        assert (n <= length (x:getList buf)) (return ())
                        let src0 = Prelude.take n (x:getList buf)
                            src  = Prelude.reverse src0
                        -- XXX This would make it quadratic. We should probably
                        -- use StreamK if we have to append many times.
                        return (b, Nesting.append (fromList src) (Stream step s))
                    PR.Error err -> throwM $ ParseError err
            Skip s -> go SPEC s buf pst
            Stop -> goStop buf pst

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
            PR.Continue n pst1 -> do
                assert (n <= length (x:getList buf)) (return ())
                let (src0, buf1) = splitAt n (x:getList buf)
                    src  = Prelude.reverse src0 ++ xs
                gobuf SPEC s (List buf1) (List src) pst1
            PR.Done n b -> do
                assert (n <= length (x:getList buf)) (return ())
                let src0 = Prelude.take n (x:getList buf)
                    src  = Prelude.reverse src0
                return (b, Nesting.append (fromList src) (Stream step s))
            PR.Error err -> throwM $ ParseError err

    -- This is simplified gobuf
    goExtract !_ buf (List []) !pst = goStop buf pst
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
            PR.Continue n pst1 -> do
                assert (n <= length (x:getList buf)) (return ())
                let (src0, buf1) = splitAt n (x:getList buf)
                    src  = Prelude.reverse src0 ++ xs
                goExtract SPEC (List buf1) (List src) pst1
            PR.Done n b -> do
                assert (n <= length (x:getList buf)) (return ())
                let src0 = Prelude.take n (x:getList buf)
                    src  = Prelude.reverse src0
                return (b, fromList src)
            PR.Error err -> throwM $ ParseError err

    -- This is simplified goExtract
    {-# INLINE goStop #-}
    goStop buf pst = do
        pRes <- extract pst
        case pRes of
            PR.Partial _ _ -> error "Bug: parseBreak: Partial in extract"
            PR.Continue 0 _ ->
                error "parseBreak: extract, Continue 0 creates infinite loop"
            PR.Continue n pst1 -> do
                assert (n <= length (getList buf)) (return ())
                let (src0, buf1) = splitAt n (getList buf)
                    src = Prelude.reverse src0
                goExtract SPEC (List buf1) (List src) pst1
            PR.Done 0 b -> return (b, StreamD.nil)
            PR.Done n b -> do
                assert (n <= length (getList buf)) (return ())
                let src0 = Prelude.take n (getList buf)
                    src  = Prelude.reverse src0
                return (b, fromList src)
            PR.Error err -> throwM $ ParseError err

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
head = fold Fold.next
#else
head = foldrM (\x _ -> return (Just x)) (return Nothing)
#endif

{-# INLINE_NORMAL headElse #-}
headElse :: Monad m => a -> Stream m a -> m a
headElse a = foldrM (\x _ -> return x) (return a)

-- Does not fuse, has the same performance as the StreamK version.
{-# INLINE_NORMAL tail #-}
tail :: Monad m => Stream m a -> m (Maybe (Stream m a))
tail (UnStream step state) = go SPEC state
  where
    go !_ st = do
        r <- step defState st
        case r of
            Yield _ s -> return (Just $ Stream step s)
            Skip  s   -> go SPEC s
            Stop      -> return Nothing

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
notElem e s = fmap not (elem e s)

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

{-# INLINE_NORMAL isPrefixOf #-}
isPrefixOf :: (Eq a, Monad m) => Stream m a -> Stream m a -> m Bool
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

{-# INLINE_NORMAL isSubsequenceOf #-}
isSubsequenceOf :: (Eq a, Monad m) => Stream m a -> Stream m a -> m Bool
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

{-# INLINE_NORMAL stripPrefix #-}
stripPrefix
    :: (Eq a, Monad m)
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
