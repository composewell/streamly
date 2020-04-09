{-# LANGUAGE CPP #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

#include "inline.hs"

-- |
-- Module      : Streamly.Internal.Data.Zipper
-- Copyright   : (c) 2020 Composewell Technologies
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- A special zipper derived from a stream  to facilitate stream parsing.
--
-- = Zipper
--
-- The zipper is designed to enable moving backward and forward in the stream
-- efficiently to be able to implement backtracking parsers. The zipper can
-- buffer data until the parser indicates that it is not needed, we can move
-- back in the buffered data if the parser needs to backtrack.
--
-- = Checkpointing
--
-- We have a checkpointing mechanism built into the zipper to handle
-- backtracking. Whenever we enter a computation that can fail and has an
-- alternative to run then we add a checkpoint to the zipper. The checkpoint
-- ensures that we do not release the buffer upto the checkpoint until this
-- computation succeeds, if the computation fails we can backtrack to focus the
-- cursor back to the checkpoint position.
--
-- = Future Work
--
-- We can possibly have a typeclass providing a zipper interface so that we can
-- derive a zipper from different data sources efficiently. For example,
--
-- * stream zipper
-- * zippers from different types of arrays
-- * zipper from a tree of arrays for dynamic in-memory storage
-- * file handle zipper can utilize the seek to go back and forth in the file
--
-- A tree of zippers where a lower level zipper is dependent on (references
-- data in) a higher level zipper could be useful in implementing distributive
-- parsers.
--
-- See https://hackage.haskell.org/package/zipper for an existing type class.
-- We need a monadic one.

module Streamly.Internal.Data.Zipper
    (
      Zipper (..)

    -- * Construction
    , nil
    , fromStream
    , fromList

    -- * Checkpointing
    , checkpoint
    , release
    , restore

    -- * Parsing
    , parse
    )
where

import Control.Exception (assert, Exception(..))
import Control.Monad.Catch (MonadCatch, try, SomeException)
-- import GHC.Types (SPEC(..))
import Prelude hiding (splitAt)

import Streamly.Internal.Data.Stream.StreamK.Type (Stream(..))
import Streamly.Internal.Data.SVar (defState)

import qualified Streamly.Internal.Data.Parser.Types as PR
import qualified Streamly.Internal.Data.Stream.StreamK as K

-------------------------------------------------------------------------------
-- Zipper type
-------------------------------------------------------------------------------

-- | @Zipper checkpoints lefts rights tail@.  The focus is on the first element
-- of @rights@.  @lefts@ is buffered data on the right of the cursor.  @tail@
-- is a stream that is used to generate more data if the cursor moves past
-- @rights@.
--
-- @checkpoints@ is a stack of checkpoints. A new checkpoint is created by a
-- @checkpoint@ operation. A checkpoint consists of a count that tracks how
-- many elements we have yielded after the checkpoint was taken.  We need this
-- information to backtrack to the checkpoint.  If we enter a nested
-- alternative we add another checkpoint in the stack.  When we exit an
-- alternative we call a @release@ on the checkpoint. The @release@ removes the
-- checkpoint from the stack and adds its element count to the previous
-- checkpoint in the stack. When the last checkpoint is removed the buffer is
-- released.
--
-- /Internal/
--
data Zipper m a = Zipper [Int] [a] [a] (Stream m a)

-------------------------------------------------------------------------------
-- Construction
-------------------------------------------------------------------------------

-- | Empty zipper.
--
-- /Internal/
--
{-# INLINE nil #-}
nil :: Zipper m a
nil = Zipper [] [] [] K.nil

-- | Create a zipper from a stream.
--
-- /Internal/
--
{-# INLINE fromStream #-}
fromStream :: Stream m a -> Zipper m a
fromStream = Zipper [] [] []

-- | Create a zipper from a list.
--
-- /Internal/
--
{-# INLINE fromList #-}
fromList :: [a] -> Zipper m a
fromList xs = Zipper [] [] xs K.nil

-- | Add a checkpoint to the Zipper so that we do not release the buffer beyond
-- the checkpoint.
--
-- /Internal/
--
{-# INLINE checkpoint #-}
checkpoint :: Zipper m a -> Zipper m a
checkpoint (Zipper cps xs ys stream) = Zipper (0:cps) xs ys stream

-- | Release the latest checkpoint, releases any values held by the checkpoint.
--
-- /Internal/
--
{-# INLINE release #-}
release :: Zipper m a -> Zipper m a
release (Zipper [] _ _ _) = error "Bug: release, no checkpoint exists!"
release (Zipper (n:cps) xs ys stream) =
    assert (n <= length xs) $
            case cps of
                [] -> assert (n == length xs) $ Zipper [] [] ys stream
                cp:rest -> Zipper ((cp + n) : rest) xs ys stream

-- | Rewind to restore the cursor to the latest checkpoint.
--
-- /Internal/
--
{-# INLINE restore #-}
restore :: Zipper m a -> Zipper m a
restore (Zipper [] _ _ _) = error "Bug: restore, no checkpoint exists!"
restore (Zipper (n:cps) xs ys stream) =
    assert (n <= length xs) $
        let (src0, buf1) = splitAt n xs
            src  = Prelude.reverse src0
         in Zipper cps buf1 (src ++ ys) stream

-------------------------------------------------------------------------------
-- Parse driver for a Zipper
-------------------------------------------------------------------------------

-- Inlined definition. Without the inline "serially/parser/take" benchmark
-- degrades and splitParse does not fuse. Even using "inline" at the callsite
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

-- | Parse a stream zipper using a parser's @step@, @initial@ and @extract@
-- functions.
--
{-# INLINE_NORMAL parse #-}
parse
    :: MonadCatch m
    => (s -> a -> m (PR.Step s b))
    -> m s
    -> (s -> m b)
    -> Zipper m a
    -> m (Zipper m a, Either String b)

-- The case when no checkpoints exist
parse pstep initial extract (Zipper [] ls rs stream) =
    case rs of
        [] -> go stream ls initial
        _ -> gobuf stream ls rs initial

    where

    {-# INLINE go #-}
    go st buf !acc =
        let stop = do
                pst <- acc
                r <- try $ extract pst
                return $ case r of
                    Left (e :: SomeException) ->
                        (nil, Left (displayException e))
                    Right b -> (nil, Right b)
            single x = do
                acc1 <- acc >>= \b -> pstep b x
                case acc1 of
                    -- PR.Yield 0 pst1 -> go SPEC s [] pst1
                    PR.Yield n pst1 -> do
                        assert (n <= length (x:buf)) (return ())
                        go K.nil (Prelude.take n (x:buf)) (return pst1)
                    PR.Skip 0 pst1 -> go K.nil (x:buf) (return pst1)
                    PR.Skip n pst1 -> do
                        assert (n <= length (x:buf)) (return ())
                        let (src0, buf1) = splitAt n (x:buf)
                            src  = Prelude.reverse src0
                        gobuf K.nil buf1 src (return pst1)
                    PR.Stop n b -> do
                        assert (n <= length (x:buf)) (return ())
                        let (src0, buf1) = splitAt n (x:buf)
                            src  = Prelude.reverse src0
                        return (Zipper [] buf1 src K.nil, Right b)
                    PR.Error err ->
                        return (Zipper [] (x:buf) [] K.nil, Left err)
            yieldk x r = do
                acc1 <- acc >>= \b -> pstep b x
                case acc1 of
                    -- PR.Yield 0 pst1 -> go SPEC s [] pst1
                    PR.Yield n pst1 -> do
                        assert (n <= length (x:buf)) (return ())
                        go r (Prelude.take n (x:buf)) (return pst1)
                    PR.Skip 0 pst1 -> go r (x:buf) (return pst1)
                    PR.Skip n pst1 -> do
                        assert (n <= length (x:buf)) (return ())
                        let (src0, buf1) = splitAt n (x:buf)
                            src  = Prelude.reverse src0
                        gobuf r buf1 src (return pst1)
                    PR.Stop n b -> do
                        assert (n <= length (x:buf)) (return ())
                        let (src0, buf1) = splitAt n (x:buf)
                            src  = Prelude.reverse src0
                        return (Zipper [] buf1 src r, Right b)
                    PR.Error err ->
                        return (Zipper [] (x:buf) [] r, Left err)
         in K.foldStream defState yieldk single stop st

    gobuf s buf [] !pst = go s buf pst
    gobuf s buf (x:xs) !pst = do
        r <- pst
        pRes <- pstep r x
        case pRes of
            -- PR.Yield 0 pst1 -> go SPEC s [] pst1
            PR.Yield n pst1 -> do
                assert (n <= length (x:buf)) (return ())
                gobuf s (Prelude.take n (x:buf)) xs (return pst1)
            PR.Skip 0 pst1 -> gobuf s (x:buf) xs (return pst1)
            PR.Skip n pst1 -> do
                assert (n <= length (x:buf)) (return ())
                let (src0, buf1) = splitAt n (x:buf)
                    src  = Prelude.reverse src0 ++ xs
                gobuf s buf1 src (return pst1)
            PR.Stop n b -> do
                assert (n <= length (x:buf)) (return ())
                let (src0, buf1) = splitAt n (x:buf)
                    src  = Prelude.reverse src0 ++ xs
                return (Zipper [] buf1 src s, Right b)
            PR.Error err ->
                return (Zipper [] (x:buf) xs s, Left err)

-- The case when checkpoints exist
-- XXX code duplication alert!
parse pstep initial extract (Zipper (cp:cps) ls rs stream) =
    case rs of
        [] -> go 0 stream ls initial
        _ -> gobuf 0 stream ls rs initial

    where

    {-# INLINE go #-}
    go cnt st buf !acc =
        let stop = do
                pst <- acc
                r <- try $ extract pst
                return $ case r of
                    Left (e :: SomeException) ->
                        (nil, Left (displayException e))
                    Right b -> (nil, Right b)
            single x = do
                acc1 <- acc >>= \b -> pstep b x
                let cnt1 = cnt + 1
                case acc1 of
                    -- PR.Yield 0 pst1 -> go SPEC s [] pst1
                    PR.Yield n pst1 -> do
                        assert (n <= length (x:buf)) (return ())
                        go cnt1 K.nil (x:buf) (return pst1)
                    PR.Skip 0 pst1 -> go cnt1 K.nil (x:buf) (return pst1)
                    PR.Skip n pst1 -> do
                        assert (n <= length (x:buf)) (return ())
                        let (src0, buf1) = splitAt n (x:buf)
                            src  = Prelude.reverse src0
                        assert (cnt1 - n >= 0) (return ())
                        gobuf (cnt1 - n) K.nil buf1 src (return pst1)
                    PR.Stop n b -> do
                        assert (n <= length (x:buf)) (return ())
                        let (src0, buf1) = splitAt n (x:buf)
                            src  = Prelude.reverse src0
                        assert (cp + cnt1 - n >= 0) (return ())
                        return ( Zipper (cp + cnt1 - n : cps) buf1 src K.nil
                               , Right b
                               )
                    PR.Error err ->
                        return ( Zipper (cp + cnt1 : cps) (x:buf) [] K.nil
                               , Left err
                               )
            yieldk x r = do
                acc1 <- acc >>= \b -> pstep b x
                let cnt1 = cnt + 1
                case acc1 of
                    -- PR.Yield 0 pst1 -> go SPEC s [] pst1
                    PR.Yield n pst1 -> do
                        assert (n <= length (x:buf)) (return ())
                        go cnt1 r (x:buf) (return pst1)
                    PR.Skip 0 pst1 -> go cnt1 r (x:buf) (return pst1)
                    PR.Skip n pst1 -> do
                        assert (n <= length (x:buf)) (return ())
                        let (src0, buf1) = splitAt n (x:buf)
                            src  = Prelude.reverse src0
                        assert (cnt1 - n >= 0) (return ())
                        gobuf (cnt1 - n) r buf1 src (return pst1)
                    PR.Stop n b -> do
                        assert (n <= length (x:buf)) (return ())
                        let (src0, buf1) = splitAt n (x:buf)
                            src  = Prelude.reverse src0
                        assert (cp + cnt1 - n >= 0) (return ())
                        return ( Zipper (cp + cnt1 - n : cps) buf1 src r
                               , Right b
                               )
                    PR.Error err ->
                        return ( Zipper (cp + cnt1 : cps) (x:buf) [] r
                               , Left err
                               )
         in K.foldStream defState yieldk single stop st

    gobuf cnt s buf [] !pst = go cnt s buf pst
    gobuf cnt s buf (x:xs) !pst = do
        r <- pst
        pRes <- pstep r x
        let cnt1 = cnt + 1
        case pRes of
            -- PR.Yield 0 pst1 -> go SPEC s [] pst1
            PR.Yield n pst1 -> do
                assert (n <= length (x:buf)) (return ())
                gobuf cnt1 s (x:buf) xs (return pst1)
            PR.Skip 0 pst1 -> gobuf cnt1 s (x:buf) xs (return pst1)
            PR.Skip n pst1 -> do
                assert (n <= length (x:buf)) (return ())
                let (src0, buf1) = splitAt n (x:buf)
                    src  = Prelude.reverse src0 ++ xs
                assert (cnt1 - n >= 0) (return ())
                gobuf (cnt1 - n) s buf1 src (return pst1)
            PR.Stop n b -> do
                assert (n <= length (x:buf)) (return ())
                let (src0, buf1) = splitAt n (x:buf)
                    src  = Prelude.reverse src0 ++ xs
                assert (cp + cnt1 - n >= 0) (return ())
                return (Zipper (cp + cnt1 - n : cps) buf1 src s, Right b)
            PR.Error err ->
                return (Zipper (cp + cnt1 : cps) (x:buf) xs s, Left err)
