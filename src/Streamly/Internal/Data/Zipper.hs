#include "inline.hs"

-- |
-- Module      : Streamly.Internal.Data.Zipper
-- Copyright   : (c) 2020 Composewell Technologies
-- License     : BSD-3-Clause
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

    -- * Updating
    , append

    -- * Checkpointing
    , checkpoint
    , release
    , restore
    )
where

import Control.Exception (assert)
#if !(MIN_VERSION_base(4,13,0))
import Data.Semigroup ((<>))
#endif
import Prelude hiding (splitAt)

import Streamly.Internal.Data.Stream.StreamK.Type (Stream(..))

import qualified Streamly.Internal.Data.Stream.StreamK as K

-------------------------------------------------------------------------------
-- Zipper type
-------------------------------------------------------------------------------

-- | @Zipper checkpoints lefts rights tail@.  The focus is on the first element
-- of @rights@.  @lefts@ is buffered data on the right of the cursor. Note that
-- @lefts@ is stored as a reversed list, this helps is adding more items to the
-- list quickly. @tail@ is a stream that is used to generate more data if the
-- cursor moves past @rights@.
--
-- @checkpoints@ is a stack of checkpoints. A new checkpoint is created by a
-- @checkpoint@ operation. A checkpoint consists of a count that tracks how
-- many elements we have yielded after the checkpoint was created.  We need
-- this information to backtrack to the checkpoint.  If we enter a nested
-- alternative we add another checkpoint in the stack.  When we exit an
-- alternative we call a @release@ on the checkpoint. The @release@ removes the
-- checkpoint from the stack and adds its element count to the previous
-- checkpoint in the stack. When the last checkpoint is removed the buffer is
-- released.
--
-- /Internal/
--
data Zipper m a = Zipper
    [Int]        -- checkpoints
    [a]          -- past buffered inputs (for backtracking)
    [a]          -- future buffered inputs (created by backtracking)
    (Stream m a) -- stream input to use after buffer is over

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

-------------------------------------------------------------------------------
-- Updating
-------------------------------------------------------------------------------

-- | Add an element at the tail of the Zipper.
--
-- /Internal/
--
{-# INLINE append #-}
append :: a -> Zipper m a -> Zipper m a
append x (Zipper checkpoints backward forward stream) =
    Zipper checkpoints backward forward (stream <> K.yield x)

-------------------------------------------------------------------------------
-- Checkpointing
-------------------------------------------------------------------------------

-- | Add a checkpoint to the Zipper so that we do not release the buffer beyond
-- the checkpoint.
--
-- /Internal/
--
{-# INLINE checkpoint #-}
checkpoint :: Zipper m a -> Zipper m a
checkpoint (Zipper cps xs ys stream) = Zipper (0:cps) xs ys stream

-- | Release the latest checkpoint, releases any values held by the checkpoint.
-- Note that the values may still be held by other checkpoints in the stack of
-- checkpoints.
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

-- XXX recheck this, and unify with the definition in StreamD and ParserK
--
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
