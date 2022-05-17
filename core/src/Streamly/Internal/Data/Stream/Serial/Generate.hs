module Streamly.Internal.Data.Stream.Serial.Generate
    (
      repeat
    , replicate
    , replicateM
    , serial
    , unfold
    , unfoldrM
    , fromIndices
    , fromIndicesM
    , iterate
    , iterateM
    , fromFoldable
    , fromFoldableM
    , mfix
    )
where

#if __GLASGOW_HASKELL__ < 808
import Data.Semigroup (Semigroup(..))
#endif
#if __GLASGOW_HASKELL__ < 808
import Data.Semigroup (Semigroup(..))
#endif

import qualified Streamly.Internal.Data.Stream.StreamD.Type as D
import qualified Streamly.Internal.Data.Stream.StreamD.Generate as DG
import qualified Streamly.Internal.Data.Stream.StreamK.Type as K

import Prelude hiding (iterate, map, mapM, repeat, filter, replicate)

import Streamly.Internal.Data.Stream.Serial.Type
import Streamly.Internal.Data.Unfold.Type (Unfold)

#include "inline.hs"

-- |
-- Generate an infinite stream by repeating a pure value.
--
{-# INLINE_NORMAL repeat #-}
repeat :: Monad m => a -> SerialT m a
repeat = SerialT . D.toStreamK . DG.repeat

------------------------------------------------------------------------------
-- Combining
------------------------------------------------------------------------------

{-# INLINE serial #-}
serial :: SerialT m a -> SerialT m a -> SerialT m a
serial = (<>)

-- | Build a stream by unfolding a /monadic/ step function starting from a
-- seed.  The step function returns the next element in the stream and the next
-- seed value. When it is done it returns 'Nothing' and the stream ends. For
-- example,
--
-- @
-- let f b =
--         if b > 3
--         then return Nothing
--         else print b >> return (Just (b, b + 1))
-- in drain $ unfoldrM f 0
-- @
-- @
--  0
--  1
--  2
--  3
-- @
--
-- /Pre-release/
--
{-# INLINE unfoldrM #-}
unfoldrM :: Monad m => (b -> m (Maybe (a, b))) -> b -> SerialT m a
unfoldrM step seed = SerialT $ D.toStreamK (DG.unfoldrM step seed)

{-# INLINE unfold #-}
unfold :: Monad m => Unfold m a b -> a -> SerialT m b
unfold unf x = SerialT $ D.toStreamK $ D.unfold unf x

{-# INLINE_NORMAL replicate #-}
replicate :: Monad m => Int -> a -> SerialT m a
replicate n = SerialT . D.toStreamK . DG.replicate n

{-# INLINE_EARLY replicateM #-}
replicateM :: Monad m => Int -> m a -> SerialT m a
replicateM count = SerialT . D.toStreamK . DG.replicateM count

{-# INLINE fromIndices #-}
fromIndices :: Monad m => (Int -> a) -> SerialT m a
fromIndices =  SerialT . D.toStreamK . DG.fromIndices

{-# INLINE_EARLY fromIndicesM #-}
fromIndicesM ::  Monad m => (Int -> m a) -> SerialT m a
fromIndicesM = SerialT . D.toStreamK . DG.fromIndicesM

{-# INLINE_NORMAL iterate #-}
iterate :: Monad m => (a -> a) -> a -> SerialT m a
iterate step = SerialT . D.toStreamK . DG.iterate step

{-# INLINE_NORMAL iterateM #-}
iterateM :: Monad m => (a -> m a) -> m a -> SerialT m a
iterateM step = SerialT . D.toStreamK . DG.iterateM step

{-# INLINE mfix #-}
mfix :: Monad m => (m a -> SerialT m a) -> SerialT m a
mfix f = SerialT $ K.mfix (getSerialT . f)

{-# INLINE fromFoldable #-}
fromFoldable :: Foldable f => f a -> SerialT m a
fromFoldable = SerialT . K.fromFoldable

{-# INLINE fromFoldableM #-}
fromFoldableM :: (Monad m, Foldable f) => f (m a) -> SerialT m a
fromFoldableM = Prelude.foldr consM nil
