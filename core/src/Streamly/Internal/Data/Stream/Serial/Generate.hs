module Streamly.Internal.Data.Stream.Serial.Generate
    (
      cons
    , consM
    , repeat
    , serial
    , unfoldrM
    )
where

#if __GLASGOW_HASKELL__ < 808
import Data.Semigroup (Semigroup(..))
#endif
#if __GLASGOW_HASKELL__ < 808
import Data.Semigroup (Semigroup(..))
#endif

import qualified Streamly.Internal.Data.Stream.StreamD.Type as D
import qualified Streamly.Internal.Data.Stream.StreamD.Generate as D
import qualified Streamly.Internal.Data.Stream.StreamK.Type as K

import Prelude hiding (map, mapM, repeat, filter)

import Streamly.Internal.Data.Stream.Serial.Type

#include "inline.hs"

{-# INLINE cons #-}
cons :: a -> SerialT m a -> SerialT m a
cons x (SerialT ms) = SerialT $ K.cons x ms

{-# INLINE consM #-}
{-# SPECIALIZE consM :: IO a -> SerialT IO a -> SerialT IO a #-}
consM :: Monad m => m a -> SerialT m a -> SerialT m a
consM m (SerialT ms) = SerialT $ K.consM m ms

-- |
-- Generate an infinite stream by repeating a pure value.
--
{-# INLINE_NORMAL repeat #-}
repeat :: Monad m => a -> SerialT m a
repeat = SerialT . D.toStreamK . D.repeat

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
unfoldrM step seed = SerialT $ D.toStreamK (D.unfoldrM step seed)
