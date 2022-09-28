-- |
-- Module      : Streamly.Internal.Data.Stream.Serial
-- Copyright   : (c) 2017 Composewell Technologies
--
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- To run examples in this module:
--
-- >>> import qualified Streamly.Data.Stream as Stream
--
module Streamly.Internal.Data.Stream.Serial
    (
    -- * Serial appending stream
      SerialT
    , Serial
    , serial

    -- * Construction
    , Stream.cons
    , Stream.consM
    , Stream.repeat
    , Stream.unfoldrM
    , fromList

    -- * Elimination
    , toList

    -- * Transformation
    , map
    , Stream.mapM
    )
where

import GHC.Exts (IsList(..))
import Streamly.Data.Stream (Stream)

import qualified Streamly.Data.Stream as Stream

import Prelude hiding (map)

-- $setup
-- >>> import qualified Streamly.Data.Stream as Stream

------------------------------------------------------------------------------
-- SerialT
------------------------------------------------------------------------------

type SerialT = Stream

-- | A serial IO stream of elements of type @a@. See 'SerialT' documentation
-- for more details.
--
-- /Since: 0.2.0 ("Streamly")/
--
-- @since 0.8.0
type Serial = SerialT IO

------------------------------------------------------------------------------
-- Combining
------------------------------------------------------------------------------

{-# INLINE serial #-}
serial :: SerialT m a -> SerialT m a -> SerialT m a
serial = (<>)

-- |
-- @
-- map = fmap
-- @
--
-- Same as 'fmap'.
--
-- @
-- > S.toList $ S.map (+1) $ S.fromList [1,2,3]
-- [2,3,4]
-- @
--
-- @since 0.4.0
{-# INLINE map #-}
map :: Monad m => (a -> b) -> SerialT m a -> SerialT m b
map f = Stream.mapM (return . f)
