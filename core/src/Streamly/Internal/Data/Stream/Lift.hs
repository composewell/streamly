-- |
-- Module      : Streamly.Internal.Data.Stream.Lift
-- Copyright   : (c) 2019 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

module Streamly.Internal.Data.Stream.Lift
    (
    -- * Generalize Inner Monad
      hoist
    , generally
    )
where

import Data.Functor.Identity (Identity (..))
import Streamly.Internal.Data.Stream.Type (Stream, fromStreamK, toStreamK)

import qualified Streamly.Internal.Data.Stream.StreamK as K

-- $setup
-- >>> :m
-- >>> import Streamly.Internal.Data.Stream as Stream

------------------------------------------------------------------------------
-- Generalize the underlying monad
------------------------------------------------------------------------------

-- | Transform the inner monad of a stream using a natural transformation.
--
-- / Internal/
--
-- /CPS/
{-# INLINE hoist #-}
hoist :: (Monad m, Monad n)
    => (forall x. m x -> n x) -> Stream m a -> Stream n a
hoist f xs = fromStreamK $ K.hoist f (toStreamK xs)

-- | Generalize the inner monad of the stream from 'Identity' to any monad.
--
-- / Internal/
--
-- /CPS/
--
{-# INLINE generally #-}
generally :: Monad m => Stream Identity a -> Stream m a
generally xs = fromStreamK $ K.hoist (return . runIdentity) (toStreamK xs)
