-- |
-- Module      : Streamly.Internal.Data.Stream.StreamD.Lift
-- Copyright   : (c) 2018 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Transform the underlying monad of a stream.

module Streamly.Internal.Data.Stream.StreamD.Lift
    (
    -- * Generalize Inner Monad
      hoist
    , generally -- XXX generalize
    )
where

#include "inline.hs"

import Data.Functor.Identity (Identity(..))
import Streamly.Internal.Data.SVar.Type (adaptState)

import Streamly.Internal.Data.Stream.StreamD.Type

-------------------------------------------------------------------------------
-- Generalize Inner Monad
-------------------------------------------------------------------------------

{-# INLINE_NORMAL hoist #-}
hoist :: Monad n => (forall x. m x -> n x) -> Stream m a -> Stream n a
hoist f (Stream step state) = Stream step' state
    where
    {-# INLINE_LATE step' #-}
    step' gst st = do
        r <- f $ step (adaptState gst) st
        return $ case r of
            Yield x s -> Yield x s
            Skip  s   -> Skip s
            Stop      -> Stop

{-# INLINE generally #-}
generally :: Monad m => Stream Identity a -> Stream m a
generally = hoist (return . runIdentity)
