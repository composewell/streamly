-- |
-- Module      : Streamly.Internal.Data.Producer
-- Copyright   : (c) 2026 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Combinators on stream transition functions of type
-- @s -> m (Step s a)@. These are shared by the @Stream@ and @Unfold@
-- step functions.

module Streamly.Internal.Data.Producer
    (
      mapMaybeM
    , takeWhileM
    )
where

#include "inline.hs"

import Streamly.Internal.Data.Stream.Step (Step(..))

-- | A stream transition: given the current state, produce the next 'Step'.
-- The state type @a@ is also the type carried inside 'Step', so a 'Yield'
-- delivers a new value alongside the updated state.
type Producer m a b = a -> m (Step a b)

{-# INLINE_LATE mapMaybeM #-}
mapMaybeM :: Monad m
    => (b -> m (Maybe c)) -> Producer m s b -> Producer m s c
mapMaybeM f step1 st = do
    r <- step1 st
    case r of
        Yield x s -> do
            b <- f x
            return $ case b of
                Just c  -> Yield c s
                Nothing -> Skip s
        Skip s -> return (Skip s)
        Stop   -> return Stop

{-# INLINE_LATE takeWhileM #-}
takeWhileM :: Monad m
    => (b -> m Bool) -> Producer m s b -> Producer m s b
takeWhileM f step1 st = do
    r <- step1 st
    case r of
        Yield x s -> do
            b <- f x
            return $ if b then Yield x s else Stop
        Skip s -> return (Skip s)
        Stop   -> return Stop
