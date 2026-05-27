-- |
-- Module      : Streamly.Internal.Data.Transition
-- Copyright   : (c) 2026 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Combinators on stream transition functions of type
-- @s -> m (Step s a)@. These are shared by the @Stream@ and @Unfold@
-- step functions.

module Streamly.Internal.Data.Transition
    (
      mapMaybeM
    )
where

#include "inline.hs"

import Streamly.Internal.Data.Stream.Step (Step(..))

{-# INLINE_LATE mapMaybeM #-}
mapMaybeM :: Monad m
    => (b -> m (Maybe c)) -> (s -> m (Step s b)) -> (s -> m (Step s c))
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
