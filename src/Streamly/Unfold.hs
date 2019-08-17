{-# LANGUAGE BangPatterns              #-}
{-# LANGUAGE CPP                       #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE PatternSynonyms           #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

#include "inline.hs"

-- |
-- Module      : Streamly.Unfold
-- Copyright   : (c) 2019 Composewell Technologies
-- License     : BSD3
-- Maintainer  : harendra.kumar@gmail.com
-- Stability   : experimental
-- Portability : GHC
--

module Streamly.Unfold
    (
    -- * Unfold Type
    -- |
    -- A 'Fold' can be run over a seed using the 'unfold' combinator.
    --
    -- >>> unfold UF.replicateM (putStrLn "hello") 10

      Unfold
    , unfold

    -- ** Unfolds
    , replicateM
    )
where

import Streamly.Streams.StreamD.Type (Stream(..), Step(..))
#if __GLASGOW_HASKELL__ < 800
import Streamly.Streams.StreamD.Type (pattern Stream)
#endif
import Streamly.Unfold.Types (Unfold(..))

-------------------------------------------------------------------------------
-- Running unfolds
-------------------------------------------------------------------------------

{-# INLINE_NORMAL unfold #-}
unfold :: Monad m => a -> Unfold m a b -> Stream m b
unfold seed (Unfold ustep inject) = Stream step Nothing
  where
    {-# INLINE_LATE step #-}
    step _ Nothing = inject seed >>= return . Skip . Just
    step _ (Just st) = do
        r <- ustep st
        return $ case r of
            Yield x s -> Yield x (Just s)
            Skip s    -> Skip (Just s)
            Stop      -> Stop

-------------------------------------------------------------------------------
-- Unfolds
-------------------------------------------------------------------------------

{-# INLINE replicateM #-}
replicateM :: Monad m => Int -> Unfold m a a
replicateM n = Unfold step inject
    where
    inject x = return (x, n)
    step (x, i) = return $
        if i <= 0
        then Stop
        else Yield x (x, (i - 1))
