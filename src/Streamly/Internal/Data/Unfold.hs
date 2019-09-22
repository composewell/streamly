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
-- Module      : Streamly.Internal.Data.Unfold
-- Copyright   : (c) 2019 Composewell Technologies
-- License     : BSD3
-- Maintainer  : harendra.kumar@gmail.com
-- Stability   : experimental
-- Portability : GHC
--
-- Streams forcing a closed control flow loop can be categorized under
-- two types, unfolds and folds, both of these are duals of each other.
--
-- Unfold streams are really generators of a sequence of elements, we can also
-- call them pull style streams. These are lazy producers of streams. On each
-- evaluation the producer generates the next element.  A consumer can
-- therefore pull elements from the stream whenever it wants to.  A stream
-- consumer can multiplex pull streams by pulling elements from the chosen
-- streams, therefore, pull streams allow merging or multiplexing.  On the
-- other hand, with this representation we cannot split or demultiplex a
-- stream.  So really these are stream sources that can be generated from a
-- seed and can be merged or zipped into a single stream.
--
-- The dual of Unfolds are Folds. Folds can also be called as push style
-- streams or reducers. These are strict consumers of streams. We keep pushing
-- elements to a fold and we can extract the result at any point. A driver can
-- choose which fold to push to and can also push the same element to multiple
-- folds. Therefore, folds allow splitting or demultiplexing a stream. On the
-- other hand, we cannot merge streams using this representation. So really
-- these are stream consumers that reduce the stream to a single value, these
-- consumers can be composed such that a stream can be split over multiple
-- consumers.

-- Open control flow style streams can also have two representations. StreamK
-- is a producer style representation. We can also have a consumer style
-- representation. We can use that for composable folds in StreamK
-- representation.
--
module Streamly.Internal.Data.Unfold
    (
    -- * Unfold Type
    -- |
    -- A 'Fold' can be run over a seed using the 'unfold' combinator.
    --
    -- >>> unfold UF.replicateM (putStrLn "hello") 10

      Unfold
    , unfold

    -- ** Unfolds
    , singleton
    , replicateM
    , fromList
    )
where

import Streamly.Streams.StreamD.Type (Stream(..), Step(..))
#if __GLASGOW_HASKELL__ < 800
import Streamly.Streams.StreamD.Type (pattern Stream)
#endif
import Streamly.Internal.Data.Unfold.Types (Unfold(..))

-------------------------------------------------------------------------------
-- Running unfolds
-------------------------------------------------------------------------------

-- | Convert an 'Unfold' into a 'Stream' by supplying it a seed.
--
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

-- | Identity unfold. Generates a singleton stream with the seed as the only
-- element in the stream.
--
-- > singleton = replicateM 1
--
{-# INLINE singleton #-}
singleton :: Monad m => Unfold m a a
singleton = Unfold step inject
    where
    inject x = return $ Just x
    {-# INLINE_LATE step #-}
    step (Just x) = return $ Yield x Nothing
    step Nothing = return Stop

-- | Generates a stream replicating the seed @n@ times.
--
{-# INLINE replicateM #-}
replicateM :: Monad m => Int -> Unfold m a a
replicateM n = Unfold step inject
    where
    inject x = return (x, n)
    {-# INLINE_LATE step #-}
    step (x, i) = return $
        if i <= 0
        then Stop
        else Yield x (x, (i - 1))

-- | Convert a list of pure values to a 'Stream'
{-# INLINE_LATE fromList #-}
fromList :: Monad m => Unfold m [a] a
fromList = Unfold step inject
  where
    inject x = return x
    {-# INLINE_LATE step #-}
    step (x:xs) = return $ Yield x xs
    step []     = return Stop
