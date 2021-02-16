-- |
-- Module      : Streamly.Internal.Data.Unfold.Source
-- Copyright   : (c) 2021 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- A stream source which can be unfolded with some possibly buffered data.
-- Allows to 'unread' data i.e. push unused data back to the source. This is
-- useful in parsing applications with backtracking.
--

module Streamly.Internal.Data.Unfold.Source
    ( Source

    -- * Creation
    , source

    -- * Transformation
    , unread

    -- * Consumption
    , isEmpty
    , read
    )
where

#include "inline.hs"

import Streamly.Internal.Data.Stream.StreamD.Step (Step(..))
import Streamly.Internal.Data.Unfold.Resume.Type (Unfold(..))
import Prelude hiding (read)

-- | An unfold seed with some buffered data. It allows us to 'unread' or return
-- some data after reading it. Useful in backtracked parsing.
--
data Source a b = Source [b] (Maybe a)

-- | Make a source from a seed value. The buffer would start as empty. You can
-- use 'unread' to add to the buffer.
--
-- /Internal/
source :: Maybe a -> Source a b
source = Source []


-- | Return some unused data back to the source. The data is prepended (or
-- consed) to the source.
--
-- /Internal/
unread :: [b] -> Source a b -> Source a b
unread xs (Source ys seed) = Source (xs ++ ys) seed
isEmpty :: Source a b -> Bool
isEmpty (Source [] Nothing) = True
isEmpty _ = False

-- | An unfold to read from a 'Source'. If the buffer has data then it is read
-- first and then the seed is unfolded using the supplied unfold.
--
-- /Internal/
{-# INLINE_NORMAL read #-}
read :: Monad m => Unfold m a b -> Unfold m (Source a b) b
read (Unfold step1 inject1 extract1) = Unfold step inject extract

    where

    inject (Source [] (Just a)) = do
        s <- inject1 a
        return $ Left s
    inject (Source xs a) = return $ Right (xs, a)

    {-# INLINE_LATE step #-}
    step (Left s) = do
        r <- step1 s
        return $ case r of
            Yield x s1 -> Yield x (Left s1)
            Skip s1 -> Skip (Left s1)
            Stop -> Stop
    step (Right ([], Nothing)) = return Stop
    step (Right ([], Just _)) = error "Bug: unreachable"
    step (Right (x:[], Just a)) = do
        s <- inject1 a
        return $ Yield x (Left s)
    step (Right (x:xs, a)) = return $ Yield x (Right (xs, a))

    extract (Left s) = Source [] . Just <$> extract1 s
    extract (Right (xs, a)) = return $ Source xs a
