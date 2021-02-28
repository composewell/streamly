-- |
-- Module      : Streamly.Internal.Data.Producer
-- Copyright   : (c) 2021 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- A 'Producer' is an 'Unfold' with an 'extract' function added to extract
-- the state. It is more powerful but less general than an Unfold.
--
-- A 'Producer' represents steps of a loop generating a sequence of elements.
-- While unfolds are closed representation of imperative loops with some opaque
-- internal state, producers are open loops with the state being accessible to
-- the user.
--
-- Unlike an unfold, which runs a loop till completion, a producer can be
-- stopped in the middle, its state can be extracted, examined, changed, and
-- then it can be resumed later from the stopped state.
--
-- A producer can be used in places where a CPS stream would otherwise be
-- needed, because the state of the loop can be passed around. However, it can
-- be much more efficient than CPS because it allows stream fusion and
-- unecessary function calls can be avoided.

module Streamly.Internal.Data.Producer
    ( Producer (..)

    -- * Converting
    , simplify

    -- * Producers
    -- , nil
    -- , nilM
    -- , unfoldrM
    , fromStreamD
    , fromList

    -- * Combinators
    -- , NestedLoop (..)
    -- , concat
    )
where

#include "inline.hs"

import Streamly.Internal.Data.Stream.StreamD.Type (Stream(..))
import Streamly.Internal.Data.SVar (defState)
import Streamly.Internal.Data.Unfold.Types (Unfold(..))

import qualified Streamly.Internal.Data.Stream.StreamD.Step as D

import Streamly.Internal.Data.Producer.Type
import Prelude hiding (concat)

-- XXX We should write unfolds as producers where possible and define
-- unfolds using "simplify".
--
-------------------------------------------------------------------------------
-- Converting to unfolds
-------------------------------------------------------------------------------

data SimplifyState s b = SimplDone | SimplOne b | Simpl s

-- | Simplify a producer to an unfold.
--
-- /Internal/
{-# INLINE simplify #-}
simplify :: Monad m => Producer m a b -> Unfold m a b
simplify (Producer step inject _) = Unfold step1 inject1

    where

    inject1 a = do
        r <- inject a
        return $ case r of
            INil _ -> SimplDone
            IFinal b _ -> SimplOne b
            ISkip s -> Simpl s

    step1 (Simpl st) = do
        res <- step st
        return $ case res of
            Stop -> D.Stop
            Skip s -> D.Skip (Simpl s)
            Nil _ -> D.Stop
            Result b -> D.Yield b SimplDone
            Final b _ -> D.Yield b SimplDone
            Partial b s -> D.Yield b (Simpl s)
    step1 (SimplOne b) = return $ D.Yield b SimplDone
    step1 SimplDone = return D.Stop

-------------------------------------------------------------------------------
-- Unfolds
-------------------------------------------------------------------------------

-- | Convert a StreamD stream into a producer.
--
-- /Internal/
{-# INLINE_NORMAL fromStreamD #-}
fromStreamD :: Monad m => Producer m (Stream m a) a
fromStreamD = Producer step (return . ISkip) return

    where

    {-# INLINE_LATE step #-}
    step (UnStream step1 state1) = do
        r <- step1 defState state1
        return $ case r of
            D.Yield x s -> Partial x (Stream step1 s)
            D.Skip s    -> Skip (Stream step1 s)
            D.Stop      -> Stop
