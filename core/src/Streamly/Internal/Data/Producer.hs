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
    (
      module Streamly.Internal.Data.Producer.Source
    , module Streamly.Internal.Data.Producer.Type

    -- * Converting
    , simplify
    , fromStreamD
    )
where

#include "inline.hs"

import Streamly.Internal.Data.Stream.Step (Step(..))
import Streamly.Internal.Data.Stream.Type (Stream(..))
import Streamly.Internal.Data.SVar.Type (defState)
import Streamly.Internal.Data.Unfold.Type (Unfold(..))

import Streamly.Internal.Data.Producer.Source
import Streamly.Internal.Data.Producer.Type
import Prelude hiding (concat)

-- XXX We should write unfolds as producers where possible and define
-- unfolds using "simplify".
--
-------------------------------------------------------------------------------
-- Converting to unfolds
-------------------------------------------------------------------------------

-- | Simplify a producer to an unfold.
--
-- /Pre-release/
{-# INLINE simplify #-}
simplify :: Producer m a b -> Unfold m a b
simplify (Producer step inject _) = Unfold step inject

-------------------------------------------------------------------------------
-- Unfolds
-------------------------------------------------------------------------------

-- | Convert a StreamD stream into a producer.
--
-- /Pre-release/
{-# INLINE_NORMAL fromStreamD #-}
fromStreamD :: Monad m => Producer m (Stream m a) a
fromStreamD = Producer step return return

    where

    {-# INLINE_LATE step #-}
    step (UnStream step1 state1) = do
        r <- step1 defState state1
        return $ case r of
            Yield x s -> Yield x (Stream step1 s)
            Skip s    -> Skip (Stream step1 s)
            Stop      -> Stop
