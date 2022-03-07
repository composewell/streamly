-- |
-- Module      : Streamly.Internal.Data.Sink.Type
-- Copyright   : (c) 2019 Composewell Technologies
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

module Streamly.Internal.Data.Sink.Type
    (
      Sink (..)
    )
where

------------------------------------------------------------------------------
-- Sink
------------------------------------------------------------------------------

-- | A 'Sink' is a special type of 'Fold' that does not accumulate any value,
-- but runs only effects. A 'Sink' has no state to maintain therefore can be a
-- bit more efficient than a 'Fold' with '()' as the state, especially when
-- 'Sink's are composed with other operations. A Sink can be upgraded to a
-- 'Fold', but a 'Fold' cannot be converted into a Sink.
newtype Sink m a = Sink (a -> m ())
