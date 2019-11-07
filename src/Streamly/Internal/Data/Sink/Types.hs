{-# OPTIONS_HADDOCK hide #-}

-- |
-- Module      : Streamly.Internal.Data.Sink.Types
-- Copyright   : (c) 2019 Composewell Technologies
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

module Streamly.Internal.Data.Sink.Types
    (
      Sink (..)
    )
where

------------------------------------------------------------------------------
-- Sink
------------------------------------------------------------------------------

-- | A 'Sink' is a special type of 'Foldl' that does not accumulate any value,
-- but runs only effects. A 'Sink' has no state to maintain therefore can be a
-- bit more efficient than a 'Foldl' with '()' as the state, especially when
-- 'Sink's are composed with other operations. A Sink can be upgraded to a
-- 'Foldl', but a 'Foldl' cannot be converted into a Sink.
data Sink m a = Sink (a -> m ())
