-- |
-- Module      : Streamly.Internal.Control.Exception
-- Copyright   : (c) 2019 Composewell Technologies
--
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Additional "Control.Exception" utilities.

module Streamly.Internal.Control.Exception
    ( assertM
    , verify
    , verifyM
    )
where

import Control.Exception (assert)

-- Like 'assert' but returns @()@ in an 'Applicative' context so that it can be
-- used as an independent statement in a @do@ block.
--
-- /Pre-release/
--
{-# INLINE assertM #-}
assertM :: Applicative f => Bool -> f ()
assertM predicate = assert predicate (pure ())

-- | Like 'assert' but is not removed by the compiler, it is always present in
-- production code.
--
-- /Pre-release/
--
{-# INLINE verify #-}
verify :: Bool -> a -> a
verify predicate val =
    if predicate
    -- XXX it would be nice if we can print the predicate expr.
    then error "verify failed"
    else val

-- Like 'verify' but returns @()@ in an 'Applicative' context so that it can be
-- used as an independent statement in a @do@ block.
--
-- /Pre-release/
--
{-# INLINE verifyM #-}
verifyM :: Applicative f => Bool -> f ()
verifyM predicate = verify predicate (pure ())
