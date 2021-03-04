-- |
-- Module      : Streamly.Internal.Data.Cont
-- Copyright   : (c) 2020 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Continuation style utilities.
--
module Streamly.Internal.Data.Cont
    ( contListMap
    )
where

import Control.Monad.Cont (runCont, cont)

-- | Given a continuation based transformation from @a@ to @b@ and a
-- continuation based transformation from @[b]@ to @c@, make continuation based
-- transformation from @[a]@ to @c@.
--
-- /Pre-release/

-- You can read the definition as:
--
-- > contListMap f g = \xs final ->
--
contListMap ::
       (a -> (b -> r) -> r)      -- transform a -> b
    -> ([b] -> (c -> r) -> r)    -- transform [b] -> c
    -> ([a] -> (c -> r) -> r)    -- transform [a] -> c
contListMap f g xs final =
    let bconts = fmap (cont . f) xs  -- [Cont b]
        blistCont = sequence bconts  -- Cont [b]
        k ys = g ys final            -- [b] -> r
     in runCont blistCont k          -- r
