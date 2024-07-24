{-# LANGUAGE CPP #-}
-- |
-- Module      : Streamly.Internal.Data.Scanl
-- Copyright   : (c) 2024 Composewell Technologies
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--

module Streamly.Internal.Data.Scanl
    (
    -- * Imports
    -- $setup

      module Streamly.Internal.Data.Scanl.Type
    , module Streamly.Internal.Data.Scanl.Combinators
    -- , module Streamly.Internal.Data.Fold.Container
    )
where

import Streamly.Internal.Data.Scanl.Combinators
-- import Streamly.Internal.Data.Fold.Container
import Streamly.Internal.Data.Scanl.Type

#include "DocTestDataFold.hs"
