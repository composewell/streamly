-- |
-- Module      : Streamly.Internal
-- Copyright   : (c) 2018 Harendra Kumar
--
-- License     : BSD3
-- Maintainer  : harendra.kumar@gmail.com
-- Stability   : experimental
-- Portability : GHC
--
-- This module is only for internal use. There is no warranty for the routines
-- in this module to work correctly, please use at your own risk.  The
-- constructors and routines exposed through this module are likely to change
-- or to be removed in future without notice.
--
module Streamly.Internal
    ( Fold (..)
    , inspectMode
    )
where

import Streamly.Fold.Types (Fold(..))
import Streamly.Streams.Combinators (inspectMode)
