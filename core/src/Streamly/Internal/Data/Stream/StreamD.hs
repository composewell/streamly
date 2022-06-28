-- |
-- Module      : Streamly.Internal.Data.Stream.StreamD
-- Copyright   : (c) 2018 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Direct style re-implementation of CPS stream in
-- "Streamly.Internal.Data.Stream.StreamK".  The symbol or suffix 'D' in this
-- module denotes the "Direct" style.  GHC is able to INLINE and fuse direct
-- style better, providing better performance than CPS implementation.
--
-- @
-- import qualified Streamly.Internal.Data.Stream.StreamD as D
-- @

module Streamly.Internal.Data.Stream.StreamD
    (
      module Streamly.Internal.Data.StreamD.Type
    , module Streamly.Internal.Data.StreamD.Generate
    , module Streamly.Internal.Data.StreamD.Eliminate
    , module Streamly.Internal.Data.StreamD.Exception
    , module Streamly.Internal.Data.StreamD.Lift
    , module Streamly.Internal.Data.StreamD.Nesting
    , module Streamly.Internal.Data.StreamD.Transform
    )
where

import Streamly.Internal.Data.StreamD.Type
import Streamly.Internal.Data.StreamD.Generate
import Streamly.Internal.Data.StreamD.Eliminate
import Streamly.Internal.Data.StreamD.Exception
import Streamly.Internal.Data.StreamD.Lift
import Streamly.Internal.Data.StreamD.Nesting
import Streamly.Internal.Data.StreamD.Transform
