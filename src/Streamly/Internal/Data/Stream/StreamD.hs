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
      module Streamly.Internal.Data.Stream.StreamD.Type
    , module Streamly.Internal.Data.Stream.StreamD.Generate
    , module Streamly.Internal.Data.Stream.StreamD.Eliminate
    , module Streamly.Internal.Data.Stream.StreamD.Exception
    , module Streamly.Internal.Data.Stream.StreamD.Lift
    , module Streamly.Internal.Data.Stream.StreamD.Nesting
    , module Streamly.Internal.Data.Stream.StreamD.Transform
    )
where

import Streamly.Internal.Data.Stream.StreamD.Type
import Streamly.Internal.Data.Stream.StreamD.Generate
import Streamly.Internal.Data.Stream.StreamD.Eliminate
import Streamly.Internal.Data.Stream.StreamD.Exception
import Streamly.Internal.Data.Stream.StreamD.Lift
import Streamly.Internal.Data.Stream.StreamD.Nesting
import Streamly.Internal.Data.Stream.StreamD.Transform
