-- |
-- Module      : Streamly.Internal.Data.Stream
-- Copyright   : (c) 2018 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Direct style re-implementation of CPS stream in
-- "Streamly.Internal.Data.StreamK".  The symbol or suffix 'D' in this
-- module denotes the "Direct" style.  GHC is able to INLINE and fuse direct
-- style better, providing better performance than CPS implementation.
--
-- @
-- import qualified Streamly.Internal.Data.Stream as D
-- @

module Streamly.Internal.Data.Stream
    (
      module Streamly.Internal.Data.Stream.Type
    , module Streamly.Internal.Data.Stream.Generate
    , module Streamly.Internal.Data.Stream.Eliminate
    , module Streamly.Internal.Data.Stream.Exception
    , module Streamly.Internal.Data.Stream.Lift
    , module Streamly.Internal.Data.Stream.Transformer
    , module Streamly.Internal.Data.Stream.Nesting
    , module Streamly.Internal.Data.Stream.Transform
    , module Streamly.Internal.Data.Stream.Top
    , module Streamly.Internal.Data.Stream.Container
    )
where

import Streamly.Internal.Data.Stream.Type
import Streamly.Internal.Data.Stream.Generate
import Streamly.Internal.Data.Stream.Eliminate
import Streamly.Internal.Data.Stream.Exception
import Streamly.Internal.Data.Stream.Lift
import Streamly.Internal.Data.Stream.Transformer
import Streamly.Internal.Data.Stream.Nesting
import Streamly.Internal.Data.Stream.Transform
import Streamly.Internal.Data.Stream.Top
import Streamly.Internal.Data.Stream.Container
