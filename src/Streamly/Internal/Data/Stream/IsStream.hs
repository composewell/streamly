-- |
-- Module      : Streamly.Internal.Data.Stream.IsStream
-- Copyright   : (c) 2017 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- This is an internal module which is a superset of the corresponding released
-- module "Streamly.Prelude". It contains some additional unreleased or
-- experimental APIs.

module Streamly.Internal.Data.Stream.IsStream
    ( module Streamly.Internal.Data.Stream.IsStream.Types
    , module Streamly.Internal.Data.Stream.IsStream.Generate
    , module Streamly.Internal.Data.Stream.IsStream.Eliminate
    , module Streamly.Internal.Data.Stream.IsStream.Transform
    , module Streamly.Internal.Data.Stream.IsStream.Expand
    , module Streamly.Internal.Data.Stream.IsStream.Reduce
    , module Streamly.Internal.Data.Stream.IsStream.Exception
    , module Streamly.Internal.Data.Stream.IsStream.Lift
    , module Streamly.Internal.Data.Stream.IsStream.Top
    )
where

import Streamly.Internal.Data.Stream.IsStream.Top
import Streamly.Internal.Data.Stream.IsStream.Eliminate
import Streamly.Internal.Data.Stream.IsStream.Exception
import Streamly.Internal.Data.Stream.IsStream.Generate
import Streamly.Internal.Data.Stream.IsStream.Lift
import Streamly.Internal.Data.Stream.IsStream.Expand
import Streamly.Internal.Data.Stream.IsStream.Reduce
import Streamly.Internal.Data.Stream.IsStream.Transform
import Streamly.Internal.Data.Stream.IsStream.Types
