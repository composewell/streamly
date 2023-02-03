-- |
-- Module      : Streamly.Internal.Data.Stream.StreamDK
-- Copyright   : (c) 2019 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
--
-- This module has the following problems due to rewrite rules:
--
-- * Rewrite rules lead to optimization problems, blocking fusion in some
-- cases, specifically when combining multiple operations e.g. (filter . drop).
-- * Rewrite rules lead to problems when calling a function recursively. For
-- example, the StreamD version of foldBreak cannot be used recursively when
-- wrapped in rewrite rules because each recursive call adds a roundtrip
-- conversion from D to K and back to D. We can use the StreamK versions of
-- these though because the rewrite rule gets eliminated in that case.
-- * If we have a unified module, we need two different versions of several
-- operations e.g. appendK and appendD, both are useful in different cases.
--
module Streamly.Internal.Data.Stream.StreamDK
    ( module Streamly.Internal.Data.Stream.Type
    , module Streamly.Internal.Data.Stream.Bottom
    , module Streamly.Internal.Data.Stream.Eliminate
    , module Streamly.Internal.Data.Stream.Exception
    , module Streamly.Internal.Data.Stream.Expand
    , module Streamly.Internal.Data.Stream.Generate
    , module Streamly.Internal.Data.Stream.Lift
    , module Streamly.Internal.Data.Stream.Reduce
    , module Streamly.Internal.Data.Stream.Transform
    , module Streamly.Internal.Data.Stream.Cross
    , module Streamly.Internal.Data.Stream.Zip

    -- modules having dependencies on libraries other than base
    , module Streamly.Internal.Data.Stream.Transformer
    , module Streamly.Internal.Data.Stream.Container
    )
where

import Streamly.Internal.Data.Stream.Bottom
import Streamly.Internal.Data.Stream.Cross
import Streamly.Internal.Data.Stream.Eliminate
import Streamly.Internal.Data.Stream.Exception
import Streamly.Internal.Data.Stream.Expand
import Streamly.Internal.Data.Stream.Generate
import Streamly.Internal.Data.Stream.Lift
import Streamly.Internal.Data.Stream.Reduce
import Streamly.Internal.Data.Stream.Transform
import Streamly.Internal.Data.Stream.Type
import Streamly.Internal.Data.Stream.Zip

import Streamly.Internal.Data.Stream.Container
import Streamly.Internal.Data.Stream.Transformer
