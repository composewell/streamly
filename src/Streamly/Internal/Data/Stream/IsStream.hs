{-# OPTIONS_GHC -Wno-deprecations -Wno-orphans #-}

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

module Streamly.Internal.Data.Stream.IsStream  {-# DEPRECATED "Please use \"Streamly.Internal.Data.Stream from streamly-core package\", \"Streamly.Internal.Data.Stream.Concurrent\", \"Streamly.Internal.Data.Stream.Exception.Lifted\", & \"Streamly.Internal.Data.Stream.Time\" from streamly package instead." #-}
    ( module Streamly.Internal.Data.Stream.IsStream.Type
    , module Streamly.Internal.Data.Stream.IsStream.Generate
    , module Streamly.Internal.Data.Stream.IsStream.Eliminate
    , module Streamly.Internal.Data.Stream.IsStream.Transform
    , module Streamly.Internal.Data.Stream.IsStream.Expand
    , module Streamly.Internal.Data.Stream.IsStream.Reduce
    , module Streamly.Internal.Data.Stream.IsStream.Exception
    , module Streamly.Internal.Data.Stream.IsStream.Lift
    , module Streamly.Internal.Data.Stream.IsStream.Top
    , module Streamly.Internal.Data.Stream.IsStream.Combinators
    , module Streamly.Internal.Data.Stream.IsStream.Common
    , module Streamly.Internal.Data.Stream.IsStream.Enumeration
    , fromStream
    , toStream
    )
where

import Streamly.Internal.Data.Stream.IsStream.Top
import Streamly.Internal.Data.Stream.IsStream.Eliminate hiding (toStream)
import Streamly.Internal.Data.Stream.IsStream.Exception
import Streamly.Internal.Data.Stream.IsStream.Generate
import Streamly.Internal.Data.Stream.IsStream.Lift
import Streamly.Internal.Data.Stream.IsStream.Expand
import Streamly.Internal.Data.Stream.IsStream.Reduce
import Streamly.Internal.Data.Stream.IsStream.Transform
import Streamly.Internal.Data.Stream.IsStream.Type
    hiding (cmpBy, drain, eqBy, foldl', fold, toList, toStream
        , fromEffect, fromPure, repeat, fromStream)
import Streamly.Internal.Data.Stream.IsStream.Combinators
import Streamly.Internal.Data.Stream.IsStream.Common
import Streamly.Internal.Data.Stream.IsStream.Enumeration

import qualified Streamly.Internal.Data.Stream as D

{-# INLINE fromStream #-}
fromStream :: (IsStream t, Monad m) => D.Stream m a -> t m a
fromStream = fromStreamD

{-# INLINE toStream #-}
toStream :: (IsStream t, Monad m) => t m a -> D.Stream m a
toStream = toStreamD
