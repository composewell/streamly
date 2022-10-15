-- |
-- Module      : Streamly.Data.Stream.Concurrent
-- Copyright   : (c) 2022 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : released
-- Portability : GHC

module Streamly.Data.Stream.Concurrent
    (
      MonadAsync
    , Config

    -- ** Set config
    , maxThreads
    , maxBuffer
    , inspect
    , eager
    , stopWhen
    , ordered
    , interleaved

    , rate
    , avgRate
    , minRate
    , maxRate
    , constRate

    , mapM
    , mapMWith
    , sequence
    , sequenceWith

    , append
    , ahead
    , parallel
    , combineWith

    , apply
    , applyWith

    , concatList
    , concat
    , concatWith
    , concatMap
    , concatMapWith
    )
where

import Streamly.Internal.Data.Stream.Concurrent
import Prelude hiding (mapM, sequence, concat, concatMap)
