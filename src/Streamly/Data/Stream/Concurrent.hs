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
    , defaultConfig

    -- ** Set config
    , maxThreads
    , maxBuffer
    , maxYields
    , inspectMode
    , eagerEval
    , stopWhen
    , ordered

    , rate
    , avgRate
    , minRate
    , maxRate
    , constRate

    , mapM
    , mapMWith
    , sequence
    , sequenceWith

    -- XXX experimental binary ops, move to parallel?
    , append
    , appendWith
    , interleave
    , interleaveWith
    , ahead
    , parallel
    , parallelFst
    , parallelMin

    , apply
    , applyWith
    , concatList
    , concat
    , concatWith
    , concatMap
    , concatMapInterleave
    , concatMapWith
    , concatMapInterleaveWith
    )
where

import Streamly.Internal.Data.Stream.Concurrent
import Streamly.Internal.Data.Stream.Channel.Types
    ( defaultConfig, maxYields )
import Prelude hiding (mapM, sequence, concat, concatMap)
