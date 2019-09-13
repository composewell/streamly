-- |
-- Module      : Streamly.Internal
-- Copyright   : (c) 2018 Harendra Kumar
--
-- License     : BSD3
-- Maintainer  : harendra.kumar@gmail.com
-- Stability   : experimental
-- Portability : GHC
--
-- This module is only for internal or experimental APIs.  The constructors and
-- routines exposed through this module are likely to change or to be removed
-- at any time in future without notice.  There is no warranty for the routines
-- in this module to work correctly, please use at your own risk.
--
-- If you think some of the routines exported through this module should be
-- included in the officially supported modules please raise an issue at
-- https://github.com/composewell/streamly/issues .
--
module Streamly.Internal
    (
    -- * Diagnostics
      inspectMode

    -- * Streamly.Prelude Experimental Exports
    , foldrS
    , foldrT
    , prescanl'
    , prescanlM'

    , toListRev
    , toListRevF
    , toStream
    , toStreamRev

    , insertAfterEach
    , interject
    , reverse'

    , splitAt -- spanN
    , span  -- spanWhile
    , break -- breakBefore
    , spanBy
    , spanByRolling
    , arraysOf
    , splitOnSeq
    , splitOnSuffixSeq
    , splitBySeq
    , splitWithSuffixSeq
    , splitInnerBy

    , tapAsync

    , classifySessionsOf
    , classifySessionsBy
    , classifyKeepAliveSessions

    , append
    , interleave
    , interleaveFst
    , interleaveMin
    , roundrobin
    , wSerialFst
    , wSerialMin
    , parallelFst
    , parallelMin

    , concatMapU
    , concatUnfoldInterleave
    , concatUnfoldRoundrobin
    , intercalateSuffix

    -- * Streamly.Fold Experimental Exports
    , Fold (..)
    , rollingHash
    , rollingHashWithSalt
    , lmap
    , lmapM
    , lfilter
    , lfilterM
    , lcatMaybes
    , ltake
    , ltakeWhile
    , lsessionsOf
    , lchunksOf
    , duplicate
    , initialize
    , runStep

    -- * Streamly.Unfold Experimental Exports
    , Unfold (..)

    -- * Streamly.Memory.Array
    , readU
    )
where

import Prelude hiding (break, span, splitAt)

import Streamly.Memory.Array.Types (readU)
import Streamly.Streams.Combinators (inspectMode)
import Streamly.Streams.Parallel (tapAsync, parallelFst, parallelMin)
import Streamly.Streams.Serial (wSerialFst, wSerialMin)
import Streamly.Unfold.Types (Unfold(..))

import Streamly.Fold.Internal
import Streamly.Fold.Types
import Streamly.Prelude.Internal
