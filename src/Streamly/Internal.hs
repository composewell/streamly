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
    , intersperseByTime
    , reverse'

    , splitAt -- spanN
    , span  -- spanWhile
    , break -- breakBefore
    , spanBy
    , spanByRolling
    , splitOnSeq
    , splitOnSuffixSeq
    , splitBySeq
    , splitBySuffixSeq

    , tapAsync

    , classifySessionsOf
    , classifySessionsBy
    , classifyKeepAliveSessions

    -- * Streamly.Fold Experimental Exports
    , Fold (..)
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
    )
where

import Prelude hiding (break, span, splitAt)

import Streamly.Streams.Combinators (inspectMode)
import Streamly.Streams.Parallel (tapAsync)

import Streamly.Prelude.Internal
import Streamly.Fold.Types
