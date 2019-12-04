-- |
-- Module      : Streamly.Data.SmallArray
-- Copyright   : (c) 2019 Composewell Technologies
--
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

module Streamly.Data.SmallArray
  ( SmallArray

  -- * Construction
  , A.fromListN
  , A.fromStreamN

  , A.writeN

  -- * Elimination
  , A.toStream
  , A.toStreamRev
  , A.read

  -- * Folding Arrays
  , A.streamFold
  , A.fold

  , A.length
  )
where

import Streamly.Internal.Data.SmallArray (SmallArray)

import qualified Streamly.Internal.Data.SmallArray as A
