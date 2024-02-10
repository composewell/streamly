-- |
-- Module      : Streamly.Internal.Data.Pipe
-- Copyright   : (c) 2019 Composewell Technologies
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- There are three fundamental types that make up a stream pipeline:
--
-- * Stream: sources
-- * Scan: transformations
-- * Fold: sinks
--
-- Streams are sources or producers of values, multiple sources can be merged
-- into a single source but a source cannot be split into multiple stream
-- sources.  Folds are sinks or consumers, a stream can be split and
-- distributed to multiple folds but the results cannot be merged back into a
-- stream source again. Scans are simple one-to-one transformations with
-- filtering. One element cannot be transformed to multiple elements.
--
-- The Pipe type is a super type of all the above, it is the most complex type.
-- All of these can be represented by a pipe. A pipe can act as a source or a
-- sink or a transformation, dynamically. A stream source can be split and
-- distributed to multiple pipes each pipe can apply its own transform on the
-- stream and the results can be merged back into a single pipe. Pipes can be
-- attached to a source to produce a source or they can be attached to a fold
-- to produce a fold, or multiple pipes can be merged or zipped into a single
-- pipe.
--
-- > import qualified Streamly.Internal.Data.Pipe as Pipe

module Streamly.Internal.Data.Pipe
    (
      module Streamly.Internal.Data.Pipe.Type
    )
where

import Streamly.Internal.Data.Pipe.Type
