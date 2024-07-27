{-# LANGUAGE CPP #-}
-- |
-- Module      : Streamly.Internal.Data.Scanl
-- Copyright   : (c) 2024 Composewell Technologies
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Left scans.
--
-- == Scanl vs Fold
--
-- Folds and scans both are consumers of streams. A left scan is a
-- generalization of a fold. While the output of a fold is a singleton value,
-- the output of a scan is a stream. A fold is equivalent to a left scan which
-- produces only the final value in the output stream.
--
-- Like folds, a scan has an internal state. Unlike a fold, a scan produces an
-- output on each input, the output is a function of the scan state and the
-- input.
--
-- A @Scanl m a b@ can represent a @Fold m a b@ by discarding the intermediate
-- outputs and keeping only the final output of the scan.
--
-- == Scanl vs Pipe
--
-- A scan is a simpler version of the consumer side of pipes. A left scan
-- always produces an output whereas a pipe has an additional ability to skip
-- output. Scans are simpler abstractions to think about compared to pipes and
-- easier for the compiler to optimize and fuse.
--
-- == Compositions
--
-- Scans can be chained in the same way as function composition (Category) and
-- can distribute input (tee Applicative). Folds provide an applicative and
-- monad behavior to consume the stream in parts and compose the folded
-- results. Folds are also a special case of parsers.

-- TBD: A scan can produce more than one output on an input, in other words,
-- it can produce output on its own.
--
module Streamly.Internal.Data.Scanl
    (
    -- * Imports
    -- $setup

      module Streamly.Internal.Data.Scanl.Type
    , module Streamly.Internal.Data.Scanl.Combinators
    , module Streamly.Internal.Data.Scanl.Container
    )
where

import Streamly.Internal.Data.Scanl.Combinators
import Streamly.Internal.Data.Scanl.Container
import Streamly.Internal.Data.Scanl.Type

#include "DocTestDataFold.hs"
