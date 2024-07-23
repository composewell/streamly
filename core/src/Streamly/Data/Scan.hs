-- |
-- Module      : Streamly.Data.Scan
-- Copyright   : (c) 2019 Composewell Technologies
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- The 'Scan' type represents a stateful transformation from a stream to
-- another stream. In general, using scans, we can split a stream into
-- multiple streams and perform different stateful computations in each branch
-- and then merge the results back into a single stream.
--
-- You can run a scan on a stream using 'Streamly.Data.Stream.runScan'.
--
-- == Example
--
-- As a contrived example to demonstrate the basic functionality of scans let
-- us compute the value of the expression @x^4 + 3x^2 + 4@ for each number in a
-- stream.
--
-- >>> scan1 = Scan.map (\x -> x * x)
-- >>> scan2 = Scan.map (\x -> 3 * x)
-- >>> scan3 = Scan.teeWith (+) scan1 scan2 -- Compute x^2 + 3x
-- >>> scan4 = Scan.compose scan1 scan3     -- compute x^2 then pass it to scan3
--
-- >>> :{
-- main =
--     Stream.enumerateFromTo 1 3             -- Stream IO Int
--       & Stream.runScan scan4               -- Stream IO Int
--       & fmap (+4)                          -- Stream IO Int
--       & Stream.fold (Fold.drainMapM print) -- IO ()
-- :}
--
-- @scan3@ splits the computation into two parts, one part computes @x^2@ using
-- @scan1@ and the other part computes @3x@ using @scan2@ and then it zips the
-- two parts back into a single stream by summing them. @scan4@ first passes an
-- element through @scan1@ thus squaring it and then it passes the result
-- through @scan3@, the final result is @x^4 + 3x^2@. Then we add 4 in the
-- resulting stream and print each element in the stream.
--
-- == Stateful Transformations
--
-- A scan represents a stateful transformation of a stream. It is a consumer as
-- well as a producer of streams. When it consumes an input, it uses its
-- internal state and the input to produce an output. It may skip an output, on
-- one input it can produce at most one output.
--
-- == Scans vs Folds
--
-- Folds and scans both are consumers of streams. While the output of scans is
-- a stream, the output of folds is a singleton value. A fold is equivalent to
-- a scan which produces only the final value in the output stream.
--
-- Folds provide an applicative and monad behavior to consume the stream in
-- parts and compose the folded results. Scans provide category like
-- composition and stream zip applicative behavior.
--
-- == Compositions
--
-- Scans can be composed in the following ways:
--
-- @append@: The output of a scan is supplied as input to the next scan.
--
-- @demux@: For each input one scan out of many is chosen based on the input,
-- the outputs of the scans are merged in a single stream.
--
-- @distribute@: Each input is supplied to all scans and the outputs are zipped
-- or merged.
--
module Streamly.Data.Scan
    (
    -- * Type
      Scan

    -- * Primitive Scans
    , identity
    , function
    , functionM
    , filter
    , filterM

    -- * Combinators
    , compose
    , teeWithMay
    , teeWith
    )
where

import Streamly.Internal.Data.Scan
import Prelude hiding (filter)
