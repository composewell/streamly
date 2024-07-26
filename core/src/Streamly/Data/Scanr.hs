-- |
-- Module      : Streamly.Data.Scanr
-- Copyright   : (c) 2019 Composewell Technologies
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- 'Scanr' is a right scan. The 'Scanr' type facilitates a stateful
-- transformation from a stream to another stream. In addition to simple
-- transformation of a stream, scans can also be used to split a stream into
-- multiple branches, perform different stateful transformations in each branch
-- and then merge the results back into a single stream.
--
-- A scan can be run on a stream using 'Streamly.Data.Stream.scanr'.
--
-- == Example
--
-- As a contrived example to demonstrate the basic functionality of scans let
-- us compute the value of the expression @x^4 + 3x^2 + 4@ for each number in a
-- stream.
--
-- >>> import qualified Streamly.Internal.Data.Fold as Fold
-- >>> import qualified Streamly.Internal.Data.Scanr as Scanr
-- >>> import qualified Streamly.Internal.Data.Stream as Stream
-- >>> import Data.Function ((&))
--
-- >>> scan1 = Scanr.function (\x -> x * x)
-- >>> scan2 = Scanr.function (\x -> 3 * x)
-- >>> scan3 = Scanr.teeWith (+) scan1 scan2 -- Compute x^2 + 3x
-- >>> scan4 = Scanr.compose scan1 scan3     -- compute x^2 then pass it to scan3
--
-- >>> :{
-- main =
--     Stream.enumerateFromTo 1 3             -- Stream IO Int
--       & Stream.scanr scan4                 -- Stream IO Int
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
-- one input it can produce at most one output, in other words it can shrink or
-- filter a stream but it cannot expand it.
--
-- == Scanr vs Scanl
--
-- 'Scanr' is a right scan, and 'Scanl' is a left scan. 'Scanr' is to 'Stream'
-- as 'Scanl' is to 'Fold'. 'Scanr' is for composing producer stream
-- transformations whereas 'Scanl' is for composing consumer stream
-- transformations.
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
module Streamly.Data.Scanr
    (
    -- * Type
      Scanr

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

import Streamly.Internal.Data.Scanr
import Prelude hiding (filter)
