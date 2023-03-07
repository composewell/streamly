-- |
-- Module      : Streamly.Data.StreamK
-- Copyright   : (c) 2017 Composewell Technologies
--
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Stability   : released
-- Portability : GHC
--
-- Streams using Continuation Passing Style (CPS). See notes in "Data.Stream"
-- module to know when to use this module.
--
-- 'StreamK' operations have more overhead than 'Stream' operations, however,
-- 'Stream' operations fuse only when the pipeline is statically composed, is
-- not too large, and does not involve recursion. For such cases, a typical
-- example is creating a large or infinite stream by appending, 'StreamK'
-- should be used.
--
-- You can use operations from "Streamly.Data.Stream" for StreamK as well by
-- converting StreamK to Stream, and vice-versa. Most of the time converting to
-- 'Stream' is better than using native StreamK operations, however in certain
-- cases using native StreamK operations may work better. Using 'Stream'
-- operations are especially when multiple contiguous operations can fuse
-- together.
--
-- There are no native exception handling operations in the StreamK module,
-- please use 'toStream' to convert to 'Stream' type and use exception
-- handling operations from "Streamly.Data.Stream".
--
-- See "Streamly.Internal.Data.Stream.StreamK" for more native StreamK
-- operations.

-- primitive/open loop operations that can be used recursively e.g. uncons,
-- foldBreak, parseBreak should not be converted from StreamD for use in
-- StreamK, instead native StreamK impl should be used.
--
-- Closed loop operations like repeat, replicate, iterate etc can be converted
-- from StreamD.
--
-- In the last phase any operation like (toStreamK . f . toStreamD) should be
-- rewritten to a K version of f.
-- XXX Need to add rewrite rules for all missing StreamD operations.
--
module Streamly.Data.StreamK
    (
      StreamK

    -- * Construction
    -- ** Primitives
    , nil
    , nilM
    , cons
    , consM

    -- ** From Values
    , fromPure
    , fromEffect

    -- ** From Stream
    , fromStream
    , toStream

    -- ** From Containers
    , fromFoldable

    -- * Elimination

    -- ** Primitives
    , uncons
    , drain

    -- -- ** Folding
    -- , foldBreak

    -- ** Parsing
    -- , parseBreak
    , parseBreakChunks
    , parseChunks

    -- * Transformation
    , mapM
    , dropWhile
    , take

    -- * Combining Two Streams
    -- ** Appending
    , append

    -- ** Interleaving
    , interleave

    -- ** Merging
    , mergeBy
    , mergeByM

    -- ** Zipping
    , zipWith
    , zipWithM

    -- ** Cross Product
    -- XXX is "bind/concatFor" better to have than crossWith?
    -- crossWith f xs1 xs2 = concatFor xs1 (\x -> fmap (f x) xs2)
    , crossWith
    -- , cross
    -- , joinInner
    -- , CrossStreamK (..)

    -- * Stream of streams
    , concatEffect
    -- , concatMap
    , concatMapWith
    , mergeMapWith

    -- * Buffered Operations
    , reverse
    , sortBy
    )
where

import Streamly.Internal.Data.Stream.StreamK
import Prelude hiding (reverse, zipWith, mapM, dropWhile, take)
