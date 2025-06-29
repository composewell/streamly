{-# LANGUAGE CPP #-}
-- |
-- Module      : Streamly.Data.StreamK
-- Copyright   : (c) 2017 Composewell Technologies
--
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Stability   : released
-- Portability : GHC
--
-- Streams represented as chains of function calls using Continuation Passing
-- Style (CPS), suitable for dynamically and recursively composing potentially
-- large number of streams. The 'K' in 'StreamK' stands for Kontinuation.
--
-- Unlike the statically fused operations in "Streamly.Data.Stream", StreamK
-- operations are less efficient, involving a function call overhead for each
-- element, but they exhibit linear O(n) time complexity wrt to the number of
-- stream compositions. Therefore, they are suitable for dynamically composing
-- streams e.g. appending potentially infinite streams in recursive loops.
-- While fused streams can be used efficiently to process elements as small as
-- a single byte, CPS streams are typically used on bigger chunks of data to
-- avoid the larger overhead per element.
--
-- In addition to the combinators in this module, you can use operations from
-- "Streamly.Data.Stream" for StreamK as well by converting StreamK to Stream
-- ('toStream'), and vice-versa ('fromStream'). Please refer to
-- "Streamly.Internal.Data.StreamK" for more functions that have not yet been
-- released.
--
-- For documentation see the corresponding combinators in
-- "Streamly.Data.Stream". Documentation has been omitted in this module unless
-- there is a difference worth mentioning or if the combinator does not exist
-- in "Streamly.Data.Stream".
--
-- = Overview
--
-- StreamK can be constructed like lists, except that they use 'nil' instead of
-- '[]' and 'cons' instead of ':'.
--
-- >>> import Streamly.Data.StreamK (StreamK, cons, consM, nil)
--
-- `cons` constructs a stream from pure values:
--
-- >>> stream = 1 `cons` 2 `cons` nil :: StreamK IO Int
--
-- Operations from "Streamly.Data.Stream" can be used for StreamK as well by
-- converting StreamK to Stream ('toStream'), and vice-versa ('fromStream').
--
-- >>> Stream.fold Fold.toList $ StreamK.toStream stream -- IO [Int]
-- [1,2]
--
-- Stream can also be constructed from effects not just pure values:
--
-- >>> effect n = print n >> return n
-- >>> stream = effect 1 `consM` effect 2 `consM` nil
-- >>> Stream.fold Fold.toList $ StreamK.toStream stream
-- 1
-- 2
-- [1,2]

-- Notes:
--
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
    -- * Setup
    -- | To execute the code examples provided in this module in ghci, please
    -- run the following commands first.
    --
    -- $setup

    -- * Type
      StreamK

    -- * CrossStreamK
    , CrossStreamK
    , unCross
    , mkCross

    -- * Construction
    -- ** Primitives
    -- | Primitives to construct a stream from pure values or monadic actions.
    -- All other stream construction and generation combinators described later
    -- can be expressed in terms of these primitives. However, the special
    -- versions provided in this module can be much more efficient in some
    -- cases. Users can create custom combinators using these primitives.
    , nil
    , nilM
    , cons
    , consM

    -- ** From Values
    , fromPure
    , fromEffect

    -- ** From Stream
    -- | Please note that 'Stream' type does not observe any exceptions from
    -- the consumer of the stream whereas 'StreamK' does.
    , fromStream
    , toStream

    -- ** From Containers
    , fromFoldable

    -- ** To Containers
    , toList

    -- * Elimination

    -- ** Primitives
    , uncons
    , drain

    -- -- ** Folding
    -- , foldBreak

    -- ** Parsing
    , parserK
    , parse
    , parseBreak
    , parsePos
    , parseBreakPos

    -- * Transformation
    , mapM
    , dropWhile
    , take
    , filter

    -- * Combining Two Streams
    -- | Unlike the operations in "Streamly.Data.Stream", these operations can
    -- be used to dynamically compose large number of streams e.g. using the
    -- 'concatMapWith' and 'mergeMapWith' operations. They have a linear O(n)
    -- time complexity wrt to the number of streams being composed.

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
    -- | Some useful idioms:
    --
    -- >>> concatFoldableWith f = Prelude.foldr f StreamK.nil
    -- >>> concatMapFoldableWith f g = Prelude.foldr (f . g) StreamK.nil
    -- >>> concatForFoldableWith f xs g = Prelude.foldr (f . g) StreamK.nil xs
    --
    , concatEffect
    , concatMap
    , bindWith
    , concatMapWith
    , mergeMapWith

    -- * Buffered Operations
    , reverse
    , sortBy

    -- * Exceptions
    -- | Please note that 'Stream' type does not observe any exceptions from
    -- the consumer of the stream whereas 'StreamK' does.
    , handle

    -- * Resource Management
    -- | Please note that 'Stream' type does not observe any exceptions from
    -- the consumer of the stream whereas 'StreamK' does.
    , bracketIO

    -- * Deprecated
    , parseBreakChunks
    , parseChunks
    )
where

import Streamly.Internal.Data.StreamK
import Prelude hiding
    (reverse, zipWith, mapM, dropWhile, take, filter, concatMap)

#include "DocTestDataStreamK.hs"
