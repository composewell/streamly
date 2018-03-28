-- |
-- Module      : Streamly
-- Copyright   : (c) 2017 Harendra Kumar
--
-- License     : BSD3
-- Maintainer  : harendra.kumar@gmail.com
-- Stability   : experimental
-- Portability : GHC

module Streamly
    (
    -- * Background
    -- $background

    -- * Overview
    -- $overview

      MonadAsync
    , Streaming

    -- * Product Style Composition
    -- $product
    , StreamT
    , ReverseT
    , InterleavedT
    , AsyncT
    , ParallelT

    -- * Zip Style Composition
    -- $zipping
    , ZipStream
    , ZipAsync

    -- * Sum Style Composition
    -- $sum
    , (<=>)
    , (<|)

    -- * Transformation
    , async

    -- * Stream Type Adapters
    -- $adapters
    , serially
    , reversely
    , interleaving
    , asyncly
    , parallely
    , zipping
    , zippingAsync
    , adapt

    -- * Running Streams
    , runStreaming
    , runStreamT
    , runInterleavedT
    , runAsyncT
    , runParallelT
    , runZipStream
    , runZipAsync

    -- * Fold Utilities
    -- $foldutils
    , foldWith
    , foldMapWith
    , forEachWith

    -- * Re-exports
    , Monoid (..)
    , Semigroup (..)
    , Alternative (..)
    , MonadPlus (..)
    , MonadIO (..)
    , MonadTrans (..)
    )
where

import Streamly.Streams
import Data.Semigroup (Semigroup(..))
import Control.Applicative (Alternative(..))
import Control.Monad (MonadPlus(..))
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Class (MonadTrans (..))

-- $background
--
-- Streamly provides a monad transformer that extends the product style
-- composition of monads to streams of many elements of the same type; it is a
-- functional programming equivalent of nested loops from imperative
-- programming. Composing each element in one stream with each element in the
-- other stream generalizes the monadic product of single elements. You can
-- think of the IO monad as a special case of the more general @StreamT IO@
-- monad; with single element streams.  List transformers and logic programming
-- monads also provide a similar product style composition of streams, however
-- streamly generalizes it with the time dimension; allowing streams to be
-- composed in an asynchronous and concurrent fashion in many different ways.
-- It also provides multiple alternative ways of composing streams e.g.
-- serial, interleaved or concurrent.
--
-- The seemingly simple addition of asynchronicity and concurrency to product
-- style streaming composition unifies a number of disparate abstractions into
-- one powerful and elegant abstraction.  A wide variety of programming
-- problems can be solved elegantly with this abstraction. In particular, it
-- unifies three major programming domains namely non-deterministic (logic)
-- programming, concurrent programming and functional reactive programming. In
-- other words, you can do everything with this one abstraction that you could
-- with list transformers (e.g.
-- <https://hackage.haskell.org/package/list-t list-t>), logic programming
-- monads (e.g.  <https://hackage.haskell.org/package/logict logict>),
-- streaming libraries (a lot of what
-- <https://hackage.haskell.org/package/conduit conduit> or
-- <https://hackage.haskell.org/package/pipes pipes> can do), concurrency
-- libraries (e.g. <https://hackage.haskell.org/package/async async>) and FRP
-- libraries (e.g. <https://hackage.haskell.org/package/Yampa Yampa> or
-- <https://hackage.haskell.org/package/reflex reflex>).

-- $overview
--
-- Streamly provides six distinct stream types i.e. 'StreamT', 'InterleavedT',
-- 'AsyncT' and 'ParallelT', 'ZipStream' and 'ZipAsync', each representing a
-- stream of elements.  All these types have the same underlying representation
-- and can be adapted from one to another using type adaptor combinators
-- described later. Each of these types belongs to the 'Streaming' type class
-- which helps converting the specific type to and from the underlying generic
-- stream type.
--
-- The types 'StreamT', 'InterleavedT', 'AsyncT' and 'ParallelT' are 'Monad'
-- transformers with the monadic bind operation combining streams in a product
-- style in much the same way as a list monad or a list transformer i.e. each
-- element from one stream is combined with every element of the other stream.
-- However, the applicative and monadic composition of these types differ in
-- terms of the ordering and time sequence in which the elements from two
-- streams are combined. 'StreamT' and 'InterleavedT' compose streams serially
-- whereas 'AsyncT' and 'ParallelT' are their concurrent counterparts. See the
-- documentation of the respective types for more details.
--
-- The types 'ZipStream' and 'ZipAsync' provide 'Applicative' instances to zip
-- two streams together i.e.  each element in one stream is combined with the
-- corresponding element in the other stream. 'ZipStream' generates the streams
-- being zipped serially whereas 'ZipAsync' produces both the elements being
-- zipped concurrently.
--
-- Two streams of the same type can be combined using a sum style composition
-- to generate a stream of the same type where the output stream would contain
-- all elements of both the streams. However, the sequence in which the
-- elements in the resulting stream are produced depends on the combining
-- operator. Four distinct sum style operators, '<>', '<=>', '<|' and '<|>'
-- combine two streams in different ways, each corresponding to the one of the
-- four ways of combining monadically. See the respective section below for
-- more details.
--
-- Concurrent composition types 'AsyncT', 'ParallelT', 'ZipAsync' and
-- concurrent composition operators '<|' and '<|>' require the underlying monad
-- of the streaming monad transformer to be 'MonadAsync'.
--
-- For more details please see the "Streamly.Tutorial" and "Streamly.Examples"
-- (the latter is available only when built with the 'examples' build flag).

-- A simple inline example here illustrating applicative, monad and alternative
-- compositions.

-- $product
--
-- Streams that compose serially or non-concurrently come in two flavors i.e.
-- 'StreamT' and 'InterleavedT'.  Both of these serial flavors have
-- corresponding concurrent equivalents, those are 'AsyncT' and 'ParallelT'
-- respectively.

-- $zipping
--
-- 'ZipStream' and 'ZipAsync', provide 'Applicative' instances for zipping the
-- corresponding elements of two streams together. Note that these types are
-- not monads.

-- $sum
--
-- Just like product style composition there are four distinct ways to combine
-- streams in sum style each directly corresponding to one of the product style
-- composition.
--
-- The standard semigroup append '<>' operator appends two streams serially,
-- this style corresponds to the 'StreamT' style of monadic composition.
--
-- @
-- main = ('toList' . 'serially' $ (return 1 <> return 2) <> (return 3 <> return 4)) >>= print
-- @
-- @
-- [1,2,3,4]
-- @
--
-- The standard 'Alternative' operator '<|>'  fairly interleaves two streams in
-- parallel, this operator corresponds to the 'ParallelT' style.
--
-- @
-- main = ('toList' . 'serially' $ (return 1 <> return 2) \<|\> (return 3 <> return 4)) >>= print
-- @
-- @
-- [1,3,2,4]
-- @
--
-- Unlike '<|', this operator cannot be used to fold infinite containers since
-- that might accumulate too many partially drained streams.  To be clear, it
-- can combine infinite streams but not infinite number of streams.
--
-- Two additional sum style composition operators that streamly introduces are
-- described below.

-- $adapters
--
-- Code using streamly is usually written such that it is agnostic of any
-- specific streaming type.  We use a type variable (polymorphic type) with the
-- 'Streaming' class constraint. Finally, when running the monad we can specify
-- the actual type that we want to use to interpret the code. However, in
-- certain cases we may want to use a specific type to force a certain type of
-- composition. These combinators can be used to convert the stream types from
-- one to another at no cost as all the types have the same underlying
-- representation.
--
-- If you see an @ambiguous type variable@ error then most likely it is because
-- you have not specified the stream type. You either need a type annotation or
-- one of the following combinators to specify what type of stream you mean.
--
-- This code:
--
-- @
-- main = ('toList' $ (return 1 <> return 2)) >>= print
-- @
--
-- will result in a type error like this:
--
-- @
-- Ambiguous type variable ‘t0’ arising from a use of ...
-- @
--
--  To fix the error just tell 'toList' what kind of stream are we feeding it:
--
-- @
-- main = ('toList' $ 'serially' $ (return 1 <> return 2)) >>= print
-- @
-- @
-- main = ('toList' $ (return 1 <> return 2 :: StreamT IO Int)) >>= print
-- @
--
-- Note that using the combinators is easier as you do not have to think about
-- the specific types, they are just inferred.
--

-- $foldutils
--
-- These are some convenience functions to fold any 'Foldable' container using
-- one of the sum composition operators to convert it into a streamly stream.
