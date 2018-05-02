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

      MonadParallel

    -- * IO Streams
    , Stream
    , Costream
    , Coparallel
    , Parallel
    , ZipStream
    , ZipParallel

    -- * Stream transformers
    , IsStream

    -- ** Serial Streams
    -- $serial
    , StreamT
    , CostreamT

    -- ** Parallel Streams
    -- $parallel
    , CoparallelT
    , ParallelT

    -- ** Zipping Streams
    -- $zipping
    , ZipStreamM
    , ZipParallelM

    -- * Stream Type Adapters
    -- $adapters
    , streamly
    , costreamly
    , coparallely
    , parallely
    , zipStreamly
    , zipParallely
    , adapt

    -- * Running Streams
    , runStream

    -- * Transformation
    , async

    -- * Polymorphic Sum Operations
    -- $sum
    , splice
    , cosplice
    , coparallel
    , parallel

    -- * Polymorphic Fold Utilities
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

    -- * Deprecated
    , MonadAsync
    , Streaming
    , runStreaming
    , runStreamT
    , runInterleavedT
    , runParallelT
    , runZipAsync
    , runAsyncT
    , runZipStream
    , InterleavedT
    , AsyncT
    , ZipAsync
    , interleaving
    , asyncly
    , zipping
    , zippingAsync
    , (<=>)
    , (<|)
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
-- Streamly provides six distinct stream types i.e. 'StreamT', 'CostreamT',
-- 'CoparallelT' and 'ParallelT', 'ZipStreamM' and 'ZipParallelM', each representing a
-- stream of elements.  All these types have the same underlying representation
-- and can be adapted from one to another using type adaptor combinators
-- described later. Each of these types belongs to the 'IsStream' type class
-- which helps converting the specific type to and from the underlying generic
-- stream type.
--
-- The types 'StreamT', 'CostreamT', 'CoparallelT' and 'ParallelT' are 'Monad'
-- transformers with the monadic bind operation combining streams in a product
-- style in much the same way as a list monad or a list transformer i.e. each
-- element from one stream is combined with every element of the other stream.
-- However, the applicative and monadic composition of these types differ in
-- terms of the ordering and time sequence in which the elements from two
-- streams are combined. 'StreamT' and 'CostreamT' compose streams streamly
-- whereas 'CoparallelT' and 'ParallelT' are their concurrent counterparts. See the
-- documentation of the respective types for more details.
--
-- The types 'ZipStreamM' and 'ZipParallelM' provide 'Applicative' instances to zip
-- two streams together i.e.  each element in one stream is combined with the
-- corresponding element in the other stream. 'ZipStreamM' generates the streams
-- being zipped serially whereas 'ZipParallelM' produces both the elements being
-- zipped concurrently.
--
-- Two streams of the same type can be merged using a sum style composition to
-- generate a stream of the same type where the output stream would contain all
-- elements of both the streams. However, the sequence or the manner
-- (concurrent or serial) in which the elements in the resulting stream are
-- produced depends on the type of the stream.
--
-- For more details please see the "Streamly.Tutorial" and "Streamly.Examples"
-- (the latter is available only when built with the 'examples' build flag).

-- A simple inline example here illustrating applicative, monad and alternative
-- compositions.

-- $serial
--
-- Serial streams compose serially or non-concurrently. In a composed stream,
-- at any point of time only one constituent stream runs and yields an element.
-- The two serial stream types 'StreamT' and 'CostreamT' differ in how they
-- merge streams together in a 'Semigroup' or 'Monad' composition. As these
-- streams are serial, the sequence of items in a composed stream can be solely
-- determined by the position of elements in the consituent streams.

-- $parallel
--
-- Parallel streams compose parallely or concurrently. In a composed stream, at
-- any point of time more than one stream can run concurrently and yield
-- elements. The two parallel types 'ParallelT' and 'CoparallelT' differ in how
-- they merge streams together in a 'Semigroup' or 'Monad' composition. As
-- these streams compose concurrently, the sequence of items in a composed
-- stream cannot be determined by the position of elements in the constituent
-- streams.  The elements are yielded by the composed stream as they are
-- generated by the constituent streams on a first come first serve basis.
-- Therefore, on each run the stream may yield elements in a different sequence
-- depending on the delays introduced by scheduling.

-- $zipping
--
-- 'ZipStreamM' and 'ZipParallelM', provide 'Applicative' instances for zipping the
-- corresponding elements of two streams together. Note that these types are
-- not monads.

-- $sum
-- The 'Semigroup' operation '<>' of each stream type combines two streams in a
-- type specific manner. This section provides polymorphic versions of '<>'
-- which can be used to combine two streams in a predetermined way irrespective
-- of the type.

-- $adapters
--
-- You may want to use different stream types at different points in your
-- program. Stream types can be converted or adapted from one type to another
-- to make them interwork with each other.
--
-- To adapt from one monomorphic type (e.g. 'ParallelT') to another monomorphic
-- type (e.g. 'StreamT') use the 'adapt' combinator. To give a polymorphic code
-- a specific interpretation or to adapt a specific type to a polymorphic type
-- use the type specific combinators e.g. 'parallely' or 'costreamly'. You
-- cannot adapt polymorphic code to polymorphic code, as it would not know
-- which specific type you are converting from or to. If you see a an
-- @ambiguous type variable@ error then most likely you are using 'adapt'
-- unnecessarily on polymorphic code.
--

-- $foldutils
--
-- These are variants of standard 'Foldable' fold functions that use a
-- polymorphic stream sum operation (e.g. 'parallel' or 'cosplice') to fold a
-- container of streams.
