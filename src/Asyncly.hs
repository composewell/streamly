-- |
-- Module      : Asyncly
-- Copyright   : (c) 2017 Harendra Kumar
--
-- License     : BSD3
-- Maintainer  : harendra.kumar@gmail.com
-- Stability   : experimental
-- Portability : GHC
--
-- 'AsyncT' is a monad transformer that extends the product style composition
-- of monads to streams; it is a functional porgramming equivalent to nested
-- loops in imperative programming. This style of composition allows composing
-- each element in one stream with each element in the other stream,
-- generalizing the monadic product of single elements. For example, you can
-- think of the IO Monad as a special case of the more general @AsyncT IO@
-- monad; with single element streams.  List transformers and logic programming
-- monads also provide a similar product style composition of streams, however
-- 'AsyncT' generalizes it with the time dimension; allowing streams to be
-- composed in an asynchronous and concurrent fashion in many different ways.
--
-- The power of this seemingly simple addition of asynchronicity and
-- concurrency to product style streaming composition should not be
-- underestimated.  It unifies a number of disparate abstractions into one
-- powerful and elegant abstraction.  A wide variety of programming problems
-- can be solved elegantly with this abstraction. In particular, it unifies
-- three major programming domains namely non-deterministic (logic)
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
--
-- When it comes to streaming composition, if @conduit@ or @pipes@ are arrows
-- then asyncly is monad. Streaming libraries like conduit or pipes provide
-- a categorical composition (sum) of streams whereas asyncly provides a more
-- general monadic composition (product) of streams, with concurrency.  Asyncly
-- interworks with major streaming libraries providing a way to use both types
-- of compositions and using the right tool at the right place.
-- When it comes to concurrency, asyncly may be comared with
-- imperative style concurrency frameworks like
-- <https://en.wikipedia.org/wiki/OpenMP OpenMP> or
-- <https://en.wikipedia.org/wiki/Cilk Cilk>.
--
-- For more details please see the 'Asyncly.Tutorial' and 'Asyncly.Examples'.

-- A simple inline example here illustrating applicative, monad and alternative
-- compositions.
module Asyncly
    ( MonadAsync
    , Streaming

    -- * Construction
    , fromCallback

    -- * Product Style Composition
    , StreamT
    , InterleavedT
    , AsyncT
    , ParallelT
    , ZipStream
    , ZipAsync

    -- * Sum Style Composition
    -- $monoidal
    , (<=>)
    , (<|)

    -- * Custom Composition
    , async

    -- * Running Streams
    , runStreaming
    , toList

    -- * Stream Type Adapters
    , serially
    , interleaving
    , asyncly
    , parallely
    , zipping
    , zippingAsync
    , adapt

    -- * Fold Utilities
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

import Asyncly.Core
import Asyncly.Prelude
import Data.Semigroup (Semigroup(..))
import Control.Applicative (Alternative(..))
import Control.Monad (MonadPlus(..))
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Class (MonadTrans (..))

-- $monoidal
--
-- These combinators can be used in place of 'Monoid' ('<>') or 'Alternative'
-- ('<|>') composition to compose the streams in alternate ways.
