-- |
-- Module      : Streamly
-- Copyright   : (c) 2017 Harendra Kumar
--
-- License     : BSD3
-- Maintainer  : harendra.kumar@gmail.com
-- Stability   : experimental
-- Portability : GHC
--
-- Haskell lists provide a simple and powerful API but they are limited to
-- holding only pure values. Streamly streams are a logical extension of
-- Haskell lists to support monadic sequences and powerful declarative
-- concurrency.  They provide almost the same API as lists (See
-- "Streamly.Prelude"). In fact, Haskell lists can be expressed as pure
-- streams, a special case of monadic streams, with similar or better
-- performance and almost drop-in replacement.
--
-- Streams are designed for high performance applications and do not exhibit
-- issues that are usually associated with lists. For example, streams
-- express strings as streams of 'Char' and bytestrings as streams of 'Word8'
-- without any issues, obviating the need for special purpose libraries like
-- bytestring and text, and various lazy and strict falvors of those. Streamly
-- arrays (See "Streamly.Array") complement streams for storing or buffering
-- data efficiently and facilitating efficient interfacing of streams with IO
-- systems.

{-# LANGUAGE CPP                       #-}
{-# LANGUAGE MultiParamTypeClasses     #-}

#if __GLASGOW_HASKELL__ >= 800
{-# OPTIONS_GHC -Wno-orphans #-}
#endif

#include "Streamly/Streams/inline.hs"

module Streamly
    (
    -- * Streams and Folds
    -- $overview

    -- * Streams Overview
    -- $streams

    -- * Type Synonyms
      MonadAsync

    -- * Stream transformers
    -- ** Serial Streams
    -- $serial
    , SerialT
    , WSerialT

    -- ** Speculative Streams
    -- $lookahead
    , AheadT

    -- ** Asynchronous Streams
    -- $async
    , AsyncT
    , WAsyncT
    , ParallelT

    -- ** Zipping Streams
    -- $zipping
    , ZipSerialM
    , ZipAsyncM

    -- * Parallel Function Application
    -- $application
    , (|$)
    , (|&)
    , (|$.)
    , (|&.)
    , mkAsync

    -- * Merging Streams
    -- $sum
    , serial
    , wSerial
    , ahead
    , async
    , wAsync
    , parallel

    -- * Concurrency Control
    -- $concurrency
    , maxThreads
    , maxBuffer

    -- * Rate Limiting
    , Rate (..)
    , rate
    , avgRate
    , minRate
    , maxRate
    , constRate

    -- * Stream Type Adapters
    -- $adapters
    , IsStream ()

    , serially
    , wSerially
    , asyncly
    , aheadly
    , wAsyncly
    , parallely
    , zipSerially
    , zipAsyncly
    , adapt

    -- * IO Streams
    , Serial
    , WSerial
    , Ahead
    , Async
    , WAsync
    , Parallel
    , ZipSerial
    , ZipAsync

    -- * Re-exports
    , Semigroup (..)

    -- * Deprecated
    , Streaming
    , runStream
    , runStreaming
    , runStreamT
    , runInterleavedT
    , runAsyncT
    , runParallelT
    , runZipStream
    , runZipAsync
    , StreamT
    , InterleavedT
    , ZipStream
    , interleaving
    , zipping
    , zippingAsync
    , (<=>)
    , (<|)

    -- * Moved
    -- | These APIs have been moved to other modules
    , foldWith
    , foldMapWith
    , forEachWith
    )
where

import Data.Semigroup (Semigroup(..))
import Streamly.SVar (MonadAsync, Rate(..))
import Streamly.Streams.Ahead
import Streamly.Streams.Async
import Streamly.Streams.Combinators
import Streamly.Streams.Parallel
import Streamly.Streams.Serial
import Streamly.Streams.StreamK hiding (serial)
import Streamly.Streams.Zip

import qualified Streamly.Prelude as P
import qualified Streamly.Streams.StreamK as K

-- XXX provide good succinct examples of pipelining, merging, splitting ect.
-- below.
--
-- $overview
-- Streamly is a general purpose concurrent data flow programming engine
-- expressing a program as a network of streams and folds. Stream types (e.g.
-- 'SerialT') exported from this module represent streams or more specifically
-- stream producers while the fold type exported from "Streamly.Fold"
-- represents stream consumers.  Both producers and consumers can be
-- transformed in many ways e.g.  using map, scan, filter etc.  Transformations
-- can be chained into a pipeline, multiple streams can be merged together or a
-- stream can be split into multiple independent data flows. All these
-- compositions can be combined together to form an arbitrary data flow network
-- expressing any kind of general purpose computing in a declarative manner.
--
-- The simplest way to compose a data processing pipeline is to generate a
-- stream of data (e.g. from a file or network), chain pipelines of
-- transformations on the stream and finally pass it to a fold. Folds represent
-- specific ways to consume a stream (e.g. producing a sum of numbers).
-- Transformations can also be applied on folds before they are attached with a
-- stream.
--
-- @
--
-- ---Stream m a-->stream transforms-->fold transforms-->Fold m a b
-- @
--
-- Stream producers can be appended, merged, zipped or nested to build an
-- arbitrarily complex composed stream producer. Transformations can be applied
-- at any point in the composition tree. See "Streamly.Prelude" module for more
-- details on the combinators.
--
-- @
--
-- -------Stream m a-->transform-->|
--                                 |
-- -------Stream m a-->transform-->|=>---transform-->Stream m a--->
--                                 |
-- -------Stream m a-->transform-->|
-- @
--
-- Folds are consumers of streams and can be used to split a stream into
-- multiple independent flows. Grouping transforms a stream by applying a fold
-- on segments of a stream, distributing applies multiple folds in parallel on
-- the same stream and combines them, partitioning sends different elements of
-- a stream to different folds, unzipping divides the elements of a stream into
-- parts and sends them through different folds. Parsers are nothing but a
-- particular type of folds. Transformations can be applied contravariantly on
-- the input of a fold.
--
-- @
--
--                             |---transform----Fold m a b--------|
-- ---stream m a-->transform-->|                                  |---f b c ...
--                             |---transform----Fold m a c--------|
--                             |                                  |
--                                        ...
-- @
--

-- $streams
-- The basic stream type is 'Serial', it represents a sequence of IO actions,
-- and is a 'Monad'.  The type 'SerialT' is a monad transformer that can
-- represent a sequence of actions in an arbitrary monad. The type 'Serial' is
-- in fact a synonym for @SerialT IO@.  There are a few more types similar to
-- 'SerialT', all of them represent a stream and differ only in the
-- 'Semigroup', 'Applicative' and 'Monad' compositions of the stream. 'Serial'
-- and 'WSerial' types compose serially whereas 'Async' and 'WAsync'
-- types compose concurrently. All these types can be freely inter-converted
-- using type combinators without any cost. You can freely switch to any type
-- of composition at any point in the program.  When no type annotation or
-- explicit stream type combinators are used, the default stream type is
-- inferred as 'Serial'.
--
-- Here is a simple console echo program example:
--
-- @
-- > drain $ S.repeatM getLine & S.mapM putStrLn
-- @
--
-- For more details please see the "Streamly.Tutorial" module and the examples
-- directory in this package.
--
-- This module exports stream types, instances and some basic operations.
-- Functionality exported by this module include:
--
-- * Semigroup append ('<>') instances as well as explicit  operations for merging streams
-- * Monad and Applicative instances for looping over streams
-- * Zip Applicatives for zipping streams
-- * Stream type combinators to convert between different composition styles
-- * Some basic utilities to run and fold streams
--
-- See the "Streamly.Prelude" module for comprehensive APIs for construction,
-- generation, elimination and transformation of streams.
--
-- This module is designed to be imported unqualified:
--
-- @
-- import Streamly
-- @

------------------------------------------------------------------------------
-- Eliminating a stream
------------------------------------------------------------------------------

-- | Same as 'runStream'
--
-- @since 0.1.0
{-# DEPRECATED runStreaming "Please use runStream instead." #-}
runStreaming :: (Monad m, IsStream t) => t m a -> m ()
runStreaming = P.drain . K.adapt

-- | Same as @runStream@.
--
-- @since 0.1.0
{-# DEPRECATED runStreamT "Please use runStream instead." #-}
runStreamT :: Monad m => SerialT m a -> m ()
runStreamT = P.drain

-- | Same as "Streamly.Prelude.runStream".
--
{-# DEPRECATED runStream "Please use Streamly.Prelude.drain instead." #-}
runStream :: Monad m => SerialT m a -> m ()
runStream = P.drain

-- | Same as @runStream . wSerially@.
--
-- @since 0.1.0
{-# DEPRECATED runInterleavedT "Please use 'runStream . interleaving' instead." #-}
runInterleavedT :: Monad m => WSerialT m a -> m ()
runInterleavedT = P.drain . K.adapt

-- | Same as @runStream . parallely@.
--
-- @since 0.1.0
{-# DEPRECATED runParallelT "Please use 'runStream . parallely' instead." #-}
runParallelT :: Monad m => ParallelT m a -> m ()
runParallelT = P.drain . K.adapt

-- | Same as @runStream . asyncly@.
--
-- @since 0.1.0
{-# DEPRECATED runAsyncT "Please use 'runStream . asyncly' instead." #-}
runAsyncT :: Monad m => AsyncT m a -> m ()
runAsyncT = P.drain . K.adapt

-- | Same as @runStream . zipping@.
--
-- @since 0.1.0
{-# DEPRECATED runZipStream "Please use 'runStream . zipSerially instead." #-}
runZipStream :: Monad m => ZipSerialM m a -> m ()
runZipStream = P.drain . K.adapt

-- | Same as @runStream . zippingAsync@.
--
-- @since 0.1.0
{-# DEPRECATED runZipAsync "Please use 'runStream . zipAsyncly instead." #-}
runZipAsync :: Monad m => ZipAsyncM m a -> m ()
runZipAsync = P.drain . K.adapt

-- | Same as "Streamly.Prelude.foldWith".
--
{-# DEPRECATED foldWith "Please use Streamly.Prelude.foldWith instead." #-}
{-# INLINABLE foldWith #-}
foldWith :: (IsStream t, Foldable f)
    => (t m a -> t m a -> t m a) -> f (t m a) -> t m a
foldWith = P.foldWith

-- | Same as "Streamly.Prelude.foldMapWith".
--
{-# DEPRECATED foldMapWith "Please use Streamly.Prelude.foldMapWith instead." #-}
{-# INLINABLE foldMapWith #-}
foldMapWith :: (IsStream t, Foldable f)
    => (t m b -> t m b -> t m b) -> (a -> t m b) -> f a -> t m b
foldMapWith = P.foldMapWith

-- | Same as "Streamly.Prelude.forEachWith".
--
{-# DEPRECATED forEachWith "Please use Streamly.Prelude.forEachWith instead." #-}
{-# INLINABLE forEachWith #-}
forEachWith :: (IsStream t, Foldable f)
    => (t m b -> t m b -> t m b) -> f a -> (a -> t m b) -> t m b
forEachWith = P.forEachWith

------------------------------------------------------------------------------
-- Documentation
------------------------------------------------------------------------------

-- $serial
--
-- Serial streams compose serially or non-concurrently. In a composed stream,
-- each action is executed only after the previous action has finished.  The two
-- serial stream types 'SerialT' and 'WSerialT' differ in how they traverse the
-- streams in a 'Semigroup' or 'Monad' composition.

-- $async
--
-- The async style streams execute actions asynchronously and consume the
-- outputs as well asynchronously. In a composed stream, at any point of time
-- more than one stream can run concurrently and yield elements.  The elements
-- are yielded by the composed stream as they are generated by the constituent
-- streams on a first come first serve basis.  Therefore, on each run the
-- stream may yield elements in a different sequence depending on the delays
-- introduced by scheduling.  The two async types 'AsyncT' and 'WAsyncT' differ
-- in how they traverse streams in 'Semigroup' or 'Monad' compositions.

-- $zipping
--
-- 'ZipSerialM' and 'ZipAsyncM', provide 'Applicative' instances for zipping the
-- corresponding elements of two streams together. Note that these types are
-- not monads.

-- $application
--
-- Stream processing functions can be composed in a chain using function
-- application with or without the '$' operator, or with reverse function
-- application operator '&'. Streamly provides concurrent versions of these
-- operators applying stream processing functions such that each stage of the
-- stream can run in parallel. The operators start with a @|@; we can read '|$'
-- as "@parallel dollar@" to remember that @|@ comes before '$'.
--
-- Imports for the code snippets below:
--
-- @
--  import Streamly
--  import qualified Streamly.Prelude as S
--  import Control.Concurrent
-- @

-- $sum
-- The 'Semigroup' operation '<>' of each stream type combines two streams in a
-- type specific manner. This section provides polymorphic versions of '<>'
-- which can be used to combine two streams in a predetermined way irrespective
-- of the type.

-- XXX An alternative design choice would be to let a control parameter affect
-- the nearest SVar only and then it gets cleared. The benefit of the current
-- choice is that it is simply just like global configuration, just like state
-- behaves, so should be easy to comprehend. But it has the downside of leaking
-- to undesired actions, that is we can forget to reset it.
--
-- $concurrency
--
-- These combinators can be used at any point in a stream composition to set
-- parameters to control the concurrency of the enclosed stream.  A parameter
-- set at any point remains effective for any concurrent combinators used
-- downstream until it is reset.  These control parameters have no effect on
-- non-concurrent combinators in the stream, or on non-concurrent streams.

-- $adapters
--
-- You may want to use different stream composition styles at different points
-- in your program. Stream types can be freely converted or adapted from one
-- type to another.  The 'IsStream' type class facilitates type conversion of
-- one stream type to another. It is not used directly, instead the type
-- combinators provided below are used for conversions.
--
-- To adapt from one monomorphic type (e.g. 'AsyncT') to another monomorphic
-- type (e.g. 'SerialT') use the 'adapt' combinator. To give a polymorphic code
-- a specific interpretation or to adapt a specific type to a polymorphic type
-- use the type specific combinators e.g. 'asyncly' or 'wSerially'. You
-- cannot adapt polymorphic code to polymorphic code, as the compiler would not know
-- which specific type you are converting from or to. If you see a an
-- @ambiguous type variable@ error then most likely you are using 'adapt'
-- unnecessarily on polymorphic code.
