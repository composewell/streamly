-- |
-- Module      : Streamly
-- Copyright   : (c) 2017 Composewell Technologies
--
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Streamly is a general purpose programming framework using cocnurrent data
-- flow programming paradigm.  It can be considered as a generalization of
-- Haskell lists to monadic streaming with concurrent composition capability.
-- The serial stream type in streamly @SerialT m a@ is like the list type @[a]@
-- parameterized by the monad @m@. For example, @SerialT IO a@ is a moral
-- equivalent of @[a]@ in the IO monad.  Streams are constructed very much like
-- lists, except that they use 'nil' and 'cons' instead of '[]' and ':'.
--
-- @
-- > import "Streamly"
-- > import "Streamly.Prelude" (cons, consM)
-- > import qualified "Streamly.Prelude" as S
-- >
-- > S.toList $ 1 \`cons` 2 \`cons` 3 \`cons` nil
-- [1,2,3]
-- @
--
-- Unlike lists, streams can be constructed from monadic effects:
--
-- @
-- > S.'toList' $ 'getLine' \`consM` 'getLine' \`consM` S.'nil'
-- hello
-- world
-- ["hello","world"]
-- @
--
-- Streams are processed just like lists, with list like combinators, except
-- that they are monadic and work in a streaming fashion.  Here is a simple
-- console echo program example:
--
-- @
-- > S.drain $ S.repeatM getLine & S.mapM putStrLn
-- @
--
-- @SerialT Identity a@ is a moral equivalent of pure lists. Streamly utilizes
-- fusion for high performance, therefore, we can represent and process strings
-- as streams of 'Char', encode and decode the streams to/from UTF8 and
-- serialize them to @Array Word8@ obviating the need for special purpose
-- libraries like @bytestring@ and @text@.
--
-- For more details
-- please see the "Streamly.Tutorial" module and the examples directory in this
-- package.

{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -Wno-orphans #-}

#include "inline.hs"

module Streamly {-# DEPRECATED "Please use \"Streamly.Prelude\" instead." #-}
    (
    -- -- * Concepts Overview
    -- -- ** Streams
    -- -- $streams

    -- -- ** Folds
    -- -- $folds

    -- -- ** Arrays
    -- -- $arrays

    -- * Module Overview
    -- $streamtypes

    -- * Type Synonyms
      MonadAsync

    -- * Stream transformers
    -- | A stream represents a sequence of pure or effectful actions. The
    -- `cons` and `consM` operations and the corresponding operators '.:' and
    -- '|:' can be used to join pure values or effectful actions in a sequence.
    -- The effects in the stream can be executed in many different ways
    -- depending on the type of stream. In other words, the behavior of 'consM'
    -- depends on the type of the stream.
    --
    -- There are three high level categories of streams, /spatially ordered/
    -- streams, /speculative/ streams and /time ordered/ streams. Spatially
    -- ordered streams, 'SerialT' and 'WSerialT', execute the effects in serial
    -- order i.e. one at a time and present the outputs of those effects to the
    -- consumer in the same order.  Speculative streams, 'AheadT', may execute
    -- many effects concurrently but present the outputs to the consumer in the
    -- specified spatial order.  Time ordered streams, 'AsyncT', 'WAsyncT' and
    -- 'ParallelT', may execute many effects concurrently and present the
    -- outputs of those effects to the consumer in time order i.e. as soon as
    -- the output is generated.
    --
    -- We described above how the effects in a sequence are executed for
    -- different types of streams. The behvavior of the 'Semigroup' and 'Monad'
    -- instances follow the behavior of 'consM'. Stream generation operations
    -- like 'repeatM' also execute the effects differently for different
    -- streams, providing a concurrent generation capability when used with
    -- stream types that execute effects concurrently. Similarly, effectful
    -- transformation operations like 'mapM' also execute the transforming
    -- effects differently for different types of streams.

    -- ** Serial Streams
    -- $serial
    , SerialT
    , WSerialT

    -- ** Speculative Streams
    -- $ahead
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
    , (IP.|$)
    , (IP.|&)
    , (IP.|$.)
    , (IP.|&.)
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

    -- ** Folding Containers of Streams
    -- | These are variants of standard 'Foldable' fold functions that use a
    -- polymorphic stream sum operation (e.g. 'async' or 'wSerial') to fold a
    -- finite container of streams. Note that these are just special cases of
    -- the more general 'concatMapWith' operation.
    --
    , foldWith
    , foldMapWith
    , forEachWith

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
    )
where

import Data.Semigroup (Semigroup(..))
import Streamly.Internal.Data.SVar (MonadAsync, Rate(..))
import Streamly.Internal.Data.Stream.Ahead
import Streamly.Internal.Data.Stream.Async hiding (mkAsync)
import Streamly.Internal.Data.Stream.Combinators
import Streamly.Internal.Data.Stream.Parallel
import Streamly.Internal.Data.Stream.Serial
import Streamly.Internal.Data.Stream.StreamK hiding (serial)
import Streamly.Internal.Data.Stream.Zip

import qualified Streamly.Prelude as P
import qualified Streamly.Internal.Data.Stream.IsStream as IP
import qualified Streamly.Internal.Data.Stream.StreamK as K
import qualified Streamly.Internal.Data.Stream.Async as Async

-- XXX provide good succinct examples of pipelining, merging, splitting etc.
-- below.
--
-- $streams
--
-- A program is expressed as a network of streams and folds. A stream is a
-- source or generator of data elements and a fold is a consumer of data elements that
-- reduces multiple input elements to a single value.
--
-- In the following example, a 'Word8' stream is generated by using
-- 'Streamly.FileSystem.Handle.read' on a file handle, then the
-- 'Streamly.Prelude.splitBySuffix' transformation splits the stream on
-- newlines (ascii value 10); it uses the 'Streamly.Data.Fold.drain' fold to reduce
-- the resulting lines to unit values (@()@), 'Streamly.Prelude.length' fold
-- then counts the unit elements in the resulting stream which gives us the
-- number of lines in the file:
--
-- > S.length $ S.splitOnSuffix FL.drain 10 $ FH.read fh
--
-- The following example folds the lines to arrays of 'Word8' using the
-- 'Streamly.Data.Array.Storable.Foreign.writeF' fold and then wraps the lines in square
-- brackets before writing them to standard output using
-- 'Streamly.FileSystem.Handle.write':
--
-- > wrapLine ln = S.fromList "[" <> A.read ln <> S.fromList "]\n"
-- > readLines = S.splitOnSuffix A.writeF 10
-- > FH.write stdout $ S.concatMap wrapLine $ readLines fh1
--
-- One stream can be appended after another:
--
-- > FH.write stdout $ S.concatMap wrapLine $ readLines fh1 <> readLines fh2
--
-- The following example reads two files concurrently, merges the lines from
-- the two streams and writes the resulting stream to another file:
--
-- > FH.write stdout $ S.concatMap wrapLine $ readLines fh1 `parallel` readLines fh2
--
-- There are many ways to generate, merge, zip, transform and fold data
-- streams.  Many transformations can be chained in a stream pipeline. See
-- "Streamly.Prelude" module for combinators to manipulate streams.

-- $folds
--
-- The way stream types in this module like 'SerialT' represent data sources,
-- the same way the 'Fold' type from "Streamly.Data.Fold" represents data sinks or
-- reducers of streams. Reducers can be combined to consume a stream source in
-- many ways. The simplest is to reduce a stream source using a fold e.g.:
--
-- > S.foldOnce FL.length $ S.enumerateTo 100
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

-- $arrays
--
-- Streamly arrays (See "Streamly.Data.Array.Storable.Foreign") complement streams to provide an
-- efficient computing paradigm.  Streams are suitable for immutable
-- transformations of /potentially infinite/ data using /sequential access/ and
-- pipelined transformations whereas arrays are suitable for in-place
-- transformations of /necessarily finite/ data using /random access/. Streams
-- are synonymous with /sequential pipelined processing/ whereas arrays are
-- synonymous with /efficient buffering and random access/.
--
-- In general, a data processing pipeline reads data from some IO device, does
-- some processing on it and finally writes the output to another IO device.
-- Streams provide the overall framework of sequential processing pipeline in
-- which arrays are used as buffering elements in the middle.  In addition to
-- buffering in the middle, arrays can also be used at the boundaries of the
-- pipeline to efficiently interface with external storage systems like memory,
-- files and network.  If streams are the pipes in a water pipeline network
-- then arrays are like the storage tanks in the middle.  On the input side,
-- think of arrays as buckets to fetch water to feed the pipeline and on the
-- output side buckets to remove the processed water.
--
-- 'ByteString' data type from the 'bytestring' package and the 'Text' data
-- type from the 'text' package are special cases of arrays.  'ByteString' is
-- like @Array Word8@ and 'Text' is like @utf16@ encoded @Array Word8@.
-- Streamly arrays can be transformed as efficiently as @bytestring@ or @text@
-- by using stream operations on them.

-- Streams and arrays are equally important in computing. They are computing
-- duals of each other.

-- $streamtypes
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
-- This module exports stream types, instances and combinators for:
--
-- * converting between different stream types
-- * appending and concurrently merging streams
-- * Concurrency control
-- * Concurrent function application
-- * Stream rate control
--
-- This module is designed to be imported unqualified:
--
-- @
-- import Streamly
-- @
--
-- See the "Streamly.Prelude" module for APIs for construction,
-- generation, elimination and transformation of streams.

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

{-
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
-}

-- XXX Deprecate it in 0.8.0
--
-- | Make a stream asynchronous, triggers the computation and returns a stream
-- in the underlying monad representing the output generated by the original
-- computation. The returned action is exhaustible and must be drained once. If
-- not drained fully we may have a thread blocked forever and once exhausted it
-- will always return 'empty'.
--
-- @since 0.2.0
{-# INLINABLE mkAsync #-}
mkAsync :: (IsStream t, MonadAsync m) => t m a -> m (t m a)
mkAsync = return . Async.mkAsync

------------------------------------------------------------------------------
-- Documentation
------------------------------------------------------------------------------

-- $serial
--
-- When a stream consumer demands an element from a serial stream constructed
-- as @a \`consM` b \`consM` ... nil@, the action @a@ at the head of the stream
-- sequence is executed and the result is supplied to the consumer. When the
-- next element is demanded, the action @b@ is executed and its result is
-- supplied.  Thus, the effects are performed and results are consumed strictly
-- in a serial order.  Serial streams can be considered as /spatially ordered/
-- streams as the order of execution and consumption is the same as the spatial
-- order in which the actions are composed by the programmer.
--
-- Serial streams enforce the side effects as well as the results of the
-- actions to be in the same order in which the actions are added to the
-- stream.  Therefore, the semigroup operation for serial streams is not
-- commutative:
--
-- @
-- a <> b is not the same as b <> a
-- @
--
-- There are two serial stream types 'SerialT' and 'WSerialT'. The stream
-- evaluation of both the variants works in the same way as described above,
-- they differ only in the 'Semigroup' and 'Monad' implementaitons.

-- $ahead
--
-- When a stream consumer demands an element from a speculative stream
-- constructed as @a \`consM` b \`consM` ... nil@, the action @a@ at the head
-- of the stream is executed and the output of the action is supplied to the
-- consumer. However, in addition to the action at the head multiple actions
-- following it may also be executed concurrently and the results buffered.
-- When the next element is demanded it may be served from the buffer and we
-- may execute the next action in the sequence to keep the buffer adequately
-- filled.  Thus, the actions are executed concurrently but results consumed in
-- serial order just like serial streams.  `consM` can be used to fold an
-- infinite lazy container of effects, as the number of concurrent executions
-- is limited.
--
-- Similar to 'consM', the monadic stream generation (e.g. replicateM) and
-- transformation operations (e.g. mapM) on speculative streams can execute
-- multiple effects concurrently in a speculative manner.
--
-- How many effects can be executed concurrently and how many results can be
-- buffered are controlled by 'maxThreads' and 'maxBuffer' combinators
-- respectively.  The actual number of concurrent threads is adjusted according
-- to the rate at which the consumer is consuming the stream. It may even
-- execute actions serially in a single thread if that is enough to match the
-- consumer's speed.
--
-- Speculative streams enforce ordering of the results of actions in the stream
-- but the side effects are only partially ordered.  Therefore, the semigroup
-- operation for speculative streams is not commutative from the pure outputs
-- perspective but commutative from side effects perspective.

-- $async
--
-- /Scheduling and execution:/ In an asynchronous stream @a \`consM` b \`consM`
-- c ...@, the actions @a@, @b@, and @c@ are executed concurrently with the
-- consumer of the stream.  The actions are /scheduled/ for execution in the
-- same order as they are specified in the stream. Multiple scheduled actions
-- may be /executed/ concurrently in parallel threads of execution.  The
-- actions may be executed out of order and they may complete at arbitrary
-- times.  Therefore, the /effects/ of the actions may be observed out of
-- order.
--
-- /Buffering:/ The /results/ from multiple threads of execution are queued in
-- a buffer as soon as they become available. The consumer of the stream is
-- served from this buffer.  Therefore, the consumer may observe the results to
-- be out of order.  In other words, an asynchronous stream is an unordered
-- stream i.e.  order does not matter.
--
-- /Concurrency control:/ Threads are suspended if the `maxBuffer` limit is
-- reached, and resumed when the consumer makes space in the buffer.  The
-- maximum number of concurrent threads depends on `maxThreads`. Number of
-- threads is increased or decreased based on the speed of the consumer.
--
-- /Generation operations:/ Concurrent stream generation operations e.g.
-- 'Streamly.Prelude.replicateM' when used in async style schedule and execute
-- the stream generating actions in the manner described above. The generation
-- actions run concurrently, effects and results of the actions as observed by
-- the consumer of the stream may be out of order.
--
-- /Transformation operations:/ Concurrent stream transformation operations
-- e.g.  'Streamly.Prelude.mapM', when used in async style, schedule and
-- execute transformation actions in the manner described above. Transformation
-- actions run concurrently, effects and results of the actions may be
-- observed by the consumer out of order.
--
-- /Variants:/ There are two asynchronous stream types 'AsyncT' and 'WAsyncT'.
-- They are identical with respect to single stream evaluation behavior.  Their
-- behaviors differ in how they combine multiple streams using 'Semigroup' or
-- 'Monad' composition. Since the order of elements does not matter in
-- asynchronous streams the 'Semigroup' operation is effectively commutative.

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
-- parameters to control the concurrency of the /argument stream/.  A control
-- parameter set at any point remains effective for any concurrent combinators
-- used in the argument stream until it is reset by using the combinator again.
-- These control parameters have no effect on non-concurrent combinators in the
-- stream, or on non-concurrent streams.
--
-- /Pitfall:/ Remember that 'maxBuffer' in the following example applies to
-- 'mapM' and any other combinators that may follow it, and it does not apply
-- to the combinators before it:
--
-- @
--  ...
--  $ maxBuffer 10
--  $ S.mapM ...
--  ...
-- @
--
-- If we use '&' instead of '$' the situation will reverse, in the following
-- example, 'maxBuffer' does not apply to 'mapM', it applies to combinators
-- that come before it, because those are the arguments to 'maxBuffer':
--
-- @
--  ...
--  & maxBuffer 10
--  & S.mapM ...
--  ...
-- @

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

-- | Same as 'Streamly.Prelude.concatFoldableWith'
--
-- @since 0.1.0
{-# DEPRECATED foldWith "Please use 'Streamly.Prelude.concatFoldableWith' instead." #-}
{-# INLINEABLE foldWith #-}
foldWith :: (IsStream t, Foldable f) => (t m a -> t m a -> t m a) -> f (t m a) -> t m a
foldWith = P.concatFoldableWith

-- | Same as 'Streamly.Prelude.concatMapFoldableWith'
--
-- @since 0.1.0
{-# DEPRECATED foldMapWith "Please use 'Streamly.Prelude.concatMapFoldableWith' instead." #-}
{-# INLINEABLE foldMapWith #-}
foldMapWith :: (IsStream t, Foldable f) => (t m b -> t m b -> t m b) -> (a -> t m b) -> f a -> t m b
foldMapWith = P.concatMapFoldableWith

-- | Same as 'Streamly.Prelude.concatForFoldableWith'
--
-- @since 0.1.0
{-# DEPRECATED forEachWith "Please use 'Streamly.Prelude.concatForFoldableWith' instead." #-}
{-# INLINEABLE forEachWith #-}
forEachWith :: (IsStream t, Foldable f) => (t m b -> t m b -> t m b) -> f a -> (a -> t m b) -> t m b
forEachWith = P.concatForFoldableWith
