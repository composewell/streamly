-- |
-- Module      : Streamly.Internal.Data.Stream.Parallel.Channel
-- Copyright   : (c) 2017 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

module Streamly.Internal.Data.Stream.Parallel.Channel
    (
    -- * Channel
      Channel

    -- * Configuration
    , Config
    -- XXX We can implement maxThreads, by blocking the dispatcher when
    -- maxThreads is reached.
    -- , maxThreads
    , maxBuffer

    -- * Operations
    , fromChannel
    , toChannel
    , withChannelK
    , concatMapWithChanK

    -- XXX Move to the higher level Async module. Low level module would have
    -- only explicit channel operations.

    -- * Stream operations
    , evalWithD
    , evalWithK
    , appendWithD
    , appendWithK
    , concatMapWithK
    )
where

import Control.Monad.IO.Class (MonadIO(..))
import Data.IORef (readIORef, writeIORef)
import Streamly.Internal.Control.Concurrent (MonadAsync)

import qualified Data.Set as Set
import qualified Streamly.Internal.Data.Stream as Stream
import qualified Streamly.Internal.Data.Stream.StreamD as D
import qualified Streamly.Internal.Data.Stream.StreamK.Type as K
-- import qualified Streamly.Internal.Data.Unfold as Unfold

import Streamly.Internal.Data.Stream.Channel.Types
import Streamly.Internal.Data.Stream.Parallel.Channel.Type

-- | Allocate a channel and evaluate the stream using the channel and the
-- supplied evaluator function. The evaluator is run in a worker thread.
{-# INLINE withChannelD #-}
withChannelD :: MonadAsync m =>
       (Channel m b -> D.Stream m a -> D.Stream m b)
    -> (Config -> Config)
    -> D.Stream m a
    -> K.Stream m b
{-
withChannelD evaluator modifier input =
    Stream.toStreamK $ Stream.unfold (Unfold.lmapM action readChannel) ()

    where

    action () = do
        chan <- newChannel StopNone modifier
        toChannelD chan (evaluator chan input)
        return chan
-}
withChannelD evaluator modifier input = K.concatEffect action

    where

    action = do
        chan <- newChannel StopNone modifier
        toChannelD chan (evaluator chan input)
        return $ fromChannelK chan

{-# INLINE withChannelK #-}
withChannelK :: MonadAsync m =>
       (Channel m b -> K.Stream m a -> K.Stream m b)
    -> (Config -> Config)
    -> K.Stream m a
    -> K.Stream m b
withChannelK evaluator modifier input = K.concatEffect action

    where

    action = do
        chan <- newChannel StopNone modifier
        toChannelK chan (evaluator chan input)
        return $ fromChannelK chan

-------------------------------------------------------------------------------
-- Evaluating a stream
-------------------------------------------------------------------------------

-- | Evaluate the stream asynchronously in a worker thread separate from the
-- consumer thread.
--
-- >> evalWithD = withChannelD (const id)
--
{-# INLINABLE evalWithD #-}
evalWithD :: MonadAsync m => (Config -> Config) -> D.Stream m a -> K.Stream m a
evalWithD = withChannelD (const id)

{-# INLINABLE evalWithK #-}
evalWithK :: MonadAsync m => (Config -> Config) -> K.Stream m a -> K.Stream m a
evalWithK = withChannelK (const id)

-------------------------------------------------------------------------------
-- appending two streams
-------------------------------------------------------------------------------

-- | Create a new channel and add both the streams to it for async evaluation.
-- The output stream is the result of the evaluation.
{-# INLINE appendWithD #-}
appendWithD :: MonadAsync m =>
       SVarStopStyle
    -> (Config -> Config)
    -> D.Stream m a
    -> D.Stream m a
    -> K.Stream m a
appendWithD stopStyle modifier stream1 stream2 = K.concatEffect action

    where

    action = do
        chan <- newChannel stopStyle modifier
        toChannelD chan stream2
        case stopStyle of
            StopBy -> liftIO $ do
                set <- readIORef (workerThreads chan)
                writeIORef (svarStopBy chan) $ Set.elemAt 0 set
            _ -> return ()
        toChannelD chan stream1
        return $ Stream.toStreamK $ fromChannel chan
{-
appendWithD stopStyle modifier stream1 stream2 =
    Stream.toStreamK $ Stream.unfold (Unfold.lmapM action readChannel) ()

    where

    action () = do
        chan <- newChannel stopStyle modifier
        toChannelD chan stream2
        case stopStyle of
            StopBy -> liftIO $ do
                set <- readIORef (workerThreads chan)
                writeIORef (svarStopBy chan) $ Set.elemAt 0 set
            _ -> return ()
        toChannelD chan stream1
        return chan
-}

{-# INLINE appendWithK #-}
appendWithK :: MonadAsync m =>
       SVarStopStyle
    -> (Config -> Config)
    -> K.Stream m a
    -> K.Stream m a
    -> K.Stream m a
appendWithK stopStyle modifier stream1 stream2 = K.concatEffect action

    where

    action = do
        chan <- newChannel stopStyle modifier
        toChannelK chan stream2
        case stopStyle of
            StopBy -> liftIO $ do
                set <- readIORef (workerThreads chan)
                writeIORef (svarStopBy chan) $ Set.elemAt 0 set
            _ -> return ()
        toChannelK chan stream1
        return $ fromChannelK chan

-- | Evaluate the first stream in the current thread and add the second stream
-- to the supplied channel. This is to be used by a worker thread.
--
-- This can be used with concatMapWith:
--
-- @
-- concatMap = K.concatMapWith (_appendWithChanK chan) f stream
-- @
--
{-# INLINE _appendWithChanK #-}
_appendWithChanK :: MonadAsync m =>
    Channel m a -> K.Stream m a -> K.Stream m a -> K.Stream m a
_appendWithChanK chan stream1 stream2 =
    K.before (toChannelK chan stream2) stream1

-------------------------------------------------------------------------------
-- concat
-------------------------------------------------------------------------------

-- | A runner function takes a queuing function @q@ and a stream, it splits the
-- input stream, queuing the tail and using the head to generate a stream.
-- 'mkEnqueue' takes a runner function and generates the queuing function @q@.
-- Note that @q@ and the runner are mutually recursive, mkEnqueue ties the knot
-- between the two.
{-# INLINE mkEnqueue #-}
mkEnqueue :: MonadAsync m =>
    Channel m b
    -> ((K.Stream m a -> m ()) -> K.Stream m a -> K.Stream m b)
    -> m (K.Stream m a -> m ())
mkEnqueue chan runner =
    return $ let q stream = toChannelK chan (runner q stream) in q

-- XXX Can be renamed to concatMapWithK if we move concatMapWithK to higher
-- level module. We can keep only Channel based ops in this module.

-- | Takes the head element of the input stream and queues the tail of the
-- stream to the channel, then maps the supplied function on the head and
-- evaluates the resulting stream.
--
-- This function is designed to be used by worker threads on a channel to
-- concurrently map and evaluate a stream.
{-# INLINE concatMapWithChanK #-}
concatMapWithChanK :: MonadAsync m =>
    Channel m b -> (a -> K.Stream m b) -> K.Stream m a -> K.Stream m b
concatMapWithChanK chan f stream =
   let run q = concatMapDivK q f
    in K.concatMapEffect (`run` stream) (mkEnqueue chan run)
    -- K.concatMapWith (_appendWithChanK chan) f stream

-- XXX Add a deep evaluation variant that evaluates individual elements in the
-- generated streams in parallel.

-- | Allocate a channel and use it to concurrently evaluate the streams
-- generated by the mapped function.
--
-- >> concatMapWithK modifier f = withChannelK (`concatMapWithChanK` f)  modifier
--
{-# INLINE concatMapWithK #-}
concatMapWithK :: MonadAsync m =>
    (Config -> Config) -> (a -> K.Stream m b) -> K.Stream m a -> K.Stream m b
concatMapWithK modifier f = withChannelK (`concatMapWithChanK` f)  modifier
