-- |
-- Module      : Streamly.Internal.Data.Stream.Concurrent.Channel
-- Copyright   : (c) 2017 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

module Streamly.Internal.Data.Stream.Concurrent.Channel
    (
    -- * Channel
      Channel

    -- * Configuration
    , Config
    , maxThreads
    , maxBuffer
    , eagerEval
    , Rate(..)
    , rate
    , avgRate
    , minRate
    , maxRate
    , constRate
    , inspectMode
    , StopWhen (..)
    , stopWhen
    , ordered

    -- * Operations
    , fromChannel
    , toChannel
    , withChannelK
    , concatMapWithChanK

    -- XXX Move to the higher level Async module. Low level module would have
    -- only explicit channel operations.

    -- * Stream operations
    , evalWithK
    , appendWithK
    , interleaveWithK
    , concatMapWithK -- appendMap
    , concatMapInterleaveWithK -- interleaveMap
    )
where

import Control.Monad.IO.Class (MonadIO(liftIO))
import Streamly.Internal.Control.Concurrent (MonadAsync, askRunInIO)
import Streamly.Internal.Data.Stream.Concurrent.Channel.Operations
    (fromChannel, fromChannelK, toChannel, toChannelK)

import qualified Streamly.Internal.Data.Stream as Stream
import qualified Streamly.Internal.Data.Stream.Concurrent.Channel.Append as Append
import qualified Streamly.Internal.Data.Stream.Concurrent.Channel.Interleave
    as Interleave
import qualified Streamly.Internal.Data.Stream.StreamK.Type as K

import Streamly.Internal.Data.Stream.Concurrent.Channel.Type
import Streamly.Internal.Data.Stream.Channel.Types

-- | Allocate a channel and evaluate the stream using the channel and the
-- supplied evaluator function. The evaluator is run in a worker thread.
{-# INLINE withChannelK #-}
withChannelK :: MonadAsync m =>
       (Channel m b -> K.Stream m a -> K.Stream m b)
    -> (Config -> Config)
    -> K.Stream m a
    -> K.Stream m b
withChannelK evaluator modifier input = K.concatEffect action

    where

    action = do
        chan <- Append.newChannel modifier
        toChannelK chan (evaluator chan input)
        return $ fromChannelK chan

-- XXX Dedup with withChannelK

{-# INLINE withInterleaveChannelK #-}
withInterleaveChannelK :: MonadAsync m =>
       (Channel m b -> K.Stream m a -> K.Stream m b)
    -> (Config -> Config)
    -> K.Stream m a
    -> K.Stream m b
withInterleaveChannelK evaluator modifier input = K.concatEffect action

    where

    action = do
        chan <- Interleave.newChannel modifier
        toChannelK chan (evaluator chan input)
        return $ Stream.toStreamK $ fromChannel chan

-------------------------------------------------------------------------------
-- Evaluating a stream
-------------------------------------------------------------------------------

-- XXX This should go in the parallel module, this is not specific to async.

-- | Evaluate the stream asynchronously in a worker thread separate from the
-- consumer thread.
--
-- >> evalWithK = withChannelK (const id)
--
{-# INLINABLE evalWithK #-}
evalWithK :: MonadAsync m => (Config -> Config) -> K.Stream m a -> K.Stream m a
evalWithK = withChannelK (const id)

{-
{-# INLINE_NORMAL evalWithD #-}
evalWithD :: MonadAsync m => (Config -> Config) -> D.Stream m a -> D.Stream m a
evalWithD modifier m = D.Stream step Nothing
    where

    step _ Nothing = do
        chan <- newChannel modifier
        sendFirstWorker chan (D.toStreamK m)
        -- XXX should use an unfold to make this efficient
        return $ D.Skip $ Just $ fromChannelD chan

    step gst (Just (D.UnStream step1 st)) = do
        r <- step1 gst st
        return $ case r of
            D.Yield a s -> D.Yield a (Just $ D.Stream step1 s)
            D.Skip s    -> D.Skip (Just $ D.Stream step1 s)
            D.Stop      -> D.Stop
-}

-------------------------------------------------------------------------------
-- appending two streams
-------------------------------------------------------------------------------

{-# INLINE _appendGeneric #-}
_appendGeneric :: MonadAsync m =>
       ((Config -> Config) -> m (Channel m a))
    -> (Config -> Config)
    -> K.Stream m a
    -> K.Stream m a
    -> K.Stream m a
_appendGeneric newChan modifier stream1 stream2 = K.concatEffect action

    where

    action = do
        chan <- newChan modifier
        let cfg = modifier defaultConfig
            done = K.nilM (stopChannel chan)
        case getStopWhen cfg of
            AllStop -> do
                toChannelK chan stream2
                toChannelK chan stream1
            FirstStops -> do
                toChannelK chan stream2
                toChannelK chan (K.serial stream1 done)
            AnyStops -> do
                toChannelK chan (K.serial stream2 done)
                toChannelK chan (K.serial stream1 done)
        return $ Stream.toStreamK $ fromChannel chan

-- | Create a new channel and add both the streams to it for async evaluation.
-- The output stream is the result of the evaluation.
{-# INLINE appendWithK #-}
appendWithK :: MonadAsync m =>
    (Config -> Config) -> K.Stream m a -> K.Stream m a -> K.Stream m a
appendWithK modifier stream1 stream2 =
{-
    if getOrdered (modifier defaultConfig)
    then concatMapWithK modifier id (stream1 `K.cons` K.fromPure stream2)
    else _appendGeneric Append.newChannel modifier stream1 stream2
-}
    concatMapWithK modifier id (stream1 `K.cons` K.fromPure stream2)

-- | Create a new channel and add both the streams to it for async evaluation.
-- The output stream is the result of the evaluation.
{-# INLINE interleaveWithK #-}
interleaveWithK :: MonadAsync m =>
    (Config -> Config) -> K.Stream m a -> K.Stream m a -> K.Stream m a
-- interleaveWithK = _appendGeneric Interleave.newChannel
interleaveWithK modifier stream1 stream2 =
    concatMapInterleaveWithK modifier id (stream1 `K.cons` K.fromPure stream2)

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
mkEnqueue chan runner = do
    runInIO <- askRunInIO
    return
        $ let q stream = do
                -- Enqueue the outer loop
                liftIO $ enqueue chan False (runInIO, runner q stream)
                -- XXX In case of eager dispatch we can just directly dispatch
                -- a worker with the tail stream here rather than first queuing
                -- and then dispatching a worker which dequeues the work. The
                -- older implementation did a direct dispatch here and its perf
                -- characterstics looked much better.
                eagerDispatch chan
           in q

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

{-# INLINE concatMapWithChanKAny #-}
concatMapWithChanKAny :: MonadAsync m =>
    Channel m b -> (a -> K.Stream m b) -> K.Stream m a -> K.Stream m b
concatMapWithChanKAny chan f stream =
   let done = K.nilM (stopChannel chan)
       run q = concatMapDivK q (\x -> K.serial (f x) done)
    in K.concatMapEffect (`run` stream) (mkEnqueue chan run)

{-# INLINE concatMapWithChanKFirst #-}
concatMapWithChanKFirst :: MonadAsync m =>
    Channel m b -> (a -> K.Stream m b) -> K.Stream m a -> K.Stream m b
concatMapWithChanKFirst chan f stream =
   let done = K.nilM (stopChannel chan)
       run q = concatMapDivK q f
    in K.concatEffect $ do
        res <- K.uncons stream
        case res of
            Nothing -> return K.nil
            Just (h, t) -> do
                q <- mkEnqueue chan run
                q t
                return $ K.serial (f h) done

{-# INLINE concatMapWithChanKGeneric #-}
concatMapWithChanKGeneric :: MonadAsync m =>
       (Config -> Config)
    -> Channel m b
    -> (a -> K.Stream m b)
    -> K.Stream m a
    -> K.Stream m b
concatMapWithChanKGeneric modifier chan f stream = do
        let cfg = modifier defaultConfig
        case getStopWhen cfg of
            AllStop -> concatMapWithChanK chan f stream
            FirstStops -> concatMapWithChanKFirst chan f stream
            AnyStops -> concatMapWithChanKAny chan f stream

-- XXX Add a deep evaluation variant that evaluates individual elements in the
-- generated streams in parallel.

-- | Allocate a channel and use it to concurrently evaluate the streams
-- generated by the mapped function.
--
{-# INLINE concatMapWithK #-}
concatMapWithK :: MonadAsync m =>
    (Config -> Config) -> (a -> K.Stream m b) -> K.Stream m a -> K.Stream m b
concatMapWithK modifier f =
    let g = concatMapWithChanKGeneric modifier
     in withChannelK (`g` f) modifier

-- | Allocate a channel and use it to concurrently evaluate the streams
-- generated by the mapped function.
--
{-# INLINE concatMapInterleaveWithK #-}
concatMapInterleaveWithK :: MonadAsync m =>
    (Config -> Config) -> (a -> K.Stream m b) -> K.Stream m a -> K.Stream m b
concatMapInterleaveWithK modifier f =
    let g = concatMapWithChanKGeneric modifier
     in withInterleaveChannelK (`g` f) modifier
