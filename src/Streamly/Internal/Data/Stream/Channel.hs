-- |
-- Module      : Streamly.Internal.Data.Stream.Channel
-- Copyright   : (c) 2017 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

module Streamly.Internal.Data.Stream.Channel
    (
      module Streamly.Internal.Data.Stream.Channel.Type

    -- ** Allocation
    , module Streamly.Internal.Data.Stream.Channel.Append
    , module Streamly.Internal.Data.Stream.Channel.Interleave
    , newChannel

    -- ** Event Processing Loop
    , module Streamly.Internal.Data.Stream.Channel.Dispatcher
    , module Streamly.Internal.Data.Stream.Channel.Consumer
    , module Streamly.Internal.Data.Stream.Channel.Operations

    -- ** Evaluation
    , withChannelK
    , withChannel
    , chanConcatMapK
    -- quiesceChannel -- wait for running tasks but do not schedule any more.
    )
where

import Streamly.Internal.Control.Concurrent (MonadAsync)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Streamly.Internal.Data.Stream (Stream)
import Streamly.Internal.Control.Concurrent (askRunInIO)
import Streamly.Internal.Data.SVar.Type (adaptState)

import qualified Streamly.Internal.Data.StreamK as K

import Streamly.Internal.Data.Channel.Types
import Streamly.Internal.Data.Stream.Channel.Type
import Streamly.Internal.Data.Stream.Channel.Operations
import Streamly.Internal.Data.Stream.Channel.Append
import Streamly.Internal.Data.Stream.Channel.Interleave
import Streamly.Internal.Data.Stream.Channel.Dispatcher
import Streamly.Internal.Data.Stream.Channel.Consumer

-- | Create a new concurrent stream evaluation channel. The monad
-- state used to run the stream actions is captured from the call site of
-- newChannel.
{-# INLINE newChannel #-}
newChannel :: MonadAsync m =>
    (Config -> Config) -> m (Channel m a)
newChannel modifier =
    let cfg = modifier defaultConfig
     in if getInterleaved cfg
        then newInterleaveChannel modifier
        else newAppendChannel modifier

-- | Allocate a channel and evaluate the stream concurrently using the channel
-- and the supplied evaluator function. The evaluator is run in a worker
-- thread.
{-# INLINE withChannelK #-}
withChannelK :: MonadAsync m =>
       (Config -> Config) -- ^ config modifier
    -> K.StreamK m a -- ^ input stream
    -> (Channel m b -> K.StreamK m a -> K.StreamK m b) -- ^ stream evaluator
    -> K.StreamK m b -- ^ output stream
withChannelK modifier input evaluator = K.concatEffect action

    where

    action = do
        chan <- newChannel modifier
        toChannelK chan (evaluator chan input)
        return $ fromChannelK chan

-- | A wrapper over 'withChannelK', converts 'Stream' to 'StreamK' and invokes
-- 'withChannelK'.
{-# INLINE withChannel #-}
withChannel :: MonadAsync m =>
       (Config -> Config)
    -> Stream m a
    -> (Channel m b -> Stream m a -> Stream m b)
    -> Stream m b
withChannel modifier input evaluator =
    let f chan stream = K.fromStream $ evaluator chan (K.toStream stream)
     in K.toStream $ withChannelK modifier (K.fromStream input) f

-------------------------------------------------------------------------------
-- Evaluator
-------------------------------------------------------------------------------

-- | @concatMapHeadK consumeTail mapHead stream@, maps a stream generation
-- function on the head element and performs a side effect on the tail.
--
-- Used for concurrent evaluation of streams using a Channel. A worker
-- evaluating the stream would queue the tail and go on to evaluate the head.
-- The tail is picked up by another worker which does the same.
{-# INLINE concatMapHeadK #-}
concatMapHeadK :: Monad m =>
       (K.StreamK m a -> m ()) -- ^ Queue the tail
    -> (a -> K.StreamK m b) -- ^ Generate a stream from the head
    -> K.StreamK m a
    -> K.StreamK m b
concatMapHeadK consumeTail mapHead stream =
    K.mkStream $ \st yld sng stp -> do
        let foldShared = K.foldStreamShared st yld sng stp
            single a = foldShared $ mapHead a
            yieldk a r = consumeTail r >> single a
         in K.foldStreamShared (adaptState st) yieldk single stp stream

-------------------------------------------------------------------------------
-- concat streams
-------------------------------------------------------------------------------

-- | 'mkEnqueue chan f returns a queuing function @enq@. @enq@ takes a
-- @stream@ and enqueues @f enq stream@ on the channel. One example of @f@ is
-- 'concatMapHeadK'. When the enqueued value with 'concatMapHeadK' as @f@ is
-- evaluated, it generates an output stream from the head and enqueues @f enq
-- tail@ on the channel. Thus whenever the enqueued stream is evaluated it
-- generates a stream from the head and queues the tail on the channel.
--
-- Note that @enq@ and runner are mutually recursive, mkEnqueue ties the
-- knot between the two.
--
{-# INLINE mkEnqueue #-}
mkEnqueue :: MonadAsync m =>
    Channel m b
    -- | @divider enq stream@
    -> ((K.StreamK m a -> m ()) -> K.StreamK m a -> K.StreamK m b)
    -- | Queuing function @enq@
    -> m (K.StreamK m a -> m ())
mkEnqueue chan runner = do
    runInIO <- askRunInIO
    return
        $ let f stream = do
                liftIO $ enqueue chan (runInIO, runner f stream)
                -- XXX In case of eager dispatch we can just directly dispatch
                -- a worker with the tail stream here rather than first queuing
                -- and then dispatching a worker which dequeues the work. The
                -- older implementation did a direct dispatch here and its perf
                -- characterstics looked much better.
                eagerDispatch chan
           in f

{-# INLINE parConcatMapChanKAll #-}
parConcatMapChanKAll :: MonadAsync m =>
    Channel m b -> (a -> K.StreamK m b) -> K.StreamK m a -> K.StreamK m b
parConcatMapChanKAll chan f stream =
   let run q = concatMapHeadK q f
    in K.concatMapEffect (`run` stream) (mkEnqueue chan run)
    -- K.parConcatMap (_appendWithChanK chan) f stream

{-# INLINE parConcatMapChanKAny #-}
parConcatMapChanKAny :: MonadAsync m =>
    Channel m b -> (a -> K.StreamK m b) -> K.StreamK m a -> K.StreamK m b
parConcatMapChanKAny chan f stream =
   let done = K.nilM (shutdown chan)
       run q = concatMapHeadK q (\x -> K.append (f x) done)
    in K.concatMapEffect (`run` stream) (mkEnqueue chan run)

{-# INLINE parConcatMapChanKFirst #-}
parConcatMapChanKFirst :: MonadAsync m =>
    Channel m b -> (a -> K.StreamK m b) -> K.StreamK m a -> K.StreamK m b
parConcatMapChanKFirst chan f stream =
   let done = K.nilM (shutdown chan)
       run q = concatMapHeadK q f
    in K.concatEffect $ do
        res <- K.uncons stream
        case res of
            Nothing -> return K.nil
            Just (h, t) -> do
                q <- mkEnqueue chan run
                q t
                return $ K.append (f h) done

-- | Make a concurrent stream evaluator from a stream, to be used in
-- 'withChannelK' or 'toChannelK'. Maps a stream generation function on each
-- element of the stream, the evaluation of the map on each element happens
-- concurrently. All the generated streams are merged together in the output of
-- the channel. The scheduling and termination behavior depends on the channel
-- settings.
--
-- Note that if you queue a stream on the channel using 'toChannelK', it will
-- be picked up by a worker and the worker would evaluate the entire stream
-- serially and emit the results on the channel. However, if you transform the
-- stream using 'parConcatMapChanK' and queue it on the channel, it
-- parallelizes the function map on each element of the stream. The simplest
-- example is @parConcatMapChanK id id@ which is equivalent to evaluating each
-- element of the stream concurrently.
--
-- A channel worker evaluating this function would enqueue the tail on the
-- channel's work queue and go on to evaluate the head generating an output
-- stream. The tail is picked up by another worker which does the same and so
-- on.
{-# INLINE chanConcatMapK #-}
chanConcatMapK :: MonadAsync m =>
       (Config -> Config)
    -> Channel m b
    -> (a -> K.StreamK m b)
    -> K.StreamK m a
    -> K.StreamK m b
chanConcatMapK modifier chan f stream = do
        let cfg = modifier defaultConfig
        case getStopWhen cfg of
            AllStop -> parConcatMapChanKAll chan f stream
            FirstStops -> parConcatMapChanKFirst chan f stream
            AnyStops -> parConcatMapChanKAny chan f stream
