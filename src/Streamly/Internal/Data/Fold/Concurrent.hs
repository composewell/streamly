-- |
-- Module      : Streamly.Internal.Data.Fold.Concurrent
-- Copyright   : (c) 2022 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Using 'parEval' a fold can be decoupled from the driver and evaluated
-- concurrently with the driver. The driver just pushes an element to the
-- fold's buffer and waits for async evaluation to finish.
--
-- Avoid scanning a stream using a concurrent fold. When scanning a stream
-- using a concurrent fold we need to keep in mind that the result of the scan
-- may be delayed because of the asynchronous execution. The results may not be
-- same as in the case of a synchronous fold.
--
-- Stages in a fold pipeline can be made concurrent using 'parEval'.
--
-- The 'demux' combinator can be made concurrent by using 'parEval' on the fold
-- returned by the fold-generating function. Thus, we can fold values for each
-- key in the input stream concurrently.
--
-- Similarly, we can use 'parEval' with other cobminators like 'toMap',
-- 'demuxToMap', 'classify', 'tee', 'distribute', 'partition' etc. Basically,
-- any combinator that composes multiple folds or multiple instances of a fold
-- is a good candidate for running folds concurrently.

-- TODO:
--
-- Before a fold returns "done" it has to drain the child folds. For example,
-- consider a "take" operation on a `parEval` fold, the take would return as
-- soon as it has taken required number of elements irrespective of whether the
-- child fold has yet finished or not.
--
-- parLmapM on a fold.
--
-- Use multiple worker folds to fold serial chunks of a stream and collect the
-- results using another fold, combine using a monoid. The results can be
-- collected out-of-order or in-order. This would be easier if each input
-- element is a streamable chunk and each fold consumes one at a time. This is
-- like parConcatMap in streams. We also need to have a lconcatMap to expand
-- the chunks in the input to streams before folding. This will require an
-- input Skip constructor. In fact, parLmapM would be implemented in terms of
-- this like in streams.
--
-- Concurrent append: if one fold's buffer becomes full then use the next one
-- Concurrent interleave/partition: Round robin to n folds.
-- Concurrent distribute to multiple folds.

module Streamly.Internal.Data.Fold.Concurrent
    (
      parEval
    )
where

import Control.Concurrent (takeMVar)
import Control.Monad (void, when)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.IORef (writeIORef)
import Streamly.Internal.Control.Concurrent (MonadAsync)
import Streamly.Internal.Data.Fold (Fold(..), Step (..))
import Streamly.Internal.Data.Channel.Worker (sendEvent)
import Streamly.Internal.Data.Time.Clock (Clock(Monotonic), getTime)

import Streamly.Internal.Data.Fold.Channel.Type
import Streamly.Internal.Data.Channel.Types

-------------------------------------------------------------------------------
-- Evaluating a Fold
-------------------------------------------------------------------------------

-- XXX Cleanup the fold if the stream is interrupted. Add a GC hook.

-- | Evaluate a fold asynchronously using a concurrent channel. The driver just
-- queues the input stream values to the fold channel buffer and returns. The
-- fold evaluates the queued values asynchronously. On finalization, 'parEval'
-- waits for the asynchronous fold to complete before it returns.
--
{-# INLINABLE parEval #-}
parEval :: MonadAsync m => (Config -> Config) -> Fold m a b -> Fold m a b
parEval modifier f =
    Fold step initial extract final

    where

    -- XXX Supply an output channel to the fold. The fold would send the result
    -- from each step (i.e. scan result) to the channel. The Partial and Done
    -- constructors are sent to the channel. We then draw the resulting stream
    -- from that channel. Kind of concurrrent mapping on the stream but with a
    -- fold/scan.
    --
    -- There can also be a model where multiple folds pick input from the same
    -- channel.
    --
    -- We can also run parsers this way. So instead of sending output on each
    -- step it can send once it is done.
    initial = Partial <$> newChannel modifier f

    -- XXX This is not truly asynchronous. If the fold is done we only get to
    -- know when we send the next input unless the stream ends. We could
    -- potentially throw an async exception to the driver to inform it
    -- asynchronously. Alternatively, the stream should not block forever, it
    -- should keep polling the fold status. We can insert a timer tick in the
    -- input stream to do that.
    --
    -- A polled stream abstraction may be useful, it would consist of normal
    -- events and tick events, latter are guaranteed to arrive.
    --
    -- XXX We can use the config to indicate if the fold is a scanning type or
    -- one-shot, or use a separate parEvalScan for scanning. For a scanning
    -- type fold the worker would always send the intermediate values back to
    -- the driver. An intermediate value can be returned on an input, or the
    -- driver can poll even without input, if we have the Skip input support.
    -- When the buffer is full we can return "Skip" and then the next step
    -- without input can wait for an output to arrive. Similarly, when "final"
    -- is called it can return "Skip" to continue or "Done" to indicate
    -- termination.
    step chan a = do
        status <- sendToWorker chan a
        return $ case status of
            Nothing -> Partial chan
            Just b -> Done b

    -- XXX We can use a separate type for non-scanning folds that will
    -- introduce a lot of complexity. Are there combinators that rely on the
    -- "extract" function even in non-scanning use cases?
    -- Instead of making such folds partial we can also make them return a
    -- Maybe type.
    extract _ = error "Concurrent folds do not support scanning"

    -- XXX depending on the use case we may want to either wait for the result
    -- or cancel the ongoing work. We can use the config to control that?
    -- Currently it waits for the work to complete.
    final chan = do
        liftIO $ void
            $ sendEvent
                (inputQueue chan)
                (inputItemDoorBell chan)
                ChildStopChannel
        status <- checkFoldStatus chan
        case status of
            Nothing -> do
                liftIO
                    $ withDiagMVar
                        (svarInspectMode chan)
                        (dumpChannel chan)
                        "parEval: waiting to drain"
                    $ takeMVar (outputDoorBell chan)
                -- XXX remove recursion
                final chan
            Just b -> do
                when (svarInspectMode chan) $ liftIO $ do
                    t <- getTime Monotonic
                    writeIORef (svarStopTime (svarStats chan)) (Just t)
                    printSVar (dumpChannel chan) "SVar Done"
                return b
