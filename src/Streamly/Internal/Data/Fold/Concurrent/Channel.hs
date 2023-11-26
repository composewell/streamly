-- |
-- Module      : Streamly.Internal.Data.Fold.Concurrent.Channel
-- Copyright   : (c) 2022 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

module Streamly.Internal.Data.Fold.Concurrent.Channel
    (
    module Streamly.Internal.Data.Fold.Concurrent.Channel.Type

    -- * Configuration
    , maxBuffer
    , boundThreads
    , inspect

    -- * Fold operations
    , parEval
    )
where

import Control.Concurrent (takeMVar)
import Control.Monad (void, when)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.IORef (writeIORef)
import Streamly.Internal.Control.Concurrent (MonadAsync)
import Streamly.Internal.Data.Fold (Fold(..), Step (..))
import Streamly.Internal.Data.Channel.Worker (sendWithDoorBell)
import Streamly.Internal.Data.Time.Clock (Clock(Monotonic), getTime)

import Streamly.Internal.Data.Fold.Concurrent.Channel.Type
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
            $ sendWithDoorBell
                (outputQueue chan)
                (outputDoorBell chan)
                ChildStopChannel
        status <- checkFoldStatus chan
        case status of
            Nothing -> do
                liftIO
                    $ withDiagMVar
                        (svarInspectMode chan)
                        (dumpSVar chan)
                        "parEval: waiting to drain"
                    $ takeMVar (outputDoorBellFromConsumer chan)
                -- XXX remove recursion
                final chan
            Just b -> do
                when (svarInspectMode chan) $ liftIO $ do
                    t <- getTime Monotonic
                    writeIORef (svarStopTime (svarStats chan)) (Just t)
                    printSVar (dumpSVar chan) "SVar Done"
                return b
