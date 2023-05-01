-- |
-- Module      : Streamly.Internal.Data.Fold.Concurrent.Channel
-- Copyright   : (c) 2022 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

module Streamly.Internal.Data.Fold.Concurrent.Channel
    (
    -- * Channel
      Channel

    -- * Configuration
    , Config
    , maxBuffer
    , bound
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
import Streamly.Internal.Data.Stream.Channel.Worker (sendWithDoorBell)
import Streamly.Internal.Data.Time.Clock (Clock(Monotonic), getTime)

import Streamly.Internal.Data.Fold.Concurrent.Channel.Type
import Streamly.Internal.Data.Stream.Channel.Types

-------------------------------------------------------------------------------
-- Evaluating a Fold
-------------------------------------------------------------------------------

-- XXX Cleanup the fold if the stream is interrupted. Add a GC hook.

-- | Evaluate the fold asynchronously in a worker thread separate from the
-- driver thread.
--
{-# INLINABLE parEval #-}
parEval :: MonadAsync m => (Config -> Config) -> Fold m a b -> Fold m a b
parEval modifier f =
    Fold step initial extract

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
    step chan a = do
        status <- sendToWorker chan a
        return $ case status of
            Nothing -> Partial chan
            Just b -> Done b

    extract chan = do
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
                extract chan
            Just b -> do
                when (svarInspectMode chan) $ liftIO $ do
                    t <- getTime Monotonic
                    writeIORef (svarStopTime (svarStats chan)) (Just t)
                    printSVar (dumpSVar chan) "SVar Done"
                return b
