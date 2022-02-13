-- |
-- Module      : Streamly.Internal.Control.ForkLifted
-- Copyright   : (c) 2017 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

module Streamly.Internal.Control.ForkLifted
    (
      doFork
    , fork
    , forkManaged
    )
where

import Control.Concurrent (ThreadId, forkIO)
import Control.Exception (SomeException(..), catch, mask)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Control (MonadBaseControl, control, liftBaseDiscard)
import Data.Functor (void)
import Streamly.Internal.Control.Concurrent (RunInIO(..))
import Streamly.Internal.Control.ForkIO (rawForkIO, forkManagedWith)

-- | Fork a thread to run the given computation, installing the provided
-- exception handler. Lifted to any monad with 'MonadBaseControl IO m'
-- capability.
--
-- TODO: the RunInIO argument can be removed, we can directly pass the action
-- as "mrun action" instead.
{-# INLINE doFork #-}
doFork :: MonadBaseControl IO m
    => m ()
    -> RunInIO m
    -> (SomeException -> IO ())
    -> m ThreadId
doFork action (RunInIO mrun) exHandler =
    control $ \run ->
        mask $ \restore -> do
                tid <- rawForkIO $ catch (restore $ void $ mrun action)
                                         exHandler
                run (return tid)

-- | 'fork' lifted to any monad with 'MonadBaseControl IO m' capability.
--
{-# INLINABLE fork #-}
fork :: MonadBaseControl IO m => m () -> m ThreadId
fork = liftBaseDiscard forkIO

-- | Fork a thread that is automatically killed as soon as the reference to the
-- returned threadId is garbage collected.
--
{-# INLINABLE forkManaged #-}
forkManaged :: (MonadIO m, MonadBaseControl IO m) => m () -> m ThreadId
forkManaged = forkManagedWith fork
