-- |
-- Module      : Streamly.Internal.Control.Concurrent
-- Copyright   : (c) 2017 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Note: This module is primarily for abstractions related to MonadBaseControl.
-- Please do not add any general routines in this. It should be renamed
-- appropriately.

module Streamly.Internal.Control.Concurrent
    (
      MonadAsync
    , RunInIO(..)
    , captureMonadState
    )
where

import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Control (MonadBaseControl, control, StM)

-- /Since: 0.8.0 ("Streamly.Prelude")/
--
-- | A monad that can perform concurrent or parallel IO operations. Streams
-- that can be composed concurrently require the underlying monad to be
-- 'MonadAsync'.
--
-- /Since: 0.1.0 ("Streamly")/
--
-- @since 0.8.0
type MonadAsync m = (MonadIO m, MonadBaseControl IO m, MonadThrow m)

newtype RunInIO m = RunInIO { runInIO :: forall b. m b -> IO (StM m b) }

-- | When we run computations concurrently, we completely isolate the state of
-- the concurrent computations from the parent computation.  The invariant is
-- that we should never be running two concurrent computations in the same
-- thread without using the runInIO function.  Also, we should never be running
-- a concurrent computation in the parent thread, otherwise it may affect the
-- state of the parent which is against the defined semantics of concurrent
-- execution.
captureMonadState :: MonadBaseControl IO m => m (RunInIO m)
captureMonadState = control $ \run -> run (return $ RunInIO run)
