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
    , MonadRunInIO
    , RunInIO(..)
    , askRunInIO
    , withRunInIO
    , withRunInIONoRestore
    , restoreM
    )
where

import Control.Monad.Catch (MonadThrow)

#ifdef USE_UNLIFTIO
import Control.Monad.IO.Unlift (MonadUnliftIO(..), UnliftIO(..), askUnliftIO)
#else
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Control (MonadBaseControl(..), control)
#endif

#ifdef USE_UNLIFTIO
type MonadRunInIO m = MonadUnliftIO m
#else
type MonadRunInIO m = (MonadIO m, MonadBaseControl IO m)
#endif

-- /Since: 0.8.0 ("Streamly.Prelude")/
--
-- | A monad that can perform concurrent or parallel IO operations. Streams
-- that can be composed concurrently require the underlying monad to be
-- 'MonadAsync'.
--
-- /Since: 0.1.0 ("Streamly")/
--
-- @since 0.8.0
#ifdef USE_UNLIFTIO
type MonadAsync m = (MonadUnliftIO m, MonadThrow m)
#else
type MonadAsync m = (MonadIO m, MonadBaseControl IO m, MonadThrow m)
#endif


#ifdef USE_UNLIFTIO
newtype RunInIO m = RunInIO { runInIO :: forall b. m b -> IO b }
#else
newtype RunInIO m = RunInIO { runInIO :: forall b. m b -> IO (StM m b) }
#endif

-- | When we run computations concurrently, we completely isolate the state of
-- the concurrent computations from the parent computation.  The invariant is
-- that we should never be running two concurrent computations in the same
-- thread without using the runInIO function.  Also, we should never be running
-- a concurrent computation in the parent thread, otherwise it may affect the
-- state of the parent which is against the defined semantics of concurrent
-- execution.
askRunInIO :: MonadRunInIO m => m (RunInIO m)
#ifdef USE_UNLIFTIO
askRunInIO = fmap (\(UnliftIO run) -> RunInIO run) askUnliftIO
#else
askRunInIO = control $ \run -> run (return $ RunInIO run)
#endif

#ifdef USE_UNLIFTIO
withRunInIONoRestore :: MonadRunInIO m => ((forall a. m a -> IO a) -> IO b) -> m b
withRunInIONoRestore = withRunInIO

restoreM :: MonadRunInIO m => a -> m a
restoreM = return
#else
withRunInIO :: MonadRunInIO m => ((forall a. m a -> IO (StM m a)) -> IO (StM m b)) -> m b
withRunInIO = control

withRunInIONoRestore :: MonadRunInIO m => ((forall a. m a -> IO (StM m a)) -> IO b) -> m b
withRunInIONoRestore = liftBaseWith
#endif
