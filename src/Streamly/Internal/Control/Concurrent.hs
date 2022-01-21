{-# LANGUAGE UnboxedTuples #-}

-- |
-- Module      : Streamly.Internal.Control.Concurrent
-- Copyright   : (c) 2017 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

module Streamly.Internal.Control.Concurrent
    (
      MonadAsync
    , RunInIO(..)
    , captureMonadState
    , doFork
    , fork
    , forkManaged
    )
where

import Control.Concurrent (ThreadId, forkIO, killThread)
import Control.Exception (SomeException(..), catch, mask)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Control
       (MonadBaseControl, control, StM, liftBaseDiscard)
import Data.Functor (void)
import GHC.Conc (ThreadId(..))
import GHC.Exts
import GHC.IO (IO(..))
import System.Mem.Weak (addFinalizer)

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

-- Stolen from the async package. The perf improvement is modest, 2% on a
-- thread heavy benchmark (parallel composition using noop computations).
-- A version of forkIO that does not include the outer exception
-- handler: saves a bit of time when we will be installing our own
-- exception handler.
{-# INLINE rawForkIO #-}
rawForkIO :: IO () -> IO ThreadId
rawForkIO (IO action) = IO $ \ s ->
   case fork# action s of (# s1, tid #) -> (# s1, ThreadId tid #)

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
forkManaged action = do
    tid <- fork action
    liftIO $ addFinalizer tid (killThread tid)
    return tid
