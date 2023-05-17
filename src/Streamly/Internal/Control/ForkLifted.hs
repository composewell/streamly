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
    , doForkWith
    , fork
    , forkManaged
    )
where

import Control.Concurrent (ThreadId, forkIO, forkOS)
import Control.Exception (SomeException(..), catch, mask)
import Data.Functor (void)
import Streamly.Internal.Control.Concurrent (MonadRunInIO, RunInIO(..), withRunInIO, withRunInIONoRestore)
import Streamly.Internal.Control.ForkIO (rawForkIO, forkManagedWith)

-- | Fork a thread to run the given computation, installing the provided
-- exception handler. Lifted to any monad with 'MonadRunInIO m'
-- capability.
--
-- TODO: the RunInIO argument can be removed, we can directly pass the action
-- as "mrun action" instead.
{-# INLINE doFork #-}
doFork :: MonadRunInIO m
    => m ()
    -> RunInIO m
    -> (SomeException -> IO ())
    -> m ThreadId
doFork = doForkWith False

-- | Similar to 'doFork', but has a \"bound\" boolean parameter for specifying
-- whether 'forkOS' should be used instead of 'rawForkIO'.
{-# INLINE doForkWith #-}
doForkWith :: MonadRunInIO m
    => Bool
    -> m ()
    -> RunInIO m
    -> (SomeException -> IO ())
    -> m ThreadId
doForkWith bound action (RunInIO mrun) exHandler =
    withRunInIO $ \run ->
        mask $ \restore -> do
                tid <- (if bound then forkOS else rawForkIO) $
                    catch (restore $ void $ mrun action)
                          exHandler
                run (return tid)

-- | 'fork' lifted to any monad with 'MonadBaseControl IO m' capability.
--
{-# INLINABLE fork #-}
fork :: MonadRunInIO m => m () -> m ThreadId
fork m = withRunInIONoRestore $ \run -> forkIO $ void $ run m

-- | Fork a thread that is automatically killed as soon as the reference to the
-- returned threadId is garbage collected.
--
{-# INLINABLE forkManaged #-}
forkManaged :: MonadRunInIO m => m () -> m ThreadId
forkManaged = forkManagedWith fork
