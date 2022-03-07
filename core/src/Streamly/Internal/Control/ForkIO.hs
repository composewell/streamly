{-# LANGUAGE UnboxedTuples #-}

-- |
-- Module      : Streamly.Internal.Control.ForkIO
-- Copyright   : (c) 2017 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

module Streamly.Internal.Control.ForkIO
    ( rawForkIO
    , forkIOManaged
    , forkManagedWith
    )
where

import Control.Concurrent (ThreadId, forkIO, killThread)
import Control.Monad.IO.Class (MonadIO(..))
import GHC.Conc (ThreadId(..))
import GHC.Exts
import GHC.IO (IO(..))
import System.Mem.Weak (addFinalizer)

-- | Stolen from the async package. The perf improvement is modest, 2% on a
-- thread heavy benchmark (parallel composition using noop computations).
-- A version of forkIO that does not include the outer exception
-- handler: saves a bit of time when we will be installing our own
-- exception handler.
{-# INLINE rawForkIO #-}
rawForkIO :: IO () -> IO ThreadId
rawForkIO (IO action) = IO $ \ s ->
   case fork# action s of (# s1, tid #) -> (# s1, ThreadId tid #)

-- | Fork a thread that is automatically killed as soon as the reference to the
-- returned threadId is garbage collected.
--
{-# INLINABLE forkManagedWith #-}
forkManagedWith :: MonadIO m => (m () -> m ThreadId) -> m () -> m ThreadId
forkManagedWith fork action = do
    tid <- fork action
    liftIO $ addFinalizer tid (killThread tid)
    return tid

-- | Fork a thread that is automatically killed as soon as the reference to the
-- returned threadId is garbage collected.
--
{-# INLINABLE forkIOManaged #-}
forkIOManaged :: IO () -> IO ThreadId
forkIOManaged = forkManagedWith forkIO
