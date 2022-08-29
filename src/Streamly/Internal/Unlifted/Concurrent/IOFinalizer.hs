-- |
-- Module      : Streamly.Internal.Data.IOFinalizer
-- Copyright   : (c) 2020 Composewell Technologies and Contributors
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- A value associated with an IO action that is automatically called whenever
-- the value is garbage collected.

module Streamly.Internal.Unlifted.Concurrent.IOFinalizer
    (
      IOFinalizer
    , newIOFinalizer
    , runIOFinalizer
    , clearingIOFinalizer
    )
where

import Control.Exception (mask_)
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO(..))
import Data.IORef (newIORef, readIORef, mkWeakIORef, writeIORef, IORef)
import Streamly.Internal.Control.Concurrent (MonadRunInIO, askRunInIO, runInIO, withRunInIO)

-- | An 'IOFinalizer' has an associated IO action that is automatically called
-- whenever the finalizer is garbage collected. The action can be run and
-- cleared prematurely.
--
-- You can hold a reference to the finalizer in your data structure, if the
-- data structure gets garbage collected the finalizer will be called.
--
-- It is implemented using 'mkWeakIORef'.
--
-- /Pre-release/
newtype IOFinalizer = IOFinalizer (IORef (Maybe (IO ())))

-- | Make a finalizer from a monadic action @m a@ that can run in IO monad.
mkIOFinalizer :: MonadRunInIO m => m b -> m (IO ())
mkIOFinalizer f = do
    mrun <- askRunInIO
    return $
        void $ do
            _ <- runInIO mrun f
            return ()

-- | GC hook to run an IO action stored in a finalized IORef.
runFinalizerGC :: IORef (Maybe (IO ())) -> IO ()
runFinalizerGC ref = do
    res <- readIORef ref
    case res of
        Nothing -> return ()
        Just f -> f

-- | Create a finalizer that calls the supplied function automatically when the
-- it is garbage collected.
--
-- /The finalizer is always run using the state of the monad that is captured
-- at the time of calling 'newFinalizer'./
--
-- Note: To run it on garbage collection we have no option but to use the monad
-- state captured at some earlier point of time.  For the case when the
-- finalizer is run manually before GC we could run it with the current state
-- of the monad but we want to keep both the cases consistent.
--
-- /Pre-release/
newIOFinalizer :: MonadRunInIO m => m a -> m IOFinalizer
newIOFinalizer finalizer = do
    f <- mkIOFinalizer finalizer
    ref <- liftIO $ newIORef $ Just f
    _ <- liftIO $ mkWeakIORef ref (runFinalizerGC ref)
    return $ IOFinalizer ref

-- | Run the action associated with the finalizer and deactivate it so that it
-- never runs again.  Note, the finalizing action runs with async exceptions
-- masked.
--
-- /Pre-release/
runIOFinalizer :: MonadIO m => IOFinalizer -> m ()
runIOFinalizer (IOFinalizer ref) = liftIO $ do
    res <- readIORef ref
    case res of
        Nothing -> return ()
        Just action -> do
            -- if an async exception comes after writing 'Nothing' then the
            -- finalizing action will never be run. We need to do this
            -- atomically wrt async exceptions.
            mask_ $ do
                writeIORef ref Nothing
                action

-- | Run an action clearing the finalizer atomically wrt async exceptions. The
-- action is run with async exceptions masked.
--
-- /Pre-release/
clearingIOFinalizer :: MonadRunInIO m => IOFinalizer -> m a -> m a
clearingIOFinalizer (IOFinalizer ref) action = do
    withRunInIO $ \runinio ->
        mask_ $ do
            writeIORef ref Nothing
            runinio action