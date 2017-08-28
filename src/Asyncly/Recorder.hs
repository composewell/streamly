{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- |
-- Module      : Asyncly.Recorder
-- Copyright   : (c) 2017 Harendra Kumar
--
-- License     : MIT-style
-- Maintainer  : harendra.kumar@gmail.com
-- Stability   : experimental
-- Portability : GHC
--
module Asyncly.Recorder
    ( runAsynclyRecorded
--    , toListRecorded
    , playRecordings
    )
where

import           Control.Applicative         (Alternative (..))
import           Control.Monad               (liftM)
import           Control.Monad.Catch         (MonadThrow)
import           Control.Monad.IO.Class      (MonadIO (..))
import           Control.Monad.Trans.Class   (MonadTrans (lift))
import           Control.Monad.State         (StateT(..), runStateT)
import           Data.Monoid                 ((<>))
import           Data.IORef                  (IORef, newIORef, readIORef)

import           Control.Monad.Trans.Recorder (MonadRecorder(..), RecorderT,
                                               Recording, blank, runRecorderT)
import           Asyncly.AsyncT

------------------------------------------------------------------------------
-- Running the monad with recording
------------------------------------------------------------------------------

-- | Compose a computation using previously captured logs
playRecording :: (MonadAsync m, MonadRecorder m)
    => AsyncT m a -> Recording -> AsyncT m a
playRecording m recording = play recording >> m

-- | Resume an 'AsyncT' computation using previously recorded logs. The
-- recording consists of a list of journals one for each thread in the
-- computation.
playRecordings :: (MonadAsync m, MonadRecorder m)
    => AsyncT m a -> [Recording] -> AsyncT m a
playRecordings m logs = each logs >>= playRecording m

{-
-- | Run an 'AsyncT' computation with recording enabled, wait for it to finish
-- returning results for completed threads and recordings for paused threads.
toListRecorded :: (MonadAsync m, MonadCatch m)
    => AsyncT m a -> m ([a], [Recording])
toListRecorded m = do
    resultsRef <- liftIO $ newIORef []
    lref <- liftIO $ newIORef []
    waitAsync (gatherResult resultsRef) (Just lref) m
    res <- liftIO $ readIORef resultsRef
    logs <- liftIO $ readIORef lref
    return (res, logs)
    -}

-- | Run an 'AsyncT' computation with recording enabled, wait for it to finish
-- and discard the results and return the recordings for paused threads, if
-- any.
runAsynclyRecorded :: MonadAsync m => AsyncT (RecorderT m) a -> m [Recording]
runAsynclyRecorded m = do
    lref <- liftIO $ newIORef []
    runRecorderT blank (runAsynclyLogged (Just lref) m)
    logs <- liftIO $ readIORef lref
    return logs
