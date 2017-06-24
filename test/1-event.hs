{-# LANGUAGE ScopedTypeVariables       #-}

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad.State
import Data.IORef

import Strands.Context

f :: StateT Context IO String
f = do
    setData "x"
    Just x <- getData
    return x

runEvent :: forall m a. (Alternative m, MonadIO m) => StateT Context m a -> m a
runEvent t = do
  zombieChan <- liftIO $ atomically newTChan
  pendingRef <- liftIO $ newIORef []
  credit     <- liftIO $ newIORef maxBound

  (r, _) <- runStateT t $ initContext
    (empty :: m a) zombieChan pendingRef credit
  return r

main = do
    r <- runEvent f
    putStrLn r
