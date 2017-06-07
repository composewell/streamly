{-# LANGUAGE ScopedTypeVariables       #-}

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad.State
import Data.IORef

import Duct.Event

f :: StateT EventF IO String
f = do
    setData "x"
    Just x <- getData
    return x

runEvent :: forall m a. (Alternative m, MonadIO m) => StateT EventF m a -> m a
runEvent t = do
  zombieChan <- liftIO $ atomically newTChan
  pendingRef <- liftIO $ newIORef []
  credit     <- liftIO $ newIORef maxBound
  th         <- liftIO $ myThreadId

  (r, _) <- runStateT t $ initEventF
    (empty :: m a) th zombieChan pendingRef credit
  return r

main = do
    r <- runEvent f
    putStrLn r
