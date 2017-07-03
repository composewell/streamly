{-# LANGUAGE FlexibleContexts #-}
import Control.Applicative
import Control.Concurrent (threadDelay, myThreadId)
import Control.Monad.IO.Class (liftIO, MonadIO)
import System.Random (randomIO)
import System.IO
import Control.Monad (forever, mzero)
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.IORef
import Control.Monad.Catch (MonadThrow)

import Asyncly

main = wait_ $ threads 0 $ do
    liftIO $ hSetBuffering stdout LineBuffering
    mainThread <- liftIO myThreadId
    liftIO $ putStrLn $ "Main thread: " ++ show mainThread

    x <- sample (randomIO :: IO Int) 1000000
    {-
    evThread <- liftIO myThreadId
    liftIO $ putStrLn $ "\nX Event thread: " ++ show evThread
    liftIO $ putStrLn $ "x = " ++ (show x)
    -}

    y <- sample (randomIO :: IO Int) 1000000

    -- liftIO $ threadDelay 10000000
    evThread <- liftIO myThreadId
    --liftIO $ putStrLn $ "\nY Event thread: " ++ show evThread
    liftIO $ putStrLn $ "(x,y) = " ++ (show (x,y))

sample :: (Eq a, MonadIO m, MonadBaseControl IO m, MonadThrow m)
    => IO a -> Int -> AsyncT m a
sample action interval = do
  v    <- async $ liftIO action
  prev <- liftIO $ newIORef v
  async (return v) <|> loop action prev
  where loop act prev = loop'
          where loop' = do
                  liftIO $ threadDelay interval
                  v' <- liftIO $ readIORef prev
                  v  <- liftIO act
                  if v /= v' then do
                    liftIO (writeIORef prev v)
                    async (return v) <|> loop'
                  else do
                    loop'
