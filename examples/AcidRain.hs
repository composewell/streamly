{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- This example is adapted from Gabriel Gonzalez's pipes-concurrency package.
-- https://hackage.haskell.org/package/pipes-concurrency-2.0.8/docs/Pipes-Concurrent-Tutorial.html

import Streamly
import Streamly.Prelude as S
import Control.Concurrent.STM (TVar, atomically, newTVarIO, readTVar, writeTVar)
import Control.Monad (when)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.IO.Unlift -- (MonadUnliftIO(askUnliftIO))
import Control.Monad.Reader (ReaderT(..))
import Control.Monad.State (MonadState, get, modify, runStateT, state)
import Data.Coerce (coerce)

data Event = Harm Int | Heal Int | Quit deriving (Show)

newtype GameMonad a = GameMonad { unGameMonad :: ReaderT (TVar Int) IO a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadThrow)

instance MonadState Int GameMonad where
  state f = GameMonad $ ReaderT $ \healthVar -> do
    atomically $ do
      health <- readTVar healthVar
      let (x, health') = f health
      writeTVar healthVar health'
      return x

instance MonadUnliftIO GameMonad where
  askUnliftIO = GameMonad $ do
    UnliftIO f <- askUnliftIO
    pure (UnliftIO (\m -> f (unGameMonad m)))

userAction :: MonadAsync m => SerialT m Event
userAction = S.repeatM $ liftIO askUser
    where
    askUser = do
        command <- getLine
        case command of
            "potion" -> return (Heal 10)
            "quit"   -> return  Quit
            _        -> putStrLn "What?" >> askUser

acidRain :: MonadAsync m => SerialT m Event
acidRain = asyncly $ constRate 1 $ S.repeatM $ liftIO $ return $ Harm 1

game :: (MonadAsync m, MonadState Int m) => SerialT m ()
game = do
    event <- userAction `parallel` acidRain
    case event of
        Harm n -> modify $ \h -> h - n
        Heal n -> modify $ \h -> h + n
        Quit   -> fail "quit"

    h <- get
    when (h <= 0) $ fail "You die!"
    liftIO $ putStrLn $ "Health = " ++ show h

main :: IO ()
main = do
    putStrLn "Your health is deteriorating due to acid rain,\
             \ type \"potion\" or \"quit\""
    healthVar <- newTVarIO 60
    _ <- runReaderT (unGameMonad (runStream game)) healthVar
    return ()
