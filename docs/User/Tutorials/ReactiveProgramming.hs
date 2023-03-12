{-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- |
-- Module      : User.Tutorials.ReactiveProgramming
-- Copyright   : (c) 2017 Composewell Technologies
--
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
--
-- THIS TUTORIAL IS OBSOLETE.
--
-- In this tutorial we will show how Streamly can be used for reactive
-- programming.  Before you go through this tutorial we recommend that you take
-- a look at the Streamly concurrent programming tutorial.

module User.Tutorials.ReactiveProgramming
    (
    -- * Reactive Programming
    -- $reactive

    -- * Where to go next?
    -- $furtherReading
    )
where

import Streamly.Data.Stream
import Data.Semigroup
import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class      (MonadIO(..))
import Control.Monad.Trans.Class   (MonadTrans (lift))

-- $reactive
--
-- Reactive programming is nothing but concurrent streaming which is what
-- streamly is all about. With streamly we can generate streams of events,
-- merge streams that are generated concurrently and process events
-- concurrently. We can do all this without any knowledge about the specifics
-- of the implementation of concurrency. In the following example you will see
-- that the code is just regular Haskell code without much streamly APIs used
-- (active hyperlinks are the streamly APIs) and yet it is a reactive
-- application.
--
-- This application has two independent and concurrent sources of event
-- streams, @acidRain@ and @userAction@. @acidRain@ continuously generates
-- events that deteriorate the health of the character in the game.
-- @userAction@ can be "potion" or "quit". When the user types "potion" the
-- health improves and the game continues.
--
-- @
-- {-\# LANGUAGE FlexibleContexts \#-}
--
-- import "Streamly.Prelude" (MonadAsync, SerialT)
-- import "Streamly.Prelude" as Stream
-- import Control.Monad (void)
-- import Control.Monad.IO.Class (MonadIO(liftIO))
-- import Control.Monad.State (MonadState, get, modify, runStateT)
--
-- data Event = Quit | Harm Int | Heal Int deriving (Show)
--
-- userAction :: MonadAsync m => 'SerialT' m Event
-- userAction = Stream.'repeatM' $ liftIO askUser
--     where
--     askUser = do
--         command <- getLine
--         case command of
--             "potion" -> return (Heal 10)
--             "harm"   -> return (Harm 10)
--             "quit"   -> return Quit
--             _        -> putStrLn "Type potion or harm or quit" >> askUser
--
-- acidRain :: MonadAsync m => 'SerialT' m Event
-- acidRain = Stream.'fromAsync' $ Stream.'constRate' 1 $ Stream.'repeatM' $ liftIO $ return $ Harm 1
--
-- data Result = Check | Done
--
-- runEvents :: (MonadAsync m, MonadState Int m) => 'SerialT' m Result
-- runEvents = do
--     event \<- userAction \`Stream.'parallel'` acidRain
--     case event of
--         Harm n -> modify (\\h -> h - n) >> return Check
--         Heal n -> modify (\\h -> h + n) >> return Check
--         Quit -> return Done
--
-- data Status = Alive | GameOver deriving Eq
--
-- getStatus :: (MonadAsync m, MonadState Int m) => Result -> m Status
-- getStatus result =
--     case result of
--         Done  -> liftIO $ putStrLn "You quit!" >> return GameOver
--         Check -> do
--             h <- get
--             liftIO $ if (h <= 0)
--                      then putStrLn "You die!" >> return GameOver
--                      else putStrLn ("Health = " <> show h) >> return Alive
--
-- main :: IO ()
-- main = do
--     putStrLn "Your health is deteriorating due to acid rain, type \\\"potion\\\" or \\\"quit\\\""
--     let runGame = Stream.'drainWhile' (== Alive) $ Stream.'mapM' getStatus runEvents
--     void $ runStateT runGame 60
-- @
--
-- You can also find the source of this example in the streamly-examples repo
-- as <https://github.com/composewell/streamly-examples/tree/master/AcidRain.hs AcidRain.hs>.
-- It has been adapted from Gabriel's
-- <https://hackage.haskell.org/package/pipes-concurrency-2.0.8/docs/Pipes-Concurrent-Tutorial.html pipes-concurrency>
-- package.
-- This is much simpler compared to the pipes version because of the builtin
-- concurrency in streamly. You can also find a SDL based reactive programming
-- example adapted from Yampa in
-- <https://github.com/composewell/streamly-examples/tree/master/CirclingSquare.hs CirclingSquare.hs>.

-- $performance
--
-- Streamly is highly optimized for performance, it is designed for serious
-- high performing, concurrent and scalable applications. We have created the
-- <https://hackage.haskell.org/package/streaming-benchmarks streaming-benchmarks>
-- package which is specifically and carefully designed to measure the
-- performance of Haskell streaming libraries fairly and squarely in the right
-- way. Streamly performs at par or even better than most streaming libraries
-- for serial operations even though it needs to deal with the concurrency
-- capability.

-- $furtherReading
--
-- * See the examples in <https://github.com/composewell/streamly-examples streamly-examples> repo.
