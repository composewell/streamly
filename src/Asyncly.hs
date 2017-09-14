-- |
-- Module      : Asyncly
-- Copyright   : (c) 2017 Harendra Kumar
--
-- License     : BSD3
-- Maintainer  : harendra.kumar@gmail.com
-- Stability   : experimental
-- Portability : GHC
--
--
-- Asyncly allows expressing and composing state machines and event driven
-- programs in a straightforward manner. It allows waiting for events to occur
-- in a non-blocking manner and process them asynchronously; multiple events
-- can be processed in parallel. Tasks can be easily split into smaller tasks,
-- processed in parallel and results combined.
--
-- The 'Alternative' composition @(\<|\>)@ is used to express asynchronous or
-- parallel tasks. The following example demonstrates generation and printing
-- of random numbers happening in parallel:
--
-- @
-- import Control.Applicative ((\<|\>))
-- import Control.Concurrent (threadDelay)
-- import Control.Monad.IO.Class (liftIO)
-- import System.Random (randomIO)
-- import Asyncly
--
-- main = wait_ $ do
--     x <- loop
--     liftIO $ print x
--
--     where
--
--     loop = do
--         liftIO $ threadDelay 1000000
--         x <- liftIO (randomIO :: IO Int)
--         return x \<|\> loop
-- @
--
-- Here two random number generation loops run in parallel so two numbers are
-- printed every second. Note that the threadId printed for each is different:
--
-- @
-- import Control.Applicative ((\<|\>))
-- import Control.Concurrent (myThreadId, threadDelay)
-- import Control.Monad.IO.Class (liftIO)
-- import System.IO (stdout, hSetBuffering, BufferMode(LineBuffering))
-- import System.Random (randomIO)
-- import Asyncly
--
-- main = wait_ $ do
--     liftIO $ hSetBuffering stdout LineBuffering
--     x <- loop \"A" \<|\> loop \"B"
--     liftIO $ myThreadId >>= putStr . show
--              >> putStr " "
--              >> print x
--
--     where
--
--     loop name = do
--         liftIO $ threadDelay 1000000
--         rnd <- liftIO (randomIO :: IO Int)
--         return (name, rnd) \<|\> loop name
-- @
-- Here the two loops are serially composed. For each value yielded by loop A,
-- loop B is executed. Four results are printed, all four run in separate
-- parallel threads. The composition is like ListT except that this is
-- concurrent:
--
-- @
-- import Control.Applicative ((\<|\>), empty)
-- import Control.Concurrent (myThreadId)
-- import Control.Monad.IO.Class (liftIO)
-- import System.IO (stdout, hSetBuffering, BufferMode(LineBuffering))
-- import System.Random (randomIO)
-- import Asyncly
--
-- main = wait_ $ do
--     liftIO $ hSetBuffering stdout LineBuffering
--     x <- loop "A " 2
--     y <- loop "B " 2
--     liftIO $ myThreadId >>= putStr . show
--              >> putStr " "
--              >> print (x, y)
--
--     where
--
--     loop name n = do
--         rnd <- liftIO (randomIO :: IO Int)
--         let result = (name ++ show rnd)
--             repeat = if n > 1 then loop name (n - 1) else empty
--          in (return result) \<|\> repeat
-- @
--
-- Here we perform a simple multi-threaded map-reduce by squaring each number
-- in a separate thread and then summing the squares:
--
-- @
-- import Control.Applicative ((\<|\>), empty)
-- import Data.List (sum)
-- import Asyncly
--
-- main = do
--     squares <- wait $ do
--         x <- foldl (\<|\>) empty $ map return [1..100]
--         return (x * x)
--     print . sum $ squares
-- @
--
-- 'Applicative' and 'Monoid' compositions work as expected.

-- It can be thought of as a non-deterministic continuation monad or a
-- combination of ContT and ListT. It allows to capture the state of the
-- application at any point and trigger arbitrary number of continuations from
-- the capture point. Non-determinism or parallel continuations are introduced
-- using the <|> operator. It provides a convenient way to implement and
-- compose state machines without using callbacks. An event in the state
-- machine corresponds to a continuation.  There are no cycles in the state
-- machine as each transition in the state machine is an independent instance
-- of the state machine. It is an immutable state machine!

module Asyncly
    ( AsyncT
    , MonadAsync

    -- * Running
    , runAsyncly
    , toList

    -- * Monadic Composition (Conjunction)
    -- $bind
    , (>->)
    , (>>|)
    , (>|>)

    -- * Monoidal Composition (Disjunction)
    -- $monoidal
    , (<=>)
    , (<|)

    -- * General Fold Utilities
    , foldWith
    , foldMapWith
    , forEachWith

    -- * Special folds
    , take
    , drop

    -- * Re-exports
    , Monoid (..)
    , Semigroup (..)
    , Alternative (..)
    , MonadPlus (..)
    , MonadIO (..)
    , MonadTrans (..)
    )
where

import Asyncly.AsyncT
import Data.Semigroup (Semigroup(..))
import Control.Applicative (Alternative(..))
import Control.Monad (MonadPlus(..))
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Class (MonadTrans (..))
import Prelude hiding (take, drop)

-- $monoidal
--
-- These combinators can be used in place of 'Monoid' ('<>') or 'Alternative'
-- ('<|>') composition to achieve the desired variant of monoidal composition.
--
-- $bind
--
-- These combinators can be used in place of the standard monadic bind ('>>=')
-- to achieve the desired variant of monadic composition.
