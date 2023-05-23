-- |
-- Module      : Streamly.Internal.Data.Fold.Async
-- Copyright   : (c) 2019 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
module Streamly.Internal.Data.Fold.Async
    (
    -- * Trimming
      takeInterval

    -- * Splitting
    , intervalsOf
    )
where

import Control.Concurrent (threadDelay, forkIO, killThread)
import Control.Concurrent.MVar (MVar, newMVar, swapMVar, readMVar)
import Control.Exception (SomeException(..), catch, mask)
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO(..))
import Streamly.Data.Fold (many)
import Streamly.Internal.Data.Fold (Fold(..), Step (..))
import Streamly.Internal.Control.Concurrent (MonadAsync, withRunInIO)
import Streamly.Internal.Data.Tuple.Strict (Tuple3'(..))

-- $setup
-- >>> :m
-- >>> :set -fno-warn-deprecations
-- >>> :set -XFlexibleContexts
-- >>> import qualified Streamly.Prelude as Stream
-- >>> import qualified Streamly.Data.Fold as Fold
-- >>> import qualified Streamly.Internal.Data.Fold.Async as Fold

-- XXX We can use asyncClock here. A parser can be used to return an input that
-- arrives after the timeout.
-- XXX If n is 0 return immediately in initial.
-- XXX we should probably discard the input received after the timeout like
-- takeEndBy_.

-- XXX The foldMany doctest is important for consistency with the "many" fold,
-- and intervalsOf. We should put it in tests.

-- | @takeInterval n fold@ uses @fold@ to fold the input items arriving within
-- a window of first @n@ seconds.
--
-- >>> input = Stream.delay 0.2 $ Stream.fromList [1..10]
-- >>> Stream.fold (Fold.takeInterval 1.0 Fold.toList) input
-- [1,2,3,4,5,6]
--
-- >>> f = Fold.takeInterval 0.5 Fold.toList
-- >>> Stream.fold Fold.toList $ Stream.foldMany f input
-- [[1,2,3,4],[5,6,7],[8,9,10]]
--
-- Stops when @fold@ stops or when the timeout occurs. Note that the fold needs
-- an input after the timeout to stop. For example, if no input is pushed to
-- the fold until one hour after the timeout had occurred, then the fold will
-- be done only after consuming that input.
--
-- /Pre-release/
--
{-# INLINE takeInterval #-}
takeInterval :: MonadAsync m => Double -> Fold m a b -> Fold m a b
takeInterval n (Fold step initial done final) =
    Fold step' initial' done' final'

    where

    initial' = do
        res <- initial
        case res of
            Partial s -> do
                mv <- liftIO $ newMVar False
                t <-
                    withRunInIO $ \run ->
                        mask $ \restore -> do
                            tid <-
                                forkIO
                                  $ catch
                                        (restore $ void $ run (timerThread mv))
                                        (handleChildException mv)
                            run (return tid)
                return $ Partial $ Tuple3' s mv t
            Done b -> return $ Done b

    step' (Tuple3' s mv t) a = do
        val <- liftIO $ readMVar mv
        if val
        then do
            res <- step s a
            case res of
                Partial sres -> Done <$> final sres
                Done bres -> return $ Done bres
        else do
            res <- step s a
            case res of
                Partial fs -> return $ Partial $ Tuple3' fs mv t
                Done b -> liftIO (killThread t) >> return (Done b)

    done' (Tuple3' s _ _) = done s

    final' (Tuple3' s _ _) = final s

    timerThread mv = do
        liftIO $ threadDelay (round $ n * 1000000)
        -- Use IORef + CAS? instead of MVar since its a Bool?
        liftIO $ void $ swapMVar mv True

    handleChildException :: MVar Bool -> SomeException -> IO ()
    handleChildException mv _ = void $ swapMVar mv True

-- For example, we can copy and distribute a stream to multiple folds where
-- each fold can group the input differently e.g. by one second, one minute and
-- one hour windows respectively and fold each resulting stream of folds.

-- XXX This needs to be fixed like intervalsOf in Data.Stream.Time.

-- | Group the input stream into windows of n second each using the first fold
-- and then fold the resulting groups using the second fold.
--
-- >>> intervals = Fold.intervalsOf 0.5 Fold.toList Fold.toList
-- >>> Stream.fold intervals $ Stream.delay 0.2 $ Stream.fromList [1..10]
-- [[1,2,3,4],[5,6,7],[8,9,10]]
--
-- > intervalsOf n split = many (takeInterval n split)
--
-- /Pre-release/
--
{-# INLINE intervalsOf #-}
intervalsOf :: MonadAsync m => Double -> Fold m a b -> Fold m b c -> Fold m a c
intervalsOf n split = many (takeInterval n split)
