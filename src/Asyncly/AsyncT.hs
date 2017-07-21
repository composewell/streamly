{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE InstanceSigs              #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE Rank2Types                #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE UndecidableInstances      #-}

-- |
-- Module      : Asyncly.AsyncT
-- Copyright   : (c) 2017 Harendra Kumar
--
-- License     : MIT-style
-- Maintainer  : harendra.kumar@gmail.com
-- Stability   : experimental
-- Portability : GHC
--
--
module Asyncly.AsyncT
    ( AsyncT (..)

    , threads
    , async
    , makeAsync
    , each

    , before
    , (*>>)
    , thereafter
    , (>>*)
    , afterfirst
    , (>>|)

    -- internal
    , dbg
    )
where

import           Control.Applicative         (Alternative (..))
import           Control.Monad               (ap, mzero, when)
import           Control.Monad.Base          (MonadBase (..), liftBaseDefault)
import           Control.Monad.Catch         (MonadThrow, throwM)
import           Control.Monad.State         (MonadIO (..), MonadPlus (..),
                                              StateT (..), liftM, runStateT,
                                              modify)
import           Control.Monad.Trans.Class   (MonadTrans (lift))
import           Control.Monad.Trans.Control (ComposeSt, MonadBaseControl (..),
                                              MonadTransControl (..),
                                              defaultLiftBaseWith,
                                              defaultRestoreM, liftBaseWith)
import           Data.IORef                  (newIORef, readIORef, writeIORef)
import           Unsafe.Coerce               (unsafeCoerce)

import           Control.Monad.Trans.Recorder (MonadRecorder(..))
--import           Debug.Trace (traceM)

import Asyncly.Threads

newtype AsyncT m a = AsyncT { runAsyncT :: StateT Context m (Maybe a) }

------------------------------------------------------------------------------
-- Utilities
------------------------------------------------------------------------------

-- Toggle the dbg definition for debug traces
--dbg :: Monad m => String -> m ()
--dbg = traceM
dbg :: Monad m => a -> m ()
dbg _ = return ()

------------------------------------------------------------------------------
-- Monad
------------------------------------------------------------------------------

instance MonadAsync m => Monad (AsyncT m) where
    {-# SPECIALIZE instance Monad (AsyncT IO) #-}
    return a = AsyncT . return $ Just a
    m >>= f = bind

        where

        {-# NOINLINE[1] bind #-}
        bind = AsyncT $ do
            modify $ \Context {continuations = fs, ..} ->
                 Context {continuations = (unsafeCoerce f) : fs, ..}
            x <- runAsyncT m
            modify $ \Context {continuations = (_ : fs), ..} ->
                Context {continuations = fs, ..}
            case x of
                Just y -> runAsyncT $ f y
                Nothing -> return Nothing

------------------------------------------------------------------------------
-- Functor
------------------------------------------------------------------------------

instance MonadAsync m => Functor (AsyncT m) where
    fmap = liftM

------------------------------------------------------------------------------
-- Applicative
------------------------------------------------------------------------------

instance MonadAsync m => Applicative (AsyncT m) where
    pure  = return
    (<*>) = ap

------------------------------------------------------------------------------
-- Alternative
------------------------------------------------------------------------------

-- Thread pool vs transient threads
--
-- The current model is to start a new thread for every task. The computation
-- input is provided at the time of the creation and therefore no
-- synchronization is needed compared to a pool of threads contending to get
-- the input from a channel. However the thread creation overhead may be more
-- than the synchronization cost needed in the case of pool?
--
-- When the task is over, the outputs need to be collected and that requires
-- synchronization irrespective of a thread pool model or per task new thread
-- model.
--
-- XXX instead of starting a new thread every time, reuse the existing child
-- threads and send them work via a shared channel. When there is no more work
-- available we need a way to close the channel and wakeup all waiters so that
-- they can go away rather than waiting indefinitely.

instance MonadAsync m => Alternative (AsyncT m) where
    {-# SPECIALIZE instance Alternative (AsyncT IO) #-}
    empty = AsyncT $ return Nothing
    (<|>) m1 m2 = AsyncT $ do
        r <- runAsyncTask False (runAsyncT m1)
        maybe (runAsyncT m2) (return . Just) r

instance MonadAsync m => MonadPlus (AsyncT m) where
    mzero = empty
    mplus = (<|>)

instance (Monoid a, MonadAsync m) => Monoid (AsyncT m a) where
    mappend x y = mappend <$> x <*> y
    mempty      = return mempty

-- scatter
{-# SPECIALIZE each :: [a] -> AsyncT IO a #-}
each :: (MonadIO m, MonadBaseControl IO m, MonadThrow m)
    => [a] -> AsyncT m a
each xs = foldr (<|>) empty $ map return xs

------------------------------------------------------------------------------
-- Num
------------------------------------------------------------------------------

instance (Num a, Monad (AsyncT m)) => Num (AsyncT m a) where
  fromInteger = return . fromInteger
  mf + mg     = (+) <$> mf <*> mg
  mf * mg     = (*) <$> mf <*> mg
  negate f    = f >>= return . negate
  abs f       = f >>= return . abs
  signum f    = f >>= return . signum

-------------------------------------------------------------------------------
-- AsyncT transformer
-------------------------------------------------------------------------------

instance MonadTrans AsyncT where
    lift mx = AsyncT $ lift mx >>= return . Just

instance (MonadBase b m, MonadAsync m) => MonadBase b (AsyncT m) where
    liftBase = liftBaseDefault

-------------------------------------------------------------------------------
-- monad-control
-------------------------------------------------------------------------------

instance MonadTransControl AsyncT where
    type StT AsyncT a = (Maybe a, Context)
    liftWith f = AsyncT $ StateT $ \s ->
                   liftM (\x -> (Just x, s))
                         (f $ \t -> runStateT (runAsyncT t) s)
    restoreT = AsyncT . StateT . const
    {-# INLINABLE liftWith #-}
    {-# INLINABLE restoreT #-}

instance (MonadBaseControl b m, MonadAsync m) => MonadBaseControl b (AsyncT m) where
    type StM (AsyncT m) a = ComposeSt AsyncT m a
    liftBaseWith = defaultLiftBaseWith
    restoreM     = defaultRestoreM
    {-# INLINABLE liftBaseWith #-}
    {-# INLINABLE restoreM #-}

------------------------------------------------------------------------------
-- Standard transformer instances
------------------------------------------------------------------------------

instance MonadAsync m => MonadIO (AsyncT m) where
    liftIO = lift . liftIO

instance MonadAsync m => MonadThrow (AsyncT m) where
    throwM = lift . throwM

------------------------------------------------------------------------------
-- MonadRecorder
------------------------------------------------------------------------------

instance (MonadAsync m, MonadRecorder m) => MonadRecorder (AsyncT m) where
    getJournal = lift getJournal
    putJournal = lift . putJournal
    play = lift . play

------------------------------------------------------------------------------
-- Special compositions
------------------------------------------------------------------------------

-- | Run @m a@ before running @m b@. The result of @m a@ is discarded.
before :: MonadAsync m => AsyncT m a -> AsyncT m b -> AsyncT m b
before ma mb = AsyncT $ do
    _ <- runAsyncT (ma >> mzero)
    runAsyncT mb

infixr 1 *>>
-- | Same as 'before'.
(*>>) :: MonadAsync m => AsyncT m a -> AsyncT m b -> AsyncT m b
(*>>) = before

-- | Run @m b@ after running @m a@. The result of @m b@ is discarded.
thereafter :: MonadAsync m => AsyncT m a -> AsyncT m b -> AsyncT m a
thereafter ma mb = AsyncT $ do
    a <- runAsyncT ma
    _ <- runAsyncT (mb >> mzero)
    return a

infixr 1 >>*
-- | Same as 'thereafter'.
(>>*) :: MonadAsync m => AsyncT m a -> AsyncT m b -> AsyncT m a
(>>*) = thereafter

-- XXX This can be moved to utility functions as it is purely app level
-- | Run @m b@ right after the first event in @m a@ is generated but before it
-- is yielded. The result of @m b@ is discarded.
afterfirst :: MonadAsync m => AsyncT m a -> AsyncT m b -> AsyncT m a
afterfirst ma mb = do
    ref <- liftIO $ newIORef False
    x <- ma
    done <- liftIO $ readIORef ref
    when (not done) $ (liftIO $ writeIORef ref True) >>* mb
    return x

infixr 1 >>|
-- | Same as 'afterfirst'.
(>>|) :: MonadAsync m => AsyncT m a -> AsyncT m b -> AsyncT m a
(>>|) = afterfirst

------------------------------------------------------------------------------
-- Async primitives
------------------------------------------------------------------------------
--
-- Only those actions that are marked with 'async' are guaranteed to be
-- asynchronous. Asyncly is free to run other actions synchronously or
-- asynchronously and it should not matter to the semantics of the program, if
-- it does then use async to force.
--
-- Why not make async as default and ask the programmer to use a 'sync'
-- primitive to force an action to run synchronously? But then we would not
-- have the freedom to convert the async actions to sync dynamically. Note that
-- it is safe to convert a sync action to async but vice-versa is not true.
-- Converting an async to sync can cause change in semantics if the async
-- action was an infinite loop for example.
--
-- | In an 'Alternative' composition, force the action to run asynchronously.
-- The @\<|\>@ operator implies "can be parallel", whereas 'async' implies
-- "must be parallel". Note that outside an 'Alternative' composition 'async'
-- is not useful and should not be used.  Even in an 'Alternative' composition
-- 'async' is not useful as the last action as the last action always runs in
-- the current thread.
async :: MonadAsync m => AsyncT m a -> AsyncT m a
async action = AsyncT $ runAsyncTask True (runAsyncT action)

makeAsync :: MonadAsync m => ((a -> m ()) -> m ()) -> AsyncT m a
makeAsync = AsyncT . makeCont

------------------------------------------------------------------------------
-- Controlling thread quota
------------------------------------------------------------------------------

-- | Runs a computation under a given thread limit.  A limit of 0 means all new
-- tasks start synchronously in the current thread unless overridden by
-- 'async'.
threads :: MonadAsync m => Int -> AsyncT m a -> AsyncT m a
threads n action = AsyncT $ threadCtl n (runAsyncT action)
