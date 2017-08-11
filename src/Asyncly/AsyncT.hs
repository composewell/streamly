{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE RankNTypes                #-}

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
--    , async
--    , makeAsync
    )
where

import           Control.Applicative         (Alternative (..))
import           Control.Monad               (ap, liftM, MonadPlus(..), mzero)
--import           Control.Monad.Base          (MonadBase (..), liftBaseDefault)
import           Control.Monad.Catch         (MonadThrow, throwM)
import           Control.Monad.IO.Class      (MonadIO(..))
import           Control.Monad.Trans.Class   (MonadTrans (lift))
{-
import           Control.Monad.Trans.Control (ComposeSt, MonadBaseControl (..),
                                              MonadTransControl (..),
                                              defaultLiftBaseWith,
                                              defaultRestoreM, liftBaseWith)
                                              -}
import           Data.Maybe                  (maybe)

import           Control.Monad.Trans.Recorder (MonadRecorder(..))
import           Asyncly.Threads              (Context)

-- The 'Maybe' is redundant as we can use 'stop' value in the Nothing case,
-- but it makes the fold using '<|>' 25% faster.
newtype AsyncT m a =
    AsyncT {
        runAsyncT :: forall r.
               Context                                      -- state
            -> m r                                          -- stop
            -> (a -> Context -> Maybe (AsyncT m a) -> m r)  -- yield
            -> m r
    }

------------------------------------------------------------------------------
-- Monad
------------------------------------------------------------------------------

-- We do not use bind for parallelism. That is, we do not start each iteration
-- of the list in parallel. That will introduce too much uncontrolled
-- parallelism. Instead we control parallelsim using <|>, it is used for
-- actions that 'may' be run in parallel. The library decides whether they will
-- be started in parallel. This puts the parallel semantics in the hands of the
-- user and operational semantics in the hands of the library. Ideally we would
-- want parallelism to be completely hidden from the user, for example we can
-- automatically decide where to start an action in parallel so that it is most
-- efficient. Maybe sometime we shall be able to do that?

serially :: AsyncT m a -> AsyncT m a -> AsyncT m a
serially (AsyncT m1) m2 = AsyncT $ \ctx stp yld ->
    m1 ctx ((runAsyncT m2) ctx stp yld) $ \a c r ->
        let yield x = yld a c (Just x)
        in maybe (yield m2) (\rx -> yield $ serially rx m2) r

instance Monad m => Monad (AsyncT m) where
    return a = AsyncT $ \ctx _ yld -> yld a ctx Nothing

    AsyncT m >>= f = AsyncT $ \ctx stp yld ->
        m ctx stp $ \a c r ->
            let run x = (runAsyncT x) c stp yld
            in maybe (run $ f a) (\rx -> run $ serially (f a) (rx >>= f)) r

------------------------------------------------------------------------------
-- Functor
------------------------------------------------------------------------------

instance Monad m => Functor (AsyncT m) where
    fmap = liftM

------------------------------------------------------------------------------
-- Applicative
------------------------------------------------------------------------------

instance Monad m => Applicative (AsyncT m) where
    pure  = return
    (<*>) = ap

------------------------------------------------------------------------------
-- Alternative
------------------------------------------------------------------------------

instance Monad m => Alternative (AsyncT m) where
    empty = AsyncT $ \_ stp _ -> stp

    -- XXX need to wait for the async threads in case of a stop is returned due
    -- to async thread spawning.
    AsyncT m1 <|> m2 = AsyncT $ \ctx stp yld ->
        m1 ctx ((runAsyncT m2) ctx stp yld) $ \a c r ->
            let yield x = yld a c (Just x)
            in maybe (yield m2) (\rx -> yield $ rx <|> m2) r

instance Monad m => MonadPlus (AsyncT m) where
    mzero = empty
    mplus = (<|>)

instance (Monoid a, Monad m) => Monoid (AsyncT m a) where
    mappend x y = mappend <$> x <*> y
    mempty      = return mempty

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
    lift mx = AsyncT $ \ctx _ yld -> mx >>= (\a -> (yld a ctx Nothing))

{-
instance (MonadBase b m, Monad m) => MonadBase b (AsyncT m) where
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

instance (MonadBaseControl b m, Monad m) => MonadBaseControl b (AsyncT m) where
    type StM (AsyncT m) a = ComposeSt AsyncT m a
    liftBaseWith = defaultLiftBaseWith
    restoreM     = defaultRestoreM
    {-# INLINABLE liftBaseWith #-}
    {-# INLINABLE restoreM #-}
-}

------------------------------------------------------------------------------
-- Standard transformer instances
------------------------------------------------------------------------------

instance MonadIO m => MonadIO (AsyncT m) where
    liftIO = lift . liftIO

instance MonadThrow m => MonadThrow (AsyncT m) where
    throwM = lift . throwM

------------------------------------------------------------------------------
-- MonadRecorder
------------------------------------------------------------------------------

instance (Monad m, MonadRecorder m) => MonadRecorder (AsyncT m) where
    getJournal = lift getJournal
    putJournal = lift . putJournal
    play = lift . play

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
{-
async :: Monad m => AsyncT m a -> AsyncT m a
async action = AsyncT $ runAsyncTask True (runAsyncT action)

makeAsync :: Monad m => ((a -> m ()) -> m ()) -> AsyncT m a
makeAsync = AsyncT . makeCont
-}
