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

module Asyncly.AsyncT
    ( AsyncT (..)
    , ChildEvent(..)
    , Loggable
    , Log (..)
    , CtxLog (..)
    , LogEntry (..)
    , Context(..)
    , initContext
    , Location(..)
    , getLocation
    , setLocation
    , (<**)
    , (**>)
    , dbg
    )
where

import           Control.Applicative         (Alternative (..))
import           Control.Concurrent          (ThreadId)
import           Control.Concurrent.STM      (TChan)
import           Control.Exception           (SomeException)
import           Control.Monad.Base          (MonadBase (..), liftBaseDefault)
import           Control.Monad.Catch         (MonadThrow, throwM)
import           Control.Monad.State         (MonadIO (..), MonadPlus (..),
                                              StateT (..), liftM, runStateT,
                                              get, gets, modify)
import           Control.Monad.Trans.Class   (MonadTrans (lift))
import           Control.Monad.Trans.Control (ComposeSt, MonadBaseControl (..),
                                              MonadTransControl (..),
                                              defaultLiftBaseWith,
                                              defaultRestoreM)
import           Data.IORef                  (IORef)
import           GHC.Prim                    (Any)
import           Unsafe.Coerce               (unsafeCoerce)

--import           Debug.Trace (traceM)

newtype AsyncT m a = AsyncT { runAsyncT :: StateT Context m (Maybe a) }

------------------------------------------------------------------------------
-- Log types
------------------------------------------------------------------------------

-- Logging is done using the 'logged' combinator. It remembers the last value
-- returned by a given computation and replays it the next time the computation
-- is resumed. However this could be problematic if we do not annotate all
-- impure computations. The program can take a different path due to a
-- non-logged computation returning a different value. In that case we may
-- replay a wrong value. To detect this we can use a unique id for each logging
-- site and abort if the id does not match on replay.

-- | Constraint type synonym for a value that can be logged.
type Loggable a = (Show a, Read a)

-- XXX remove the Maybe as we never log Nothing
data LogEntry =
      Executing             -- we are inside this computation, not yet done
    | Result (Maybe String) -- computation done, we have the result to replay
  deriving (Read, Show)

data Log = Log [LogEntry] deriving Show

-- log entries and replay entries
data CtxLog = CtxLog [LogEntry] [LogEntry] deriving (Read, Show)

------------------------------------------------------------------------------
-- Parent child thread communication types
------------------------------------------------------------------------------

data ChildEvent = ChildDone ThreadId (Maybe SomeException)

------------------------------------------------------------------------------
-- State threaded around the monad
------------------------------------------------------------------------------

data Context = Context
  {
    ---------------------------------------------------------------------------
    -- Execution state
    ---------------------------------------------------------------------------

    -- a -> AsyncT m b
    continuation   :: Any
  -- Logging is done by the 'logged' primitive in this journal only if logsRef
  -- exists.
  , journal :: CtxLog

  -- When we suspend we save the logs in this IORef and exit.
  , logsRef :: Maybe (IORef [Log])
  , location :: Location

    ---------------------------------------------------------------------------
    -- Thread creation, communication and cleanup
    ---------------------------------------------------------------------------
    -- When a new thread is created the parent records it in the
    -- 'pendingThreads' field and 'threadCredit' is decremented.  When a child
    -- thread is done it sends a done event to the parent on an unbounded
    -- channel and goes away.  Before starting a new thread the parent always
    -- processes the unbounded channel to clear up the pending zombies. This
    -- strategy ensures that the zombie queue will never grow more than the
    -- number of running threads.  Before exiting, the parent thread waits on
    -- the channel until all its children are cleaned up.
    ---------------------------------------------------------------------------

    -- XXX setup a cleanup computation to run rather than passing all these
    -- params.

  , childChannel    :: TChan ChildEvent
    -- ^ A channel for the immediate children to communicate to us when they
    -- die.  Each thread has its own dedicated channel for its children

    -- We always track the child threads, otherwise the programmer needs to
    -- worry about if some threads may remain hanging because they are stuck in
    -- an infinite loop or a forever blocked IO. Also, it is not clear whether
    -- there is a significant gain by disabling tracking. Instead of using
    -- threadIds we can also keep a count instead, that will allow us to wait
    -- for all children to drain but we cannot kill them.
    --
    -- We need these as IORefs since we need any changes to these to be
    -- reflected back in the calling code that waits for the children.

  , pendingThreads :: IORef [ThreadId]
    -- ^ Active immediate child threads spawned by this thread, modified only
    -- by this thread, therefore atomic modification is not needed.

    -- By default this limit is shared by the current tree of threads. But at
    -- any point the 'threads' primitive can be used to start a new limit for
    -- the computation under that primitive.
    --
    -- Shared across many threads, therefore atomic access is required.
    -- This is incremented by the child thread itelf before it exits.
    --
  , threadCredit   :: IORef Int
    -- ^ How many more threads are allowed to be created?
  }

initContext
    :: TChan ChildEvent
    -> IORef [ThreadId]
    -> IORef Int
    -> (a -> AsyncT m a)
    -> Maybe (IORef [Log])
    -> Context
initContext childChan pending credit finalizer lref =
  Context { continuation    = unsafeCoerce finalizer
          , journal         = CtxLog [] []
          , logsRef         = lref
          , location        = Worker
          , childChannel    = unsafeCoerce childChan
          , pendingThreads  = pending
          , threadCredit    = credit }

------------------------------------------------------------------------------
-- Where is the computation running?
------------------------------------------------------------------------------

-- RemoteNode is used in the Alternative instance. We stop the computation when
-- the location is RemoteNode, we do not execute any alternatives.
--
data Location = Worker | WaitingParent | RemoteNode
  deriving (Eq, Show)

getLocation :: Monad m => StateT Context m Location
getLocation = gets location

setLocation :: Monad m => Location -> StateT Context m ()
setLocation loc = modify $ \Context { .. } -> Context { location = loc, .. }

------------------------------------------------------------------------------
-- Utilities
------------------------------------------------------------------------------

-- Toggle the dbg definition for debug traces
--dbg :: Monad m => String -> m ()
--dbg = traceM
dbg :: Monad m => a -> m ()
dbg _ = return ()

------------------------------------------------------------------------------
-- Functor
------------------------------------------------------------------------------

instance Monad (AsyncT m) => Functor (AsyncT m) where
    fmap f mx = do
        x <- mx
        return $ f x

------------------------------------------------------------------------------
-- Applicative
------------------------------------------------------------------------------

instance Monad m => Applicative (AsyncT m) where
    pure a  = AsyncT . return $ Just a
    m1 <*> m2 = do { x1 <- m1; x2 <- m2; return (x1 x2) }

------------------------------------------------------------------------------
-- Alternative
------------------------------------------------------------------------------

instance Monad m => Alternative (AsyncT m) where
    empty = AsyncT $ return Nothing
    (<|>) x y = AsyncT $ do
        mx <- runAsyncT x
        loc <- getLocation
        case loc of
            RemoteNode -> return Nothing
            _          ->  maybe (runAsyncT y) (return . Just) mx

------------------------------------------------------------------------------
-- Monad
------------------------------------------------------------------------------

instance Monad m => Monad (AsyncT m) where
    return = pure
    m >>= f = AsyncT $ do
        modify $ \Context { continuation = g, .. } ->
            let k x = f x >>= unsafeCoerce g
            in Context { continuation = unsafeCoerce k, .. }

        x <- runAsyncT m
        Context { continuation = g } <- get
        runAsyncT $ maybe mzero (unsafeCoerce g) x

instance Monad m => MonadPlus (AsyncT m) where
    mzero = empty
    mplus = (<|>)

instance (Monoid a, Monad m) => Monoid (AsyncT m a) where
    mappend x y = mappend <$> x <*> y
    mempty      = return mempty

------------------------------------------------------------------------------
-- MonadIO
------------------------------------------------------------------------------

instance MonadIO m => MonadIO (AsyncT m) where
    liftIO mx = AsyncT $ liftIO mx >>= return . Just

-------------------------------------------------------------------------------
-- AsyncT transformer
-------------------------------------------------------------------------------

instance MonadTrans AsyncT where
    lift mx = AsyncT $ lift mx >>= return . Just

instance MonadTransControl AsyncT where
    type StT AsyncT a = (Maybe a, Context)
    liftWith f = AsyncT $ StateT $ \s ->
                   liftM (\x -> (Just x, s))
                         (f $ \t -> runStateT (runAsyncT t) s)
    restoreT = AsyncT . StateT . const
    {-# INLINABLE liftWith #-}
    {-# INLINABLE restoreT #-}

instance (MonadBase b m, MonadIO m) => MonadBase b (AsyncT m) where
    liftBase = liftBaseDefault

instance (MonadBaseControl b m, MonadIO m) => MonadBaseControl b (AsyncT m) where
    type StM (AsyncT m) a = ComposeSt AsyncT m a
    liftBaseWith = defaultLiftBaseWith
    restoreM     = defaultRestoreM
    {-# INLINABLE liftBaseWith #-}
    {-# INLINABLE restoreM #-}

instance MonadThrow m => MonadThrow (AsyncT m) where
    throwM e = lift $ throwM e

------------------------------------------------------------------------------
-- More operators, instances
------------------------------------------------------------------------------

instance (Num a, Monad (AsyncT m)) => Num (AsyncT m a) where
  fromInteger = return . fromInteger
  mf + mg     = (+) <$> mf <*> mg
  mf * mg     = (*) <$> mf <*> mg
  negate f    = f >>= return . negate
  abs f       = f >>= return . abs
  signum f    = f >>= return . signum

{-
-- | Warning: Radically untyped stuff. handle with care
getContinuations :: Monad m => StateM m [a -> AsyncT m b]
getContinuations = do
  EventF { fcomp = fs } <- get
  return $ unsafeCoerce fs

-- | Save a closure and a continuation ('x' and 'f' in 'x >>= f').
setContinuation :: Monad m
    => AsyncT m a -> (a -> AsyncT m b) -> [c -> AsyncT m c] -> StateM m ()
setContinuation  b c fs = do
  modify $ \EventF{..} -> EventF { xcomp = b
                                  , fcomp = unsafeCoerce c : fs
                                  , .. }

-- | Restore the continuations to the provided ones.
-- | NOTE: Events are also cleared out.
restoreStack :: MonadState EventF m => t -> m ()
restoreStack fs = modify $ \EventF {..} ->
    EventF { event = Nothing, fcomp = (unsafeCoerce fs), .. }

-}

{-
  (<***) :: AsyncT m a -> AsyncT m b -> AsyncT m a
  (<***) ma mb =
    AsyncT $ do
      fs  <- getContinuations
      setContinuation ma (\x -> mb >> return x)  fs
      a <- runAsyncT ma
      runAsyncT mb
      restoreStack fs
      return  a

infixr 1 <***, <**, **>

-- | Run @b@ once, discarding its result when the first task in task set @a@
-- has finished. Useful to start a singleton task after the first task has been
-- setup.
(<|) :: MonadIO m => AsyncT m a -> AsyncT m b -> AsyncT m a
(<|) ma mb = AsyncT $ do
  fs  <- getContinuations
  ref <- liftIO $ newIORef False
  setContinuation ma (cont ref)  fs
  r   <- runAsyncT ma
  restoreStack fs
  return  r
  where cont ref x = AsyncT $ do
          n <- liftIO $ readIORef ref
          if n == True
            then return $ Just x
            else do liftIO $ writeIORef ref True
                    runAsyncT mb
                    return $ Just x

-}

infixr 1 <**, **>

-- | Run @m a@ discarding its result before running @m b@.
(**>) :: Monad m => AsyncT m a -> AsyncT m b -> AsyncT m b
(**>) x y = AsyncT $ do
      _ <- runAsyncT x
      runAsyncT y

-- | Run @m b@ discarding its result, after the whole task set @m a@ is done.
(<**) :: Monad m => AsyncT m a -> AsyncT m b -> AsyncT m a
(<**) ma mb = AsyncT $ do
      a <- runAsyncT ma
      _ <- runAsyncT  mb
      return a
