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

module Strands.AsyncT
    ( AsyncT (..)
    , waitAsync
    , processOneEvent
    , drainChildren
    , waitForChildren
    , getCtxResultDest
    , (<**)
    , onNothing
    , dbg
    )
where

import           Control.Applicative         (Alternative (..))
import           Control.Concurrent          (ThreadId, killThread)
import           Control.Concurrent.STM      (TChan, atomically, newTChan,
                                              readTChan, writeTChan)
import           Control.Monad.Base          (MonadBase (..), liftBaseDefault)
import           Control.Monad.Catch         (MonadCatch, MonadThrow, try,
                                              throwM, SomeException)
import           Control.Monad.State         (MonadIO (..), MonadPlus (..),
                                              MonadState (..), StateT (..),
                                              liftM, modify, runStateT, when)
import           Control.Monad.Trans.Class   (MonadTrans (lift))
import           Control.Monad.Trans.Control (ComposeSt, MonadBaseControl (..),
                                              MonadTransControl (..),
                                              defaultLiftBaseWith,
                                              defaultRestoreM)
import           Data.Dynamic                (Typeable)
import           Data.IORef                  (IORef, modifyIORef, newIORef, readIORef,
                                              writeIORef)
import           Data.List                   (delete)
import           Data.Maybe                  (fromJust, isJust, isNothing)
import           Unsafe.Coerce               (unsafeCoerce)

import           Strands.Context

--import           Debug.Trace (traceM)

newtype AsyncT m a = AsyncT { runAsyncT :: StateT Context m (Maybe a) }

------------------------------------------------------------------------------
-- Utilities
------------------------------------------------------------------------------

-- Toggle the dbg definition for debug traces
--dbg :: Monad m => String -> m ()
--dbg = traceM
dbg :: Monad m => a -> m ()
dbg _ = return ()

-- | If the first parameter is 'Nothing' return the second parameter otherwise
-- return the first parameter..
onNothing :: Monad m => m (Maybe b) -> m b -> m b
onNothing iox iox'= do
       mx <- iox
       case mx of
           Just x -> return x
           Nothing -> iox'

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
    empty = AsyncT $ return  Nothing
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

    -- Inner bind-operations in 'm' add their 'f' to fstack.  If we migrate the
    -- context to a new thread, somewhere in the middle, we unwind the fstack
    -- and run these functions when we resume the context after migration.
    m >>= f = AsyncT $ do
        saveContext m f
        runAsyncT m >>= restoreContext >>= runAsyncT

instance Monad m => MonadPlus (AsyncT m) where
  mzero = empty
  mplus = (<|>)

instance (Monoid a, Monad (AsyncT m)) => Monoid (AsyncT m a) where
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
-- Thread management
------------------------------------------------------------------------------

-- XXX We are using unbounded channels so this will not block on writing to
-- pchan. We can use bounded channels to throttle the creation of threads based
-- on consumption rate.
processOneEvent :: MonadIO m
    => ChildEvent a
    -> Either (TChan (ChildEvent a)) (IORef [a])
    -> [ThreadId]
    -> m ([ThreadId], Maybe SomeException)
processOneEvent ev dest pending = do
    -- Collect results unless we have already encountered an exception.
    case ev of
        ChildDone _ (Just e)  -> handleException e
        ChildResult (Left e)  -> handleException e
        ChildDone tid Nothing -> return (delete tid pending, Nothing)
        ChildResult (Right r) -> do
            case dest of
                Left chan -> liftIO $ atomically $ writeTChan chan ev
                Right ref ->
                    liftIO $ modifyIORef ref $ \rs -> unsafeCoerce r : rs
            return (pending, Nothing)
    where

    handleException e = do
        liftIO $ mapM_ killThread pending
        return (pending, Just e)

drainChildren :: MonadIO m
    => Either (TChan (ChildEvent a)) (IORef [a])
    -> TChan (ChildEvent a)
    -> [ThreadId]
    -> m ([ThreadId], Maybe SomeException)
drainChildren dest cchan pending =
    case pending of
        [] -> return (pending, Nothing)
        _  ->  do
            ev <- liftIO $ atomically $ readTChan cchan
            (p, e) <- processOneEvent ev dest pending
            maybe (drainChildren dest cchan p) (const $ return (p, e)) e

waitForChildren :: MonadIO m => Context -> m (Maybe SomeException)
waitForChildren ctx = do
    let pendingRef = pendingThreads ctx
    pending <- liftIO $ readIORef pendingRef
    (p, e) <- drainChildren (getCtxResultDest ctx) (childChannel ctx) pending
    liftIO $ writeIORef pendingRef p
    return e

getCtxResultDest :: Context -> Either (TChan (ChildEvent a)) (IORef [a])
getCtxResultDest ctx =
    maybe (Right $ unsafeCoerce $ fromJust $ accumResults ctx)
          (Left . unsafeCoerce) (parentChannel ctx)

------------------------------------------------------------------------------
-- Running the monad
------------------------------------------------------------------------------

collectResult :: MonadIO m => a -> StateT Context m ()
collectResult r = do
        ctx <- get
        case parentChannel ctx of
            Nothing -> do
                let ref = fromJust $ accumResults ctx
                liftIO $ modifyIORef ref $ \rs -> unsafeCoerce r : rs
            Just chan ->  do
                -- XXX can we pass the result directly to the root thread
                -- instead of passing through all the parents? We can let the
                -- parent go away and handle the ChildDone events as well in
                -- the root thread.
                liftIO $ atomically $ writeTChan chan
                                        (ChildResult (Right (unsafeCoerce r)))

-- | Invoked to store the result of the computation in the context and finish
-- the computation when the computation is done
finishComputation :: MonadIO m => a -> AsyncT m b
finishComputation x = AsyncT $ do
    collectResult x
    return Nothing

-- XXX pass a collector function and return a Traversable.
-- XXX Ideally it should be a non-empty list instead.
-- | Run an 'AsyncT m' computation. Returns a list of results of the
-- computation or may throw an exception.
waitAsync :: forall m a. (MonadIO m, MonadCatch m)
    => AsyncT m a -> m [a]
waitAsync m = do
    childChan  <- liftIO $ atomically newTChan
    pendingRef <- liftIO $ newIORef []
    resultsRef <- liftIO $ newIORef []
    credit     <- liftIO $ newIORef maxBound

    let ctx = initContext (empty :: AsyncT m a) childChan pendingRef credit
                  finishComputation resultsRef

    r <- try $ runStateT (runAsyncT $ m >>= finishComputation) ctx

    case r of
        Left (exc :: SomeException) -> do
            liftIO $ readIORef pendingRef >>= mapM_ killThread
            throwM exc
        Right _ -> do
            e <- waitForChildren ctx
            case e of
                Just (exc :: SomeException) -> throwM exc
                Nothing -> liftIO $ readIORef resultsRef

------------------------------------------------------------------------------
-- * Extensible State: Session Data Management
------------------------------------------------------------------------------

-- | Retrieve a previously stored data item of the given data type from the
-- monad state. The data type to retrieve is implicitly determined from the
-- requested type context.
-- If the data item is not found, an 'empty' value (a void event) is returned.
-- Remember that an empty value stops the monad computation. If you want to
-- print an error message or a default value in that case, you can use an
-- 'Alternative' composition. For example:
--
-- > getSData <|> error "no data"
-- > getInt = getSData <|> return (0 :: Int)
getSData ::  (Monad m, Typeable a) => AsyncT m a
getSData = AsyncT getData

-- | 'setSData' stores a data item in the monad state which can be retrieved
-- later using 'getData' or 'getSData'. Stored data items are keyed by their
-- data type, and therefore the data type must be 'Typeable' and only one item
-- of a given type can be stored. A newtype wrapper can be used to distinguish
-- two data items of the same type when required.
--
-- @
-- import Control.Monad.IO.Class (liftIO)
-- import Transient.Base
-- import Data.Typeable
--
-- data Person = Person
--    { name :: String
--    , age :: Int
--    } deriving Typeable
--
-- main = keep $ do
--      setSData $ Person "Alberto"  55
--      Person name age <- getSData
--      liftIO $ print (name, age)
-- @
setSData ::  (Monad m, Typeable a) => a -> AsyncT m ()
setSData x = AsyncT $ setData x >> return (Just ())

-- | Accepts a function that takes the current value of the stored data type
-- and returns the modified value. If the function returns 'Nothing' the value
-- is deleted otherwise updated.
modifySData :: (Monad m, Typeable a) => (Maybe a -> Maybe a) -> AsyncT m ()
modifySData x = AsyncT $ modifyData x >> return (Just ())

-- | Delete the data item of the given type from the monad state.
delSData :: (Monad m, Typeable a) => a -> AsyncT m ()
delSData x = AsyncT $ delData x >> return (Just ())

------------------------------------------------------------------------------
-- MonadState for AsyncT m
------------------------------------------------------------------------------

instance (Monad m, Monad (AsyncT m)) => MonadState Context (AsyncT m) where
  get     = AsyncT $ get   >>= return . Just
  put x   = AsyncT $ put x >>  return (Just ())
  state f = AsyncT $ do
    s <- get
    let ~(a, s') = f s
    put s'
    return $ Just a

{-
-- | Run an action, if the result is a void action undo any state changes
-- that it might have caused.
try :: MonadIO m => AsyncT m a -> AsyncT m a
try mx = do
  sd <- gets mfData
  mx <|> (modify (\s -> s { mfData = sd }) >> empty)

-- | Executes the computation and reset the state either if it fails or not
sandbox :: MonadIO m => AsyncT m a -> AsyncT m a
sandbox mx = do
  sd <- gets mfData
  mx <*** modify (\s ->s { mfData = sd})
    -}

------------------------------------------------------------------------------
-- Backtracking
------------------------------------------------------------------------------

{-
-- | Run the closure  (the 'x' in 'x >>= f') of the current bind operation.
runClosure :: EventF -> StateM m (Maybe a)
runClosure EventF { xcomp = x } = runAsyncT (unsafeCoerce x)

-- | Run the continuation (the 'f' in 'x >>= f') of the current bind operation with the current state.
runContinuation :: MonadIO m => EventF -> a -> StateM m (Maybe b)
runContinuation EventF { fcomp = fs } =
  runAsyncT . (compose $  (unsafeCoerce fs))

data Backtrack b = Show b => Backtrack
    { backtracking :: Maybe b
    , backStack :: [EventF]
    } deriving Typeable

backStateOf :: (Monad m, Show a) => a -> m (Backtrack a)
backStateOf reason = return $ Backtrack (Nothing `asTypeOf` (Just reason)) []

-- | Start the undo process for the given undo track id. Performs all the undo
-- actions registered till now in reverse order. An undo action can use
-- 'forward' to stop the undo process and resume forward execution. If there
-- are no more undo actions registered execution stops and a 'stop' action is
-- returned.
--
back :: (MonadIO m, Typeable b, Show b) => b -> AsyncT m a
back reason = AsyncT $ do
  bs <- getData  `onNothing`  backStateOf  reason
  goBackt  bs

  where

  goBackt (Backtrack _ [] )= return Nothing
  goBackt (Backtrack _ (stack@(first : bs)) )= do
        (setData $ Backtrack (Just reason) stack)

        mr <-  runClosure first

        Backtrack b _ <- getData `onNothing`  backStateOf  reason
        case mr of
           Nothing -> return empty
           Just x -> case b of
                 Nothing -> runContinuation first x
                 justreason -> goBackt $ Backtrack justreason bs

-}

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
      runAsyncT x
      runAsyncT y

-- | Run @m b@ discarding its result, after the whole task set @m a@ is done.
(<**) :: Monad m => AsyncT m a -> AsyncT m b -> AsyncT m a
(<**) ma mb = AsyncT $ do
      a <- runAsyncT ma
      runAsyncT  mb
      return a
