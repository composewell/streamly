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

module Duct.AsyncT
    ( AsyncT (..)
    , waitAsync
    , compose
    , (<**)
    , onNothing
    , RemoteStatus (..)
    , back
    , waitForChildren
    , dbg
    )
where

import           Control.Applicative         (Alternative (..))
import           Control.Concurrent          (ThreadId, myThreadId)
import           Control.Concurrent.STM      (TChan, atomically, newTChan,
                                              readTChan)
import           Control.Exception           (SomeException, catch)
import           Control.Monad.Base          (MonadBase (..), liftBaseDefault)
import           Control.Monad.State         (MonadIO (..), MonadPlus (..),
                                              MonadState (..), StateT (..),
                                              liftM, modify, runStateT, when)
import           Control.Monad.Trans.Class   (MonadTrans (lift))
import           Control.Monad.Trans.Control (ComposeSt, MonadBaseControl (..),
                                              MonadTransControl (..),
                                              defaultLiftBaseWith,
                                              defaultRestoreM)
import           Data.Dynamic                (Typeable)
import           Data.IORef                  (IORef, newIORef, readIORef,
                                              writeIORef)
import           Data.List                   (delete)
import           Data.Maybe                  (isJust, isNothing)
import           Unsafe.Coerce               (unsafeCoerce)

import           Duct.Event
import           Duct.Log

-- import           Debug.Trace

type StateM m = StateT EventF m

newtype AsyncT m a = AsyncT { runAsyncT :: StateM m (Maybe a) }

------------------------------------------------------------------------------
-- Remote data types
------------------------------------------------------------------------------

data RemoteStatus   = WasRemote | WasParallel | NoRemote
  deriving (Typeable, Eq, Show)

------------------------------------------------------------------------------
-- Utilities
------------------------------------------------------------------------------

-- dbg x = trace x (return ())
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

instance MonadIO m => Applicative (AsyncT m) where
  pure a  = AsyncT . return $ Just a

  f <*> g = AsyncT $ do
         rf <- liftIO $ newIORef (Nothing,[])
         rg <- liftIO $ newIORef (Nothing,[])
         fs <- getContinuations

         let hasWait (_:Wait:_) = True
             hasWait _          = False

             appf k = AsyncT $  do
                   Log rec _ full <- getData `onNothing` return (Log False [] [])
                   (liftIO $ writeIORef rf  (Just k,full))
                   (x, full2)<- liftIO $ readIORef rg
                   when (hasWait  full ) $
                        let full'= head full: full2
                        in (setData $ Log rec full' full')
                   return $ Just (unsafeCoerce k) <*> x

             appg x = AsyncT $  do
                   Log rec _ full <- getData `onNothing` return (Log False [] [])
                   liftIO $ writeIORef rg (Just x, full)
                   (k,full1) <- liftIO $ readIORef rf
                   when (hasWait  full) $
                        let full'= head full: full1
                        in (setData $ Log rec full' full')
                   return $ unsafeCoerce k <*> Just x

         setContinuation f appf fs

         k <- runAsyncT f
         was <- getData `onNothing` return NoRemote
         when (was == WasParallel) $  setData NoRemote

         Log recovery _ full <- getData `onNothing` return (Log False [] [])



         if was== WasRemote  || (not recovery && was == NoRemote  && isNothing k )
         -- if the first operand was a remote request
         -- (so this node is not master and hasn't to execute the whole expression)
         -- or it was not an asyncronous term (a normal term without async or parallel
         -- like primitives) and is nothing
           then  do
             restoreStack fs
             return Nothing
           else do
             when (isJust k) $ liftIO $ writeIORef rf  (k,full)
                -- when necessary since it maybe WasParallel and Nothing

             setContinuation g appg fs

             x <- runAsyncT g
             Log _ _ full' <- getData `onNothing` return (Log False [] [])
             liftIO $ writeIORef rg  (x,full')
             restoreStack fs
             k'' <- if was== WasParallel
                      then do
                        (k',_) <- liftIO $ readIORef rf -- since k may have been updated by a parallel f
                        return k'
                      else return k
             return $ k'' <*> x

------------------------------------------------------------------------------
-- Alternative
------------------------------------------------------------------------------

instance MonadIO m => Alternative (AsyncT m) where
    empty = AsyncT $ return  Nothing
    (<|>) x y = AsyncT $ do
        mx <- runAsyncT x
        was <- getData `onNothing` return NoRemote
        if was == WasRemote
          then return Nothing
          else case mx of
                Nothing -> runAsyncT y
                justx -> return justx

-- | A synonym of 'empty' that can be used in a monadic expression. It stops
-- the computation, which allows the next computation in an 'Alternative'
-- ('<|>') composition to run.
stop :: Alternative m => m stopped
stop = empty

------------------------------------------------------------------------------
-- Monad
------------------------------------------------------------------------------

-- | Total variant of `tail` that returns an empty list when given an empty list.
tailsafe :: [a] -> [a]
tailsafe []     = []
tailsafe (_:xs) = xs

-- | Set the current closure and continuation for the current statement
setEventCont :: Monad m => AsyncT m a -> (a -> AsyncT m b) -> StateM m EventF
setEventCont x f  = do
  EventF { fcomp = fs, .. } <- get
  let cont = EventF { xcomp = x
                    , fcomp = unsafeCoerce f : unsafeCoerce fs
                    , .. }
  put cont
  return cont

-- | Reset the closure and continuation. Remove inner binds that the previous
-- computations may have stacked in the list of continuations.
resetEventCont :: forall m a b. MonadIO m
    => Maybe a -> EventF -> StateM m (AsyncT m b -> AsyncT m b)
resetEventCont mx _ = do
  EventF { fcomp = fs, .. } <- get
  let f my = case my of
        Nothing -> (empty :: AsyncT m a)
        Just x  -> unsafeCoerce (head fs) x
  put $ EventF { xcomp = f mx
               , fcomp = tailsafe (unsafeCoerce fs)
               , .. }
  return  id

instance MonadIO m => Monad (AsyncT m) where
  return   = pure
  x >>= f  = AsyncT $ do
    c  <- setEventCont x f
    mk <- runAsyncT x
    _ <- resetEventCont mk c
    case mk of
      Just k  -> runAsyncT (f k)
      Nothing -> return Nothing

instance MonadIO m => MonadPlus (AsyncT m) where
  mzero = empty
  mplus = (<|>)

instance (Monoid a, Monad (AsyncT m)) => Monoid (AsyncT m a) where
  mappend x y = mappend <$> x <*> y
  mempty      = return mempty

------------------------------------------------------------------------------
-- Backtracking
------------------------------------------------------------------------------

-- | Run the closure  (the 'x' in 'x >>= f') of the current bind operation.
runClosure :: EventF -> StateM m (Maybe a)
runClosure EventF { xcomp = x } = runAsyncT (unsafeCoerce x)

-- | Compose a list of continuations.
compose :: MonadIO m => [a -> AsyncT m a] -> (a -> AsyncT m b)
compose []     = const empty
compose (f:fs) = \x -> f x >>= compose fs

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

------------------------------------------------------------------------------
-- MonadIO
------------------------------------------------------------------------------

instance MonadIO m => MonadIO (AsyncT m) where
  liftIO mx = do
    ex <- liftIO' $ (mx >>= return . Right) `catch`
                    (\(e :: SomeException) -> return $ Left e)
    case ex of
      Left  e -> back e
      Right x -> return x
    where liftIO' x = AsyncT $ liftIO x >>= return . Just

-------------------------------------------------------------------------------
-- AsyncT transformer
-------------------------------------------------------------------------------

instance MonadTrans AsyncT where
  lift mx = AsyncT $ lift mx >>= return . Just

instance MonadTransControl AsyncT where
    type StT AsyncT a = (Maybe a, EventF)
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

------------------------------------------------------------------------------
-- Thread management
------------------------------------------------------------------------------

drainChildren :: MonadIO m => TChan ThreadId -> [ThreadId] -> m ()
drainChildren chan pending =
    if length pending == 0
        then return ()
        else do
            tid <- liftIO $ atomically $ readTChan chan
            dbg $ "Reaping child: " ++ show tid
            drainChildren chan $ delete tid pending

waitForChildren :: MonadIO m => TChan ThreadId -> IORef [ThreadId] -> m ()
waitForChildren chan pendingRef = do
    pending <- liftIO $ readIORef pendingRef
    drainChildren chan pending
    liftIO $ writeIORef pendingRef []

------------------------------------------------------------------------------
-- Running the monad
------------------------------------------------------------------------------

-- | Run a transient computation with a default initial state
runTransient :: forall m a. MonadIO m => AsyncT m a -> m (Maybe a, EventF)
runTransient t = do
  zombieChan <- liftIO $ atomically newTChan
  pendingRef <- liftIO $ newIORef []
  credit     <- liftIO $ newIORef maxBound
  th         <- liftIO $ myThreadId

  r <- runStateT (runAsyncT t) $ initEventF
    (empty :: AsyncT m a) th zombieChan pendingRef credit

  waitForChildren zombieChan pendingRef
  return r

-- | Run an 'AsyncT m' computation. Returns the result of the computation or
-- may throw an exception. The computation finishes with a 'Nothing' result
-- when the input sources are exhausted.
-- XXX pass a collector function and return a Traversable.
-- Ideally it should be a non-empty list instead.
--waitAsync :: MonadIO m => AsyncT m a -> m [a]
waitAsync :: MonadIO m => AsyncT m a -> m (Maybe a)
waitAsync m  = do
    (r, _) <- runTransient m
    return r

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

instance (Monad m, Monad (AsyncT m)) => MonadState EventF (AsyncT m) where
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
-- More operators, instances
------------------------------------------------------------------------------

instance (Num a, Monad (AsyncT m)) => Num (AsyncT m a) where
  fromInteger = return . fromInteger
  mf + mg     = (+) <$> mf <*> mg
  mf * mg     = (*) <$> mf <*> mg
  negate f    = f >>= return . negate
  abs f       = f >>= return . abs
  signum f    = f >>= return . signum

class AdditionalOperators m where

  -- | Run @m a@ discarding its result before running @m b@.
  (**>)  :: m a -> m b -> m b

  -- | Run @m b@ discarding its result, after the whole task set @m a@ is
  -- done.
  (<**)  :: m a -> m b -> m a

  atEnd' :: m a -> m b -> m a
  atEnd' = (<**)

  -- | Run @m b@ discarding its result, once after each task in @m a@, and
  -- once again after the whole task set is done.
  (<***) :: m a -> m b -> m a

  atEnd  :: m a -> m b -> m a
  atEnd  = (<***)

instance (Monad m, Monad (AsyncT m)) => AdditionalOperators (AsyncT m) where

  (**>) :: AsyncT m a -> AsyncT m b -> AsyncT m b
  (**>) x y =
    AsyncT $ do
      runAsyncT x
      runAsyncT y

  (<***) :: AsyncT m a -> AsyncT m b -> AsyncT m a
  (<***) ma mb =
    AsyncT $ do
      fs  <- getContinuations
      setContinuation ma (\x -> mb >> return x)  fs
      a <- runAsyncT ma
      runAsyncT mb
      restoreStack fs
      return  a

  (<**) :: AsyncT m a -> AsyncT m b -> AsyncT m a
  (<**) ma mb =
    AsyncT $ do
      a <- runAsyncT ma

      runAsyncT  mb
      return a

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

