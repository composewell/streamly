{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE RecordWildCards           #-}

module Duct.Context
    ( Context(..)
    , initContext
    , saveContext
    , restoreContext
    , resumeContext
    , Location(..)
    , getLocation
    , setLocation
    , getData
    , setData
    , modifyData
    , delData
    )
where

import Control.Applicative (Alternative(..))
import           Control.Concurrent     (ThreadId)
import           Control.Concurrent.STM (TChan)
import           Control.Monad.State    (MonadState, gets, modify, StateT, MonadPlus)
import qualified Control.Monad.Trans.State.Lazy as Lazy (get, gets, modify, put)
import           Data.Dynamic           (TypeRep, Typeable, typeOf)
import           Data.IORef             (IORef)
import qualified Data.Map               as M
import           Unsafe.Coerce          (unsafeCoerce)
import GHC.Prim (Any)

------------------------------------------------------------------------------
-- State of a continuation
------------------------------------------------------------------------------

-- | Describes the context of a computation.
data Context = Context
  { event       :: Maybe ()  -- untyped, XXX rename to mailbox - can we do
  -- away with this?
  -- ^ event data to use in a continuation in a new thread

  -- the 'm' and 'f' in an 'm >>= f' operation of the monad
  -- In nested binds we store the current m only, but the whole stack of fs
  , currentm     :: Any   -- untyped, the real type is: m a
  , fstack       :: [Any] -- untyped, the real type is: [a -> m b]
    -- ^ List of continuations

  -- log only when there is a restore or if we are teleporting
  -- We can use a HasLog constraint to statically enable/disable logging
  -- , journal :: Maybe Log
  , location :: Location

  , mfData      :: M.Map TypeRep Any -- untyped, type coerced
    -- ^ State data accessed with get or put operations

    -- XXX All of the following can be removed
    -- Even the remote status can be removed by the same logic

  , parentChannel  :: Maybe (TChan ThreadId)
    -- ^ Our parent thread's channel to communicate to when we die

    -- Thread creation and cleanup handling. When a new thread is created the
    -- parent records it in the 'children' field and 'threadCredit' is
    -- decremented.  When a child thread is done it sends a done event to the
    -- parent on an unbounded channel and goes away. Before starting a new
    -- thread the parent always processes the unbounded channel to clear up the
    -- pending zombies. This strategy ensures that the zombie queue will never
    -- grow more than the number of running threads.  Before exiting, the
    -- parent thread waits on the channel until all its children are cleaned
    -- up.

  , childChannel    :: TChan ThreadId
    -- ^ A channel for the immediate children to communicate to us when they die.
    -- Each thread has its own dedicated channel for its children

    -- We always track the child threads, otherwise the programmer needs to
    -- worry about if some threads may remain hanging due because they are
    -- stuck in an infinite loop or a forever blocked IO. Also, it is not clear
    -- whether there is a significant gain by disabling tracking. Instead of
    -- using the threadIds we can also keep a count instead, that will allow us
    -- to wait for all children to drain but we cannot kill them.
    --
    -- We need these as IORefs since we need any changes to these to be
    -- reflected back in the calling code that waits for the children.

  , pendingThreads :: IORef [ThreadId]
    -- ^ Active immediate child threads of this thread, modified only by this
    -- thread, therefore atomic modification is not needed.

    -- By default this limit is shared by the current tree of threads. But at
    -- any point the 'threads' primitive can be used to start a new limit for
    -- the computation under that primitive.
    --
    -- Shared across many threads, therefore atomic access is required.
    -- This is incremented by the child thread itelf before it exits.
    --
  , threadCredit   :: IORef Int
    -- ^ How many more threads are allowed to be created?
  } deriving Typeable

initContext
    :: m a
    -> TChan ThreadId
    -> IORef [ThreadId]
    -> IORef Int
    -> Context
initContext x childChan pending credit =
  Context { event          = mempty
         , currentm        = unsafeCoerce x
         , fstack          = []
         , location        = Worker
         , mfData          = mempty
         , parentChannel   = Nothing
         , childChannel    = childChan
         , pendingThreads  = pending
         , threadCredit    = credit }

------------------------------------------------------------------------------
-- Where is the computation running?
------------------------------------------------------------------------------

-- RemoteNode is used in the Alternative instance. We stop the computation when
-- the location is RemoteNode, we do not execute any alternatives.
--
-- WaitingParent is used in logging. When a parent forks a child and waits for
-- it, it is marked as WaitingParent and we log a "Wait" entry in the log.
--
data Location = Worker | WaitingParent | RemoteNode
  deriving (Typeable, Eq, Show)

getLocation :: Monad m => StateT Context m Location
getLocation = Lazy.gets location

setLocation :: Monad m => Location -> StateT Context m ()
setLocation loc = Lazy.modify $ \Context { .. } -> Context { location = loc, .. }

------------------------------------------------------------------------------
-- Saving and restoring the context of a computation
------------------------------------------------------------------------------

-- Save the 'm' and 'f', in case we migrate to a new thread before the current
-- bind operation completes, we run the operation manually in the new thread
-- using this context.
saveContext :: Monad m => f a -> (a -> f b) -> StateT Context m ()
saveContext m f =
    Lazy.modify $ \Context { fstack = fs, .. } ->
        Context { currentm = unsafeCoerce m
                , fstack   = unsafeCoerce f : fs
                , .. }

-- pop the top function from the continuation stack, create the next closure,
-- set it as the current closure and return it.
restoreContext :: (Alternative f, Monad m) => Maybe a -> StateT Context m (f b)
restoreContext x = do
    -- XXX fstack must be non-empty when this is called.
    ctx@Context { fstack = f:fs } <- Lazy.get

    let mres = case x of
            Nothing -> empty
            Just y -> (unsafeCoerce f) y

    Lazy.put ctx { currentm = unsafeCoerce mres, fstack = fs }

    return mres

-- | Resume execution of the computation from a given context. We can retrieve
-- a context at any point and resume that context at some later point, which
-- means start executing again from the same point where the context was
-- retrieved.
--
-- Run the stack of pending functions in fstack. The types don't match. We just
-- coerce the types here, we know that they actually match.

resumeContext :: MonadPlus m => Context -> m a
resumeContext Context { currentm = m, fstack = fs } =
    unsafeCoerce m >>= runfStack (unsafeCoerce fs)

    where

    -- runfStack :: Monad m => [a -> AsyncT m a] -> a -> AsyncT m b
    runfStack [] _     = empty
    runfStack (f:ff) x = f x >>= runfStack ff

------------------------------------------------------------------------------
-- * Extensible State: Session Data Management
------------------------------------------------------------------------------

-- | Same as 'getSData' but with a more general type. If the data is found, a
-- 'Just' value is returned. Otherwise, a 'Nothing' value is returned.
getData :: (MonadState Context m, Typeable a) => m (Maybe a)
getData = resp
  where resp = do
          list <- gets mfData
          case M.lookup (typeOf $ typeResp resp) list of
            Just x  -> return . Just $ unsafeCoerce x
            Nothing -> return Nothing
        typeResp :: m (Maybe x) -> x
        typeResp = undefined

-- | Same as 'setSData' but with a more general type.
setData :: (MonadState Context m, Typeable a) => a -> m ()
setData x = modify $ \st -> st { mfData = M.insert t (unsafeCoerce x) (mfData st) }
  where t = typeOf x

modifyData :: (MonadState Context m, Typeable a) => (Maybe a -> Maybe a) -> m ()
modifyData f = modify $ \st -> st { mfData = M.alter alterf t (mfData st) }
  where typeResp :: (Maybe a -> b) -> a
        typeResp   = undefined
        t          = typeOf (typeResp f)
        alterf mx  = unsafeCoerce $ f x'
          where x' = case mx of
                       Just x  -> Just $ unsafeCoerce x
                       Nothing -> Nothing

delData :: (MonadState Context m, Typeable a) => a -> m ()
delData x = modify $ \st -> st { mfData = M.delete (typeOf x) (mfData st) }
