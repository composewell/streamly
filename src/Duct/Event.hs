{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}

module Duct.Event
    ( Context(..)
    , initContext
    , getData
    , setData
    , modifyData
    , delData
    )
where

import           Control.Concurrent     (ThreadId)
import           Control.Concurrent.STM (TChan)
import           Control.Monad.State    (MonadState, gets, modify)
import           Data.Dynamic           (TypeRep, Typeable, typeOf)
import           Data.IORef             (IORef)
import qualified Data.Map               as M
import           Unsafe.Coerce          (unsafeCoerce)

------------------------------------------------------------------------------
-- State of a continuation
------------------------------------------------------------------------------

type SData = ()

-- | Describes the context of a computation.
data Context = forall m a b. Context
  { event       :: Maybe SData  -- untyped, XXX rename to mailbox - can we do
  -- away with this?
  -- ^ event data to use in a continuation in a new thread

  -- the 'm' and 'f' in an 'm >>= f' operation of the monad
  -- In nested binds we store the current m only, but the whole stack of fs
  , currentm     :: m a
  , fstack       :: [a -> m b]
    -- ^ List of continuations

  , mfData      :: M.Map TypeRep SData
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
  Context { event           = mempty
         , currentm        = x
         , fstack          = []
         , mfData          = mempty
         , parentChannel   = Nothing
         , childChannel    = childChan
         , pendingThreads  = pending
         , threadCredit    = credit }

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
