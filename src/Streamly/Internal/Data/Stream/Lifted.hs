-- |
-- Module      : Streamly.Internal.Data.Stream.Lifted
-- Copyright   : (c) 2019 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

module Streamly.Internal.Data.Stream.Lifted
    (
      after
    , bracket
    , bracket3
    , finally
    , retry

    -- For IsStream module
    , afterD
    , bracket3D
    , retryD
    )
where

#include "inline.hs"

import Control.Exception (Exception, SomeException, mask_)
import Control.Monad.Catch (MonadCatch)
#ifdef USE_UNLIFTIO
import Control.Monad.IO.Unlift (MonadUnliftIO)
#else
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Control (MonadBaseControl)
#endif
import Data.Map.Strict (Map)
import GHC.Exts (inline)
import Streamly.Internal.Control.Concurrent
    (MonadRunInIO, MonadAsync, withRunInIO)
import Streamly.Internal.Data.Stream (Stream)
import Streamly.Internal.Data.IOFinalizer.Lifted
    (newIOFinalizer, runIOFinalizer, clearingIOFinalizer)
import Streamly.Internal.Data.Stream (Step(..))

import qualified Control.Monad.Catch as MC
import qualified Data.Map.Strict as Map
import qualified Streamly.Internal.Data.Stream as D

-- $setup
-- >>> :m
-- >>> import qualified Streamly.Internal.Data.Stream.Exception.Lifted as Stream

-- XXX Implement in terms of the corresponding IO operation (gbracketIO).

data GbracketIOState s1 s2 v wref
    = GBracketIOInit
    | GBracketIONormal s1 v wref
    | GBracketIOException s2

{-# INLINE_NORMAL gbracket #-}
gbracket
    :: MonadRunInIO m
    => m c -- ^ before
    -> (c -> m d1) -- ^ on normal stop
    -> (c -> e -> D.Stream m b -> m (D.Stream m b)) -- ^ on exception
    -> (c -> m d2) -- ^ on GC without normal stop or exception
    -> (forall s. m s -> m (Either e s)) -- ^ try (exception handling)
    -> (c -> D.Stream m b) -- ^ stream generator
    -> D.Stream m b
gbracket bef aft onExc onGC ftry action =
    D.Stream step GBracketIOInit

    where

    -- If the stream is never evaluated the "aft" action will never be
    -- called. For that to occur we will need the user of this API to pass a
    -- weak pointer to us.
    {-# INLINE_LATE step #-}
    step _ GBracketIOInit = do
        -- We mask asynchronous exceptions to make the execution
        -- of 'bef' and the registration of 'aft' atomic.
        -- A similar thing is done in the resourcet package: https://git.io/JvKV3
        -- Tutorial: https://markkarpov.com/tutorial/exceptions.html
        (r, ref) <- withRunInIO $ \run -> mask_ $ run $ do
            r <- bef
            ref <- newIOFinalizer (onGC r)
            return (r, ref)
        return $ Skip $ GBracketIONormal (action r) r ref

    step gst (GBracketIONormal (D.UnStream step1 st) v ref) = do
        res <- ftry $ step1 gst st
        case res of
            Right r -> case r of
                Yield x s ->
                    return $ Yield x (GBracketIONormal (D.Stream step1 s) v ref)
                Skip s ->
                    return $ Skip (GBracketIONormal (D.Stream step1 s) v ref)
                Stop ->
                    clearingIOFinalizer ref (aft v) >> return Stop
            -- XXX Do not handle async exceptions, just rethrow them.
            Left e -> do
                -- Clearing of finalizer and running of exception handler must
                -- be atomic wrt async exceptions. Otherwise if we have cleared
                -- the finalizer and have not run the exception handler then we
                -- may leak the resource.
                stream <-
                    clearingIOFinalizer ref (onExc v e (D.UnStream step1 st))
                return $ Skip (GBracketIOException stream)
    step gst (GBracketIOException (D.UnStream step1 st)) = do
        res <- step1 gst st
        case res of
            Yield x s ->
                return $ Yield x (GBracketIOException (D.Stream step1 s))
            Skip s    -> return $ Skip (GBracketIOException (D.Stream step1 s))
            Stop      -> return Stop

{-# INLINE_NORMAL bracket3D #-}
bracket3D :: (MonadAsync m, MonadCatch m) =>
       m b
    -> (b -> m c)
    -> (b -> m d)
    -> (b -> m e)
    -> (b -> D.Stream m a)
    -> D.Stream m a
bracket3D bef aft onExc onGC =
    gbracket
        bef
        aft
        (\a (e :: SomeException) _ -> onExc a >> MC.throwM e)
        onGC
        (inline MC.try)

-- For a use case of this see the "streamly-process" package. It needs to kill
-- the process in case of exception or garbage collection, but waits for the
-- process to terminate in normal cases.

-- | Like 'bracket' but can use 3 separate cleanup actions depending on the
-- mode of termination:
--
-- 1. When the stream stops normally
-- 2. When the stream is garbage collected
-- 3. When the stream encounters an exception
--
-- @bracket3 before onStop onGC onException action@ runs @action@ using the
-- result of @before@. If the stream stops, @onStop@ action is executed, if the
-- stream is abandoned @onGC@ is executed, if the stream encounters an
-- exception @onException@ is executed.
--
-- The exception is not caught, it is rethrown.
--
-- /Pre-release/
{-# INLINE bracket3 #-}
bracket3 :: (MonadAsync m, MonadCatch m)
    => m b
    -> (b -> m c)
    -> (b -> m d)
    -> (b -> m e)
    -> (b -> Stream m a)
    -> Stream m a
bracket3 = bracket3D

-- | Run the alloc action @IO b@ with async exceptions disabled but keeping
-- blocking operations interruptible (see 'Control.Exception.mask').  Use the
-- output @b@ of the IO action as input to the function @b -> Stream m a@ to
-- generate an output stream.
--
-- @b@ is usually a resource under the IO monad, e.g. a file handle, that
-- requires a cleanup after use. The cleanup action @b -> m c@, runs whenever
-- (1) the stream ends normally, (2) due to a sync or async exception or, (3)
-- if it gets garbage collected after a partial lazy evaluation. The exception
-- is not caught, it is rethrown.
--
-- 'bracket' only guarantees that the cleanup action runs, and it runs with
-- async exceptions enabled. The action must ensure that it can successfully
-- cleanup the resource in the face of sync or async exceptions.
--
-- When the stream ends normally or on a sync exception, cleanup action runs
-- immediately in the current thread context, whereas in other cases it runs in
-- the GC context, therefore, cleanup may be delayed until the GC gets to run.
--
-- /See also: 'bracket_'/
--
-- /Inhibits stream fusion/
--
{-# INLINE bracket #-}
bracket :: (MonadAsync m, MonadCatch m)
    => m b -> (b -> m c) -> (b -> Stream m a) -> Stream m a
bracket bef aft = bracket3 bef aft aft aft

-- | Run the action @m b@ whenever the stream @Stream m a@ stops normally,
-- aborts due to an exception or if it is garbage collected after a partial
-- lazy evaluation.
--
-- The semantics of running the action @m b@ are similar to the cleanup action
-- semantics described in 'bracket'.
--
-- >>> finally action xs = Stream.bracket (return ()) (const action) (const xs)
--
-- /See also 'finally_'/
--
-- /Inhibits stream fusion/
--
{-# INLINE finally #-}
finally :: (MonadAsync m, MonadCatch m) =>
    m b -> Stream m a -> Stream m a
finally action xs = bracket (return ()) (const action) (const xs)

{-# INLINE_NORMAL afterD #-}
afterD :: MonadRunInIO m
    => m b -> D.Stream m a -> D.Stream m a
afterD action (D.Stream step state) = D.Stream step' Nothing

    where

    {-# INLINE_LATE step' #-}
    step' _ Nothing = do
        ref <- newIOFinalizer action
        return $ Skip $ Just (state, ref)
    step' gst (Just (st, ref)) = do
        res <- step gst st
        case res of
            Yield x s -> return $ Yield x (Just (s, ref))
            Skip s    -> return $ Skip (Just (s, ref))
            Stop      -> do
                runIOFinalizer ref
                return Stop

-- | Run the action @m b@ whenever the stream @Stream m a@ stops normally, or
-- if it is garbage collected after a partial lazy evaluation.
--
-- The semantics of the action @m b@ are similar to the semantics of cleanup
-- action in 'bracket'.
--
-- /See also 'after_'/
--
{-# INLINE after #-}
after ::
#ifdef USE_UNLIFTIO
    MonadUnliftIO m
#else
    (MonadIO m, MonadBaseControl IO m)
#endif
    => m b -> Stream m a -> Stream m a
after = afterD

data RetryState emap s1 s2
    = RetryWithMap emap s1
    | RetryDefault s2

-- | See 'Streamly.Internal.Data.Stream.retry'
--
{-# INLINE_NORMAL retryD #-}
retryD
    :: forall e m a. (Exception e, Ord e, MonadCatch m)
    => Map e Int
       -- ^ map from exception to retry count
    -> (e -> D.Stream m a)
       -- ^ default handler for those exceptions that are not in the map
    -> D.Stream m a
    -> D.Stream m a
retryD emap0 defaultHandler (D.Stream step0 state0) = D.Stream step state

    where

    state = RetryWithMap emap0 state0

    {-# INLINE_LATE step #-}
    step gst (RetryWithMap emap st) = do
        eres <- MC.try $ step0 gst st
        case eres of
            Left e -> handler e emap st
            Right res ->
                return
                    $ case res of
                          Yield x st1 -> Yield x $ RetryWithMap emap st1
                          Skip st1 -> Skip $ RetryWithMap emap st1
                          Stop -> Stop
    step gst (RetryDefault (D.UnStream step1 state1)) = do
        res <- step1 gst state1
        return
            $ case res of
                  Yield x st1 -> Yield x $ RetryDefault (D.Stream step1 st1)
                  Skip st1 -> Skip $ RetryDefault (D.Stream step1 st1)
                  Stop -> Stop

    {-# INLINE handler #-}
    handler e emap st =
        return
            $ Skip
            $ case Map.lookup e emap of
                  Just i
                      | i > 0 ->
                          let emap1 = Map.insert e (i - 1) emap
                           in RetryWithMap emap1 st
                      | otherwise -> RetryDefault $ defaultHandler e
                  Nothing -> RetryDefault $ defaultHandler e

-- | @retry@ takes 3 arguments
--
-- 1. A map @m@ whose keys are exceptions and values are the number of times to
-- retry the action given that the exception occurs.
--
-- 2. A handler @han@ that decides how to handle an exception when the exception
-- cannot be retried.
--
-- 3. The stream itself that we want to run this mechanism on.
--
-- When evaluating a stream if an exception occurs,
--
-- 1. The stream evaluation aborts
--
-- 2. The exception is looked up in @m@
--
--    a. If the exception exists and the mapped value is > 0 then,
--
--       i. The value is decreased by 1.
--
--       ii. The stream is resumed from where the exception was called, retrying
--       the action.
--
--    b. If the exception exists and the mapped value is == 0 then the stream
--    evaluation stops.
--
--    c. If the exception does not exist then we handle the exception using
--    @han@.
--
-- /Internal/
--
{-# INLINE retry #-}
retry :: (MonadCatch m, Exception e, Ord e)
    => Map e Int
       -- ^ map from exception to retry count
    -> (e -> Stream m a)
       -- ^ default handler for those exceptions that are not in the map
    -> Stream m a
    -> Stream m a
retry = retryD
