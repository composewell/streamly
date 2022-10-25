-- |
-- Module      : Streamly.Internal.Data.Stream.Exception.Lifted
-- Copyright   : (c) 2019 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

module Streamly.Internal.Data.Stream.Exception.Lifted
    (
      after
    , bracket
    , bracket3
    , finally

    -- For IsStream module
    , afterD
    , bracket3D
    )
where

#include "inline.hs"

import Control.Exception (SomeException, mask_)
import Control.Monad.Catch (MonadCatch)
import GHC.Exts (inline)
import Streamly.Internal.Control.Concurrent
    (MonadRunInIO, MonadAsync, withRunInIO)
import Streamly.Internal.Data.Stream.Type (Stream, fromStreamD, toStreamD)
import Streamly.Internal.Data.IOFinalizer.Lifted
    (newIOFinalizer, runIOFinalizer, clearingIOFinalizer)

import qualified Control.Monad.Catch as MC
import qualified Streamly.Internal.Data.Stream.StreamD as D

import Streamly.Internal.Data.Stream.StreamD.Type hiding (Stream)

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

    step gst (GBracketIONormal (UnStream step1 st) v ref) = do
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
                    clearingIOFinalizer ref (onExc v e (UnStream step1 st))
                return $ Skip (GBracketIOException stream)
    step gst (GBracketIOException (UnStream step1 st)) = do
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
        (\a (e :: SomeException) _ -> onExc a >> return (nilM (MC.throwM e)))
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
-- /Pre-release/
{-# INLINE bracket3 #-}
bracket3 :: (MonadRunInIO m, MonadCatch m)
    => m b
    -> (b -> m c)
    -> (b -> m d)
    -> (b -> m e)
    -> (b -> Stream m a)
    -> Stream m a
bracket3 bef aft gc exc bet = fromStreamD $
    bracket3D bef aft exc gc (toStreamD . bet)

-- | Run the alloc action @m b@ with async exceptions disabled but keeping
-- blocking operations interruptible (see 'Control.Exception.mask').  Use the
-- output @b@ as input to @b -> Stream m a@ to generate an output stream.
--
-- @b@ is usually a resource under the state of monad @m@, e.g. a file
-- handle, that requires a cleanup after use. The cleanup action @b -> m c@,
-- runs whenever the stream ends normally, due to a sync or async exception or
-- if it gets garbage collected after a partial lazy evaluation.
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
bracket :: (MonadRunInIO m, MonadCatch m)
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
finally :: (MonadRunInIO m, MonadCatch m) =>
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
after :: MonadRunInIO m => m b -> Stream m a -> Stream m a
after action xs = fromStreamD $ afterD action $ toStreamD xs
