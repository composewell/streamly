{-# LANGUAGE CPP #-}
-- |
-- Module      : Streamly.Internal.Data.Stream.Exception
-- Copyright   : (c) 2020 Composewell Technologies and Contributors
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

module Streamly.Internal.Data.Stream.Exception
    (
    -- * Resources
      before
    , afterIO
    , afterUnsafe
    , finallyIO
    , finallyUnsafe
    , gbracket_
    , gbracket
    , bracketUnsafe
    , bracketIO3
    , bracketIO
    , cleanupIO
    , cleanupEffectIO

    -- * Exceptions
    , onException
    , ghandle
    , handle
    )
where

#include "inline.hs"

import Control.Monad.IO.Class (MonadIO(..))
import Control.Exception (Exception, SomeException, mask_)
import Control.Monad.Catch (MonadCatch)
import Data.Foldable (sequenceA_)
import Data.IORef (newIORef, readIORef, atomicModifyIORef)
import GHC.Exts (inline)
import Streamly.Internal.Data.IOFinalizer
    (newIOFinalizer, runIOFinalizer, clearingIOFinalizer)

import qualified Control.Monad.Catch as MC

import Streamly.Internal.Data.Stream.Type

#include "DocTestDataStream.hs"

data GbracketState s1 s2 v
    = GBracketInit
    | GBracketNormal s1 v
    | GBracketException s2

-- | Like 'gbracket' but with following differences:
--
-- * alloc action @m c@ runs with async exceptions enabled
-- * cleanup action @c -> m d@ won't run if the stream is garbage collected
--   after partial evaluation.
--
-- /Inhibits stream fusion/
--
-- /Pre-release/
--
{-# INLINE_NORMAL gbracket_ #-}
gbracket_
    :: Monad m
    => m c                                  -- ^ before
    -> (c -> m d)                           -- ^ after, on normal stop
    -> (c -> e -> Stream m b -> m (Stream m b)) -- ^ on exception
    -> (forall s. m s -> m (Either e s))    -- ^ try (exception handling)
    -> (c -> Stream m b)                    -- ^ stream generator
    -> Stream m b
gbracket_ bef aft onExc ftry action =
    Stream step GBracketInit

    where

    {-# INLINE_LATE step #-}
    step _ GBracketInit = do
        r <- bef
        return $ Skip $ GBracketNormal (action r) r

    step gst (GBracketNormal (UnStream step1 st) v) = do
        res <- ftry $ step1 gst st
        case res of
            Right r -> case r of
                Yield x s ->
                    return $ Yield x (GBracketNormal (Stream step1 s) v)
                Skip s -> return $ Skip (GBracketNormal (Stream step1 s) v)
                Stop -> aft v >> return Stop
            -- XXX Do not handle async exceptions, just rethrow them.
            Left e -> do
                strm <- onExc v e (UnStream step1 st)
                return $ Skip (GBracketException strm)
    step gst (GBracketException (UnStream step1 st)) = do
        res <- step1 gst st
        case res of
            Yield x s -> return $ Yield x (GBracketException (Stream step1 s))
            Skip s    -> return $ Skip (GBracketException (Stream step1 s))
            Stop      -> return Stop

data GbracketIOState s1 s2 v wref
    = GBracketIOInit
    | GBracketIONormal s1 v wref
    | GBracketIOException s2

-- | Run the alloc action @m c@ with async exceptions disabled but keeping
-- blocking operations interruptible (see 'Control.Exception.mask').  Use the
-- output @c@ as input to @c -> Stream m b@ to generate an output stream. When
-- generating the stream use the supplied @try@ operation @forall s. m s -> m
-- (Either e s)@ to catch synchronous exceptions. If an exception occurs run
-- the exception handler @c -> e -> Stream m b -> m (Stream m b)@. Note that
-- 'gbracket' does not rethrow the exception, it has to be done by the
-- exception handler if desired.
--
-- The cleanup action @c -> m d@, runs whenever the stream ends normally, due
-- to a sync or async exception or if it gets garbage collected after a partial
-- lazy evaluation.  See 'bracket' for the semantics of the cleanup action.
--
-- 'gbracket' can express all other exception handling combinators.
--
-- /Inhibits stream fusion/
--
-- /Pre-release/
{-# INLINE_NORMAL gbracket #-}
gbracket
    :: MonadIO m
    => IO c -- ^ before
    -> (c -> IO d1) -- ^ on normal stop
    -> (c -> e -> Stream m b -> IO (Stream m b)) -- ^ on exception
    -> (c -> IO d2) -- ^ on GC without normal stop or exception
    -> (forall s. m s -> m (Either e s)) -- ^ try (exception handling)
    -> (c -> Stream m b) -- ^ stream generator
    -> Stream m b
gbracket bef aft onExc onGC ftry action =
    Stream step GBracketIOInit

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
        (r, ref) <- liftIO $ mask_ $ do
            r <- bef
            ref <- newIOFinalizer (onGC r)
            return (r, ref)
        return $ Skip $ GBracketIONormal (action r) r ref

    step gst (GBracketIONormal (UnStream step1 st) v ref) = do
        res <- ftry $ step1 gst st
        case res of
            Right r -> case r of
                Yield x s ->
                    return $ Yield x (GBracketIONormal (Stream step1 s) v ref)
                Skip s ->
                    return $ Skip (GBracketIONormal (Stream step1 s) v ref)
                Stop ->
                    liftIO (clearingIOFinalizer ref (aft v)) >> return Stop
            -- XXX Do not handle async exceptions, just rethrow them.
            Left e -> do
                -- Clearing of finalizer and running of exception handler must
                -- be atomic wrt async exceptions. Otherwise if we have cleared
                -- the finalizer and have not run the exception handler then we
                -- may leak the resource.
                stream <-
                    liftIO (clearingIOFinalizer ref (onExc v e (UnStream step1 st)))
                return $ Skip (GBracketIOException stream)
    step gst (GBracketIOException (UnStream step1 st)) = do
        res <- step1 gst st
        case res of
            Yield x s ->
                return $ Yield x (GBracketIOException (Stream step1 s))
            Skip s    -> return $ Skip (GBracketIOException (Stream step1 s))
            Stop      -> return Stop

-- | Run the action @m b@ before the stream yields its first element.
--
-- Same as the following but more efficient due to fusion:
--
-- >>> before action xs = Stream.concatMap (const xs) (Stream.fromEffect action)
--
{-# INLINE_NORMAL before #-}
before :: Monad m => m b -> Stream m a -> Stream m a
before action (Stream step state) = Stream step' Nothing

    where

    {-# INLINE_LATE step' #-}
    step' _ Nothing = action >> return (Skip (Just state))

    step' gst (Just st) = do
        res <- step gst st
        case res of
            Yield x s -> return $ Yield x (Just s)
            Skip s    -> return $ Skip (Just s)
            Stop      -> return Stop

-- | Like 'after', with following differences:
--
-- * action @m b@ won't run if the stream is garbage collected
--   after partial evaluation.
-- * Monad @m@ does not require any other constraints.
-- * has slightly better performance than 'after'.
--
-- Same as the following, but with stream fusion:
--
-- >>> afterUnsafe action xs = xs <> Stream.nilM action
--
-- /Pre-release/
--
{-# INLINE_NORMAL afterUnsafe #-}
afterUnsafe :: Monad m => m b -> Stream m a -> Stream m a
afterUnsafe action (Stream step state) = Stream step' state

    where

    {-# INLINE_LATE step' #-}
    step' gst st = do
        res <- step gst st
        case res of
            Yield x s -> return $ Yield x s
            Skip s    -> return $ Skip s
            Stop      -> action >> return Stop

-- | Run the action @IO b@ whenever the stream is evaluated to completion, or
-- if it is garbage collected after a partial lazy evaluation.
--
-- The semantics of the action @IO b@ are similar to the semantics of cleanup
-- action in 'bracketIO'.
--
-- /See also 'afterUnsafe'/
--
{-# INLINE_NORMAL afterIO #-}
afterIO :: MonadIO m
    => IO b -> Stream m a -> Stream m a
afterIO action (Stream step state) = Stream step' Nothing

    where

    {-# INLINE_LATE step' #-}
    step' _ Nothing = do
        ref <- liftIO $ newIOFinalizer action
        return $ Skip $ Just (state, ref)
    step' gst (Just (st, ref)) = do
        res <- step gst st
        case res of
            Yield x s -> return $ Yield x (Just (s, ref))
            Skip s    -> return $ Skip (Just (s, ref))
            Stop      -> do
                runIOFinalizer ref
                return Stop

-- XXX For high performance error checks in busy streams we may need another
-- Error constructor in step.

-- | Run the action @m b@ if the stream evaluation is aborted due to an
-- exception. The exception is not caught, simply rethrown.
--
-- Observes exceptions only in the stream generation, and not in stream
-- consumers.
--
-- /Inhibits stream fusion/
--
{-# INLINE_NORMAL onException #-}
onException :: MonadCatch m => m b -> Stream m a -> Stream m a
onException action stream =
    gbracket_
        (return ()) -- before
        return      -- after
        (\_ (e :: MC.SomeException) _ -> action >> MC.throwM e)
        (inline MC.try)
        (const stream)

{-# INLINE_NORMAL _onException #-}
_onException :: MonadCatch m => m b -> Stream m a -> Stream m a
_onException action (Stream step state) = Stream step' state

    where

    {-# INLINE_LATE step' #-}
    step' gst st = do
        res <- step gst st `MC.onException` action
        case res of
            Yield x s -> return $ Yield x s
            Skip s    -> return $ Skip s
            Stop      -> return Stop

-- | Like 'bracket' but with following differences:
--
-- * alloc action @m b@ runs with async exceptions enabled
-- * cleanup action @b -> m c@ won't run if the stream is garbage collected
--   after partial evaluation.
-- * has slightly better performance than 'bracketIO'.
--
-- /Inhibits stream fusion/
--
-- /Pre-release/
--
{-# INLINE_NORMAL bracketUnsafe #-}
bracketUnsafe :: MonadCatch m
    => m b -> (b -> m c) -> (b -> Stream m a) -> Stream m a
bracketUnsafe bef aft =
    gbracket_
        bef
        aft
        (\a (e :: SomeException) _ -> aft a >> MC.throwM e)
        (inline MC.try)

-- For a use case of this see the "streamly-process" package. It needs to kill
-- the process in case of exception or garbage collection, but waits for the
-- process to terminate in normal cases.

-- | Like 'bracketIO' but can use 3 separate cleanup actions depending on the
-- mode of termination:
--
-- 1. When the stream stops normally
-- 2. When the stream is garbage collected
-- 3. When the stream encounters an exception
--
-- @bracketIO3 before onStop onGC onException action@ runs @action@ using the
-- result of @before@. If the stream stops, @onStop@ action is executed, if the
-- stream is abandoned @onGC@ is executed, if the stream encounters an
-- exception @onException@ is executed.
--
-- The exception is not caught, it is rethrown.
--
-- /Inhibits stream fusion/
--
-- /Pre-release/
{-# INLINE_NORMAL bracketIO3 #-}
bracketIO3 :: (MonadIO m, MonadCatch m) =>
       IO b
    -> (b -> IO c)
    -> (b -> IO d)
    -> (b -> IO e)
    -> (b -> Stream m a)
    -> Stream m a
bracketIO3 bef aft onExc onGC =
    gbracket
        bef
        aft
        (\a (e :: SomeException) _ -> onExc a >> MC.throwM e)
        onGC
        (inline MC.try)

-- | Run a monadic effect supplying it with a function to register cleanup
-- actions that are automatically invoked on exception or after the effect
-- function is done.
{-# INLINE cleanupEffectIO #-}
cleanupEffectIO :: (MonadIO m, MonadCatch m) =>
    ((IO () -> IO ()) -> m ()) -> m ()
cleanupEffectIO action = do
    ref <- liftIO $ newIORef []
    -- XXX use mask or MC.finally?
    action (register ref) `MC.onException` aft ref
    aft ref

    where

    aft ref = liftIO $ do
        xs <- readIORef ref
        sequenceA_ xs

    register ref f =
        atomicModifyIORef ref (\xs -> (f : xs, ()))

-- | Run the alloc action @IO b@ with async exceptions disabled but keeping
-- blocking operations interruptible (see 'Control.Exception.mask').  Use the
-- output @b@ of the IO action as input to the function @b -> Stream m a@ to
-- generate an output stream.
--
-- @b@ is usually a resource under the IO monad, e.g. a file handle, that
-- requires a cleanup after use. The cleanup action @b -> IO c@, runs whenever
-- (1) the stream ends normally, (2) due to a sync or async exception or, (3)
-- if it gets garbage collected after a partial lazy evaluation. The exception
-- is not caught, it is rethrown.
--
-- 'bracketIO' only guarantees that the cleanup action runs, and it runs with
-- async exceptions enabled. The action must ensure that it can successfully
-- cleanup the resource in the face of sync or async exceptions.
--
-- When the stream ends normally or on a sync exception, cleanup action runs
-- immediately in the current thread context, whereas in other cases it runs in
-- the GC context, therefore, cleanup may be delayed until the GC gets to run.
-- An example where GC based cleanup happens is when a stream is being folded
-- but the fold terminates without draining the entire stream or if the
-- consumer of the stream encounters an exception.
--
-- Observes exceptions only in the stream generation, and not in stream
-- consumers.
--
-- /See also: 'bracketUnsafe'/
--
-- /Inhibits stream fusion/
--
{-# INLINE bracketIO #-}
bracketIO :: (MonadIO m, MonadCatch m)
    => IO b -> (b -> IO c) -> (b -> Stream m a) -> Stream m a
bracketIO bef aft = bracketIO3 bef aft aft aft

-- | Run a stream supplying it a function to register cleanup actions which are
-- automatically called on exception or when the stream is done.
{-# INLINE cleanupIO #-}
cleanupIO :: (MonadIO m, MonadCatch m) =>
    ((IO () -> IO ()) -> Stream m a) -> Stream m a
cleanupIO action = do
    bracketIO bef aft (\(_, reg) -> action reg)

    where

    bef = do
        ref <- liftIO $ newIORef []
        return (ref, register ref)

    aft (ref, _) = liftIO $ do
        xs <- readIORef ref
        sequenceA_ xs

    register ref f =
        atomicModifyIORef ref (\xs -> (f : xs, ()))

data BracketState s v = BracketInit | BracketRun s v

-- | Alternate (custom) implementation of 'bracket'.
--
{-# INLINE_NORMAL _bracket #-}
_bracket :: MonadCatch m
    => m b -> (b -> m c) -> (b -> Stream m a) -> Stream m a
_bracket bef aft bet = Stream step' BracketInit

    where

    {-# INLINE_LATE step' #-}
    step' _ BracketInit = bef >>= \x -> return (Skip (BracketRun (bet x) x))

    -- NOTE: It is important to use UnStream instead of the Stream pattern
    -- here, otherwise we get huge perf degradation, see note in concatMap.
    step' gst (BracketRun (UnStream step state) v) = do
        -- res <- step gst state `MC.onException` aft v
        res <- inline MC.try $ step gst state
        case res of
            Left (e :: SomeException) -> aft v >> MC.throwM e >> return Stop
            Right r -> case r of
                Yield x s -> return $ Yield x (BracketRun (Stream step s) v)
                Skip s    -> return $ Skip (BracketRun (Stream step s) v)
                Stop      -> aft v >> return Stop

-- | Like 'finally' with following differences:
--
-- * action @m b@ won't run if the stream is garbage collected
--   after partial evaluation.
-- * has slightly better performance than 'finallyIO'.
--
-- /Inhibits stream fusion/
--
-- /Pre-release/
--
{-# INLINE finallyUnsafe #-}
finallyUnsafe :: MonadCatch m => m b -> Stream m a -> Stream m a
finallyUnsafe action xs = bracketUnsafe (return ()) (const action) (const xs)

-- | Run the action @IO b@ whenever the stream stream stops normally, aborts
-- due to an exception or if it is garbage collected after a partial lazy
-- evaluation.
--
-- The semantics of running the action @IO b@ are similar to the cleanup action
-- semantics described in 'bracketIO'.
--
-- >>> finallyIO release = Stream.bracketIO (return ()) (const release)
--
-- /See also 'finallyUnsafe'/
--
-- /Inhibits stream fusion/
--
{-# INLINE finallyIO #-}
finallyIO :: (MonadIO m, MonadCatch m) => IO b -> Stream m a -> Stream m a
finallyIO action xs = bracketIO3 (return ()) act act act (const xs)
    where act _ = action

-- | Like 'handle' but the exception handler is also provided with the stream
-- that generated the exception as input. The exception handler can thus
-- re-evaluate the stream to retry the action that failed. The exception
-- handler can again call 'ghandle' on it to retry the action multiple times.
--
-- This is highly experimental. In a stream of actions we can map the stream
-- with a retry combinator to retry each action on failure.
--
-- /Inhibits stream fusion/
--
-- /Pre-release/
--
{-# INLINE_NORMAL ghandle #-}
ghandle :: (MonadCatch m, Exception e)
    => (e -> Stream m a -> m (Stream m a)) -> Stream m a -> Stream m a
ghandle f stream =
    gbracket_ (return ()) return (const f) (inline MC.try) (const stream)

-- | When evaluating a stream if an exception occurs, stream evaluation aborts
-- and the specified exception handler is run with the exception as argument.
-- The exception is caught and handled unless the handler decides to rethrow
-- it. Note that exception handling is not applied to the stream returned by
-- the exception handler.
--
-- Observes exceptions only in the stream generation, and not in stream
-- consumers.
--
-- /Inhibits stream fusion/
--
{-# INLINE_NORMAL handle #-}
handle :: (MonadCatch m, Exception e)
    => (e -> m (Stream m a)) -> Stream m a -> Stream m a
handle f stream =
    gbracket_ (return ()) return (\_ e _ -> f e) (inline MC.try) (const stream)

-- | Alternate (custom) implementation of 'handle'.
--
{-# INLINE_NORMAL _handle #-}
_handle :: (MonadCatch m, Exception e)
    => (e -> Stream m a) -> Stream m a -> Stream m a
_handle f (Stream step state) = Stream step' (Left state)

    where

    {-# INLINE_LATE step' #-}
    step' gst (Left st) = do
        res <- inline MC.try $ step gst st
        case res of
            Left e -> return $ Skip $ Right (f e)
            Right r -> case r of
                Yield x s -> return $ Yield x (Left s)
                Skip s    -> return $ Skip (Left s)
                Stop      -> return Stop

    step' gst (Right (UnStream step1 st)) = do
        res <- step1 gst st
        case res of
            Yield x s -> return $ Yield x (Right (Stream step1 s))
            Skip s    -> return $ Skip (Right (Stream step1 s))
            Stop      -> return Stop
