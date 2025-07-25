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
    , finallyIO'
    , finallyIO''
    , finallyUnsafe
    , gbracket_
    , gbracket
    , bracketUnsafe
    , bracketIO3
    , bracketIO
    , bracketIO'
    , bracketIO''

    , withAcquireIO
    , withAcquireIO'

    -- * Exceptions
    , onException
    , onExceptionE
    , ghandle
    , handle
    )
where

#include "inline.hs"

import Control.Monad.IO.Class (MonadIO(..))
import Control.Exception (Exception, SomeException, mask_)
import Control.Monad.Catch (MonadCatch)
import Data.IORef (newIORef)
import GHC.Exts (inline)
import Streamly.Internal.Control.Exception
    (AcquireIO(..), acquire, allocator, releaser)
import Streamly.Internal.Data.IOFinalizer
    (newIOFinalizer, runIOFinalizer, clearingIOFinalizer)

import qualified Control.Monad.Catch as MC
import qualified Data.IntMap.Strict as Map

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
        -- allocation of resource and installation of finalizer must be atomic
        -- with respect to async exception, otherwise we may leave a window
        -- where the resource may not be freed.
        (r, ref) <- liftIO $ mask_ $ do
            r <- bef
            ref <- newIOFinalizer (onGC r)
            return (r, ref)
        return $ Skip $ GBracketIONormal (action r) r ref

    step gst (GBracketIONormal (UnStream step1 st) v ref) = do
        -- IMPORTANT: Note that if an async exception occurs before try or
        -- after try, in those cases the exception will not be intercepted and
        -- the cleanup handler won't run. In those cases the cleanup handler
        -- will run via GC.
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

{-# INLINE_NORMAL onExceptionE #-}
onExceptionE :: forall e m a. (Exception e, MonadCatch m) => (forall b. e -> m b) -> Stream m a -> Stream m a
onExceptionE onE stream =
    gbracket_
        (return ()) -- before
        return      -- after
        (\_ (e :: e) _ -> onE e)
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

-- XXX Just use bracketIO2 instead - stop and exception.

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

-- XXX Fix the early termination case not being prompt. Will require a "final"
-- function in the stream constructor.

-- Examples of cases where the stream is not fully consumed:
--
-- * a bracketed stream is folded but before the stream ends, the fold
-- terminates or encounters an exception abandoning the original stream.
-- * 'take' on a bracketed stream terminates without draining the stream
-- completely. To avoid this, bracket should be outermost combinator on a
-- stream.
-- * A synchronous exception is handled using 'handle', in that case the
-- original stream is abandoned and collected by GC.
--
-- In case of async exceptions, if the async exception occurs when we are
-- executing the stream code then it will be intercepted. After the stream
-- element is generated, control is handed over to the consumer (fold), async
-- exceptions occurring in this period are not intercepted by bracketIO, they
-- are intercepted by the fold's bracket instead. If an async exceptions occurs
-- in this part and the stream is abandoned, the cleanup handler runs on GC.

-- | The alloc action @IO b@ is executed with async exceptions disabled but keeping
-- blocking operations interruptible (see 'Control.Exception.mask').  Uses the
-- output @b@ of the IO action as input to the function @b -> Stream m a@ to
-- generate an output stream.
--
-- @b@ is usually a resource allocated under the IO monad, e.g. a file handle, that
-- requires a cleanup after use. The cleanup is done using the @b -> IO c@
-- action. bracketIO guarantees that the allocated resource is eventually (see
-- details below) cleaned up even in the face of sync or async exceptions. If
-- an exception occurs it is not caught, simply rethrown.
--
-- 'bracketIO' only guarantees that the cleanup action runs, and it runs with
-- __async exceptions enabled__. The action must ensure that it can successfully
-- cleanup the resource in the face of sync or async exceptions.
--
-- /Best case/: Cleanup happens immediately in the following cases:
--
-- * the stream is consumed completely
-- * an exception occurs in the bracketed part of the pipeline
--
-- /Worst case/: In the following cases cleanup is deferred to GC.
--
-- * the bracketed stream is partially consumed and abandoned
-- * pipeline is aborted due to an exception outside the bracket
--
-- Use Streamly.Control.Exception.'Streamly.Control.Exception.withAcquireIO'
-- for covering the entire pipeline with guaranteed cleanup at the end of
-- bracket.
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

-- If you are recovering from exceptions using 'handle' then you should use
-- bracketIO'' which releases the resource promptly on exception before the
-- exception handler generates another stream. But for better performance
-- bracketIO' may be better and leave the resource to be freed by GC.
--
-- XXX If we want to recover from exceptions then we should probably have an
-- integrated combinator combining handling with bracketIO'' otherwise we will
-- have multiple layers of "try" which will not be good for perf.

data GbracketIO'State s ref release
    = GBracketIO'Init
    | GBracketIO'Normal s ref release

-- | Like 'bracketIO' but requires an 'Streamly.Control.Exception.AcquireIO' reference in the underlying monad
-- of the stream, and guarantees that all resources are freed before the
-- scope of the monad level resource manager
-- (Streamly.Control.Exception.'Streamly.Control.Exception.withAcquireIO')
-- ends. Where fusion matters, this combinator can be much faster than 'bracketIO' as it
-- allows stream fusion.
--
-- /Best case/: Cleanup happens immediately if the stream is consumed
-- completely.
--
-- /Worst case/: In the following cases cleanup is guaranteed to occur at the
-- end of the monad level bracket. However, if a GC occurs then cleanup will
-- occur even earlier than that.
--
-- * the bracketed stream is partially consumed and abandoned
-- * pipeline is aborted due to an exception
--
-- This is the recommended default bracket operation.
--
-- Note: You can use 'Streamly.Control.Exception.acquire' directly, instead of using this combinator, if
-- you don’t need to release the resource when the stream ends. However, if
-- you're using the stream inside another stream (like with concatMap), you
-- usually do want to release it at the end of the stream.
--
-- /Allows stream fusion/
--
{-# INLINE bracketIO' #-}
bracketIO' :: MonadIO m
    => AcquireIO -> IO b -> (b -> IO c) -> (b -> Stream m a) -> Stream m a
bracketIO' bracket alloc free action =
    Stream step GBracketIO'Init

    where

    -- In nested stream cases, where the inner stream is abandoned due to early
    -- termination or due to exception handling, we use GC based cleanup as
    -- fallback because the monad level cleanup may not occur in deterministic
    -- amount of time, but GC may. Users can also implement backpressure
    -- themselves e.g. if the number of open fds is greater than n then perform
    -- GC until it comes down.
    {-# INLINE_LATE step #-}
    step _ GBracketIO'Init = do
        (r, ref, release) <- liftIO $ mask_ $ do
            (r, release) <- liftIO $ acquire bracket alloc free
            ref <- newIOFinalizer release
            return (r, ref, release)
        return $ Skip $ GBracketIO'Normal (action r) ref release

    step gst (GBracketIO'Normal (UnStream step1 st) ref release) = do
        res <- step1 gst st
        case res of
            Yield x s ->
                return $ Yield x (GBracketIO'Normal (Stream step1 s) ref release)
            Skip s ->
                return $ Skip (GBracketIO'Normal (Stream step1 s) ref release)
            Stop ->
                liftIO (clearingIOFinalizer ref release) >> return Stop

-- | Like bracketIO, the only difference is that there is a guarantee that the
-- resources will be freed at the end of the monad level bracket
-- ('Streamly.Control.Exception.AcquireIO').
--
-- /Best case/: Cleanup happens immediately in the following cases:
--
-- * the stream is consumed completely
-- * an exception occurs in the bracketed part of the pipeline
--
-- /Worst case/: In the following cases cleanup is guaranteed to occur at the
-- end of the monad level bracket. However, if a GC occurs before that then
-- cleanup will occur early.
--
-- * the bracketed stream is partially consumed and abandoned
-- * pipeline is aborted due to an exception outside the bracket
--
-- Note: Instead of using this combinator you can directly use
-- 'Streamly.Control.Exception.acquire'
-- if you do not care about releasing the resource at the end of the stream
-- and if you are not recovering from an exception using 'handle'. You may want
-- to care about releasing the resource at the end of a stream if you are using
-- it in a nested manner (e.g. in concatMap).
--
-- /Inhibits stream fusion/
--
{-# INLINE bracketIO'' #-}
bracketIO'' :: (MonadIO m, MonadCatch m)
    => AcquireIO -> IO b -> (b -> IO c) -> (b -> Stream m a) -> Stream m a
bracketIO'' bracket alloc free action =
    Stream step GBracketIO'Init

    where

    {-# INLINE_LATE step #-}
    step _ GBracketIO'Init = do
        (r, ref, release) <- liftIO $ mask_ $ do
            (r, release) <- liftIO $ acquire bracket alloc free
            ref <- newIOFinalizer release
            return (r, ref, release)
        return $ Skip $ GBracketIO'Normal (action r) ref release

    step gst (GBracketIO'Normal (UnStream step1 st) ref release) = do
        -- If an async exception occurs before try or after try, in those cases
        -- the exception will not be intercepted here. In those cases the
        -- release action will run via AcquireIO release hook.
        res <- MC.try $ step1 gst st
        case res of
            Right r ->
                case r of
                    Yield x s ->
                        return
                            $ Yield x (GBracketIO'Normal (Stream step1 s) ref release)
                    Skip s ->
                        return
                            $ Skip (GBracketIO'Normal (Stream step1 s) ref release)
                    Stop ->
                        liftIO (clearingIOFinalizer ref release) >> return Stop
            Left (e :: SomeException) ->
                liftIO (clearingIOFinalizer ref release) >> MC.throwM e

-- | Like finallyIO, based on bracketIO' semantics.
{-# INLINE finallyIO' #-}
finallyIO' :: MonadIO m => AcquireIO -> IO b -> Stream m a -> Stream m a
finallyIO' bracket free stream =
    bracketIO' bracket (return ()) (const free) (const stream)

-- | Like finallyIO, based on bracketIO'' semantics.
{-# INLINE finallyIO'' #-}
finallyIO'' :: (MonadIO m, MonadCatch m) =>
    AcquireIO -> IO b -> Stream m a -> Stream m a
finallyIO'' bracket free stream =
    bracketIO'' bracket (return ()) (const free) (const stream)

-- | Like 'bracketIO' but with on-demand allocations and manual release
-- facility.
--
-- Here is an example:
--
-- >>> :{
-- close x h = do
--  putStrLn $ "closing: " ++ x
--  hClose h
-- :}
--
-- >>> :{
-- generate ref =
--      Stream.fromList ["file1", "file2"]
--    & Stream.mapM
--        (\x -> do
--            (h, release) <- Exception.acquire ref (openFile x ReadMode) (close x)
--            -- use h here
--            threadDelay 1000000
--            when (x == "file1") $ do
--                putStrLn $ "Manually releasing: " ++ x
--                release
--            return x
--        )
--    & Stream.trace print
-- :}
--
-- >>> :{
-- run =
--     Stream.withAcquireIO generate
--         & Stream.fold Fold.drain
-- :}
--
-- In the above code, you should see the \"closing:\" message for both the
-- files, and only once for each file. Make sure you create "file1" and "file2"
-- before running it.
--
-- Here is an example for just registering hooks to be called eventually:
--
-- >>> :{
-- generate ref =
--      Stream.fromList ["file1", "file2"]
--    & Stream.mapM
--        (\x -> do
--            Exception.register ref $ putStrLn $ "saw: " ++ x
--            threadDelay 1000000
--            return x
--        )
--    & Stream.trace print
-- :}
--
-- >>> :{
-- run =
--     Stream.withAcquireIO generate
--         & Stream.fold Fold.drain
-- :}
--
-- In the above code, even if you interrupt the program with CTRL-C you should
-- still see the "saw:" message for the elements seen before the interrupt.
--
-- See 'bracketIO' documentation for the caveats related to partially consumed
-- streams and async exceptions.
--
-- Use monad level bracket Streamly.Control.Exception.'Streamly..Control.Exception.withAcquireIO'
-- for guaranteed cleanup in the entire pipeline, however, monad level bracket does not provide
-- an automatic cleanup at the end of the stream; you can only release
-- resources manually or via automatic cleanup at the end of the monad bracket.
-- The end of stream cleanup is useful especially in nested streams where we
-- want to cleanup at the end of every inner stream instead of waiting for the
-- outer stream to end for cleaning up to happen.
--
{-# INLINE withAcquireIO #-}
withAcquireIO :: (MonadIO m, MonadCatch m) =>
    (AcquireIO -> Stream m a) -> Stream m a
withAcquireIO action = do
    bracketIO bef (releaser . fst) (\(_, alloc) -> action alloc)

    where

    bef = do
        -- Assuming 64-bit int counter will never overflow
        ref <- liftIO $ newIORef (0 :: Int, Map.empty, Map.empty)
        return (ref, AcquireIO (allocator ref))

-- | We can also combine the stream local 'withAcquireIO' with the global monad
-- level bracket
-- Streamly.Internal.Control.Exception.'Streamly.Internal.Control.Exception.withAcquireIO'.
-- The release actions returned by the local allocator can be registered to be
-- called by the monad level bracket. This way we can guarantee that in the
-- worst case release actions happen at the end of bracket and do not depend on
-- GC. This is the most powerful way of allocating resources on-demand with
-- manual release inside a stream. If required a custom combinator can be
-- written to register the local allocator's release in the global allocator
-- automatically.
--
-- /Unimplemented/
{-# INLINE withAcquireIO' #-}
withAcquireIO' :: -- (MonadIO m, MonadCatch m) =>
    AcquireIO -> (AcquireIO -> Stream m a) -> Stream m a
withAcquireIO' _globalAlloc _action = undefined

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
-- >>> finallyIO release stream = Stream.bracketIO (return ()) (const release) (const stream)
--
-- See also finallyIO' for stricter resource release guarantees.
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
