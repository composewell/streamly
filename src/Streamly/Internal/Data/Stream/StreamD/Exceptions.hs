#include "inline.hs"

-- |
-- Module      : Streamly.Internal.Data.Stream.StreamD.Exceptions
-- Copyright   : (c) 2020 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

module Streamly.Internal.Data.Stream.StreamD.Exceptions
    (
    -- * Exceptions
      newFinalizedIORef
    , runIORefFinalizer
    , withIORefFinalizer
    , gbracket_
    , gbracket
    , before
    , after_
    , after
    , bracket_
    , bracket
    , onException
    , finally_
    , finally
    , ghandle
    , handle
    )
where


import Control.Exception
       (Exception, SomeException, mask_)
import Control.Monad (void)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Control (MonadBaseControl, liftBaseOp_, control)
import Data.IORef (newIORef, readIORef, mkWeakIORef, writeIORef, IORef)

import qualified Control.Monad.Catch as MC

import Prelude hiding
       ( map, mapM, mapM_, repeat, foldr, last, take, filter
       , takeWhile, drop, dropWhile, all, any, maximum, minimum, elem
       , notElem, null, head, tail, zipWith, lookup, foldr1, sequence
       , (!!), scanl, scanl1, concatMap, replicate, enumFromTo, concat
       , reverse, iterate, splitAt)
import Streamly.Internal.Data.Stream.StreamD.Type
import Streamly.Internal.Data.Stream.StreamD.Common
import Streamly.Internal.Data.SVar


------------------------------------------------------------------------------
-- Exceptions
------------------------------------------------------------------------------

data GbracketState s1 s2 v
    = GBracketInit
    | GBracketNormal s1 v
    | GBracketException s2

-- | Like 'gbracket' but with following differences:
--
-- * alloc action @m c@ runs with async exceptions enabled
-- * cleanup action @c -> m d@ won't run if the stream is garbage collected
--   after partial evaluation.
-- * does not require a 'MonadAsync' constraint.
--
-- /Inhibits stream fusion/
--
-- /Internal/
--
{-# INLINE_NORMAL gbracket_ #-}
gbracket_
    :: Monad m
    => m c                                  -- ^ before
    -> (forall s. m s -> m (Either e s))    -- ^ try (exception handling)
    -> (c -> m d)                           -- ^ after, on normal stop
    -> (c -> e -> Stream m b -> Stream m b) -- ^ on exception
    -> (c -> Stream m b)                    -- ^ stream generator
    -> Stream m b
gbracket_ bef exc aft fexc fnormal =
    Stream step GBracketInit

    where

    {-# INLINE_LATE step #-}
    step _ GBracketInit = do
        r <- bef
        return $ Skip $ GBracketNormal (fnormal r) r

    step gst (GBracketNormal (UnStream step1 st) v) = do
        res <- exc $ step1 gst st
        case res of
            Right r -> case r of
                Yield x s ->
                    return $ Yield x (GBracketNormal (Stream step1 s) v)
                Skip s -> return $ Skip (GBracketNormal (Stream step1 s) v)
                Stop -> aft v >> return Stop
            -- XXX Do not handle async exceptions, just rethrow them.
            Left e ->
                return $ Skip (GBracketException (fexc v e (UnStream step1 st)))
    step gst (GBracketException (UnStream step1 st)) = do
        res <- step1 gst st
        case res of
            Yield x s -> return $ Yield x (GBracketException (Stream step1 s))
            Skip s    -> return $ Skip (GBracketException (Stream step1 s))
            Stop      -> return Stop

------------------------------------------------------------------------------
-- Finalizers
------------------------------------------------------------------------------

-- | Make a finalizer from a monadic action @m a@ that can run in IO monad.
mkIOFinalizer :: MonadBaseControl IO m => m b -> m (IO ())
mkIOFinalizer f = do
    mrun <- captureMonadState
    return $
        void $ do
            _ <- runInIO mrun f
            return ()

-- | Run an IO action stored in a finalized IORef.
runIORefFinalizerGC :: IORef (Maybe (IO ())) -> IO ()
runIORefFinalizerGC ref = do
    res <- readIORef ref
    case res of
        Nothing -> return ()
        Just f -> f

-- | Create an IORef holding a finalizer that is called automatically when
-- the IORef is garbage collected. The IORef can be written to with a 'Nothing'
-- value to deactivate the finalizer.
--
-- Note: The finalizer is always run with the state of the monad captured at
-- the time of calling newFinalizedIORef. To run it on garbage collection we
-- have no option but to take a snapshot of the monadic state at some point of
-- time. For normal case we could run it with the current state of the monad
-- but we want to keep both the cases consistent.
--
newFinalizedIORef :: (MonadIO m, MonadBaseControl IO m)
    => m a -> m (IORef (Maybe (IO ())))
newFinalizedIORef finalizer = do
    f <- mkIOFinalizer finalizer
    ref <- liftIO $ newIORef $ Just f
    _ <- liftIO $ mkWeakIORef ref (runIORefFinalizerGC ref)
    return ref

-- | Run the finalizer stored in an IORef and deactivate it so that it is run
-- only once. Note, the action runs with async exceptions masked.
--
runIORefFinalizer :: MonadIO m => IORef (Maybe (IO ())) -> m ()
runIORefFinalizer ref = liftIO $ do
    res <- readIORef ref
    case res of
        Nothing -> return ()
        Just action -> do
            -- if an async exception comes after writing 'Nothing' then the
            -- finalizing action will never be run. We need to do this
            -- atomically wrt async exceptions.
            mask_ $ do
                writeIORef ref Nothing
                action

-- | Run an action clearing the finalizer IORef atomically wrt async
-- exceptions. The action is run with async exceptions masked.
withIORefFinalizer :: MonadBaseControl IO m
    => IORef (Maybe (IO ())) -> m a -> m a
withIORefFinalizer ref action = do
    control $ \runinio ->
        mask_ $ do
            writeIORef ref Nothing
            runinio action

------------------------------------------------------------------------------

data GbracketIOState s1 s2 v wref
    = GBracketIOInit
    | GBracketIONormal s1 v wref
    | GBracketIOException s2

-- | Run the alloc action @m c@ with async exceptions disabled but keeping
-- blocking operations interruptible (see 'Control.Exception.mask').  Use the
-- output @c@ as input to @c -> Stream m b@ to generate an output stream. When
-- generating the stream use the supplied @try@ operation @forall s. m s -> m
-- (Either e s)@ to catch synchronous exceptions. If an exception occurs run
-- the exception handler @c -> e -> Stream m b -> m (Stream m b)@.
--
-- The cleanup action @c -> m d@, runs whenever the stream ends normally, due
-- to a sync or async exception or if it gets garbage collected after a partial
-- lazy evaluation.  See 'bracket' for the semantics of the cleanup action.
--
-- 'gbracket' can express all other exception handling combinators.
--
-- /Inhibits stream fusion/
--
-- /Internal/
{-# INLINE_NORMAL gbracket #-}
gbracket
    :: (MonadIO m, MonadBaseControl IO m)
    => m c                                  -- ^ before
    -> (forall s. m s -> m (Either e s))    -- ^ try (exception handling)
    -> (c -> m d)                           -- ^ after, on normal stop or GC
    -> (c -> e -> Stream m b -> m (Stream m b)) -- ^ on exception
    -> (c -> Stream m b)                    -- ^ stream generator
    -> Stream m b
gbracket bef exc aft fexc fnormal =
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
        (r, ref) <- liftBaseOp_ mask_ $ do
            r <- bef
            ref <- newFinalizedIORef (aft r)
            return (r, ref)
        return $ Skip $ GBracketIONormal (fnormal r) r ref

    step gst (GBracketIONormal (UnStream step1 st) v ref) = do
        res <- exc $ step1 gst st
        case res of
            Right r -> case r of
                Yield x s ->
                    return $ Yield x (GBracketIONormal (Stream step1 s) v ref)
                Skip s ->
                    return $ Skip (GBracketIONormal (Stream step1 s) v ref)
                Stop -> do
                    runIORefFinalizer ref
                    return Stop
            -- XXX Do not handle async exceptions, just rethrow them.
            Left e -> do
                -- Clearing of finalizer and running of exception handler must
                -- be atomic wrt async exceptions. Otherwise if we have cleared
                -- the finalizer and have not run the exception handler then we
                -- may leak the resource.
                stream <- withIORefFinalizer ref (fexc v e (UnStream step1 st))
                return $ Skip (GBracketIOException stream)
    step gst (GBracketIOException (UnStream step1 st)) = do
        res <- step1 gst st
        case res of
            Yield x s ->
                return $ Yield x (GBracketIOException (Stream step1 s))
            Skip s    -> return $ Skip (GBracketIOException (Stream step1 s))
            Stop      -> return Stop

-- | See 'Streamly.Internal.Data.Stream.IsStream.after_'.
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

-- | See 'Streamly.Internal.Data.Stream.IsStream.after_'.
--
{-# INLINE_NORMAL after_ #-}
after_ :: Monad m => m b -> Stream m a -> Stream m a
after_ action (Stream step state) = Stream step' state

    where

    {-# INLINE_LATE step' #-}
    step' gst st = do
        res <- step gst st
        case res of
            Yield x s -> return $ Yield x s
            Skip s    -> return $ Skip s
            Stop      -> action >> return Stop

-- | See 'Streamly.Internal.Data.Stream.IsStream.after'.
--
{-# INLINE_NORMAL after #-}
after :: (MonadIO m, MonadBaseControl IO m)
    => m b -> Stream m a -> Stream m a
after action (Stream step state) = Stream step' Nothing

    where

    {-# INLINE_LATE step' #-}
    step' _ Nothing = do
        ref <- newFinalizedIORef action
        return $ Skip $ Just (state, ref)
    step' gst (Just (st, ref)) = do
        res <- step gst st
        case res of
            Yield x s -> return $ Yield x (Just (s, ref))
            Skip s    -> return $ Skip (Just (s, ref))
            Stop      -> do
                runIORefFinalizer ref
                return Stop

-- XXX For high performance error checks in busy streams we may need another
-- Error constructor in step.
--
-- | See 'Streamly.Internal.Data.Stream.IsStream.onException'.
--
{-# INLINE_NORMAL onException #-}
onException :: MonadCatch m => m b -> Stream m a -> Stream m a
onException action str =
    gbracket_ (return ()) MC.try return
        (\_ (e :: MC.SomeException) _ -> nilM (action >> MC.throwM e))
        (\_ -> str)

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

-- | See 'Streamly.Internal.Data.Stream.IsStream.bracket_'.
--
{-# INLINE_NORMAL bracket_ #-}
bracket_ :: MonadCatch m
    => m b -> (b -> m c) -> (b -> Stream m a) -> Stream m a
bracket_ bef aft bet =
    gbracket_ bef MC.try aft
        (\a (e :: SomeException) _ -> nilM (aft a >> MC.throwM e)) bet

-- | See 'Streamly.Internal.Data.Stream.IsStream.bracket'.
--
{-# INLINE_NORMAL bracket #-}
bracket :: (MonadAsync m, MonadCatch m)
    => m b -> (b -> m c) -> (b -> Stream m a) -> Stream m a
bracket bef aft bet =
    gbracket bef MC.try aft
        (\a (e :: SomeException) _ -> aft a >> return (nilM (MC.throwM e))) bet

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
        res <- MC.try $ step gst state
        case res of
            Left (e :: SomeException) -> aft v >> MC.throwM e >> return Stop
            Right r -> case r of
                Yield x s -> return $ Yield x (BracketRun (Stream step s) v)
                Skip s    -> return $ Skip (BracketRun (Stream step s) v)
                Stop      -> aft v >> return Stop

-- | See 'Streamly.Internal.Data.Stream.IsStream.finally_'.
--
{-# INLINE finally_ #-}
finally_ :: MonadCatch m => m b -> Stream m a -> Stream m a
finally_ action xs = bracket_ (return ()) (\_ -> action) (const xs)

-- | See 'Streamly.Internal.Data.Stream.IsStream.finally'.
--
-- finally action xs = after action $ onException action xs
--
{-# INLINE finally #-}
finally :: (MonadAsync m, MonadCatch m) => m b -> Stream m a -> Stream m a
finally action xs = bracket (return ()) (\_ -> action) (const xs)

-- | See 'Streamly.Internal.Data.Stream.IsStream.ghandle'.
--
{-# INLINE_NORMAL ghandle #-}
ghandle :: (MonadCatch m, Exception e)
    => (e -> Stream m a -> Stream m a) -> Stream m a -> Stream m a
ghandle f str =
    gbracket_ (return ()) MC.try return (\_ -> f) (\_ -> str)

-- | See 'Streamly.Internal.Data.Stream.IsStream.handle'.
--
{-# INLINE_NORMAL handle #-}
handle :: (MonadCatch m, Exception e)
    => (e -> Stream m a) -> Stream m a -> Stream m a
handle f str =
    gbracket_ (return ()) MC.try return (\_ e _ -> f e) (\_ -> str)

-- | Alternate (custom) implementation of 'handle'.
--
{-# INLINE_NORMAL _handle #-}
_handle :: (MonadCatch m, Exception e)
    => (e -> Stream m a) -> Stream m a -> Stream m a
_handle f (Stream step state) = Stream step' (Left state)

    where

    {-# INLINE_LATE step' #-}
    step' gst (Left st) = do
        res <- MC.try $ step gst st
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
