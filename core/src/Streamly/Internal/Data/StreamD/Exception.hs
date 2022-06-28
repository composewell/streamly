-- |
-- Module      : Streamly.Internal.Data.StreamD.Exception
-- Copyright   : (c) 2020 Composewell Technologies and Contributors
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

module Streamly.Internal.Data.StreamD.Exception
    (
      gbracket_
    , gbracket
    , before
    , after_
    , after
    , bracket_
    , bracket'
    , onException
    , finally_
    , finally
    , ghandle
    , handle
    , retry
    )
where

#include "inline.hs"

import Control.Exception (Exception, SomeException, mask_)
import Control.Monad.Catch (MonadCatch)
import Data.Map.Strict (Map)
import GHC.Exts (inline)
import Streamly.Internal.Control.Concurrent (MonadRunInIO, MonadAsync, withRunInIO)
import Streamly.Internal.Data.IOFinalizer
    (newIOFinalizer, runIOFinalizer, clearingIOFinalizer)

import qualified Control.Monad.Catch as MC
import qualified Data.Map.Strict as Map

import Streamly.Internal.Data.StreamD.Type

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
-- /Pre-release/
--
{-# INLINE_NORMAL gbracket_ #-}
gbracket_
    :: Monad m
    => m c                                  -- ^ before
    -> (c -> m d)                           -- ^ after, on normal stop
    -> (c -> e -> Stream m b -> Stream m b) -- ^ on exception
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
            Left e ->
                return
                    $ Skip (GBracketException (onExc v e (UnStream step1 st)))
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
    :: MonadRunInIO m
    => m c -- ^ before
    -> (c -> m d1) -- ^ on normal stop
    -> (c -> e -> Stream m b -> m (Stream m b)) -- ^ on exception
    -> (c -> m d2) -- ^ on GC without normal stop or exception
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
                    return $ Yield x (GBracketIONormal (Stream step1 s) v ref)
                Skip s ->
                    return $ Skip (GBracketIONormal (Stream step1 s) v ref)
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
                return $ Yield x (GBracketIOException (Stream step1 s))
            Skip s    -> return $ Skip (GBracketIOException (Stream step1 s))
            Stop      -> return Stop

-- | See 'Streamly.Internal.Data.Stream.IsStream.before'.
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
after :: MonadRunInIO m
    => m b -> Stream m a -> Stream m a
after action (Stream step state) = Stream step' Nothing

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

-- XXX For high performance error checks in busy streams we may need another
-- Error constructor in step.
--
-- | See 'Streamly.Internal.Data.Stream.IsStream.onException'.
--
{-# INLINE_NORMAL onException #-}
onException :: MonadCatch m => m b -> Stream m a -> Stream m a
onException action stream =
    gbracket_
        (return ()) -- before
        return      -- after
        (\_ (e :: MC.SomeException) _ -> nilM (action >> MC.throwM e))
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

-- | See 'Streamly.Internal.Data.Stream.IsStream.bracket_'.
--
{-# INLINE_NORMAL bracket_ #-}
bracket_ :: MonadCatch m
    => m b -> (b -> m c) -> (b -> Stream m a) -> Stream m a
bracket_ bef aft =
    gbracket_
        bef
        aft
        (\a (e :: SomeException) _ -> nilM (aft a >> MC.throwM e))
        (inline MC.try)

-- | See 'Streamly.Internal.Data.Stream.IsStream.bracket'.
--
{-# INLINE_NORMAL bracket' #-}
bracket' :: (MonadAsync m, MonadCatch m) =>
       m b
    -> (b -> m c)
    -> (b -> m d)
    -> (b -> m e)
    -> (b -> Stream m a)
    -> Stream m a
bracket' bef aft onExc onGC =
    gbracket
        bef
        aft
        (\a (e :: SomeException) _ -> onExc a >> return (nilM (MC.throwM e)))
        onGC
        (inline MC.try)

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

-- | See 'Streamly.Internal.Data.Stream.IsStream.finally_'.
--
{-# INLINE finally_ #-}
finally_ :: MonadCatch m => m b -> Stream m a -> Stream m a
finally_ action xs = bracket_ (return ()) (const action) (const xs)

-- | See 'Streamly.Internal.Data.Stream.IsStream.finally'.
--
-- finally action xs = after action $ onException action xs
--
{-# INLINE finally #-}
finally :: (MonadAsync m, MonadCatch m) => m b -> Stream m a -> Stream m a
finally action xs = bracket' (return ()) act act act (const xs)
    where act _ = action

-- | See 'Streamly.Internal.Data.Stream.IsStream.ghandle'.
--
{-# INLINE_NORMAL ghandle #-}
ghandle :: (MonadCatch m, Exception e)
    => (e -> Stream m a -> Stream m a) -> Stream m a -> Stream m a
ghandle f stream =
    gbracket_ (return ()) return (const f) (inline MC.try) (const stream)

-- | See 'Streamly.Internal.Data.Stream.IsStream.handle'.
--
{-# INLINE_NORMAL handle #-}
handle :: (MonadCatch m, Exception e)
    => (e -> Stream m a) -> Stream m a -> Stream m a
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

data RetryState emap s1 s2
    = RetryWithMap emap s1
    | RetryDefault s2

-- | See 'Streamly.Internal.Data.Stream.IsStream.retry'
--
{-# INLINE_NORMAL retry #-}
retry
    :: forall e m a. (Exception e, Ord e, MonadCatch m)
    => Map e Int
       -- ^ map from exception to retry count
    -> (e -> Stream m a)
       -- ^ default handler for those exceptions that are not in the map
    -> Stream m a
    -> Stream m a
retry emap0 defaultHandler (Stream step0 state0) = Stream step state

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
    step gst (RetryDefault (UnStream step1 state1)) = do
        res <- step1 gst state1
        return
            $ case res of
                  Yield x st1 -> Yield x $ RetryDefault (Stream step1 st1)
                  Skip st1 -> Skip $ RetryDefault (Stream step1 st1)
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
