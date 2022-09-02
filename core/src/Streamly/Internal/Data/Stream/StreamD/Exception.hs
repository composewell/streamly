-- |
-- Module      : Streamly.Internal.Data.Stream.StreamD.Exception
-- Copyright   : (c) 2020 Composewell Technologies and Contributors
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

module Streamly.Internal.Data.Stream.StreamD.Exception
    (
      before
    , onException
    , ghandle
    , handle
    , retry
    )
where

#include "inline.hs"

import Control.Exception (Exception)
import Control.Monad.Catch (MonadCatch)
import Data.Map.Strict (Map)
import GHC.Exts (inline)

import qualified Control.Monad.Catch as MC
import qualified Data.Map.Strict as Map

import Streamly.Internal.Data.Stream.StreamD.Type

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


-- | See 'Streamly.Internal.Data.Stream.before'.
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

-- XXX For high performance error checks in busy streams we may need another
-- Error constructor in step.
--
-- | See 'Streamly.Internal.Data.Stream.onException'.
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


-- | See 'Streamly.Internal.Data.Stream.ghandle'.
--
{-# INLINE_NORMAL ghandle #-}
ghandle :: (MonadCatch m, Exception e)
    => (e -> Stream m a -> Stream m a) -> Stream m a -> Stream m a
ghandle f stream =
    gbracket_ (return ()) return (const f) (inline MC.try) (const stream)

-- | See 'Streamly.Internal.Data.Stream.handle'.
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

-- | See 'Streamly.Internal.Data.Stream.retry'
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
