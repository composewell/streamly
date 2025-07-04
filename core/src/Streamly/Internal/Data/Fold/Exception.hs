-- |
-- Module      : Streamly.Internal.Data.Fold.Exception
-- Copyright   : (c) 2025 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
module Streamly.Internal.Data.Fold.Exception
    (
    -- * Resources
      before
    , bracketIO
    , finallyIO

    -- * Exceptions
    , onException
    )
where

------------------------------------------------------------------------------
-- Imports
------------------------------------------------------------------------------

import Streamly.Internal.Data.Tuple.Strict (Tuple'(..))
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Catch (MonadCatch)
import Streamly.Internal.Data.IOFinalizer (newIOFinalizer, runIOFinalizer)

import qualified Control.Monad.Catch as MC

import Streamly.Internal.Data.Fold.Step
import Streamly.Internal.Data.Fold.Type

------------------------------------------------------------------------------
-- Exceptions
------------------------------------------------------------------------------

{-

-- | Exception handling states of a fold
data HandleExc s f1 f2 = InitDone !s | InitFailed !f1 | StepFailed !f2

-- | @handle initHandler stepHandler fold@ produces a new fold from a given
-- fold.  The new fold executes the original @fold@, if an exception occurs
-- when initializing the fold then @initHandler@ is executed and fold resulting
-- from that starts execution.  If an exception occurs while executing the
-- @step@ function of a fold then the @stephandler@ is executed and we start
-- executing the fold resulting from that.
--
-- The exception is caught and handled, not rethrown. If the exception handler
-- itself throws an exception that exception is thrown.
--
-- /Internal/
--
{-# INLINE handle #-}
handle :: (MonadCatch m, Exception e)
    => (e -> m (Fold m a b))
    -> (e -> Fold m a b -> m (Fold m a b))
    -> Fold m a b
    -> Fold m a b
handle initH stepH (Fold step1 initial1 extract1) = Fold step initial extract

    where

    initial = fmap InitDone initial1 `MC.catch` (fmap InitFailed . initH)

    step (InitDone s) a =
        let f = Fold step1 (return s) extract1
         in fmap InitDone (step1 s a)
                `MC.catch` (\e -> fmap StepFailed (stepH e f))
    step (InitFailed (Fold step2 initial2 extract2)) a = do
        s <- initial2
        s1 <- step2 s a
        return $ InitFailed $ Fold step2 (return s1) extract2
    step (StepFailed (Fold step2 initial2 extract2)) a = do
        s <- initial2
        s1 <- step2 s a
        return $ StepFailed $ Fold step2 (return s1) extract2

    extract (InitDone s) = extract1 s
    extract (InitFailed (Fold _ initial2 extract2)) = initial2 >>= extract2
    extract (StepFailed (Fold _ initial2 extract2)) = initial2 >>= extract2

-}

-- | @onException action fold@ runs @action@ whenever the fold throws an
-- exception.  The action is executed on any exception whether it is in
-- initial, step or extract action of the fold.
--
-- The exception is not caught, simply rethrown. If the @action@ itself
-- throws an exception that exception is thrown instead of the original
-- exception.
--
-- /Internal/
--
{-# INLINE onException #-}
onException :: MonadCatch m => m x -> Fold m a b -> Fold m a b
onException action (Fold step1 initial1 extract1 final1) =
    Fold step initial extract final

    where

    initial = initial1 `MC.onException` action
    step s a = step1 s a `MC.onException` action
    extract s = extract1 s `MC.onException` action
    final s = final1 s `MC.onException` action

-- | @bracketIO before after between@ runs @before@ and invokes @between@ using
-- its output, then runs the fold generated by @between@.  If the fold ends
-- normally, due to an exception or if it is garbage collected prematurely then
-- @after@ is run with the output of @before@ as argument.
--
-- If @before@ or @after@ throw an exception that exception is thrown.
--
{-# INLINE bracketIO #-}
bracketIO :: (MonadIO m, MonadCatch m)
    => IO x -> (x -> IO c) -> (x -> Fold m a b) -> Fold m a b
bracketIO bef aft bet = Fold step initial extract final

    where

    initial = do
        r <- liftIO bef
        ref <- liftIO $ newIOFinalizer (aft r)
        case bet r of
            Fold step1 initial1 extract1 final1 -> do
                res <- initial1 `MC.onException` liftIO (runIOFinalizer ref)
                case res of
                    Partial s -> do
                        let fld1 = Fold step1 (pure (Partial s)) extract1 final1
                        pure $ Partial $ Tuple' ref fld1
                    Done b -> do
                        liftIO $ runIOFinalizer ref
                        pure $ Done b

    step (Tuple' ref (Fold step1 initial1 extract1 final1)) a = do
        res <- initial1
        case res of
            Partial s -> do
                s1 <- step1 s a `MC.onException` liftIO (runIOFinalizer ref)
                let fld1 = Fold step1 (pure s1) extract1 final1
                pure $ Partial $ Tuple' ref fld1
            Done b -> do
                liftIO $ runIOFinalizer ref
                pure $ Done b

    extract (Tuple' ref (Fold _ initial1 extract1 _)) = do
        res <- initial1
        case res of
            Partial s -> extract1 s `MC.onException` liftIO (runIOFinalizer ref)
            Done b -> pure b

    final (Tuple' ref (Fold _ initial1 _ final1)) = do
        res <- initial1
        case res of
            Partial s -> do
                val <- final1 s `MC.onException` liftIO (runIOFinalizer ref)
                runIOFinalizer ref
                pure val
            Done b -> pure b

-- | Run a side effect whenever the fold stops normally, aborts due to an
-- exception or is garbage collected.
--
{-# INLINE finallyIO #-}
finallyIO :: (MonadIO m, MonadCatch m) => IO b -> Fold m a b -> Fold m a b
finallyIO aft (Fold step1 initial1 extract1 final1) =
    Fold step initial extract final

    where

    initial = do
        ref <- liftIO $ newIOFinalizer aft
        res <- initial1 `MC.onException` liftIO (runIOFinalizer ref)
        pure $ case res of
            Done b -> Done b
            Partial s -> Partial $ Tuple' ref s

    step (Tuple' ref s) a = do
        res <- step1 s a `MC.onException` liftIO (runIOFinalizer ref)
        pure $ case res of
            Done b -> Done b
            Partial s1 -> Partial $ Tuple' ref s1

    extract (Tuple' ref s) =
        extract1 s `MC.onException` liftIO (runIOFinalizer ref)

    final (Tuple' ref s) = do
        res <- final1 s `MC.onException` liftIO (runIOFinalizer ref)
        liftIO $ runIOFinalizer ref
        pure res


-- | Run a side effect before the initialization of the fold.
--
{-# INLINE before #-}
before :: Monad m => m x -> Fold m a b -> Fold m a b
before effect (Fold s i e f) = Fold s (effect *> i) e f
