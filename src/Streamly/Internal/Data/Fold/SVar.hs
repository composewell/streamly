{-# OPTIONS_GHC -Wno-deprecations #-}
-- |
-- Module      : Streamly.Internal.Data.Fold.SVar
-- Copyright   : (c) 2017 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
--
module Streamly.Internal.Data.Fold.SVar
    (
      write
    , writeLimited
    )
where

#include "inline.hs"

import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Streamly.Internal.Data.Fold (Fold(..))

import qualified Streamly.Internal.Data.Fold as FL (Step(Done, Partial))

import Streamly.Internal.Data.SVar

-- | A fold to write a stream to an SVar. Unlike 'toSVar' this does not allow
-- for concurrent evaluation of the stream, as the fold receives the input one
-- element at a time, it just forwards the elements to the SVar. However, we
-- can safely execute the fold in an independent thread, the SVar can act as a
-- buffer decoupling the sender from the receiver. Also, we can have multiple
-- folds running concurrently pusing the streams to the SVar.
--
{-# INLINE write #-}
write :: MonadIO m => SVar t m a -> Maybe WorkerInfo -> Fold m a ()
write svar winfo = Fold step initial return final

    where

    initial = return $ FL.Partial ()

    -- XXX we can have a separate fold for unlimited buffer case to avoid a
    -- branch in the step here.
    step () x =
        liftIO $ do
            decrementBufferLimit svar
            void $ send svar (ChildYield x)
            return $ FL.Partial ()

    final () = liftIO $ sendStop svar winfo

-- | Like write, but applies a yield limit.
--
{-# INLINE writeLimited #-}
writeLimited :: MonadIO m
    => SVar t m a -> Maybe WorkerInfo -> Fold m a ()
writeLimited svar winfo = Fold step initial (const (return ())) final

    where

    initial = return $ FL.Partial True

    step True x =
        liftIO $ do
            yieldLimitOk <- decrementYieldLimit svar
            if yieldLimitOk
            then do
                decrementBufferLimit svar
                void $ send svar (ChildYield x)
                return $ FL.Partial True
            else do
                cleanupSVarFromWorker svar
                sendStop svar winfo
                return $ FL.Done ()
    step False _ = return $ FL.Done ()

    final True = liftIO $ sendStop svar winfo
    final False = return ()
