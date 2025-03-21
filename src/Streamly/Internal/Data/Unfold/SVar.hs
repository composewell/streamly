{-# OPTIONS_GHC -Wno-deprecations #-}
-- |
-- Module      : Streamly.Internal.Data.Unfold.SVar
-- Copyright   : (c) 2019 Composewell Technologies
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
module Streamly.Internal.Data.Unfold.SVar
    {-# DEPRECATED "The functionality is moved to Channel.*" #-}
    (
      fromSVar
    , fromProducer
    )
where

#include "inline.hs"

import Control.Exception (fromException)
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO(..))
import Data.IORef (newIORef, readIORef, mkWeakIORef, writeIORef)
import Data.Maybe (isNothing)
import Streamly.Internal.Control.Concurrent (MonadAsync)
import Streamly.Internal.Data.Stream (Step(..))
import Streamly.Internal.Data.Time.Clock (Clock(Monotonic), getTime)
import Streamly.Internal.Data.SVar (printSVar, cleanupSVar, sendStopToProducer)
import Streamly.Internal.Data.Unfold (Unfold(..))
import System.Mem (performMajorGC)

import qualified Control.Monad.Catch as MC

import Streamly.Internal.Data.SVar.Type
-------------------------------------------------------------------------------
-- Generation from SVar
-------------------------------------------------------------------------------

data FromSVarState t m a =
      FromSVarInit (SVar t m a)
    | FromSVarRead (SVar t m a)
    | FromSVarLoop (SVar t m a) [ChildEvent a]
    | FromSVarDone (SVar t m a)

-- | /Internal/
--
{-# INLINE_NORMAL fromSVar #-}
fromSVar :: MonadAsync m => Unfold m (SVar t m a) a
fromSVar = Unfold step (return . FromSVarInit)
    where

    {-# INLINE_LATE step #-}
    step (FromSVarInit svar) = do
        ref <- liftIO $ newIORef ()
        _ <- liftIO $ mkWeakIORef ref hook
        -- when this copy of svar gets garbage collected "ref" will get
        -- garbage collected and our GC hook will be called.
        let sv = svar{svarRef = Just ref}
        return $ Skip (FromSVarRead sv)

        where

        {-# NOINLINE hook #-}
        hook = do
            when (svarInspectMode svar) $ do
                r <- liftIO $ readIORef (svarStopTime (svarStats svar))
                when (isNothing r) $
                    printSVar svar "SVar Garbage Collected"
            cleanupSVar svar
            -- If there are any SVars referenced by this SVar a GC will prompt
            -- them to be cleaned up quickly.
            when (svarInspectMode svar) performMajorGC

    step (FromSVarRead sv) = do
        list <- readOutputQ sv
        -- Reversing the output is important to guarantee that we process the
        -- outputs in the same order as they were generated by the constituent
        -- streams.
        return $ Skip $ FromSVarLoop sv (Prelude.reverse list)

    step (FromSVarLoop sv []) = do
        done <- postProcess sv
        return $ Skip $ if done
                      then FromSVarDone sv
                      else FromSVarRead sv

    step (FromSVarLoop sv (ev : es)) = do
        case ev of
            ChildYield a -> return $ Yield a (FromSVarLoop sv es)
            ChildStop tid e -> do
                accountThread sv tid
                case e of
                    Nothing -> do
                        stop <- shouldStop tid
                        if stop
                        then do
                            liftIO (cleanupSVar sv)
                            return $ Skip (FromSVarDone sv)
                        else return $ Skip (FromSVarLoop sv es)
                    Just ex ->
                        case fromException ex of
                            Just ThreadAbort ->
                                return $ Skip (FromSVarLoop sv es)
                            Nothing -> liftIO (cleanupSVar sv) >> MC.throwM ex
        where

        shouldStop tid =
            case svarStopStyle sv of
                StopNone -> return False
                StopAny -> return True
                StopBy -> do
                    sid <- liftIO $ readIORef (svarStopBy sv)
                    return $ tid == sid

    step (FromSVarDone sv) = do
        when (svarInspectMode sv) $ do
            t <- liftIO $ getTime Monotonic
            liftIO $ writeIORef (svarStopTime (svarStats sv)) (Just t)
            liftIO $ printSVar sv "SVar Done"
        return Stop

-------------------------------------------------------------------------------
-- Process events received by a fold consumer from a stream producer
-------------------------------------------------------------------------------

-- XXX Have an error for "FromSVarInit" instead of undefined. The error can
-- redirect the user to report the failure to the developers.
-- | /Internal/
--
{-# INLINE_NORMAL fromProducer #-}
fromProducer :: MonadAsync m => Unfold m (SVar t m a) a
fromProducer = Unfold step (return . FromSVarRead)
    where

    {-# INLINE_LATE step #-}
    step (FromSVarRead sv) = do
        list <- readOutputQ sv
        -- Reversing the output is important to guarantee that we process the
        -- outputs in the same order as they were generated by the constituent
        -- streams.
        return $ Skip $ FromSVarLoop sv (Prelude.reverse list)

    step (FromSVarLoop sv []) = return $ Skip $ FromSVarRead sv
    step (FromSVarLoop sv (ev : es)) = do
        case ev of
            ChildYield a -> return $ Yield a (FromSVarLoop sv es)
            ChildStop tid e -> do
                accountThread sv tid
                case e of
                    Nothing -> do
                        sendStopToProducer sv
                        return $ Skip (FromSVarDone sv)
                    Just _ -> error "Bug: fromProducer: received exception"

    step (FromSVarDone sv) = do
        when (svarInspectMode sv) $ do
            t <- liftIO $ getTime Monotonic
            liftIO $ writeIORef (svarStopTime (svarStats sv)) (Just t)
            liftIO $ printSVar sv "SVar Done"
        return Stop

    step (FromSVarInit _) = undefined
