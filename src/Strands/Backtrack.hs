{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE InstanceSigs              #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE Rank2Types                #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE UndecidableInstances      #-}

module Strands.AsyncT
    (
    )
where

import           Control.Applicative         (Alternative (..))
import           Control.Monad.Base          (MonadBase (..), liftBaseDefault)
import           Control.Monad.Catch         (MonadThrow, throwM)
import           Control.Monad.State         (MonadIO (..), MonadPlus (..),
                                              MonadState (..), StateT (..),
                                              liftM, modify, runStateT, when)
import           Control.Monad.Trans.Class   (MonadTrans (lift))
import           Control.Monad.Trans.Control (ComposeSt, MonadBaseControl (..),
                                              MonadTransControl (..),
                                              defaultLiftBaseWith,
                                              defaultRestoreM)
import           Data.Dynamic                (Typeable)

import           Strands.Context

--import           Debug.Trace (traceM)

------------------------------------------------------------------------------
-- Utilities
------------------------------------------------------------------------------

-- | If the first parameter is 'Nothing' return the second parameter otherwise
-- return the first parameter..
onNothing :: Monad m => m (Maybe b) -> m b -> m b
onNothing iox iox'= do
       mx <- iox
       case mx of
           Just x -> return x
           Nothing -> iox'

------------------------------------------------------------------------------
-- Backtracking
------------------------------------------------------------------------------

{-
-- | Run the closure  (the 'x' in 'x >>= f') of the current bind operation.
runClosure :: EventF -> StateM m (Maybe a)
runClosure EventF { xcomp = x } = runAsyncT (unsafeCoerce x)

-- | Run the continuation (the 'f' in 'x >>= f') of the current bind operation with the current state.
runContinuation :: MonadIO m => EventF -> a -> StateM m (Maybe b)
runContinuation EventF { fcomp = fs } =
  runAsyncT . (compose $  (unsafeCoerce fs))

data Backtrack b = Show b => Backtrack
    { backtracking :: Maybe b
    , backStack :: [EventF]
    } deriving Typeable

backStateOf :: (Monad m, Show a) => a -> m (Backtrack a)
backStateOf reason = return $ Backtrack (Nothing `asTypeOf` (Just reason)) []

-- | Start the undo process for the given undo track id. Performs all the undo
-- actions registered till now in reverse order. An undo action can use
-- 'forward' to stop the undo process and resume forward execution. If there
-- are no more undo actions registered execution stops and a 'stop' action is
-- returned.
--
back :: (MonadIO m, Typeable b, Show b) => b -> AsyncT m a
back reason = AsyncT $ do
  bs <- getData  `onNothing`  backStateOf  reason
  goBackt  bs

  where

  goBackt (Backtrack _ [] )= return Nothing
  goBackt (Backtrack _ (stack@(first : bs)) )= do
        (setData $ Backtrack (Just reason) stack)

        mr <-  runClosure first

        Backtrack b _ <- getData `onNothing`  backStateOf  reason
        case mr of
           Nothing -> return empty
           Just x -> case b of
                 Nothing -> runContinuation first x
                 justreason -> goBackt $ Backtrack justreason bs

-}
