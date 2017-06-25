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

module Strands.State
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

------------------------------------------------------------------------------
-- * Extensible State: Session Data Management
------------------------------------------------------------------------------

-- | Retrieve a previously stored data item of the given data type from the
-- monad state. The data type to retrieve is implicitly determined from the
-- requested type context.
-- If the data item is not found, an 'empty' value (a void event) is returned.
-- Remember that an empty value stops the monad computation. If you want to
-- print an error message or a default value in that case, you can use an
-- 'Alternative' composition. For example:
--
-- > getSData <|> error "no data"
-- > getInt = getSData <|> return (0 :: Int)
getSData ::  (Monad m, Typeable a) => AsyncT m a
getSData = AsyncT getData

-- | 'setSData' stores a data item in the monad state which can be retrieved
-- later using 'getData' or 'getSData'. Stored data items are keyed by their
-- data type, and therefore the data type must be 'Typeable' and only one item
-- of a given type can be stored. A newtype wrapper can be used to distinguish
-- two data items of the same type when required.
--
-- @
-- import Control.Monad.IO.Class (liftIO)
-- import Transient.Base
-- import Data.Typeable
--
-- data Person = Person
--    { name :: String
--    , age :: Int
--    } deriving Typeable
--
-- main = keep $ do
--      setSData $ Person "Alberto"  55
--      Person name age <- getSData
--      liftIO $ print (name, age)
-- @
setSData ::  (Monad m, Typeable a) => a -> AsyncT m ()
setSData x = AsyncT $ setData x >> return (Just ())

-- | Accepts a function that takes the current value of the stored data type
-- and returns the modified value. If the function returns 'Nothing' the value
-- is deleted otherwise updated.
modifySData :: (Monad m, Typeable a) => (Maybe a -> Maybe a) -> AsyncT m ()
modifySData x = AsyncT $ modifyData x >> return (Just ())

-- | Delete the data item of the given type from the monad state.
delSData :: (Monad m, Typeable a) => a -> AsyncT m ()
delSData x = AsyncT $ delData x >> return (Just ())

------------------------------------------------------------------------------
-- MonadState for AsyncT m
------------------------------------------------------------------------------

instance (Monad m, Monad (AsyncT m)) => MonadState Context (AsyncT m) where
  get     = AsyncT $ get   >>= return . Just
  put x   = AsyncT $ put x >>  return (Just ())
  state f = AsyncT $ do
    s <- get
    let ~(a, s') = f s
    put s'
    return $ Just a

{-
-- | Run an action, if the result is a void action undo any state changes
-- that it might have caused.
try :: MonadIO m => AsyncT m a -> AsyncT m a
try mx = do
  sd <- gets mfData
  mx <|> (modify (\s -> s { mfData = sd }) >> empty)

-- | Executes the computation and reset the state either if it fails or not
sandbox :: MonadIO m => AsyncT m a -> AsyncT m a
sandbox mx = do
  sd <- gets mfData
  mx <*** modify (\s ->s { mfData = sd})
    -}
