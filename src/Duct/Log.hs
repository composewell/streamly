{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables       #-}

module Duct.Log
    ( Log(..)
    , LogElem(..)
    )
where

import           Control.Exception (SomeException, catch)
import           Data.Dynamic      (Typeable, typeOf)
import           System.IO.Unsafe  (unsafePerformIO)

------------------------------------------------------------------------------
-- Log types
------------------------------------------------------------------------------

-- | Constraint type synonym for a value that can be logged.
type Loggable a = (Show a, Read a, Typeable a)

-- | Dynamic serializable data for logging.
data IDynamic =
    IDyns String
  | forall a. Loggable a => IDynamic a

instance Show IDynamic where
  show (IDynamic x) = show (show x)
  show (IDyns    s) = show s

readWithErr :: (Typeable a, Read a) => String -> IO [(a, String)]
readWithErr line =
  (v `seq` return [(v, left)])
     `catch` (\(_ :: SomeException) ->
                error $ "read error trying to read type: \"" ++ show (typeOf v)
                     ++ "\" in:  " ++ " <" ++ show line ++ "> ")
  where [(v, left)] = readsPrec 0 line

readsPrec' :: (Typeable a, Read a) => t -> String -> [(a, String)]
readsPrec' _ = unsafePerformIO . readWithErr

instance Read IDynamic where
  readsPrec n str = map (\(x,s) -> (IDyns x,s)) $ readsPrec' n str

data LogElem        =  Wait | Exec | Var IDynamic
  deriving (Read, Show)

type Recover        = Bool
type CurrentPointer = [LogElem]
type LogEntries     = [LogElem]

data Log            = Log Recover CurrentPointer LogEntries
  deriving (Typeable, Show)

