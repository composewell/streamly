{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables       #-}

module Strands.Log
    ( Loggable
    , Replaying
    , LogEntry (..)
    , Log (..)
    , IDynamic (..)
    , fromIDyn
    , toIDyn
    )
where

import           Unsafe.Coerce          (unsafeCoerce)

------------------------------------------------------------------------------
-- Log types
------------------------------------------------------------------------------

-- | Constraint type synonym for a value that can be logged.
type Loggable a = (Show a, Read a)

------------------------------------------------------------------------------
-- Serialization
------------------------------------------------------------------------------

-- | Dynamic serializable data for logging.
data IDynamic =
    IDyns String
  | forall a. Loggable a => IDynamic a

instance Show IDynamic where
  show (IDynamic x) = show (show x)
  show (IDyns    s) = show s

instance Read IDynamic where
  readsPrec _ str = map (\(x,s) -> (IDyns x,s)) $ readsPrec 0 str

fromIDyn :: Loggable a => IDynamic -> a
fromIDyn (IDynamic x) = r
    where r = unsafeCoerce x

fromIDyn (IDyns s) = r `seq` r
    where r = read s

toIDyn :: Loggable a => a -> IDynamic
toIDyn x = IDynamic x

------------------------------------------------------------------------------
-- Log
------------------------------------------------------------------------------

data LogEntry        =
      Executing        -- we are inside this computation, not yet done
    | Waiting          -- the computation is done and it returned mzero
    | Result IDynamic  -- computation is done and we have the result to replay
  deriving (Read, Show)

type Replaying = Bool

data Log = Log Replaying [LogEntry] deriving (Show)
