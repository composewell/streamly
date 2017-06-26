{-# LANGUAGE ConstraintKinds           #-}

module Strands.Log
    ( Loggable
    , LogEntry (..)
    , Replaying
    , Log (..)
    )
where

------------------------------------------------------------------------------
-- Log types
------------------------------------------------------------------------------

-- | Constraint type synonym for a value that can be logged.
type Loggable a = (Show a, Read a)

data LogEntry        =
      Executing    -- we are inside this computation, not yet done
    | Maybe String -- computation is done and we have the result to replay
  deriving (Read, Show)

type Replaying = Bool

data Log = Log Replaying [LogEntry] deriving (Read, Show)
