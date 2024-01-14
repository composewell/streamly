-- |
-- Module      : Streamly.Internal.Data.Path
-- Copyright   : (c) 2023 Composewell Technologies
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Portability : GHC
--
module Streamly.Internal.Data.Path
    (
    -- * Path Types
      File (..)
    , Dir (..)
    , Abs (..)
    , Rel (..)
    , PathException (..)

    -- * Conversions
    , IsPath (..)
    , adapt
    )
where

import Control.Exception (Exception)
import Control.Monad.Catch (MonadThrow(..))

------------------------------------------------------------------------------
-- Exceptions
------------------------------------------------------------------------------

-- | Exceptions thrown by path operations.
data PathException =
    InvalidPath String
  | InvalidAbsPath String
  | InvalidRelPath String
  | InvalidFilePath String
  | InvalidDirPath String
    deriving (Show,Eq)

instance Exception PathException

------------------------------------------------------------------------------
-- Types
------------------------------------------------------------------------------

-- XXX Do we need a type for file or dir Name as names cannot have the
-- separator char and there may be other restrictions on names? For example,
-- length restriction.  A file name cannot be "." or "..". We can use the types
-- "File Name" and "Dir Name" to represent names. Also, file systems may put
-- limits on names. Can have an IsName type class with members Name, (File
-- Name), (Dir Name).

-- | A type representing a file path.
newtype File a = File a

-- | A type representing a directory path.
newtype Dir a = Dir a

-- | A type representing absolute paths.
newtype Abs a = Abs a

-- | A type representing relative paths.
newtype Rel a = Rel a

------------------------------------------------------------------------------
-- Conversions
------------------------------------------------------------------------------

-- XXX call it IsBase? It is a more abstract concept and can be used for URLs
-- as well, but those can also be called paths. But if we make it more abstract
-- then File/Dir will have to be called something like Leaf/Branch which will
-- become more obscure.

-- | A member of 'IsPath' knows how to convert to and from the base path type.
class IsPath f a where
    -- | Like 'fromPath' but does not check the properties of 'Path'. The user
    -- is responsible to maintain the invariants mentioned in the definition of
    -- 'Path' type otherwise surprising behavior may result.
    --
    -- Provides performance and simplicity when we know that the properties of
    -- the path are already verified, for example, when we get the path from
    -- the file system or the OS APIs.
    unsafeFromPath :: a -> f a

    -- | Convert a raw 'Path' to other forms of well-typed paths. It may fail
    -- if the path does not satisfy the properties of the target type.
    --
    -- Path components may have limits.
    -- Total path length may have a limit.
    fromPath :: MonadThrow m => a -> m (f a)

    -- | Convert a well-typed path to a raw 'Path'. Never fails.
    toPath :: f a -> a

-- XXX Use rewrite rules to eliminate intermediate conversions for better
-- efficiency.

-- | Convert a path type to another path type. This operation may fail with a
-- 'PathException' when converting a less restrictive path type to a more
-- restrictive one.
adapt :: (MonadThrow m, IsPath a p, IsPath b p) => a p -> m (b p)
adapt p = fromPath $ toPath p
