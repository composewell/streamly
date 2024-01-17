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
    -- | When @Abs/Rel@ and @File/Dir@ both are present, @Abs/Rel@ must be
    -- outermost constructors and @File/Dir@ as inner. Thus the types File (Abs
    -- a) or Dir (Abs a) are not allowed but Abs (Dir a) and Abs (File a) are
    -- allowed.

      File (..)
    , Dir (..)
    , Abs (..)
    , Rel (..)

    -- * Constraints
    , IsAbsRel
    , NotAbsRel
    , IsDir

    -- * Exceptions
    , PathException (..)

    -- * Conversions
    , IsPath (..)
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

-- | File path. A qualifier to annotate a path as file path, or in general as a
-- leaf node in a tree or graph.
newtype File a = File a

-- | Directory path. A qualifier to annotate a path as dir path, or in general
-- as a non-leaf node in a tree or graph.
newtype Dir a = Dir a

-- | Absolute path. A qualifier to annotate a path which is relative to an
-- explicitly known permanent node called a root node.
newtype Abs a = Abs a

-- | Relative path. A qualifier to annotate a path which is can be relative to
-- anything, not relative to an explicitly known permanent node.
newtype Rel a = Rel a

-- Abs (Dir a) etc. are also covered by these.

-- | Constraint to check if a type has Abs or Rel annotations.
class IsAbsRel a

instance IsAbsRel (Abs a)
instance IsAbsRel (Rel a)

-- | Constraint to check if a type does not have Abs or Rel annotations.
class NotAbsRel a

instance NotAbsRel (File a)
instance NotAbsRel (Dir a)

-- Note that (Abs a) may also be a directory if "a" is (Dir b), but it can also
-- be a file if "a" is (File b). Therefore, the constraints are put on a more
-- spspecific type e.g. (Abs PosixPath) may be a dir.

-- | Constraint to check if a type may be a directory.
class IsDir a

instance IsDir (Dir a)
instance IsDir (Abs (Dir a))
instance IsDir (Rel (Dir a))

------------------------------------------------------------------------------
-- Conversions
------------------------------------------------------------------------------

-- XXX call it IsBase? It is a more abstract concept and can be used for URLs
-- as well, but those can also be called paths. But if we make it more abstract
-- then File/Dir will have to be called something like Leaf/Branch which will
-- become more obscure.

-- | A member of 'IsPath' knows how to convert to and from the base path type.
class IsPath a b where
    -- | Like 'fromPath' but does not check the properties of 'Path'. The user
    -- is responsible to maintain the invariants mentioned in the definition of
    -- 'Path' type otherwise surprising behavior may result.
    --
    -- Provides performance and simplicity when we know that the properties of
    -- the path are already verified, for example, when we get the path from
    -- the file system or the OS APIs.
    unsafeFromPath :: a -> b

    -- | Convert a raw 'Path' to other forms of well-typed paths. It may fail
    -- if the path does not satisfy the properties of the target type.
    --
    -- Path components may have limits.
    -- Total path length may have a limit.
    fromPath :: MonadThrow m => a -> m b

    -- | Convert a well-typed path to a raw 'Path'. Never fails.
    toPath :: b -> a
