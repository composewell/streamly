-- |
-- Module      : Streamly.Internal.Data.Path
-- Copyright   : (c) 2023 Composewell Technologies
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Portability : GHC
--
module Streamly.Internal.Data.Path
    (
    -- * Exceptions
      PathException (..)

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
newtype PathException =
    InvalidPath String
    deriving (Show,Eq)

instance Exception PathException

------------------------------------------------------------------------------
-- Conversions
------------------------------------------------------------------------------

-- XXX call it IsBase? It is a more abstract concept and can be used for URLs
-- as well, but those can also be called paths. But if we make it more abstract
-- then File/Dir will have to be called something like Leaf/Branch which will
-- become more obscure.

-- | A member of 'IsPath' knows how to convert to and from the base path type.
-- Create instances such that the type @b@ is one of:
--
--  * File a
--  * Dir a
--  * Abs a
--  * Rel a
--  * Abs (File a)
--  * Abs (Dir a)
--  * Rel (File a)
--  * Rel (Dir a)
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
