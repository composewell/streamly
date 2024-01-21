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
    deriving (Show, Eq)

instance Exception PathException

------------------------------------------------------------------------------
-- Conversions
------------------------------------------------------------------------------

-- XXX Swap the order of IsPath arguments?
-- XXX rename to fromBase, fromBasePath, fromOsPath?

-- | If the type @a b@ is a member of 'IsPath' it means we know how to convert
-- the type @b@ to and from the base type @a@.
--
class IsPath a b where
    -- | Like 'fromPath' but does not check the properties of 'Path'. The user
    -- is responsible to maintain the invariants enforced by the type @b@
    -- otherwise surprising behavior may result.
    --
    -- This operation provides performance and simplicity when we know that the
    -- properties of the path are already verified, for example, when we get
    -- the path from the file system or from the OS APIs.
    unsafeFromPath :: a -> b

    -- | Convert a base path type to other forms of well-typed paths. It may
    -- fail if the path does not satisfy the properties of the target type.
    --
    fromPath :: MonadThrow m => a -> m b

    -- | Convert a well-typed path to the base path type. Never fails.
    toPath :: b -> a
