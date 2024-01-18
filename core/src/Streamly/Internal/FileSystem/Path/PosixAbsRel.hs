{-# LANGUAGE TemplateHaskell #-}
-- For constraints on "append"
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- |
-- Module      : Streamly.Internal.FileSystem.Path.Posix
-- Copyright   : (c) 2023 Composewell Technologies
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Portability : GHC
--
module Streamly.Internal.FileSystem.Path.PosixAbsRel
    (
    -- * Path Types
      Abs (..)
    , Rel (..)
    , IsAbsRel

    -- * Conversions
    , IsPath (..)
    , adapt

    -- * Construction
    , absFromString
    , relFromString

    -- * Statically Verified String Literals
    -- quasiquoters
    , abs
    , rel

    -- * Statically Verified Strings
    -- XXX Do we need these if we have quasiquoters? These may be useful if we
    -- are generating strings statically using methods other than literals or
    -- if we are doing some text processing on strings before using them.
    -- TH macros
    , mkAbs
    , mkRel

    -- * Operations
    , append
    )
where

import Control.Monad.Catch (MonadThrow(..))
import Data.Word (Word8)
import Language.Haskell.TH.Syntax (lift)
import Streamly.Internal.Data.Array (Array(..))
import Streamly.Internal.FileSystem.Path.Common (OS(..), mkQ)
import Streamly.Internal.FileSystem.Path.Posix (PosixPath(..))

import qualified Streamly.Internal.FileSystem.Path.Common as Common
import qualified Streamly.Internal.FileSystem.Path.Posix as Posix

import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Streamly.Internal.Data.Path
import Prelude hiding (abs)

{- $setup
>>> :m
>>> :set -XQuasiQuotes

For APIs that have not been released yet.

>>> import qualified Streamly.Internal.FileSystem.Path.Posix as Path
-}

newtype Abs a = Abs a
newtype Rel a = Rel a

instance IsPath PosixPath (Abs PosixPath) where
    unsafeFromPath = Abs
    fromPath p = pure (Abs p)
    toPath (Abs p) = p

instance IsPath PosixPath (Rel PosixPath) where
    unsafeFromPath = Rel
    fromPath p = pure (Rel p)
    toPath (Rel p) = p

-- | Constraint to check if a type has Abs or Rel annotations.
class IsAbsRel a

instance IsAbsRel (Abs a)
instance IsAbsRel (Rel a)

-- XXX Use rewrite rules to eliminate intermediate conversions for better
-- efficiency. If the argument path is already verfied for a property, we
-- should not verify it again e.g. if we adapt (Abs path) as (Abs (Dir path))
-- then we should not verify it to be Abs again.

-- | Convert a path type to another path type. This operation may fail with a
-- 'PathException' when converting a less restrictive path type to a more
-- restrictive one.
--
-- You can only upgrade or downgrade type safety. Converting Abs to Rel or File
-- to Dir will definitely fail.
adapt :: (MonadThrow m, IsPath PosixPath a, IsPath PosixPath b) => a -> m b
adapt p = fromPath (toPath p :: PosixPath)

------------------------------------------------------------------------------
--
------------------------------------------------------------------------------

absFromChunk :: MonadThrow m => Array Word8 -> m (Abs PosixPath)
absFromChunk arr = do
    if Common.isRelative Posix arr
    -- XXX Add more detailed error msg with all valid examples.
    then throwM $ InvalidPath "Path must be absolute or located"
    else pure $ Abs (PosixPath arr)

absFromString :: MonadThrow m => String -> m (Abs PosixPath)
absFromString s = do
    PosixPath arr <- Posix.fromString s
    absFromChunk arr

relFromChunk :: MonadThrow m => Array Word8 -> m (Rel PosixPath)
relFromChunk arr = do
    if Common.isAbsolute Posix arr
    -- XXX Add more detailed error msg with all valid examples.
    then throwM $ InvalidPath "Path must not be absolute or located"
    else pure $ Rel (PosixPath arr)

relFromString :: MonadThrow m => String -> m (Rel PosixPath)
relFromString s = do
    PosixPath arr <- Posix.fromString s
    relFromChunk arr

------------------------------------------------------------------------------
-- Statically Verified Strings
------------------------------------------------------------------------------

liftRel :: Quote m => Rel PosixPath -> m Exp
liftRel p =
    [| Rel (PosixPath (Common.unsafePosixFromString $(lift $ Posix.toString p))) |]

liftAbs :: Quote m => Abs PosixPath -> m Exp
liftAbs p =
    [| Abs (PosixPath (Common.unsafePosixFromString $(lift $ Posix.toString p))) |]

-- | Generates an @Abs Path@ type.
--
mkAbs :: String -> Q Exp
mkAbs = either (error . show) liftAbs . absFromString

-- | Generates an @Rel Path@ type.
--
mkRel :: String -> Q Exp
mkRel = either (error . show) liftRel . relFromString

------------------------------------------------------------------------------
-- Statically Verified Literals
------------------------------------------------------------------------------

-- XXX Define folds or parsers to parse the paths.
-- XXX Build these on top of the str quasiquoter so that we get interpolation
-- for free. Interpolated vars if any have to be of appropriate type depending
-- on the context so that we can splice them safely.

-- XXX Change to "loc"?

-- | Generates an @Abs PosixPath@ type from a quoted literal.
--
-- >>> Path.toString ([abs|/usr|] :: Abs PosixPath)
-- "/usr"
--
abs :: QuasiQuoter
abs = mkQ mkAbs

-- | Generates a @Rel PosixPath@ type from a quoted literal.
--
-- >>> Path.toString ([rel|usr|] :: Rel PosixPath)
-- "usr"
--
rel :: QuasiQuoter
rel = mkQ mkRel

-- The only safety we need for paths is: (1) The first path can only be a Dir
-- type path, and (2) second path can only be a Rel path.

-- | Use this API to combine paths when the first path is @Abs@ or @Rel@.
-- Second path must be @Rel@.
--
-- If the first path is absolute then the return type is also absolute.
--
-- >>> Path.toString (Path.append [abs|/usr|] [rel|bin|] :: Abs PosixPath)
-- "/usr/bin"
-- >>> Path.toString (Path.append [rel|usr|] [rel|bin|] :: Rel PosixPath)
-- "usr/bin"
--
-- Type error cases:
--
-- >> Path.append [abs|/usr|] [abs|/bin|] -- second arg must be rel
--
{-# INLINE append #-}
append ::
    (
      IsAbsRel (a PosixPath)
    , IsPath PosixPath (a PosixPath)
    ) => a PosixPath -> Rel PosixPath -> a PosixPath
append a (Rel c) = unsafeFromPath $ Posix.unsafeAppend (toPath a) (toPath c)
