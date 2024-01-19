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
    -- * Types
      Loc (..)
    , Seg (..)
    , IsLocSeg
    , IsPath (..)

    -- * Construction
    , locFromString
    , segFromString

    -- * Statically Verified String Literals
    -- quasiquoters
    , loc
    , seg

    -- * Statically Verified Strings
    -- XXX Do we need these if we have quasiquoters? These may be useful if we
    -- are generating strings statically using methods other than literals or
    -- if we are doing some text processing on strings before using them.
    -- TH macros
    , mkLoc
    , mkSeg

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

import Language.Haskell.TH hiding (Loc)
import Language.Haskell.TH.Quote
import Streamly.Internal.Data.Path

{- $setup
>>> :m
>>> :set -XQuasiQuotes

For APIs that have not been released yet.

>>> import qualified Streamly.Internal.FileSystem.Path.Posix as Path
-}

newtype Loc a = Loc a
newtype Seg a = Seg a

instance IsPath PosixPath (Loc PosixPath) where
    unsafeFromPath = Loc
    fromPath p = pure (Loc p)
    toPath (Loc p) = p

instance IsPath PosixPath (Seg PosixPath) where
    unsafeFromPath = Seg
    fromPath p = pure (Seg p)
    toPath (Seg p) = p

-- | Constraint to check if a type has Loc or Seg annotations.
class IsLocSeg a

instance IsLocSeg (Loc a)
instance IsLocSeg (Seg a)

------------------------------------------------------------------------------
--
------------------------------------------------------------------------------

locFromChunk :: MonadThrow m => Array Word8 -> m (Loc PosixPath)
locFromChunk arr = do
    if Common.isLocation Posix arr
    then pure $ Loc (PosixPath arr)
    -- XXX Add more detailed error msg with all valid examples.
    else throwM $ InvalidPath "Must be a specific location, not a path segment"

locFromString :: MonadThrow m => String -> m (Loc PosixPath)
locFromString s = do
    PosixPath arr <- Posix.fromString s
    locFromChunk arr

segFromChunk :: MonadThrow m => Array Word8 -> m (Seg PosixPath)
segFromChunk arr = do
    if Common.isSegment Posix arr
    then pure $ Seg (PosixPath arr)
    -- XXX Add more detailed error msg with all valid examples.
    else throwM $ InvalidPath "Must be a path segment, not a specific location"

segFromString :: MonadThrow m => String -> m (Seg PosixPath)
segFromString s = do
    PosixPath arr <- Posix.fromString s
    segFromChunk arr

------------------------------------------------------------------------------
-- Statically Verified Strings
------------------------------------------------------------------------------

liftLoc :: Quote m => Loc PosixPath -> m Exp
liftLoc p =
    [| Loc (PosixPath (Common.unsafePosixFromString $(lift $ Posix.toString p))) |]

liftSeg :: Quote m => Seg PosixPath -> m Exp
liftSeg p =
    [| Seg (PosixPath (Common.unsafePosixFromString $(lift $ Posix.toString p))) |]

-- | Generates an @Loc Path@ type.
--
mkLoc :: String -> Q Exp
mkLoc = either (error . show) liftLoc . locFromString

-- | Generates an @Seg Path@ type.
--
mkSeg :: String -> Q Exp
mkSeg = either (error . show) liftSeg . segFromString

------------------------------------------------------------------------------
-- Statically Verified Literals
------------------------------------------------------------------------------

-- XXX Define folds or parsers to parse the paths.
-- XXX Build these on top of the str quasiquoter so that we get interpolation
-- for free. Interpolated vars if any have to be of appropriate type depending
-- on the context so that we can splice them safely.

-- | Generates an @Loc PosixPath@ type from a quoted literal.
--
-- >>> Path.toString ([loc|/usr|] :: Loc PosixPath)
-- "/usr"
--
loc :: QuasiQuoter
loc = mkQ mkLoc

-- | Generates a @Seg PosixPath@ type from a quoted literal.
--
-- >>> Path.toString ([seg|usr|] :: Seg PosixPath)
-- "usr"
--
seg :: QuasiQuoter
seg = mkQ mkSeg

-- The only safety we need for paths is: (1) The first path can only be a Dir
-- type path, and (2) second path can only be a Seg path.

-- | Append a path segment to a segment or location.
--
-- >>> Path.toString (Path.append [loc|/usr|] [seg|bin|] :: Loc PosixPath)
-- "/usr/bin"
-- >>> Path.toString (Path.append [seg|usr|] [seg|bin|] :: Seg PosixPath)
-- "usr/bin"
--
{-# INLINE append #-}
append ::
    (
      IsLocSeg (a PosixPath)
    , IsPath PosixPath (a PosixPath)
    ) => a PosixPath -> Seg PosixPath -> a PosixPath
append a (Seg c) = unsafeFromPath $ Posix.unsafeAppend (toPath a) (toPath c)
