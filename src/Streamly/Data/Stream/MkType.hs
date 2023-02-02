-- |
-- Module      : Streamly.Data.Stream.MkType
-- Copyright   : (c) 2022 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : released
-- Portability : GHC
--
-- XXX Need to fix.
--
-- Template Haskell macros to create custom newtype wrappers for the 'Stream'
-- type, deriving all the usual instances.
--
-- To use this module, the following extensions must be enabled:
--
-- >>> :set -XStandaloneDeriving
-- >>> :set -XTemplateHaskell
-- >>> :set -XTypeFamilies
-- >>> :set -XUndecidableInstances
--
-- Import this module unqualified to bring everything needed in scope without
-- having to import several other modules. Also, "Streamly.Data.Stream" or
-- "Streamly.Data.Stream.Prelude" must be imported @as Stream@.
--
-- >>> import Streamly.Data.Stream.MkType
-- >>> import qualified Streamly.Data.Stream.Prelude as Stream
--
-- Example, create an applicative type with zipping apply:
--
-- >> :{
--  zipApply = Stream.zipWith ($)
--  $(mkZipType "ZipStream" "zipApply" False)
-- :}
--
-- Example, create an applicative type with concurrent zipping apply:
--
-- >> :{
--  parApply = Stream.parApply id
--  $(mkZipType "ParZipStream" "parApply" True)
-- :}
--
-- Example, create a monad type with an interleaving cross product bind:
--
-- >> :{
--  interleaveBind = flip (Stream.concatMapWith Stream.interleave)
--  $(mkCrossType "InterleaveStream" "interleaveBind" False)
-- :}
--
-- Example, create a monad type with an eager concurrent cross product bind:
--
-- >> :{
--  parBind = flip (Stream.parConcatMap (Stream.eager True))
--  $(mkCrossType "ParEagerStream" "parBind" True)
-- :}
--
-- Instead of using these macros directly you could use the generated code as
-- well. Use these macros in ghci to generate the required code and paste it in
-- your package, you can customize the code as desired. See the docs of the
-- macros below for examples about how to view the generated code.
--
module Streamly.Data.Stream.MkType
    (
    -- * Imports for Examples
    -- $setup

    -- * Template Haskell Macros
      mkZipType
    , mkCrossType

    -- * Re-exports
    , Read(..)
    , MonadIO(..)
    , MonadThrow(..)
    , MonadReader(..)
    , MonadTrans(..)
    , Identity
    , IsList
    , IsString
    , ap
    )
where

import Data.Functor.Identity (Identity)
import GHC.Exts (IsList, IsString)
import Text.Read (Read(..))

import Streamly.Internal.Data.Stream.MkType

-- $setup
-- >>> :m
-- >>> import Language.Haskell.TH
-- >>> import qualified Streamly.Data.Stream.Prelude as Stream
-- >>> import Streamly.Data.Stream.MkType
