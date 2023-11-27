-- |
-- Module      : Streamly.Data.Stream.MkType
-- Copyright   : (c) 2022 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : released
-- Portability : GHC
--
-- Template Haskell macros to create custom newtype wrappers for the 'Stream'
-- type. See the examples below to create the standard stream types that were
-- available in streamly versions before 0.9.0.
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
-- For 'Streamly.Prelude.AsyncT' monad type with a concurrent cross product
-- bind:
--
-- >>> :{
--  bind = flip (Stream.parConcatMap id)
--  $(mkCrossType "AsyncT" "bind" True)
-- :}
--
-- For 'Streamly.Prelude.WAsyncT' monad type with a concurrent interleaved
-- bind:
--
-- >>> :{
--  bind = flip (Stream.parConcatMap (Stream.interleaved True))
--  $(mkCrossType "WAsyncT" "bind" True)
-- :}
--
-- For 'Streamly.Prelude.AheadT' monad type with a concurrent ordered
-- cross product bind:
--
-- >>> :{
--  bind = flip (Stream.parConcatMap (Stream.ordered True))
--  $(mkCrossType "AheadT" "bind" True)
-- :}
--
-- For 'Streamly.Prelude.ParallelT' monad type with an eager concurrent cross
-- product bind:
--
-- >>> :{
--  parBind = flip (Stream.parConcatMap (Stream.eager True))
--  $(mkCrossType "ParallelT" "parBind" True)
-- :}
--
-- For 'Streamly.Prelude.ZipSerialM' serial zipping applicative type:
--
-- >>> :{
--  zipApply = Stream.zipWith ($)
--  $(mkZipType "ZipSerialM" "zipApply" False)
-- :}
--
-- For 'Streamly.Prelude.ZipAsync' concurrent zipping applicative type:
--
-- >>> :{
--  parApply = Stream.parApply id
--  $(mkZipType "ZipAsync" "parApply" True)
-- :}
--
-- Instead of using these macros directly you could use the generated code as
-- well. Use these macros in ghci to generate the required code and paste it in
-- your package, you can customize the code as desired. See the docs of the
-- macros below for examples about how to view the generated code. For example:
--
-- >>> bind = flip (Stream.parConcatMap id)
-- >>> expr <- runQ (mkCrossType "AsyncT" "bind" True)
--
-- >> putStrLn $ pprint expr
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
