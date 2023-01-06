-- |
-- Module      : Streamly.Data.Stream.MkType
-- Copyright   : (c) 2022 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : released
-- Portability : GHC
--
-- Template Haskell macros to create custom newtype wrappers for the 'Stream'
-- type, deriving all the usual instances.
--
-- To use this module, "Streamly.Data.Stream" or "Streamly.Data.Stream.Prelude"
-- must be imported as "Stream". Also, it is recommended to import this module
-- unqualified to bring everything needed in scope without having to import
-- several other modules.
--
-- >>> import qualified Streamly.Data.Stream.Prelude as Stream
-- >>> import Streamly.Internal.Data.Stream.MkType
--
-- Instead of using these macros directly you could use the generated code as
-- well. Use these macros in ghci to generate the required code and paste it in
-- your package, you can customize it as you want. See the docs of the macros
-- below for examples of viewing the generated code.
--
-- Example, create an applicative type with zipping apply:
--
-- >>> import Streamly.Data.Stream.Prelude (Stream)
-- >>> :{
-- zipApply :: Monad m => Stream m (a -> b) -> Stream m a -> Stream m b
-- zipApply = Stream.zipWith ($)
-- :}
--
-- > $(mkZipType "ZipStream" "zipApply" False)
--
-- Example, create an applicative type with concurrent zipping apply:
--
-- > $(mkZipType "ParZipStream" "parApply" True)
--
-- Example, create a monad type with an interleaving cross product bind:
--
-- >>> :{
-- interleaveBind :: Stream m a -> (a -> Stream m b) -> Stream m b
-- interleaveBind = flip (Stream.concatMapWith Stream.interleave)
-- :}
--
-- > $(mkCrossType "InterleaveStream" "interleaveBind" False)
--
-- Example, create a monad type with an eager concurrent cross product bind:
--
-- >>> :{
-- parBind :: Stream.MonadAsync m => Stream m a -> (a -> Stream m b) -> Stream m b
-- parBind = flip (Stream.parConcatMap (Stream.eager True))
-- :}
--
-- > $(mkCrossType "ParEagerStream" "parBind" True)
--
module Streamly.Data.Stream.MkType
    (
    -- * Imports for Examples
    -- $setup

    -- * Template Haskell Macros
      mkZipType
    , mkCrossType

    -- * Re-exports
    , MonadIO(..)
    , MonadThrow(..)
    , MonadReader(..)
    , MonadTrans(..)
    , ap
    )
where

import Streamly.Internal.Data.Stream.MkType

-- $setup
-- >>> :m
-- >>> import Language.Haskell.TH
-- >>> import qualified Streamly.Data.Stream.Prelude as Stream
-- >>> import Streamly.Internal.Data.Stream.MkType
