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
-- We are describing below many useful types that can be created using macros
-- in this module and the behavior of those types. These could be useful if you
-- like to program using the monad \"do notation\" instead of using concatMap
-- like operations.
--
-- == Parallel
--
-- A newtype wrapper over the 'Stream' type; the Applicative and Monad
-- instances generate a cross product of the two streams in a concurrent
-- manner. The order in which the stream elements are produced is not
-- deterministic, this is supposed to be used if order does not matter.

-- Loops over the outer stream, generating multiple elements concurrently; for
-- each outer stream element, loop over the inner stream concurrently. More
-- outer iterations are started only if the existing inner iterations are not
-- saturating the resources.
--
-- Use 'mkParallel' to construct from 'Stream' type and 'unParallel' to
-- deconstruct back to 'Stream'.
--
-- >>> :{
--  bind :: MonadAsync m => Stream m a -> (a -> Stream m b) -> Stream m b
--  bind = flip (Stream.parConcatMap id)
--  $(mkCrossType "Parallel" "bind" True)
-- :}
--
-- This is a bounded concurrent, unordered list-transformer (ListT) monad.
--
-- WARNING! By design, monad bind of this type is not associative, because of
-- concurrency, order of effects as well as results is non-deterministic.
--
-- Serves the same purpose as the 'Streamly.Prelude.AsyncT' type in older
-- releases.
--
-- == FairParallel
--
-- Like Parallel but strikes a balance between going deeper into existing
-- iterations of the loop and starting new outer loop iterations.
--
-- Use 'mkFairParallel' to construct from 'Stream' type and 'unFairParallel' to
-- deconstruct back to 'Stream'.
--
-- >>> :{
--  bind :: MonadAsync m => Stream m a -> (a -> Stream m b) -> Stream m b
--  bind = flip (Stream.parConcatMap (Stream.interleaved True))
--  $(mkCrossType "FairParallel" "bind" True)
-- :}
--
-- This is a bounded concurrent, fair logic programming (LogicT) monad.
--
-- WARNING! By design, monad bind of this type is not associative, because of
-- concurrency, order of effects as well as results may be unpredictable.
--
-- Serves the same purpose as the 'Streamly.Prelude.WAsyncT' type in older
-- releases.
--
-- == EagerParallel
--
-- Like Parallel, but executes as many actions concurrently as possible. This
-- is useful if you want all actions to be scheduled at the same time so that
-- something does not get starved due to others.
--
-- Use 'mkEagerParallel' to construct from 'Stream' type and 'unEagerParallel'
-- to deconstruct back to 'Stream'.
--
-- >>> :{
--  bind :: MonadAsync m => Stream m a -> (a -> Stream m b) -> Stream m b
--  parBind = flip (Stream.parConcatMap (Stream.eager True))
--  $(mkCrossType "EagerParallel" "parBind" True)
-- :}
--
-- This is an unbounded concurrent, unordered list transformer (ListT) monad.
--
-- WARNING! By design, monad bind of this type is not associative, because of
-- concurrency order of effects as well as results may be unpredictable.
--
-- Serves the same purpose as the 'Streamly.Prelude.ParallelT' type in older
-- releases.
--
-- == OrderedParallel
--
-- Like Parallel, runs many iterations concurrently, but stages the results
-- such that the results of iterations are presented in the same order as
-- specified in the code. This is closest to the serial Nested type in behavior
-- among all the concurrent types.
--
-- Use 'mkOrderedParallel' to construct from 'Stream' type and
-- 'unOrderedParallel' to deconstruct back to 'Stream'.
--
-- >>> :{
--  bind :: MonadAsync m => Stream m a -> (a -> Stream m b) -> Stream m b
--  bind = flip (Stream.parConcatMap (Stream.ordered True))
--  $(mkCrossType "OrderedParallel" "bind" True)
-- :}
--
-- This is a bounded concurrent, ordered list transformer (ListT) monad.
--
-- WARNING! Monad bind of this type is associative for values, but because of
-- concurrency, order of effects may be unpredictable.
--
-- Serves the same purpose as the 'Streamly.Prelude.AheadT' type in older
-- releases.
--
-- == Zip
--
-- A newtype wrapper over the 'Stream' type, the applicative instance zips two
-- streams.
--
-- Use 'mkZip' to construct from 'Stream' type and 'unZip' to deconstruct back
-- to 'Stream'.
--
-- >>> :{
--  zipApply :: Monad m => Stream m (a -> b) -> Stream m a -> Stream m b
--  zipApply = Stream.zipWith ($)
--  $(mkZipType "Zip" "zipApply" False)
-- :}
--
-- Same as the deprcated 'Streamly.Prelude.ZipSerialM' type.
--
-- == ZipParallel
--
-- Like Zip but evaluates the streams being zipped concurrently.
--
-- Use 'mkZipParallel' to construct from 'Stream' type and 'unZipParallel' to
-- deconstruct back to 'Stream'.
--
-- >>> :{
--  parZipApply :: MonadAsync m => Stream m (a -> b) -> Stream m a -> Stream m b
--  parZipApply = Stream.parZipWith id id
--  $(mkZipType "ZipParallel" "parZipApply" True)
-- :}
--
-- Same as the deprecated 'Streamly.Prelude.ZipAsync' type.
--
-- == Avoiding Template Haskell
--
-- Instead of using these macros directly you could copy and paste the
-- generated code as well. Use these macros in ghci to generate the required
-- code and paste it in your package, you can customize the code as desired.
-- See the docs of the macros below for examples about how to view the
-- generated code. For example:
--
-- >>> bind = flip (Stream.parConcatMap id)
-- >>> expr <- runQ (mkCrossType "AsyncT" "bind" True)
--
-- >> putStrLn $ pprint expr

-- XXX TBD
--
-- == Nested
--
-- A serial cross monad type, same as 'Streamly.Data.StreamK.Nested' but with
-- mtl instances.
--
-- == FairNested
--
-- Like Nested but strikes a balance between going deeper into existing
-- iterations of the loop and starting new iterations. Same as
-- 'Streamly.Data.StreamK.FairNested' but additional with mtl instances.
--
-- == InvertedParallel
--
-- gives more priority to opening new iterations than to run existing
-- iterations. The interleaved options should be diagonal, and interleaved can
-- be for giving priority to iterations.

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
