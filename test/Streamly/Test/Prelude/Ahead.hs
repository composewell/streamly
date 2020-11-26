-- |
-- Module      : Streamly.Test.Prelude.Ahead
-- Copyright   : (c) 2020 Composewell Technologies
--
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

module Streamly.Test.Prelude.Ahead where

#if __GLASGOW_HASKELL__ < 808
import Data.Semigroup ((<>))
#endif
import Test.QuickCheck (Property)
import Test.Hspec.QuickCheck
import Test.QuickCheck.Monadic (monadicIO, run)
import Test.Hspec as H

import Streamly.Prelude (IsStream, SerialT, AheadT)
import qualified Streamly.Prelude as S

import Streamly.Test.Common
import Streamly.Test.Prelude.Common

associativityCheck
    :: String
    -> (AheadT IO Int -> SerialT IO Int)
    -> Spec
associativityCheck desc t = prop desc assocCheckProp
  where
    assocCheckProp :: [Int] -> [Int] -> [Int] -> Property
    assocCheckProp xs ys zs =
        monadicIO $ do
            let xStream = S.fromList xs
                yStream = S.fromList ys
                zStream = S.fromList zs
            infixAssocstream <-
                run $ S.toList $ t $ xStream `S.ahead` yStream `S.ahead` zStream
            assocStream <- run $ S.toList $ t $ xStream <> yStream <> zStream
            listEquals (==) infixAssocstream assocStream

main :: IO ()
main = hspec
    $ H.parallel
#ifdef COVERAGE_BUILD
    $ modifyMaxSuccess (const 10)
#endif
    $ do
    let aheadOps :: IsStream t => ((AheadT IO a -> t IO a) -> Spec) -> Spec
        aheadOps spec = mapOps spec $ makeOps S.aheadly
#ifndef COVERAGE_BUILD
              <> [("maxBuffer (-1)", S.aheadly . S.maxBuffer (-1))]
#endif

    describe "Construction" $ do
        aheadOps    $ prop "aheadly replicateM" . constructWithReplicateM
        aheadOps $ prop "aheadly cons" . constructWithCons S.cons
        aheadOps $ prop "aheadly consM" . constructWithConsM S.consM id
        aheadOps $ prop "aheadly (.:)" . constructWithCons (S..:)
        aheadOps $ prop "aheadly (|:)" . constructWithConsM (S.|:) id

    describe "Functor operations" $ do
        aheadOps     $ functorOps S.fromFoldable "aheadly" (==)
        aheadOps     $ functorOps folded "aheadly folded" (==)

    describe "Monoid operations" $ do
        aheadOps     $ monoidOps "aheadly" mempty (==)

    describe "Ahead loops" $ loops S.aheadly id reverse

    describe "Bind and Monoidal composition combinations" $ do
        aheadOps $ bindAndComposeSimpleOps "Ahead" sortEq
        aheadOps $ bindAndComposeHierarchyOps "Ahead"
        aheadOps $ nestTwoStreams "Ahead" id id
        aheadOps $ nestTwoStreamsApp "Ahead" id id
        composeAndComposeSimpleSerially "Ahead <> " (repeat [1..9]) S.aheadly
        composeAndComposeSimpleAheadly "Ahead <> " (repeat [1 .. 9]) S.aheadly
        composeAndComposeSimpleWSerially
            "Ahead <> "
            [[1..9], [1..9], [1,3,2,4,6,5,7,9,8], [1,3,2,4,6,5,7,9,8]]
            S.aheadly

    describe "Semigroup operations" $ do
        aheadOps $ semigroupOps "aheadly" (==)
        aheadOps $ associativityCheck "ahead == <>"

    describe "Applicative operations" $ do
        aheadOps $ applicativeOps S.fromFoldable "aheadly applicative" (==)
        aheadOps $ applicativeOps folded "aheadly applicative folded" (==)

    -- XXX add tests for indexed/indexedR
    describe "Zip operations" $ do
        -- We test only the serial zip with serial streams and the parallel
        -- stream, because the rate setting in these streams can slow down
        -- zipAsync.
        aheadOps    $ prop "zip monadic aheadly" . zipAsyncMonadic S.fromFoldable (==)
        aheadOps    $ prop "zip monadic aheadly folded" . zipAsyncMonadic folded (==)

    -- XXX add merge tests like zip tests
    -- for mergeBy, we can split a list randomly into two lists and
    -- then merge them, it should result in original list
    -- describe "Merge operations" $ do

    describe "Monad operations" $ do
        aheadOps    $ prop "aheadly monad then" . monadThen S.fromFoldable (==)
        aheadOps    $ prop "aheadly monad then folded" . monadThen folded (==)
        aheadOps    $ prop "aheadly monad bind" . monadBind S.fromFoldable (==)
        aheadOps    $ prop "aheadly monad bind folded"   . monadBind folded (==)

    describe "Stream transform and combine operations" $ do
        aheadOps     $ transformCombineOpsCommon S.fromFoldable "aheadly" (==)
        aheadOps     $ transformCombineOpsCommon folded "aheadly" (==)
        aheadOps     $ transformCombineOpsOrdered S.fromFoldable "aheadly" (==)
        aheadOps     $ transformCombineOpsOrdered folded "aheadly" (==)

    describe "Stream elimination operations" $ do
        aheadOps     $ eliminationOps S.fromFoldable "aheadly"
        aheadOps     $ eliminationOps folded "aheadly folded"
        aheadOps     $ eliminationOpsWord8 S.fromFoldable "aheadly"
        aheadOps     $ eliminationOpsWord8 folded "aheadly folded"

    -- XXX Add a test where we chain all transformation APIs and make sure that
    -- the state is being passed through all of them.
    describe "Stream serial elimination operations" $ do
        aheadOps     $ eliminationOpsOrdered S.fromFoldable "aheadly"
        aheadOps     $ eliminationOpsOrdered folded "aheadly folded"

    describe "Tests for exceptions" $ aheadOps $ exceptionOps "aheadly"
    describe "Composed MonadThrow aheadly" $ composeWithMonadThrow S.aheadly

    -- Ad-hoc tests
    it "takes n from stream of streams" $ takeCombined 1 S.aheadly
