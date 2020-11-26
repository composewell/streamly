-- |
-- Module      : Streamly.Test.Prelude.WSerial
-- Copyright   : (c) 2020 Composewell Technologies
--
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

module Streamly.Test.Prelude.WSerial where

import Data.List (sort)
#if __GLASGOW_HASKELL__ < 808
import Data.Semigroup ((<>))
#endif
import Test.QuickCheck (Property)
import Test.Hspec.QuickCheck
import Test.QuickCheck.Monadic (monadicIO, run)
import Test.Hspec as H

import Streamly.Prelude hiding (repeat)
import qualified Streamly.Prelude as S

import Streamly.Test.Common
import Streamly.Test.Prelude.Common

associativityCheck
    :: String
    -> (WSerialT IO Int -> SerialT IO Int)
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
                run $ S.toList $ t $ xStream `wSerial` yStream `wSerial` zStream
            assocStream <- run $ S.toList $ t $ xStream <> yStream <> zStream
            listEquals (==) infixAssocstream assocStream

interleaveCheck :: IsStream t
    => (t IO Int -> SerialT IO Int)
    -> (t IO Int -> t IO Int -> t IO Int)
    -> Spec
interleaveCheck t f =
    it "Interleave four" $
        (S.toList . t) ((singleton 0 `f` singleton 1) `f` (singleton 100 `f` singleton 101))
            `shouldReturn` [0, 100, 1, 101]

    where

    singleton :: IsStream t => a -> t m a
    singleton a = a .: nil

main :: IO ()
main = hspec
    $ H.parallel
#ifdef COVERAGE_BUILD
    $ modifyMaxSuccess (const 10)
#endif
    $ do
    let wSerialOps :: IsStream t => ((WSerialT IO a -> t IO a) -> Spec) -> Spec
        wSerialOps spec = mapOps spec $ makeOps wSerially
#ifndef COVERAGE_BUILD
            <> [("rate AvgRate 0.00000001", wSerially . avgRate 0.00000001)]
            <> [("maxBuffer (-1)", wSerially . maxBuffer (-1))]
#endif

    describe "Construction" $ do
        wSerialOps  $ prop "wSerially replicateM" . constructWithReplicateM
        wSerialOps $ prop "wSerially cons" . constructWithCons S.cons
        wSerialOps $ prop "wSerially consM" . constructWithConsM S.consM id
        wSerialOps $ prop "wSerially (.:)" . constructWithCons (S..:)
        wSerialOps $ prop "wSerially (|:)" . constructWithConsM (S.|:) id

    describe "Functor operations" $ do
        wSerialOps   $ functorOps S.fromFoldable "wSerially" (==)
        wSerialOps   $ functorOps folded "wSerially folded" (==)

    describe "Monoid operations" $ do
        wSerialOps   $ monoidOps "wSerially" mempty sortEq

    describe "Bind and Monoidal composition combinations" $ do
        -- XXX Taking a long time when wSerialOps is used.
        bindAndComposeSimpleOps "WSerial" sortEq wSerially
        bindAndComposeHierarchyOps "WSerial" wSerially
        wSerialOps $ nestTwoStreams "WSerial" id sort
        wSerialOps $ nestTwoStreamsApp "WSerial" id sort
        composeAndComposeSimpleSerially
            "WSerial <> "
            [ [1, 4, 2, 7, 3, 5, 8, 6, 9]
            , [1, 7, 4, 8, 2, 9, 5, 3, 6]
            , [1, 4, 2, 7, 3, 5, 8, 6, 9]
            , [1, 7, 4, 8, 2, 9, 5, 3, 6]
            ]
            wSerially
        composeAndComposeSimpleWSerially
            "WSerial <> "
            [ [1, 4, 2, 7, 3, 5, 8, 6, 9]
            , [1, 7, 4, 8, 2, 9, 5, 3, 6]
            , [1, 4, 3, 7, 2, 6, 9, 5, 8]
            , [1, 7, 4, 9, 3, 8, 6, 2, 5]
            ]
            wSerially

    describe "Semigroup operations" $ do
        wSerialOps $ semigroupOps "wSerially" (==)
        wSerialOps $ associativityCheck "wSerial == <>"

    describe "Applicative operations" $ do
        wSerialOps $ applicativeOps S.fromFoldable "wSerially applicative" sortEq
        wSerialOps $ applicativeOps folded "wSerially applicative folded" sortEq

    -- XXX add tests for indexed/indexedR
    describe "Zip operations" $ do
        -- We test only the serial zip with serial streams and the parallel
        -- stream, because the rate setting in these streams can slow down
        -- zipAsync.
        wSerialOps  $ prop "zip monadic wSerially" . zipMonadic S.fromFoldable (==)
        wSerialOps  $ prop "zip monadic wSerially folded" . zipMonadic folded (==)

    describe "Monad operations" $ do
        wSerialOps  $ prop "wSerially monad then" . monadThen S.fromFoldable sortEq
        wSerialOps  $ prop "wSerially monad then folded" . monadThen folded sortEq
        wSerialOps  $ prop "wSerially monad bind" . monadBind S.fromFoldable sortEq
        wSerialOps  $ prop "wSerially monad bind folded" . monadBind folded sortEq

    describe "Stream transform and combine operations" $ do
        wSerialOps   $ transformCombineOpsCommon S.fromFoldable "wSerially" sortEq
        wSerialOps   $ transformCombineOpsCommon folded "wSerially" sortEq

    describe "Stream elimination operations" $ do
        wSerialOps   $ eliminationOps S.fromFoldable "wSerially"
        wSerialOps   $ eliminationOps folded "wSerially folded"
        wSerialOps   $ eliminationOpsWord8 S.fromFoldable "wSerially"
        wSerialOps   $ eliminationOpsWord8 folded "wSerially folded"

    -- XXX Add a test where we chain all transformation APIs and make sure that
    -- the state is being passed through all of them.
    describe "Stream serial elimination operations" $ do
        wSerialOps   $ eliminationOpsOrdered S.fromFoldable "wSerially"
        wSerialOps   $ eliminationOpsOrdered folded "wSerially folded"

    ---------------------------------------------------------------------------
    -- Semigroup/Monoidal Composition strict ordering checks
    ---------------------------------------------------------------------------

    describe "WSerial interleaved (<>) ordering check" $
        interleaveCheck wSerially (<>)
    describe "WSerial interleaved mappend ordering check" $
        interleaveCheck wSerially mappend

    describe "Tests for exceptions" $ wSerialOps $ exceptionOps "wSerially"
    describe "Composed MonadThrow wSerially" $ composeWithMonadThrow wSerially
