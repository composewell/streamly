module Streamly.Test.Data.Fold.Window (main) where

import Test.Hspec (hspec, describe, it, runIO)
import qualified Streamly.Internal.Data.Ring as Ring
import qualified Streamly.Internal.Data.Stream as S
import qualified Streamly.Internal.Data.Fold as Fold

import Prelude hiding (sum, maximum, minimum)

moduleName :: String
moduleName = "Window"

main :: IO ()
main = hspec $ do
    describe moduleName $ do
        let numElem = 80000
            winSize = 800
            testCaseChunk = [9007199254740992, 1, 1.0 :: Double,
                                9007199254740992, 1, 1, 1, 9007199254740992]
            testCase = take numElem $ cycle testCaseChunk
            deviationLimit = 1
            testFunc f = do
                let c = S.fromList testCase
                a <- runIO $ S.fold (Ring.slidingWindow winSize f) c
                b <- runIO $ S.fold f $ S.drop (numElem - winSize)
                        $ fmap (, Nothing) c
                let c1 = a - b
                it ("should not deviate more than " ++ show deviationLimit)
                    $ c1 >= -1 * deviationLimit && c1 <= deviationLimit

        describe "Sum" $ testFunc Fold.windowSum
        describe "mean" $ testFunc Fold.windowMean

    describe "Correctness" $ do
        let winSize = 3
            testCase1 =
                [1.0, 4.0, 3.0, 2.1, -5.1, -2.0, 7.0, 3.0, -2.5] :: [Double]

            testCase2 = replicate 5 1.0 ++ [7.0]

            testFunc tc f sI sW = do
                let c = S.fromList tc
                a <- runIO $ S.fold Fold.toList $ S.postscan f $ fmap (, Nothing) c
                b <- runIO $ S.fold Fold.toList $ S.postscan
                        (Ring.slidingWindow winSize f) c
                it "Infinite" $ a  == sI
                it ("Finite " ++ show winSize) $ b == sW

            testFunc2 tc expec f = do
                let c = S.fromList tc
                a <- runIO $ S.fold (f winSize) c
                it (show tc) $ a == expec

        describe "minimum" $ do
            testFunc2 testCase1 (Just (-2.5)) Fold.windowMinimum
        describe "maximum" $ do
            testFunc2 testCase1 (Just 7.0) Fold.windowMaximum
        describe "range" $ do
            testFunc2 testCase1 (Just (-2.5, 7.0)) Fold.windowRange
        describe "sum" $ do
            let scanInf = [1, 2, 3, 4, 5, 12] :: [Double]
                scanWin = [1, 2, 3, 3, 3, 9] :: [Double]
            testFunc testCase2 Fold.windowSum scanInf scanWin
        describe "mean" $ do
            let scanInf = [1, 1, 1, 1, 1, 2] :: [Double]
                scanWin = [1, 1, 1, 1, 1, 3] :: [Double]
            testFunc testCase2 Fold.windowMean scanInf scanWin
