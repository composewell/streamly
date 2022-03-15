module Streamly.Test.Data.Fold.Window (main) where

import Streamly.Internal.Data.Fold.Window
import Test.Hspec (hspec, describe, it, runIO)
import qualified Streamly.Internal.Data.Ring.Foreign as Ring
import qualified Streamly.Prelude as S

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
                        $ S.map (, Nothing) c
                let c1 = a - b
                it ("should not deviate more than " ++ show deviationLimit)
                    $ c1 >= -1 * deviationLimit && c1 <= deviationLimit

        describe "Sum" $ testFunc sum

    describe "Correctness" $ do
        let winSize = 3
            testCase1 = [31, 41, 59, 26, 53, 58, 97] :: [Double]
            testCase2 = replicate 5 1.0 ++ [7.0]

            testFunc tc f sI sW = do
                let c = S.fromList tc
                a <- runIO $ S.toList $ S.postscan f $ S.map (, Nothing) c
                b <- runIO $ S.toList $ S.postscan
                        (Ring.slidingWindow winSize f) c
                it "Infinite" $ a  == sI
                it ("Finite " ++ show winSize) $ b == sW

        describe "minimum" $ do
            let scanInf = [31, 31, 31, 26, 26, 26, 26] :: [Double]
                scanWin = [31, 31, 31, 26, 26, 26, 53] :: [Double]
            testFunc testCase1 minimum scanInf scanWin
        describe "maximum" $ do
            let scanInf = [31, 41, 59, 59, 59, 59, 97] :: [Double]
                scanWin = [31, 41, 59, 59, 59, 58, 97] :: [Double]
            testFunc testCase1 maximum scanInf scanWin
        describe "range" $ do
            let scanInf = [0, 10, 28, 33, 33, 33, 71] :: [Double]
                scanWin = [0, 10, 28, 33, 33, 32, 44] :: [Double]
            testFunc testCase1 range scanInf scanWin
        describe "sum" $ do
            let scanInf = [1, 2, 3, 4, 5, 12] :: [Double]
                scanWin = [1, 2, 3, 3, 3, 9] :: [Double]
            testFunc testCase2 sum scanInf scanWin
