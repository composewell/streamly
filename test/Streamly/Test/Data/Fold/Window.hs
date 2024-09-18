module Streamly.Test.Data.Fold.Window (main) where

import Test.Hspec (hspec, describe, it, runIO)
import Streamly.Internal.Data.Scanl (Incr(..))
import qualified Streamly.Internal.Data.Fold as Fold
import qualified Streamly.Internal.Data.Ring as Ring
import qualified Streamly.Internal.Data.Scanl as Scanl
import qualified Streamly.Internal.Data.Stream as S

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
                    f1 = Fold.fromScanl $ Scanl.incrScan winSize f
                    f2 = Fold.fromScanl f
                a <- runIO $ S.fold f1 c
                b <- runIO $ S.fold f2 $ S.drop (numElem - winSize)
                        $ fmap Insert c
                let c1 = a - b
                it ("should not deviate more than " ++ show deviationLimit)
                    $ c1 >= -1 * deviationLimit && c1 <= deviationLimit

        describe "Sum" $ testFunc Scanl.incrSum
        describe "mean" $ testFunc Scanl.incrMean

    describe "Correctness" $ do
        let winSize = 3
            testCase1 =
                [1.0, 4.0, 3.0, 2.1, -5.1, -2.0, 7.0, 3.0, -2.5] :: [Double]

            testCase2 = replicate 5 1.0 ++ [7.0]

            testFunc tc f sI sW = do
                let c = S.fromList tc
                a <- runIO $ S.fold Fold.toList $ S.postscanl f $ fmap Insert c
                b <- runIO $ S.fold Fold.toList $ S.postscanl
                        (Scanl.incrScan winSize f) c
                it "Infinite" $ a  == sI
                it ("Finite " ++ show winSize) $ b == sW

            testFunc2 tc expec f = do
                let c = S.fromList tc
                a <- runIO $ S.toList $ S.postscanl (f winSize) c
                runIO $ print a
                it (show tc) $ a == expec

        describe "toList" $ do
            testFunc2 testCase1
                [[1.0],[1.0,4.0],[1.0,4.0,3.0],[4.0,3.0,2.1],[3.0,2.1,-5.1]
                ,[2.1,-5.1,-2.0],[-5.1,-2.0,7.0],[-2.0,7.0,3.0],[7.0,3.0,-2.5]
                ]
                (Ring.scanFoldRingsBy Fold.toList)
        describe "minimum" $ do
            testFunc2 testCase1
                [Just 1.0,Just 1.0,Just 1.0,Just 2.1,Just (-5.1),Just (-5.1)
                ,Just (-5.1),Just (-2.0),Just (-2.5)]
                (Ring.scanFoldRingsBy Fold.minimum)
        describe "maximum" $ do
            testFunc2 testCase1
                [Just 1.0,Just 4.0,Just 4.0,Just 4.0,Just 3.0,Just 2.1
                ,Just 7.0,Just 7.0,Just 7.0]
                (Ring.scanFoldRingsBy Fold.maximum)
        describe "range" $ do
            testFunc2 testCase1
                [Just (1.0,1.0),Just (1.0,4.0),Just (1.0,4.0),Just (2.1,4.0)
                ,Just (-5.1,3.0),Just (-5.1,2.1),Just (-5.1,7.0)
                ,Just (-2.0,7.0),Just (-2.5,7.0)]
                (Ring.scanFoldRingsBy Fold.range)
        describe "sum" $ do
            let scanInf = [1, 2, 3, 4, 5, 12] :: [Double]
                scanWin = [1, 2, 3, 3, 3, 9] :: [Double]
            testFunc testCase2 Scanl.incrSum scanInf scanWin
        describe "mean" $ do
            let scanInf = [1, 1, 1, 1, 1, 2] :: [Double]
                scanWin = [1, 1, 1, 1, 1, 3] :: [Double]
            testFunc testCase2 Scanl.incrMean scanInf scanWin
