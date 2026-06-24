module Streamly.Test.Data.Scanl.Window (main) where

import Test.Hspec (hspec, describe, it, runIO, shouldReturn)
import Streamly.Internal.Data.Scanl (Incr(..))
import qualified Streamly.Internal.Data.Fold as Fold
import qualified Streamly.Internal.Data.RingArray as RingArray
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
                    $ c1 >= -(1 * deviationLimit) && c1 <= deviationLimit

        describe "Sum" $ testFunc Scanl.incrSum
        describe "mean" $ testFunc Scanl.incrMean

    describe "Correctness" $ do
        let winSize = 3
            testCase1 =
                [1.0, 4.0, 3.0, 2.1, -5.1, -2.0, 7.0, 3.0, -2.5] :: [Double]

            testCase2 = replicate 5 1.0 ++ [7.0]

            testFunc tc f sI sW = do
                let c = S.fromList tc
                a <- runIO $ S.toList $ S.postscanl f $ fmap Insert c
                b <- runIO $ S.toList $ S.postscanl
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
                (RingArray.scanFoldRingsBy (Fold.fromScanl Scanl.toList))
        describe "minimum" $ do
            testFunc2 testCase1
                [Just 1.0,Just 1.0,Just 1.0,Just 2.1,Just (-5.1),Just (-5.1)
                ,Just (-5.1),Just (-2.0),Just (-2.5)]
                (RingArray.scanFoldRingsBy (Fold.fromScanl Scanl.minimum))
        describe "maximum" $ do
            testFunc2 testCase1
                [Just 1.0,Just 4.0,Just 4.0,Just 4.0,Just 3.0,Just 2.1
                ,Just 7.0,Just 7.0,Just 7.0]
                (RingArray.scanFoldRingsBy (Fold.fromScanl Scanl.maximum))
        describe "range" $ do
            testFunc2 testCase1
                [Just (1.0,1.0),Just (1.0,4.0),Just (1.0,4.0),Just (2.1,4.0)
                ,Just (-5.1,3.0),Just (-5.1,2.1),Just (-5.1,7.0)
                ,Just (-2.0,7.0),Just (-2.5,7.0)]
                (RingArray.scanFoldRingsBy (Fold.fromScanl Scanl.range))
        describe "sum" $ do
            let scanInf = [1, 2, 3, 4, 5, 12] :: [Double]
                scanWin = [1, 2, 3, 3, 3, 9] :: [Double]
            testFunc testCase2 Scanl.incrSum scanInf scanWin
        describe "mean" $ do
            let scanInf = [1, 1, 1, 1, 1, 2] :: [Double]
                scanWin = [1, 1, 1, 1, 1, 3] :: [Double]
            testFunc testCase2 Scanl.incrMean scanInf scanWin

    -- Direct tests for the incremental and windowed scans. 'incrScan' and the
    -- cumulative versions ('incrSum', 'incrMean') are exercised above; here we
    -- cover the remaining exported scans.
    describe "Scanl operations" $ do
        let scl s xs = S.toList (S.scanl s (S.fromList xs))
            -- A stream of incremental operations exercising both Insert and
            -- Replace constructors. Window of {1,2} then replace 1 with 5.
            incrs = [Insert 1, Insert 2, Replace 1 5] :: [Incr Int]

        it "cumulativeScan" $
            scl (Scanl.cumulativeScan Scanl.incrSum) ([1, 2, 3] :: [Double])
                `shouldReturn` [0, 1, 3, 6]
        it "incrScanWith" $
            scl (Scanl.incrScanWith 3 (Scanl.lmap fst Scanl.incrSum))
                ([1, 2, 3, 4, 5] :: [Double])
                `shouldReturn` [0, 1, 3, 6, 9, 12]
        it "incrRollingMap" $
            scl (Scanl.incrRollingMap (\p c -> Just (maybe 0 (c -) p)))
                ([Insert 1, Replace 1 3, Replace 3 6] :: [Incr Int])
                `shouldReturn` [Nothing, Just 0, Just 2, Just 3]
        it "incrRollingMapM" $
            scl (Scanl.incrRollingMapM (\p c -> return (Just (maybe 0 (c -) p))))
                ([Insert 1, Replace 1 3, Replace 3 6] :: [Incr Int])
                `shouldReturn` [Nothing, Just 0, Just 2, Just 3]
        it "incrCount" $
            (scl Scanl.incrCount incrs :: IO [Int])
                `shouldReturn` [0, 1, 2, 2]
        it "incrSumInt" $
            scl Scanl.incrSumInt incrs `shouldReturn` [0, 1, 3, 7]
        it "incrPowerSum" $
            scl (Scanl.incrPowerSum 2) ([Insert 2, Insert 3] :: [Incr Int])
                `shouldReturn` [0, 4, 13]
        it "incrPowerSumFrac" $
            scl (Scanl.incrPowerSumFrac 0.5)
                ([Insert 4, Insert 9] :: [Incr Double])
                `shouldReturn` [0, 2, 5]
        it "windowRange" $
            scl (Scanl.windowRange 3) ([1, 2, 3, 4, 5] :: [Int])
                `shouldReturn`
                [ Nothing, Just (1, 1), Just (1, 2), Just (1, 3), Just (2, 4)
                , Just (3, 5)
                ]
        it "windowMinimum" $
            scl (Scanl.windowMinimum 3) ([1, 2, 3, 4, 5] :: [Int])
                `shouldReturn`
                [Nothing, Just 1, Just 1, Just 1, Just 2, Just 3]
        it "windowMaximum" $
            scl (Scanl.windowMaximum 3) ([1, 2, 3, 4, 5] :: [Int])
                `shouldReturn`
                [Nothing, Just 1, Just 2, Just 3, Just 4, Just 5]
