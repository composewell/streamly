{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}
-- |
-- Module      : Streamly.Test.Data.Fold.Container
-- Copyright   : (c) 2019 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

module Streamly.Test.Data.Fold.Container (main) where

import qualified Data.IntSet as IntSet
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Streamly.Internal.Data.Fold as Fold
import qualified Streamly.Internal.Data.Fold as F
import qualified Streamly.Internal.Data.Stream as Stream

import Streamly.Test.Data.Fold.Type (check, checkPostscanl)
import Test.Hspec

#include "Streamly/Test/Data/Scanl/CommonContainer.hs"

demux :: Expectation
demux =
    let table "SUM" = return $ Just Fold.sum
        table "PRODUCT" = return $ Just Fold.product
        table _ = return $ Just Fold.length
        input = Stream.fromList
                ([ ("SUM", 1)
                , ("abc", 1)
                , ("PRODUCT", 2)
                , ("abc", 2)
                , ("SUM",3)
                , ("xyz", 1)
                , ("PRODUCT", 4)
                , ("xyz", 2)
                , ("abc", 2)
                ] :: [(String, Int)])
    in Stream.fold
        (Fold.demuxKvToMap table)
        input
        `shouldReturn`
        Map.fromList [("PRODUCT", 8),("SUM", 4),("abc",3),("xyz",2)]

demuxWith :: Expectation
demuxWith =
    let getKey x | even x = "SUM"
                 | otherwise = "PRODUCT"

        getFold "SUM" = return Fold.sum
        getFold "PRODUCT" = return Fold.product
        getFold _ = error "demuxWith: bug"

        input = Stream.fromList [1, 2, 3, 4 :: Int]
    in Stream.fold
        (Fold.demuxToContainer getKey (getFold . getKey))
        input
        `shouldReturn`
        Map.fromList [("PRODUCT",3),("SUM",6)]

classifyWith :: Expectation
classifyWith =
    let input = Stream.fromList [("ONE",1),("ONE",1.1),("TWO",2), ("TWO",2.2)]
    in Stream.fold
        (Fold.toContainer fst (Fold.lmap snd Fold.toList))
        input
        `shouldReturn`
        Map.fromList
        [("ONE",[1.0, 1.1 :: Double]), ("TWO",[2.0, 2.2])]

classify :: Expectation
classify =
    let input =
            Stream.fromList
            [
              ("ONE", (1::Int, 1))
            , ("ONE", (1, 1.1:: Double))
            , ("TWO", (2, 2))
            , ("TWO",(2, 2.2))
            ]
    in Stream.fold
        (Fold.kvToMap (Fold.lmap snd Fold.toList))
        input
        `shouldReturn`
        Map.fromList
        [("ONE",[1.0, 1.1 :: Double]), ("TWO",[2.0, 2.2])]

classifyScan :: Expectation
classifyScan =
    let getKey = fst
        innerFold = Fold.lmap snd (Fold.take 2 Fold.sum)
        input = Stream.fromList
            [ ("ONE", 1::Int)
            , ("TWO", 2)
            , ("ONE", 3)
            , ("TWO", 4)
            , ("ONE", 5)
            ]
    in Stream.fold Fold.toList
        (Stream.postscanlMaybe (Fold.classifyScan getKey innerFold) input)
        `shouldReturn` [("ONE", 4), ("TWO", 6)]

frequency :: Expectation
frequency =
    Stream.fold Fold.frequency
        (Stream.fromList ["a","b","a","c","b","a" :: String])
    `shouldReturn`
    Map.fromList [("a",3),("b",2),("c",1)]

demuxerToMap :: Expectation
demuxerToMap =
    let getKey = fst
        getFold "SUM" = return $ Just (Fold.lmap snd Fold.sum)
        getFold _     = return $ Just (Fold.lmap snd Fold.length)
        input = Stream.fromList
                    [("SUM",1),("X",1),("SUM",3),("X",2),("X",3)] :: Stream.Stream IO (String, Int)
    in Stream.fold
        (Fold.demuxerToMap getKey getFold)
        input
        `shouldReturn`
        Map.fromList [("SUM", 4 :: Int), ("X", 3)]

demuxerToContainer :: Expectation
demuxerToContainer =
    let getKey = fst
        getFold "SUM" = return $ Just (Fold.lmap snd Fold.sum)
        getFold _     = return $ Just (Fold.lmap snd Fold.length)
        input = Stream.fromList
                    [("SUM",1),("X",1),("SUM",3),("X",2),("X",3)] :: Stream.Stream IO (String, Int)
    in Stream.fold
        (Fold.demuxerToContainer getKey getFold)
        input
        `shouldReturn`
        Map.fromList [("SUM", 4 :: Int), ("X", 3)]

demuxKvToContainer :: Expectation
demuxKvToContainer =
    let table "SUM" = return $ Just Fold.sum
        table "PRODUCT" = return $ Just Fold.product
        table _ = return $ Just Fold.length
        input = Stream.fromList
                    [("SUM",1),("PRODUCT",2),("SUM",3),("PRODUCT",4)] :: Stream.Stream IO (String, Int)
    in Stream.fold
        (Fold.demuxKvToContainer table)
        input
        `shouldReturn`
        Map.fromList [("PRODUCT",8 :: Int),("SUM",4)]

toMap :: Expectation
toMap =
    let input = Stream.fromList [("ONE",1),("ONE",1.1),("TWO",2),("TWO",2.2)]
    in Stream.fold
        (Fold.toMap fst (Fold.lmap snd Fold.toList))
        input
        `shouldReturn`
        Map.fromList [("ONE",[1.0,1.1 :: Double]), ("TWO",[2.0,2.2])]

demuxScan :: Expectation
demuxScan = do
    let getKey = fst
        getFold _ = return $ Just (Fold.take 2 (Fold.lmap snd Fold.sum))
        input = Stream.fromList
                    [("A",1),("A",2),("B",3)] :: Stream.Stream IO (String, Int)
    r <- Stream.fold Fold.toList
             $ Stream.catMaybes
             $ Stream.postscanl (Fold.demuxScan getKey getFold) input
    r `shouldBe` [("A", 3 :: Int)]

moduleName :: String
moduleName = "Data.Fold.Container"

main :: IO ()
main = hspec $ do
    describe moduleName $ do
        describe "common" commonContainerSpec

        -- Before adding any tests here consider if it can be added to the
        -- common tests above.

        it "demux" demux
        it "demuxWith" demuxWith
        it "classifyWith" classifyWith
        it "classify" classify
        it "classifyScan" classifyScan
        it "frequency" frequency
        it "demuxerToMap" demuxerToMap
        it "demuxerToContainer" demuxerToContainer
        it "demuxKvToContainer" demuxKvToContainer
        it "toMap" toMap
        it "demuxScan" demuxScan
