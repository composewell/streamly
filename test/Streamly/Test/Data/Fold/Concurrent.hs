-- |
-- Module      : Streamly.Test.Data.Fold.Concurrent
-- Copyright   : (c) 2026 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

module Streamly.Test.Data.Fold.Concurrent (main) where

import Control.Concurrent (threadDelay)
import Control.Exception (ErrorCall(..), throwIO, try)
import Data.IORef (atomicModifyIORef', newIORef)
import System.Timeout (timeout)
import Test.Hspec (describe, hspec, it, shouldBe, shouldReturn)

import qualified Streamly.Data.Fold as Fold
import qualified Streamly.Data.Stream as Stream
import qualified Streamly.Internal.Data.Fold.Prelude as Fold

-- | The output stream has no empty lists.
parNoEmptyOutput :: IO ()
parNoEmptyOutput = do
    Stream.toList
        (Fold.parDistributeScan id (return [] :: IO [Fold.Fold IO Int Int])
            (Stream.fromList [1]))
        `shouldReturn` []
    -- The generator returns the fold only once.
    ref <- newIORef [Fold.take 2 Fold.sum]
    let gen = atomicModifyIORef' ref (\fs -> ([], fs))
    xs <- Stream.toList
        $ Fold.parDistributeScan id gen (Stream.fromList [1..10 :: Int])
    filter null xs `shouldBe` []
    concat xs `shouldBe` [3]
    ys <- Stream.toList
        $ Fold.parDemuxScan id even (const (return (Fold.take 2 Fold.sum)))
            (Stream.fromList [1..10 :: Int])
    filter null ys `shouldBe` []

parBuffered :: IO ()
parBuffered = do
    Stream.fold (Fold.parBuffered id Fold.sum)
        (Stream.fromList [1..100 :: Int])
        `shouldReturn` 5050
    Stream.fold
        (Fold.parBuffered (Fold.maxBuffer 1) (Fold.take 1 Fold.toList))
        (Stream.fromList [1..1000 :: Int])
        `shouldReturn` [1]
    -- The fold terminates while the driver waits for buffer space.
    let slow x = threadDelay 100000 >> return x
    timeout 5000000
        (Stream.fold
            (Fold.parBuffered (Fold.maxBuffer 1)
                (Fold.take 1 (Fold.lmapM slow Fold.toList)))
            (Stream.fromList [1..1000 :: Int]))
        `shouldReturn` Just [1]

parTeeWith :: IO ()
parTeeWith =
    Stream.fold (Fold.parTeeWith id (,) Fold.sum Fold.length)
        (Stream.fromList [1..100 :: Int])
        `shouldReturn` (5050, 100)

input :: Stream.Stream IO Int
input = Stream.fromList [1..100]

parTee :: IO ()
parTee =
    Stream.fold (Fold.parTee id (Fold.take 2 Fold.toList) Fold.sum) input
        `shouldReturn` ([1, 2], 5050)

parDistribute :: IO ()
parDistribute = do
    Stream.fold (Fold.parDistribute id [Fold.sum, Fold.length]) input
        `shouldReturn` [5050, 100]
    Stream.fold (Fold.parDistribute id [Fold.sum, Fold.length]) Stream.nil
        `shouldReturn` [0, 0]

parPartition :: IO ()
parPartition =
    Stream.fold (Fold.parPartition id Fold.sum Fold.length)
        (fmap (\x -> if odd x then Left x else Right x) input)
        `shouldReturn` (2500, 50)

parUnzip :: IO ()
parUnzip = do
    Stream.fold (Fold.parUnzip id Fold.sum Fold.length)
        (fmap (\x -> (x, x)) input)
        `shouldReturn` (5050, 100)
    Stream.fold
        (Fold.parUnzipWithM id (\x -> return (x, show x)) Fold.sum Fold.toList)
        (Stream.fromList [1, 2, 3 :: Int])
        `shouldReturn` (6, ["1", "2", "3"])

-- | An exception in the fold is raised by the driver.
parBufferedException :: IO ()
parBufferedException = do
    let failAt3 = Fold.foldlM'
            (\_ x -> if x == 3 then throwIO (ErrorCall "fold") else return x)
            (return 0)
    r <- try $ Stream.fold (Fold.parBuffered id failAt3) input
    r `shouldBe` (Left (ErrorCall "fold") :: Either ErrorCall Int)

main :: IO ()
main = hspec $ describe "Data.Fold.Concurrent" $ do
    it "parTee" parTee
    it "parDistribute" parDistribute
    it "parPartition" parPartition
    it "parUnzip/parUnzipWithM" parUnzip
    it "parBuffered exception" parBufferedException
    it "parDistributeScan/parDemuxScan no empty output" parNoEmptyOutput
    it "parBuffered" parBuffered
    it "parTeeWith" parTeeWith
