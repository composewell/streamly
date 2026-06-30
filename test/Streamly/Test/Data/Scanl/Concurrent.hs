-- |
-- Module      : Streamly.Test.Data.Scanl.Concurrent
-- Copyright   : (c) 2020 Composewell Technologies
--
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

module Streamly.Test.Data.Scanl.Concurrent (main) where

import Control.Concurrent (threadDelay)
import Control.Exception (ErrorCall(..), try)
import Data.Function ( (&) )
import Data.IORef (atomicModifyIORef', newIORef, readIORef)
import Data.List (sort)
import Streamly.Data.Scanl (Scanl)
import Streamly.Test.Data.Scanl.Common (evenScanl, filterLawScan)
import Test.Hspec as H
import Test.Hspec.QuickCheck (prop)

import qualified Streamly.Data.Fold as Fold
import qualified Streamly.Data.Stream as Stream
import qualified Streamly.Internal.Data.Scanl as Scanl
import qualified Streamly.Internal.Data.Scanl.Prelude as Scanl

moduleName :: String
moduleName = "Data.Scanl.Concurrent"

---------------------------------------------------------------------------
-- Main
---------------------------------------------------------------------------

evenScan :: Scanl IO Int (Maybe Int)
evenScan =
    Scanl.filtering even
        & Scanl.lmapM (\x -> threadDelay 100 >> pure x)

oddScan :: Scanl IO Int (Maybe Int)
oddScan =
    Scanl.filtering odd
        & Scanl.lmapM (\x -> threadDelay 100 >> pure x)

parDistributeScan_ScanEnd :: (Scanl.Config -> Scanl.Config) -> IO ()
parDistributeScan_ScanEnd concOpts = do
    let streamLen = 10000
        evenLen = 100
    ref <- newIORef [Scanl.take evenLen evenScan, oddScan]
    let gen = atomicModifyIORef' ref (\xs -> ([], xs))
        inpList = [1..streamLen]
        inpStream = Stream.fromList inpList
    res1 <-
        Scanl.parDistributeScanM concOpts gen inpStream
            & Stream.concatMap Stream.fromList
            & Stream.catMaybes
            & Stream.fold Fold.toList
    sort res1 `shouldBe` [1..evenLen] ++ filter odd [(evenLen+1)..streamLen]

parDemuxScan_ScanEnd :: (Scanl.Config -> Scanl.Config) -> IO ()
parDemuxScan_ScanEnd concOpts = do
    let streamLen = 10000
        evenLen = 100
        demuxer i = even (i :: Int)
    ref <- newIORef (Scanl.take evenLen $ Scanl.scanl1' (\_ x -> x))
    let gen True =
            atomicModifyIORef' ref (\xs -> (fmap (const Nothing) Scanl.drain, xs))
        gen False = pure $ Scanl.scanl1' (\_ x -> x)
        inpList = [1..streamLen]
        inpStream = Stream.fromList inpList
    res <-
        Scanl.parDemuxScanM concOpts demuxer gen inpStream
            & Stream.concatMap Stream.fromList
            & fmap (\x -> (fst x,) <$> snd x)
            & Stream.catMaybes
            & Stream.fold Fold.toList
    map snd (filter fst res) `shouldBe` take evenLen [2, 4 ..]
    map snd (filter (not . fst) res) `shouldBe` filter odd [1..streamLen]

parDistributeScan_StreamEnd :: (Scanl.Config -> Scanl.Config) -> IO ()
parDistributeScan_StreamEnd concOpts = do
    let streamLen = 10000
    ref <- newIORef [evenScan, oddScan]
    let gen = atomicModifyIORef' ref (\xs -> ([], xs))
        inpList = [1..streamLen]
        inpStream = Stream.fromList inpList
    res1 <-
        Scanl.parDistributeScanM concOpts gen inpStream
            & Stream.concatMap Stream.fromList
            & Stream.catMaybes
            & Stream.fold Fold.toList
    sort res1 `shouldBe` inpList

parDemuxScan_StreamEnd :: (Scanl.Config -> Scanl.Config) -> IO ()
parDemuxScan_StreamEnd concOpts = do
    let streamLen = 10000
        demuxer i = even (i :: Int)
        gen _ = pure $ Scanl.scanl1' (\_ x -> x)
        inpList = [1..streamLen]
        inpStream = Stream.fromList inpList
    res <-
        Scanl.parDemuxScanM concOpts demuxer gen inpStream
            & Stream.concatMap Stream.fromList
            & fmap (\x -> (fst x,) <$> snd x)
            & Stream.catMaybes
            & Stream.fold Fold.toList
    map snd (filter fst res) `shouldBe` filter even [1..streamLen]
    map snd (filter (not . fst) res) `shouldBe` filter odd [1..streamLen]

parDemuxScan_WorkerException :: (Scanl.Config -> Scanl.Config) -> IO ()
parDemuxScan_WorkerException concOpts = do
    let throwAfter = 3
        -- All items go to the same key so the driver stays in sendToWorker_
        -- for a single worker channel, maximizing the chance of blocking on a
        -- full buffer.
        demuxer _ = (0 :: Int)
        gen _ = pure
            $ Scanl.lmapM
                (\x -> do
                    -- Slow the worker down so the buffer fills up and the
                    -- driver blocks in sendToWorker_ waiting for space.
                    threadDelay 50000
                    if (x :: Int) > throwAfter
                    then error "worker exception"
                    else pure x)
            $ Scanl.scanl1' (\_ x -> x)
        -- Send enough items to fill the buffer (maxBuffer 1) and block
        inpList = [1..100]
        inpStream = Stream.fromList inpList
    res <- try
        $ Scanl.parDemuxScanM concOpts demuxer gen inpStream
            & Stream.concatMap Stream.fromList
            & fmap (\x -> (fst x,) <$> snd x)
            & Stream.catMaybes
            & Stream.fold Fold.toList
    case res of
        Left (ErrorCall msg) -> msg `shouldBe` "worker exception"
        Right _ -> expectationFailure
            "Expected ErrorCall exception but stream completed successfully"

parTeeWithFilter :: IO ()
parTeeWithFilter = do
    ref <- newIORef []
    out <-
        Stream.toList
            $ Stream.postscanl
                (Scanl.parTeeWith id (,) (evenScanl ref) (Scanl.scanl' (\_ x -> x) 0))
                (Stream.fromList [1 .. 4 :: Int])
    calls <- reverse <$> readIORef ref
    out `shouldBe` [(2, 2), (4, 4)]
    calls `shouldBe` [0, 2, 4]

parTeeWithRightFilter :: IO ()
parTeeWithRightFilter = do
    ref <- newIORef []
    out <-
        Stream.toList
            $ Stream.postscanl
                (Scanl.parTeeWith id (,) (Scanl.scanl' (\_ x -> x) 0) (evenScanl ref))
                (Stream.fromList [1 .. 4 :: Int])
    calls <- reverse <$> readIORef ref
    out `shouldBe` [(2, 2), (4, 4)]
    calls `shouldBe` [0, 2, 4]

main :: IO ()
main = hspec
  $ H.parallel
#ifdef COVERAGE_BUILD
  $ modifyMaxSuccess (const 10)
#endif
  $ describe moduleName $ do
        it "parDistributeScanM (stream end) (maxBuffer 1)"
            $ parDistributeScan_StreamEnd (Scanl.maxBuffer 1)
        it "parDistributeScanM (scan end) (maxBuffer 1)"
            $ parDistributeScan_ScanEnd (Scanl.maxBuffer 1)
        it "parDemuxScanM (stream end) (maxBuffer 1)"
            $ parDemuxScan_StreamEnd (Scanl.maxBuffer 1)
        it "parDemuxScanM (scan end) (maxBuffer 1)"
            $ parDemuxScan_ScanEnd (Scanl.maxBuffer 1)
        it "parDemuxScanM (worker exception) (maxBuffer 1)"
            $ parDemuxScan_WorkerException (Scanl.maxBuffer 1)
        it "parTeeWith filter" parTeeWithFilter
        it "parTeeWith right filter" parTeeWithRightFilter
        prop "parTeeWith filter law"
            $ filterLawScan (Scanl.parTeeWith id (,) Scanl.sum Scanl.sum)
