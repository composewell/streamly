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
import Data.Function ( (&) )
import Data.IORef (newIORef, atomicModifyIORef')
import Data.List (sort)
import Streamly.Data.Scanl (Scanl)
import Test.Hspec as H

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
    ref <- newIORef (Scanl.take evenLen $ Scanl.mkScanl1 (\_ x -> x))
    let gen True =
            atomicModifyIORef' ref (\xs -> (fmap (const Nothing) Scanl.drain, xs))
        gen False = pure $ Scanl.mkScanl1 (\_ x -> x)
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
        gen _ = pure $ Scanl.mkScanl1 (\_ x -> x)
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
