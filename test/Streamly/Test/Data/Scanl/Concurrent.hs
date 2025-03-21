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
import qualified Streamly.Data.Stream.Prelude as Stream
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

parDistributeScanTestScanEnd :: (Stream.Config -> Stream.Config) -> IO ()
parDistributeScanTestScanEnd concOpts = do
    let streamLen = 10000
        evenLen = 100
    ref <- newIORef [Scanl.take evenLen evenScan, oddScan]
    let gen = atomicModifyIORef' ref (\xs -> ([], xs))
        inpList = [1..streamLen]
        inpStream = Stream.fromList inpList
    res1 <-
        Scanl.parDistributeScan concOpts gen inpStream
            & Stream.concatMap Stream.fromList
            & Stream.catMaybes
            & Stream.fold Fold.toList
    sort res1 `shouldBe` [1..evenLen] ++ filter odd [(evenLen+1)..streamLen]

parDistributeScanTestStreamEnd :: (Stream.Config -> Stream.Config) -> IO ()
parDistributeScanTestStreamEnd concOpts = do
    let streamLen = 10000
    ref <- newIORef [evenScan, oddScan]
    let gen = atomicModifyIORef' ref (\xs -> ([], xs))
        inpList = [1..streamLen]
        inpStream = Stream.fromList inpList
    res1 <-
        Scanl.parDistributeScan concOpts gen inpStream
            & Stream.concatMap Stream.fromList
            & Stream.catMaybes
            & Stream.fold Fold.toList
    sort res1 `shouldBe` inpList

main :: IO ()
main = hspec
  $ H.parallel
#ifdef COVERAGE_BUILD
  $ modifyMaxSuccess (const 10)
#endif
  $ describe moduleName $ do
        it "parDistributeScan (stream end) (maxBuffer 1)"
            $ parDistributeScanTestStreamEnd (Stream.maxBuffer 1)
        it "parDistributeScan (scan end)"
            $ parDistributeScanTestScanEnd (Stream.maxBuffer 1)
