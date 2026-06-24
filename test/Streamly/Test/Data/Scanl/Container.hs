{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}
-- |
-- Module      : Streamly.Test.Data.Scanl.Container
-- Copyright   : (c) 2024 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

module Streamly.Test.Data.Scanl.Container (main) where

import qualified Data.IntSet as IntSet
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Streamly.Internal.Data.Scanl as F

import Streamly.Test.Data.Scanl.Type (check, checkPostscanl)
import Test.Hspec

#include "Streamly/Test/Data/Scanl/CommonContainer.hs"

-------------------------------------------------------------------------------
-- Scanl-only tests: demultiplexing and classifying.
-------------------------------------------------------------------------------

-- Each key's inner scan is @take 2 sum@; the demux/classify scan emits
-- @Just (key, result)@ on the step at which a key's inner scan terminates and
-- @Nothing@ otherwise.

demuxInput :: [(String, Int)]
demuxInput = [("A", 1), ("A", 2), ("B", 3)]

demuxExpected :: [Maybe (String, Int)]
demuxExpected = [Just ("A", 1), Just ("A", 3), Just ("B", 3)]

demuxGetScanl :: String -> IO (Maybe (F.Scanl IO (String, Int) Int))
demuxGetScanl _ = return (Just (F.take 2 (F.lmap snd F.sum)))

demuxS :: Expectation
demuxS = checkPostscanl (F.demux fst demuxGetScanl) demuxInput demuxExpected

demuxIOS :: Expectation
demuxIOS = checkPostscanl (F.demuxIO fst demuxGetScanl) demuxInput demuxExpected

demuxGenericS :: Expectation
demuxGenericS =
    checkPostscanl
        (fmap snd
            (F.demuxGeneric fst demuxGetScanl
                :: F.Scanl IO (String, Int)
                    (IO (Map.Map String Int), Maybe (String, Int))))
        demuxInput demuxExpected

demuxGenericIOS :: Expectation
demuxGenericIOS =
    checkPostscanl
        (fmap snd
            (F.demuxGenericIO fst demuxGetScanl
                :: F.Scanl IO (String, Int)
                    (IO (Map.Map String Int), Maybe (String, Int))))
        demuxInput demuxExpected

classifyInput :: [(String, Int)]
classifyInput = [("ONE", 1), ("TWO", 2), ("ONE", 3), ("TWO", 4), ("ONE", 5)]

classifyExpected :: [Maybe (String, Int)]
classifyExpected =
    [Just ("ONE", 1), Just ("TWO", 2), Just ("ONE", 4), Just ("TWO", 6), Nothing]

classifyInner :: F.Scanl IO (String, Int) Int
classifyInner = F.lmap snd (F.take 2 F.sum)

classifyS :: Expectation
classifyS =
    checkPostscanl (F.classify fst classifyInner) classifyInput classifyExpected

classifyIOS :: Expectation
classifyIOS =
    checkPostscanl (F.classifyIO fst classifyInner) classifyInput classifyExpected

classifyGenericS :: Expectation
classifyGenericS =
    checkPostscanl
        (fmap snd
            (F.classifyGeneric fst classifyInner
                :: F.Scanl IO (String, Int)
                    (IO (Map.Map String Int), Maybe (String, Int))))
        classifyInput classifyExpected

classifyGenericIOS :: Expectation
classifyGenericIOS =
    checkPostscanl
        (fmap snd
            (F.classifyGenericIO fst classifyInner
                :: F.Scanl IO (String, Int)
                    (IO (Map.Map String Int), Maybe (String, Int))))
        classifyInput classifyExpected

moduleName :: String
moduleName = "Data.Scanl.Container"

main :: IO ()
main = hspec $
    describe moduleName $ do
        describe "common" commonContainerSpec

        -- Before adding any tests here consider if it can be added to the
        -- common tests above.
        it "demux" demuxS
        it "demuxIO" demuxIOS
        it "demuxGeneric" demuxGenericS
        it "demuxGenericIO" demuxGenericIOS
        it "classify" classifyS
        it "classifyIO" classifyIOS
        it "classifyGeneric" classifyGenericS
        it "classifyGenericIO" classifyGenericIOS
