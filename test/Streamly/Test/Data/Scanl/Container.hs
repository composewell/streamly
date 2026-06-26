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

-------------------------------------------------------------------------------
-- 'Continue' handling.
--
-- The inner scan below filters out odd values and sums the even ones. A
-- filtered-out (odd) input makes the inner scan emit 'Continue': the scan
-- state must advance but NO output must be emitted for that input. The
-- demux/classify driver must translate this 'Continue' into 'Nothing' rather
-- than re-emitting the last valid value.
--
-- The earlier tests used @take 2 sum@ as the inner scan, which only ever emits
-- 'Partial'/'Done' and never 'Continue', so the 'Continue' branch of the
-- driver was never exercised. With the buggy driver, the odd inputs below
-- would wrongly produce @Just (key, lastSum)@ (e.g. @Just ("A", 2)@ and
-- @Just ("B", 0)@) instead of 'Nothing'.
-------------------------------------------------------------------------------

filterInput :: [(String, Int)]
filterInput = [("A", 2), ("A", 1), ("A", 4), ("B", 3), ("B", 6)]

filterExpected :: [Maybe (String, Int)]
filterExpected =
    [Just ("A", 2), Nothing, Just ("A", 6), Nothing, Just ("B", 6)]

filterInner :: F.Scanl IO (String, Int) Int
filterInner = F.lmap snd (F.filter even F.sum)

demuxFilterGetScanl :: String -> IO (Maybe (F.Scanl IO (String, Int) Int))
demuxFilterGetScanl _ = return (Just filterInner)

demuxFilterS :: Expectation
demuxFilterS =
    checkPostscanl (F.demux fst demuxFilterGetScanl) filterInput filterExpected

demuxFilterIOS :: Expectation
demuxFilterIOS =
    checkPostscanl
        (F.demuxIO fst demuxFilterGetScanl) filterInput filterExpected

demuxGenericFilterS :: Expectation
demuxGenericFilterS =
    checkPostscanl
        (fmap snd
            (F.demuxGeneric fst demuxFilterGetScanl
                :: F.Scanl IO (String, Int)
                    (IO (Map.Map String Int), Maybe (String, Int))))
        filterInput filterExpected

demuxGenericFilterIOS :: Expectation
demuxGenericFilterIOS =
    checkPostscanl
        (fmap snd
            (F.demuxGenericIO fst demuxFilterGetScanl
                :: F.Scanl IO (String, Int)
                    (IO (Map.Map String Int), Maybe (String, Int))))
        filterInput filterExpected

classifyFilterS :: Expectation
classifyFilterS =
    checkPostscanl (F.classify fst filterInner) filterInput filterExpected

classifyFilterIOS :: Expectation
classifyFilterIOS =
    checkPostscanl (F.classifyIO fst filterInner) filterInput filterExpected

classifyGenericFilterS :: Expectation
classifyGenericFilterS =
    checkPostscanl
        (fmap snd
            (F.classifyGeneric fst filterInner
                :: F.Scanl IO (String, Int)
                    (IO (Map.Map String Int), Maybe (String, Int))))
        filterInput filterExpected

classifyGenericFilterIOS :: Expectation
classifyGenericFilterIOS =
    checkPostscanl
        (fmap snd
            (F.classifyGenericIO fst filterInner
                :: F.Scanl IO (String, Int)
                    (IO (Map.Map String Int), Maybe (String, Int))))
        filterInput filterExpected

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

        -- demux
        it "demux" demuxS
        it "demux: filter" demuxFilterS
        it "demuxIO" demuxIOS
        it "demuxIO: filter" demuxFilterIOS
        it "demuxGeneric" demuxGenericS
        it "demuxGeneric: filter" demuxGenericFilterS
        it "demuxGenericIO" demuxGenericIOS
        it "demuxGenericIO: filter" demuxGenericFilterIOS

        -- classify
        it "classify" classifyS
        it "classify: filter" classifyFilterS
        it "classifyIO" classifyIOS
        it "classifyIO: filter" classifyFilterIOS
        it "classifyGeneric" classifyGenericS
        it "classifyGeneric: filter" classifyGenericFilterS
        it "classifyGenericIO" classifyGenericIOS
        it "classifyGenericIO: filter" classifyGenericFilterIOS
