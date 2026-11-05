{-# LANGUAGE CPP #-}

-- |
-- Module      : Streamly.Test.Data.Fold.Time
-- Copyright   : (c) 2026 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

module Streamly.Test.Data.Fold.Time (main) where

#if MIN_VERSION_base(4,18,0)
import Control.Concurrent (threadDelay)
import GHC.Conc (ThreadStatus(..), listThreads, threadStatus)
#endif
import Test.Hspec (describe, hspec, it, shouldReturn)
#if MIN_VERSION_base(4,18,0)
import Test.Hspec (shouldBe)
#endif

import qualified Streamly.Data.Fold as Fold
import qualified Streamly.Data.Stream as Stream
import qualified Streamly.Internal.Data.Fold.Prelude as Fold

#if MIN_VERSION_base(4,18,0)
-- | The timer thread does not outlive the fold.
testTimerThreadKilled :: IO ()
testTimerThreadKilled = do
    before <- listThreads
    xs <- Stream.fold (Fold.takeInterval 100 Fold.toList)
            (Stream.fromList [1..5 :: Int])
    xs `shouldBe` [1..5]
    threadDelay 100000
    after <- listThreads
    let new = filter (`notElem` before) after
    statuses <- mapM threadStatus new
    filter (`notElem` [ThreadFinished, ThreadDied]) statuses `shouldBe` []
#endif

main :: IO ()
main = hspec $ describe "Data.Fold.Time" $ do
    it "intervalsOf" $ do
        -- All the elements are in the first interval.
        Stream.fold (Fold.intervalsOf 10 Fold.sum Fold.toList)
            (Stream.fromList [1..5 :: Int])
            `shouldReturn` [15]
        Stream.fold (Fold.intervalsOf 10 Fold.sum Fold.toList) Stream.nil
            `shouldReturn` ([] :: [Int])
    it "takeInterval before the timeout" $
        Stream.fold (Fold.takeInterval 10 Fold.toList)
            (Stream.fromList [1..5 :: Int])
            `shouldReturn` [1..5]
#if MIN_VERSION_base(4,18,0)
    it "takeInterval kills the timer thread" testTimerThreadKilled
#endif
