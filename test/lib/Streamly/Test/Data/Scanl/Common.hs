-- |
-- Module      : Streamly.Test.Data.Scanl.Common
-- Copyright   : (c) 2024 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Shared test helpers for Scanl Continue-contract testing.  Used by
-- Scanl, Stream and Unfold test modules.

module Streamly.Test.Data.Scanl.Common
    ( evenScanl
    , filterLawScanModifier
    , filterLawScan
    ) where

import Data.IORef (IORef, modifyIORef')
import Test.QuickCheck (Property, counterexample, ioProperty)

import qualified Streamly.Internal.Data.Scanl as Scanl
import qualified Streamly.Internal.Data.Stream as Stream

-- | A scan that emits even inputs and returns 'Continue' (no output) for odd
-- ones.  Its 'extract' records every call so tests can assert it never fires
-- on a 'Continue' step.
evenScanl :: IORef [Int] -> Scanl.Scanl IO Int Int
evenScanl ref = Scanl.Scanl step initial extract final
  where
    initial = return (Scanl.toFoldStep (Scanl.Partial 0))
    step s a = return $ if even a then Scanl.Partial a else Scanl.Continue s
    extract s = modifyIORef' ref (s :) >> return s
    final = return

-- | Property: wrapping an inner scan in 'Scanl.filter' must give the same
-- output as pre-filtering the input stream before the scan sees it:
--
-- > postscanl\/scanl (ctx (Scanl.filter even inner)) xs
-- >   === postscanl\/scanl (ctx inner) (filter even xs)
--
-- Tests that combinators that wrap an inner scan honour the Continue contract
-- (no spurious output or extract on Continue steps).
filterLawScanModifier
    :: (Eq b, Show b)
    => (Scanl.Scanl IO Int Int -> Scanl.Scanl IO Int b)
    -> [Int]
    -> Property
filterLawScanModifier ctx xs = ioProperty $ do
    let inner = Scanl.sum
        run scn = Stream.toList . scn . Stream.fromList
    postL <- run (Stream.postscanl (ctx (Scanl.filter even inner))) xs
    postR <- run (Stream.postscanl (ctx inner)) (filter even xs)
    scanL <- run (Stream.scanl (ctx (Scanl.filter even inner))) xs
    scanR <- run (Stream.scanl (ctx inner)) (filter even xs)
    return
        $ counterexample
            ("postscanl: " ++ show postL ++ " /= " ++ show postR
                 ++ "\nscanl: " ++ show scanL ++ " /= " ++ show scanR)
            (postL == postR && scanL == scanR)

-- | Property: wrapping a scan in 'Scanl.filter' must give the same output as
-- pre-filtering the input stream:
--
-- > postscanl\/scanl (Scanl.filter even s) xs
-- >   === postscanl\/scanl s (filter even xs)
filterLawScan :: (Eq b, Show b) => Scanl.Scanl IO Int b -> [Int] -> Property
filterLawScan s xs = ioProperty $ do
    let run scn = Stream.toList . scn . Stream.fromList
    postL <- run (Stream.postscanl (Scanl.filter even s)) xs
    postR <- run (Stream.postscanl s) (filter even xs)
    scanL <- run (Stream.scanl (Scanl.filter even s)) xs
    scanR <- run (Stream.scanl s) (filter even xs)
    return
        $ counterexample
            ("postscanl: " ++ show postL ++ " /= " ++ show postR
                 ++ "\nscanl: " ++ show scanL ++ " /= " ++ show scanR)
            (postL == postR && scanL == scanR)
