-- |
-- Module      : Streamly.Test.Data.Unfold.Prelude
-- Copyright   : (c) 2026 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

module Streamly.Test.Data.Unfold.Prelude (main) where

import Control.Exception (Exception, try)
import Control.Monad.Catch (throwM)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Streamly.Internal.Data.Unfold (Unfold)

import qualified Streamly.Data.Fold as Fold
import qualified Streamly.Data.Stream as Stream
import qualified Streamly.Internal.Data.Unfold as Unfold
import qualified Streamly.Internal.Data.Unfold.Prelude as Unfold

import Test.Hspec as H

newtype TestException = TestException String deriving (Eq, Show)

instance Exception TestException

-- | An unfold whose inject raises an exception.
throwingInject :: Unfold IO [Int] Int
throwingInject =
    Unfold.lmapM (\_ -> throwM (TestException "inject")) Unfold.fromList

-- | An unfold that yields 1 and then raises an exception.
throwingStep :: Unfold IO [Int] Int
throwingStep =
    Unfold.lmap
        (\_ -> [return 1, throwM (TestException "step")]) Unfold.fromListM

-- | Run an unfold on @[1,2,3]@ and return the result or the exception.
run :: Unfold IO [Int] Int -> IO (Either TestException [Int])
run unf = try $ Stream.fold Fold.toList $ Stream.unfold unf [1,2,3]

-- | Run the unfold made from the IORef and return the result and the final
-- value of the IORef.
runRef ::
    (IORef Int -> Unfold IO [Int] Int) -> IO (Either TestException [Int], Int)
runRef f = do
    ref <- newIORef 0
    res <- run (f ref)
    val <- readIORef ref
    return (res, val)

alloc :: IORef Int -> [Int] -> IO [Int]
alloc ref xs = writeIORef ref 1 >> return xs

set :: IORef Int -> Int -> a -> IO ()
set ref n _ = writeIORef ref n

injectErr, stepErr :: Either TestException a
injectErr = Left (TestException "inject")
stepErr = Left (TestException "step")

testBracket :: Spec
testBracket =
    describe "bracket" $ do
        let unf ref = Unfold.bracket (alloc ref) (set ref 2)
        it "normal end" $
            runRef (\ref -> unf ref Unfold.fromList)
                `shouldReturn` (Right [1,2,3], 2)
        it "exception in step" $
            runRef (\ref -> unf ref throwingStep) `shouldReturn` (stepErr, 2)
        it "exception in inject" $
            runRef (\ref -> unf ref throwingInject)
                `shouldReturn` (injectErr, 2)

testFinally :: Spec
testFinally =
    describe "finally" $ do
        let unf ref = Unfold.finally (set ref 2)
        it "normal end" $
            runRef (\ref -> unf ref Unfold.fromList)
                `shouldReturn` (Right [1,2,3], 2)
        it "exception in step" $
            runRef (\ref -> unf ref throwingStep) `shouldReturn` (stepErr, 2)
        it "exception in inject" $
            runRef (\ref -> unf ref throwingInject)
                `shouldReturn` (injectErr, 2)

testAfter :: Spec
testAfter =
    describe "after" $ do
        it "normal end" $
            runRef (\ref -> Unfold.after (set ref 2) Unfold.fromList)
                `shouldReturn` (Right [1,2,3], 2)

testGbracket :: Spec
testGbracket =
    describe "gbracket" $ do
        let handler = Unfold.lmap (\(_, _ :: TestException) -> [99])
                        Unfold.fromList
            unf ref = Unfold.gbracket (alloc ref) (set ref 2) handler try
        it "normal end" $
            runRef (\ref -> unf ref Unfold.fromList)
                `shouldReturn` (Right [1,2,3], 2)
        it "exception in step" $
            runRef (\ref -> unf ref throwingStep)
                `shouldReturn` (Right [1,99], 1)
        it "exception in inject" $
            runRef (\ref -> unf ref throwingInject)
                `shouldReturn` (Right [99], 1)

moduleName :: String
moduleName = "Data.Unfold.Prelude"

main :: IO ()
main = hspec $ describe moduleName $ do
    testBracket
    testFinally
    testAfter
    testGbracket
