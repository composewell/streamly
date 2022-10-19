{-# OPTIONS_GHC -Wno-deprecations #-}

-- |
-- Module      : Streamly.Test.Prelude.Fold
-- Copyright   : (c) 2020 Composewell Technologies
--
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

module Streamly.Test.Prelude.Fold (main) where

#ifdef DEVBUILD
import Control.Concurrent (threadDelay)
#endif
import Control.Exception (ErrorCall(..), catch)
import Data.IORef (newIORef, readIORef, writeIORef, IORef)
#ifdef COVERAGE_BUILD
import Test.Hspec.QuickCheck (modifyMaxSuccess)
#endif
import Test.Hspec as H
#ifdef DEVBUILD
import Data.IORef (modifyIORef)
import System.Mem (performMajorGC)
#endif

import qualified Streamly.Internal.Data.Stream.IsStream as IS
import qualified Streamly.Prelude as S

import Streamly.Prelude (SerialT)
#ifdef DEVBUILD
import Streamly.Prelude (IsStream)
#endif

#ifdef DEVBUILD
checkFoldxStrictness :: IO ()
checkFoldxStrictness = do
  let s = return (1 :: Int) `S.consM` error "failure"
  catch (S.foldx (\_ a -> if a == 1 then error "success" else "done")
                      "begin" id s)
    (\(ErrorCall err) -> return err)
    `shouldReturn` "success"

checkScanxStrictness :: IO ()
checkScanxStrictness = do
  let s = return (1 :: Int) `S.consM` error "failure"
  catch
    (S.drain (
        S.scanx (\_ a ->
                    if a == 1
                    then error "success"
                    else "done")
                "begin" id s
        )
        >> return "finished"
    )
    (\(ErrorCall err) -> return err)
    `shouldReturn` "success"

foldxMStrictCheck :: IORef Int -> SerialT IO Int -> IO ()
foldxMStrictCheck ref = S.foldxM (\_ _ -> writeIORef ref 1) (return ()) return

checkCleanupFold :: IsStream t
    => (t IO Int -> SerialT IO Int)
    -> (SerialT IO Int -> IO (Maybe Int))
    -> IO ()
checkCleanupFold t op = do
    r <- newIORef ([] :: [Int])
    _ <- op $ t $ delay r 0 S.|: delay r 2 S.|: delay r 3 S.|: S.nil
    performMajorGC
    -- In case the actions are not killed let them fire before we check so that
    -- the test fails.
    threadDelay (4 * delayUnit)
    res <- readIORef r
    res `shouldBe` [0]

    where

    delayUnit = 400000
    delay ref i =
        threadDelay (i * delayUnit) >> modifyIORef ref (i :) >> return i

testFoldOpsCleanup :: String -> (SerialT IO Int -> IO a) -> Spec
testFoldOpsCleanup name f = do
    let testOp op x = op x >> return Nothing
    it (name <> " asyncly") $ checkCleanupFold S.fromAsync (testOp f)
    it (name <> " wAsyncly") $ checkCleanupFold S.fromWAsync (testOp f)
    it (name <> " aheadly") $ checkCleanupFold S.fromAhead (testOp f)
#ifdef INCLUDE_FLAKY_TESTS
    it (name <> " parallely") $ checkCleanupFold S.fromParallel (testOp f)
#endif
#endif

checkFoldMStrictness :: (IORef Int -> SerialT IO Int -> IO ()) -> IO ()
checkFoldMStrictness f = do
  ref <- newIORef 0
  let s = return 1 `S.consM` error "x"
  catch (f ref s) (\(_ :: ErrorCall) -> return ())
  readIORef ref `shouldReturn` 1

checkFoldl'Strictness :: IO ()
checkFoldl'Strictness = do
  let s = return (1 :: Int) `S.consM` error "failure"
  catch (S.foldl' (\_ a -> if a == 1 then error "success" else "done")
                      "begin" s)
    (\(ErrorCall err) -> return err)
    `shouldReturn` "success"

checkScanl'Strictness :: IO ()
checkScanl'Strictness = do
    let s = return (1 :: Int) `S.consM` error "failure"
    catch
        (S.drain
             (S.scanl'
                  (\_ a ->
                       if a == 1
                           then error "success"
                           else "done")
                  "begin"
                  s)
             >> return "finished"
        )
        (\(ErrorCall err) -> return err)
        `shouldReturn` "success"

foldlM'StrictCheck :: IORef Int -> SerialT IO Int -> IO ()
foldlM'StrictCheck ref = S.foldlM' (\_ _ -> writeIORef ref 1) (return ())

scanlM'StrictCheck :: IORef Int -> SerialT IO Int -> SerialT IO ()
scanlM'StrictCheck ref = S.scanlM' (\_ _ -> writeIORef ref 1) (return ())

checkScanlMStrictness :: (IORef Int -> SerialT IO Int -> SerialT IO ()) -> IO ()
checkScanlMStrictness f = do
  ref <- newIORef 0
  let s = return 1 `S.consM` error "x"
  catch (S.drain $ f ref s) (\(_ :: ErrorCall) -> return ())
  readIORef ref `shouldReturn` 1

checkFoldrLaziness :: IO ()
checkFoldrLaziness = do
    S.foldrM (\x xs -> if odd x then return True else xs)
             (return False) (S.fromList (2:4:5:undefined :: [Int]))
        `shouldReturn` True

    S.toList (IS.foldrS (\x xs -> if odd x then return True else xs)
                        (return False)
                        $ (S.fromList (2:4:5:undefined) :: SerialT IO Int))
        `shouldReturn` [True]

    S.toList (IS.foldrT (\x xs -> if odd x then return True else xs)
                        (return False)
                        $ (S.fromList (2:4:5:undefined) :: SerialT IO Int))
        `shouldReturn` [True]

moduleName :: String
moduleName = "Prelude.Fold"

main :: IO ()
main = hspec
  $ H.parallel
#ifdef COVERAGE_BUILD
  $ modifyMaxSuccess (const 10)
#endif
  $ describe moduleName $ do

    ---------------------------------------------------------------------------
    -- Left folds are strict enough
    ---------------------------------------------------------------------------

#ifdef DEVBUILD
    it "foldx is strict enough" checkFoldxStrictness
    it "scanx is strict enough" checkScanxStrictness
    it "foldxM is strict enough" (checkFoldMStrictness foldxMStrictCheck)
#endif
    it "foldl' is strict enough" checkFoldl'Strictness
    it "scanl' is strict enough" checkScanl'Strictness
    it "foldlM' is strict enough" (checkFoldMStrictness foldlM'StrictCheck)
    it "scanlM' is strict enough" (checkScanlMStrictness scanlM'StrictCheck)

    ---------------------------------------------------------------------------
    -- Right folds are lazy enough
    ---------------------------------------------------------------------------

    it "foldrM is lazy enough" checkFoldrLaziness

#ifdef DEVBUILD
    testFoldOpsCleanup "head" S.head
    testFoldOpsCleanup "null" S.null
    testFoldOpsCleanup "elem" (S.elem 0)
    testFoldOpsCleanup "notElem" (S.notElem 0)
    testFoldOpsCleanup "elemIndex" (S.elemIndex 0)
    -- S.lookup
    testFoldOpsCleanup "notElem" (S.notElem 0)
    testFoldOpsCleanup "find" (S.find (==0))
    testFoldOpsCleanup "findIndex" (S.findIndex (==0))
    testFoldOpsCleanup "all" (S.all (==1))
    testFoldOpsCleanup "any" (S.any (==0))
    testFoldOpsCleanup "and" (S.and . S.map (==1))
    testFoldOpsCleanup "or" (S.or . S.map (==0))
#endif
