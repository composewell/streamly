{-# OPTIONS_GHC -Wno-deprecations #-}

-- |
-- Module      : Streamly.Test.Data.Stream.Fold
-- Copyright   : (c) 2020 Composewell Technologies
--
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

module Streamly.Test.Data.Stream.Fold (main) where

import Control.Exception (ErrorCall(..), catch)
import Data.IORef (newIORef, readIORef, writeIORef, IORef)
#ifdef COVERAGE_BUILD
import Test.Hspec.QuickCheck (modifyMaxSuccess)
#endif
import Test.Hspec as H
#ifdef DEVBUILD
#endif

import Control.Monad.Trans.Identity (runIdentityT)
import Streamly.Internal.Data.Stream (Stream)
import qualified Streamly.Internal.Data.Stream as Stream

#ifdef DEVBUILD
checkFoldxStrictness :: IO ()
checkFoldxStrictness = do
  -- Note: consM is strict which causes the tests to fail
  -- This should work with StreamK though.
  -- let s = return (1 :: Int) `Stream.consM` error "failure"
  let s = Stream.fromList ((1 :: Int) : error "failure")
  catch (Stream.foldlx' (\_ a -> if a == 1 then error "success" else "done")
                      "begin" id s)
    (\(ErrorCall err) -> return err)
    `shouldReturn` "success"

checkScanxStrictness :: IO ()
checkScanxStrictness = do
  let s = Stream.fromList ((1 :: Int) : error "failure")
  catch
    (Stream.drain (
        Stream.scanlx' (\_ a ->
                    if a == 1
                    then error "success"
                    else "done")
                "begin" id s
        )
        >> return "finished"
    )
    (\(ErrorCall err) -> return err)
    `shouldReturn` "success"

foldxMStrictCheck :: IORef Int -> Stream IO Int -> IO ()
foldxMStrictCheck ref = Stream.foldlMx' (\_ _ -> writeIORef ref 1) (return ()) return

#endif

checkFoldMStrictness :: (IORef Int -> Stream IO Int -> IO ()) -> IO ()
checkFoldMStrictness f = do
  ref <- newIORef 0
  let s = Stream.fromList ((1 :: Int) : error "x")
  catch (f ref s) (\(_ :: ErrorCall) -> return ())
  readIORef ref `shouldReturn` 1

checkFoldl'Strictness :: IO ()
checkFoldl'Strictness = do
  let s = Stream.fromList ((1 :: Int) : error "failure")
  catch (Stream.foldl' (\_ a -> if a == 1 then error "success" else "done")
                      "begin" s)
    (\(ErrorCall err) -> return err)
    `shouldReturn` "success"

checkScanl'Strictness :: IO ()
checkScanl'Strictness = do
    let s = Stream.fromList ((1 :: Int) : error "failure")
    catch
        (Stream.drain
             (Stream.scanl'
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

foldlM'StrictCheck :: IORef Int -> Stream IO Int -> IO ()
foldlM'StrictCheck ref = Stream.foldlM' (\_ _ -> writeIORef ref 1) (return ())

scanlM'StrictCheck :: IORef Int -> Stream IO Int -> Stream IO ()
scanlM'StrictCheck ref = Stream.scanlM' (\_ _ -> writeIORef ref 1) (return ())

checkScanlMStrictness :: (IORef Int -> Stream IO Int -> Stream IO ()) -> IO ()
checkScanlMStrictness f = do
  ref <- newIORef 0
  let s = Stream.fromList ((1 :: Int) : error "x")
  catch (Stream.drain $ f ref s) (\(_ :: ErrorCall) -> return ())
  readIORef ref `shouldReturn` 1

checkFoldrLaziness :: IO ()
checkFoldrLaziness = do
    Stream.foldrM (\x xs -> if odd x then return True else xs)
             (return False) (Stream.fromList (2:4:5:undefined :: [Int]))
        `shouldReturn` True

    Stream.toList (Stream.foldrS (\x xs -> if odd x then Stream.fromPure True else xs)
                        (Stream.fromPure False)
                        (Stream.fromList (2:4:5:undefined) :: Stream IO Int))
        `shouldReturn` [True]

    runIdentityT (Stream.foldrT (\x xs -> if odd x then return True else xs)
                        (return False)
                        (Stream.fromList (2:4:5:undefined) :: Stream IO Int))
        `shouldReturn` True

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
