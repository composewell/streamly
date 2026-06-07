-- |
-- Module      : Streamly.Test.Data.Stream.Transform
-- Copyright   : (c) 2019 Composewell technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

module Streamly.Test.Data.Stream.Transform (main) where

import Control.Exception (ErrorCall(..), catch)
import Data.IORef (newIORef, readIORef, writeIORef, IORef)
import Streamly.Internal.Data.Stream (Stream)
import qualified Streamly.Internal.Data.Stream as Stream

import Test.Hspec as H
#ifdef DEVBUILD
#endif

checkScanlMStrictness :: (IORef Int -> Stream IO Int -> Stream IO ()) -> IO ()
checkScanlMStrictness f = do
  ref <- newIORef 0
  let s = Stream.fromList ((1 :: Int) : error "x")
  catch (Stream.drain $ f ref s) (\(_ :: ErrorCall) -> return ())
  readIORef ref `shouldReturn` 1

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

scanlM'StrictCheck :: IORef Int -> Stream IO Int -> Stream IO ()
scanlM'StrictCheck ref = Stream.scanlM' (\_ _ -> writeIORef ref 1) (return ())

#ifdef DEVBUILD
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
#endif

moduleName :: String
moduleName = "Data.Stream.Transform"

main :: IO ()
main = hspec
  $ H.parallel
  $ describe moduleName $ do
#ifdef DEVBUILD
    it "scanx is strict enough" checkScanxStrictness
#endif
    it "scanl' is strict enough" checkScanl'Strictness
    it "scanlM' is strict enough" (checkScanlMStrictness scanlM'StrictCheck)
