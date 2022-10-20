{-# OPTIONS_GHC -Wno-deprecations #-}

-- |
-- Module      : Streamly.Test.Prelude
-- Copyright   : (c) 2020 Composewell Technologies
--
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

module Streamly.Test.Prelude (main) where

import Control.Concurrent (myThreadId, threadDelay)
import Control.Exception (Exception, try)
import Control.Monad.Catch (throwM)
import Control.Monad.Error.Class (throwError, MonadError)
import Control.Monad.Trans.Except (runExceptT, ExceptT)
import Data.List (sort)
import System.IO (stdout, hSetBuffering, BufferMode(LineBuffering))
import System.Random (randomIO)
import Test.Hspec as H

import Streamly.Prelude (SerialT, IsStream)

import qualified Streamly.Prelude as S

toListSerial :: SerialT IO a -> IO [a]
toListSerial = S.toList . S.fromSerial

-- XXX need to test that we have promptly cleaned up everything after the error
-- XXX We can also check the output that we are expected to get before the
-- error occurs.

newtype ExampleException = ExampleException String deriving (Eq, Show)

instance Exception ExampleException

simpleMonadError :: Spec
simpleMonadError = do
{-
    it "simple runExceptT" $ do
        (runExceptT $ S.drain $ return ())
        `shouldReturn` (Right () :: Either String ())
    it "simple runExceptT with error" $ do
        (runExceptT $ S.drain $ throwError "E") `shouldReturn` Left "E"
        -}
    it "simple try" $
        try (S.drain $ return ())
        `shouldReturn` (Right () :: Either ExampleException ())
    it "simple try with throw error" $
        try (S.drain $ throwM $ ExampleException "E")
        `shouldReturn` (Left (ExampleException "E") :: Either ExampleException ())

_composeWithMonadError
    :: ( IsStream t
       , Semigroup (t (ExceptT String IO) Int)
       , MonadError String (t (ExceptT String IO))
       )
    => (t (ExceptT String IO) Int -> SerialT (ExceptT String IO) Int) -> Spec
_composeWithMonadError t = do
    let tl = S.toList . t
    it "Compose throwError, nil" $
        runExceptT (tl (throwError "E" <> S.nil)) `shouldReturn` Left "E"
    it "Compose nil, error" $
        runExceptT (tl (S.nil <> throwError "E")) `shouldReturn` Left "E"

mixedOps :: Spec
mixedOps =
    it "Compose many ops" $
        (sort <$> toListSerial composeMixed)
            `shouldReturn` ([8,9,9,9,9,9,10,10,10,10,10,10,10,10,10,10,11,11
                            ,11,11,11,11,11,11,11,11,12,12,12,12,12,13
                            ] :: [Int])
    where

    composeMixed :: SerialT IO Int
    composeMixed = do
        S.fromEffect $ return ()
        S.fromEffect $ putStr ""
        let x = 1
        let y = 2
        z <- do
                x1 <- S.fromWAsync $ return 1 <> return 2
                S.fromEffect $ return ()
                S.fromEffect $ putStr ""
                y1 <- S.fromAsync $ return 1 <> return 2
                z1 <- do
                    x11 <- return 1 <> return 2
                    y11 <- S.fromAsync $ return 1 <> return 2
                    z11 <- S.fromWSerial $ return 1 <> return 2
                    S.fromEffect $ return ()
                    S.fromEffect $ putStr ""
                    return (x11 + y11 + z11)
                return (x1 + y1 + z1)
        return (x + y + z)

mixedOpsAheadly :: Spec
mixedOpsAheadly =
    it "Compose many ops" $
        (sort <$> toListSerial composeMixed)
            `shouldReturn` ([8,9,9,9,9,9,10,10,10,10,10,10,10,10,10,10,11,11
                            ,11,11,11,11,11,11,11,11,12,12,12,12,12,13
                            ] :: [Int])
    where

    composeMixed :: SerialT IO Int
    composeMixed = do
        S.fromEffect $ return ()
        S.fromEffect $ putStr ""
        let x = 1
        let y = 2
        z <- do
                x1 <- S.fromWAsync $ return 1 <> return 2
                S.fromEffect $ return ()
                S.fromEffect $ putStr ""
                y1 <- S.fromAhead $ return 1 <> return 2
                z1 <- do
                    x11 <- return 1 <> return 2
                    y11 <- S.fromAhead $ return 1 <> return 2
                    z11 <- S.fromParallel $ return 1 <> return 2
                    S.fromEffect $ return ()
                    S.fromEffect $ putStr ""
                    return (x11 + y11 + z11)
                return (x1 + y1 + z1)
        return (x + y + z)

-- XXX Merge both the loops.
nestedLoops :: IO ()
nestedLoops = S.drain $ do
    S.fromEffect $ hSetBuffering stdout LineBuffering
    x <- loop "A " 2
    y <- loop "B " 2
    S.fromEffect $ myThreadId >>= putStr . show
             >> putStr " "
             >> print (x, y)

    where

    -- we can just use
    -- fromParallel $ mconcat $ replicate n $ fromEffect (...)
    loop :: String -> Int -> SerialT IO String
    loop name n = do
        rnd <- S.fromEffect (randomIO :: IO Int)
        let result = name <> show rnd
            repeatIt = if n > 1 then loop name (n - 1) else S.nil
         in return result `S.wAsync` repeatIt

parallelLoops :: IO ()
parallelLoops = do
    hSetBuffering stdout LineBuffering
    S.drain $ do
        x <- S.take 10 $ loop "A" `S.parallel` loop "B"
        S.fromEffect $ myThreadId >>= putStr . show
               >> putStr " got "
               >> print x

    where

    -- we can just use
    -- fromParallel $ cycle1 $ fromEffect (...)
    loop :: String -> SerialT IO (String, Int)
    loop name = do
        S.fromEffect $ threadDelay 1000000
        rnd <- S.fromEffect (randomIO :: IO Int)
        S.fromEffect $ myThreadId >>= putStr . show
               >> putStr " yielding "
               >> print rnd
        return (name, rnd) `S.parallel` loop name

moduleName :: String
moduleName = "Prelude"

main :: IO ()
main = hspec $ H.parallel $ do
  describe moduleName $ do
    describe "Miscellaneous combined examples" mixedOps
    describe "Miscellaneous combined examples fromAhead" mixedOpsAheadly
    describe "Simple MonadError and MonadThrow" simpleMonadError

    it "Nested loops" nestedLoops
    it "Parallel loops" parallelLoops
    {-
    describe "Composed MonadError fromSerial" $ composeWithMonadError fromSerial
    describe "Composed MonadError fromWSerial" $ composeWithMonadError fromWSerial
    describe "Composed MonadError fromAsync" $ composeWithMonadError fromAsync
    describe "Composed MonadError fromWAsync" $ composeWithMonadError fromWAsync
    -}
