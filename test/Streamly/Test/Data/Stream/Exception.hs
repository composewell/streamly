-- |
-- Module      : Streamly.Test.Data.Stream.Exception
-- Copyright   : (c) 2020 Composewell Technologies
--
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

module Streamly.Test.Data.Stream.Exception (main) where

import Control.Exception (Exception, SomeException, try)
import Data.IORef (newIORef, readIORef, writeIORef)

import qualified Control.Monad.Catch as MC
import qualified Streamly.Internal.Control.Exception as Exception
import qualified Streamly.Internal.Data.Stream as Stream

import Test.Hspec

newtype ExampleException = ExampleException String
    deriving (Eq, Show, Ord)

instance Exception ExampleException

-------------------------------------------------------------------------------
-- afterUnsafe
-------------------------------------------------------------------------------

testAfterUnsafe :: Expectation
testAfterUnsafe = do
    ref <- newIORef (0 :: Int)
    xs <- Stream.toList $
            Stream.afterUnsafe (writeIORef ref 1)
                (Stream.fromList [1, 2, 3 :: Int])
    xs `shouldBe` [1, 2, 3]
    readIORef ref `shouldReturn` 1

testAfterUnsafeEmpty :: Expectation
testAfterUnsafeEmpty = do
    ref <- newIORef (0 :: Int)
    xs <- Stream.toList $
            Stream.afterUnsafe (writeIORef ref 1)
                (Stream.fromList ([] :: [Int]))
    xs `shouldBe` []
    readIORef ref `shouldReturn` 1

-------------------------------------------------------------------------------
-- finallyUnsafe
-------------------------------------------------------------------------------

testFinallyUnsafe :: Expectation
testFinallyUnsafe = do
    ref <- newIORef (0 :: Int)
    xs <- Stream.toList $
            Stream.finallyUnsafe (writeIORef ref 1)
                (Stream.fromList [1, 2, 3 :: Int])
    xs `shouldBe` [1, 2, 3]
    readIORef ref `shouldReturn` 1

testFinallyUnsafeException :: Expectation
testFinallyUnsafeException = do
    ref <- newIORef (0 :: Int)
    res <- try $ Stream.toList $
            Stream.finallyUnsafe (writeIORef ref 1)
                (Stream.fromEffect (MC.throwM (ExampleException "E")))
    res `shouldBe` (Left (ExampleException "E") :: Either ExampleException [Int])
    readIORef ref `shouldReturn` 1

-------------------------------------------------------------------------------
-- gbracket_
-------------------------------------------------------------------------------

testGbracket_ :: Expectation
testGbracket_ = do
    ref <- newIORef (0 :: Int)
    xs <- Stream.toList $
            Stream.gbracket_
                (return ())
                (\_ -> writeIORef ref 1)
                (\_ (e :: SomeException) _ -> MC.throwM e)
                MC.try
                (\_ -> Stream.fromList [1, 2, 3 :: Int])
    xs `shouldBe` [1, 2, 3]
    readIORef ref `shouldReturn` 1

testGbracket_Exception :: Expectation
testGbracket_Exception = do
    refAft <- newIORef (0 :: Int)
    refExc <- newIORef (0 :: Int)
    res <- try $ Stream.toList $
            Stream.gbracket_
                (return ())
                (\_ -> writeIORef refAft 1)
                (\_ (e :: ExampleException) _ ->
                    writeIORef refExc 1 >> MC.throwM e)
                MC.try
                (\_ -> Stream.fromEffect (MC.throwM (ExampleException "E")))
    res `shouldBe` (Left (ExampleException "E") :: Either ExampleException [Int])
    readIORef refAft `shouldReturn` 0
    readIORef refExc `shouldReturn` 1

-------------------------------------------------------------------------------
-- gbracket
-------------------------------------------------------------------------------

testGbracket :: Expectation
testGbracket = do
    ref <- newIORef (0 :: Int)
    xs <- Stream.toList $
            Stream.gbracket
                (return ())
                (\_ -> writeIORef ref 1)
                (\_ (e :: SomeException) _ -> MC.throwM e)
                (\_ -> return ())
                MC.try
                (\_ -> Stream.fromList [1, 2, 3 :: Int])
    xs `shouldBe` [1, 2, 3]
    readIORef ref `shouldReturn` 1

testGbracketException :: Expectation
testGbracketException = do
    refAft <- newIORef (0 :: Int)
    refExc <- newIORef (0 :: Int)
    res <- try $ Stream.toList $
            Stream.gbracket
                (return ())
                (\_ -> writeIORef refAft 1)
                (\_ (e :: ExampleException) _ ->
                    writeIORef refExc 1 >> MC.throwM e)
                (\_ -> return ())
                MC.try
                (\_ -> Stream.fromEffect (MC.throwM (ExampleException "E")))
    res `shouldBe` (Left (ExampleException "E") :: Either ExampleException [Int])
    readIORef refAft `shouldReturn` 0
    readIORef refExc `shouldReturn` 1

-------------------------------------------------------------------------------
-- bracketUnsafe
-------------------------------------------------------------------------------

testBracketUnsafe :: Expectation
testBracketUnsafe = do
    ref <- newIORef (0 :: Int)
    xs <- Stream.toList $
            Stream.bracketUnsafe
                (return ())
                (\_ -> writeIORef ref 1)
                (\_ -> Stream.fromList [1, 2, 3 :: Int])
    xs `shouldBe` [1, 2, 3]
    readIORef ref `shouldReturn` 1

testBracketUnsafeException :: Expectation
testBracketUnsafeException = do
    ref <- newIORef (0 :: Int)
    res <- try $ Stream.toList $
            Stream.bracketUnsafe
                (return ())
                (\_ -> writeIORef ref 1)
                (\_ -> Stream.fromEffect (MC.throwM (ExampleException "E")))
    res `shouldBe` (Left (ExampleException "E") :: Either ExampleException [Int])
    readIORef ref `shouldReturn` 1

-------------------------------------------------------------------------------
-- bracketIO3
-------------------------------------------------------------------------------

testBracketIO3Stop :: Expectation
testBracketIO3Stop = do
    refStop <- newIORef (0 :: Int)
    refExc  <- newIORef (0 :: Int)
    xs <- Stream.toList $
            Stream.bracketIO3
                (return ())
                (\_ -> writeIORef refStop 1)
                (\_ -> writeIORef refExc 1)
                (\_ -> return ())
                (\_ -> Stream.fromList [1, 2, 3 :: Int])
    xs `shouldBe` [1, 2, 3]
    readIORef refStop `shouldReturn` 1
    readIORef refExc  `shouldReturn` 0

testBracketIO3Exception :: Expectation
testBracketIO3Exception = do
    refStop <- newIORef (0 :: Int)
    refExc  <- newIORef (0 :: Int)
    res <- try $ Stream.toList $
            Stream.bracketIO3
                (return ())
                (\_ -> writeIORef refStop 1)
                (\_ -> writeIORef refExc 1)
                (\_ -> return ())
                (\_ -> Stream.fromEffect (MC.throwM (ExampleException "E")))
    res `shouldBe` (Left (ExampleException "E") :: Either ExampleException [Int])
    readIORef refStop `shouldReturn` 0
    readIORef refExc  `shouldReturn` 1

-------------------------------------------------------------------------------
-- bracketIO' and bracketIO'' (via Exception.withAcquireIO)
-------------------------------------------------------------------------------

testBracketIO' :: Expectation
testBracketIO' = do
    ref <- newIORef (0 :: Int)
    xs <- Exception.withAcquireIO $ \aref ->
            Stream.toList $
                Stream.bracketIO' aref
                    (return ())
                    (\_ -> writeIORef ref 1)
                    (\_ -> Stream.fromList [1, 2, 3 :: Int])
    xs `shouldBe` [1, 2, 3]
    readIORef ref `shouldReturn` 1

testBracketIO'' :: Expectation
testBracketIO'' = do
    ref <- newIORef (0 :: Int)
    xs <- Exception.withAcquireIO $ \aref ->
            Stream.toList $
                Stream.bracketIO'' aref
                    (return ())
                    (\_ -> writeIORef ref 1)
                    (\_ -> Stream.fromList [1, 2, 3 :: Int])
    xs `shouldBe` [1, 2, 3]
    readIORef ref `shouldReturn` 1

testBracketIO''Exception :: Expectation
testBracketIO''Exception = do
    ref <- newIORef (0 :: Int)
    res <- try $ Exception.withAcquireIO $ \aref ->
            Stream.toList $
                Stream.bracketIO'' aref
                    (return ())
                    (\_ -> writeIORef ref 1)
                    (\_ -> Stream.fromEffect (MC.throwM (ExampleException "E")))
    res `shouldBe` (Left (ExampleException "E") :: Either ExampleException [Int])
    readIORef ref `shouldReturn` 1

-------------------------------------------------------------------------------
-- finallyIO' and finallyIO'' (via Exception.withAcquireIO)
-------------------------------------------------------------------------------

testFinallyIO' :: Expectation
testFinallyIO' = do
    ref <- newIORef (0 :: Int)
    xs <- Exception.withAcquireIO $ \aref ->
            Stream.toList $
                Stream.finallyIO' aref
                    (writeIORef ref 1)
                    (Stream.fromList [1, 2, 3 :: Int])
    xs `shouldBe` [1, 2, 3]
    readIORef ref `shouldReturn` 1

testFinallyIO'' :: Expectation
testFinallyIO'' = do
    ref <- newIORef (0 :: Int)
    xs <- Exception.withAcquireIO $ \aref ->
            Stream.toList $
                Stream.finallyIO'' aref
                    (writeIORef ref 1)
                    (Stream.fromList [1, 2, 3 :: Int])
    xs `shouldBe` [1, 2, 3]
    readIORef ref `shouldReturn` 1

testFinallyIO''Exception :: Expectation
testFinallyIO''Exception = do
    ref <- newIORef (0 :: Int)
    res <- try $ Exception.withAcquireIO $ \aref ->
            Stream.toList $
                Stream.finallyIO'' aref
                    (writeIORef ref 1)
                    (Stream.fromEffect (MC.throwM (ExampleException "E")))
    res `shouldBe` (Left (ExampleException "E") :: Either ExampleException [Int])
    readIORef ref `shouldReturn` 1

-------------------------------------------------------------------------------
-- withAcquireIO (Stream version)
-------------------------------------------------------------------------------

testWithAcquireIO :: Expectation
testWithAcquireIO = do
    ref <- newIORef (0 :: Int)
    xs <- Stream.toList $
            Stream.withAcquireIO $ \aref ->
                Stream.bracketIO' aref
                    (return ())
                    (\_ -> writeIORef ref 1)
                    (\_ -> Stream.fromList [1, 2, 3 :: Int])
    xs `shouldBe` [1, 2, 3]
    readIORef ref `shouldReturn` 1

-------------------------------------------------------------------------------
-- ghandle
-------------------------------------------------------------------------------

testGhandle :: Expectation
testGhandle = do
    xs <- Stream.toList $
            Stream.ghandle
                (\(_ :: ExampleException) _rest ->
                    return (Stream.fromList [10, 20 :: Int]))
                (Stream.append
                    (Stream.fromList [1, 2 :: Int])
                    (Stream.fromEffect (MC.throwM (ExampleException "E"))))
    xs `shouldBe` [1, 2, 10, 20]

testGhandleRethrow :: Expectation
testGhandleRethrow = do
    res <- try $ Stream.toList $
            Stream.ghandle
                (\(e :: ExampleException) _rest -> MC.throwM e)
                (Stream.fromEffect (MC.throwM (ExampleException "E")))
    res `shouldBe` (Left (ExampleException "E") :: Either ExampleException [Int])

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

moduleName :: String
moduleName = "Data.Stream.Exception"

main :: IO ()
main = hspec $ describe moduleName $ do
    describe "afterUnsafe" $ do
        it "runs after stream" testAfterUnsafe
        it "runs after empty stream" testAfterUnsafeEmpty

    describe "finallyUnsafe" $ do
        it "runs after normal stream" testFinallyUnsafe
        it "runs after exception" testFinallyUnsafeException

    describe "gbracket_" $ do
        it "runs aft on normal stop" testGbracket_
        it "runs onExc on exception" testGbracket_Exception

    describe "gbracket" $ do
        it "runs aft on normal stop" testGbracket
        it "runs onExc on exception" testGbracketException

    describe "bracketUnsafe" $ do
        it "runs cleanup on normal stop" testBracketUnsafe
        it "runs cleanup on exception" testBracketUnsafeException

    describe "bracketIO3" $ do
        it "runs onStop on normal stop" testBracketIO3Stop
        it "runs onException on exception" testBracketIO3Exception

    describe "bracketIO'" $ do
        it "runs cleanup on normal stop" testBracketIO'

    describe "bracketIO''" $ do
        it "runs cleanup on normal stop" testBracketIO''
        it "runs cleanup on exception" testBracketIO''Exception

    describe "finallyIO'" $ do
        it "runs after normal stream" testFinallyIO'

    describe "finallyIO''" $ do
        it "runs after normal stream" testFinallyIO''
        it "runs after exception" testFinallyIO''Exception

    describe "withAcquireIO" $ do
        it "runs cleanup after stream" testWithAcquireIO

    describe "ghandle" $ do
        it "handler recovers from exception" testGhandle
        it "handler can rethrow" testGhandleRethrow
