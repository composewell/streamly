module Main (main) where

import Control.Applicative ((<|>), empty)
import Control.Monad (mzero)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (threadDelay)
import Data.List (sort)
import Data.Monoid ((<>))
import Prelude hiding (take, drop)
import Test.Hspec

import Asyncly

main :: IO ()
main = hspec $ do
    describe "Runners" $ do
        it "simple runAsyncly" $
            runAsyncly (return (0 :: Int)) `shouldReturn` ()
        it "simple runAsyncly with IO" $
            runAsyncly (liftIO $ putStrLn "hello") `shouldReturn` ()
        it "Captures a return value using toList" $
            toList (return 0) `shouldReturn` ([0] :: [Int])

    describe "Empty" $ do
        it "Monoid - mempty" $
            (toList mempty) `shouldReturn` ([] :: [Int])
        it "Alternative - empty" $
            (toList empty) `shouldReturn` ([] :: [Int])
        it "MonadPlus - mzero" $
            (toList mzero) `shouldReturn` ([] :: [Int])

    describe "Bind" bind

    describe "Serial Composition (<>)" $ compose (<>) id
    describe "Serial interleaved (<=>)" $ interleaved (<=>)
    describe "Left biased parallel Composition (<|)" $ compose (<|) sort
    describe "Fair parallel Composition (<|>)" $ compose (<|>) sort
    describe "Left biased parallel time order check" $ parallelCheck (<|)
    describe "Fair parallel time order check" $ parallelCheck (<|>)
    -- This is not predicatable
    -- describe "Parallel interleaved (<|>)" $ interleaved (<|>)

    describe "Serial loops (<>)" $ loops (<>) id reverse
    describe "Left biased parallel loops (<|)" $ loops (<|) sort sort
    describe "Fair parallel loops (<|>)" $ loops (<|>) sort sort

    describe "Transformation" $ transformOps (<>)
    describe "Miscellaneouse combined examples" mixedOps

bind :: Spec
bind = do
    it "Simple runAsyncly and 'then' with IO" $
        runAsyncly (liftIO (putStrLn "hello") >> liftIO (putStrLn "world"))
            `shouldReturn` ()
    it "Then and toList" $
        toList (return (1 :: Int) >> return 2) `shouldReturn` ([2] :: [Int])
    it "Bind and toList" $
        toList (do x <- return 1; y <- return 2; return (x + y))
            `shouldReturn` ([3] :: [Int])

interleaved :: (AsyncT IO Int -> AsyncT IO Int -> AsyncT IO Int) -> Spec
interleaved f =
    it "Interleave four" $
        toList ((return 0 <> return 1) `f` (return 100 <> return 101))
            `shouldReturn` ([0, 100, 1, 101])

parallelCheck :: (AsyncT IO Int -> AsyncT IO Int -> AsyncT IO Int) -> Spec
parallelCheck f = do
    it "Parallel ordering left associated" $
        toList (((event 4 `f` event 3) `f` event 2) `f` event 1)
            `shouldReturn` ([1..4])

    it "Parallel ordering right associated" $
        toList (event 4 `f` (event 3 `f` (event 2 `f` event 1)))
            `shouldReturn` ([1..4])

    where event n = (liftIO $ threadDelay (n * 100000)) >> (return n)

compose
    :: (AsyncT IO Int -> AsyncT IO Int -> AsyncT IO Int)
    -> ([Int] -> [Int])
    -> Spec
compose f srt = do
    it "Compose mempty, mempty" $
        (toList (mempty `f` mempty)) `shouldReturn` []
    it "Compose empty, empty" $
        (toList (empty `f` empty)) `shouldReturn` []
    it "Compose empty at the beginning" $
        (toList $ (empty `f` return 1)) `shouldReturn` [1]
    it "Compose empty at the end" $
        (toList $ (return 1 `f` empty)) `shouldReturn` [1]
    it "Compose two" $
        (toList (return 0 `f` return 1) >>= return . srt)
            `shouldReturn` [0, 1]
    it "Compose three - empty in the middle" $
        ((toList $ (return 0 `f` empty `f` return 1)) >>= return . srt)
            `shouldReturn` [0, 1]
    it "Compose left associated" $
        ((toList $ (((return 0 `f` return 1) `f` return 2) `f` return 3))
            >>= return . srt) `shouldReturn` [0, 1, 2, 3]
    it "Compose right associated" $
        ((toList $ (return 0 `f` (return 1 `f` (return 2 `f` return 3))))
            >>= return . srt) `shouldReturn` [0, 1, 2, 3]
    it "Compose many" $
        ((toList $ forEachWith f [1..100] return) >>= return . srt)
            `shouldReturn` [1..100]
    it "Compose many (right fold) with bind" $
        (toList (forEachWith f [1..10 :: Int] $ \x -> return x >>= return . id)
            >>= return . srt) `shouldReturn` [1..10]
    it "Compose many (left fold) with bind" $
        let forL xs k = foldl f empty $ map k xs
         in (toList (forL [1..10 :: Int] $ \x -> return x >>= return . id)
                >>= return . srt) `shouldReturn` [1..10]
    it "Compose hierarchical (multiple levels)" $
        ((toList $ (((return 0 `f` return 1) `f` (return 2 `f` return 3))
                `f` ((return 4 `f` return 5) `f` (return 6 `f` return 7)))
            ) >>= return . srt) `shouldReturn` [0..7]

loops
    :: (AsyncT IO Int -> AsyncT IO Int -> AsyncT IO Int)
    -> ([Int] -> [Int])
    -> ([Int] -> [Int])
    -> Spec
loops f tsrt hsrt = do
    it "Tail recursive loop" $ (toList (loopTail 0) >>= return . tsrt)
            `shouldReturn` [0..3]

    it "Head recursive loop" $ (toList (loopHead 0) >>= return . hsrt)
            `shouldReturn` [0..3]

    where
        loopHead x = do
            -- this print line is important for the test (causes a bind)
            liftIO $ putStrLn "LoopHead..."
            (if x < 3 then loopHead (x + 1) else empty) `f` return x

        loopTail x = do
            -- this print line is important for the test (causes a bind)
            liftIO $ putStrLn "LoopTail..."
            return x `f` (if x < 3 then loopTail (x + 1) else empty)

transformOps :: (AsyncT IO Int -> AsyncT IO Int -> AsyncT IO Int) -> Spec
transformOps f = do
    it "take all" $
        (toList $ take 10 $ foldMapWith f return [1..10])
            `shouldReturn` [1..10]
    it "take none" $
        (toList $ take 0 $ foldMapWith f return [1..10])
            `shouldReturn` []
    it "take 5" $
        (toList $ take 5 $ foldMapWith f return [1..10])
            `shouldReturn` [1..5]

    it "drop all" $
        (toList $ drop 10 $ foldMapWith f return [1..10])
            `shouldReturn` []
    it "drop none" $
        (toList $ drop 0 $ foldMapWith f return [1..10])
            `shouldReturn` [1..10]
    it "drop 5" $
        (toList $ drop 5 $ foldMapWith f return [1..10])
            `shouldReturn` [6..10]

mixedOps :: Spec
mixedOps = do
    it "Compose many ops" $
        (toList composeMixed >>= return . sort)
            `shouldReturn` ([8,9,9,9,9,9,10,10,10,10,10,10,10,10,10,10,11,11
                            ,11,11,11,11,11,11,11,11,12,12,12,12,12,13
                            ] :: [Int])

composeMixed :: AsyncT IO Int
composeMixed = do
    liftIO $ return ()
    liftIO $ putStr ""
    x <- return 1
    y <- return 2
    z <- do
            x1 <- return 1 <|> return 2
            liftIO $ return ()
            liftIO $ putStr ""
            y1 <- return 1 <| return 2
            z1 <- do
                x11 <- return 1 <> return 2
                y11 <- return 1 <| return 2
                z11 <- return 1 <=> return 2
                liftIO $ return ()
                liftIO $ putStr ""
                return (x11 + y11 + z11)
            return (x1 + y1 + z1)
    return (x + y + z)
