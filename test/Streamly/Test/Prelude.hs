-- |
-- Module      : Streamly.Test.Prelude
-- Copyright   : (c) 2020 Composewell Technologies
--
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

module Streamly.Test.Prelude (main) where

import Control.Concurrent (threadDelay)
import Control.Exception (Exception, try)
import Control.Monad (when)
import Control.Monad.Catch (throwM)
import Control.Monad.Error.Class (throwError, MonadError)
import Control.Monad.Trans.Except (runExceptT, ExceptT)
import Data.Function ((&))
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Int (Int64)
import Data.List (sort)
import Data.Maybe (fromJust, isJust)
#if !(MIN_VERSION_base(4,11,0))
import Data.Semigroup (Semigroup(..))
#endif
import Test.Hspec as H
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Property, choose)
import Test.QuickCheck.Monadic (monadicIO, pick)

import Streamly.Prelude (SerialT, IsStream)
import Streamly.Internal.Data.Time.Units
       (AbsTime, NanoSecond64(..), toRelTime64, diffAbsTime64)
import Streamly.Internal.Data.Time.Clock (Clock(Monotonic), getTime)

import qualified Streamly.Internal.Data.Fold as FL
import qualified Streamly.Internal.Data.Unfold as UF
import qualified Streamly.Internal.Data.Stream.IsStream as IS
import qualified Streamly.Prelude as S

toListSerial :: SerialT IO a -> IO [a]
toListSerial = S.toList . S.serially

max_length :: Int
max_length = 1000

tenPow8 :: Int64
tenPow8 = 10^(8 :: Int)

tenPow7 :: Int64
tenPow7 = 10^(7 :: Int)

takeDropTime :: NanoSecond64
takeDropTime = NanoSecond64 $ 5 * tenPow8

checkTakeDropTime :: (Maybe AbsTime, Maybe AbsTime) -> IO Bool
checkTakeDropTime (mt0, mt1) = do
    let graceTime = NanoSecond64 $ 8 * tenPow7
    case mt0 of
        Nothing -> return True
        Just t0 ->
            case mt1 of
                Nothing -> return True
                Just t1 -> do
                    let tMax = toRelTime64 (takeDropTime + graceTime)
                    let tMin = toRelTime64 (takeDropTime - graceTime)
                    let t = diffAbsTime64 t1 t0
                    let r = t >= tMin && t <= tMax
                    when (not r) $ putStrLn $
                        "t = " ++ show t ++
                        " tMin = " ++ show tMin ++
                        " tMax = " ++ show tMax
                    return r

testTakeByTime :: IO Bool
testTakeByTime = do
    r <-
          S.fold ((,) <$> FL.head <*> FL.last)
        $ IS.takeByTime takeDropTime
        $ S.repeatM (threadDelay 1000 >> getTime Monotonic)
    checkTakeDropTime r

testDropByTime :: IO Bool
testDropByTime = do
    t0 <- getTime Monotonic
    mt1 <-
          S.head
        $ IS.dropByTime takeDropTime
        $ S.repeatM (threadDelay 1000 >> getTime Monotonic)
    checkTakeDropTime (Just t0, mt1)

unfold :: Property
unfold = monadicIO $ do
    a <- pick $ choose (0, max_length `div` 2)
    b <- pick $ choose (0, max_length)
    let unf = UF.enumerateFromToIntegral b
    ls <- S.toList $ S.unfold unf a
    return $ ls == [a..b]

unfold0 :: Property
unfold0 = monadicIO $ do
    a <- pick $ choose (0, max_length `div` 2)
    b <- pick $ choose (0, max_length)
    let unf = UF.supply (UF.enumerateFromToIntegral b) a
    ls <- S.toList $ IS.unfold0 unf
    return $ ls == [a..b]

testFromCallback :: IO Int
testFromCallback = do
    ref <- newIORef Nothing
    let stream = S.map Just (IS.fromCallback (setCallback ref))
                    `S.parallel` runCallback ref
    S.sum $ S.map fromJust $ S.takeWhile isJust stream

    where

    setCallback ref cb = do
        writeIORef ref (Just cb)

    runCallback ref = S.yieldM $ do
        cb <-
              S.repeatM (readIORef ref)
                & IS.delayPost 0.1
                & S.mapMaybe id
                & S.head

        S.fromList [1..100]
            & IS.delayPost 0.001
            & S.mapM_ (fromJust cb)
        threadDelay 100000
        return Nothing

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
        S.yieldM $ return ()
        S.yieldM $ putStr ""
        let x = 1
        let y = 2
        z <- do
                x1 <- S.wAsyncly $ return 1 <> return 2
                S.yieldM $ return ()
                S.yieldM $ putStr ""
                y1 <- S.asyncly $ return 1 <> return 2
                z1 <- do
                    x11 <- return 1 <> return 2
                    y11 <- S.asyncly $ return 1 <> return 2
                    z11 <- S.wSerially $ return 1 <> return 2
                    S.yieldM $ return ()
                    S.yieldM $ putStr ""
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
        S.yieldM $ return ()
        S.yieldM $ putStr ""
        let x = 1
        let y = 2
        z <- do
                x1 <- S.wAsyncly $ return 1 <> return 2
                S.yieldM $ return ()
                S.yieldM $ putStr ""
                y1 <- S.aheadly $ return 1 <> return 2
                z1 <- do
                    x11 <- return 1 <> return 2
                    y11 <- S.aheadly $ return 1 <> return 2
                    z11 <- S.parallely $ return 1 <> return 2
                    S.yieldM $ return ()
                    S.yieldM $ putStr ""
                    return (x11 + y11 + z11)
                return (x1 + y1 + z1)
        return (x + y + z)

main :: IO ()
main = hspec $ H.parallel $ do
    describe "Filtering" $ do
        it "takeByTime" (testTakeByTime `shouldReturn` True)
        it "dropByTime" (testDropByTime `shouldReturn` True)

    describe "From Generators" $ do
        prop "unfold" unfold
        prop "unfold0" unfold0

    it "fromCallback" $ testFromCallback `shouldReturn` (50*101)

    describe "Miscellaneous combined examples" mixedOps
    describe "Miscellaneous combined examples aheadly" mixedOpsAheadly
    describe "Simple MonadError and MonadThrow" simpleMonadError

    {-
    describe "Composed MonadError serially" $ composeWithMonadError serially
    describe "Composed MonadError wSerially" $ composeWithMonadError wSerially
    describe "Composed MonadError asyncly" $ composeWithMonadError asyncly
    describe "Composed MonadError wAsyncly" $ composeWithMonadError wAsyncly
    -}