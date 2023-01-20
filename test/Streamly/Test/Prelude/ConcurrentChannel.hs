-- |
-- Module      : Streamly.Test.Prelude.ConcurrentChannel
-- Copyright   : (c) 2020 Composewell Technologies
--
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

{-# LANGUAGE OverloadedLists #-}

module Streamly.Test.Prelude.ConcurrentChannel (main) where

import Control.Concurrent (MVar, takeMVar, threadDelay, putMVar, newEmptyMVar, tryTakeMVar, tryPutMVar)
import Control.Exception
       (BlockedIndefinitelyOnMVar(..), catches,
        BlockedIndefinitelyOnSTM(..), Handler(..))
import Control.Monad (void, when, forM_, replicateM_)
import Control.Monad.State (get, modify, StateT(..), evalStateT)
import Data.IORef (readIORef, modifyIORef, newIORef)
import GHC.Word (Word8)
import Test.Hspec.QuickCheck
import Test.Hspec as H
import Test.QuickCheck
       (Property, withMaxSuccess)
import Test.QuickCheck.Monadic (monadicIO, run)
import Streamly.Internal.Data.Stream (Stream)
import Streamly.Internal.Control.Concurrent (MonadAsync)

import qualified Streamly.Data.Fold as Fold
import qualified Streamly.Internal.Data.Stream as S
import qualified Streamly.Internal.Data.Stream.StreamD as D (foldlM')
import qualified Streamly.Internal.Data.Stream.Concurrent as Concur

import Streamly.Test.Common
import Streamly.Test.Prelude.Common

-------------------------------------------------------------------------------
-- Concurrent generation
-------------------------------------------------------------------------------

mvarExcHandler :: String -> BlockedIndefinitelyOnMVar -> IO ()
mvarExcHandler label BlockedIndefinitelyOnMVar =
    error $ label <> " " <> "BlockedIndefinitelyOnMVar\n"

stmExcHandler :: String -> BlockedIndefinitelyOnSTM -> IO ()
stmExcHandler label BlockedIndefinitelyOnSTM =
    error $ label <> " " <> "BlockedIndefinitelyOnSTM\n"

dbgMVar :: String -> IO () -> IO ()
dbgMVar label action =
    action `catches` [ Handler (mvarExcHandler label)
                     , Handler (stmExcHandler label)
                     ]

-- | first n actions takeMVar and the last action performs putMVar n times
mvarSequenceOp :: MVar () -> Word8 -> Word8 -> IO Word8
mvarSequenceOp mv n x = do
    let msg = show x <> "/" <> show n
    if x < n
    then dbgMVar ("take mvarSequenceOp " <> msg) (void $ tryTakeMVar  mv) >>  return x
    else dbgMVar ("put mvarSequenceOp" <> msg)
            (replicateM_ (fromIntegral n) (tryPutMVar mv ())) >> return x

concurrentMapM
    :: ([Word8] -> Stream IO Word8)
    -> ([Word8] -> [Word8] -> Bool)
    -> (Word8 -> MVar () -> Stream IO Word8 -> Stream IO Word8)
    -> Word8
    -> Property
concurrentMapM constr eq op n =
    monadicIO $ do
        let list = [0..n]
        stream <- run $ do
            mv <- newEmptyMVar :: IO (MVar ())
            (S.fold Fold.toList . op n mv) (constr list)
        listEquals eq stream list

fromFoldableM :: [IO a] -> Stream IO a
fromFoldableM = Prelude.foldr S.consM S.nil

concurrentFromFoldable
    :: ([Word8] -> [Word8] -> Bool)
    -> (Stream IO Word8 -> Stream IO Word8)
    -> Word8
    -> Property
concurrentFromFoldable eq op n =
    monadicIO $ do
        let list = [0..n]
        stream <- run $ do
            mv <- newEmptyMVar :: IO (MVar ())
            (S.fold Fold.toList . op) (fromFoldableM (fmap (mvarSequenceOp mv n) list))
        listEquals eq stream list

sourceUnfoldrM ::  MVar () -> Word8 -> Stream IO Word8
sourceUnfoldrM mv n = S.unfoldrM step 0
    where
    -- argument must be integer to avoid overflow of word8 at 255
    step :: Int -> IO (Maybe (Word8, Int))
    step cnt = do
        let msg = show cnt <> "/" <> show n
        if cnt > fromIntegral n
        then return Nothing
        else do
            dbgMVar ("put sourceUnfoldrM " <> msg) (putMVar mv ())
            return (Just (fromIntegral cnt, cnt + 1))

-- Note that this test is not guaranteed to succeed, because there is no
-- guarantee of parallelism in case of Async/Ahead streams.
concurrentUnfoldrM
    :: ([Word8] -> [Word8] -> Bool)
    -> (Stream IO Word8 -> Stream IO Word8)
    -> Word8
    -> Property
concurrentUnfoldrM eq op n =
    monadicIO $ do
        -- XXX we should test empty list case as well
        let list = [0..n]
        stream <- run $ do
            -- putStrLn $ "concurrentUnfoldrM: " <> show n
            mv <- newEmptyMVar :: IO (MVar ())
            cnt <- newIORef 0
            -- since unfoldr happens in parallel with the stream processing we
            -- can do two takeMVar in one iteration. If it is not parallel then
            -- this will not work and the test will fail.
            S.fold Fold.toList $ do
                x <- op (sourceUnfoldrM mv n)
                -- results may not be yielded in order, in case of
                -- Async/WAsync/Parallel. So we use an increasing count
                -- instead.
                i <- S.fromEffect $ readIORef cnt
                S.fromEffect $ modifyIORef cnt (+1)
                let msg = show i <> "/" <> show n
                S.fromEffect $
                    when (even i) $ do
                        dbgMVar ("first take concurrentUnfoldrM " <> msg)
                                (takeMVar mv)
                        when (n > i) $
                            dbgMVar ("second take concurrentUnfoldrM " <> msg)
                                     (takeMVar mv)
                return x
        listEquals eq stream list

concurrentOps
    :: ([Word8] -> Stream IO Word8)
    -> String
    -> ([Word8] -> [Word8] -> Bool)
    -> (Stream IO Word8 -> Stream IO Word8)
    -> Spec
concurrentOps constr desc eq t = do
    let prop1 d p = prop d $ withMaxSuccess maxTestCount p

    prop1 (desc <> " fromFoldableM") $ concurrentFromFoldable eq t
    prop1 (desc <> " unfoldrM") $ concurrentUnfoldrM eq t
    -- we pass it the length of the stream n and an mvar mv.
    -- The stream is [0..n]. The threads communicate in such a way that the
    -- actions coming first in the stream are dependent on the last action. So
    -- if the stream is not processed concurrently it will block forever.
    -- Note that if the size of the stream is bigger than the thread limit
    -- then it will block even if it is concurrent.
    prop1 (desc <> " mapM") $
        concurrentMapM constr eq $ \n mv stream ->
            t $ S.mapM (mvarSequenceOp mv n) stream

    prop1 (desc <> " sequence") $
        concurrentMapM constr eq $ \n mv stream ->
            t $ S.sequence $ fmap (mvarSequenceOp mv n) stream

-------------------------------------------------------------------------------
-- Concurrent Application
-------------------------------------------------------------------------------

{-# INLINE mkParallel #-}
mkParallel :: (MonadAsync m) => Stream m a -> Stream m a
mkParallel = Concur.evalWith $ Concur.eager True

{-# INLINE (|$) #-}
(|$) :: (MonadAsync m) => (Stream m a -> Stream m b) -> (Stream m a -> Stream m b)
(|$) f = f . mkParallel

infixr 0 |$

{-# INLINE (|$.) #-}
(|$.) :: (MonadAsync m) => (Stream m a -> m b) -> (Stream m a -> m b)
-- (|$.) f = f . Concur.mkAsync
(|$.) f = f . mkParallel

infixr 0 |$.

-- | Same as '|$.' but with arguments reversed.
--
-- > (|&.) = flip (|$.)
--
-- /Concurrent/
--
-- /Since: 0.3.0 ("Streamly")/
--
-- @since 0.8.0
{-# INLINE (|&.) #-}
(|&.) :: (MonadAsync m) => Stream m a -> (Stream m a -> m b) -> m b
x |&. f = f |$. x

infixl 1 |&.

{-# INLINE (|&) #-}
(|&) :: (MonadAsync m) => Stream m a -> (Stream m a -> Stream m b) -> Stream m b
x |& f = f |$ x

infixl 1 |&


concurrentApplication :: ([Word8] -> [Word8] -> Bool)
    -> (Stream IO Word8 -> Stream IO Word8)
    -> Word8
    -> Property
concurrentApplication eq t n = withMaxSuccess maxTestCount $
    monadicIO $ do
        -- XXX we should test empty list case as well
        let list = [0..n]
        stream <- run $ do
            -- putStrLn $ "concurrentApplication: " <> show n
            mv <- newEmptyMVar :: IO (MVar ())
            -- since unfoldr happens in parallel with the stream processing we
            -- can do two takeMVar in one iteration. If it is not parallel then
            -- this will not work and the test will fail.
            (S.fold Fold.toList . t) $
                sourceUnfoldrM mv n |&
                    S.mapM (\x -> do
                        let msg = show x <> "/" <> show n
                        when (even x) $ do
                            dbgMVar ("first take concurrentApp " <> msg)
                                    (takeMVar mv)
                            when (n > x) $
                                dbgMVar ("second take concurrentApp " <> msg)
                                         (takeMVar mv)
                        return x)
        listEquals eq stream list

sourceUnfoldrM1 ::  Word8 -> Stream IO Word8
sourceUnfoldrM1 n = S.unfoldrM step 0
    where
    -- argument must be integer to avoid overflow of word8 at 255
    step :: Int -> IO (Maybe (Word8, Int))
    step cnt =
        if cnt > fromIntegral n
        then return Nothing
        else return (Just (fromIntegral cnt, cnt + 1))

foldlM' :: Monad m => (b -> a -> m b) -> m b -> Stream m a -> m b
foldlM' step begin = D.foldlM' step begin . S.toStreamD

concurrentFoldlApplication :: Word8 -> Property
concurrentFoldlApplication n =
    monadicIO $ do
        -- XXX we should test empty list case as well
        let list = [0..n]
        stream <- run $
            sourceUnfoldrM1 n |&. foldlM' (\xs x -> return (x : xs)) (return [])
        listEquals (==) (reverse stream) list

concurrentFoldrApplication :: Word8 -> Property
concurrentFoldrApplication n =
    monadicIO $ do
        -- XXX we should test empty list case as well
        let list = [0..n]
        stream <- run $
            sourceUnfoldrM1 n |&. S.foldrM (\x xs -> xs >>= return . (x :))
                                           (return [])
        listEquals (==) stream list

checkMonadicStateTransfer
    ::  (   Stream (StateT Int IO) ()
        ->  Stream (StateT Int IO) ()
        ->  Stream (StateT Int IO) a3 )
    -> IO ()
checkMonadicStateTransfer op = evalStateT str (0 :: Int)
  where
    str =
        S.fold Fold.drain $
        Concur.evalWith (Concur.maxBuffer 1) $
            S.mapM snapshoti (S.fromList [1 .. 10])  `op`
            S.mapM snapshoti (S.fromList [1 .. 10])
    snapshoti y = do
        modify (+ 1)
        x <- get
        lift1 $ x `shouldBe` y
    lift1 m = StateT $ \s -> do
        a <- m
        return (a, s)

takeInfinite ::  (Stream IO Int -> Stream IO Int) -> Spec
takeInfinite t =
    it "take 1" $
        S.fold Fold.drain (t $ S.take 1 $ (S.sequence . S.repeat) (print "hello" >> return (1::Int)))
        `shouldReturn` ()

moduleName :: String
moduleName = "Prelude.ConcurrentChannel"

#ifndef COVERAGE_BUILD
makeCommonOps1 :: MonadAsync m => (Stream m a -> c) -> [(String, Stream m a -> c)]
#else
makeCommonOps1 :: b -> [(String, b)]
#endif
makeCommonOps1 t =
            [ ("default", t)
#ifndef COVERAGE_BUILD
            , ("rate AvgRate 10000", t .  Concur.evalWith (Concur.avgRate 10000))
            , ("rate Nothing", t .  Concur.evalWith (Concur.rate Nothing))
            , ("maxBuffer 0", t .  Concur.evalWith (Concur.maxBuffer 0))
            , ("maxThreads 0", t . Concur.evalWith (Concur.maxThreads 0))
            , ("maxThreads 1", t . Concur.evalWith (Concur.maxThreads 1))
#ifdef USE_LARGE_MEMORY
            , ("maxThreads -1", t . Concur.evalWith (Concur.maxThreads (-1)))
#endif
#endif
            ]

#ifndef COVERAGE_BUILD
makeOps1 :: MonadAsync m => (Stream m a -> c) -> [(String, Stream m a -> c)]
#else
makeOps1 :: b -> [(String, b)]
#endif
makeOps1 t = makeCommonOps1 t ++
            [
#ifndef COVERAGE_BUILD
              ("maxBuffer 1", t . Concur.evalWith (Concur.maxBuffer 1))
#endif
            ]

mapOps1 :: (a -> Spec) -> [(String, a)] -> Spec
mapOps1 spec = mapM_ (\(desc, f) -> describe desc $ spec f)

main :: IO ()
main = hspec
  $ H.parallel
#ifdef COVERAGE_BUILD
  $ modifyMaxSuccess (const 10)
#endif
  $ describe moduleName $ do
    -- We can have these in Test.Prelude, but I think it's unnecessary.
    let serialOps ::  ((Stream IO a -> Stream IO a) -> Spec) -> Spec
        serialOps spec = mapOps1 spec $ makeOps1 id
#ifndef COVERAGE_BUILD
           -- <> [("rate AvgRate 0.00000001", Concur.evalWith (Concur.avgRate 0.00000001))]
            <> [("maxBuffer -1", Concur.evalWith (Concur.maxBuffer (-1)))]
#endif

    let aheadOps ::  ((Stream IO a -> Stream IO a) -> Spec) -> Spec
        aheadOps spec = mapOps1 spec $ makeOps1 id
#ifndef COVERAGE_BUILD
              <> [("maxBuffer (-1)", Concur.evalWith (Concur.maxBuffer (-1)))]
#endif

    let asyncOps ::  ((Stream IO a -> Stream IO a) -> Spec) -> Spec
        asyncOps spec = mapOps1 spec $ makeOps1 id
#ifndef COVERAGE_BUILD
            <> [("maxBuffer (-1)",  Concur.evalWith (Concur.maxBuffer (-1)))]
#endif

    -- For concurrent application test we need a buffer of at least size 2 to
    -- allow two threads to run.
#ifndef COVERAGE_BUILD
    let makeConcurrentAppOps :: MonadAsync m => (Stream m a -> c) -> [(String, Stream m a -> c)]
#endif
        makeConcurrentAppOps t = makeCommonOps1 t ++
            [
#ifndef COVERAGE_BUILD
              ("maxBuffer 2", t . Concur.evalWith (Concur.maxBuffer 2))
#endif
            ]

#ifndef COVERAGE_BUILD
    let parallelCommonOps :: MonadAsync m => [(String, Stream m a -> Stream m a)]
#else
    let parallelCommonOps :: MonadAsync m => [(String, Stream m a -> Stream m a)]
#endif
        parallelCommonOps = []
#ifndef COVERAGE_BUILD
            -- <> [("rate AvgRate 0.00000001", Concur.evalWith (Concur.avgRate 0.00000001))]
            <> [("maxBuffer (-1)", Concur.evalWith (Concur.maxBuffer (-1)))]
#endif

    let parallelConcurrentAppOps :: ((Stream IO a -> Stream IO a) -> Spec) -> Spec
        parallelConcurrentAppOps spec =
            mapOps1 spec $
                makeConcurrentAppOps (Concur.evalWith $ Concur.eager True) <> parallelCommonOps

    -- These tests won't work with maxBuffer or maxThreads set to 1, so we
    -- exclude those cases from these.
#ifndef COVERAGE_BUILD
    let mkOps ::   MonadAsync m => (Stream m a -> c) -> [(String, Stream m a -> c)]
#else
    let mkOps :: t -> [(String, t)]
    -- let mkOps :: (Stream m a -> c) -> [(String, Stream m a -> c)]
#endif
        mkOps t =
            [ ("default", t )
#ifndef COVERAGE_BUILD
            , ("rate Nothing", t . Concur.evalWith (Concur.rate Nothing))
            , ("maxBuffer 0", t . Concur.evalWith (Concur.maxBuffer 0))
            , ("maxThreads 0", t . Concur.evalWith (Concur.maxThreads 0))
            , ("maxThreads 0", t . Concur.evalWith (Concur.maxThreads (-1)))
#endif
            ]

    let forOps ops spec = forM_ ops (\(desc, f) -> describe desc $ spec f)

    describe "Stream concurrent operations" $ do

        forOps (mkOps (Concur.evalWith $ Concur.ordered True)) $ concurrentOps S.fromFoldable "aheadly" (==)
        forOps (mkOps Concur.eval) $ concurrentOps S.fromFoldable "asyncly" sortEq
        forOps (mkOps (Concur.evalWith $ Concur.interleaved True))  $ concurrentOps S.fromFoldable "wAsyncly" sortEq
        forOps (mkOps (Concur.evalWith $ Concur.eager True)) $ concurrentOps S.fromFoldable "parallely" sortEq

        forOps (mkOps (Concur.evalWith $ Concur.ordered True))   $ concurrentOps folded "aheadly folded" (==)
        forOps (mkOps Concur.eval) $ concurrentOps folded "asyncly folded" sortEq
        forOps (mkOps (Concur.evalWith $ Concur.interleaved True))  $ concurrentOps folded "wAsyncly folded" sortEq
        forOps (mkOps (Concur.evalWith $ Concur.eager True)) $ concurrentOps folded "parallely folded" sortEq

    describe "Concurrent application" $ do
        serialOps $ prop "serial" . concurrentApplication (==)
        asyncOps  $ prop "async" . concurrentApplication sortEq
        aheadOps  $ prop "ahead" . concurrentApplication (==)

        parallelConcurrentAppOps $
            prop "parallel" . concurrentApplication sortEq

        prop "concurrent foldr application" $ withMaxSuccess maxTestCount
            concurrentFoldrApplication
        prop "concurrent foldl application" $ withMaxSuccess maxTestCount
            concurrentFoldlApplication

    describe "take on infinite concurrent stream" $ takeInfinite id
    describe "take on infinite concurrent stream" $ takeInfinite id
    describe "take on infinite concurrent stream" $ takeInfinite id

    ---------------------------------------------------------------------------
    -- Monadic state transfer in concurrent tasks
    ---------------------------------------------------------------------------

    describe "Monadic state transfer in concurrent tasks" $ do
        -- XXX Can we write better test cases to hit every case?
        it "async: state is saved and used if the work is partially enqueued"
            (checkMonadicStateTransfer Concur.append2)
        it "wAsync: state is saved and used if the work is partially enqueued"
            (checkMonadicStateTransfer Concur.interleave2)
        it "ahead: state is saved and used if the work is partially enqueued"
            (checkMonadicStateTransfer Concur.ahead2)

    ---------------------------------------------------------------------------
    -- Slower tests are at the end
    ---------------------------------------------------------------------------

    ---------------------------------------------------------------------------
    -- Thread limits
    ---------------------------------------------------------------------------

        it "asyncly crosses thread limit (2000 threads)" $
            S.fold Fold.drain (Concur.append $
                    replicate 20 $  S.fromEffect $ threadDelay 1000000)
            `shouldReturn` ()

        it "aheadly crosses thread limit (4000 threads)" $
            S.fold Fold.drain (Concur.ahead $
                    replicate 4000 $ S.fromEffect $ threadDelay 1000000)
            `shouldReturn` ()

#ifdef DEVBUILD
    describe "restricts concurrency and cleans up extra tasks" $ do
        it "take 1 asyncly" $ checkCleanup 2  (S.take 1)
        it "take 1 wAsyncly" $ checkCleanup 2 id (S.take 1)
        it "take 1 aheadly" $ checkCleanup 2 id (S.take 1)

        it "takeWhile (< 0) asyncly" $ checkCleanup 2  (S.takeWhile (< 0))
        it "takeWhile (< 0) wAsyncly" $ checkCleanup 2 id (S.takeWhile (< 0))
        it "takeWhile (< 0) aheadly" $ checkCleanup 2 id (S.takeWhile (< 0))
#endif
