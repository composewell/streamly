{-# LANGUAGE OverloadedLists #-}

module Main (main) where

import Control.Concurrent (MVar, takeMVar, putMVar, newEmptyMVar)
import Control.Exception
       (BlockedIndefinitelyOnMVar(..), catches,
        BlockedIndefinitelyOnSTM(..), Handler(..))
import Control.Monad (when, forM_, replicateM_)
import Data.IORef (readIORef, modifyIORef, newIORef)
import GHC.Word (Word8)

import Test.Hspec.QuickCheck
import Test.QuickCheck
       (Property, withMaxSuccess)
import Test.QuickCheck.Monadic (monadicIO, run)

import Test.Hspec as H

import Streamly
import qualified Streamly.Prelude as S

import Streamly.Test.Common
import Streamly.Test.Prelude

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
    then dbgMVar ("take mvarSequenceOp " <> msg) (takeMVar mv) >>  return x
    else dbgMVar ("put mvarSequenceOp" <> msg)
            (replicateM_ (fromIntegral n) (putMVar mv ())) >> return x

concurrentMapM
    :: ([Word8] -> t IO Word8)
    -> ([Word8] -> [Word8] -> Bool)
    -> (Word8 -> MVar () -> t IO Word8 -> SerialT IO Word8)
    -> Word8
    -> Property
concurrentMapM constr eq op n =
    monadicIO $ do
        let list = [0..n]
        stream <- run $ do
            mv <- newEmptyMVar :: IO (MVar ())
            (S.toList . op n mv) (constr list)
        listEquals eq stream list

concurrentFromFoldable
    :: IsStream t
    => ([Word8] -> [Word8] -> Bool)
    -> (t IO Word8 -> SerialT IO Word8)
    -> Word8
    -> Property
concurrentFromFoldable eq op n =
    monadicIO $ do
        let list = [0..n]
        stream <- run $ do
            mv <- newEmptyMVar :: IO (MVar ())
            (S.toList . op) (S.fromFoldableM (fmap (mvarSequenceOp mv n) list))
        listEquals eq stream list

sourceUnfoldrM :: IsStream t => MVar () -> Word8 -> t IO Word8
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
    :: IsStream t
    => ([Word8] -> [Word8] -> Bool)
    -> (t IO Word8 -> SerialT IO Word8)
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
            S.toList $ do
                x <- op (sourceUnfoldrM mv n)
                -- results may not be yielded in order, in case of
                -- Async/WAsync/Parallel. So we use an increasing count
                -- instead.
                i <- S.yieldM $ readIORef cnt
                S.yieldM $ modifyIORef cnt (+1)
                let msg = show i <> "/" <> show n
                S.yieldM $
                    when (even i) $ do
                        dbgMVar ("first take concurrentUnfoldrM " <> msg)
                                (takeMVar mv)
                        when (n > i) $
                            dbgMVar ("second take concurrentUnfoldrM " <> msg)
                                     (takeMVar mv)
                return x
        listEquals eq stream list

concurrentOps
    :: IsStream t
    => ([Word8] -> t IO Word8)
    -> String
    -> ([Word8] -> [Word8] -> Bool)
    -> (t IO Word8 -> SerialT IO Word8)
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

-------------------------------------------------------------------------------
-- Concurrent Application
-------------------------------------------------------------------------------

concurrentApplication :: IsStream t
    => ([Word8] -> [Word8] -> Bool)
    -> (t IO Word8 -> SerialT IO Word8)
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
            (S.toList . t) $
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

sourceUnfoldrM1 :: IsStream t => Word8 -> t IO Word8
sourceUnfoldrM1 n = S.unfoldrM step 0
    where
    -- argument must be integer to avoid overflow of word8 at 255
    step :: Int -> IO (Maybe (Word8, Int))
    step cnt =
        if cnt > fromIntegral n
        then return Nothing
        else return (Just (fromIntegral cnt, cnt + 1))

concurrentFoldlApplication :: Word8 -> Property
concurrentFoldlApplication n =
    monadicIO $ do
        -- XXX we should test empty list case as well
        let list = [0..n]
        stream <- run $
            sourceUnfoldrM1 n |&. S.foldlM' (\xs x -> return (x : xs)) (return [])
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

main :: IO ()
main = hspec
    $ H.parallel
#ifdef COVERAGE_BUILD
    $ modifyMaxSuccess (const 10)
#endif
    $ do
    -- For concurrent application test we need a buffer of at least size 2 to
    -- allow two threads to run.
    let makeConcurrentAppOps :: IsStream t
            => (t m a -> c) -> [(String, t m a -> c)]
        makeConcurrentAppOps t = makeCommonOps t ++
            [
#ifndef COVERAGE_BUILD
              ("maxBuffer 2", t . maxBuffer 2)
#endif
            ]

    let parallelCommonOps :: IsStream t => [(String, ParallelT m a -> t m a)]
        parallelCommonOps = []
#ifndef COVERAGE_BUILD
            <> [("rate AvgRate 0.00000001", parallely . avgRate 0.00000001)]
            <> [("maxBuffer (-1)", parallely . maxBuffer (-1))]
#endif
    let parallelOps :: IsStream t
            => ((ParallelT IO a -> t IO a) -> Spec) -> Spec
        parallelOps spec = mapOps spec $ makeOps parallely <> parallelCommonOps

    let parallelConcurrentAppOps :: IsStream t
            => ((ParallelT IO a -> t IO a) -> Spec) -> Spec
        parallelConcurrentAppOps spec =
            mapOps spec $ makeConcurrentAppOps parallely <> parallelCommonOps

    describe "Construction" $ do
        parallelOps $ prop "parallely replicateM" .  constructWithReplicateM

        -- take doesn't work well on concurrent streams. Even though it
        -- seems like take only has a problem when used with parallely.
        -- wSerialOps $ prop "wSerially iterateM" wSerially . constructWithIterate
        -- aheadOps $ prop "aheadly iterateM" aheadly . onstructWithIterate
        -- asyncOps $ prop "asyncly iterateM" asyncly . constructWithIterate
        -- wAsyncOps $ prop "wAsyncly iterateM" wAsyncly . onstructWithIterate
        -- parallelOps $ prop "parallely iterateM" parallely . onstructWithIterate
        -- XXX add tests for fromIndices

    describe "Functor operations" $ do
        parallelOps  $ functorOps S.fromFoldable "parallely" sortEq
        parallelOps  $ functorOps folded "parallely folded" sortEq

    describe "Semigroup operations" $ do
        parallelOps  $ semigroupOps "parallely" sortEq

    describe "Applicative operations" $ do
        parallelOps $ prop "parallely applicative folded" . applicativeOps folded sortEq

    -- XXX add tests for indexed/indexedR
    describe "Zip operations" $ do
        -- We test only the serial zip with serial streams and the parallel
        -- stream, because the rate setting in these streams can slow down
        -- zipAsync.
        parallelOps $ prop "zip monadic parallely" . zipMonadic S.fromFoldable (==)
        parallelOps $ prop "zip monadic parallely folded" . zipMonadic folded (==)

    -- XXX add merge tests like zip tests
    -- for mergeBy, we can split a list randomly into two lists and
    -- then merge them, it should result in original list
    -- describe "Merge operations" $ do

    describe "Monad operations" $ do
        parallelOps $ prop "parallely monad then" . monadThen S.fromFoldable sortEq

        parallelOps $ prop "parallely monad then folded" . monadThen folded sortEq

        parallelOps $ prop "parallely monad bind" . monadBind S.fromFoldable sortEq

        parallelOps $ prop "parallely monad bind folded" . monadBind folded sortEq

    -- These tests won't work with maxBuffer or maxThreads set to 1, so we
    -- exclude those cases from these.
    let mkOps :: IsStream t => (t m a -> c) -> [(String, t m a -> c)]
        mkOps t =
            [ ("default", t)
#ifndef COVERAGE_BUILD
            , ("rate Nothing", t . rate Nothing)
            , ("maxBuffer 0", t . maxBuffer 0)
            , ("maxThreads 0", t . maxThreads 0)
            , ("maxThreads 0", t . maxThreads (-1))
#endif
            ]

    let forOps ops spec = forM_ ops (\(desc, f) -> describe desc $ spec f)
    describe "Stream concurrent operations" $ do
        forOps (mkOps aheadly)   $ concurrentOps S.fromFoldable "aheadly" (==)
        forOps (mkOps asyncly)   $ concurrentOps S.fromFoldable "asyncly" sortEq
        forOps (mkOps wAsyncly)  $ concurrentOps S.fromFoldable "wAsyncly" sortEq
        forOps (mkOps parallely) $ concurrentOps S.fromFoldable "parallely" sortEq

        forOps (mkOps aheadly)   $ concurrentOps folded "aheadly folded" (==)
        forOps (mkOps asyncly)   $ concurrentOps folded "asyncly folded" sortEq
        forOps (mkOps wAsyncly)  $ concurrentOps folded "wAsyncly folded" sortEq
        forOps (mkOps parallely) $ concurrentOps folded "parallely folded" sortEq

    describe "Concurrent application" $ do
--        serialOps $ prop "serial" . concurrentApplication (==)
--        asyncOps $ prop "async" . concurrentApplication sortEq
--        aheadOps $ prop "ahead" . concurrentApplication (==)
        parallelConcurrentAppOps $
            prop "parallel" . concurrentApplication sortEq

        prop "concurrent foldr application" $ withMaxSuccess maxTestCount
            concurrentFoldrApplication
        prop "concurrent foldl application" $ withMaxSuccess maxTestCount
            concurrentFoldlApplication

    describe "Stream transform and combine operations" $ do
        parallelOps  $ transformCombineOpsCommon S.fromFoldable "parallely" sortEq

        parallelOps  $ transformCombineOpsCommon folded "parallely" sortEq

    describe "Stream elimination operations" $ do
        parallelOps  $ eliminationOps S.fromFoldable "parallely"

        parallelOps  $ eliminationOps folded "parallely folded"

        parallelOps  $ eliminationOpsWord8 S.fromFoldable "parallely"

        parallelOps  $ eliminationOpsWord8 folded "parallely folded"
