-- |
-- Module      : Streamly.Internal.Data.Scanl.Concurrent
-- Copyright   : (c) 2024 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

module Streamly.Internal.Data.Scanl.Concurrent
    (
      parTeeWith
    , parDistributeScan
    , parDemuxScan
    )
where

#include "inline.hs"

import Control.Concurrent (newEmptyMVar, takeMVar, throwTo)
import Control.Monad.Catch (throwM)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.IORef (newIORef, readIORef)
import Fusion.Plugin.Types (Fuse(..))
import Streamly.Internal.Control.Concurrent (MonadAsync)
import Streamly.Internal.Data.Atomics (atomicModifyIORefCAS)
import Streamly.Internal.Data.Fold (Step (..))
import Streamly.Internal.Data.Scanl (Scanl(..))
import Streamly.Internal.Data.Stream (Stream(..), Step(..))
import Streamly.Internal.Data.SVar.Type (adaptState)
import Streamly.Internal.Data.Tuple.Strict (Tuple3'(..))

import qualified Data.Map.Strict as Map

import Streamly.Internal.Data.Fold.Channel.Type
import Streamly.Internal.Data.Channel.Types

-- $setup
-- >>> :set -fno-warn-deprecations
-- >>> import Control.Concurrent (threadDelay)
-- >>> import qualified Streamly.Internal.Data.Stream as Stream
-- >>> import qualified Streamly.Internal.Data.Scanl as Scanl
-- >>> import qualified Streamly.Internal.Data.Scanl.Concurrent as Scanl

-------------------------------------------------------------------------------
-- Concurrent scans
-------------------------------------------------------------------------------

-- | Execute both the scans in a tee concurrently.
--
-- Example:
--
-- >>> src = Stream.delay 1 (Stream.enumerateFromTo 1 3)
-- >>> delay x = threadDelay 1000000 >> print x >> return x
-- >>> c1 = Scanl.lmapM delay Scanl.sum
-- >>> c2 = Scanl.lmapM delay Scanl.length
-- >>> dst = Scanl.parTeeWith id (,) c1 c2
-- >>> Stream.toList $ Stream.scanl dst src
-- ...
--
{-# INLINABLE parTeeWith #-}
parTeeWith :: MonadAsync m =>
       (Config -> Config)
    -> (a -> b -> c)
    -> Scanl m x a
    -> Scanl m x b
    -> Scanl m x c
parTeeWith cfg f c1 c2 = Scanl step initial extract final

    where

    getResponse ch1 ch2 = do
        -- NOTE: We do not need a queue and doorbell mechanism for this, a single
        -- MVar should be enough. Also, there is only one writer and it writes
        -- only once before we read it.
        let db1 = outputDoorBell ch1
        let q1 = outputQueue ch1
        (xs1, _) <- liftIO $ atomicModifyIORefCAS q1 $ \x -> (([],0), x)
        case xs1 of
            [] -> do
                liftIO $ takeMVar db1
                getResponse ch1 ch2
            x1 : [] -> do
                case x1 of
                    FoldException _tid ex -> do
                        -- XXX
                        -- liftIO $ throwTo ch2Tid ThreadAbort
                        cleanup ch1
                        cleanup ch2
                        liftIO $ throwM ex
                    FoldDone _tid b -> return (Left b)
                    FoldPartial b -> return (Right b)
            _ -> error "parTeeWith: not expecting more than one msg in q"

    processResponses ch1 ch2 r1 r2 =
        return $ case r1 of
            Left b1 -> do
                case r2 of
                    Left b2 -> Done (f b1 b2)
                    Right b2 -> Done (f b1 b2)
            Right b1 -> do
                case r2 of
                    Left b2 -> Done (f b1 b2)
                    Right b2 -> Partial $ Tuple3' ch1 ch2 (f b1 b2)

    initial = do
        ch1 <- newScanChannel cfg c1
        ch2 <- newScanChannel cfg c2
        r1 <- getResponse ch1 ch2
        r2 <- getResponse ch2 ch1
        processResponses ch1 ch2 r1 r2

    step (Tuple3' ch1 ch2 _) x = do
        sendToWorker_ ch1 x
        sendToWorker_ ch2 x
        r1 <- getResponse ch1 ch2
        r2 <- getResponse ch2 ch1
        processResponses ch1 ch2 r1 r2

    extract (Tuple3' _ _ x) = return x

    final (Tuple3' ch1 ch2 x) = do
        finalize ch1
        finalize ch2
        -- XXX generate the final value?
        return x

-- There are two ways to implement a concurrent scan.
--
-- 1. Make the scan itself asynchronous, add the input to the queue, and then
-- extract the output. Extraction will have to be asynchronous, which will
-- require changes to the scan driver. This will require a different Scanl
-- type.
--
-- 2. A monolithic implementation of concurrent Stream->Stream scan, using a
-- custom implementation of the scan and the driver.

{-# ANN type ScanState Fuse #-}
data ScanState s q db f =
      ScanInit
    | ScanGo s q db [f]
    | ScanDrain q db [f]
    | ScanStop

-- XXX return [b] or just b?
-- XXX We can use a one way mailbox type abstraction instead of using an IORef
-- for adding new folds dynamically.

-- | Evaluate a stream and scan its outputs using zero or more dynamically
-- generated parallel scans. It checks for any new folds at each input
-- generation step. Any new fold is added to the list of folds which are
-- currently running. If there are no folds available, the input is discarded.
-- If a fold completes its output is emitted in the output of the scan. The
-- outputs of the parallel scans are merged in the output stream.
--
-- >>> import Data.IORef
-- >>> ref <- newIORef [Scanl.take 5 Scanl.sum, Scanl.take 5 Scanl.length :: Scanl.Scanl IO Int Int]
-- >>> gen = atomicModifyIORef ref (\xs -> ([], xs))
-- >>> Stream.toList $ Scanl.parDistributeScan id gen (Stream.enumerateFromTo 1 10)
-- ...
--
{-# INLINE parDistributeScan #-}
parDistributeScan :: MonadAsync m =>
    (Config -> Config) -> m [Scanl m a b] -> Stream m a -> Stream m [b]
parDistributeScan cfg getFolds (Stream sstep state) =
    Stream step ScanInit

    where

    -- XXX can be written as a fold
    processOutputs chans events done = do
        case events of
            [] -> return (chans, done)
            (x:xs) ->
                case x of
                    FoldException _tid ex -> do
                        -- XXX report the fold that threw the exception
                        liftIO $ mapM_ (`throwTo` ThreadAbort) (fmap snd chans)
                        mapM_ cleanup (fmap fst chans)
                        liftIO $ throwM ex
                    FoldDone tid b ->
                        let ch = filter (\(_, t) -> t /= tid) chans
                         in processOutputs ch xs (b:done)
                    FoldPartial b ->
                         processOutputs chans xs (b:done)

    collectOutputs qref chans = do
        (_, n) <- liftIO $ readIORef qref
        if n > 0
        then do
            r <- fmap fst $ liftIO $ readOutputQBasic qref
            processOutputs chans r []
        else return (chans, [])

    step _ ScanInit = do
        q <- liftIO $ newIORef ([], 0)
        db <- liftIO newEmptyMVar
        return $ Skip (ScanGo state q db [])

    step gst (ScanGo st q db chans) = do
        -- merge any new channels added since last input
        fxs <- getFolds
        newChans <- Prelude.mapM (newChannelWithScan q db cfg) fxs
        let allChans = chans ++ newChans

        -- Collect outputs from running channels
        (running, outputs) <- collectOutputs q allChans

        -- Send input to running folds
        res <- sstep (adaptState gst) st
        next <- case res of
            Yield x s -> do
                -- XXX We might block forever if some folds are already
                -- done but we have not read the output queue yet. To
                -- avoid that we have to either (1) precheck if space
                -- is available in the input queues of all folds so
                -- that this does not block, or (2) we have to use a
                -- non-blocking read and track progress so that we can
                -- restart from where we left.
                --
                -- If there is no space available then we should block
                -- on doorbell db or inputSpaceDoorBell of the relevant
                -- channel. To avoid deadlock the output space can be
                -- kept unlimited. However, the blocking will delay the
                -- processing of outputs. We should yield the outputs
                -- before blocking.
                Prelude.mapM_ (`sendToWorker_` x) (fmap fst running)
                return $ ScanGo s q db running
            Skip s -> do
                return $ ScanGo s q db running
            Stop -> do
                Prelude.mapM_ finalize (fmap fst running)
                return $ ScanDrain q db running
        if null outputs
        then return $ Skip next
        else return $ Yield outputs next
    step _ (ScanDrain q db chans) = do
        (running, outputs) <- collectOutputs q chans
        case running of
            [] -> return $ Yield outputs ScanStop
            _ -> do
                if null outputs
                then do
                    liftIO $ takeMVar db
                    return $ Skip (ScanDrain q db running)
                else return $ Yield outputs (ScanDrain q db running)
    step _ ScanStop = return Stop

{-# ANN type DemuxState Fuse #-}
data DemuxState s q db f =
      DemuxInit
    | DemuxGo s q db f
    | DemuxDrain q db f
    | DemuxStop

-- XXX We need to either (1) remember a key when done so that we do not add the
-- fold again because some inputs would be lost in between, or (2) have a
-- FoldYield constructor to yield repeatedly so that we can restart the
-- existing fold itself when it is done. But in that case we cannot change the
-- fold once it is started. Also the Map would keep on increasing in size as we
-- never delete a key. Whatever we do we should keep the non-concurrent fold as
-- well consistent with that.

-- | Evaluate a stream and send its outputs to the selected scan. The scan is
-- dynamically selected using a key at the time of the first input seen for
-- that key. Any new scan is added to the list of scans which are currently
-- running. If there are no scans available for a given key, the input is
-- discarded. If a constituent scan completes its output is emitted in the
-- output of the composed scan.
--
-- >>> import qualified Data.Map.Strict as Map
-- >>> import Data.Maybe (fromJust)
-- >>> f1 = ("even", Scanl.take 5 Scanl.sum)
-- >>> f2 = ("odd", Scanl.take 5 Scanl.sum)
-- >>> kv = Map.fromList [f1, f2]
-- >>> getScan k = return (fromJust $ Map.lookup k kv)
-- >>> getKey x = if even x then "even" else "odd"
-- >>> input = Stream.enumerateFromTo 1 10
-- >>> Stream.toList $ Scanl.parDemuxScan id getKey getScan input
-- ...
--
{-# INLINE parDemuxScan #-}
parDemuxScan :: (MonadAsync m, Ord k) =>
       (Config -> Config)
    -> (a -> k)
    -> (k -> m (Scanl m a b))
    -> Stream m a
    -> Stream m [(k, b)]
parDemuxScan cfg getKey getFold (Stream sstep state) =
    Stream step DemuxInit

    where

    -- XXX can be written as a fold
    processOutputs keyToChan events done = do
        case events of
            [] -> return (keyToChan, done)
            (x:xs) ->
                case x of
                    FoldException _tid ex -> do
                        -- XXX report the fold that threw the exception
                        let chans = fmap snd $ Map.toList keyToChan
                        liftIO $ mapM_ (`throwTo` ThreadAbort) (fmap snd chans)
                        mapM_ cleanup (fmap fst chans)
                        liftIO $ throwM ex
                    FoldDone _tid o@(k, _) ->
                        let ch = Map.delete k keyToChan
                         in processOutputs ch xs (o:done)
                    FoldPartial b ->
                         processOutputs keyToChan xs (b:done)

    collectOutputs qref keyToChan = do
        (_, n) <- liftIO $ readIORef qref
        if n > 0
        then do
            r <- fmap fst $ liftIO $ readOutputQBasic qref
            processOutputs keyToChan r []
        else return (keyToChan, [])

    step _ DemuxInit = do
        q <- liftIO $ newIORef ([], 0)
        db <- liftIO newEmptyMVar
        return $ Skip (DemuxGo state q db Map.empty)

    step gst (DemuxGo st q db keyToChan) = do
        -- Collect outputs from running channels
        (keyToChan1, outputs) <- collectOutputs q keyToChan

        -- Send input to the selected fold
        res <- sstep (adaptState gst) st

        next <- case res of
            Yield x s -> do
                -- XXX If the fold for a particular key is done and we see that
                -- key again. If we have not yet collected the done event we
                -- cannot restart the fold because the previous key is already
                -- installed. Thererfore, restarting the fold for the same key
                -- fraught with races.
                let k = getKey x
                (keyToChan2, ch) <-
                    case Map.lookup k keyToChan1 of
                        Nothing -> do
                            fld <- getFold k
                            r@(chan, _) <- newChannelWithScan q db cfg (fmap (k,) fld)
                            return (Map.insert k r keyToChan1, chan)
                        Just (chan, _) -> return (keyToChan1, chan)
                -- XXX We might block forever if some folds are already
                -- done but we have not read the output queue yet. To
                -- avoid that we have to either (1) precheck if space
                -- is available in the input queues of all folds so
                -- that this does not block, or (2) we have to use a
                -- non-blocking read and track progress so that we can
                -- restart from where we left.
                --
                -- If there is no space available then we should block
                -- on doorbell db or inputSpaceDoorBell of the relevant
                -- channel. To avoid deadlock the output space can be
                -- kept unlimited. However, the blocking will delay the
                -- processing of outputs. We should yield the outputs
                -- before blocking.
                sendToWorker_ ch x
                return $ DemuxGo s q db keyToChan2
            Skip s ->
                return $ DemuxGo s q db keyToChan1
            Stop -> do
                let chans = fmap (fst . snd) $ Map.toList keyToChan1
                Prelude.mapM_ finalize chans
                return $ DemuxDrain q db keyToChan1
        if null outputs
        then return $ Skip next
        else return $ Yield outputs next
    step _ (DemuxDrain q db keyToChan) = do
        (keyToChan1, outputs) <- collectOutputs q keyToChan
        if Map.null keyToChan1
        -- XXX null outputs case
        then return $ Yield outputs DemuxStop
        else do
            if null outputs
            then do
                liftIO $ takeMVar db
                return $ Skip (DemuxDrain q db keyToChan1)
            else return $ Yield outputs (DemuxDrain q db keyToChan1)
    step _ DemuxStop = return Stop
