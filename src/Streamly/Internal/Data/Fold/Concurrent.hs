-- |
-- Module      : Streamly.Internal.Data.Fold.Concurrent
-- Copyright   : (c) 2022 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- = Asynchronous Evaluation
--
-- Using 'parBuffered' a fold can be decoupled from the driver and evaluated
-- concurrently with the driver. The driver just pushes an element to the
-- fold's buffer and waits for async evaluation to finish.
--
-- Stages in a fold pipeline can be made concurrent using 'parBuffered'.
--
-- = Concurrent Fold Combinators
--
-- The 'demux' combinator can be made concurrent by using 'parBuffered' on the fold
-- returned by the fold-generating function. Thus, we can fold values for each
-- key in the input stream concurrently.
--
-- Similarly, we can use 'parBuffered' with other cobminators like 'toMap',
-- 'demuxToMap', 'classify', 'tee', 'distribute', 'partition' etc. Basically,
-- any combinator that composes multiple folds or multiple instances of a fold
-- is a good candidate for running folds concurrently.
--
-- = Finalization
--
-- Before a fold returns "done" it has to drain the child folds. For example,
-- consider a "take" operation on a `parBuffered` fold, the take should return as
-- soon as it has taken required number of elements but we have to ensure that
-- any asynchronous child folds finish before it returns. This is achieved by
-- calling the "final" operation of the fold.

-- = TODO
--
-- Use multiple worker folds to fold serial chunks of a stream and collect the
-- results using another fold, combine using a monoid. The results can be
-- collected out-of-order or in-order. This would be easier if each input
-- element is a streamable chunk and each fold consumes one chunk at a time.
-- This is like parConcatMap in streams.
--
-- Concurrent append: if one fold's buffer becomes full then use the next one
-- Concurrent interleave/partition: Round robin to n folds.
-- Concurrent distribute to multiple folds.

module Streamly.Internal.Data.Fold.Concurrent
    (
      parBuffered
    , parLmapM
    , parTeeWith
    , parTee
    , parDistribute
    , parPartition
    , parUnzipWithM
    , parUnzip
    , parDistributeScan
    , parDemuxScan

    -- Deprecated
    , parEval
    )
where

#include "inline.hs"
#include "deprecation.h"

import Control.Concurrent (newEmptyMVar, takeMVar, throwTo)
import Control.Monad.Catch (throwM)
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.IORef (newIORef, readIORef)
import Fusion.Plugin.Types (Fuse(..))
import Streamly.Internal.Control.Concurrent (MonadAsync)
import Streamly.Internal.Data.Channel.Worker (sendEvent)
import Streamly.Internal.Data.Fold (Fold(..), Step (..))
import Streamly.Internal.Data.Stream (Stream(..), Step(..))
import Streamly.Internal.Data.SVar.Type (adaptState)

import qualified Data.Map.Strict as Map
import qualified Streamly.Internal.Data.Fold as Fold

import Streamly.Internal.Data.Fold.Channel.Type
import Streamly.Internal.Data.Channel.Types

-- $setup
-- >>> :set -fno-warn-deprecations
-- >>> import Control.Concurrent (threadDelay)
-- >>> import qualified Streamly.Internal.Data.Stream as Stream
-- >>> import qualified Streamly.Internal.Data.Stream.Prelude as Stream
-- >>> import qualified Streamly.Internal.Data.Fold as Fold
-- >>> import qualified Streamly.Internal.Data.Fold.Concurrent as Fold

-------------------------------------------------------------------------------
-- Evaluating a Fold
-------------------------------------------------------------------------------

-- | 'parBuffered' introduces a concurrent stage at the input of the fold. The
-- inputs are asynchronously queued in a buffer and evaluated concurrently with
-- the evaluation of the source stream. On finalization, 'parBuffered' waits for
-- the asynchronous fold to complete before it returns.
--
-- In the following example both the stream and the fold have a 1 second delay,
-- but the delay is not compounded because both run concurrently.
--
-- >>> delay x = threadDelay 1000000 >> print x >> return x
--
-- >>> src = Stream.delay 1 (Stream.enumerateFromTo 1 3)
-- >>> dst = Fold.parBuffered id (Fold.lmapM delay Fold.sum)
-- >>> Stream.fold dst src
-- ...
--
-- Another example:
--
-- >>> Stream.toList $ Stream.groupsOf 4 dst src
-- ...
--
{-# INLINABLE parBuffered #-}
parBuffered, parEval
    :: MonadAsync m => (Config -> Config) -> Fold m a b -> Fold m a b
parBuffered modifier f =
    Fold step initial extract final

    where

    -- XXX Supply an output channel to the fold. The fold would send the result
    -- from each step (i.e. scan result) to the channel. The Partial and Done
    -- constructors are sent to the channel. We then draw the resulting stream
    -- from that channel. Kind of concurrrent mapping on the stream but with a
    -- fold/scan.
    --
    -- There can also be a model where multiple folds pick input from the same
    -- channel.
    --
    -- We can also run parsers this way. So instead of sending output on each
    -- step it can send once it is done.
    initial = Partial <$> newChannel modifier f

    -- XXX This is not truly asynchronous. If the fold is done we only get to
    -- know when we send the next input unless the stream ends. We could
    -- potentially throw an async exception to the driver to inform it
    -- asynchronously. Alternatively, the stream should not block forever, it
    -- should keep polling the fold status. We can insert a timer tick in the
    -- input stream to do that.
    --
    -- A polled stream abstraction may be useful, it would consist of normal
    -- events and tick events, latter are guaranteed to arrive.
    --
    -- XXX We can use the config to indicate if the fold is a scanning type or
    -- one-shot, or use a separate parBufferedScan for scanning. For a scanning
    -- type fold the worker would always send the intermediate values back to
    -- the driver. An intermediate value can be returned on an input, or the
    -- driver can poll even without input, if we have the Skip input support.
    -- When the buffer is full we can return "Skip" and then the next step
    -- without input can wait for an output to arrive. Similarly, when "final"
    -- is called it can return "Skip" to continue or "Done" to indicate
    -- termination.
    step chan a = do
        status <- sendToWorker chan a
        return $ case status of
            Nothing -> Partial chan
            Just b -> Done b

    extract _ = error "Concurrent folds do not support scanning"

    -- XXX depending on the use case we may want to either wait for the result
    -- or cancel the ongoing work. We can use the config to control that?
    -- Currently it waits for the work to complete.
    final chan = do
        liftIO $ void
            $ sendEvent
                (inputQueue chan)
                (inputItemDoorBell chan)
                ChildStopChannel
        status <- checkFoldStatus chan
        case status of
            Nothing -> do
                liftIO
                    $ withDiagMVar
                        (svarInspectMode chan)
                        (dumpChannel chan)
                        "parBuffered: waiting to drain"
                    $ takeMVar (outputDoorBell chan)
                -- XXX remove recursion
                final chan
            Just b -> do
                cleanup chan
                return b
RENAME(parEval,parBuffered)

-- XXX We can have a lconcatMap (unfoldMany) to expand the chunks in the input
-- to streams before folding. This will require an input Skip constructor. In
-- fact, parLmapM can be implemented in terms of this like in streams.

-- | Evaluate the mapped actions concurrently with respect to each other. The
-- results may be unordered or ordered depending on the configuration.
--
-- /Unimplemented/
{-# INLINABLE parLmapM #-}
parLmapM :: -- MonadAsync m =>
    (Config -> Config) -> (a -> m b) -> Fold m b r -> Fold m a r
parLmapM = undefined

-- | Execute both the folds in a tee concurrently.
--
-- Definition:
--
-- >>> parTeeWith cfg f c1 c2 = Fold.teeWith f (Fold.parBuffered cfg c1) (Fold.parBuffered cfg c2)
--
-- Example:
--
-- >>> delay x = threadDelay 1000000 >> print x >> return x
-- >>> c1 = Fold.lmapM delay Fold.sum
-- >>> c2 = Fold.lmapM delay Fold.length
-- >>> dst = Fold.parTeeWith id (,) c1 c2
-- >>> Stream.fold dst src
-- ...
--
{-# INLINABLE parTeeWith #-}
parTeeWith :: MonadAsync m =>
       (Config -> Config)
    -> (a -> b -> c)
    -> Fold m x a
    -> Fold m x b
    -> Fold m x c
parTeeWith cfg f c1 c2 = Fold.teeWith f (parBuffered cfg c1) (parBuffered cfg c2)

-- | Execute both the folds in a tee concurrently.
--
-- Definition:
--
-- >>> parTee cfg c1 c2 = Fold.teeWith (,) (Fold.parBuffered cfg c1) (Fold.parBuffered cfg c2)
--
-- Example:
--
-- >>> delay x = threadDelay 1000000 >> print x >> return x
-- >>> c1 = Fold.lmapM delay Fold.sum
-- >>> c2 = Fold.lmapM delay Fold.length
-- >>> dst = Fold.parTee id c1 c2
-- >>> Stream.fold dst src
-- ...
--
{-# INLINABLE parTee #-}
parTee :: MonadAsync m =>
       (Config -> Config)
    -> Fold m x a
    -> Fold m x b
    -> Fold m x (a, b)
parTee cfg c1 c2 = Fold.teeWith (,) (parBuffered cfg c1) (parBuffered cfg c2)

-- | Distribute the input to all the folds in the supplied list concurrently.
--
-- Definition:
--
-- >>> parDistribute cfg = Fold.distribute . fmap (Fold.parBuffered cfg)
--
-- Example:
--
-- >>> delay x = threadDelay 1000000 >> print x >> return x
-- >>> c = Fold.lmapM delay Fold.sum
-- >>> dst = Fold.parDistribute id [c,c,c]
-- >>> Stream.fold dst src
-- ...
--
{-# INLINABLE parDistribute #-}
parDistribute :: MonadAsync m =>
    (Config -> Config) -> [Fold m a b] -> Fold m a [b]
parDistribute cfg = Fold.distribute . fmap (parBuffered cfg)

-- | Select first fold for Left input and second for Right input. Both folds
-- run concurrently.
--
-- Definition
--
-- >>> parPartition cfg c1 c2 = Fold.partition (Fold.parBuffered cfg c1) (Fold.parBuffered cfg c2)
--
-- Example:
--
-- >>> delay x = threadDelay 1000000 >> print x >> return x
-- >>> c1 = Fold.lmapM delay Fold.sum
-- >>> c2 = Fold.lmapM delay Fold.sum
-- >>> dst = Fold.parPartition id c1 c2
-- >>> Stream.fold dst $ (fmap (\x -> if even x then Left x else Right x)) src
-- ...
--
{-# INLINABLE parPartition #-}
parPartition :: MonadAsync m =>
    (Config -> Config) -> Fold m b x -> Fold m c y -> Fold m (Either b c) (x, y)
parPartition cfg c1 c2 = Fold.partition (parBuffered cfg c1) (parBuffered cfg c2)

-- | Split and distribute the output to two different folds and then zip the
-- results. Both the consumer folds run concurrently.
--
-- Definition
--
-- >>> parUnzipWithM cfg f c1 c2 = Fold.unzipWithM f (Fold.parBuffered cfg c1) (Fold.parBuffered cfg c2)
--
-- Example:
--
-- >>> delay x = threadDelay 1000000 >> print x >> return x
-- >>> c1 = Fold.lmapM delay Fold.sum
-- >>> c2 = Fold.lmapM delay Fold.sum
-- >>> dst = Fold.parUnzipWithM id (pure . id) c1 c2
-- >>> Stream.fold dst $ (fmap (\x -> (x, x* x))) src
-- ...
--
{-# INLINABLE parUnzipWithM #-}
parUnzipWithM :: MonadAsync m
    => (Config -> Config) -> (a -> m (b,c)) -> Fold m b x -> Fold m c y -> Fold m a (x,y)
parUnzipWithM cfg f c1 c2 = Fold.unzipWithM f (parBuffered cfg c1) (parBuffered cfg c2)

-- | Split and distribute the output to two different folds and then zip the
-- results. Both the consumer folds run concurrently.
--
-- Definition
--
-- >>> parUnzip cfg c1 c2 = Fold.unzip (Fold.parBuffered cfg c1) (Fold.parBuffered cfg c2)
--
-- Example:
--
-- >>> delay x = threadDelay 1000000 >> print x >> return x
-- >>> c1 = Fold.lmapM delay Fold.sum
-- >>> c2 = Fold.lmapM delay Fold.sum
-- >>> dst = Fold.parUnzip id c1 c2
-- >>> Stream.fold dst $ (fmap (\x -> (x, x * x))) src
-- ...
--
{-# INLINABLE parUnzip #-}
parUnzip :: MonadAsync m
    => (Config -> Config) -> Fold m b x -> Fold m c y -> Fold m (b,c) (x,y)
parUnzip cfg c1 c2 = Fold.unzip (parBuffered cfg c1) (parBuffered cfg c2)

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

-- | Evaluate a stream and send its outputs to zero or more dynamically
-- generated folds. It checks for any new folds at each input generation step.
-- Any new fold is added to the list of folds which are currently running. If
-- there are no folds available, the input is discarded. If a fold completes
-- its output is emitted in the output of the scan.
--
-- >>> import Data.IORef
-- >>> ref <- newIORef [Fold.take 2 Fold.sum, Fold.take 2 Fold.length :: Fold.Fold IO Int Int]
-- >>> gen = atomicModifyIORef ref (\xs -> ([], xs))
-- >>> Stream.toList $ Fold.parDistributeScan id gen (Stream.enumerateFromTo 1 10)
-- ...
--
{-# INLINE parDistributeScan #-}
parDistributeScan :: MonadAsync m =>
    (Config -> Config) -> m [Fold m a b] -> Stream m a -> Stream m [b]
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
                    FoldPartial _ ->
                        error "parDistributeScan: cannot occur for folds"
                    FoldEOF _ ->
                        error
                            "parDistributeScan: FoldEOF cannot occur for folds"

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
        newChans <- Prelude.mapM (newChannelWith q db cfg) fxs
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

-- | Evaluate a stream and send its outputs to the selected fold. The fold is
-- dynamically selected using a key at the time of the first input seen for
-- that key. Any new fold is added to the list of folds which are currently
-- running. If there are no folds available for a given key, the input is
-- discarded. If a fold completes its output is emitted in the output of the
-- scan.
--
-- >>> import qualified Data.Map.Strict as Map
-- >>> import Data.Maybe (fromJust)
-- >>> f1 = ("even", Fold.take 2 Fold.sum)
-- >>> f2 = ("odd", Fold.take 2 Fold.sum)
-- >>> kv = Map.fromList [f1, f2]
-- >>> getFold k = return (fromJust $ Map.lookup k kv)
-- >>> getKey x = if even x then "even" else "odd"
-- >>> input = Stream.enumerateFromTo 1 10
-- >>> Stream.toList $ Fold.parDemuxScan id getKey getFold input
-- ...
--
{-# INLINE parDemuxScan #-}
parDemuxScan :: (MonadAsync m, Ord k) =>
       (Config -> Config)
    -> (a -> k)
    -> (k -> m (Fold m a b))
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
                    FoldPartial _ ->
                        error "parDemuxScan: cannot occur for folds"
                    FoldEOF _ ->
                        error "parDemuxScan: FoldEOF cannot occur for folds"

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
                            r@(chan, _) <- newChannelWith q db cfg (fmap (k,) fld)
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
