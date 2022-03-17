-- |
-- Module      : Streamly.Internal.Data.Scan
-- Copyright   : (c) 2021 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Stateful transformations without termination.  A Scan can be used on a
-- stream, after an unfold or before a fold. Because it does not terminate it
-- is just a transformation, it is neither a stream nor a fold. Therefore, it
-- can be combined with both stream and fold to extend them.
--
-- Unlike folds, a scan does not emit a default value even without an input.
--
-- Summary of abstractions:
--
-- Unfold - Generation with static input
-- Stream - combine multiple producers
-- Scan - transformation without termination (map and filter)
-- Fold - combine multiple consumers
-- Refold - elimination with static input
--
-- Generator - a more powerful stream (with seeking and failure)
-- Parser - a more powerful fold (with seeking and failure)
--
-- Pipe - can generate, transform or fold
--
-- Duality (Correspondence):
--
-- Unfold <=> Refold
-- Stream <=> Fold
-- Generator <=> Parser
-- Scan
-- Universal: Pipe
--
-- Nested transformation duality:
--
-- The stream abstraction can generate streams and multiply (expand) them
-- (nesting).  intersperse, concatMap etc. expand the streams. However, the
-- Skip constructor also allows reducing the streams (filtering). concatMap
-- itself can support filtering and no surprise that concatMap requires Skip
-- constructor to implement.
--
-- What is analogous to Skip in folds? We should be able to Skip consuming.
-- Where can something like this be useful? Implementing the consumer analogue
-- to concatMap i.e. consume many elements in one go (unfoldMany for folds).
-- That will require the driver to skip supplying input to the fold. The fold
-- will be consuming internally generated input.
--
-- Since you cannot have a monad instance without concatMap in streams, can you
-- have a comonad instance without Skip/unfoldMany in folds? With unfoldMany we
-- could consume an array stream one array at a time. Interestingly we can do
-- the same by using concatMap on the array stream and then consuming the
-- resulting stream.

{-
data Step s b =
      Partial b s
    | Skip b s
    | Continue s
    | Done !b

data Scan m a b =
    -- | @Scan@ @step@ @initial@ @extract@
    forall s. Scan
        (s -> m (Step s b))       -- producer
        (s -> a -> m (Step s b))  -- consumer
        (m (Step s b))            -- initial
        (s -> m (Step s b))       -- drain
-}

-- However, this type can be used to implement unfoldMany for folds without
-- having to use a recursive step function.
--
-- If we have a separate scan type with Partial, Continue and Done, we can use
-- a fold type without Continue i.e. Partial/Skip/Done. Note folds would become
-- dual to streams. Scan has no nesting, fold does not need Continue as we do
-- not scan using intermdiate values. Also scans would be used to for scanning
-- with intermediate output whereas folds would be used to scan with terminal
-- output, that will also solve an oddity.
--
-- However, parsers with 5 constructors would become a tough beast.

module Streamly.Internal.Data.Scan
    (
      Scan (..)
    , map
    , compose
    , scan
    )
where

#include "inline.hs"
import Control.Category (Category(..))
-- import Control.Monad ((>=>))
-- import Control.Monad.Catch (MonadCatch, try, throwM, MonadThrow)

import qualified Streamly.Internal.Data.Stream.StreamD as D
import qualified Streamly.Internal.Data.Fold.Step as Fold
import Streamly.Internal.Data.Fold.Type (Fold(..))
import Streamly.Internal.Data.SVar.Type (adaptState)
import Streamly.Internal.Data.Stream.Serial (SerialT(..))
-- import Streamly.Internal.Data.Tuple.Strict (Tuple'(..))
import Fusion.Plugin.Types (Fuse(..))
import Prelude hiding (zipWith, map)

-------------------------------------------------------------------------------
-- Scan step
-------------------------------------------------------------------------------

-- XXX Should we use "Partial s b" instead like in the type "Step s b". But
-- that would not be the same as in Stream. We should probably change "Yield a
-- s" to "Yield s a" in streams?
--
-- This type is similar to the Stream Step type the only difference is that
-- here "Done" returns a result whereas in stream Stop does not yield a value.
--
-- Scans are postscan only, therefore, result is not allowed in the initial
-- action.
--
{-# ANN type Step Fuse #-}
data Step s b =
      Partial !b !s -- for transformation, not allowed in initial
    | Done !b       -- for termination with output, not allowed in initial
    | Continue !s   -- for filtering, not allowed in zipWith
    | Stop          -- termination without output (empty)

instance Functor (Step s) where
    {-# INLINE fmap #-}
    fmap f (Partial b s) = Partial (f b) s
    fmap f (Done b) = Done (f b)
    fmap _ (Continue s) = Continue s
    fmap _ Stop = Stop

{-
-- | Map a monadic function over the result @b@ in @Step s b@.
--
-- /Internal/
{-# INLINE mapMStep #-}
mapMStep :: Applicative m => (a -> m b) -> Step s a -> m (Step s b)
mapMStep f res =
    case res of
        Partial b s -> (`Partial` s) <$> f b
        Done b -> Done <$> f b
        Continue s -> pure $ Continue s
        Stop -> pure Stop
-}

-------------------------------------------------------------------------------
-- Scan
-------------------------------------------------------------------------------

-- Initial can avoid 'm' but we need this to be able to convert folds to scans.
-- Note: The type of initial allows only postscan.
data Scan m a b =
    -- | @Scan@ @step@ @initial@ @extract@
    forall s. Scan (s -> a -> m (Step s b)) (m (Maybe s))

------------------------------------------------------------------------------
-- Mapping on the output
------------------------------------------------------------------------------

instance Functor m => Functor (Scan m a) where
    {-# INLINE fmap #-}
    fmap f (Scan step1 initial1) =
        Scan step initial1

        where

        step s b = fmap2 f (step1 s b)
        fmap2 g = fmap (fmap g)

{-
-- |
--
-- /Pre-release/
--
{-# INLINE_NORMAL fromPure #-}
fromPure :: Monad m => b -> Scan m a b
fromPure b = Scan undefined (pure $ Done b)

-- |
--
-- /Pre-release/
--
{-# INLINE fromEffect #-}
fromEffect :: Monad m => m b -> Scan m a b
fromEffect b = Scan undefined (Done <$> b)
-}

{-
-- XXX The tee folds can be zipped, can we use the same type for zipping scans?
-- Tees also have Partial and Done only, zipping scans also need the same.
-- We can write a scanning zipWith for the tee folds. tee folds will require an
-- output in Partial for that though, but we can use extract as well.
--
-- So folds can be converted to scans except that folds cannot be filtering
-- scans. So unless it requires filtering we can write it as a fold. Filtering
-- stuff can be used with both streams and folds and therefore has to be
-- written as scans.
--
{-# ANN type TeeState Fuse #-}
data TeeState sL sR = TeeBoth !sL !sR

-- XXX Merging of streams is not possible because that would require a Skip
-- loop.
--
-- XXX There is not much point of a zip, as we can acheive this using a map as
-- well. Unless this is concurrent in which case this becomes a fork and join.
--
-- | Start the next scan from where the previous scan left off. We may need a
-- CPS version for better scaling, or may need a Array stream scan.
--
{-# INLINE zipWith #-}
zipWith :: Monad m =>
    (a -> b -> c) -> Scan m x a -> Scan m x b -> Scan m x c
zipWith func (Scan stepL initialL) (Scan stepR initialR) =
    Scan step initial -- extract

    where

    {-# INLINE runBoth #-}
    runBoth actionL actionR = do
        resL <- actionL
        resR <- actionR
        return
            $ case resL of
                  Partial bl sl ->
                      case resR of
                            Partial br sr ->
                                Partial (func bl br) $ TeeBoth sl sr
                            Done br -> Done (func bl br)
                            Continue _ -> error "zipWith: Continue"
                            Stop -> Stop
                  Done bl ->
                      case resR of
                        Partial br _ -> Done (func bl br)
                        Done br -> Done (func bl br)
                        Continue _ -> error "zipWith: Continue"
                        Stop -> Stop
                  Continue _ -> error "zipWith: Continue"
                  Stop -> Stop

    -- XXX initial needs to discard the initial values
    initial = runBoth initialL initialR

    step (TeeBoth sL sR) a = runBoth (stepL sL a) (stepR sR a)
-}

{-
instance Monad m => Applicative (Scan m a) where
    pure = fromPure -- XXX this should be a stream

    (<*>) = zipWith Prelude.id
-}

-- | postscan a scan
{-# INLINE compose #-}
compose :: Monad m => Scan m a b -> Scan m b c -> Scan m a c
compose (Scan stepL initialL) (Scan stepR initialR) = Scan step initial

    where

    {-# INLINE runStep #-}
    runStep actionL sR = do
        rL <- actionL
        case rL of
            Done bL -> do
                rR <- stepR sR bL
                return
                    $ case rR of
                        Partial br _ -> Done br
                        Done bR -> Done bR
                        Continue _ -> Stop
                        Stop -> Stop
            Partial bL sL -> do
                rR <- stepR sR bL
                return
                    $ case rR of
                        Partial br sR1 -> Partial br (sL, sR1)
                        Done bR -> Done bR
                        Continue sR1 -> Continue (sL, sR1)
                        Stop -> Stop
            Continue sL -> return $ Continue (sL, sR)
            Stop -> return Stop

    initial = do
        rR <- initialR
        rL <- initialL
        return
            $ case rR of
                Just sR -> do
                    case rL of
                        Just sL -> Just (sL, sR)
                        Nothing -> Nothing
                Nothing -> Nothing

    step (sL, sR) x = runStep (stepL sL x) sR

{-# INLINE identity #-}
identity :: Monad m => Scan m a a
identity = Scan (\_ a -> return $ Partial a ()) (return $ Just ())

instance Monad m => Category (Scan m) where
    id = identity

    (.) = flip compose

{-# INLINE map #-}
map :: Monad m => (a -> m b) -> Scan m a b
map f =
    Scan (\() a -> f a >>= \b -> return $ Partial b ()) (return $ Just ())

data ScanState s sr = ScanInit | ScanRun s sr | ScanDone

{-# INLINE scanD #-}
scanD :: Monad m => Scan m a b -> D.Stream m a -> D.Stream m b
scanD (Scan scan_step initial) (D.UnStream stream_step stream_state) =
    D.UnStream step ScanInit

    where

    {-# INLINE runStep #-}
    runStep s action = do
        res <- action
        return
            $ case res of
                Partial b ss -> D.Yield b (ScanRun s ss)
                Done b -> D.Yield b ScanDone
                Continue ss -> D.Skip (ScanRun s ss)
                Stop -> D.Stop

    step _ ScanInit = do
        r <- initial
        return
            $ case r of
                Just s -> D.Skip (ScanRun stream_state s)
                Nothing -> D.Stop

    step gst (ScanRun st acc) = do
        r <- stream_step (adaptState gst) st
        case r of
            D.Yield x s -> runStep s (scan_step acc x)
            D.Skip s -> return $ D.Skip (ScanRun s acc)
            D.Stop -> return D.Stop

    step _ ScanDone = return D.Stop

-- | postscan a stream
{-# INLINE scan #-}
scan :: Monad m => Scan m a b -> SerialT m a -> SerialT m b
scan s (SerialT stream) =
    SerialT $ D.toStreamK $ scanD s (D.fromStreamK stream)

-- | postscan a fold
{-# INLINE scanFold #-}
scanFold :: Monad m => Scan m a b -> Fold m b c -> Fold m a c
scanFold (Scan stepL initialL) (Fold stepR initialR extractR) =
    Fold step initial extract

    where

    {-# INLINE runStep #-}
    runStep actionL sR = do
        rL <- actionL
        case rL of
            Done bL -> do
                rR <- stepR sR bL
                case rR of
                    Fold.Partial sR1 -> Fold.Done <$> extractR sR1
                    Fold.Done bR -> return $ Fold.Done bR
            Partial bL sL -> do
                rR <- stepR sR bL
                return
                    $ case rR of
                        Fold.Partial sR1 -> Fold.Partial (sL, sR1)
                        Fold.Done bR -> Fold.Done bR
            -- XXX We need Continue in Folds for this
            Continue sL -> undefined
            Stop -> Fold.Done <$> extractR sR

    initial = do
        rR <- initialR
        rL <- initialL
        case rR of
            Fold.Partial sR ->
                case rL of
                    Just sL -> return $ Fold.Partial (sL, sR)
                    Nothing -> Fold.Done <$> extractR sR
            Fold.Done b -> return $ Fold.Done b

    step (sL, sR) x = runStep (stepL sL x) sR

    extract = extractR Prelude.. snd

{-
scanFromFold :: Monad m => FL.Fold m a b -> Scan m a b
scanFromFold (FL.Fold fl_step fl_initial fl_extract) = Scan step fl_initial
  where
    step ms a = do
        s <- ms
        s' <- fl_step s a
        b <- fl_extract s'
        return $ Tuple' (return s') b
        -}
