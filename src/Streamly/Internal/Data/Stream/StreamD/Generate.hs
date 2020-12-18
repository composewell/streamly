#include "inline.hs"

-- |
-- Module      : Streamly.Internal.Data.Stream.StreamD.Generate
-- Copyright   : (c) 2020 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

module Streamly.Internal.Data.Stream.StreamD.Generate
  (
    -- * Generation
    -- ** Unfolds
      unfoldr
    , unfoldrM
    , unfold

    -- ** Specialized Generation
    -- | Generate a monadic stream from a seed.
    , repeat
    , repeatM
    , replicate
    , replicateM
    , fromIndices
    , fromIndicesM
    , generate
    , generateM
    , iterate
    , iterateM

    -- ** Enumerations
    , enumerateFromStepIntegral
    , enumerateFromThenIntegral
    , enumerateFromThenToIntegral

    , enumerateFromStepNum
    , numFrom
    , numFromThen

    -- ** Time
    , times

    -- ** Conversions
    -- | Transform an input structure into a stream.
    -- | Direct style stream does not support @fromFoldable@.
    , yield
    , yieldM
    , fromList
    , fromListM
    , fromStreamK
    , fromStreamD
    , fromPrimIORef
    , fromProducer
    , fromSVar
    , toStreamD
    )
where

import Control.Concurrent (threadDelay)
import Control.Exception
       (fromException)
import Control.Monad (when, forever)
import Control.Monad.Catch (throwM)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Int (Int64)
import Data.IORef (newIORef, readIORef, mkWeakIORef, writeIORef)
import Data.Maybe (isNothing)
import System.Mem (performMajorGC)
import Streamly.Internal.Data.IORef.Prim (Prim)
import Streamly.Internal.Data.Time.Units
       (toRelTime64, RelTime64)
import Streamly.Internal.Data.Time.Clock (Clock(Monotonic), getTime)
import Streamly.Internal.Data.Time.Units
       (MicroSecond64(..), fromAbsTime, toAbsTime, AbsTime)
import Streamly.Internal.Data.Unfold.Types (Unfold(..))

import qualified Streamly.Internal.Data.IORef.Prim as Prim
import qualified Streamly.Internal.Data.Stream.StreamK as K
import qualified Prelude

import Prelude hiding
       ( map, mapM, mapM_, repeat, foldr, last, take, filter
       , takeWhile, drop, dropWhile, all, any, maximum, minimum, elem
       , notElem, null, head, tail, zipWith, lookup, foldr1, sequence
       , (!!), scanl, scanl1, concatMap, replicate, enumFromTo, concat
       , reverse, iterate, splitAt)
import Streamly.Internal.Data.Stream.StreamD.Type
import Streamly.Internal.Data.SVar


------------------------------------------------------------------------------
-- Generation by unfold
------------------------------------------------------------------------------

{-# INLINE_NORMAL unfoldrM #-}
unfoldrM :: Monad m => (s -> m (Maybe (a, s))) -> s -> Stream m a
unfoldrM next state = Stream step state
  where
    {-# INLINE_LATE step #-}
    step _ st = do
        r <- next st
        return $ case r of
            Just (x, s) -> Yield x s
            Nothing     -> Stop

{-# INLINE_LATE unfoldr #-}
unfoldr :: Monad m => (s -> Maybe (a, s)) -> s -> Stream m a
unfoldr f = unfoldrM (return . f)

data UnfoldState s = UnfoldNothing | UnfoldJust s

-- | Convert an 'Unfold' into a 'Stream' by supplying it a seed.
--
{-# INLINE_NORMAL unfold #-}
unfold :: Monad m => Unfold m a b -> a -> Stream m b
unfold (Unfold ustep inject) seed = Stream step UnfoldNothing
  where
    {-# INLINE_LATE step #-}
    step _ UnfoldNothing = inject seed >>= return . Skip . UnfoldJust
    step _ (UnfoldJust st) = do
        r <- ustep st
        return $ case r of
            Yield x s -> Yield x (UnfoldJust s)
            Skip s    -> Skip (UnfoldJust s)
            Stop      -> Stop

------------------------------------------------------------------------------
-- Specialized Generation
------------------------------------------------------------------------------

{-# INLINE_NORMAL repeatM #-}
repeatM :: Monad m => m a -> Stream m a
repeatM x = Stream (\_ _ -> x >>= \r -> return $ Yield r ()) ()

{-# INLINE_NORMAL repeat #-}
repeat :: Monad m => a -> Stream m a
repeat x = Stream (\_ _ -> return $ Yield x ()) ()

{-# INLINE_NORMAL iterateM #-}
iterateM :: Monad m => (a -> m a) -> m a -> Stream m a
iterateM step = Stream (\_ st -> st >>= \x -> return $ Yield x (step x))

{-# INLINE_NORMAL iterate #-}
iterate :: Monad m => (a -> a) -> a -> Stream m a
iterate step st = iterateM (return . step) (return st)

{-# INLINE_NORMAL replicateM #-}
replicateM :: forall m a. Monad m => Int -> m a -> Stream m a
replicateM n p = Stream step n
  where
    {-# INLINE_LATE step #-}
    step _ (i :: Int)
      | i <= 0    = return Stop
      | otherwise = do
          x <- p
          return $ Yield x (i - 1)

{-# INLINE_NORMAL replicate #-}
replicate :: Monad m => Int -> a -> Stream m a
replicate n x = replicateM n (return x)

-- This would not work properly for floats, therefore we put an Integral
-- constraint.
-- | Can be used to enumerate unbounded integrals. This does not check for
-- overflow or underflow for bounded integrals.
{-# INLINE_NORMAL enumerateFromStepIntegral #-}
enumerateFromStepIntegral :: (Integral a, Monad m) => a -> a -> Stream m a
enumerateFromStepIntegral from stride =
    from `seq` stride `seq` Stream step from
    where
        {-# INLINE_LATE step #-}
        step _ !x = return $ Yield x $! (x + stride)

data EnumState a = EnumInit | EnumYield a a a | EnumStop

{-# INLINE_NORMAL enumerateFromThenToIntegralUp #-}
enumerateFromThenToIntegralUp
    :: (Monad m, Integral a)
    => a -> a -> a -> Stream m a
enumerateFromThenToIntegralUp from next to = Stream step EnumInit
    where
    {-# INLINE_LATE step #-}
    step _ EnumInit =
        return $
            if to < next
            then if to < from
                 then Stop
                 else Yield from EnumStop
            else -- from <= next <= to
                let stride = next - from
                in Skip $ EnumYield from stride (to - stride)

    step _ (EnumYield x stride toMinus) =
        return $
            if x > toMinus
            then Yield x EnumStop
            else Yield x $ EnumYield (x + stride) stride toMinus

    step _ EnumStop = return Stop

{-# INLINE_NORMAL enumerateFromThenToIntegralDn #-}
enumerateFromThenToIntegralDn
    :: (Monad m, Integral a)
    => a -> a -> a -> Stream m a
enumerateFromThenToIntegralDn from next to = Stream step EnumInit
    where
    {-# INLINE_LATE step #-}
    step _ EnumInit =
        return $ if to > next
            then if to > from
                 then Stop
                 else Yield from EnumStop
            else -- from >= next >= to
                let stride = next - from
                in Skip $ EnumYield from stride (to - stride)

    step _ (EnumYield x stride toMinus) =
        return $
            if x < toMinus
            then Yield x EnumStop
            else Yield x $ EnumYield (x + stride) stride toMinus

    step _ EnumStop = return Stop

{-# INLINE_NORMAL enumerateFromThenToIntegral #-}
enumerateFromThenToIntegral
    :: (Monad m, Integral a)
    => a -> a -> a -> Stream m a
enumerateFromThenToIntegral from next to
    | next >= from = enumerateFromThenToIntegralUp from next to
    | otherwise    = enumerateFromThenToIntegralDn from next to

{-# INLINE_NORMAL enumerateFromThenIntegral #-}
enumerateFromThenIntegral
    :: (Monad m, Integral a, Bounded a)
    => a -> a -> Stream m a
enumerateFromThenIntegral from next =
    if next > from
    then enumerateFromThenToIntegralUp from next maxBound
    else enumerateFromThenToIntegralDn from next minBound

-- For floating point numbers if the increment is less than the precision then
-- it just gets lost. Therefore we cannot always increment it correctly by just
-- repeated addition.
-- 9007199254740992 + 1 + 1 :: Double => 9.007199254740992e15
-- 9007199254740992 + 2     :: Double => 9.007199254740994e15

-- Instead we accumulate the increment counter and compute the increment
-- every time before adding it to the starting number.
--
-- This works for Integrals as well as floating point numbers, but
-- enumerateFromStepIntegral is faster for integrals.
{-# INLINE_NORMAL enumerateFromStepNum #-}
enumerateFromStepNum :: (Monad m, Num a) => a -> a -> Stream m a
enumerateFromStepNum from stride = Stream step 0
    where
    {-# INLINE_LATE step #-}
    step _ !i = return $ (Yield $! (from + i * stride)) $! (i + 1)

{-# INLINE_NORMAL numFrom #-}
numFrom :: (Monad m, Num a) => a -> Stream m a
numFrom from = enumerateFromStepNum from 1

{-# INLINE_NORMAL numFromThen #-}
numFromThen :: (Monad m, Num a) => a -> a -> Stream m a
numFromThen from next = enumerateFromStepNum from (next - from)

{-# INLINE updateTimeVar #-}
updateTimeVar :: Prim.IORef Int64 -> IO ()
updateTimeVar timeVar = do
    MicroSecond64 t <- fromAbsTime <$> getTime Monotonic
    Prim.modifyIORef' timeVar (const t)

{-# INLINE updateWithDelay #-}
updateWithDelay :: RealFrac a => a -> Prim.IORef Int64 -> IO ()
updateWithDelay precision timeVar = do
    threadDelay (delayTime precision)
    updateTimeVar timeVar

    where

    -- Keep the minimum at least a millisecond to avoid high CPU usage
    {-# INLINE delayTime #-}
    delayTime g
        | g' >= fromIntegral (maxBound :: Int) = maxBound
        | g' < 1000 = 1000
        | otherwise =  round g'

        where

        g' = g * 10 ^ (6 :: Int)

-- The take/drop combinators above should be moved to filtering section.

{-# INLINE_NORMAL times #-}
times :: MonadAsync m => Double -> Stream m (AbsTime, RelTime64)
times g = Stream step Nothing

    where

    {-# INLINE_LATE step #-}
    step _ Nothing = do
        -- XXX note that this is safe only on a 64-bit machine. On a 32-bit
        -- machine a 64-bit 'Var' cannot be read consistently without a lock
        -- while another thread is writing to it.
        timeVar <- liftIO $ Prim.newIORef (0 :: Int64)
        liftIO $ updateTimeVar timeVar
        tid <- forkManaged $ liftIO $ forever (updateWithDelay g timeVar)
        a <- liftIO $ Prim.readIORef timeVar
        return $ Skip $ Just (timeVar, tid, a)

    step _ s@(Just (timeVar, _, t0)) = do
        a <- liftIO $ Prim.readIORef timeVar
        -- XXX we can perhaps use an AbsTime64 using a 64 bit Int for
        -- efficiency.  or maybe we can use a representation using Double for
        -- floating precision time
        return $ Yield (toAbsTime (MicroSecond64 t0),
            (toRelTime64 (MicroSecond64 (a - t0)))) s

-------------------------------------------------------------------------------
-- Generation by Conversion
-------------------------------------------------------------------------------

{-# INLINE_NORMAL fromIndicesM #-}
fromIndicesM :: Monad m => (Int -> m a) -> Stream m a
fromIndicesM gen = Stream step 0
  where
    {-# INLINE_LATE step #-}
    step _ i = do
       x <- gen i
       return $ Yield x (i + 1)

{-# INLINE fromIndices #-}
fromIndices :: Monad m => (Int -> a) -> Stream m a
fromIndices gen = fromIndicesM (return . gen)

{-# INLINE_NORMAL generateM #-}
generateM :: Monad m => Int -> (Int -> m a) -> Stream m a
generateM n gen = n `seq` Stream step 0
  where
    {-# INLINE_LATE step #-}
    step _ i | i < n     = do
                           x <- gen i
                           return $ Yield x (i + 1)
             | otherwise = return Stop

{-# INLINE generate #-}
generate :: Monad m => Int -> (Int -> a) -> Stream m a
generate n gen = generateM n (return . gen)

-- XXX we need the MonadAsync constraint because of a rewrite rule.
-- | Convert a list of monadic actions to a 'Stream'
{-# INLINE_LATE fromListM #-}
fromListM :: MonadAsync m => [m a] -> Stream m a
fromListM = Stream step
  where
    {-# INLINE_LATE step #-}
    step _ (m:ms) = m >>= \x -> return $ Yield x ms
    step _ []     = return Stop

{-# INLINE toStreamD #-}
toStreamD :: (K.IsStream t, Monad m) => t m a -> Stream m a
toStreamD = fromStreamK . K.toStream

{-# INLINE_NORMAL fromPrimIORef #-}
fromPrimIORef :: (MonadIO m, Prim a) => Prim.IORef a -> Stream m a
fromPrimIORef var = Stream step ()
  where
    {-# INLINE_LATE step #-}
    step _ () = liftIO (Prim.readIORef var) >>= \x -> return $ Yield x ()

-------------------------------------------------------------------------------
-- Generation from SVar
-------------------------------------------------------------------------------

data FromSVarState t m a =
      FromSVarInit
    | FromSVarRead (SVar t m a)
    | FromSVarLoop (SVar t m a) [ChildEvent a]
    | FromSVarDone (SVar t m a)

{-# INLINE_NORMAL fromSVar #-}
fromSVar :: (MonadAsync m) => SVar t m a -> Stream m a
fromSVar svar = Stream step FromSVarInit
    where

    {-# INLINE_LATE step #-}
    step _ FromSVarInit = do
        ref <- liftIO $ newIORef ()
        _ <- liftIO $ mkWeakIORef ref hook
        -- when this copy of svar gets garbage collected "ref" will get
        -- garbage collected and our GC hook will be called.
        let sv = svar{svarRef = Just ref}
        return $ Skip (FromSVarRead sv)

        where

        {-# NOINLINE hook #-}
        hook = do
            when (svarInspectMode svar) $ do
                r <- liftIO $ readIORef (svarStopTime (svarStats svar))
                when (isNothing r) $
                    printSVar svar "SVar Garbage Collected"
            cleanupSVar svar
            -- If there are any SVars referenced by this SVar a GC will prompt
            -- them to be cleaned up quickly.
            when (svarInspectMode svar) performMajorGC

    step _ (FromSVarRead sv) = do
        list <- readOutputQ sv
        -- Reversing the output is important to guarantee that we process the
        -- outputs in the same order as they were generated by the constituent
        -- streams.
        return $ Skip $ FromSVarLoop sv (Prelude.reverse list)

    step _ (FromSVarLoop sv []) = do
        done <- postProcess sv
        return $ Skip $ if done
                      then (FromSVarDone sv)
                      else (FromSVarRead sv)

    step _ (FromSVarLoop sv (ev : es)) = do
        case ev of
            ChildYield a -> return $ Yield a (FromSVarLoop sv es)
            ChildStop tid e -> do
                accountThread sv tid
                case e of
                    Nothing -> do
                        stop <- shouldStop tid
                        if stop
                        then do
                            liftIO (cleanupSVar sv)
                            return $ Skip (FromSVarDone sv)
                        else return $ Skip (FromSVarLoop sv es)
                    Just ex ->
                        case fromException ex of
                            Just ThreadAbort ->
                                return $ Skip (FromSVarLoop sv es)
                            Nothing -> liftIO (cleanupSVar sv) >> throwM ex
        where

        shouldStop tid =
            case svarStopStyle sv of
                StopNone -> return False
                StopAny -> return True
                StopBy -> do
                    sid <- liftIO $ readIORef (svarStopBy sv)
                    return $ if tid == sid then True else False

    step _ (FromSVarDone sv) = do
        when (svarInspectMode sv) $ do
            t <- liftIO $ getTime Monotonic
            liftIO $ writeIORef (svarStopTime (svarStats sv)) (Just t)
            liftIO $ printSVar sv "SVar Done"
        return Stop

-------------------------------------------------------------------------------
-- Process events received by a fold consumer from a stream producer
-------------------------------------------------------------------------------

{-# INLINE_NORMAL fromProducer #-}
fromProducer :: (MonadAsync m) => SVar t m a -> Stream m a
fromProducer svar = Stream step (FromSVarRead svar)
    where

    {-# INLINE_LATE step #-}
    step _ (FromSVarRead sv) = do
        list <- readOutputQ sv
        -- Reversing the output is important to guarantee that we process the
        -- outputs in the same order as they were generated by the constituent
        -- streams.
        return $ Skip $ FromSVarLoop sv (Prelude.reverse list)

    step _ (FromSVarLoop sv []) = return $ Skip $ FromSVarRead sv
    step _ (FromSVarLoop sv (ev : es)) = do
        case ev of
            ChildYield a -> return $ Yield a (FromSVarLoop sv es)
            ChildStop tid e -> do
                accountThread sv tid
                case e of
                    Nothing -> do
                        sendStopToProducer sv
                        return $ Skip (FromSVarDone sv)
                    Just _ -> error "Bug: fromProducer: received exception"

    step _ (FromSVarDone sv) = do
        when (svarInspectMode sv) $ do
            t <- liftIO $ getTime Monotonic
            liftIO $ writeIORef (svarStopTime (svarStats sv)) (Just t)
            liftIO $ printSVar sv "SVar Done"
        return Stop

    step _ FromSVarInit = undefined
