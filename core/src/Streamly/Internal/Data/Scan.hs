-- |
-- Module      : Streamly.Internal.Data.Pipe.Scan
-- Copyright   : (c) 2021 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Unfold
-- Stream - combine multiple producers
-- Scan - combine multiple transformations
-- Pipe - combine multiple transformations, a more powerful scan
-- Fold - combine multiple consumers
-- Parser - a more powerful fold
-- Refold
--
-- Stateful transformations with termination.  A Scan can be used on a stream,
-- after an unfold or before a fold.  A scan has to consume, and a stream
-- cannot consume. A scan cannot be used to combine multiple producers and a
-- stream cannot be used to combine multiple consumers.

module Streamly.Internal.Data.Pipe.Scan
    (
      Scan (..)
    , rmapM
    , fromPure
    , fromEffect
    )
where

#include "inline.hs"
-- import Control.Category (Category(..))
import Control.Monad ((>=>))
import Control.Monad.Catch (MonadCatch, try, throwM, MonadThrow)

-- import qualified Streamly.Internal.Data.Stream.StreamD as D
-- import qualified Streamly.Internal.Data.Fold.Type as FL
-- import Streamly.Internal.Data.SVar (adaptState)
-- import Streamly.Internal.Data.Stream.Serial (SerialT)
-- import Streamly.Internal.Data.Tuple.Strict (Tuple'(..))
import Fusion.Plugin.Types (Fuse(..))

-------------------------------------------------------------------------------
-- Scan step
-------------------------------------------------------------------------------

-- XXX Folds as well as scans can have all four constructors except that they
-- won't do backtracking. Tee's won't have the Continue contructor as it can
-- result in non-uniform output from the Tee branches. We can have a Tee for
-- pipes for the same, that is no Continue so that scans can be zipped
-- together.

{-# ANN type Step Fuse #-}
data Step s b =
      Partial !s -- in initial, extract on Partial can be used for scan instead of postscan
    | Continue !s -- in initial, Continue can be used for postscan, instead of scan
    | Done !b -- in initial this can be used for `take 0` type of use case
 --  Error String

instance Functor (Step s) where
    {-# INLINE fmap #-}
    fmap _ (Partial s) = Partial s
    fmap _ (Continue s) = Continue s
    fmap f (Done b) = Done (f b)

-- | Map a monadic function over the result @b@ in @Step s b@.
--
-- /Internal/
{-# INLINE mapMStep #-}
mapMStep :: Applicative m => (a -> m b) -> Step s a -> m (Step s b)
mapMStep f res =
    case res of
        Partial s -> pure $ Partial s
        Done b -> Done <$> f b
        Continue s -> pure $ Continue s

-------------------------------------------------------------------------------
-- Scan
-------------------------------------------------------------------------------

-- XXX extract can possibly return Done, Partial or Incomplete.
data Scan m a b =
    -- | @Scan @ @ step @ @ initial @ @ extract@
    forall s. Scan (s -> a -> m (Step s b)) (m (Step s b)) (s -> m b)

------------------------------------------------------------------------------
-- Mapping on the output
------------------------------------------------------------------------------

instance Functor m => Functor (Scan m a) where
    {-# INLINE fmap #-}
    fmap f (Scan step1 initial1 extract) =
        Scan step initial (fmap2 f extract)

        where

        initial = fmap2 f initial1
        step s b = fmap2 f (step1 s b)
        fmap2 g = fmap (fmap g)

-- | Map a monadic function on the output of a scan.
--
-- /Pre-release/
{-# INLINE rmapM #-}
rmapM :: Monad m => (b -> m c) -> Scan m a b -> Scan m a c
rmapM f (Scan step initial extract) = Scan step1 initial1 (extract >=> f)

    where

    initial1 = do
        res <- initial
        mapMStep f res
    step1 s a = step s a >>= mapMStep f

-- |
--
-- /Pre-release/
--
{-# INLINE_NORMAL fromPure #-}
fromPure :: Monad m => b -> Scan m a b
fromPure b = Scan undefined (pure $ Done b) undefined

-- |
--
-- /Pre-release/
--
{-# INLINE fromEffect #-}
fromEffect :: Monad m => m b -> Scan m a b
fromEffect b = Scan undefined (Done <$> b) undefined

-- XXX See the ParserD version to implement this.
--
-- | Start the next scan from where the previous scan left off. We may need a
-- CPS version for better scaling, or may need a Array stream scan.
--
-- /Unimplemented/
{-# INLINE serialWith #-}
serialWith :: -- MonadThrow m =>
    (a -> b -> c) -> Scan m x a -> Scan m x b -> Scan m x c
serialWith = undefined

teeZip = undefined
teeMerge = undefined
compose = undefined

{-
instance Monad m => Applicative (Scan m a) where
    pure b = Scan (\_ _ -> pure (Tuple' () b)) ()

    -- (<*>) :: Scan m a (b -> c) -> Scan m a b -> Scan m a c
    Scan stepL initialL <*> Scan stepR initialR =
        Scan step initial
      where
        initial = Tuple' initialL initialR
        step (Tuple' xL xR) a = do
          Tuple' sL bcL <- stepL xL a
          Tuple' sR bR  <- stepR xR a
          pure $ Tuple' (Tuple' sL sR) (bcL bR)

dot :: Monad m => Scan m b c -> Scan m a b -> Scan m a c
Scan stepL initialL `dot` Scan stepR initialR = Scan step initial
  where
    initial = Tuple' initialR initialL
    step (Tuple' sa sb) a = do
      Tuple' sa' b <- stepR sa a
      Tuple' sb' c <- stepL sb b
      return $ Tuple' (Tuple' sa' sb') c

instance Monad m => Category (Scan m) where
    -- id :: Scan m a a
    id = Scan (\_ a -> return (Tuple' () a)) ()

    (.) = dot

mapM :: Monad m => (b -> m c) -> Scan m a b -> Scan m a c
mapM f (Scan step initial) = Scan step' initial
  where
    step' s a = do
        Tuple' sb b <- step s a
        Tuple' sb <$> f b

lmapM :: Monad m => (a -> m b) -> Scan m b c -> Scan m a c
lmapM f (Scan step initial) = Scan step' initial
  where
    step' s a = do
        b <- f a
        step s b

lmap :: Monad m => (a -> b) -> Scan m b c -> Scan m a c
lmap f = lmapM (return Prelude.. f)

-- scan :: (IsStream t, Monad m) => Scan m a b -> t m a -> t m b
-- scan s = D.fromStreamD Prelude.. scanD s Prelude.. D.toStreamD

{-
scanD :: Monad m => Scan m a b -> D.Stream m a -> D.Stream m b
scanD (Scan scan_step initial) (D.UnStream stream_step stream_state) =
    D.UnStream step (stream_state, initial)
  where
    step gst (st, acc) = do
        r <- stream_step (adaptState gst) st
        case r of
            D.Yield x s -> do
                Tuple' acc' b <- scan_step acc x
                return $ D.Yield b (s, acc')
            D.Skip s -> return $ D.Skip (s, acc)
            D.Stop -> return D.Stop
            -}

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
        -}
