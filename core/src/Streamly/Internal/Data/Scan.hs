-- |
-- Module      : Streamly.Internal.Data.Scan
-- Copyright   : (c) 2019 Composewell Technologies
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

module Streamly.Internal.Data.Scan
    (
    -- * Type
      Step (..)
    , Scan (..)

    -- * Primitive Scans
    , identity
    , map -- function?
    , mapM -- functionM?
    , filter
    , filterM

    -- * Combinators
    , compose
    , teeWithMay
    , teeWith -- zipWith -- teeZip
    )
where

#include "inline.hs"
import Control.Arrow (Arrow(..))
import Control.Category (Category(..))
import Data.Functor ((<&>))
import Data.Maybe (isJust, fromJust)
import Fusion.Plugin.Types (Fuse(..))
import Streamly.Internal.Data.Tuple.Strict (Tuple'(..))

import qualified Prelude

import Prelude hiding (filter, zipWith, map, mapM, id, unzip, null)

-- $setup
-- >>> :m
-- >>> :set -XFlexibleContexts
-- >>> import Control.Category
--
-- >>> import qualified Streamly.Internal.Data.Fold as Fold
-- >>> import qualified Streamly.Internal.Data.Scan as Scan
-- >>> import qualified Streamly.Internal.Data.Stream as Stream

------------------------------------------------------------------------------
-- Scans
------------------------------------------------------------------------------

-- A Scan is half the pipe:
--
-- A scan is a simpler version of pipes. A scan always consumes and input and
-- may or may not produce an output. It can produce at most one output on one
-- input. Whereas a pipe may produce output even without consuming anything or
-- it can produce multiple outputs on a single input. Scans are simpler
-- abstractions to think about and easier for the compiler to optimize.

-- What kind of compositions are possible with scans?
--
-- Append: this is the easiest. The behavior is simple even in presence of
-- filtering (Skip) and termination (Stop). Skip translates to Skip, Stop
-- translates to Stop.
--
-- demux: we select one of n scans to run. Behaviour with Skip is straight
-- forward. Termination behavior has multiple options, stop when first one
-- stops, stop when the last one stops, or stop when a selected one stops.
--
-- zip: run all and zip the outputs. If one of them Skips we Skip the output.
-- If one of them stops we stop. It may be possible to collect the outputs as
-- Just/Nothing values.
--
-- Another option could be if a Scan terminates do we want to start it again or
-- not.
--
{-# ANN type Step Fuse #-}
data Step s b =
      Yield s b -- ^ Yield and consume
    | Skip s -- ^ Skip and consume
    | Stop

instance Functor (Step s) where
    {-# INLINE_NORMAL fmap #-}
    fmap f (Yield s b) = Yield s (f b)
    fmap _ (Skip s) = Skip s
    fmap _ Stop = Stop

-- | Represents a stateful transformation over an input stream of values of
-- type @a@ to outputs of type @b@ in 'Monad' @m@.
--
-- The constructor is @Scan consume initial@.
data Scan m a b =
    forall s. Scan
        (s -> a -> m (Step s b))
        s

------------------------------------------------------------------------------
-- Functor: Mapping on the output
------------------------------------------------------------------------------

-- | 'fmap' maps a pure function on a scan output.
--
-- >>> Stream.toList $ Stream.runScan (fmap (+1) Scan.identity) $ Stream.fromList [1..5::Int]
-- [2,3,4,5,6]
--
instance Functor m => Functor (Scan m a) where
    {-# INLINE_NORMAL fmap #-}
    fmap f (Scan consume initial) = Scan consume1 initial

        where

        {-# INLINE_LATE consume1 #-}
        consume1 s b = fmap (fmap f) (consume s b)

-------------------------------------------------------------------------------
-- Category
-------------------------------------------------------------------------------

-- | Connect two scans in series. The second scan is the input end, and the
-- first scan is the output end.
--
-- >>> import Control.Category
-- >>> Stream.toList $ Stream.runScan (Scan.map (+1) >>> Scan.map (+1)) $ Stream.fromList [1..5::Int]
-- [3,4,5,6,7]
--
{-# INLINE_NORMAL compose #-}
compose :: Monad m => Scan m b c -> Scan m a b -> Scan m a c
compose
    (Scan stepR initialR)
    (Scan stepL initialL) = Scan step (initialL, initialR)

    where

    -- XXX Use strict tuple?
    step (sL, sR) x = do
        rL <- stepL sL x
        case rL of
            Yield sL1 bL -> do
                rR <- stepR sR bL
                return
                    $ case rR of
                        Yield sR1 br -> Yield (sL1, sR1) br
                        Skip sR1 -> Skip (sL1, sR1)
                        Stop -> Stop
            Skip sL1 -> return $ Skip (sL1, sR)
            Stop -> return Stop

-- | A scan representing mapping of a monadic action.
--
-- >>> Stream.toList $ Stream.runScan (Scan.mapM print) $ Stream.fromList [1..5::Int]
-- 1
-- 2
-- 3
-- 4
-- 5
-- [(),(),(),(),()]
--
{-# INLINE mapM #-}
mapM :: Monad m => (a -> m b) -> Scan m a b
mapM f = Scan (\() a -> f a <&> Yield ()) ()

-- | A scan representing mapping of a pure function.
--
-- >>> Stream.toList $ Stream.runScan (Scan.map (+1)) $ Stream.fromList [1..5::Int]
-- [2,3,4,5,6]
--
{-# INLINE map #-}
map :: Monad m => (a -> b) -> Scan m a b
map f = mapM (return Prelude.. f)

{- HLINT ignore "Redundant map" -}

-- | An identity scan producing the same output as input.
--
-- >>> identity = Scan.map Prelude.id
--
-- >>> Stream.toList $ Stream.runScan (Scan.identity) $ Stream.fromList [1..5::Int]
-- [1,2,3,4,5]
--
{-# INLINE identity #-}
identity :: Monad m => Scan m a a
identity = map Prelude.id

instance Monad m => Category (Scan m) where
    {-# INLINE id #-}
    id = identity

    {-# INLINE (.) #-}
    (.) = compose

{-# ANN type TeeWith Fuse #-}
data TeeWith sL sR = TeeWith !sL !sR

-- XXX zipWith?

-- | Connect two scans in parallel. Distribute the input across two scans and
-- merge their outputs as soon as they become available. Note that a scan may
-- not generate output on each input, it might filter it.
--
-- >>> Stream.toList $ Stream.runScan (Scan.teeWithMay (,) Scan.identity (Scan.map (\x -> x * x))) $ Stream.fromList [1..5::Int]
-- [(Just 1,Just 1),(Just 2,Just 4),(Just 3,Just 9),(Just 4,Just 16),(Just 5,Just 25)]
--
{-# INLINE_NORMAL teeWithMay #-}
teeWithMay :: Monad m =>
    (Maybe b -> Maybe c -> d) -> Scan m a b -> Scan m a c -> Scan m a d
teeWithMay f (Scan stepL initialL) (Scan stepR initialR) =
    Scan step (TeeWith initialL initialR)

    where

    -- XXX Use strict tuple?
    step (TeeWith sL sR) a = do
        resL <- stepL sL a
        resR <- stepR sR a
        return
            $ case resL of
                  Yield sL1 bL ->
                    case resR of
                        Yield sR1 bR ->
                            Yield (TeeWith sL1 sR1) (f (Just bL) (Just bR))
                        Skip sR1 ->
                            Yield (TeeWith sL1 sR1) (f (Just bL) Nothing)
                        Stop -> Stop
                  Skip sL1 ->
                    case resR of
                        Yield sR1 bR ->
                            Yield (TeeWith sL1 sR1) (f Nothing (Just bR))
                        Skip sR1 ->
                            Yield (TeeWith sL1 sR1) (f Nothing Nothing)
                        Stop -> Stop
                  Stop -> Stop

-- | Produces an output only when both the scans produce an output.
--
-- >>> Stream.toList $ Stream.runScan (Scan.teeWith (,) Scan.identity (Scan.map (\x -> x * x))) $ Stream.fromList [1..5::Int]
-- [Just (1,1),Just (2,4),Just (3,9),Just (4,16),Just (5,25)]
--
{-# INLINE_NORMAL teeWith #-}
teeWith :: Monad m =>
    (b -> c -> d) -> Scan m a b -> Scan m a c -> Scan m a d
teeWith f s1 s2 =
    fmap fromJust
        $ compose (filter isJust)
        $ teeWithMay (\b c -> f <$> b <*> c) s1 s2

-------------------------------------------------------------------------------
-- Arrow
-------------------------------------------------------------------------------

{-# INLINE_NORMAL unzipMay #-}
unzipMay :: Monad m =>
    Scan m a x -> Scan m b y -> Scan m (a, b) (Maybe x, Maybe y)
unzipMay (Scan stepL initialL) (Scan stepR initialR) =
    Scan step (Tuple' initialL initialR)

    where

    step (Tuple' sL sR) (a, b) = do
        resL <- stepL sL a
        resR <- stepR sR b
        return
            $ case resL of
                  Yield sL1 bL ->
                    case resR of
                        Yield sR1 bR ->
                            Yield (Tuple' sL1 sR1) (Just bL, Just bR)
                        Skip sR1 ->
                            Yield (Tuple' sL1 sR1) (Just bL, Nothing)
                        Stop -> Stop
                  Skip sL1 ->
                    case resR of
                        Yield sR1 bR ->
                            Yield (Tuple' sL1 sR1) (Nothing, Just bR)
                        Skip sR1 ->
                            Yield (Tuple' sL1 sR1) (Nothing, Nothing)
                        Stop -> Stop
                  Stop -> Stop

-- | Produces an output only when both the scans produce an output.
{-# INLINE_NORMAL unzip #-}
unzip :: Monad m => Scan m a x -> Scan m b y -> Scan m (a, b) (x, y)
unzip s1 s2 = fmap (fromJust Prelude.. f) $ unzipMay s1 s2

    where

    f (mx, my) =
        case mx of
            Just x ->
                case my of
                    Just y -> Just (x, y)
                    Nothing -> Nothing
            Nothing -> Nothing

instance Monad m => Applicative (Scan m a) where
    {-# INLINE pure #-}
    pure b = Scan (\_ _ -> pure $ Yield () b) ()

    (<*>) = teeWith id

instance Monad m => Arrow (Scan m) where
    {-# INLINE arr #-}
    arr = map

    {-# INLINE (***) #-}
    (***) = unzip

    {-# INLINE (&&&) #-}
    (&&&) = teeWith (,)

-------------------------------------------------------------------------------
-- Primitive scans
-------------------------------------------------------------------------------

-- | A filtering scan using a monadic predicate.
{-# INLINE filterM #-}
filterM :: Monad m => (a -> m Bool) -> Scan m a a
filterM f = Scan (\() a -> f a >>= g a) ()

    where

    {-# INLINE g #-}
    g a b =
        return
            $ if b
              then Yield () a
              else Skip ()

-- | A filtering scan using a pure predicate.
--
-- >>> Stream.toList $ Stream.runScan (Scan.filter odd) $ Stream.fromList [1..5::Int]
-- [1,3,5]
--
{-# INLINE filter #-}
filter :: Monad m => (a -> Bool) -> Scan m a a
filter f = filterM (return Prelude.. f)
