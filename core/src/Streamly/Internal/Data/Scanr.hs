-- |
-- Module      : Streamly.Internal.Data.Scanr
-- Copyright   : (c) 2019 Composewell Technologies
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Scans vs pipes:
--
-- A scan is a simpler version of pipes. A scan always consumes an input and
-- may or may not produce an output. It can produce at most one output on one
-- input. Whereas a pipe may produce output even without consuming anything or
-- it can produce multiple outputs on a single input. Scans are simpler
-- abstractions to think about and easier for the compiler to optimize.
--
-- Scanr vs Stream
--
-- A Scan m () a can be converted into a Stream m a by supplying the Scan with
-- () inputs.
--
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

module Streamly.Internal.Data.Scanr
    (
    -- * Type
      Scan (..)

    -- * Primitive Scans
    , identity
    , function
    , functionM
    , filter
    , filterM

    -- * Combinators
    , compose
    , teeWithMay
    , teeWith
    )
where

#include "inline.hs"
import Control.Arrow (Arrow(..))
import Control.Category (Category(..))
import Data.Maybe (isJust, fromJust)
import Fusion.Plugin.Types (Fuse(..))
import Streamly.Internal.Data.Tuple.Strict (Tuple'(..))
import Streamly.Internal.Data.Stream.Step (Step (..))

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

-- A core difference between the Scan type and the Fold type is that Scan can
-- stop without producing an output, this is required to represent an empty
-- stream. For this reason a Scan cannot be represented using a Fold because a
-- fold requires a value to be produced on Stop.

-- A core difference between a Scan and a Stream is that a scan produces an
-- output only on an input while a stream can produce output without consuming
-- an input.
--
-- XXX A scan may have buffered data which may have to be drained if the driver
-- has no more input to supply. So we need a finalizer which produces a
-- (possibly empty) stream.
--
-- XXX We should add finalizer (and Error constructor?) to it before we
-- release it.

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

-- XXX We can call this append, because corresponding operation in stream is
-- also append.

-- | Connect two scans in series. Attach the first scan on the output of the
-- second scan.
--
-- >>> import Control.Category
-- >>> Stream.toList $ Stream.runScan (Scan.function (+1) >>> Scan.function (+1)) $ Stream.fromList [1..5::Int]
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
            Yield bL sL1 -> do
                rR <- stepR sR bL
                return
                    $ case rR of
                        Yield br sR1 -> Yield br (sL1, sR1)
                        Skip sR1 -> Skip (sL1, sR1)
                        Stop -> Stop
            Skip sL1 -> return $ Skip (sL1, sR)
            Stop -> return Stop

-- | A scan representing mapping of a monadic action.
--
-- >>> Stream.toList $ Stream.runScan (Scan.functionM print) $ Stream.fromList [1..5::Int]
-- 1
-- 2
-- 3
-- 4
-- 5
-- [(),(),(),(),()]
--
{-# INLINE functionM #-}
functionM :: Monad m => (a -> m b) -> Scan m a b
functionM f = Scan (\() a -> fmap (\x -> Yield x ()) (f a)) ()

-- | A scan representing mapping of a pure function.
--
-- >>> Stream.toList $ Stream.runScan (Scan.function (+1)) $ Stream.fromList [1..5::Int]
-- [2,3,4,5,6]
--
{-# INLINE function #-}
function :: Monad m => (a -> b) -> Scan m a b
function f = functionM (return Prelude.. f)

{- HLINT ignore "Redundant map" -}

-- | An identity scan producing the same output as input.
--
-- >>> identity = Scan.function Prelude.id
--
-- >>> Stream.toList $ Stream.runScan (Scan.identity) $ Stream.fromList [1..5::Int]
-- [1,2,3,4,5]
--
{-# INLINE identity #-}
identity :: Monad m => Scan m a a
identity = function Prelude.id

instance Monad m => Category (Scan m) where
    {-# INLINE id #-}
    id = identity

    {-# INLINE (.) #-}
    (.) = compose

-------------------------------------------------------------------------------
-- Applicative Zip
-------------------------------------------------------------------------------

{-# ANN type TeeWith Fuse #-}
data TeeWith sL sR = TeeWith !sL !sR

-- XXX zipWith?

-- | Connect two scans in parallel. Distribute the input across two scans and
-- zip their outputs. If the scan filters the output, 'Nothing' is emitted
-- otherwise 'Just' is emitted. The scan stops if any of the scans stop.
--
-- >>> Stream.toList $ Stream.runScan (Scan.teeWithMay (,) Scan.identity (Scan.function (\x -> x * x))) $ Stream.fromList [1..5::Int]
-- [(Just 1,Just 1),(Just 2,Just 4),(Just 3,Just 9),(Just 4,Just 16),(Just 5,Just 25)]
--
{-# INLINE_NORMAL teeWithMay #-}
teeWithMay :: Monad m =>
    (Maybe b -> Maybe c -> d) -> Scan m a b -> Scan m a c -> Scan m a d
teeWithMay f (Scan stepL initialL) (Scan stepR initialR) =
    Scan step (TeeWith initialL initialR)

    where

    step (TeeWith sL sR) a = do
        resL <- stepL sL a
        resR <- stepR sR a
        return
            $ case resL of
                  Yield bL sL1 ->
                    case resR of
                        Yield bR sR1 ->
                            Yield
                                (f (Just bL) (Just bR))
                                (TeeWith sL1 sR1)
                        Skip sR1 ->
                            Yield
                                (f (Just bL) Nothing)
                                (TeeWith sL1 sR1)
                        Stop -> Stop
                  Skip sL1 ->
                    case resR of
                        Yield bR sR1 ->
                            Yield
                                (f Nothing (Just bR))
                                (TeeWith sL1 sR1)
                        Skip sR1 ->
                            Yield
                                (f Nothing Nothing)
                                (TeeWith sL1 sR1)
                        Stop -> Stop
                  Stop -> Stop

-- | Produces an output only when both the scans produce an output. If any of
-- the scans skips the output then the composed scan also skips. Stops when any
-- of the scans stop.
--
-- >>> Stream.toList $ Stream.runScan (Scan.teeWith (,) Scan.identity (Scan.function (\x -> x * x))) $ Stream.fromList [1..5::Int]
-- [(1,1),(2,4),(3,9),(4,16),(5,25)]
--
{-# INLINE_NORMAL teeWith #-}
teeWith :: Monad m =>
    (b -> c -> d) -> Scan m a b -> Scan m a c -> Scan m a d
teeWith f s1 s2 =
    fmap fromJust
        $ compose (filter isJust)
        $ teeWithMay (\b c -> f <$> b <*> c) s1 s2

-- | Zips the outputs only when both scans produce outputs, discards otherwise.
instance Monad m => Applicative (Scan m a) where
    {-# INLINE pure #-}
    pure b = Scan (\_ _ -> pure $ Yield b ()) ()

    (<*>) = teeWith id

-------------------------------------------------------------------------------
-- Arrow
-------------------------------------------------------------------------------

-- | Use the first scan for the first element of the tuple and second scan for
-- the second. Zip the outputs. Emits 'Nothing' if no output is generated by
-- the scan, otherwise emits 'Just'. Stops as soon as any one of the scans
-- stop.
--
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
                  Yield bL sL1 ->
                    case resR of
                        Yield bR sR1 ->
                            Yield
                                (Just bL, Just bR)
                                (Tuple' sL1 sR1)
                        Skip sR1 ->
                            Yield
                                (Just bL, Nothing)
                                (Tuple' sL1 sR1)
                        Stop -> Stop
                  Skip sL1 ->
                    case resR of
                        Yield bR sR1 ->
                            Yield
                                (Nothing, Just bR)
                                (Tuple' sL1 sR1)
                        Skip sR1 ->
                            Yield
                                (Nothing, Nothing)
                                (Tuple' sL1 sR1)
                        Stop -> Stop
                  Stop -> Stop

-- | Like 'unzipMay' but produces an output only when both the scans produce an
-- output. Other outputs are filtered out.
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

instance Monad m => Arrow (Scan m) where
    {-# INLINE arr #-}
    arr = function

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
              then Yield a ()
              else Skip ()

-- | A filtering scan using a pure predicate.
--
-- >>> Stream.toList $ Stream.runScan (Scan.filter odd) $ Stream.fromList [1..5::Int]
-- [1,3,5]
--
{-# INLINE filter #-}
filter :: Monad m => (a -> Bool) -> Scan m a a
filter f = filterM (return Prelude.. f)
