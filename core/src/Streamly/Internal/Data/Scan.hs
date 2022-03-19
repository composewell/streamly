-- |
-- Module      : Streamly.Internal.Data.Scan
-- Copyright   : (c) 2021 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Stateful transformations with filtering and termination.  A Scan can be used
-- on the output of a stream or on the input of a fold.
--
-- This is an experimental module. Scans can be combined with folds by adding
-- two additional constructors that skip producing output.

module Streamly.Internal.Data.Scan
    (
    -- * Type
      Step (..)
    , Scan (..)

    -- * Combinators
    , compose

    -- * Scans
    , map
    , mapM
    , filter
    )
where

#include "inline.hs"
import Control.Category (Category(..))
import Data.Functor ((<&>))
import Fusion.Plugin.Types (Fuse(..))

import Prelude hiding (map, mapM, filter)

-------------------------------------------------------------------------------
-- Scan step
-------------------------------------------------------------------------------

{-# ANN type Step Fuse #-}
data Step s b =
      Partial !s !b -- for transformation
    | Done !b       -- for termination with output
    | Continue !s   -- for filtering
    | Stop          -- termination without output (empty stream)

instance Functor (Step s) where
    {-# INLINE fmap #-}
    fmap f (Partial s b) = Partial s (f b)
    fmap f (Done b) = Done (f b)
    fmap _ (Continue s) = Continue s
    fmap _ Stop = Stop

-------------------------------------------------------------------------------
-- Scan
-------------------------------------------------------------------------------

-- Scans are postscan only, therefore, output is not allowed in the initial
-- action.  Initial type can avoid 'm' but we need this to be able to convert
-- folds to scans.
data Scan m a b =
    -- | @Scan@ @step@ @initial@
    forall s. Scan (s -> a -> m (Step s b)) (m (Maybe s))

------------------------------------------------------------------------------
-- Functor: Mapping on the output
------------------------------------------------------------------------------

instance Functor m => Functor (Scan m a) where
    {-# INLINE fmap #-}
    fmap f (Scan step1 initial) = Scan step initial

        where

        step s b = fmap (fmap f) (step1 s b)

-------------------------------------------------------------------------------
-- Category
-------------------------------------------------------------------------------

-- | postscan a scan
{-# INLINE compose #-}
compose :: Monad m => Scan m b c -> Scan m a b -> Scan m a c
compose (Scan stepR initialR) (Scan stepL initialL) = Scan step initial

    where

    {-# INLINE runStep #-}
    runStep actionL sR = do
        rL <- actionL
        case rL of
            Done bL -> do
                rR <- stepR sR bL
                return
                    $ case rR of
                        Partial _ br -> Done br
                        Done bR -> Done bR
                        Continue _ -> Stop
                        Stop -> Stop
            Partial sL bL -> do
                rR <- stepR sR bL
                return
                    $ case rR of
                        Partial sR1 br -> Partial (sL, sR1) br
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

-- | A scan representing mapping of a monadic action.
{-# INLINE mapM #-}
mapM :: Monad m => (a -> m b) -> Scan m a b
mapM f = Scan (\() a -> f a <&> Partial ()) (return $ Just ())

-- | A scan representing mapping of a pure function.
{-# INLINE map #-}
map :: Monad m => (a -> b) -> Scan m a b
map f = mapM (return Prelude.. f)

-- | An identity scan producing the same output as input..
{-# INLINE identity #-}
identity :: Monad m => Scan m a a
identity = map Prelude.id

instance Monad m => Category (Scan m) where
    id = identity

    (.) = compose

-------------------------------------------------------------------------------
-- Scans
-------------------------------------------------------------------------------

-- | A filtering scan.
{-# INLINE filter #-}
filter :: Monad m => (a -> m Bool) -> Scan m a a
filter f = Scan (\() a -> f a >>= g a) (return $ Just ())

    where

    {-# INLINE g #-}
    g a b =
        return
            $ if b
              then Partial () a
              else Continue ()
