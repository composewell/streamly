-- |
-- Module      : Streamly.Internal.Data.Producer.Type
-- Copyright   : (c) 2021 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- See "Streamly.Internal.Data.Producer" for introduction.
--

module Streamly.Internal.Data.Producer.Type
    (
    -- * Type
    Producer (..)

    -- * Producers
    , nil
    , nilM
    , unfoldrM
    , fromList

    -- * Mapping
    , translate
    , lmap

    -- * Nesting
    , NestedLoop (..)
    , concat
    )
where

#include "inline.hs"

import Fusion.Plugin.Types (Fuse(..))
import Streamly.Internal.Data.Stream.Step (Step(..))
import Prelude hiding (concat, map)

------------------------------------------------------------------------------
-- Type
------------------------------------------------------------------------------

-- | A @Producer m a b@ is a generator of a stream of values of type @b@ from a
-- seed of type 'a' in 'Monad' @m@.
--
-- /Pre-release/

data Producer m a b =
    -- | @Producer step inject extract@
    forall s. Producer (s -> m (Step s b)) (a -> m s) (s -> m a)

------------------------------------------------------------------------------
-- Producers
------------------------------------------------------------------------------

{-# INLINE nilM #-}
nilM :: Monad m => (a -> m c) -> Producer m a b
nilM f = Producer step return return

    where

    {-# INLINE_LATE step #-}
    step x = f x >> return Stop

{-# INLINE nil #-}
nil :: Monad m => Producer m a b
nil = nilM (\_ -> return ())

{-# INLINE unfoldrM #-}
unfoldrM :: Monad m => (a -> m (Maybe (b, a))) -> Producer m a b
unfoldrM next = Producer step return return

    where

    {-# INLINE_LATE step #-}
    step st = do
        r <- next st
        return $ case r of
            Just (x, s) -> Yield x s
            Nothing -> Stop

-- | Convert a list of pure values to a 'Stream'
--
-- /Pre-release/
{-# INLINE_LATE fromList #-}
fromList :: Monad m => Producer m [a] a
fromList = Producer step return return

    where

    {-# INLINE_LATE step #-}
    step (x:xs) = return $ Yield x xs
    step [] = return Stop

------------------------------------------------------------------------------
-- Mapping
------------------------------------------------------------------------------

-- | Interconvert the producer between two interconvertible input types.
--
-- /Pre-release/
{-# INLINE_NORMAL translate #-}
translate :: Functor m =>
    (a -> c) -> (c -> a) -> Producer m c b -> Producer m a b
translate f g (Producer step inject extract) =
    Producer step (inject . f) (fmap g . extract)

-- | Map the producer input to another value of the same type.
--
-- /Pre-release/
{-# INLINE_NORMAL lmap #-}
lmap :: (a -> a) -> Producer m a b -> Producer m a b
lmap f (Producer step inject extract) = Producer step (inject . f) extract

------------------------------------------------------------------------------
-- Functor
------------------------------------------------------------------------------

-- | Map a function on the output of the producer (the type @b@).
--
-- /Pre-release/
{-# INLINE_NORMAL map #-}
map :: Functor m => (b -> c) -> Producer m a b -> Producer m a c
map f (Producer ustep uinject uextract) = Producer step uinject uextract

    where

    {-# INLINE_LATE step #-}
    step st = fmap (fmap f) (ustep st)

-- | Maps a function on the output of the producer (the type @b@).
instance Functor m => Functor (Producer m a) where
    {-# INLINE fmap #-}
    fmap = map

------------------------------------------------------------------------------
-- Nesting
------------------------------------------------------------------------------

-- | State representing a nested loop.
{-# ANN type NestedLoop Fuse #-}
data NestedLoop s1 s2 = OuterLoop s1 | InnerLoop s1 s2

-- | Apply the second unfold to each output element of the first unfold and
-- flatten the output in a single stream.
--
-- /Pre-release/
--
{-# INLINE_NORMAL concat #-}
concat :: Monad m =>
    Producer m a b -> Producer m b c -> Producer m (NestedLoop a b) c
concat (Producer step1 inject1 extract1) (Producer step2 inject2 extract2) =
    Producer step inject extract

    where

    inject (OuterLoop x) = do
        s <- inject1 x
        return $ OuterLoop s
    inject (InnerLoop x y) = do
        s1 <- inject1 x
        s2 <- inject2 y
        return $ InnerLoop s1 s2

    {-# INLINE_LATE step #-}
    step (OuterLoop st) = do
        r <- step1 st
        case r of
            Yield x s -> do
                innerSt <- inject2 x
                return $ Skip (InnerLoop s innerSt)
            Skip s    -> return $ Skip (OuterLoop s)
            Stop      -> return Stop

    step (InnerLoop ost ist) = do
        r <- step2 ist
        return $ case r of
            Yield x s -> Yield x (InnerLoop ost s)
            Skip s    -> Skip (InnerLoop ost s)
            Stop      -> Skip (OuterLoop ost)

    extract (OuterLoop s1) = OuterLoop <$> extract1 s1
    extract (InnerLoop s1 s2) = do
        r1 <- extract1 s1
        r2 <- extract2 s2
        return (InnerLoop r1 r2)
