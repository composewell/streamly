-- |
-- Module      : Streamly.Internal.Data.Consumer.Type
-- Copyright   : (c) 2019 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- The 'Fold' type embeds a default initial value, therefore, it is like a
-- 'Monoid' whereas the 'Consumer' type has to be supplied with an initial
-- value, therefore, it is more like a 'Semigroup' operation.
--
-- See the file splitting example in the @streamly-examples@ repository for an
-- application of the 'Consumer' type. The 'Fold' type does not perform as well
-- in this situation.
--
-- 'Consumer' type is to 'Fold' as 'Unfold' type is to 'Stream'. Like 'Unfold'
-- provides better optimizaiton than stream in nested operations similarly
-- 'Consumer' provides better optimization than 'Fold'.
--
module Streamly.Internal.Data.Consumer.Type
    (
    -- * Types
      Consumer (..)

    -- Combinators
    , lmapM
    , rmapM

    -- Consumers
    , drainBy
    , take
    )
where

import Control.Monad ((>=>))
import Fusion.Plugin.Types (Fuse(..))
import Streamly.Internal.Data.Fold.Step (Step(..), mapMStep)

import Prelude hiding (take)

-- All folds in the Fold module should be implemented using Consumers.
--
-- | Like 'Fold' except that the initial state of the accmulator can be
-- generated using a dynamically supplied input. This affords better stream
-- fusion optimization in nested fold operations where the initial fold state
-- is determined based on a dynamic value.
--
-- /Internal/
data Consumer m c a b =
  -- | @Fold @ @ step @ @ inject @ @ extract@
  forall s. Consumer (s -> a -> m (Step s b)) (c -> m (Step s b)) (s -> m b)

------------------------------------------------------------------------------
-- Mapping on input
------------------------------------------------------------------------------

-- | @lmapM f fold@ maps the monadic function @f@ on the input of the fold.
--
-- /Internal/
{-# INLINE lmapM #-}
lmapM :: Monad m => (a -> m b) -> Consumer m c b r -> Consumer m c a r
lmapM f (Consumer step inject extract) = Consumer step1 inject extract

    where

    step1 x a = f a >>= step x

------------------------------------------------------------------------------
-- Mapping on the output
------------------------------------------------------------------------------

-- | Map a monadic function on the output of a fold.
--
-- /Internal/
{-# INLINE rmapM #-}
rmapM :: Monad m => (b -> m c) -> Consumer m x a b -> Consumer m x a c
rmapM f (Consumer step inject extract) = Consumer step1 inject1 (extract >=> f)

    where

    inject1 x = inject x >>= mapMStep f
    step1 s a = step s a >>= mapMStep f

------------------------------------------------------------------------------
-- Consumers
------------------------------------------------------------------------------

-- |
--
-- /Internal/
{-# INLINE drainBy #-}
drainBy ::  Monad m => (c -> a -> m b) -> Consumer m c a ()
drainBy f = Consumer step inject extract

    where

    inject = return . Partial

    step c a = f c a >> return (Partial c)

    extract _ = return ()

-- Required to fuse "take" with "many" in "chunksOf", for ghc-9.x
{-# ANN type Tuple'Fused Fuse #-}
data Tuple'Fused a b = Tuple'Fused !a !b deriving Show

-- | Take at most @n@ input elements and fold them using the supplied fold. A
-- negative count is treated as 0.
--
-- /Internal/
{-# INLINE take #-}
take :: Monad m => Int -> Consumer m x a b -> Consumer m x a b
take n (Consumer fstep finject fextract) = Consumer step inject extract

    where

    inject x = do
        res <- finject x
        case res of
            Partial s ->
                if n > 0
                then return $ Partial $ Tuple'Fused 0 s
                else Done <$> fextract s
            Done b -> return $ Done b

    step (Tuple'Fused i r) a = do
        res <- fstep r a
        case res of
            Partial sres -> do
                let i1 = i + 1
                    s1 = Tuple'Fused i1 sres
                if i1 < n
                then return $ Partial s1
                else Done <$> fextract sres
            Done bres -> return $ Done bres

    extract (Tuple'Fused _ r) = fextract r
