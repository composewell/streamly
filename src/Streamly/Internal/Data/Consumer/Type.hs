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
-- Consumers can be appended to each other or to a fold to build the fold
-- incrementally. This is useful in incremental builder like use cases.
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

    -- * Constructors
    , foldl'

    -- * Consumers
    -- ** Accumulators
    , sconcat
    , drainBy
    , iterate

    -- * Combinators
    , lmapM
    , rmapM
    , append
    , take
    )
where

import Control.Monad ((>=>))
#if __GLASGOW_HASKELL__ < 804
import Data.Semigroup (Semigroup((<>)))
#endif
import Fusion.Plugin.Types (Fuse(..))
import Streamly.Internal.Data.Fold.Step (Step(..), mapMStep)

import Prelude hiding (take, iterate)

-- $setup
-- >>> :m
-- >>> import qualified Streamly.Internal.Data.Consumer.Type as Consumer
-- >>> import qualified Streamly.Internal.Data.Fold.Type as Fold
-- >>> import qualified Streamly.Internal.Data.Stream.IsStream as Stream

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
-- Left fold constructors
------------------------------------------------------------------------------

-- | Make a consumer from a left fold style pure step function.
--
-- If your 'Fold' returns only 'Partial' (i.e. never returns a 'Done') then you
-- can use @foldl'*@ constructors.
--
-- See also: @Streamly.Prelude.foldl'@
--
-- /Internal/
--
{-# INLINE foldl' #-}
foldl' :: Monad m => (b -> a -> b) -> Consumer m b a b
foldl' step =
    Consumer
        (\s a -> return $ Partial $ step s a)
        (return . Partial)
        return

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

------------------------------------------------------------------------------
-- Semigroup
------------------------------------------------------------------------------

-- | Append the elements of an input stream to a provided starting value.
--
-- >>> stream = Stream.map Data.Monoid.Sum $ Stream.enumerateFromTo 1 10
-- >>> Stream.fold (Fold.fromConsumer Consumer.sconcat 10) stream
-- Sum {getSum = 65}
--
-- >>> sconcat = Consumer.foldl' (<>)
--
-- /Internal/
{-# INLINE sconcat #-}
sconcat :: (Monad m, Semigroup a) => Consumer m a a a
sconcat = foldl' (<>)

------------------------------------------------------------------------------
-- append
------------------------------------------------------------------------------

-- | Supply the output of the first consumer as input to the second consumer.
--
-- /Internal/
{-# INLINE append #-}
append :: Monad m => Consumer m x a b -> Consumer m b a b -> Consumer m x a b
append (Consumer step1 inject1 extract1) (Consumer step2 inject2 extract2) =
    Consumer step inject extract

    where

    goLeft r = do
        case r of
            Partial s -> return $ Partial $ Left s
            Done b -> do
                r1 <- inject2 b
                return $ case r1 of
                    Partial s -> Partial $ Right s
                    Done b1 -> Done b1

    inject x = inject1 x >>= goLeft

    step (Left s) a = step1 s a >>= goLeft

    step (Right s) a = do
        r <- step2 s a
        case r of
            Partial s1 -> return $ Partial (Right s1)
            Done b -> return $ Done b

    extract (Left s) = extract1 s
    extract (Right s) = extract2 s

-- | Keep running the same consumer over and over again on the input, feeding
-- the output of the previous run to the next.
--
-- /Internal/
iterate :: Monad m => Consumer m b a b -> Consumer m b a b
iterate (Consumer step1 inject1 extract1) =
    Consumer step inject extract1

    where

    go r =
        case r of
            Partial s -> return $ Partial s
            Done b -> inject b

    inject x = inject1 x >>= go

    step s a = step1 s a >>= go

------------------------------------------------------------------------------
-- Transformation
------------------------------------------------------------------------------

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
