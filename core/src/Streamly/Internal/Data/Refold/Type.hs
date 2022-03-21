-- |
-- Module      : Streamly.Internal.Data.Refold.Type
-- Copyright   : (c) 2019 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- The 'Fold' type embeds a default initial value, therefore, it is like a
-- 'Monoid' whereas the 'Refold' type has to be supplied with an initial
-- value, therefore, it is more like a 'Semigroup' operation.
--
-- Refolds can be appended to each other or to a fold to build the fold
-- incrementally. This is useful in incremental builder like use cases.
--
-- See the file splitting example in the @streamly-examples@ repository for an
-- application of the 'Refold' type. The 'Fold' type does not perform as well
-- in this situation.
--
-- 'Refold' type is to 'Fold' as 'Unfold' type is to 'Stream'. 'Unfold'
-- provides better optimizaiton than stream in nested operations, similarly,
-- 'Refold' provides better optimization than 'Fold'.
--
module Streamly.Internal.Data.Refold.Type
    (
    -- * Types
      Refold (..)

    -- * Constructors
    , foldl'

    -- * Refolds
    -- ** Accumulators
    , sconcat
    , drainBy
    , iterate

    -- * Combinators
    , lmapM
    , rmapM
    , append
    -- , take
    )
where

#if __GLASGOW_HASKELL__ < 804
import Data.Semigroup (Semigroup((<>)))
#endif
import Fusion.Plugin.Types (Fuse(..))
import Streamly.Internal.Data.Fold.Step (Step(..), mapMStep)

import Prelude hiding (take, iterate)

-- $setup
-- >>> :m
-- >>> :set -package streamly
-- >>> import qualified Streamly.Internal.Data.Refold.Type as Refold
-- >>> import qualified Streamly.Internal.Data.Fold.Type as Fold
-- >>> import qualified Streamly.Internal.Data.Stream.IsStream as Stream

-- All folds in the Fold module should be implemented using Refolds.
--
-- | Like 'Fold' except that the initial state of the accmulator can be
-- generated using a dynamically supplied input. This affords better stream
-- fusion optimization in nested fold operations where the initial fold state
-- is determined based on a dynamic value.
--
-- /Internal/
data Refold m c a b =
  -- | @Fold @ @ step @ @ inject @ @ extract@
  forall s. Refold (s -> a -> m (Step s b)) (c -> m (Step s b)) (s -> m (Maybe b))

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
foldl' :: Monad m => (b -> a -> b) -> Refold m b a b
foldl' step =
    Refold
        (\s a -> return $ let b = step s a in Partial b b)
        (\b -> return $ Partial b b)
        (return . Just)

------------------------------------------------------------------------------
-- Mapping on input
------------------------------------------------------------------------------

-- | @lmapM f fold@ maps the monadic function @f@ on the input of the fold.
--
-- /Internal/
{-# INLINE lmapM #-}
lmapM :: Monad m => (a -> m b) -> Refold m c b r -> Refold m c a r
lmapM f (Refold step inject extract) = Refold step1 inject extract

    where

    step1 x a = f a >>= step x

------------------------------------------------------------------------------
-- Mapping on the output
------------------------------------------------------------------------------

-- | Map a monadic function on the output of a fold.
--
-- /Internal/
{-# INLINE rmapM #-}
rmapM :: Monad m => (b -> m c) -> Refold m x a b -> Refold m x a c
rmapM f (Refold step inject extract) = Refold step1 inject1 (\s -> extract s >>= Prelude.mapM f)

    where

    inject1 x = inject x >>= mapMStep f
    step1 s a = step s a >>= mapMStep f

------------------------------------------------------------------------------
-- Refolds
------------------------------------------------------------------------------

-- |
--
-- /Internal/
{-# INLINE drainBy #-}
drainBy ::  Monad m => (c -> a -> m b) -> Refold m c a ()
drainBy f = Refold step inject extract

    where

    inject c = return $ Partial c ()

    step c a = f c a >> return (Partial c ())

    extract _ = return (Just ())

------------------------------------------------------------------------------
-- Semigroup
------------------------------------------------------------------------------

-- | Append the elements of an input stream to a provided starting value.
--
-- >>> stream = Stream.map Data.Monoid.Sum $ Stream.enumerateFromTo 1 10
-- >>> Stream.fold (Fold.fromRefold Refold.sconcat 10) stream
-- Sum {getSum = 65}
--
-- >>> sconcat = Refold.foldl' (<>)
--
-- /Internal/
{-# INLINE sconcat #-}
sconcat :: (Monad m, Semigroup a) => Refold m a a a
sconcat = foldl' (<>)

------------------------------------------------------------------------------
-- append
------------------------------------------------------------------------------

-- | Supply the output of the first consumer as input to the second consumer.
--
-- /Internal/
{-# INLINE append #-}
append :: Monad m => Refold m x a b -> Refold m b a b -> Refold m x a b
append (Refold step1 inject1 extract1) (Refold step2 inject2 extract2) =
    Refold step inject extract

    where

    goRight r = do
        case r of
            Partial s b1 -> return $ Partial (Right s) b1
            Continue s -> return $ Continue (Right s)
            Done b1 -> return $ Done b1
            Stop s -> do
                res <- extract2 s
                return $ case res of
                    Nothing -> Stop (Right s)
                    Just b1 -> Done b1

    goLeft r = do
        case r of
            Partial s b -> return $ Partial (Left s) b
            Continue s -> return $ Continue (Left s)
            Done b -> inject2 b >>= goRight
            Stop s -> do
                res <- extract1 s
                case res of
                    Nothing -> return $ Stop (Left s)
                    Just b -> inject2 b >>= goRight

    inject x = inject1 x >>= goLeft

    step (Left s) a = step1 s a >>= goLeft
    step (Right s) a = step2 s a >>= goRight

    extract (Left s) = extract1 s
    extract (Right s) = extract2 s

-- | Keep running the same consumer over and over again on the input, feeding
-- the output of the previous run to the next.
--
-- /Internal/
iterate :: Monad m => Refold m b a b -> Refold m b a b
iterate (Refold step1 inject1 extract1) =
    Refold step inject extract1

    where

    go r =
        case r of
            Partial s b -> return $ Partial s b
            Continue s -> return $ Continue s
            Done b -> inject b
            Stop s -> do
                res <- extract1 s
                case res of
                    Nothing -> return $ Stop s
                    Just b -> inject b

    inject x = inject1 x >>= go

    step s a = step1 s a >>= go

{-
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
take :: Monad m => Int -> Refold m x a b -> Refold m x a b
take n (Refold fstep finject fextract) = Refold step inject extract

    where

    inject x = do
        res <- finject x
        case res of
            Partial s b ->
                if n > 0
                then return $ Partial (Tuple'Fused 0 s) b
                else Done <$> fextract s
            Done b -> return $ Done b

    step (Tuple'Fused i r) a = do
        res <- fstep r a
        case res of
            Partial sres b -> do
                let i1 = i + 1
                    s1 = Tuple'Fused i1 sres
                if i1 < n
                then return $ Partial s1 b
                else Done <$> fextract sres
            Done bres -> return $ Done bres

    extract (Tuple'Fused _ r) = fextract r
-}
