-- |
-- Module      : Streamly.Internal.Data.Stream.Lift
-- Copyright   : (c) 2019 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

module Streamly.Internal.Data.Stream.Lift
    (
    -- * Generalize Inner Monad
      morphInner
    , generalizeInner

    -- * Transform Inner Monad
    , liftInnerWith
    , runInnerWith
    , runInnerWithState
    )
where

import Data.Functor.Identity (Identity (..))
import Streamly.Internal.Data.Stream.Type
    (Stream, fromStreamD, toStreamD, fromStreamK, toStreamK)

import qualified Streamly.Internal.Data.Stream as D
import qualified Streamly.Internal.Data.StreamK as K

-- $setup
-- >>> :m
-- >>> import Data.Functor.Identity (runIdentity)
-- >>> import Streamly.Internal.Data.Stream as Stream

------------------------------------------------------------------------------
-- Generalize the underlying monad
------------------------------------------------------------------------------

-- | Transform the inner monad of a stream using a natural transformation.
--
-- Example, generalize the inner monad from Identity to any other:
--
-- >>> generalizeInner = Stream.morphInner (return . runIdentity)
--
-- Also known as hoist.
--
-- /CPS/
{-# INLINE morphInner #-}
morphInner :: (Monad m, Monad n)
    => (forall x. m x -> n x) -> Stream m a -> Stream n a
morphInner f xs = fromStreamK $ K.hoist f (toStreamK xs)

-- | Generalize the inner monad of the stream from 'Identity' to any monad.
--
-- Definition:
--
-- >>> generalizeInner = Stream.morphInner (return . runIdentity)
--
-- /CPS/
--
{-# INLINE generalizeInner #-}
generalizeInner :: Monad m => Stream Identity a -> Stream m a
generalizeInner = morphInner (return . runIdentity)
    -- fromStreamK $ K.hoist (return . runIdentity) (toStreamK xs)

------------------------------------------------------------------------------
-- Add and remove a monad transformer
------------------------------------------------------------------------------

-- | Lift the inner monad @m@ of a stream @Stream m a@ to @t m@ using the
-- supplied lift function.
--
{-# INLINE liftInnerWith #-}
liftInnerWith :: (Monad m, Monad (t m))
    => (forall b. m b -> t m b) -> Stream m a -> Stream (t m) a
liftInnerWith lift xs = fromStreamD $ D.liftInnerWith lift (toStreamD xs)

-- | Evaluate the inner monad of a stream using the supplied runner function.
--
{-# INLINE runInnerWith #-}
runInnerWith :: (Monad m, Applicative (t m)) =>
    (forall b. t m b -> m b) -> Stream (t m) a -> Stream m a
runInnerWith run xs = fromStreamD $ D.runInnerWith run (toStreamD xs)

-- | Evaluate the inner monad of a stream using the supplied stateful runner
-- function and the initial state. The state returned by an invocation of the
-- runner is supplied as input state to the next invocation.
--
{-# INLINE runInnerWithState #-}
runInnerWithState :: (Monad m, Applicative (t m)) =>
       (forall b. s -> t m b -> m (b, s))
    -> m s
    -> Stream (t m) a
    -> Stream m (s, a)
runInnerWithState run initial xs =
    fromStreamD $ D.runInnerWithState run initial (toStreamD xs)
