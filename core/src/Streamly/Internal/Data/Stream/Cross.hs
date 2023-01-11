{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      : Streamly.Internal.Data.Stream.Cross
-- Copyright   : (c) 2017 Composewell Technologies
--
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
module Streamly.Internal.Data.Stream.Cross
    (
      CrossStream (..)
    )
where

import Control.Monad.Catch (MonadThrow, throwM)
import Control.Monad.Trans.Class (MonadTrans(lift))
import Control.Applicative (liftA2)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Functor.Identity (Identity(..))
import GHC.Exts (IsList(..), IsString(..))
import Streamly.Internal.Data.Stream.Type (Stream)

import qualified Streamly.Internal.Data.Stream.Type as Stream
import qualified Streamly.Internal.Data.Stream.StreamK.Type as K

-- $setup
-- >>> import Streamly.Internal.Data.Stream.Cross (CrossStream(..))
-- >>> import qualified Streamly.Data.Fold as Fold
-- >>> import qualified Streamly.Data.Stream as Stream

------------------------------------------------------------------------------
-- Stream with a cross product style monad instance
------------------------------------------------------------------------------

-- | A newtype wrapper for the 'Stream' type with a cross product style monad
-- instance.
--
-- Semigroup instance appends two streams.
--
-- A 'Monad' bind behaves like a @for@ loop:
--
-- >>> :{
-- Stream.fold Fold.toList $ unCrossStream $ do
--      x <- CrossStream (Stream.fromList [1,2]) -- foreach x in stream
--      return x
-- :}
-- [1,2]
--
-- Nested monad binds behave like nested @for@ loops:
--
-- >>> :{
-- Stream.fold Fold.toList $ unCrossStream $ do
--     x <- CrossStream (Stream.fromList [1,2]) -- foreach x in stream
--     y <- CrossStream (Stream.fromList [3,4]) -- foreach y in stream
--     return (x, y)
-- :}
-- [(1,3),(1,4),(2,3),(2,4)]
--
newtype CrossStream m a = CrossStream {unCrossStream :: Stream m a}
        deriving (Functor, Semigroup, Monoid, Foldable)

-- Pure (Identity monad) stream instances
deriving instance Traversable (CrossStream Identity)
deriving instance IsList (CrossStream Identity a)
deriving instance (a ~ Char) => IsString (CrossStream Identity a)
deriving instance Eq a => Eq (CrossStream Identity a)
deriving instance Ord a => Ord (CrossStream Identity a)
deriving instance Show a => Show (CrossStream Identity a)
deriving instance Read a => Read (CrossStream Identity a)

------------------------------------------------------------------------------
-- Applicative
------------------------------------------------------------------------------

-- Note: we need to define all the typeclass operations because we want to
-- INLINE them.
instance Monad m => Applicative (CrossStream m) where
    {-# INLINE pure #-}
    pure x = CrossStream (Stream.fromPure x)

    {-# INLINE (<*>) #-}
    (CrossStream s1) <*> (CrossStream s2) =
        CrossStream (Stream.crossApply s1 s2)
    -- (<*>) = K.crossApply

    {-# INLINE liftA2 #-}
    liftA2 f x = (<*>) (fmap f x)

    {-# INLINE (*>) #-}
    (CrossStream s1) *> (CrossStream s2) =
        CrossStream (Stream.crossApplySnd s1 s2)
    -- (*>)  = K.crossApplySnd

    {-# INLINE (<*) #-}
    (CrossStream s1) <* (CrossStream s2) =
        CrossStream (Stream.crossApplyFst s1 s2)
    -- (<*)  = K.crossApplyFst

------------------------------------------------------------------------------
-- Monad
------------------------------------------------------------------------------

instance Monad m => Monad (CrossStream m) where
    return = pure

    -- Benchmarks better with StreamD bind and pure:
    -- toList, filterAllout, *>, *<, >> (~2x)
    --
    -- pure = Stream . D.fromStreamD . D.fromPure
    -- m >>= f = D.fromStreamD $ D.concatMap (D.toStreamD . f) (D.toStreamD m)

    -- Benchmarks better with CPS bind and pure:
    -- Prime sieve (25x)
    -- n binds, breakAfterSome, filterAllIn, state transformer (~2x)
    --
    {-# INLINE (>>=) #-}
    (>>=) (CrossStream m) f =
        CrossStream
            (Stream.fromStreamK
                $ K.bindWith
                    K.serial
                    (Stream.toStreamK m)
                    (Stream.toStreamK . unCrossStream . f))

    {-# INLINE (>>) #-}
    (>>) = (*>)

------------------------------------------------------------------------------
-- Transformers
------------------------------------------------------------------------------

instance (MonadIO m) => MonadIO (CrossStream m) where
    liftIO x = CrossStream (Stream.fromEffect $ liftIO x)

instance MonadTrans CrossStream where
    {-# INLINE lift #-}
    lift x = CrossStream (Stream.fromEffect x)

instance (MonadThrow m) => MonadThrow (CrossStream m) where
    throwM = lift . throwM
