{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      : Streamly.Internal.Data.Stream.Type
-- Copyright   : (c) 2017 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
module Streamly.Internal.Data.Stream.Type
    (
      Stream
    , fromStreamK
    , toStreamK
    , fromStreamD
    , toStreamD
    )
where

import Control.Applicative (liftA2)
import Control.DeepSeq (NFData(..), NFData1(..))
import Control.Monad.Base (MonadBase(..), liftBaseDefault)
import Control.Monad.Catch (MonadThrow, throwM)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Reader.Class (MonadReader(..))
import Control.Monad.State.Class (MonadState(..))
import Control.Monad.Trans.Class (MonadTrans(lift))
import Data.Foldable (Foldable(foldl'), fold)
import Data.Functor.Identity (Identity(..), runIdentity)
import Data.Maybe (fromMaybe)
import Data.Semigroup (Endo(..))
#if __GLASGOW_HASKELL__ < 808
import Data.Semigroup (Semigroup(..))
#endif
import GHC.Exts (IsList(..), IsString(..), oneShot)
import Streamly.Internal.BaseCompat ((#.))
import Streamly.Internal.Data.Maybe.Strict (Maybe'(..), toMaybe)
import Text.Read
       ( Lexeme(Ident), lexP, parens, prec, readPrec, readListPrec
       , readListPrecDefault)

import qualified Streamly.Internal.Data.Stream.Common as P
import qualified Streamly.Internal.Data.Stream.StreamD.Type as D
import qualified Streamly.Internal.Data.Stream.StreamK.Type as K

#include "Instances.hs"
#include "inline.hs"

-- $setup
-- >>> import qualified Streamly.Data.Fold as Fold
-- >>> import qualified Streamly.Data.Unfold as Unfold
-- >>> import qualified Streamly.Internal.Data.Stream as Stream

------------------------------------------------------------------------------
-- Stream
------------------------------------------------------------------------------

-- | Semigroup instance appends two streams:
--
-- > (<>) = Stream.append
--
-- Monad bind maps a stream generator function on the stream and flattens the
-- resulting stream:
--
-- > (>>=) = flip . Stream.concatMapWith Stream.append
--
-- A 'Monad' bind behaves like a @for@ loop:
--
-- >>> :{
-- Stream.fold Fold.toList $ do
--      x <- Stream.unfold Unfold.fromList [1,2] -- foreach x in stream
--      return x
-- :}
-- [1,2]
--
-- Nested monad binds behave like nested @for@ loops:
--
-- >>> :{
-- Stream.fold Fold.toList $ do
--     x <- Stream.unfold Unfold.fromList [1,2] -- foreach x in stream
--     y <- Stream.unfold Unfold.fromList [3,4] -- foreach y in stream
--     return (x, y)
-- :}
-- [(1,3),(1,4),(2,3),(2,4)]
--
-- @since 0.9.0
newtype Stream m a = Stream (K.Stream m a)
    -- XXX when deriving do we inherit an INLINE?
    deriving (Semigroup, Monoid, MonadTrans)

------------------------------------------------------------------------------
-- Conversions
------------------------------------------------------------------------------

{-# INLINE fromStreamK #-}
fromStreamK :: K.Stream m a -> Stream m a
fromStreamK = Stream

{-# INLINE toStreamK #-}
toStreamK :: Stream m a -> K.Stream m a
toStreamK (Stream k) = k

{-# INLINE toStreamD #-}
toStreamD :: Applicative m => Stream m a -> D.Stream m a
toStreamD = D.fromStreamK . toStreamK

{-# INLINE fromStreamD #-}
fromStreamD :: Monad m => D.Stream m a -> Stream m a
fromStreamD = fromStreamK . D.toStreamK

------------------------------------------------------------------------------
-- Monad
------------------------------------------------------------------------------

instance Monad m => Monad (Stream m) where
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
    (>>=) (Stream m) f = Stream $ K.bindWith K.serial m (toStreamK . f)

    {-# INLINE (>>) #-}
    (>>)  = (*>)

------------------------------------------------------------------------------
-- Other instances
------------------------------------------------------------------------------

{-# INLINE apSerial #-}
apSerial :: Monad m => Stream m (a -> b) -> Stream m a -> Stream m b
apSerial (Stream m1) (Stream m2) =
    Stream $ D.toStreamK $ D.fromStreamK m1 <*> D.fromStreamK m2

{-# INLINE apSequence #-}
apSequence :: Monad m => Stream m a -> Stream m b -> Stream m b
apSequence (Stream m1) (Stream m2) =
    Stream $ D.toStreamK $ D.fromStreamK m1 *> D.fromStreamK m2

{-# INLINE apDiscardSnd #-}
apDiscardSnd :: Monad m => Stream m a -> Stream m b -> Stream m a
apDiscardSnd (Stream m1) (Stream m2) =
    Stream $ D.toStreamK $ D.fromStreamK m1 <* D.fromStreamK m2

-- Note: we need to define all the typeclass operations because we want to
-- INLINE them.
instance Monad m => Applicative (Stream m) where
    {-# INLINE pure #-}
    pure = Stream . K.fromPure

    {-# INLINE (<*>) #-}
    (<*>) = apSerial
    -- (<*>) = K.apSerial

    {-# INLINE liftA2 #-}
    liftA2 f x = (<*>) (fmap f x)

    {-# INLINE (*>) #-}
    (*>)  = apSequence
    -- (*>)  = K.apSerialDiscardFst

    {-# INLINE (<*) #-}
    (<*) = apDiscardSnd
    -- (<*)  = K.apSerialDiscardSnd

MONAD_COMMON_INSTANCES(Stream,)
LIST_INSTANCES(Stream)
NFDATA1_INSTANCE(Stream)
FOLDABLE_INSTANCE(Stream)
TRAVERSABLE_INSTANCE(Stream)
