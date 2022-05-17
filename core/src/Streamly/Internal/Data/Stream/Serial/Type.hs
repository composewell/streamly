{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- |
-- Module      : Streamly.Internal.Data.Stream.Serial
-- Copyright   : (c) 2017 Composewell Technologies
--
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- To run examples in this module:
--
-- >>> import qualified Streamly.Prelude as Stream
--
module Streamly.Internal.Data.Stream.Serial.Type
    (
    -- * Serial appending stream
      SerialT(..)
    , Serial
    , fromStreamD
    , toStreamD
    , foldWith
    , list
    , cons
    , consM
    , (.:)
    , nil
    , nilM
    , fromPure
    , fromEffect
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
import Text.Read
       ( Lexeme(Ident), lexP, parens, prec, readPrec, readListPrec
       , readListPrecDefault)
import Streamly.Internal.BaseCompat ((#.))
import Streamly.Internal.Data.Fold.Type (Fold)
import Streamly.Internal.Data.Maybe.Strict (Maybe'(..), toMaybe)
import Streamly.Internal.Data.Stream.StreamK.Type (Stream)

import qualified Streamly.Internal.Data.Stream.Common as P
import qualified Streamly.Internal.Data.Stream.StreamD.Generate as D
import qualified Streamly.Internal.Data.Stream.StreamD.Transform as D
import qualified Streamly.Internal.Data.Stream.StreamD.Type as D
import qualified Streamly.Internal.Data.Stream.StreamK.Type as K

import Prelude hiding (map, mapM, repeat, filter)

#include "Instances.hs"
#include "inline.hs"

-- $setup
-- >>> import qualified Streamly.Prelude as Stream

------------------------------------------------------------------------------
-- SerialT
------------------------------------------------------------------------------

-- | For 'SerialT' streams:
--
-- @
-- (<>) = 'Streamly.Prelude.serial'                       -- 'Semigroup'
-- (>>=) = flip . 'Streamly.Prelude.concatMapWith' 'Streamly.Prelude.serial' -- 'Monad'
-- @
--
-- A single 'Monad' bind behaves like a @for@ loop:
--
-- >>> :{
-- Stream.toList $ do
--      x <- Stream.fromList [1,2] -- foreach x in stream
--      return x
-- :}
-- [1,2]
--
-- Nested monad binds behave like nested @for@ loops:
--
-- >>> :{
-- Stream.toList $ do
--     x <- Stream.fromList [1,2] -- foreach x in stream
--     y <- Stream.fromList [3,4] -- foreach y in stream
--     return (x, y)
-- :}
-- [(1,3),(1,4),(2,3),(2,4)]
--
-- /Since: 0.2.0 ("Streamly")/
--
-- @since 0.8.0
newtype SerialT m a = SerialT {getSerialT :: Stream m a}
    -- XXX when deriving do we inherit an INLINE?
    deriving (Semigroup, Monoid, MonadTrans)

-- | A serial IO stream of elements of type @a@. See 'SerialT' documentation
-- for more details.
--
-- /Since: 0.2.0 ("Streamly")/
--
-- @since 0.8.0
type Serial = SerialT IO

------------------------------------------------------------------------------
-- Monad
------------------------------------------------------------------------------

instance Monad m => Monad (SerialT m) where
    return = pure

    -- Benchmarks better with StreamD bind and pure:
    -- toList, filterAllout, *>, *<, >> (~2x)
    --
    -- pure = SerialT . D.fromStreamD . D.fromPure
    -- m >>= f = D.fromStreamD $ D.concatMap (D.toStreamD . f) (D.toStreamD m)

    -- Benchmarks better with CPS bind and pure:
    -- Prime sieve (25x)
    -- n binds, breakAfterSome, filterAllIn, state transformer (~2x)
    --
    {-# INLINE (>>=) #-}
    (>>=) (SerialT m) f = SerialT $ K.bindWith K.serial m (getSerialT . f)

    {-# INLINE (>>) #-}
    (>>)  = (*>)

------------------------------------------------------------------------------
-- Other instances
------------------------------------------------------------------------------

{-# INLINE apSerial #-}
apSerial :: Monad m => SerialT m (a -> b) -> SerialT m a -> SerialT m b
apSerial (SerialT m1) (SerialT m2) =
    SerialT $ D.toStreamK $ D.fromStreamK m1 <*> D.fromStreamK m2

{-# INLINE apSequence #-}
apSequence :: Monad m => SerialT m a -> SerialT m b -> SerialT m b
apSequence (SerialT m1) (SerialT m2) =
    SerialT $ D.toStreamK $ D.fromStreamK m1 *> D.fromStreamK m2

{-# INLINE apDiscardSnd #-}
apDiscardSnd :: Monad m => SerialT m a -> SerialT m b -> SerialT m a
apDiscardSnd (SerialT m1) (SerialT m2) =
    SerialT $ D.toStreamK $ D.fromStreamK m1 <* D.fromStreamK m2

-- Note: we need to define all the typeclass operations because we want to
-- INLINE them.
instance Monad m => Applicative (SerialT m) where
    {-# INLINE pure #-}
    pure = SerialT . K.fromPure

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

MONAD_COMMON_INSTANCES(SerialT,)
LIST_INSTANCES(SerialT)
NFDATA1_INSTANCE(SerialT)
FOLDABLE_INSTANCE(SerialT)
TRAVERSABLE_INSTANCE(SerialT)

-- XXX Renamed to foldWith because SerialT has a Foldable instance having
-- method fold.
--
-- | Fold a stream using the supplied left 'Fold' and reducing the resulting
-- expression strictly at each step. The behavior is similar to 'foldl''. A
-- 'Fold' can terminate early without consuming the full stream. See the
-- documentation of individual 'Fold's for termination behavior.
--
-- >>> Stream.foldWith Fold.sum (Stream.enumerateFromTo 1 100)
-- 5050
--
-- Folds never fail, therefore, they produce a default value even when no input
-- is provided. It means we can always fold an empty stream and get a valid
-- result.  For example:
--
-- >>> Stream.foldWith Fold.sum Stream.nil
-- 0
--
-- However, 'foldMany' on an empty stream results in an empty stream.
-- Therefore, @Stream.foldWith f@ is not the same as @Stream.head . Stream.foldMany
-- f@.
--
-- @foldWith f = Stream.parse (Parser.fromFold f)@
--
-- /Pre-release/
{-# INLINE foldWith #-}
foldWith :: Monad m => Fold m a b -> SerialT m a -> m b
foldWith fld m = D.fold fld $ toStreamD m

-- XXX Renamed to "list" because fromList is present in IsList instance.
--
-- |
-- @
-- fromList = 'Prelude.foldr' 'K.cons' 'K.nil'
-- @
--
-- Construct a stream from a list of pure values. This is more efficient than
-- 'K.fromFoldable' for serial streams.
--
-- @since 0.4.0
{-# INLINE_EARLY list #-}
list :: Monad m => [a] -> SerialT m a
list = fromStreamD . D.fromList
{-# RULES "list fallback to StreamK" [1]
    forall a. D.toStreamK (D.fromList a) = K.fromFoldable a #-}

{-# INLINE toStreamD #-}
toStreamD :: Applicative m => SerialT m a -> D.Stream m a
toStreamD (SerialT m) = D.fromStreamK m

{-# INLINE fromStreamD #-}
fromStreamD :: Monad m => D.Stream m a -> SerialT m a
fromStreamD m = SerialT $ D.toStreamK m

{-# INLINE consM #-}
{-# SPECIALIZE consM :: IO a -> SerialT IO a -> SerialT IO a #-}
consM :: Monad m => m a -> SerialT m a -> SerialT m a
consM m (SerialT ms) = SerialT $ K.consM m ms

{-# INLINE_NORMAL cons #-}
cons :: a -> SerialT m a -> SerialT m a
cons x (SerialT m) = SerialT $ K.cons x m

infixr 5 .:

{-# INLINE (.:) #-}
(.:) :: a -> SerialT m a -> SerialT m a
(.:) = cons

{-# INLINE_NORMAL nil #-}
nil :: SerialT m a
nil = SerialT K.nil

{-# INLINE_NORMAL nilM #-}
nilM :: (Monad m) => m b -> SerialT m a
nilM = SerialT . K.nilM

{-# INLINE_NORMAL fromPure #-}
fromPure :: a -> SerialT m a
fromPure = SerialT . K.fromPure

{-# INLINE_NORMAL fromEffect #-}
fromEffect :: (Monad m) => m a -> SerialT m a
fromEffect = SerialT . K.fromEffect
