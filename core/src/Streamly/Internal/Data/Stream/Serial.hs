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
module Streamly.Internal.Data.Stream.Serial
    (
    -- * Serial appending stream
      SerialT(..)
    , Serial
    , serial

    -- * Construction
    , cons
    , consM
    , repeat
    , unfoldrM
    , fromList
    , list

    -- * Elimination
    , toList
    , foldWith

    -- * Transformation
    , map
    , mapM
    , filter
    , foldFilter
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
-- Generation
------------------------------------------------------------------------------

{-# INLINE cons #-}
cons :: a -> SerialT m a -> SerialT m a
cons x (SerialT ms) = SerialT $ K.cons x ms

{-# INLINE consM #-}
{-# SPECIALIZE consM :: IO a -> SerialT IO a -> SerialT IO a #-}
consM :: Monad m => m a -> SerialT m a -> SerialT m a
consM m (SerialT ms) = SerialT $ K.consM m ms

-- |
-- Generate an infinite stream by repeating a pure value.
--
{-# INLINE_NORMAL repeat #-}
repeat :: Monad m => a -> SerialT m a
repeat = SerialT . D.toStreamK . D.repeat

------------------------------------------------------------------------------
-- Combining
------------------------------------------------------------------------------

{-# INLINE serial #-}
serial :: SerialT m a -> SerialT m a -> SerialT m a
serial = (<>)

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

{-# INLINE mapM #-}
mapM :: Monad m => (a -> m b) -> SerialT m a -> SerialT m b
mapM f (SerialT m) = SerialT $ D.toStreamK $ D.mapM f $ D.fromStreamK m

-- |
-- @
-- map = fmap
-- @
--
-- Same as 'fmap'.
--
-- @
-- > S.toList $ S.map (+1) $ S.fromList [1,2,3]
-- [2,3,4]
-- @
--
-- @since 0.4.0
{-# INLINE map #-}
map :: Monad m => (a -> b) -> SerialT m a -> SerialT m b
map f = mapM (return . f)

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

{-# INLINE toStreamD #-}
toStreamD :: Applicative m => SerialT m a -> D.Stream m a
toStreamD (SerialT m) = D.fromStreamK m

{-# INLINE fromStreamD #-}
fromStreamD :: Monad m => D.Stream m a -> SerialT m a
fromStreamD m = SerialT $ D.toStreamK m

-- XXX We should only export generation and combinators from this module.
--
-- | Include only those elements that pass a predicate.
--
{-# INLINE filter #-}
filter :: Monad m => (a -> Bool) -> SerialT m a -> SerialT m a
filter p = fromStreamD . D.filter p . toStreamD

-- | Use a filtering fold on a stream.
--
-- > Stream.sum $ Stream.foldFilter (Fold.satisfy (> 5)) $ Stream.fromList [1..10]
-- 40
--
{-# INLINE foldFilter #-}
foldFilter :: Monad m => Fold m a (Maybe b) -> SerialT m a -> SerialT m b
foldFilter p = fromStreamD . D.foldFilter p . toStreamD

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

------------------------------------------------------------------------------
-- Construction
------------------------------------------------------------------------------

-- | Build a stream by unfolding a /monadic/ step function starting from a
-- seed.  The step function returns the next element in the stream and the next
-- seed value. When it is done it returns 'Nothing' and the stream ends. For
-- example,
--
-- @
-- let f b =
--         if b > 3
--         then return Nothing
--         else print b >> return (Just (b, b + 1))
-- in drain $ unfoldrM f 0
-- @
-- @
--  0
--  1
--  2
--  3
-- @
--
-- /Pre-release/
--
{-# INLINE unfoldrM #-}
unfoldrM :: Monad m => (b -> m (Maybe (a, b))) -> b -> SerialT m a
unfoldrM step seed = SerialT $ D.toStreamK (D.unfoldrM step seed)
