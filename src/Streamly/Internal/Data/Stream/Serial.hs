{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

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
module Streamly.Internal.Data.Stream.Serial {-# DEPRECATED "Please use \"Streamly.Internal.Data.Stream\" from streamly-core package instead." #-}
    (
    -- * Serial appending stream
      SerialT(..)
    , toStreamK
    , fromStreamK
    , Serial
    , serial

    -- * Serial interleaving stream
    , WSerialT(..)
    , WSerial
    , wSerial
    , wSerialFst
    , wSerialMin
    , consMWSerial

    -- * Construction
    , cons
    , consM
    , repeat
    , unfoldrM
    , fromList

    -- * Elimination
    , toList

    -- * Transformation
    , map
    , mapM
    )
where

#if !(MIN_VERSION_base(4,18,0))
import Control.Applicative (liftA2)
#endif
import Control.DeepSeq (NFData(..), NFData1(..))
#if !(MIN_VERSION_transformers(0,6,0))
import Control.Monad.Base (MonadBase(..), liftBaseDefault)
#endif
import Control.Monad.Catch (MonadThrow, throwM)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Reader.Class (MonadReader(..))
import Control.Monad.State.Class (MonadState(..))
import Control.Monad.Trans.Class (MonadTrans(lift))
import Data.Foldable (Foldable(foldl'), fold)
import Data.Functor.Identity (Identity(..), runIdentity)
import Data.Maybe (fromMaybe)
import Data.Semigroup (Endo(..))
import GHC.Exts (IsList(..), IsString(..), oneShot)
import Text.Read
       ( Lexeme(Ident), lexP, parens, prec, readPrec, readListPrec
       , readListPrecDefault)
import Streamly.Internal.BaseCompat ((#.))
import Streamly.Internal.Data.Maybe.Strict (Maybe'(..), toMaybe)
import Streamly.Internal.Data.StreamK (Stream)

import qualified Streamly.Internal.Data.Stream.Common as P
import qualified Streamly.Internal.Data.Stream as D
import qualified Streamly.Internal.Data.StreamK as K

import Prelude hiding (map, mapM, repeat, filter)

#include "Instances.hs"
#include "inline.hs"

-- $setup
-- >>> :set -fno-warn-deprecations
-- >>> import qualified Streamly.Prelude as IsStream

{-# INLINABLE withLocal #-}
withLocal :: MonadReader r m => (r -> r) -> K.Stream m a -> K.Stream m a
withLocal f m =
    K.mkStream $ \st yld sng stp ->
        let single = local f . sng
            yieldk a r = local f $ yld a (withLocal f r)
        in K.foldStream st yieldk single (local f stp) m

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
-- IsStream.toList $ do
--      x <- IsStream.fromList [1,2] -- foreach x in stream
--      return x
-- :}
-- [1,2]
--
-- Nested monad binds behave like nested @for@ loops:
--
-- >>> :{
-- IsStream.toList $ do
--     x <- IsStream.fromList [1,2] -- foreach x in stream
--     y <- IsStream.fromList [3,4] -- foreach y in stream
--     return (x, y)
-- :}
-- [(1,3),(1,4),(2,3),(2,4)]
--
-- /Since: 0.2.0 ("Streamly")/
--
-- @since 0.8.0
newtype SerialT m a = SerialT {getSerialT :: Stream m a}
    -- XXX when deriving do we inherit an INLINE?
    deriving (Semigroup, Monoid)

-- | A serial IO stream of elements of type @a@. See 'SerialT' documentation
-- for more details.
--
-- /Since: 0.2.0 ("Streamly")/
--
-- @since 0.8.0
type Serial = SerialT IO

toStreamK :: SerialT m a -> Stream m a
toStreamK = getSerialT

fromStreamK :: Stream m a -> SerialT m a
fromStreamK = SerialT

------------------------------------------------------------------------------
-- Generation
------------------------------------------------------------------------------

infixr 5 `cons`

{-# INLINE cons #-}
cons :: a -> SerialT m a -> SerialT m a
cons x (SerialT ms) = SerialT $ K.cons x ms

infixr 5 `consM`

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
    (>>=) (SerialT m) f = SerialT $ K.bindWith K.append m (getSerialT . f)

    {-# INLINE (>>) #-}
    (>>) = (*>)

instance MonadTrans SerialT where
    {-# INLINE lift #-}
    lift = SerialT . K.fromEffect

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
    SerialT $ D.toStreamK $ D.fromStreamK m1 `D.crossApply` D.fromStreamK m2

{-# INLINE apSequence #-}
apSequence :: Monad m => SerialT m a -> SerialT m b -> SerialT m b
apSequence (SerialT m1) (SerialT m2) =
    SerialT $ D.toStreamK $ D.fromStreamK m1 `D.crossApplySnd` D.fromStreamK m2

{-# INLINE apDiscardSnd #-}
apDiscardSnd :: Monad m => SerialT m a -> SerialT m b -> SerialT m a
apDiscardSnd (SerialT m1) (SerialT m2) =
    SerialT $ D.toStreamK $ D.fromStreamK m1 `D.crossApplyFst` D.fromStreamK m2

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

------------------------------------------------------------------------------
-- WSerialT
------------------------------------------------------------------------------

-- | For 'WSerialT' streams:
--
-- @
-- (<>) = 'Streamly.Prelude.wSerial'                       -- 'Semigroup'
-- (>>=) = flip . 'Streamly.Prelude.concatMapWith' 'Streamly.Prelude.wSerial' -- 'Monad'
-- @
--
-- Note that '<>' is associative only if we disregard the ordering of elements
-- in the resulting stream.
--
-- A single 'Monad' bind behaves like a @for@ loop:
--
-- >>> :{
-- IsStream.toList $ IsStream.fromWSerial $ do
--      x <- IsStream.fromList [1,2] -- foreach x in stream
--      return x
-- :}
-- [1,2]
--
-- Nested monad binds behave like interleaved nested @for@ loops:
--
-- >>> :{
-- IsStream.toList $ IsStream.fromWSerial $ do
--     x <- IsStream.fromList [1,2] -- foreach x in stream
--     y <- IsStream.fromList [3,4] -- foreach y in stream
--     return (x, y)
-- :}
-- [(1,3),(2,3),(1,4),(2,4)]
--
-- It is a result of interleaving all the nested iterations corresponding to
-- element @1@ in the first stream with all the nested iterations of element
-- @2@:
--
-- >>> import Streamly.Prelude (wSerial)
-- >>> IsStream.toList $ IsStream.fromList [(1,3),(1,4)] `IsStream.wSerial` IsStream.fromList [(2,3),(2,4)]
-- [(1,3),(2,3),(1,4),(2,4)]
--
-- The @W@ in the name stands for @wide@ or breadth wise scheduling in
-- contrast to the depth wise scheduling behavior of 'SerialT'.
--
-- /Since: 0.2.0 ("Streamly")/
--
-- @since 0.8.0
newtype WSerialT m a = WSerialT {getWSerialT :: Stream m a}

instance MonadTrans WSerialT where
    {-# INLINE lift #-}
    lift = WSerialT . K.fromEffect

-- | An interleaving serial IO stream of elements of type @a@. See 'WSerialT'
-- documentation for more details.
--
-- /Since: 0.2.0 ("Streamly")/
--
-- @since 0.8.0
type WSerial = WSerialT IO

{-# INLINE consMWSerial #-}
{-# SPECIALIZE consMWSerial :: IO a -> WSerialT IO a -> WSerialT IO a #-}
consMWSerial :: Monad m => m a -> WSerialT m a -> WSerialT m a
consMWSerial m (WSerialT ms) = WSerialT $ K.consM m ms

------------------------------------------------------------------------------
-- Semigroup
------------------------------------------------------------------------------

infixr 6 `wSerial`

-- | Interleaves two streams, yielding one element from each stream
-- alternately.  When one stream stops the rest of the other stream is used in
-- the output stream.
--
-- This gives exponential priority to earlier streams than the ones joining
-- later. Because of exponential weighting it can be used with 'concatMapWith'.
--
-- /Not fused/

-- NOTE:
--
-- Note that evaluation of @a \`wSerial` b \`wSerial` c@ does not interleave
-- @a@, @b@ and @c@ with equal priority.  This expression is equivalent to @a
-- \`wSerial` (b \`wSerial` c)@, therefore, it fairly interleaves @a@ with the
-- result of @b \`wSerial` c@.  For example, @Stream.fromList [1,2] \`wSerial`
-- Stream.fromList [3,4] \`wSerial` Stream.fromList [5,6]@ would result in
-- [1,3,2,5,4,6].  In other words, the leftmost stream gets the same scheduling
-- priority as the rest of the streams taken together. The same is true for
-- each subexpression on the right.
--
{-# INLINE wSerial #-}
wSerial :: WSerialT m a -> WSerialT m a -> WSerialT m a
wSerial (WSerialT m1) (WSerialT m2) = WSerialT $ K.interleave m1 m2

{-# INLINE wSerialFst #-}
wSerialFst :: WSerialT m a -> WSerialT m a -> WSerialT m a
wSerialFst (WSerialT m1) (WSerialT m2) = WSerialT $ K.interleaveFst m1 m2

{-# INLINE wSerialMin #-}
wSerialMin :: WSerialT m a -> WSerialT m a -> WSerialT m a
wSerialMin (WSerialT m1) (WSerialT m2) = WSerialT $ K.interleaveMin m1 m2

instance Semigroup (WSerialT m a) where
    (<>) = wSerial

------------------------------------------------------------------------------
-- Monoid
------------------------------------------------------------------------------

instance Monoid (WSerialT m a) where
    mempty = WSerialT K.nil
    mappend = (<>)

{-# INLINE apWSerial #-}
apWSerial :: WSerialT m (a -> b) -> WSerialT m a -> WSerialT m b
apWSerial (WSerialT m1) (WSerialT m2) =
    let f x1 = K.concatMapWith K.interleave (K.fromPure . x1) m2
    in WSerialT $ K.concatMapWith K.interleave f m1

instance Monad m => Applicative (WSerialT m) where
    {-# INLINE pure #-}
    pure = WSerialT . K.fromPure
    {-# INLINE (<*>) #-}
    (<*>) = apWSerial

------------------------------------------------------------------------------
-- Monad
------------------------------------------------------------------------------

instance Monad m => Monad (WSerialT m) where
    return = pure
    {-# INLINE (>>=) #-}
    (>>=) (WSerialT m) f = WSerialT $ K.bindWith K.interleave m (getWSerialT . f)

------------------------------------------------------------------------------
-- Other instances
------------------------------------------------------------------------------

#if !(MIN_VERSION_transformers(0,6,0))
instance (MonadBase b m, Monad m) => MonadBase b (SerialT m) where
    liftBase = liftBaseDefault
#endif

MONAD_COMMON_INSTANCES(WSerialT,)
LIST_INSTANCES(WSerialT)
NFDATA1_INSTANCE(WSerialT)
FOLDABLE_INSTANCE(WSerialT)
TRAVERSABLE_INSTANCE(WSerialT)

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
