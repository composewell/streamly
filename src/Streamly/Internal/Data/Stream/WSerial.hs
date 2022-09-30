{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      : Streamly.Internal.Data.Stream.WSerial
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
module Streamly.Internal.Data.Stream.WSerial
    (
    -- * Serial interleaving stream
      WSerialT(..)
    , WSerial
    , wSerial
    , wSerialFst
    , wSerialMin
    , consMWSerial
    )
where

import Control.Applicative (liftA2)
import Control.DeepSeq (NFData(..))
import Control.DeepSeq (NFData1(..))
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
import GHC.Exts (IsList(..), IsString(..), oneShot)
import Text.Read
       ( Lexeme(Ident), lexP, parens, prec, readPrec, readListPrec
       , readListPrecDefault)
import Streamly.Internal.BaseCompat ((#.))
import Streamly.Internal.Data.Maybe.Strict (Maybe'(..), toMaybe)
import Streamly.Internal.Data.Stream.StreamK.Type (Stream)

import qualified Streamly.Internal.Data.Stream.Common as P
import qualified Streamly.Internal.Data.Stream.StreamD as D
    (fromStreamK, toStreamK, mapM)
import qualified Streamly.Internal.Data.Stream.StreamK.Type as K
    (cons, consM, nil, concatMapWith, fromPure, bindWith
    , withLocal, interleave, interleaveFst, interleaveMin)

import Prelude hiding (map, mapM, repeat, filter)

#include "Instances.hs"
#include "inline.hs"

-- $setup
-- >>> import qualified Streamly.Prelude as Stream

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
-- Stream.toList $ Stream.fromWSerial $ do
--      x <- Stream.fromList [1,2] -- foreach x in stream
--      return x
-- :}
-- [1,2]
--
-- Nested monad binds behave like interleaved nested @for@ loops:
--
-- >>> :{
-- Stream.toList $ Stream.fromWSerial $ do
--     x <- Stream.fromList [1,2] -- foreach x in stream
--     y <- Stream.fromList [3,4] -- foreach y in stream
--     return (x, y)
-- :}
-- [(1,3),(2,3),(1,4),(2,4)]
--
-- It is a result of interleaving all the nested iterations corresponding to
-- element @1@ in the first stream with all the nested iterations of element
-- @2@:
--
-- >>> import Streamly.Prelude (wSerial)
-- >>> Stream.toList $ Stream.fromList [(1,3),(1,4)] `Stream.wSerial` Stream.fromList [(2,3),(2,4)]
-- [(1,3),(2,3),(1,4),(2,4)]
--
-- The @W@ in the name stands for @wide@ or breadth wise scheduling in
-- contrast to the depth wise scheduling behavior of 'Stream'.
--
-- /Since: 0.2.0 ("Streamly")/
--
-- @since 0.8.0
newtype WSerialT m a = WSerialT {getWSerialT :: Stream m a}
    deriving (MonadTrans)

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
apWSerial :: Monad m => WSerialT m (a -> b) -> WSerialT m a -> WSerialT m b
apWSerial (WSerialT m1) (WSerialT m2) =
    let f x1 = K.concatMapWith K.interleave (pure . x1) m2
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

MONAD_COMMON_INSTANCES(WSerialT,)
LIST_INSTANCES(WSerialT)
NFDATA1_INSTANCE(WSerialT)
FOLDABLE_INSTANCE(WSerialT)
TRAVERSABLE_INSTANCE(WSerialT)
