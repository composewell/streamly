{-# LANGUAGE UndecidableInstances #-}

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
      SerialT
    , Serial
    , K.serial
    , serially

    -- * Serial interleaving stream
    , WSerialT
    , WSerial
    , wSerial
    , wSerialFst
    , wSerialMin
    , wSerially

    -- * Construction
    , unfoldrM

    -- * Transformation
    , map
    , mapM

    -- * Deprecated
    , StreamT
    , InterleavedT
    , (<=>)
    , interleaving
    )
where

import Control.Applicative (liftA2)
import Control.DeepSeq (NFData(..))
#if MIN_VERSION_deepseq(1,4,3)
import Control.DeepSeq (NFData1(..))
#endif
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
import GHC.Exts (IsList(..), IsString(..))
import Text.Read
       ( Lexeme(Ident), lexP, parens, prec, readPrec, readListPrec
       , readListPrecDefault)
import Streamly.Internal.BaseCompat ((#.), errorWithoutStackTrace)
import Streamly.Internal.Data.Stream.StreamK.Type
       (IsStream(..), adapt, Stream, mkStream, foldStream)
import Streamly.Internal.Data.Maybe.Strict (Maybe'(..), toMaybe)

import qualified Streamly.Internal.Data.Stream.Prelude as P
    (cmpBy, foldl', foldr, eqBy, fromList, toList)
import qualified Streamly.Internal.Data.Stream.StreamK as K (withLocal)
import qualified Streamly.Internal.Data.Stream.StreamK.Type as K
import qualified Streamly.Internal.Data.Stream.StreamD.Generate as D (unfoldrM)
import qualified Streamly.Internal.Data.Stream.StreamD.Type as D

import Prelude hiding (map, mapM, errorWithoutStackTrace)

#include "Instances.hs"
#include "inline.hs"

------------------------------------------------------------------------------
-- SerialT
------------------------------------------------------------------------------

-- | For 'SerialT' streams:
--
-- @
-- (<>) = 'Streamly.Prelude.serial'
-- (>>=) = flip . 'Streamly.Prelude.concatMapWith' 'Streamly.Prelude.serial'
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

-- |
-- @since 0.1.0
{-# DEPRECATED StreamT "Please use 'SerialT' instead." #-}
type StreamT = SerialT

-- | Fix the type of a polymorphic stream as 'SerialT'.
--
-- /Since: 0.1.0 ("Streamly")/
--
-- @since 0.8.0
serially :: IsStream t => SerialT m a -> t m a
serially = adapt

{-# INLINE consMSerial #-}
{-# SPECIALIZE consMSerial :: IO a -> SerialT IO a -> SerialT IO a #-}
consMSerial :: Monad m => m a -> SerialT m a -> SerialT m a
consMSerial m ms = fromStream $ K.consMStream m (toStream ms)

instance IsStream SerialT where
    toStream = getSerialT
    fromStream = SerialT
    consM = consMSerial
    (|:) = consMSerial

------------------------------------------------------------------------------
-- Monad
------------------------------------------------------------------------------

instance Monad m => Monad (SerialT m) where
    return = pure

    -- Benchmarks better with StreamD bind and pure:
    -- toList, filterAllout, *>, *<, >> (~2x)
    --
    -- pure = SerialT . D.fromStreamD . D.yield
    -- m >>= f = D.fromStreamD $ D.concatMap (D.toStreamD . f) (D.toStreamD m)

    -- Benchmarks better with CPS bind and pure:
    -- Prime sieve (25x)
    -- n binds, breakAfterSome, filterAllIn, state transformer (~2x)
    --
    {-# INLINE (>>=) #-}
    (>>=) = K.bindWith K.serial

    {-# INLINE (>>) #-}
    (>>)  = (*>)

------------------------------------------------------------------------------
-- Other instances
------------------------------------------------------------------------------

{-# INLINE mapM #-}
mapM :: (IsStream t, Monad m) => (a -> m b) -> t m a -> t m b
mapM f m = D.fromStreamD $ D.mapM f $ D.toStreamD m

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
map :: (IsStream t, Monad m) => (a -> b) -> t m a -> t m b
map f = mapM (return . f)

{-# INLINE apSerial #-}
apSerial :: Monad m => SerialT m (a -> b) -> SerialT m a -> SerialT m b
apSerial (SerialT m1) (SerialT m2) =
    D.fromStreamD $ D.toStreamD m1 <*> D.toStreamD m2

{-# INLINE apSequence #-}
apSequence :: Monad m => SerialT m a -> SerialT m b -> SerialT m b
apSequence (SerialT m1) (SerialT m2) =
    D.fromStreamD $ D.toStreamD m1 *> D.toStreamD m2

{-# INLINE apDiscardSnd #-}
apDiscardSnd :: Monad m => SerialT m a -> SerialT m b -> SerialT m a
apDiscardSnd (SerialT m1) (SerialT m2) =
    D.fromStreamD $ D.toStreamD m1 <* D.toStreamD m2

-- Note: we need to define all the typeclass operations because we want to
-- INLINE them.
instance Monad m => Applicative (SerialT m) where
    {-# INLINE pure #-}
    pure = SerialT . K.yield

    {-# INLINE (<*>) #-}
    (<*>) = apSerial
    -- (<*>) = K.apSerial

#if MIN_VERSION_base(4,10,0)
    {-# INLINE liftA2 #-}
    liftA2 f x = (<*>) (fmap f x)
#endif

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
-- (<>) = 'Streamly.Prelude.wSerial'
-- (>>=) = flip . 'Streamly.Prelude.concatMapWith' 'Streamly.Prelude.wSerial'
-- @
--
-- Note that '<>' is associative only if we disregard the ordering of elements
-- in the resulting stream.
--
-- A single 'Monad' bind behaves like a @for@ loop:
--
-- >>> :{
-- Stream.toList $ Stream.wSerially $ do
--      x <- Stream.fromList [1,2] -- foreach x in stream
--      return x
-- :}
-- [1,2]
--
-- Nested monad binds behave like interleaved nested @for@ loops:
--
-- >>> :{
-- Stream.toList $ Stream.wSerially $ do
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
-- >>> Stream.fromList [(1,3),(1,4)] `wSerial` Stream.fromList [(2,3),(2,4)]
-- [(1,3),(2,3),(1,4),(2,4)]
--
-- The @W@ in the name stands for @wide@ or breadth wise scheduling in
-- contrast to the depth wise scheduling behavior of 'SerialT'.
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

-- |
-- @since 0.1.0
{-# DEPRECATED InterleavedT "Please use 'WSerialT' instead." #-}
type InterleavedT = WSerialT

-- | Fix the type of a polymorphic stream as 'WSerialT'.
--
-- /Since: 0.2.0 ("Streamly")/
--
-- @since 0.8.0
wSerially :: IsStream t => WSerialT m a -> t m a
wSerially = adapt

-- | Same as 'wSerially'.
--
-- @since 0.1.0
{-# DEPRECATED interleaving "Please use wSerially instead." #-}
interleaving :: IsStream t => WSerialT m a -> t m a
interleaving = wSerially

consMWSerial :: Monad m => m a -> WSerialT m a -> WSerialT m a
consMWSerial m ms = fromStream $ K.consMStream m (toStream ms)

instance IsStream WSerialT where
    toStream = getWSerialT
    fromStream = WSerialT

    {-# INLINE consM #-}
    {-# SPECIALIZE consM :: IO a -> WSerialT IO a -> WSerialT IO a #-}
    consM :: Monad m => m a -> WSerialT m a -> WSerialT m a
    consM = consMWSerial

    {-# INLINE (|:) #-}
    {-# SPECIALIZE (|:) :: IO a -> WSerialT IO a -> WSerialT IO a #-}
    (|:) :: Monad m => m a -> WSerialT m a -> WSerialT m a
    (|:) = consMWSerial

------------------------------------------------------------------------------
-- Semigroup
------------------------------------------------------------------------------

infixr 6 `wSerial`

-- Additionally we can have m elements yield from the first stream and n
-- elements yielding from the second stream. We can also have time slicing
-- variants of positional interleaving, e.g. run first stream for m seconds and
-- run the second stream for n seconds.
--
-- Similar combinators can be implemented using WAhead style.

-- | Interleaves two streams, yielding one element from each stream
-- alternately.  When one stream stops the rest of the other stream is used in
-- the output stream.
--
-- >>> stream1 = Stream.fromList [1,2]
-- >>> stream2 = Stream.fromList [3,4]
-- >>> Stream.toList $ Stream.wSerially $ stream1 `wSerial` stream2
-- [1,3,2,4]
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
-- Note that this operation cannot be used to fold a container of infinite
-- streams but it can be used for very large streams as the state that it needs
-- to maintain is proportional to the logarithm of the number of streams.
--
-- @since 0.8.0
--
-- /Since: 0.2.0 ("Streamly")/
{-# INLINE wSerial #-}
wSerial :: IsStream t => t m a -> t m a -> t m a
wSerial m1 m2 = mkStream $ \st yld sng stp -> do
    let stop       = foldStream st yld sng stp m2
        single a   = yld a m2
        yieldk a r = yld a (wSerial m2 r)
    foldStream st yieldk single stop m1

-- | Like `wSerial` but stops interleaving as soon as the first stream stops.
--
-- @since 0.7.0
{-# INLINE wSerialFst #-}
wSerialFst :: IsStream t => t m a -> t m a -> t m a
wSerialFst m1 m2 = mkStream $ \st yld sng stp -> do
    let yieldFirst a r = yld a (yieldSecond r m2)
     in foldStream st yieldFirst sng stp m1

    where

    yieldSecond s1 s2 = mkStream $ \st yld sng stp -> do
            let stop       = foldStream st yld sng stp s1
                single a   = yld a s1
                yieldk a r = yld a (wSerial s1 r)
             in foldStream st yieldk single stop s2

-- | Like `wSerial` but stops interleaving as soon as any of the two streams
-- stops.
--
-- @since 0.7.0
{-# INLINE wSerialMin #-}
wSerialMin :: IsStream t => t m a -> t m a -> t m a
wSerialMin m1 m2 = mkStream $ \st yld _ stp -> do
    let stop       = stp
        -- "single a" is defined as "yld a (wSerialMin m2 K.nil)" instead of
        -- "sng a" to keep the behaviour consistent with the yield continuation.
        single a   = yld a (wSerialMin m2 K.nil)
        yieldk a r = yld a (wSerialMin m2 r)
    foldStream st yieldk single stop m1

instance Semigroup (WSerialT m a) where
    (<>) = wSerial

infixr 5 <=>

-- | Same as 'wSerial'.
--
-- @since 0.1.0
{-# DEPRECATED (<=>) "Please use 'wSerial' instead." #-}
{-# INLINE (<=>) #-}
(<=>) :: IsStream t => t m a -> t m a -> t m a
(<=>) = wSerial

------------------------------------------------------------------------------
-- Monoid
------------------------------------------------------------------------------

instance Monoid (WSerialT m a) where
    mempty = K.nil
    mappend = (<>)

{-# INLINE apWSerial #-}
apWSerial :: Monad m => WSerialT m (a -> b) -> WSerialT m a -> WSerialT m b
apWSerial (WSerialT m1) (WSerialT m2) =
    let f x1 = K.concatMapBy wSerial (pure . x1) m2
    in WSerialT $ K.concatMapBy wSerial f m1

instance Monad m => Applicative (WSerialT m) where
    {-# INLINE pure #-}
    pure = WSerialT . K.yield
    {-# INLINE (<*>) #-}
    (<*>) = apWSerial

------------------------------------------------------------------------------
-- Monad
------------------------------------------------------------------------------

instance Monad m => Monad (WSerialT m) where
    return = pure
    {-# INLINE (>>=) #-}
    (>>=) = K.bindWith wSerial

------------------------------------------------------------------------------
-- Other instances
------------------------------------------------------------------------------

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
unfoldrM :: (IsStream t, Monad m) => (b -> m (Maybe (a, b))) -> b -> t m a
unfoldrM step seed = D.fromStreamD (D.unfoldrM step seed)
