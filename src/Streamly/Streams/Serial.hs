{-# LANGUAGE CPP                       #-}
{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving#-}
{-# LANGUAGE InstanceSigs              #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE UndecidableInstances      #-} -- XXX

-- |
-- Module      : Streamly.Streams.Serial
-- Copyright   : (c) 2017 Harendra Kumar
--
-- License     : BSD3
-- Maintainer  : harendra.kumar@gmail.com
-- Stability   : experimental
-- Portability : GHC
--
--
module Streamly.Streams.Serial
    (
    -- * Serial appending stream
      SerialT
    , StreamT           -- deprecated
    , Serial
    , K.serial
    , serially

    -- * Serial interleaving stream
    , WSerialT
    , InterleavedT      -- deprecated
    , WSerial
    , wSerial
    , wSerialFst
    , wSerialMin
    , (<=>)            -- deprecated
    , wSerially
    , interleaving     -- deprecated

    -- * Transformation
    , map
    , mapM
    )
where

import Control.Applicative (liftA2)
import Control.DeepSeq (NFData(..))
#if MIN_VERSION_deepseq(1,4,3)
import Control.DeepSeq (NFData1(..))
#endif
import Control.Monad (ap)
import Control.Monad.Base (MonadBase(..), liftBaseDefault)
import Control.Monad.Catch (MonadThrow, throwM)
-- import Control.Monad.Error.Class   (MonadError(..))
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Reader.Class (MonadReader(..))
import Control.Monad.State.Class (MonadState(..))
import Control.Monad.Trans.Class (MonadTrans(lift))
import Data.Functor.Identity (Identity(..), runIdentity)
import Data.Foldable (fold)
#if __GLASGOW_HASKELL__ < 808
import Data.Semigroup (Semigroup(..))
#endif
import GHC.Exts (IsList(..), IsString(..))
import Text.Read (Lexeme(Ident), lexP, parens, prec, readPrec, readListPrec,
                  readListPrecDefault)
import Prelude hiding (map, mapM)

import Streamly.Streams.StreamK (IsStream(..), adapt, Stream, mkStream,
                                 foldStream)
import qualified Streamly.Streams.Prelude as P
import qualified Streamly.Streams.StreamK as K
import qualified Streamly.Streams.StreamD as D

#include "Instances.hs"
#include "inline.hs"

------------------------------------------------------------------------------
-- SerialT
------------------------------------------------------------------------------

-- | The 'Semigroup' operation for 'SerialT' behaves like a regular append
-- operation.  Therefore, when @a <> b@ is evaluated, stream @a@ is evaluated
-- first until it exhausts and then stream @b@ is evaluated. In other words,
-- the elements of stream @b@ are appended to the elements of stream @a@. This
-- operation can be used to fold an infinite lazy container of streams.
--
-- @
-- import Streamly
-- import qualified "Streamly.Prelude" as S
--
-- main = (S.toList . 'serially' $ (S.fromList [1,2]) \<\> (S.fromList [3,4])) >>= print
-- @
-- @
-- [1,2,3,4]
-- @
--
-- The 'Monad' instance runs the /monadic continuation/ for each
-- element of the stream, serially.
--
-- @
-- main = S.drain . 'serially' $ do
--     x <- return 1 \<\> return 2
--     S.yieldM $ print x
-- @
-- @
-- 1
-- 2
-- @
--
-- 'SerialT' nests streams serially in a depth first manner.
--
-- @
-- main = S.drain . 'serially' $ do
--     x <- return 1 \<\> return 2
--     y <- return 3 \<\> return 4
--     S.yieldM $ print (x, y)
-- @
-- @
-- (1,3)
-- (1,4)
-- (2,3)
-- (2,4)
-- @
--
-- We call the monadic code being run for each element of the stream a monadic
-- continuation. In imperative paradigm we can think of this composition as
-- nested @for@ loops and the monadic continuation is the body of the loop. The
-- loop iterates for all elements of the stream.
--
-- Note that the behavior and semantics  of 'SerialT', including 'Semigroup'
-- and 'Monad' instances are exactly like Haskell lists except that 'SerialT'
-- can contain effectful actions while lists are pure.
--
-- In the code above, the 'serially' combinator can be omitted as the default
-- stream type is 'SerialT'.
--
-- @since 0.2.0
newtype SerialT m a = SerialT {getSerialT :: Stream m a}
    deriving (Semigroup, Monoid, MonadTrans)

-- | A serial IO stream of elements of type @a@. See 'SerialT' documentation
-- for more details.
--
-- @since 0.2.0
type Serial = SerialT IO

-- |
-- @since 0.1.0
{-# DEPRECATED StreamT "Please use 'SerialT' instead." #-}
type StreamT = SerialT

-- | Fix the type of a polymorphic stream as 'SerialT'.
--
-- @since 0.1.0
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
    {-# INLINE (>>=) #-}
    (>>=) = K.bindWith K.serial

    -- StreamD based implementation
    -- return = SerialT . D.fromStreamD . D.yield
    -- m >>= f = D.fromStreamD $ D.concatMap (\a -> D.toStreamD (f a)) (D.toStreamD m)

------------------------------------------------------------------------------
-- Other instances
------------------------------------------------------------------------------

{-# INLINE_EARLY mapM #-}
mapM :: (IsStream t, Monad m) => (a -> m b) -> t m a -> t m b
mapM f m = fromStream $ D.toStreamK $ D.mapM f $ D.fromStreamK (toStream m)

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

MONAD_APPLICATIVE_INSTANCE(SerialT,)
MONAD_COMMON_INSTANCES(SerialT,)
LIST_INSTANCES(SerialT)
NFDATA1_INSTANCE(SerialT)
FOLDABLE_INSTANCE(SerialT)
TRAVERSABLE_INSTANCE(SerialT)

------------------------------------------------------------------------------
-- WSerialT
------------------------------------------------------------------------------

-- | The 'Semigroup' operation for 'WSerialT' interleaves the elements from the
-- two streams.  Therefore, when @a <> b@ is evaluated, stream @a@ is evaluated
-- first to produce the first element of the combined stream and then stream
-- @b@ is evaluated to produce the next element of the combined stream, and
-- then we go back to evaluating stream @a@ and so on. In other words, the
-- elements of stream @a@ are interleaved with the elements of stream @b@.
--
-- Note that when multiple actions are combined like @a <> b <> c ... <> z@ we
-- interleave them in a binary fashion i.e. @a@ and @b@ are interleaved with
-- each other and the result is interleaved with @c@ and so on. This will not
-- act as a true round-robin scheduling across all the streams.  Note that this
-- operation cannot be used to fold a container of infinite streams as the
-- state that it needs to maintain is proportional to the number of streams.
--
-- @
-- import Streamly
-- import qualified "Streamly.Prelude" as S
--
-- main = (S.toList . 'wSerially' $ (S.fromList [1,2]) \<\> (S.fromList [3,4])) >>= print
-- @
-- @
-- [1,3,2,4]
-- @
--
-- Similarly, the 'Monad' instance interleaves the iterations of the
-- inner and the outer loop, nesting loops in a breadth first manner.
--
--
-- @
-- main = S.drain . 'wSerially' $ do
--     x <- return 1 \<\> return 2
--     y <- return 3 \<\> return 4
--     S.yieldM $ print (x, y)
-- @
-- @
-- (1,3)
-- (2,3)
-- (1,4)
-- (2,4)
-- @
--
-- @since 0.2.0
newtype WSerialT m a = WSerialT {getWSerialT :: Stream m a}
    deriving (MonadTrans)

-- | An interleaving serial IO stream of elements of type @a@. See 'WSerialT'
-- documentation for more details.
--
-- @since 0.2.0
type WSerial = WSerialT IO

-- |
-- @since 0.1.0
{-# DEPRECATED InterleavedT "Please use 'WSerialT' instead." #-}
type InterleavedT = WSerialT

-- | Fix the type of a polymorphic stream as 'WSerialT'.
--
-- @since 0.2.0
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

-- Additionally we can have m elements yield from the first stream and n
-- elements yeilding from the second stream. We can also have time slicing
-- variants of positional interleaving, e.g. run first stream for m seconds and
-- run the second stream for n seconds.
--
-- Similar combinators can be implemented using WAhead style.

-- | Polymorphic version of the 'Semigroup' operation '<>' of 'WSerialT'.
-- Interleaves two streams, yielding one element from each stream alternately.
-- When one stream stops the rest of the other stream is used in the output
-- stream.
--
-- @since 0.2.0
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
wSerialMin m1 m2 = mkStream $ \st yld sng stp -> do
    let stop       = stp
        single a   = sng a
        yieldk a r = yld a (wSerial m2 r)
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

MONAD_APPLICATIVE_INSTANCE(WSerialT,)
MONAD_COMMON_INSTANCES(WSerialT,)
LIST_INSTANCES(WSerialT)
NFDATA1_INSTANCE(WSerialT)
FOLDABLE_INSTANCE(WSerialT)
TRAVERSABLE_INSTANCE(WSerialT)
