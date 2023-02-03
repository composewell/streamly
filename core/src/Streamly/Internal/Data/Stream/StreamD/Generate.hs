-- |
-- Module      : Streamly.Internal.Data.Stream.StreamD.Generate
-- Copyright   : (c) 2020 Composewell Technologies and Contributors
--               (c) Roman Leshchinskiy 2008-2010
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--

-- A few combinators in this module have been adapted from the vector package
-- (c) Roman Leshchinskiy. See the notes in specific combinators.
--
module Streamly.Internal.Data.Stream.StreamD.Generate
  (
    -- * Primitives
      nil
    , nilM
    , cons
    , consM

    -- * From 'Unfold'
    , unfold

    -- * Unfolding
    , unfoldr
    , unfoldrM

    -- * From Values
    , fromPure
    , fromEffect
    , repeat
    , repeatM
    , replicate
    , replicateM

    -- * Enumeration
    , enumerateFromStepIntegral
    , enumerateFromIntegral
    , enumerateFromThenIntegral
    , enumerateFromToIntegral
    , enumerateFromThenToIntegral

    , enumerateFromStepNum
    , enumerateFromNum
    , enumerateFromThenNum
    , enumerateFromToFractional
    , enumerateFromThenToFractional

    , enumerate
    , enumerateTo
    , Enumerable(..)

    -- * Time Enumeration
    , timesWith
    , absTimesWith
    , relTimesWith

    -- * From Generators
    -- | Generate a monadic stream from a seed.
    , fromIndices
    , fromIndicesM
    , generate
    , generateM

    -- * Iteration
    , iterate
    , iterateM

    -- * From Containers
    -- | Transform an input structure into a stream.

    , fromList
    , fromListM
    , fromFoldable
    , fromFoldableM

    -- * From Pointers
    , fromPtr
    , fromPtrN
    , fromByteStr#

    -- * Conversions
    , fromStreamK
    , toStreamK
    )
where

#include "inline.hs"
#include "ArrayMacros.h"

import Control.Monad.IO.Class (MonadIO(..))
import Data.Functor.Identity (Identity(..))
import Foreign.Ptr (Ptr, plusPtr)
import Foreign.Storable (Storable (peek), sizeOf)
import GHC.Exts (Addr#, Ptr (Ptr))
import Streamly.Internal.Data.Time.Clock
    (Clock(Monotonic), asyncClock, readClock)
import Streamly.Internal.Data.Time.Units
    (toAbsTime, AbsTime, toRelTime64, RelTime64, addToAbsTime64)

#ifdef USE_UNFOLDS_EVERYWHERE
import qualified Streamly.Internal.Data.Unfold as Unfold
import qualified Streamly.Internal.Data.Unfold.Enumeration as Unfold
#endif

import Data.Fixed
import Data.Int
import Data.Ratio
import Data.Word
import Numeric.Natural
import Prelude hiding (iterate, repeat, replicate, take, takeWhile)
import Streamly.Internal.Data.Stream.StreamD.Type

-- $setup
-- >>> :m
-- >>> import Control.Concurrent (threadDelay)
-- >>> import qualified Streamly.Data.Fold as Fold
-- >>> import qualified Streamly.Data.Unfold as Unfold
-- >>> import qualified Streamly.Internal.Data.Stream as Stream
-- >>> import GHC.Exts (Ptr (Ptr))

------------------------------------------------------------------------------
-- Primitives
------------------------------------------------------------------------------

-- XXX implement in terms of nilM?

-- | A stream that terminates without producing any output or side effect.
--
-- >>> Stream.fold Fold.toList Stream.nil
-- []
--
{-# INLINE_NORMAL nil #-}
nil :: Applicative m => Stream m a
nil = Stream (\_ _ -> pure Stop) ()

-- XXX implement in terms of consM?
-- cons x = consM (return x)

-- | Fuse a pure value at the head of an existing stream::
--
-- >>> s = 1 `Stream.cons` Stream.fromList [2,3]
-- >>> Stream.fold Fold.toList s
-- [1,2,3]
--
-- This function should not be used to construct a stream. If a stream is
-- constructed by successive use of this function it would take O(n^2) time to
-- consume the stream.
--
-- This function should only be used to statically fuse an element with a
-- stream. Do not use this recursively or where it cannot be inlined.
--
-- See "Streamly.Data.Stream.StreamK" for a 'cons' that can be used to
-- construct a stream recursively.
--
-- Definition:
--
-- >>> cons x xs = return x `Stream.consM` xs
--
{-# INLINE_NORMAL cons #-}
cons :: Applicative m => a -> Stream m a -> Stream m a
cons x (Stream step state) = Stream step1 Nothing
    where
    {-# INLINE_LATE step1 #-}
    step1 _ Nothing = pure $ Yield x (Just state)
    step1 gst (Just st) = do
          (\case
            Yield a s -> Yield a (Just s)
            Skip  s   -> Skip (Just s)
            Stop      -> Stop) <$> step gst st

------------------------------------------------------------------------------
-- Unfolding
------------------------------------------------------------------------------

-- Adapted from vector package
{-# INLINE_NORMAL unfoldrM #-}
unfoldrM :: Monad m => (s -> m (Maybe (a, s))) -> s -> Stream m a
#ifdef USE_UNFOLDS_EVERYWHERE
unfoldrM next = unfold (Unfold.unfoldrM next)
#else
unfoldrM next = Stream step
  where
    {-# INLINE_LATE step #-}
    step _ st = do
        r <- next st
        return $ case r of
            Just (x, s) -> Yield x s
            Nothing     -> Stop
#endif

{-# INLINE_LATE unfoldr #-}
unfoldr :: Monad m => (s -> Maybe (a, s)) -> s -> Stream m a
unfoldr f = unfoldrM (return . f)

------------------------------------------------------------------------------
-- From values
------------------------------------------------------------------------------

{-# INLINE_NORMAL repeatM #-}
repeatM :: Monad m => m a -> Stream m a
#ifdef USE_UNFOLDS_EVERYWHERE
repeatM = unfold Unfold.repeatM
#else
repeatM x = Stream (\_ _ -> x >>= \r -> return $ Yield r ()) ()
#endif

{-# INLINE_NORMAL repeat #-}
repeat :: Monad m => a -> Stream m a
#ifdef USE_UNFOLDS_EVERYWHERE
repeat x = repeatM (pure x)
#else
repeat x = Stream (\_ _ -> return $ Yield x ()) ()
#endif

-- Adapted from the vector package
{-# INLINE_NORMAL replicateM #-}
replicateM :: Monad m => Int -> m a -> Stream m a
#ifdef USE_UNFOLDS_EVERYWHERE
replicateM n p = unfold Unfold.replicateM (n, p)
#else
replicateM n p = Stream step n
  where
    {-# INLINE_LATE step #-}
    step _ (i :: Int)
      | i <= 0    = return Stop
      | otherwise = do
          x <- p
          return $ Yield x (i - 1)
#endif

{-# INLINE_NORMAL replicate #-}
replicate :: Monad m => Int -> a -> Stream m a
replicate n x = replicateM n (return x)

------------------------------------------------------------------------------
-- Enumeration of Num
------------------------------------------------------------------------------

-- | For floating point numbers if the increment is less than the precision then
-- it just gets lost. Therefore we cannot always increment it correctly by just
-- repeated addition.
-- 9007199254740992 + 1 + 1 :: Double => 9.007199254740992e15
-- 9007199254740992 + 2     :: Double => 9.007199254740994e15
--
-- Instead we accumulate the increment counter and compute the increment
-- every time before adding it to the starting number.
--
-- This works for Integrals as well as floating point numbers, but
-- enumerateFromStepIntegral is faster for integrals.
{-# INLINE_NORMAL enumerateFromStepNum #-}
enumerateFromStepNum :: (Monad m, Num a) => a -> a -> Stream m a
#ifdef USE_UNFOLDS_EVERYWHERE
enumerateFromStepNum from stride =
    unfold Unfold.enumerateFromStepNum (from, stride)
#else
enumerateFromStepNum from stride = Stream step 0
    where
    {-# INLINE_LATE step #-}
    step _ !i = return $ (Yield $! (from + i * stride)) $! (i + 1)
#endif

{-# INLINE_NORMAL enumerateFromNum #-}
enumerateFromNum :: (Monad m, Num a) => a -> Stream m a
enumerateFromNum from = enumerateFromStepNum from 1

{-# INLINE_NORMAL enumerateFromThenNum #-}
enumerateFromThenNum :: (Monad m, Num a) => a -> a -> Stream m a
enumerateFromThenNum from next = enumerateFromStepNum from (next - from)

------------------------------------------------------------------------------
-- Enumeration of Integrals
------------------------------------------------------------------------------

#ifndef USE_UNFOLDS_EVERYWHERE
data EnumState a = EnumInit | EnumYield a a a | EnumStop

{-# INLINE_NORMAL enumerateFromThenToIntegralUp #-}
enumerateFromThenToIntegralUp
    :: (Monad m, Integral a)
    => a -> a -> a -> Stream m a
enumerateFromThenToIntegralUp from next to = Stream step EnumInit
    where
    {-# INLINE_LATE step #-}
    step _ EnumInit =
        return $
            if to < next
            then if to < from
                 then Stop
                 else Yield from EnumStop
            else -- from <= next <= to
                let stride = next - from
                in Skip $ EnumYield from stride (to - stride)

    step _ (EnumYield x stride toMinus) =
        return $
            if x > toMinus
            then Yield x EnumStop
            else Yield x $ EnumYield (x + stride) stride toMinus

    step _ EnumStop = return Stop

{-# INLINE_NORMAL enumerateFromThenToIntegralDn #-}
enumerateFromThenToIntegralDn
    :: (Monad m, Integral a)
    => a -> a -> a -> Stream m a
enumerateFromThenToIntegralDn from next to = Stream step EnumInit
    where
    {-# INLINE_LATE step #-}
    step _ EnumInit =
        return $ if to > next
            then if to > from
                 then Stop
                 else Yield from EnumStop
            else -- from >= next >= to
                let stride = next - from
                in Skip $ EnumYield from stride (to - stride)

    step _ (EnumYield x stride toMinus) =
        return $
            if x < toMinus
            then Yield x EnumStop
            else Yield x $ EnumYield (x + stride) stride toMinus

    step _ EnumStop = return Stop
#endif

-- XXX This can perhaps be simplified and written in terms of
-- enumeratFromStepIntegral as we have done in unfolds. But anyway we should be
-- replacing the stream generation module with unfolds.
{-# INLINE_NORMAL enumerateFromThenToIntegral #-}
enumerateFromThenToIntegral
    :: (Monad m, Integral a)
    => a -> a -> a -> Stream m a
#ifdef USE_UNFOLDS_EVERYWHERE
enumerateFromThenToIntegral from next to =
    unfold Unfold.enumerateFromThenToIntegral (from, next, to)
#else
enumerateFromThenToIntegral from next to
    | next >= from = enumerateFromThenToIntegralUp from next to
    | otherwise    = enumerateFromThenToIntegralDn from next to
#endif

{-# INLINE_NORMAL enumerateFromThenIntegral #-}
enumerateFromThenIntegral
    :: (Monad m, Integral a, Bounded a)
    => a -> a -> Stream m a
#ifdef USE_UNFOLDS_EVERYWHERE
enumerateFromThenIntegral from next =
    unfold Unfold.enumerateFromThenIntegralBounded (from, next)
#else
enumerateFromThenIntegral from next =
    if next > from
    then enumerateFromThenToIntegralUp from next maxBound
    else enumerateFromThenToIntegralDn from next minBound
#endif

-- | Can be used to enumerate unbounded integrals. This does not check for
-- overflow or underflow for bounded integrals.
--
{-# INLINE_NORMAL enumerateFromStepIntegral #-}
enumerateFromStepIntegral :: (Integral a, Monad m) => a -> a -> Stream m a
#ifdef USE_UNFOLDS_EVERYWHERE
enumerateFromStepIntegral from stride =
    unfold Unfold.enumerateFromStepIntegral (from, stride)
#else
enumerateFromStepIntegral from stride =
    from `seq` stride `seq` Stream step from
    where
        {-# INLINE_LATE step #-}
        step _ !x = return $ Yield x $! (x + stride)
#endif

-- | Enumerate upwards from @from@ to @to@. We are assuming that "to" is
-- constrained by the type to be within max/min bounds.
{-# INLINE enumerateFromToIntegral #-}
enumerateFromToIntegral :: (Monad m, Integral a) => a -> a -> Stream m a
enumerateFromToIntegral from to =
    takeWhile (<= to) $ enumerateFromStepIntegral from 1

{-# INLINE enumerateFromIntegral #-}
enumerateFromIntegral :: (Monad m, Integral a, Bounded a) => a -> Stream m a
enumerateFromIntegral from = enumerateFromToIntegral from maxBound

------------------------------------------------------------------------------
-- Enumeration of Fractionals
------------------------------------------------------------------------------

-- | We cannot write a general function for Num.  The only way to write code
-- portable between the two is to use a 'Real' constraint and convert between
-- Fractional and Integral using fromRational which is horribly slow.
{-# INLINE_NORMAL enumerateFromToFractional #-}
enumerateFromToFractional
    :: (Monad m, Fractional a, Ord a)
    => a -> a -> Stream m a
enumerateFromToFractional from to =
    takeWhile (<= to + 1 / 2) $ enumerateFromStepNum from 1

{-# INLINE_NORMAL enumerateFromThenToFractional #-}
enumerateFromThenToFractional
    :: (Monad m, Fractional a, Ord a)
    => a -> a -> a -> Stream m a
enumerateFromThenToFractional from next to =
    takeWhile predicate $ enumerateFromThenNum from next
    where
    mid = (next - from) / 2
    predicate | next >= from  = (<= to + mid)
              | otherwise     = (>= to + mid)

-------------------------------------------------------------------------------
-- Enumeration of Enum types not larger than Int
-------------------------------------------------------------------------------
--
-- | 'enumerateFromTo' for 'Enum' types not larger than 'Int'.
--
{-# INLINE enumerateFromToSmall #-}
enumerateFromToSmall :: (Monad m, Enum a) => a -> a -> Stream m a
enumerateFromToSmall from to =
      fmap toEnum
    $ enumerateFromToIntegral (fromEnum from) (fromEnum to)

-- | 'enumerateFromThenTo' for 'Enum' types not larger than 'Int'.
--
{-# INLINE enumerateFromThenToSmall #-}
enumerateFromThenToSmall :: (Monad m, Enum a)
    => a -> a -> a -> Stream m a
enumerateFromThenToSmall from next to =
          fmap toEnum
        $ enumerateFromThenToIntegral
            (fromEnum from) (fromEnum next) (fromEnum to)

-- | 'enumerateFromThen' for 'Enum' types not larger than 'Int'.
--
-- Note: We convert the 'Enum' to 'Int' and enumerate the 'Int'. If a
-- type is bounded but does not have a 'Bounded' instance then we can go on
-- enumerating it beyond the legal values of the type, resulting in the failure
-- of 'toEnum' when converting back to 'Enum'. Therefore we require a 'Bounded'
-- instance for this function to be safely used.
--
{-# INLINE enumerateFromThenSmallBounded #-}
enumerateFromThenSmallBounded :: (Monad m, Enumerable a, Bounded a)
    => a -> a -> Stream m a
enumerateFromThenSmallBounded from next =
    if fromEnum next >= fromEnum from
    then enumerateFromThenTo from next maxBound
    else enumerateFromThenTo from next minBound

-------------------------------------------------------------------------------
-- Enumerable type class
-------------------------------------------------------------------------------
--
-- NOTE: We would like to rewrite calls to fromList [1..] etc. to stream
-- enumerations like this:
--
-- {-# RULES "fromList enumFrom" [1]
--     forall (a :: Int). D.fromList (enumFrom a) = D.enumerateFromIntegral a #-}
--
-- But this does not work because enumFrom is a class method and GHC rewrites
-- it quickly, so we do not get a chance to have our rule fired.

-- | Types that can be enumerated as a stream. The operations in this type
-- class are equivalent to those in the 'Enum' type class, except that these
-- generate a stream instead of a list. Use the functions in
-- "Streamly.Internal.Data.Stream.Enumeration" module to define new instances.
--
class Enum a => Enumerable a where
    -- | @enumerateFrom from@ generates a stream starting with the element
    -- @from@, enumerating up to 'maxBound' when the type is 'Bounded' or
    -- generating an infinite stream when the type is not 'Bounded'.
    --
    -- @
    -- >>> Stream.fold Fold.toList $ Stream.take 4 $ Stream.enumerateFrom (0 :: Int)
    -- [0,1,2,3]
    --
    -- @
    --
    -- For 'Fractional' types, enumeration is numerically stable. However, no
    -- overflow or underflow checks are performed.
    --
    -- @
    -- >>> Stream.fold Fold.toList $ Stream.take 4 $ Stream.enumerateFrom 1.1
    -- [1.1,2.1,3.1,4.1]
    --
    -- @
    --
    enumerateFrom :: (Monad m) => a -> Stream m a

    -- | Generate a finite stream starting with the element @from@, enumerating
    -- the type up to the value @to@. If @to@ is smaller than @from@ then an
    -- empty stream is returned.
    --
    -- @
    -- >>> Stream.fold Fold.toList $ Stream.enumerateFromTo 0 4
    -- [0,1,2,3,4]
    --
    -- @
    --
    -- For 'Fractional' types, the last element is equal to the specified @to@
    -- value after rounding to the nearest integral value.
    --
    -- @
    -- >>> Stream.fold Fold.toList $ Stream.enumerateFromTo 1.1 4
    -- [1.1,2.1,3.1,4.1]
    --
    -- >>> Stream.fold Fold.toList $ Stream.enumerateFromTo 1.1 4.6
    -- [1.1,2.1,3.1,4.1,5.1]
    --
    -- @
    --
    enumerateFromTo :: (Monad m) => a -> a -> Stream m a

    -- | @enumerateFromThen from then@ generates a stream whose first element
    -- is @from@, the second element is @then@ and the successive elements are
    -- in increments of @then - from@.  Enumeration can occur downwards or
    -- upwards depending on whether @then@ comes before or after @from@. For
    -- 'Bounded' types the stream ends when 'maxBound' is reached, for
    -- unbounded types it keeps enumerating infinitely.
    --
    -- @
    -- >>> Stream.fold Fold.toList $ Stream.take 4 $ Stream.enumerateFromThen 0 2
    -- [0,2,4,6]
    --
    -- >>> Stream.fold Fold.toList $ Stream.take 4 $ Stream.enumerateFromThen 0 (-2)
    -- [0,-2,-4,-6]
    --
    -- @
    --
    enumerateFromThen :: (Monad m) => a -> a -> Stream m a

    -- | @enumerateFromThenTo from then to@ generates a finite stream whose
    -- first element is @from@, the second element is @then@ and the successive
    -- elements are in increments of @then - from@ up to @to@. Enumeration can
    -- occur downwards or upwards depending on whether @then@ comes before or
    -- after @from@.
    --
    -- @
    -- >>> Stream.fold Fold.toList $ Stream.enumerateFromThenTo 0 2 6
    -- [0,2,4,6]
    --
    -- >>> Stream.fold Fold.toList $ Stream.enumerateFromThenTo 0 (-2) (-6)
    -- [0,-2,-4,-6]
    --
    -- @
    --
    enumerateFromThenTo :: (Monad m) => a -> a -> a -> Stream m a

-- MAYBE: Sometimes it is more convenient to know the count rather then the
-- ending or starting element. For those cases we can define the folllowing
-- APIs. All of these will work only for bounded types if we represent the
-- count by Int.
--
-- enumerateN
-- enumerateFromN
-- enumerateToN
-- enumerateFromStep
-- enumerateFromStepN

-------------------------------------------------------------------------------
-- Convenient functions for bounded types
-------------------------------------------------------------------------------
--
-- |
-- > enumerate = enumerateFrom minBound
--
-- Enumerate a 'Bounded' type from its 'minBound' to 'maxBound'
--
{-# INLINE enumerate #-}
enumerate :: (Monad m, Bounded a, Enumerable a) => Stream m a
enumerate = enumerateFrom minBound

-- |
-- > enumerateTo = enumerateFromTo minBound
--
-- Enumerate a 'Bounded' type from its 'minBound' to specified value.
--
{-# INLINE enumerateTo #-}
enumerateTo :: (Monad m, Bounded a, Enumerable a) => a -> Stream m a
enumerateTo = enumerateFromTo minBound

-- |
-- > enumerateFromBounded = enumerateFromTo from maxBound
--
-- 'enumerateFrom' for 'Bounded' 'Enum' types.
--
{-# INLINE enumerateFromBounded #-}
enumerateFromBounded :: (Monad m, Enumerable a, Bounded a)
    => a -> Stream m a
enumerateFromBounded from = enumerateFromTo from maxBound

-------------------------------------------------------------------------------
-- Enumerable Instances
-------------------------------------------------------------------------------
--
-- For Enum types smaller than or equal to Int size.
#define ENUMERABLE_BOUNDED_SMALL(SMALL_TYPE)           \
instance Enumerable SMALL_TYPE where {                 \
    {-# INLINE enumerateFrom #-};                      \
    enumerateFrom = enumerateFromBounded;              \
    {-# INLINE enumerateFromThen #-};                  \
    enumerateFromThen = enumerateFromThenSmallBounded; \
    {-# INLINE enumerateFromTo #-};                    \
    enumerateFromTo = enumerateFromToSmall;            \
    {-# INLINE enumerateFromThenTo #-};                \
    enumerateFromThenTo = enumerateFromThenToSmall }

ENUMERABLE_BOUNDED_SMALL(())
ENUMERABLE_BOUNDED_SMALL(Bool)
ENUMERABLE_BOUNDED_SMALL(Ordering)
ENUMERABLE_BOUNDED_SMALL(Char)

-- For bounded Integral Enum types, may be larger than Int.
#define ENUMERABLE_BOUNDED_INTEGRAL(INTEGRAL_TYPE)  \
instance Enumerable INTEGRAL_TYPE where {           \
    {-# INLINE enumerateFrom #-};                   \
    enumerateFrom = enumerateFromIntegral;          \
    {-# INLINE enumerateFromThen #-};               \
    enumerateFromThen = enumerateFromThenIntegral;  \
    {-# INLINE enumerateFromTo #-};                 \
    enumerateFromTo = enumerateFromToIntegral;      \
    {-# INLINE enumerateFromThenTo #-};             \
    enumerateFromThenTo = enumerateFromThenToIntegral }

ENUMERABLE_BOUNDED_INTEGRAL(Int)
ENUMERABLE_BOUNDED_INTEGRAL(Int8)
ENUMERABLE_BOUNDED_INTEGRAL(Int16)
ENUMERABLE_BOUNDED_INTEGRAL(Int32)
ENUMERABLE_BOUNDED_INTEGRAL(Int64)
ENUMERABLE_BOUNDED_INTEGRAL(Word)
ENUMERABLE_BOUNDED_INTEGRAL(Word8)
ENUMERABLE_BOUNDED_INTEGRAL(Word16)
ENUMERABLE_BOUNDED_INTEGRAL(Word32)
ENUMERABLE_BOUNDED_INTEGRAL(Word64)

-- For unbounded Integral Enum types.
#define ENUMERABLE_UNBOUNDED_INTEGRAL(INTEGRAL_TYPE)              \
instance Enumerable INTEGRAL_TYPE where {                         \
    {-# INLINE enumerateFrom #-};                                 \
    enumerateFrom from = enumerateFromStepIntegral from 1;        \
    {-# INLINE enumerateFromThen #-};                             \
    enumerateFromThen from next =                                 \
        enumerateFromStepIntegral from (next - from);             \
    {-# INLINE enumerateFromTo #-};                               \
    enumerateFromTo = enumerateFromToIntegral;                    \
    {-# INLINE enumerateFromThenTo #-};                           \
    enumerateFromThenTo = enumerateFromThenToIntegral }

ENUMERABLE_UNBOUNDED_INTEGRAL(Integer)
ENUMERABLE_UNBOUNDED_INTEGRAL(Natural)

#define ENUMERABLE_FRACTIONAL(FRACTIONAL_TYPE,CONSTRAINT)         \
instance (CONSTRAINT) => Enumerable FRACTIONAL_TYPE where {     \
    {-# INLINE enumerateFrom #-};                                 \
    enumerateFrom = enumerateFromNum;                      \
    {-# INLINE enumerateFromThen #-};                             \
    enumerateFromThen = enumerateFromThenNum;              \
    {-# INLINE enumerateFromTo #-};                               \
    enumerateFromTo = enumerateFromToFractional;                  \
    {-# INLINE enumerateFromThenTo #-};                           \
    enumerateFromThenTo = enumerateFromThenToFractional }

ENUMERABLE_FRACTIONAL(Float,)
ENUMERABLE_FRACTIONAL(Double,)
ENUMERABLE_FRACTIONAL((Fixed a),HasResolution a)
ENUMERABLE_FRACTIONAL((Ratio a),Integral a)

instance Enumerable a => Enumerable (Identity a) where
    {-# INLINE enumerateFrom #-}
    enumerateFrom (Identity from) =
        fmap Identity $ enumerateFrom from
    {-# INLINE enumerateFromThen #-}
    enumerateFromThen (Identity from) (Identity next) =
        fmap Identity $ enumerateFromThen from next
    {-# INLINE enumerateFromTo #-}
    enumerateFromTo (Identity from) (Identity to) =
        fmap Identity $ enumerateFromTo from to
    {-# INLINE enumerateFromThenTo #-}
    enumerateFromThenTo (Identity from) (Identity next) (Identity to) =
          fmap Identity
        $ enumerateFromThenTo from next to

-- TODO
{-
instance Enumerable a => Enumerable (Last a)
instance Enumerable a => Enumerable (First a)
instance Enumerable a => Enumerable (Max a)
instance Enumerable a => Enumerable (Min a)
instance Enumerable a => Enumerable (Const a b)
instance Enumerable (f a) => Enumerable (Alt f a)
instance Enumerable (f a) => Enumerable (Ap f a)
-}
------------------------------------------------------------------------------
-- Time Enumeration
------------------------------------------------------------------------------

-- | @timesWith g@ returns a stream of time value tuples. The first component
-- of the tuple is an absolute time reference (epoch) denoting the start of the
-- stream and the second component is a time relative to the reference.
--
-- The argument @g@ specifies the granularity of the relative time in seconds.
-- A lower granularity clock gives higher precision but is more expensive in
-- terms of CPU usage. Any granularity lower than 1 ms is treated as 1 ms.
--
-- >>> import Control.Concurrent (threadDelay)
-- >>> f = Fold.drainMapM (\x -> print x >> threadDelay 1000000)
-- >>> Stream.fold f $ Stream.take 3 $ Stream.timesWith 0.01
-- (AbsTime (TimeSpec {sec = ..., nsec = ...}),RelTime64 (NanoSecond64 ...))
-- (AbsTime (TimeSpec {sec = ..., nsec = ...}),RelTime64 (NanoSecond64 ...))
-- (AbsTime (TimeSpec {sec = ..., nsec = ...}),RelTime64 (NanoSecond64 ...))
--
-- Note: This API is not safe on 32-bit machines.
--
-- /Pre-release/
--
{-# INLINE_NORMAL timesWith #-}
timesWith :: MonadIO m => Double -> Stream m (AbsTime, RelTime64)
timesWith g = Stream step Nothing

    where

    {-# INLINE_LATE step #-}
    step _ Nothing = do
        clock <- liftIO $ asyncClock Monotonic g
        a <- liftIO $ readClock clock
        return $ Skip $ Just (clock, a)

    step _ s@(Just (clock, t0)) = do
        a <- liftIO $ readClock clock
        -- XXX we can perhaps use an AbsTime64 using a 64 bit Int for
        -- efficiency.  or maybe we can use a representation using Double for
        -- floating precision time
        return $ Yield (toAbsTime t0, toRelTime64 (a - t0)) s

-- | @absTimesWith g@ returns a stream of absolute timestamps using a clock of
-- granularity @g@ specified in seconds. A low granularity clock is more
-- expensive in terms of CPU usage.  Any granularity lower than 1 ms is treated
-- as 1 ms.
--
-- >>> f = Fold.drainMapM print
-- >>> Stream.fold f $ Stream.delayPre 1 $ Stream.take 3 $ Stream.absTimesWith 0.01
-- AbsTime (TimeSpec {sec = ..., nsec = ...})
-- AbsTime (TimeSpec {sec = ..., nsec = ...})
-- AbsTime (TimeSpec {sec = ..., nsec = ...})
--
-- Note: This API is not safe on 32-bit machines.
--
-- /Pre-release/
--
{-# INLINE absTimesWith #-}
absTimesWith :: MonadIO m => Double -> Stream m AbsTime
absTimesWith = fmap (uncurry addToAbsTime64) . timesWith

-- | @relTimesWith g@ returns a stream of relative time values starting from 0,
-- using a clock of granularity @g@ specified in seconds. A low granularity
-- clock is more expensive in terms of CPU usage.  Any granularity lower than 1
-- ms is treated as 1 ms.
--
-- >>> f = Fold.drainMapM print
-- >>> Stream.fold f $ Stream.delayPre 1 $ Stream.take 3 $ Stream.relTimesWith 0.01
-- RelTime64 (NanoSecond64 ...)
-- RelTime64 (NanoSecond64 ...)
-- RelTime64 (NanoSecond64 ...)
--
-- Note: This API is not safe on 32-bit machines.
--
-- /Pre-release/
--
{-# INLINE relTimesWith #-}
relTimesWith :: MonadIO m => Double -> Stream m RelTime64
relTimesWith = fmap snd . timesWith

-------------------------------------------------------------------------------
-- From Generators
-------------------------------------------------------------------------------

{-# INLINE_NORMAL fromIndicesM #-}
fromIndicesM :: Monad m => (Int -> m a) -> Stream m a
#ifdef USE_UNFOLDS_EVERYWHERE
fromIndicesM gen = unfold (Unfold.fromIndicesM gen) 0
#else
fromIndicesM gen = Stream step 0
  where
    {-# INLINE_LATE step #-}
    step _ i = do
       x <- gen i
       return $ Yield x (i + 1)
#endif

{-# INLINE fromIndices #-}
fromIndices :: Monad m => (Int -> a) -> Stream m a
fromIndices gen = fromIndicesM (return . gen)

-- Adapted from the vector package
{-# INLINE_NORMAL generateM #-}
generateM :: Monad m => Int -> (Int -> m a) -> Stream m a
generateM n gen = n `seq` Stream step 0
  where
    {-# INLINE_LATE step #-}
    step _ i | i < n     = do
                           x <- gen i
                           return $ Yield x (i + 1)
             | otherwise = return Stop

{-# INLINE generate #-}
generate :: Monad m => Int -> (Int -> a) -> Stream m a
generate n gen = generateM n (return . gen)

-------------------------------------------------------------------------------
-- Iteration
-------------------------------------------------------------------------------

{-# INLINE_NORMAL iterateM #-}
iterateM :: Monad m => (a -> m a) -> m a -> Stream m a
#ifdef USE_UNFOLDS_EVERYWHERE
iterateM step = unfold (Unfold.iterateM step)
#else
iterateM step = Stream (\_ st -> st >>= \(!x) -> return $ Yield x (step x))
#endif

{-# INLINE_NORMAL iterate #-}
iterate :: Monad m => (a -> a) -> a -> Stream m a
iterate step st = iterateM (return . step) (return st)

-------------------------------------------------------------------------------
-- From containers
-------------------------------------------------------------------------------

-- | Convert a list of monadic actions to a 'Stream'
{-# INLINE_LATE fromListM #-}
fromListM :: Monad m => [m a] -> Stream m a
#ifdef USE_UNFOLDS_EVERYWHERE
fromListM = unfold Unfold.fromListM
#else
fromListM = Stream step
  where
    {-# INLINE_LATE step #-}
    step _ (m:ms) = m >>= \x -> return $ Yield x ms
    step _ []     = return Stop
#endif

-- |
-- >>> fromFoldable = Prelude.foldr Stream.cons Stream.nil
--
-- Construct a stream from a 'Foldable' containing pure values:
--
-- /WARNING: O(n^2), suitable only for a small number of
-- elements in the stream/
--
{-# INLINE fromFoldable #-}
fromFoldable :: (Monad m, Foldable f) => f a -> Stream m a
fromFoldable = Prelude.foldr cons nil

-- |
-- >>> fromFoldableM = Prelude.foldr Stream.consM Stream.nil
--
-- Construct a stream from a 'Foldable' containing pure values:
--
-- /WARNING: O(n^2), suitable only for a small number of
-- elements in the stream/
--
{-# INLINE fromFoldableM #-}
fromFoldableM :: (Monad m, Foldable f) => f (m a) -> Stream m a
fromFoldableM = Prelude.foldr consM nil

-------------------------------------------------------------------------------
-- From pointers
-------------------------------------------------------------------------------

-- | Read an infinite stream from a pointer, advancing the pointer as needed.
-- The caller is responsible to end the stream safely.
{-# INLINE fromPtr #-}
fromPtr :: forall m a. (MonadIO m, Storable a) => Ptr a -> Stream m a
fromPtr = Stream step

    where

    {-# INLINE_LATE step #-}
    step _ p = do
        x <- liftIO $ peek p
        return $ Yield x (PTR_NEXT(p, a))

-- | Take @n@ 'Storable' elements starting from 'Ptr' onwards.
--
-- >>> fromPtrN n = Stream.take n . Stream.fromPtr
--
-- /Unsafe:/ The caller is responsible for safe addressing.
--
-- /Pre-release/
{-# INLINE fromPtrN #-}
fromPtrN :: (MonadIO m, Storable a) => Int -> Ptr a -> Stream m a
fromPtrN n = take n . fromPtr

-- | Read bytes from an 'Addr#' until a 0 byte is encountered, the 0 byte is
-- not included in the stream.
--
-- >>> fromByteStr# addr = Stream.takeWhile (/= 0) $ Stream.fromPtr $ Ptr addr
--
-- /Unsafe:/ The caller is responsible for safe addressing.
--
-- Note that this is completely safe when reading from Haskell string
-- literals because they are guaranteed to be NULL terminated:
--
-- >>> Stream.fold Fold.toList $ Stream.fromByteStr# "\1\2\3\0"#
-- [1,2,3]
--
{-# INLINE fromByteStr# #-}
fromByteStr# :: MonadIO m => Addr# -> Stream m Word8
fromByteStr# addr =
    takeWhile (/= 0) $ fromPtr $ Ptr addr
