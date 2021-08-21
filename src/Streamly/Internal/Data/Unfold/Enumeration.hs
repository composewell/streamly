-- |
-- Module      : Streamly.Internal.Data.Unfold.Enumeration
-- Copyright   : (c) 2019, 2021 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- The functions defined in this module should be rarely needed for direct use,
-- try to use the operations from the 'Enumerable' type class
-- instances instead.
--
-- This module provides an 'Enumerable' type class to enumerate 'Enum' types
-- into a stream. The operations in this type class correspond to similar
-- operations in the 'Enum' type class, the only difference is that they produce
-- a stream instead of a list. These operations cannot be defined generically
-- based on the 'Enum' type class. We provide instances for commonly used
-- types. If instances for other types are needed convenience functions defined
-- in this module can be used to define them. Alternatively, these functions
-- can be used directly.
--
module Streamly.Internal.Data.Unfold.Enumeration
    (
      Enumerable (..)

    -- ** Enumerate 'Num' Types
    , enumerateFromNum
    , enumerateFromThenNum

    -- ** Enumerating 'Bounded Integral' Types
    , enumerateFromIntegralBounded
    , enumerateFromThenIntegralBounded
    , enumerateFromToIntegralBounded
    , enumerateFromThenToIntegralBounded

    -- ** Enumerating 'Unbounded Integral' Types
    , enumerateFromIntegral
    , enumerateFromThenIntegral
    , enumerateFromToIntegral
    , enumerateFromThenToIntegral

    -- ** Enumerating 'Small Integral' Types
    , enumerateFromSmall
    , enumerateFromThenSmall
    , enumerateFromToSmall
    , enumerateFromThenToSmall

    -- ** Enumerating 'Fractional' Types
    , enumerateFromFractional
    , enumerateFromThenFractional
    , enumerateFromToFractional
    , enumerateFromThenToFractional
    )
where

#include "inline.hs"
import Data.Fixed
import Data.Bifunctor (bimap)
import Data.Int
import Data.Ratio
import Data.Word
import Numeric.Natural
import Data.Functor.Identity (Identity(..))
import Streamly.Internal.Data.Stream.StreamD.Step (Step(..))
import Streamly.Internal.Data.Unfold.Type
import Prelude
       hiding (map, mapM, takeWhile, take, filter, const, zipWith
              , drop, dropWhile)

------------------------------------------------------------------------------
-- Enumeration of Num
------------------------------------------------------------------------------

-- | Unfolds @(from, next)@ generating an infinite stream starting from @from@
-- and @next@ values with a stride of @(next - from)@.  After the value
-- overflows it keeps enumerating in a cycle:
--
-- $setup
-- >>> :m
-- >>> import qualified Streamly.Prelude as Stream
-- >>> import qualified Streamly.Internal.Data.Unfold as Unfold
-- >>> import Streamly.Internal.Data.Unfold.Type
-- >>> import Data.Word
--
-- >>> Stream.toList $ Stream.take 10 $ Stream.unfold Unfold.enumerateFromThenNum (255::Word8,0)
-- [255,0,1,2,3,4,5,6,7,8]
--
-- The implementation is numerically stable for floating point values.
--
-- Note 'enumerateFromStepIntegral' is faster for integrals.
--
-- /Internal/
--
{-# INLINE enumerateFromThenNum #-}
enumerateFromThenNum :: (Monad m, Num a) => Unfold m (a, a) a
enumerateFromThenNum = Unfold step inject

    where

    inject (!from, !next) = return (from, next - from, 0)

    -- Note that the counter "i" is the same type as the type being enumerated.
    -- It may overflow, for example, if we are enumerating Word8, after 255 the
    -- counter will become 0, but the overflow does not affect the enumeration
    -- behavior.
    {-# INLINE_LATE step #-}
    step (from, stride, i) =
        return $
            (Yield $! (from + i * stride)) $! (from, stride, i + 1)

-- | Same as 'enumerateFromThenNum' using a stride of 1.
--
-- >> enumerateFromNum = lmap (\from -> (from, from + 1)) enumerateFromThenNum
--
-- /Internal/
--
{-# INLINE enumerateFromNum #-}
enumerateFromNum :: (Monad m, Num a) => Unfold m a a
enumerateFromNum = Unfold step inject

-- XXX It loses the precisions if we use
-- enumerateFromNum = lmap (\from -> (from, from + 1)) enumerateFromThenNum
-- Example ‘S.unfold (UF.take 5 $ UF.enumerateFromFractional) 3.9’
-- [3.9,4.9,5.9,6.900000000000001,7.900000000000002]

    where

    inject !from = return (from, 1, 0)

    {-# INLINE_LATE step #-}
    step (from, stride, i) =
        return $
            (Yield $! (from + i * stride)) $! (from, stride, i + 1)

-- | Generate a finite stream starting from a from and next values with
-- stride of (next-from) till to value. The implementation is numerically
-- stable for floating point values.
--
-- Note 'enumerateFromStepIntegral' is faster for integrals.
--
-- /Internal/
--
{-# INLINE enumerateFromThenToNumBounded #-}
enumerateFromThenToNumBounded :: (Monad m, Ord a, Fractional a) =>
    Unfold m (a, a, a) a
enumerateFromThenToNumBounded = Unfold step inject

    where

    inject (!from, !next, !to) = return (from, next - from, to, 0)

    {-# INLINE_LATE step #-}
    step (from, stride, to, !i) = return $
        if stride == 0
        then if from <= to then Yield from $! (from, stride, to, i + 1) else Stop
        else
            if stride > 0
            then
                if (from + i * stride) <= to + stride / 2
                then (Yield $! (from + i * stride)) $! (from, stride, to, i + 1)
                else Stop
            else
                if (from + i * stride) >= to + stride / 2
                then (Yield $! (from + i * stride)) $! (from, stride, to, i + 1)
                else Stop

-- | Generate a finite stream starting from a from  till to value.
-- stride of value 1 will be used. The implementation is numerically
-- stable for floating point values.
--
-- Note 'enumerateFromStepIntegralBounded' is faster for integrals.
--
-- /Internal/
--
{-# INLINE enumerateFromToNumBounded #-}
enumerateFromToNumBounded :: (Monad m, Ord a, Fractional a) =>
    Unfold m (a, a) a
enumerateFromToNumBounded = Unfold step inject

    where

    inject (!from, !to) = return (from, 1, to, 0)

    {-# INLINE_LATE step #-}
    step (from, !stride, to, !i) = return $
            if (from + i * stride) <= to + stride / 2
            then (Yield $! (from + i * stride)) $! (from, stride, to, i + 1)
            else Stop

------------------------------------------------------------------------------
-- Enumeration of Integrals
------------------------------------------------------------------------------

-- | Can be used to enumerate unbounded integrals. This does not check for
-- overflow or underflow for bounded integrals.
--
-- /Internal/
{-# INLINE_NORMAL enumerateFromStepIntegral #-}
enumerateFromStepIntegral :: (Integral a, Monad m) => Unfold m (a, a) a
enumerateFromStepIntegral = Unfold step inject
    where
    inject (from, next) = from `seq` next `seq` return (from, next - from)
    {-# INLINE_LATE step #-}
    step (x, stride) = return $ Yield x $! (x + stride, stride)

-- | Can be used to enumerate bounded integrals from starting from with
-- stride of (next-from) . This check for overflow or underflow for
-- bounded integrals.
--
-- /Internal/
--
{-# INLINE_NORMAL enumerateFromStepIntegralBounded #-}
enumerateFromStepIntegralBounded ::
    forall a m. (Integral a, Bounded a, Monad m) =>
    Unfold m (a, a) a
enumerateFromStepIntegralBounded = Unfold step inject
    where
    inject (from, next) =
        from `seq` next `seq` return (from, next - from)
    {-# INLINE_LATE step #-}
    step (x, stride) = return $
        if stride == 0
        then
            Yield x $! (x, stride)

        else
            if stride > 0
            then
                if x <= (maxBound :: a)
                then Yield x $! (x + stride, stride)
                else Stop
            else
                if x >= (minBound :: a)
                then Yield x $! (x + stride, stride)
                else Stop

-- | Can be used to enumerate bounded integrals from starting from with
-- stride of (next-from) till to value. This check for overflow or underflow
--  for bounded integrals.
--
-- /Internal/
--
{-# INLINE_NORMAL enumerateFromToStepIntegralBounded #-}
enumerateFromToStepIntegralBounded ::
    forall a m. (Integral a, Bounded a, Monad m) =>
    Unfold m (a, a, a) a
enumerateFromToStepIntegralBounded = Unfold step inject
    where
    inject (from, next, to) =
        from `seq` next `seq` to `seq` return (from, next - from, to)
    {-# INLINE_LATE step #-}
    step (x, stride, to) = return $
        if stride == 0
        then
            if x <= to then Yield x $! (x, stride, to) else Stop

        else
            if stride > 0
            then
                if x <= to && x <= (maxBound :: a)
                then Yield x $! (x + stride, stride, to)
                else Stop
            else
                if x >= to && x >= (minBound :: a)
                then Yield x $! (x + stride, stride, to)
                else Stop

-- | Can be used to enumerate unbounded integrals. Startting with value from
-- with stride of next-from till value of to.
--
-- /Internal/
--
{-# INLINE_NORMAL enumerateFromToStepIntegral #-}
enumerateFromToStepIntegral :: (Integral a, Monad m) =>
    Unfold m (a, a, a) a
enumerateFromToStepIntegral = Unfold step inject
    where
    inject (from, next, to) =
        from `seq` next `seq` to `seq` return (from, next - from, to)
    {-# INLINE_LATE step #-}
    step (x, stride, to) = return $
        if stride == 0
        then
            if x <= to then Yield x $! (x, stride, to) else Stop

        else
            if stride > 0
            then
                if x <= to
                then Yield x $! (x + stride, stride, to)
                else Stop
            else
                if x >= to
                then Yield x $! (x + stride, stride, to)
                else Stop

-- Enumerate Unbounded Integrals ----------------------------------------------
{-# INLINE enumerateFromIntegral #-}
enumerateFromIntegral :: (Monad m, Integral a) => Unfold m a a
enumerateFromIntegral = lmap (\a -> (a, a + 1)) enumerateFromStepIntegral

{-# INLINE enumerateFromThenIntegral #-}
enumerateFromThenIntegral :: (Monad m, Integral a ) => Unfold m (a, a) a
enumerateFromThenIntegral = enumerateFromStepIntegral

{-# INLINE enumerateFromToIntegral #-}
enumerateFromToIntegral :: (Monad m, Integral a) => Unfold m (a, a) a
enumerateFromToIntegral = lmap (\(a, c) -> (a, a + 1, c)) enumerateFromToStepIntegral

{-# INLINE enumerateFromThenToIntegral #-}
enumerateFromThenToIntegral :: (Monad m, Integral a) => Unfold m (a, a, a) a
enumerateFromThenToIntegral = enumerateFromToStepIntegral

-- Enumerate Bounded Integrals ------------------------------------------------
{-# INLINE enumerateFromIntegralBounded #-}
enumerateFromIntegralBounded :: (Monad m, Bounded a, Integral a) =>
    Unfold m a a
enumerateFromIntegralBounded =
    lmap (\a -> (a, a + 1, maxBound)) enumerateFromToStepIntegralBounded

{-# INLINE enumerateFromThenIntegralBounded #-}
enumerateFromThenIntegralBounded :: (Monad m, Bounded a, Integral a ) =>
    Unfold m (a, a) a
enumerateFromThenIntegralBounded = enumerateFromStepIntegralBounded

{-# INLINE enumerateFromToIntegralBounded #-}
enumerateFromToIntegralBounded :: (Monad m, Integral a, Bounded a) =>
    Unfold m (a, a) a
enumerateFromToIntegralBounded =
    lmap (\(a, c) -> (a, a + 1, c)) enumerateFromToStepIntegralBounded

{-# INLINE enumerateFromThenToIntegralBounded #-}
enumerateFromThenToIntegralBounded :: (Monad m, Integral a, Bounded a) =>
    Unfold m (a, a, a) a
enumerateFromThenToIntegralBounded = enumerateFromToStepIntegralBounded

------------------------------------------------------------------------------
-- Enumeration of Fractionals
------------------------------------------------------------------------------

{-# INLINE_NORMAL enumerateFromFractional #-}
enumerateFromFractional :: (Monad m, Fractional a) => Unfold m a a
enumerateFromFractional =  enumerateFromNum

{-# INLINE_NORMAL enumerateFromThenFractional #-}
enumerateFromThenFractional :: (Monad m, Fractional a) => Unfold m (a, a) a
enumerateFromThenFractional = enumerateFromThenNum

{-# INLINE_NORMAL enumerateFromToFractional #-}
enumerateFromToFractional :: (Monad m, Fractional a, Ord a) =>
    Unfold m (a, a) a
enumerateFromToFractional  = enumerateFromToNumBounded

{-# INLINE_NORMAL enumerateFromThenToFractional #-}
enumerateFromThenToFractional :: (Monad m, Fractional a,  Ord a) =>
    Unfold m (a, a, a) a
enumerateFromThenToFractional  = enumerateFromThenToNumBounded

-------------------------------------------------------------------------------
-- Enumerable type class
-------------------------------------------------------------------------------

-- | Types that can be enumerated as a stream. The operations in this type
-- class are equivalent to those in the 'Enum' type class, except that these
-- generate a stream instead of a list. Use the functions in
-- "Streamly.Internal.Data.Unfold.Enumeration" module to define new instances.
--
-- /Pre-release/
class Enum a => Enumerable a where

    -- | Unfolds @from@ generating a stream starting with the element
    -- @from@, enumerating up to 'maxBound' when the type is 'Bounded' or
    -- generating an infinite stream when the type is not 'Bounded'.
    --
    -- >>> import qualified Streamly.Prelude as Stream
    -- >>> import qualified Streamly.Internal.Data.Unfold as Unfold
    --
    -- @
    -- >>> Stream.toList $ Stream.take 4 $ Stream.unfold Unfold.enumerateFrom (0 :: Int)
    -- [0,1,2,3]
    --
    -- @
    --
    -- For 'Fractional' types, enumeration is numerically stable. However, no
    -- overflow or underflow checks are performed.
    --
    -- @
    -- >>> Stream.toList $ Stream.take 4 $ Stream.unfold Unfold.enumerateFrom 1.1
    -- [1.1,2.1,3.1,4.1]
    --
    -- @
    --
    -- /Pre-release/
    --
    enumerateFrom :: Monad m => Unfold m a a

    -- | Unfolds @(from, to)@ generating a finite stream starting with the element
    -- @from@, enumerating the type up to the value @to@. If @to@ is smaller than
    -- @from@ then an empty stream is returned.
    --
    -- >>> import qualified Streamly.Prelude as Stream
    -- >>> import qualified Streamly.Internal.Data.Unfold as Unfold
    --
    -- @
    -- >>> Stream.toList $ Stream.unfold Unfold.enumerateFromTo (0, 4)
    -- [0,1,2,3,4]
    --
    -- @
    --
    -- For 'Fractional' types, the last element is equal to the specified @to@
    -- value after rounding to the nearest integral value.
    --
    -- @
    -- >>> Stream.toList $ Stream.unfold Unfold.enumerateFromTo (1.1, 4)
    -- [1.1,2.1,3.1,4.1]
    --
    -- >>> Stream.toList $ Stream.unfold Unfold.enumerateFromTo (1.1, 4.6)
    -- [1.1,2.1,3.1,4.1,5.1]
    --
    -- @
    --
    -- /Pre-release/
    enumerateFromTo :: Monad m => Unfold m (a, a) a

    -- | Unfolds @(from, then)@ generating a stream whose first element is
    -- @from@ and the successive elements are in increments of @then@.  Enumeration
    -- can occur downwards or upwards depending on whether @then@ comes before or
    -- after @from@. For 'Bounded' types the stream ends when 'maxBound' is
    -- reached, for unbounded types it keeps enumerating infinitely.
    --
    -- >>> import qualified Streamly.Prelude as Stream
    -- >>> import qualified Streamly.Internal.Data.Unfold as Unfold
    --
    -- @
    -- >>> Stream.toList $ Stream.take 4 $ Stream.unfold Unfold.enumerateFromThen (0, 2)
    -- [0,2,4,6]
    --
    -- >>> Stream.toList $ Stream.take 4 $ Stream.unfold Unfold.enumerateFromThen (0,(-2))
    -- [0,-2,-4,-6]
    --
    -- @
    --
    -- /Pre-release/
    enumerateFromThen :: Monad m => Unfold m (a, a) a

    -- | Unfolds @(from, then, to)@ generating a finite stream whose first element
    -- is @from@ and the successive elements are in increments of @then@ up to
    -- @to@. Enumeration can occur downwards or upwards depending on whether @then@
    -- comes before or after @from@.
    --
    -- >>> import qualified Streamly.Prelude as Stream
    -- >>> import qualified Streamly.Internal.Data.Unfold as Unfold
    --
    -- @
    -- >>> Stream.toList $ Stream.unfold Unfold.enumerateFromThenTo (0, 2, 6)
    -- [0,2,4,6]
    --
    -- >>> Stream.toList $ Stream.unfold Unfold.enumerateFromThenTo (0, (-2), (-6))
    -- [0,-2,-4,-6]
    --
    -- @
    --
    -- /Pre-release/
    enumerateFromThenTo :: Monad m => Unfold m (a, a, a) a

-------------------------------------------------------------------------------
-- Enumeration of Enum types not larger than Int
-------------------------------------------------------------------------------

-- | Enumerate from given starting Enum value 'from' with stride of 1 till
-- maxBound
--
-- /Internal/
--
{-# INLINE enumerateFromSmall #-}
enumerateFromSmall :: forall a m. (Monad m, Bounded a, Enum a) => Unfold m a a
enumerateFromSmall = map toEnum $
    Unfold step inject
    where
        inject from =
            fromEnum from
            `seq` return (fromEnum from, 1, fromEnum (maxBound :: a))
        {-# INLINE_LATE step #-}
        step (x, stride, to) = return $
            if x <= to then Yield x $! (x + stride, stride, to) else Stop

-- | Enumerate from given starting Enum value 'from' and next Enum value 'next'
-- with stride of (fromEnum next - fromEnum from) till maxBound.
--
-- /Internal/
--
{-# INLINE enumerateFromThenSmall #-}
enumerateFromThenSmall :: forall a m. (Monad m, Bounded a, Enum a) =>
    Unfold m (a, a) a
enumerateFromThenSmall = map toEnum $
    Unfold step inject
    where
        inject (from, next) =
            fromEnum from
            `seq` fromEnum next
            `seq` return
            ( fromEnum from
            , fromEnum next - fromEnum from
            , fromEnum (maxBound :: a)
            )
        {-# INLINE_LATE step #-}
        step (x, stride, to) = return $
            if stride == 0
                then
                    if x <= to then Yield x $! (x, stride, to) else Stop

                else
                    if stride > 0
                    then
                        if x <= to
                        then Yield x $! (x + stride, stride, to)
                        else Stop
                    else
                        if x >= 0
                        then Yield x $! (x + stride, stride, to)
                        else Stop

-- | Enumerate from given starting Enum value 'from' and to Enum value 'to'
-- with stride of 1 till to value.
--
-- /Internal/
--
{-# INLINE enumerateFromToSmall #-}
enumerateFromToSmall :: (Monad m, Enum a) => Unfold m (a, a) a
enumerateFromToSmall = map toEnum $
    Unfold step inject
    where
        inject (from, to) =
            fromEnum from
            `seq` fromEnum to
            `seq` return (fromEnum from, 1, fromEnum to)
        {-# INLINE_LATE step #-}
        step (x, stride, to) = return $
                if x <= to then Yield x $! (x + stride, stride, to) else Stop

-- | Enumerate from given starting Enum value 'from' and then Enum value 'next'
-- and to Enum value 'to' with stride of (fromEnum next - fromEnum from)
-- till to value.
--
-- /Internal/
--
{-# INLINE enumerateFromThenToSmall #-}
enumerateFromThenToSmall :: (Monad m, Enum a) => Unfold m (a, a, a) a
enumerateFromThenToSmall = map toEnum $
    Unfold step inject
    where
        inject (from, next, to) =
            fromEnum from
            `seq` fromEnum next
            `seq` fromEnum to
            `seq` return
            (fromEnum from, fromEnum next - fromEnum from, fromEnum to)
        {-# INLINE_LATE step #-}
        step (x, stride, to) = return $
            if stride == 0
            then
                if x <= to then Yield x $! (x, stride, to) else Stop

            else
                if stride > 0
                then
                    if x <= to
                    then Yield x $! (x + stride, stride, to)
                    else Stop
                else
                    if x >= 0 && x >= to
                    then Yield x $! (x + stride, stride, to)
                    else Stop

-------------------------------------------------------------------------------
-- Enumerable Instances
-------------------------------------------------------------------------------
--
-- For Enum types smaller than or equal to Int size.
#define ENUMERABLE_BOUNDED_SMALL(SMALL_TYPE)           \
instance Enumerable SMALL_TYPE where {                 \
    {-# INLINE enumerateFrom #-};                      \
    enumerateFrom = enumerateFromSmall;                \
    {-# INLINE enumerateFromThen #-};                  \
    enumerateFromThen = enumerateFromThenSmall;        \
    {-# INLINE enumerateFromTo #-};                    \
    enumerateFromTo = enumerateFromToSmall;            \
    {-# INLINE enumerateFromThenTo #-};                \
    enumerateFromThenTo = enumerateFromThenToSmall }

ENUMERABLE_BOUNDED_SMALL(())
ENUMERABLE_BOUNDED_SMALL(Bool)
ENUMERABLE_BOUNDED_SMALL(Ordering)
ENUMERABLE_BOUNDED_SMALL(Char)

-- For bounded Integral Enum types, may be larger than Int.
#define ENUMERABLE_BOUNDED_INTEGRAL(INTEGRAL_TYPE)          \
instance Enumerable INTEGRAL_TYPE where {                   \
    {-# INLINE enumerateFrom #-};                           \
    enumerateFrom = enumerateFromIntegralBounded;           \
    {-# INLINE enumerateFromThen #-};                       \
    enumerateFromThen = enumerateFromThenIntegralBounded;   \
    {-# INLINE enumerateFromTo #-};                         \
    enumerateFromTo = enumerateFromToIntegralBounded;       \
    {-# INLINE enumerateFromThenTo #-};                     \
    enumerateFromThenTo = enumerateFromThenToIntegralBounded }

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
    enumerateFrom = enumerateFromIntegral;                        \
    {-# INLINE enumerateFromThen #-};                             \
    enumerateFromThen = enumerateFromThenIntegral;                \
    {-# INLINE enumerateFromTo #-};                               \
    enumerateFromTo = enumerateFromToIntegral;                    \
    {-# INLINE enumerateFromThenTo #-};                           \
    enumerateFromThenTo = enumerateFromThenToIntegral }

ENUMERABLE_UNBOUNDED_INTEGRAL(Integer)
ENUMERABLE_UNBOUNDED_INTEGRAL(Natural)

#define ENUMERABLE_FRACTIONAL(FRACTIONAL_TYPE,CONSTRAINT)         \
instance (CONSTRAINT) => Enumerable FRACTIONAL_TYPE where {       \
    {-# INLINE enumerateFrom #-};                                 \
    enumerateFrom = enumerateFromFractional;                      \
    {-# INLINE enumerateFromThen #-};                             \
    enumerateFromThen = enumerateFromThenFractional;              \
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
    enumerateFrom =
        map Identity $ lmap runIdentity enumerateFrom
    {-# INLINE enumerateFromThen #-}
    enumerateFromThen =
        map Identity $ lmap (bimap runIdentity runIdentity) enumerateFromThen
    {-# INLINE enumerateFromTo #-}
    enumerateFromTo  =
        map Identity $ lmap (bimap runIdentity runIdentity) enumerateFromThen
    {-# INLINE enumerateFromThenTo #-}
    enumerateFromThenTo  =
        map Identity $
            lmap
            (\(from, next, to) ->
                 (runIdentity from, runIdentity next, runIdentity to))
            enumerateFromThenTo
