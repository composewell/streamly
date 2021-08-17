-- |
-- Module      : Streamly.Internal.Data.Unfold.Enumeration
-- Copyright   : (c) 2019 Composewell Technologies
-- License     : BSD3
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

    -- ** Enumerate Num
    , enumerateFromStepNum
    , numFrom
    , numFromThen

    -- ** Enumerating 'Bounded' 'Integral' Types
    , enumerateFromIntegral
    , enumerateFromThenIntegral

    -- ** Enumerating 'Integral' Types
    , enumerateFromToIntegral
    , enumerateFromThenToIntegral

    -- ** Enumerating unbounded 'Integral' Types
    , enumerateFromStepIntegral

    -- ** Enumerating 'Fractional' Types
    , enumerateFromFractional
    , enumerateFromToFractional
    , enumerateFromThenFractional
    , enumerateFromThenToFractional
    )
where

#include "inline.hs"
import Data.Fixed
import Data.Int
import Data.Ratio
import Data.Word
import Numeric.Natural
import Data.Functor.Identity (Identity(..))
import Streamly.Internal.Data.Unfold.Type
import Streamly.Internal.Data.Stream.StreamD.Step (Step(..))
import Prelude
       hiding (map, mapM, takeWhile, take, filter, const, zipWith
              , drop, dropWhile)

------------------------------------------------------------------------------
-- Enumeration of Num
------------------------------------------------------------------------------

-- | Generate an infinite stream starting from a starting value with increments
-- of the given stride.  The implementation is numerically stable for floating
-- point values.
--
-- Note 'enumerateFromStepIntegral' is faster for integrals.
--
-- /Pre-release/
--
{-# INLINE enumerateFromStepNum #-}
enumerateFromStepNum :: (Monad m, Num a) => a -> Unfold m a a
enumerateFromStepNum stride = Unfold step inject

    where

    inject !from = return (from, 0)

    {-# INLINE_LATE step #-}
    step (from, !i) = return $ (Yield $! (from + i * stride)) $! (from, i + 1)

-- | @numFrom = enumerateFromStepNum 1@
--
-- /Pre-release/
--
{-# INLINE_NORMAL numFrom #-}
numFrom :: (Monad m, Num a) => Unfold m a a
numFrom = enumerateFromStepNum 1

{-# INLINE_NORMAL numFromThen #-}
numFromThen :: (Monad m, Num a) => a -> Unfold m a a
numFromThen = enumerateFromStepNum

------------------------------------------------------------------------------
-- Enumeration of Integrals
------------------------------------------------------------------------------

-- | Can be used to enumerate unbounded integrals. This does not check for
-- overflow or underflow for bounded integrals.
{-# INLINE_NORMAL enumerateFromStepIntegral #-}
enumerateFromStepIntegral :: (Integral a, Monad m) => Unfold m (a, a) a
enumerateFromStepIntegral = Unfold step inject
    where
    inject (from, stride) = from `seq` stride `seq` return (from, stride)
    {-# INLINE_LATE step #-}
    step (x, stride) = return $ Yield x $! (x + stride, stride)

-- We are assuming that "to" is constrained by the type to be within
-- max/min bounds.
{-# INLINE enumerateFromToIntegral #-}
enumerateFromToIntegral :: (Monad m, Integral a) => a -> Unfold m a a
enumerateFromToIntegral to =
    takeWhile (<= to) $ supplySecond 1 enumerateFromStepIntegral

{-# INLINE enumerateFromThenToIntegral #-}
enumerateFromThenToIntegral :: (Monad m, Integral a) => a -> a -> Unfold m a a
enumerateFromThenToIntegral stride to =
    takeWhile (<= to) $ supplySecond stride enumerateFromStepIntegral

{-# INLINE enumerateFromIntegral #-}
enumerateFromIntegral :: (Monad m, Integral a, Bounded a) => Unfold m a a
enumerateFromIntegral = enumerateFromToIntegral maxBound

{-# INLINE enumerateFromThenIntegral #-}
enumerateFromThenIntegral :: (Monad m, Integral a, Bounded a) => a -> Unfold m a a
enumerateFromThenIntegral stride = enumerateFromThenToIntegral stride maxBound

------------------------------------------------------------------------------
-- Enumeration of Fractionals
------------------------------------------------------------------------------

-- | /Internal/
--
-- > enumerateFromToFractional to = takeWhile (<= to + 1 / 2) $ enumerateFromStepNum 1
--
{-# INLINE_NORMAL enumerateFromToFractional #-}
enumerateFromToFractional :: (Monad m, Fractional a, Ord a) => a -> Unfold m a a
enumerateFromToFractional to =
    takeWhile (<= to + 1 / 2) $ enumerateFromStepNum 1

{-# INLINE_NORMAL enumerateFromFractional #-}
enumerateFromFractional :: (Monad m, Fractional a) => Unfold m a a
enumerateFromFractional =  enumerateFromStepNum 1

{-# INLINE_NORMAL enumerateFromThenToFractional #-}
enumerateFromThenToFractional :: (Monad m, Fractional a, Ord a) => a -> a -> Unfold m a a
enumerateFromThenToFractional stride to =
    takeWhile (<= to + 1 / 2) $ enumerateFromStepNum stride

{-# INLINE_NORMAL enumerateFromThenFractional #-}
enumerateFromThenFractional :: (Monad m, Fractional a) => a -> Unfold m a a
enumerateFromThenFractional = enumerateFromStepNum

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

-- @enumerateFrom from@ generates a stream starting with the element
-- @from@, enumerating up to 'maxBound' when the type is 'Bounded' or
-- generating an infinite stream when the type is not 'Bounded'.
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

-- Generate a finite stream starting with the element @from@, enumerating
-- the type up to the value @to@. If @to@ is smaller than @from@ then an
-- empty stream is returned.
--
-- @
-- >>> Stream.toList $ Stream.unfold (Unfold.enumerateFromTo 4) 0
-- [0,1,2,3,4]
--
-- @
--
-- For 'Fractional' types, the last element is equal to the specified @to@
-- value after rounding to the nearest integral value.
--
-- @
-- >>> Stream.toList $ Stream.unfold (Unfold.enumerateFromTo 4) 1.1
-- [1.1,2.1,3.1,4.1]
--
-- >>> Stream.toList $ Stream.unfold (Unfold.enumerateFromTo 4.6) 1.1
-- [1.1,2.1,3.1,4.1,5.1]
--
-- @
--
-- /Pre-release/
    enumerateFromTo :: Monad m => a -> Unfold m a a

-- @enumerateFromThen from then@ generates a stream whose first element
-- is @from@ and the successive elements are
-- in increments of @then@.  Enumeration can occur downwards or
-- upwards depending on whether @then@ comes before or after @from@. For
-- 'Bounded' types the stream ends when 'maxBound' is reached, for
-- unbounded types it keeps enumerating infinitely.
--
-- @
-- >>> Stream.toList $ Stream.take 4 $ Stream.unfold (Unfold.enumerateFromThen 2) 0
-- [0,2,4,6]
--
-- >>> Stream.toList $ Stream.take 4 $ Stream.unfold (Unfold.enumerateFromThen (-2)) 0
-- [0,-2,-4,-6]
--
-- @
--
-- /Pre-release/
    enumerateFromThen :: Monad m => a -> Unfold m a a

-- @enumerateFromThenTo from then to@ generates a finite stream whose
-- first element is @from@ and the successive elements are in
-- increments of @then@ up to @to@. Enumeration can
-- occur downwards or upwards depending on whether @then@ comes before or
-- after @from@.
--
-- @
-- >>> Stream.toList $ Stream.unfold (Unfold.enumerateFromThenTo 2 6) 0
-- [0,2,4,6]
--
-- >>> Stream.toList $ Stream.unfold (Unfold.enumerateFromThenTo (-2) (-6)) 0
-- [0,-2,-4,-6]
--
-- @
--
-- /Pre-release/
    enumerateFromThenTo :: Monad m => a-> a -> Unfold m a a

-------------------------------------------------------------------------------
-- Enumerable Instances
-------------------------------------------------------------------------------
--
-- For Enum types smaller than or equal to Int size.
#define ENUMERABLE_BOUNDED_SMALL(SMALL_TYPE)           \
instance Enumerable SMALL_TYPE where {                 \
    {-# INLINE enumerateFrom #-};                      \
    enumerateFrom = undefined;             \
    {-# INLINE enumerateFromThen #-};                  \
    enumerateFromThen = undefined;     \
    {-# INLINE enumerateFromTo #-};                    \
    enumerateFromTo = undefined;         \
    {-# INLINE enumerateFromThenTo #-};                \
    enumerateFromThenTo = undefined }

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
    enumerateFrom = supplySecond 1 enumerateFromStepIntegral;     \
    {-# INLINE enumerateFromThen #-};                             \
    enumerateFromThen next =                                      \
        lmap (\from -> (from, next - from)) enumerateFromStepIntegral; \
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
    enumerateFrom = map Identity $ lmap runIdentity enumerateFrom
    {-# INLINE enumerateFromThen #-}
    enumerateFromThen = undefined
    {-# INLINE enumerateFromTo #-}
    enumerateFromTo = undefined
    {-# INLINE enumerateFromThenTo #-}
    enumerateFromThenTo = undefined
