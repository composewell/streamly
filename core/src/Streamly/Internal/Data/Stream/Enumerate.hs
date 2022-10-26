-- |
-- Module      : Streamly.Internal.Data.Stream.Enumerate
-- Copyright   : (c) 2018 Composewell Technologies
--
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
-- perations in the 'Enum' type class, the only difference is that they produce
-- a stream instead of a list. These operations cannot be defined generically
-- based on the 'Enum' type class. We provide instances for commonly used
-- types. If instances for other types are needed convenience functions defined
-- in this module can be used to define them. Alternatively, these functions
-- can be used directly.

-- XXX The Unfold.Enumeration module is more modular, check the differences and
-- reconcile the two.

module Streamly.Internal.Data.Stream.Enumerate
    (
      Enumerable (..)

    -- ** Enumerating 'Bounded' 'Enum' Types
    , enumerate
    , enumerateTo
    , enumerateFromBounded

    -- ** Enumerating 'Enum' Types not larger than 'Int'
    , enumerateFromToSmall
    , enumerateFromThenToSmall
    , enumerateFromThenSmallBounded

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

import Data.Fixed
import Data.Int
import Data.Ratio
import Data.Word
import Numeric.Natural
import Data.Functor.Identity (Identity(..))

import Streamly.Internal.Data.Stream.Type (Stream, fromStreamD)

import qualified Streamly.Internal.Data.Stream.StreamD.Generate as D

-- $setup
-- >>> import Streamly.Data.Fold as Fold
-- >>> import Streamly.Internal.Data.Stream as Stream
-- >>> import Streamly.Internal.Data.Stream.Enumerate as Stream

-------------------------------------------------------------------------------
-- Enumeration of Integral types
-------------------------------------------------------------------------------
--
-- | @enumerateFromStepIntegral from step@ generates an infinite stream whose
-- first element is @from@ and the successive elements are in increments of
-- @step@.
--
-- CAUTION: This function is not safe for finite integral types. It does not
-- check for overflow, underflow or bounds.
--
-- @
-- >>> Stream.fold Fold.toList $ Stream.take 4 $ Stream.enumerateFromStepIntegral 0 2
-- [0,2,4,6]
--
-- >>> Stream.fold Fold.toList $ Stream.take 3 $ Stream.enumerateFromStepIntegral 0 (-2)
-- [0,-2,-4]
--
-- @
--
{-# INLINE enumerateFromStepIntegral #-}
enumerateFromStepIntegral
    :: (Monad m, Integral a)
    => a -> a -> Stream m a
enumerateFromStepIntegral from stride =
    fromStreamD $ D.enumerateFromStepIntegral from stride

-- | Enumerate an 'Integral' type. @enumerateFromIntegral from@ generates a
-- stream whose first element is @from@ and the successive elements are in
-- increments of @1@. The stream is bounded by the size of the 'Integral' type.
--
-- @
-- >>> Stream.fold Fold.toList $ Stream.take 4 $ Stream.enumerateFromIntegral (0 :: Int)
-- [0,1,2,3]
--
-- @
--
{-# INLINE enumerateFromIntegral #-}
enumerateFromIntegral
    :: (Monad m, Integral a, Bounded a)
    => a -> Stream m a
enumerateFromIntegral from = fromStreamD $ D.enumerateFromIntegral from

-- | Enumerate an 'Integral' type in steps. @enumerateFromThenIntegral from
-- then@ generates a stream whose first element is @from@, the second element
-- is @then@ and the successive elements are in increments of @then - from@.
-- The stream is bounded by the size of the 'Integral' type.
--
-- @
-- >>> Stream.fold Fold.toList $ Stream.take 4 $ Stream.enumerateFromThenIntegral (0 :: Int) 2
-- [0,2,4,6]
--
-- >>> Stream.fold Fold.toList $ Stream.take 4 $ Stream.enumerateFromThenIntegral (0 :: Int) (-2)
-- [0,-2,-4,-6]
--
-- @
--
{-# INLINE enumerateFromThenIntegral #-}
enumerateFromThenIntegral
    :: (Monad m, Integral a, Bounded a)
    => a -> a -> Stream m a
enumerateFromThenIntegral from next =
    fromStreamD $ D.enumerateFromThenIntegral from next

-- | Enumerate an 'Integral' type up to a given limit.
-- @enumerateFromToIntegral from to@ generates a finite stream whose first
-- element is @from@ and successive elements are in increments of @1@ up to
-- @to@.
--
-- @
-- >>> Stream.fold Fold.toList $ Stream.enumerateFromToIntegral 0 4
-- [0,1,2,3,4]
--
-- @
--
{-# INLINE enumerateFromToIntegral #-}
enumerateFromToIntegral :: (Monad m, Integral a) => a -> a -> Stream m a
enumerateFromToIntegral from to =
    fromStreamD $ D.enumerateFromToIntegral from to

-- | Enumerate an 'Integral' type in steps up to a given limit.
-- @enumerateFromThenToIntegral from then to@ generates a finite stream whose
-- first element is @from@, the second element is @then@ and the successive
-- elements are in increments of @then - from@ up to @to@.
--
-- @
-- >>> Stream.fold Fold.toList $ Stream.enumerateFromThenToIntegral 0 2 6
-- [0,2,4,6]
--
-- >>> Stream.fold Fold.toList $ Stream.enumerateFromThenToIntegral 0 (-2) (-6)
-- [0,-2,-4,-6]
--
-- @
--
{-# INLINE enumerateFromThenToIntegral #-}
enumerateFromThenToIntegral
    :: (Monad m, Integral a)
    => a -> a -> a -> Stream m a
enumerateFromThenToIntegral from next to =
    fromStreamD $ D.enumerateFromThenToIntegral from next to

-------------------------------------------------------------------------------
-- Enumeration of Fractional types
-------------------------------------------------------------------------------
--
-- Even though the underlying implementation of enumerateFromFractional and
-- enumerateFromThenFractional works for any 'Num' we have restricted these to
-- 'Fractional' because these do not perform any bounds check, in contrast to
-- integral versions and are therefore not equivalent substitutes for those.
--
-- | Numerically stable enumeration from a 'Fractional' number in steps of size
-- @1@. @enumerateFromFractional from@ generates a stream whose first element
-- is @from@ and the successive elements are in increments of @1@.  No overflow
-- or underflow checks are performed.
--
-- This is the equivalent to 'enumFrom' for 'Fractional' types. For example:
--
-- @
-- >>> Stream.fold Fold.toList $ Stream.take 4 $ Stream.enumerateFromFractional 1.1
-- [1.1,2.1,3.1,4.1]
--
-- @
--
--
{-# INLINE enumerateFromFractional #-}
enumerateFromFractional :: (Monad m, Fractional a) => a -> Stream m a
enumerateFromFractional from = fromStreamD $ D.enumerateFromNum from

-- | Numerically stable enumeration from a 'Fractional' number in steps.
-- @enumerateFromThenFractional from then@ generates a stream whose first
-- element is @from@, the second element is @then@ and the successive elements
-- are in increments of @then - from@.  No overflow or underflow checks are
-- performed.
--
-- This is the equivalent of 'enumFromThen' for 'Fractional' types. For
-- example:
--
-- @
-- >>> Stream.fold Fold.toList $ Stream.take 4 $ Stream.enumerateFromThenFractional 1.1 2.1
-- [1.1,2.1,3.1,4.1]
--
-- >>> Stream.fold Fold.toList $ Stream.take 4 $ Stream.enumerateFromThenFractional 1.1 (-2.1)
-- [1.1,-2.1,-5.300000000000001,-8.500000000000002]
--
-- @
--
{-# INLINE enumerateFromThenFractional #-}
enumerateFromThenFractional
    :: (Monad m, Fractional a)
    => a -> a -> Stream m a
enumerateFromThenFractional from next = fromStreamD $ D.enumerateFromThenNum from next

-- | Numerically stable enumeration from a 'Fractional' number to a given
-- limit.  @enumerateFromToFractional from to@ generates a finite stream whose
-- first element is @from@ and successive elements are in increments of @1@ up
-- to @to@.
--
-- This is the equivalent of 'enumFromTo' for 'Fractional' types. For
-- example:
--
-- @
-- >>> Stream.fold Fold.toList $ Stream.enumerateFromToFractional 1.1 4
-- [1.1,2.1,3.1,4.1]
--
-- >>> Stream.fold Fold.toList $ Stream.enumerateFromToFractional 1.1 4.6
-- [1.1,2.1,3.1,4.1,5.1]
--
-- @
--
-- Notice that the last element is equal to the specified @to@ value after
-- rounding to the nearest integer.
--
{-# INLINE enumerateFromToFractional #-}
enumerateFromToFractional
    :: (Monad m, Fractional a, Ord a)
    => a -> a -> Stream m a
enumerateFromToFractional from to =
    fromStreamD $ D.enumerateFromToFractional from to

-- | Numerically stable enumeration from a 'Fractional' number in steps up to a
-- given limit.  @enumerateFromThenToFractional from then to@ generates a
-- finite stream whose first element is @from@, the second element is @then@
-- and the successive elements are in increments of @then - from@ up to @to@.
--
-- This is the equivalent of 'enumFromThenTo' for 'Fractional' types. For
-- example:
--
-- @
-- >>> Stream.fold Fold.toList $ Stream.enumerateFromThenToFractional 0.1 2 6
-- [0.1,2.0,3.9,5.799999999999999]
--
-- >>> Stream.fold Fold.toList $ Stream.enumerateFromThenToFractional 0.1 (-2) (-6)
-- [0.1,-2.0,-4.1000000000000005,-6.200000000000001]
--
-- @
--
--
{-# INLINE enumerateFromThenToFractional #-}
enumerateFromThenToFractional
    :: (Monad m, Fractional a, Ord a)
    => a -> a -> a -> Stream m a
enumerateFromThenToFractional from next to =
    fromStreamD $ D.enumerateFromThenToFractional from next to

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
