{-# LANGUAGE CPP #-}
-- |
-- Module      : Streamly.Internal.Data.Stream.Enumeration
-- Copyright   : (c) 2020 Composewell Technologies and Contributors
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- NOTE: keep this module in sync with the
-- Streamly.Internal.Data.Unfold.Enumeration.hs module.
--
module Streamly.Internal.Data.Stream.Enumeration
  (
    -- NOTE: We enumerate up variants only, we can add enumerateDown* variants
    -- as well, to enumerate downwards to the final value. Currently that is
    -- achieved by enumerateFromThen variants.

    -- ** Enumerable Type Class
      Enumerable (..)

    -- ** 'Num' Type class Types
    -- | Most general operations via the 'Num' type class. All other
    -- enumeraitons can be expressed in terms of these. These are numerically
    -- stable for floating precision numbers. For that reason these may be
    -- slightly less efficient than intergal operations.
    --
    -- These are unbounded, can overflow, there are no bounds possible for
    -- 'Num'.
    , enumerateFromStepNum
    , enumerateFromNum
    , enumerateFromThenNum

    -- ** 'Integral' Type class Types (Unbounded Enumeration)
    -- | More efficient than 'Num' based operations for 'Integral' types.
    , enumerateFromStepIntegralUnbounded
    , enumerateFromIntegralUnbounded
    , enumerateFromThenIntegralUnbounded

    -- ** 'Integral' Types (Bounded Enumeration)
    -- | These are implemented in terms of integral operations using maxBound
    -- or minBound as the terminating codition. This is the default behavior
    -- for 'Integral' enumeration, used when no 'Unbounded' suffix is
    -- specified; use the explicit 'Unbounded' suffixed operations above for
    -- faster, unchecked enumeration.
    , enumerateFromIntegral
    , enumerateFromToIntegral
    , enumerateFromThenIntegral
    , enumerateFromThenToIntegral
    , enumerateFromThenUpToIntegral

    -- ** 'Enum' Types not larger than 'Int'
    -- | These are implemented by converting Enum to Int and using integral
    -- operations.
    , enumerateFromSmall
    , enumerateFromToSmall
    , enumerateFromThenSmall
    , enumerateFromThenToSmall

    -- ** 'Fractional' Types
    -- | These are simply specialization of Num based operations to Fractional
    -- types.
    , enumerateFromFractional
    , enumerateFromToFractional
    , enumerateFromThenFractional
    , enumerateFromThenToFractional

    -- ** Convenient functions using 'Enumerable' type class
    , enumerate
    , enumerateTo

    -- * Deprecated
    , enumerateFromBounded
    , enumerateFromThenSmallBounded
    , enumerateFromStepIntegral
    )
where

#include "inline.hs"

import Data.Fixed
import Data.Functor.Identity (Identity(..))
import Data.Int
import Data.Ratio
import Data.Word
import Numeric.Natural
import Streamly.Internal.Data.Stream.Type

#ifdef USE_UNFOLDS_EVERYWHERE
import qualified Streamly.Internal.Data.Unfold as Unfold
#else
import qualified Streamly.Internal.Data.Producer as Producer
#endif
import Prelude hiding (takeWhile)

#include "DocTestDataStream.hs"

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
-- enumerateFromStepIntegralUnbounded is faster for integrals.
{-# INLINE_NORMAL enumerateFromStepNum #-}
enumerateFromStepNum :: (Monad m, Num a) => a -> a -> Stream m a
#ifdef USE_UNFOLDS_EVERYWHERE
enumerateFromStepNum from stride =
    unfold Unfold.enumerateFromStepNum (from, stride)
#else
enumerateFromStepNum !from !stride =
    Stream (const Producer.enumerateFromStepNum) (from, stride, 0)
#endif

{-# INLINE_NORMAL enumerateFromThenNum #-}
enumerateFromThenNum :: (Monad m, Num a) => a -> a -> Stream m a
enumerateFromThenNum from next = enumerateFromStepNum from (next - from)

{-# INLINE_NORMAL enumerateFromNum #-}
enumerateFromNum :: (Monad m, Num a) => a -> Stream m a
enumerateFromNum from = enumerateFromStepNum from 1

------------------------------------------------------------------------------
-- Enumeration of Integrals
------------------------------------------------------------------------------

-- | @enumerateFromStepIntegralUnbounded from step@ generates an infinite
-- stream whose first element is @from@ and the successive elements are in
-- increments of @step@.
--
-- CAUTION: This function is not safe for finite integral types. It does not
-- check for overflow, underflow or bounds.
--
-- >>> Stream.toList $ Stream.take 4 $ Stream.enumerateFromStepIntegralUnbounded 0 2
-- [0,2,4,6]
--
-- >>> Stream.toList $ Stream.take 3 $ Stream.enumerateFromStepIntegralUnbounded 0 (-2)
-- [0,-2,-4]
--
{-# INLINE_NORMAL enumerateFromStepIntegralUnbounded #-}
enumerateFromStepIntegralUnbounded :: (Integral a, Monad m) => a -> a -> Stream m a
#ifdef USE_UNFOLDS_EVERYWHERE
enumerateFromStepIntegralUnbounded from stride =
    unfold Unfold.enumerateFromStepIntegralUnbounded (from, stride)
#else
enumerateFromStepIntegralUnbounded from stride =
    from `seq` stride `seq`
        Stream (const Producer.enumerateFromStepIntegral) (from, stride)
#endif

{-# DEPRECATED enumerateFromStepIntegral "Please use enumerateFromStepIntegralUnbounded instead." #-}
{-# INLINE enumerateFromStepIntegral #-}
enumerateFromStepIntegral :: (Integral a, Monad m) => a -> a -> Stream m a
enumerateFromStepIntegral = enumerateFromStepIntegralUnbounded

-- | Enumerate an 'Integral' type in steps up to a given limit.
-- @enumerateFromThenToIntegral from then to@ generates a finite stream whose
-- first element is @from@, the second element is @then@ and the successive
-- elements are in increments of @then - from@ up to @to@.
--
-- >>> Stream.toList $ Stream.enumerateFromThenToIntegral 0 2 6
-- [0,2,4,6]
--
-- >>> Stream.toList $ Stream.enumerateFromThenToIntegral 0 (-2) (-6)
-- [0,-2,-4,-6]
--
{-# INLINE_NORMAL enumerateFromThenToIntegral #-}
enumerateFromThenToIntegral
    :: (Monad m, Integral a)
    => a -> a -> a -> Stream m a
#ifdef USE_UNFOLDS_EVERYWHERE
enumerateFromThenToIntegral from next to =
    unfold Unfold.enumerateFromThenToIntegral (from, next, to)
#else
enumerateFromThenToIntegral from next to =
    Stream
        (const Producer.enumerateFromThenToIntegral)
        (Producer.EnumInit from next to)
#endif

-- | Like 'enumerateFromThenToIntegral' but a simplified version that only
-- works in the upward direction i.e. return empty stream if @then < from@.
--
-- >>> Stream.toList $ Stream.enumerateFromThenUpToIntegral 0 2 6
-- [0,2,4,6]
--
{-# INLINE_NORMAL enumerateFromThenUpToIntegral #-}
enumerateFromThenUpToIntegral
    :: (Monad m, Integral a)
    => a -> a -> a -> Stream m a
enumerateFromThenUpToIntegral from next to =
    Stream
        (const Producer.enumerateFromThenUpToIntegral)
        (Producer.EnumUpInit from next to)

-- | Enumerate an 'Integral' type in steps. @enumerateFromThenIntegral from
-- then@ generates a stream whose first element is @from@, the second element
-- is @then@ and the successive elements are in increments of @then - from@.
-- The stream is bounded by the size of the 'Integral' type.
--
-- >>> Stream.toList $ Stream.take 4 $ Stream.enumerateFromThenIntegral (0 :: Int) 2
-- [0,2,4,6]
--
-- >>> Stream.toList $ Stream.take 4 $ Stream.enumerateFromThenIntegral (0 :: Int) (-2)
-- [0,-2,-4,-6]
--
{-# INLINE_NORMAL enumerateFromThenIntegral #-}
enumerateFromThenIntegral :: (Monad m, Integral a, Bounded a)
    => a -> a -> Stream m a
#ifdef USE_UNFOLDS_EVERYWHERE
enumerateFromThenIntegral from next =
    unfold Unfold.enumerateFromThenIntegral (from, next)
#else
enumerateFromThenIntegral from next =
    enumerateFromThenToIntegral
        from next (if next >= from then maxBound else minBound)
#endif

-- | Enumerate an 'Integral' type up to a given limit.
-- @enumerateFromToIntegral from to@ generates a finite stream whose first
-- element is @from@ and successive elements are in increments of @1@ up to
-- @to@.
--
-- >>> Stream.toList $ Stream.enumerateFromToIntegral 0 4
-- [0,1,2,3,4]
--
{-# INLINE enumerateFromToIntegral #-}
enumerateFromToIntegral :: (Monad m, Integral a) => a -> a -> Stream m a
enumerateFromToIntegral from to =
    takeWhile (<= to)
        $ takeEndBy (== to) $ enumerateFromStepIntegralUnbounded from 1

-- | Enumerate an 'Integral' type. @enumerateFromIntegral from@ generates a
-- stream whose first element is @from@ and the successive elements are in
-- increments of @1@. The stream is bounded by the size of the 'Integral' type.
--
-- >>> Stream.toList $ Stream.take 4 $ Stream.enumerateFromIntegral (0 :: Int)
-- [0,1,2,3]
--
{-# INLINE enumerateFromIntegral #-}
enumerateFromIntegral :: (Monad m, Integral a, Bounded a) => a -> Stream m a
enumerateFromIntegral from = enumerateFromToIntegral from maxBound

{-# INLINE enumerateFromIntegralUnbounded #-}
enumerateFromIntegralUnbounded :: (Integral a, Monad m) => a -> Stream m a
enumerateFromIntegralUnbounded from = enumerateFromStepIntegralUnbounded from 1

{-# INLINE enumerateFromThenIntegralUnbounded #-}
enumerateFromThenIntegralUnbounded :: (Integral a, Monad m) => a -> a -> Stream m a
enumerateFromThenIntegralUnbounded from next =
    enumerateFromStepIntegralUnbounded from (next - from)

------------------------------------------------------------------------------
-- Enumeration of Fractionals
------------------------------------------------------------------------------

-- We cannot write a general function for Num.  The only way to write code
-- portable between the two is to use a 'Real' constraint and convert between
-- Fractional and Integral using fromRational which is horribly slow.

-- Even though the underlying implementation of enumerateFromFractional and
-- enumerateFromThenFractional works for any 'Num' we have restricted these to
-- 'Fractional' because these do not perform any bounds check, in contrast to
-- integral versions and are therefore not equivalent substitutes for those.

-- | Numerically stable enumeration from a 'Fractional' number in steps of size
-- @1@. @enumerateFromFractional from@ generates a stream whose first element
-- is @from@ and the successive elements are in increments of @1@.  No overflow
-- or underflow checks are performed.
--
-- This is the equivalent to 'enumFrom' for 'Fractional' types. For example:
--
-- >>> Stream.toList $ Stream.take 4 $ Stream.enumerateFromFractional 1.1
-- [1.1,2.1,3.1,4.1]
--
{-# INLINE enumerateFromFractional #-}
enumerateFromFractional :: (Monad m, Fractional a) => a -> Stream m a
enumerateFromFractional = enumerateFromNum

-- | Numerically stable enumeration from a 'Fractional' number in steps.
-- @enumerateFromThenFractional from then@ generates a stream whose first
-- element is @from@, the second element is @then@ and the successive elements
-- are in increments of @then - from@.  No overflow or underflow checks are
-- performed.
--
-- This is the equivalent of 'enumFromThen' for 'Fractional' types. For
-- example:
--
-- >>> Stream.toList $ Stream.take 4 $ Stream.enumerateFromThenFractional 1.1 2.1
-- [1.1,2.1,3.1,4.1]
--
-- >>> Stream.toList $ Stream.take 4 $ Stream.enumerateFromThenFractional 1.1 (-2.1)
-- [1.1,-2.1,-5.300000000000001,-8.500000000000002]
--
{-# INLINE enumerateFromThenFractional #-}
enumerateFromThenFractional
    :: (Monad m, Fractional a)
    => a -> a -> Stream m a
enumerateFromThenFractional = enumerateFromThenNum

-- | Numerically stable enumeration from a 'Fractional' number to a given
-- limit.  @enumerateFromToFractional from to@ generates a finite stream whose
-- first element is @from@ and successive elements are in increments of @1@ up
-- to @to@.
--
-- This is the equivalent of 'enumFromTo' for 'Fractional' types. For
-- example:
--
-- >>> Stream.toList $ Stream.enumerateFromToFractional 1.1 4
-- [1.1,2.1,3.1,4.1]
--
-- >>> Stream.toList $ Stream.enumerateFromToFractional 1.1 4.6
-- [1.1,2.1,3.1,4.1,5.1]
--
-- Notice that the last element is equal to the specified @to@ value after
-- rounding to the nearest integer.
--
{-# INLINE_NORMAL enumerateFromToFractional #-}
enumerateFromToFractional
    :: (Monad m, Fractional a, Ord a)
    => a -> a -> Stream m a
enumerateFromToFractional from to =
    takeWhile (<= to + 1 / 2) $ enumerateFromStepNum from 1

-- | Numerically stable enumeration from a 'Fractional' number in steps up to a
-- given limit.  @enumerateFromThenToFractional from then to@ generates a
-- finite stream whose first element is @from@, the second element is @then@
-- and the successive elements are in increments of @then - from@ up to @to@.
--
-- This is the equivalent of 'enumFromThenTo' for 'Fractional' types. For
-- example:
--
-- >>> Stream.toList $ Stream.enumerateFromThenToFractional 0.1 2 6
-- [0.1,2.0,3.9,5.799999999999999]
--
-- >>> Stream.toList $ Stream.enumerateFromThenToFractional 0.1 (-2) (-6)
-- [0.1,-2.0,-4.1000000000000005,-6.200000000000001]
--
{-# INLINE_NORMAL enumerateFromThenToFractional #-}
enumerateFromThenToFractional
    :: (Monad m, Fractional a, Ord a)
    => a -> a -> a -> Stream m a
enumerateFromThenToFractional from next to =
    takeWhile predicate $ enumerateFromThenFractional from next
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

-------------------------------------------------------------------------------
-- Bounded Enumeration of Enum types not larger than Int
-------------------------------------------------------------------------------

-- | 'enumerateFromThen' for 'Enum' types not larger than 'Int'.
--
-- Note: We convert the 'Enum' to 'Int' and enumerate the 'Int'. If a
-- type is bounded but does not have a 'Bounded' instance then we can go on
-- enumerating it beyond the legal values of the type, resulting in the failure
-- of 'toEnum' when converting back to 'Enum'. Therefore we require a 'Bounded'
-- instance for this function to be safely used.
--
{-# INLINE enumerateFromThenSmall #-}
enumerateFromThenSmall :: (Monad m, Enum a, Bounded a)
    => a -> a -> Stream m a
enumerateFromThenSmall from next =
    if fromEnum next >= fromEnum from
    then enumerateFromThenToSmall from next maxBound
    else enumerateFromThenToSmall from next minBound

{-# DEPRECATED enumerateFromThenSmallBounded "Please use enumerateFromThenSmall instead." #-}
{-# INLINE enumerateFromThenSmallBounded #-}
enumerateFromThenSmallBounded :: (Monad m, Enum a, Bounded a)
    => a -> a -> Stream m a
enumerateFromThenSmallBounded = enumerateFromThenSmall

-- | 'enumerateFrom' for 'Enum' types not larger than 'Int'.
--
{-# INLINE enumerateFromSmall #-}
enumerateFromSmall :: (Monad m, Enum a, Bounded a) => a -> Stream m a
enumerateFromSmall from = enumerateFromToSmall from maxBound

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
    -- >>> Stream.toList $ Stream.take 4 $ Stream.enumerateFrom (0 :: Int)
    -- [0,1,2,3]
    --
    -- For 'Fractional' types, enumeration is numerically stable. However, no
    -- overflow or underflow checks are performed.
    --
    -- >>> Stream.toList $ Stream.take 4 $ Stream.enumerateFrom 1.1
    -- [1.1,2.1,3.1,4.1]
    --
    enumerateFrom :: Monad m => a -> Stream m a

    -- | Generate a finite stream starting with the element @from@, enumerating
    -- the type up to the value @to@. If @to@ is smaller than @from@ then an
    -- empty stream is returned.
    --
    -- >>> Stream.toList $ Stream.enumerateFromTo 0 4
    -- [0,1,2,3,4]
    --
    -- For 'Fractional' types, the last element is equal to the specified @to@
    -- value after rounding to the nearest integral value.
    --
    -- >>> Stream.toList $ Stream.enumerateFromTo 1.1 4
    -- [1.1,2.1,3.1,4.1]
    --
    -- >>> Stream.toList $ Stream.enumerateFromTo 1.1 4.6
    -- [1.1,2.1,3.1,4.1,5.1]
    --
    enumerateFromTo :: (Monad m) => a -> a -> Stream m a

    -- | @enumerateFromThen from then@ generates a stream whose first element
    -- is @from@, the second element is @then@ and the successive elements are
    -- in increments of @then - from@.  Enumeration can occur downwards or
    -- upwards depending on whether @then@ comes before or after @from@. For
    -- 'Bounded' types the stream ends when 'maxBound' is reached, for
    -- unbounded types it keeps enumerating infinitely.
    --
    -- >>> Stream.toList $ Stream.take 4 $ Stream.enumerateFromThen 0 2
    -- [0,2,4,6]
    --
    -- >>> Stream.toList $ Stream.take 4 $ Stream.enumerateFromThen 0 (-2)
    -- [0,-2,-4,-6]
    --
    enumerateFromThen :: (Monad m) => a -> a -> Stream m a

    -- | @enumerateFromThenTo from then to@ generates a finite stream whose
    -- first element is @from@, the second element is @then@ and the successive
    -- elements are in increments of @then - from@ up to @to@. Enumeration can
    -- occur downwards or upwards depending on whether @then@ comes before or
    -- after @from@.
    --
    -- >>> Stream.toList $ Stream.enumerateFromThenTo 0 2 6
    -- [0,2,4,6]
    --
    -- >>> Stream.toList $ Stream.enumerateFromThenTo 0 (-2) (-6)
    -- [0,-2,-4,-6]
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
-- >>> enumerateTo = Stream.enumerateFromTo minBound
--
-- Enumerate a 'Bounded' type from its 'minBound' to specified value.
--
{-# INLINE enumerateTo #-}
enumerateTo :: (Monad m, Bounded a, Enumerable a) => a -> Stream m a
enumerateTo = enumerateFromTo minBound

-- | Same as 'enumerateFrom'. For a 'Bounded' type, 'enumerateFrom' is
-- already guaranteed to enumerate up to 'maxBound', so this function is
-- redundant.
--
{-# DEPRECATED enumerateFromBounded "Please use enumerateFrom instead." #-}
{-# INLINE enumerateFromBounded #-}
enumerateFromBounded :: (Monad m, Enumerable a) => a -> Stream m a
enumerateFromBounded = enumerateFrom

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
    enumerateFrom = enumerateFromIntegral;                  \
    {-# INLINE enumerateFromThen #-};                       \
    enumerateFromThen = enumerateFromThenIntegral;          \
    {-# INLINE enumerateFromTo #-};                         \
    enumerateFromTo = enumerateFromToIntegral;              \
    {-# INLINE enumerateFromThenTo #-};                     \
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
    enumerateFrom = enumerateFromIntegralUnbounded;               \
    {-# INLINE enumerateFromThen #-};                             \
    enumerateFromThen = enumerateFromThenIntegralUnbounded;       \
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
