-- |
-- Module      : Streamly.Internal.Data.Unfold.Enumeration
-- Copyright   : (c) 2019, 2021 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- NOTE: keep this module in sync with the
-- Streamly.Internal.Data.Stream.Enumeration.hs module.
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
-- XXX Will it be better to design the API around "stride" e.g.
-- enumerateFromStrideTo? If "stride" is specified as an argument then there is
-- no issue of overflow of stride itself.
--
module Streamly.Internal.Data.Unfold.Enumeration
    (
    -- ** Enumerable Type Class
      Enumerable (..)

    -- ** 'Num' Type class Types
    -- | Most general operations via the 'Num' type class. All other
    -- enumeraitons can be expressed in terms of these.
    --
    -- These are numerically unstable for floating precision numbers. Use the
    -- RealFloat specific operations for numerical stability.
    --
    -- The "To" vesions are overflow protected, others can overflow and wrap
    -- around for bounded types. Use the "To" versions with max or min bound to
    -- stop at the bound or use the bounded versions.
    , enumerateFromStepNum
    , enumerateFromNum
    , enumerateDownFromNum
    , enumerateFromThenNum
    , enumerateFromToNum
    , enumerateDownFromToNum
    , enumerateFromThenToNum
    , enumerateUpFromThenToNum
    , enumerateDownFromThenToNum

    -- ** Bounded Num Types
    , enumerateFromBoundedNum
    , enumerateFromThenBoundedNum
    , enumerateDownFromBoundedNum

    -- ** 'Enum' Types not larger than 'Int'
    -- | These are implemented by converting Enum to Int and using integral
    -- operations. Note small Enum types are always bounded though they may not
    -- have a Bounded instance.
    , enumerateFromSmall
    , enumerateFromToSmall
    , enumerateFromThenSmall
    , enumerateFromThenToSmall

    -- ** 'RealFloat' Types
    -- | For floating point numbers if the increment is less than the
    -- precision then it just gets lost. Therefore we cannot always increment
    -- it correctly by just repeated addition. Instead we accumulate the
    -- increment counter and compute the increment every time before adding
    -- it to the starting number. This is numerically stable but slower than
    -- the 'Num' based operations.
    , enumerateFromRealFloat
    , enumerateFromToRealFloat
    , enumerateFromThenRealFloat
    , enumerateFromThenToRealFloat

    -- * Deprecated
    , enumerateFromStepIntegral
    , enumerateFromIntegral
    , enumerateFromThenIntegral
    , enumerateFromToIntegral
    , enumerateFromThenToIntegral
    , enumerateFromIntegralBounded
    , enumerateFromThenIntegralBounded
    , enumerateFromToIntegralBounded
    , enumerateFromThenToIntegralBounded
    , enumerateFromSmallBounded
    , enumerateFromThenSmallBounded
    , enumerateFromFractional
    , enumerateFromThenFractional
    , enumerateFromToFractional
    , enumerateFromThenToFractional
    )
where

#include "inline.hs"
#include "deprecation.h"

import Data.Fixed
import Data.Bifunctor (bimap)
import Data.Int
import Data.Ord (Down(..))
import Data.Ratio
import Data.Word
import Numeric.Natural
import Data.Functor.Identity (Identity(..))
import Streamly.Internal.Data.Unfold.Type hiding (takeWhileMWithInput)
import qualified Streamly.Internal.Data.Producer as Producer
import Prelude hiding (map, takeWhile, zipWith)

-- $setup
-- >>> :m
-- >>> import qualified Streamly.Data.Fold as Fold
-- >>> import qualified Streamly.Data.Stream as Stream
-- >>> import qualified Streamly.Internal.Data.Unfold as Unfold
-- >>> import Streamly.Internal.Data.Unfold.Type
-- >>> import Data.Word

------------------------------------------------------------------------------
-- Enumeration of Num
------------------------------------------------------------------------------

-- | Unfolds @(from, stride)@ generating an infinite stream starting from
-- @from@ and incrementing every time by @stride@.  For 'Bounded' types, after
-- the value overflows it keeps enumerating in a cycle:
--
-- @
-- >>> Stream.toList $ Stream.take 10 $ Stream.unfold Unfold.enumerateFromStepNum (255::Word8,1)
-- [255,0,1,2,3,4,5,6,7,8]
--
-- @
--
-- CAUTION: This is NOT NUMERICALLY STABLE for floating point numbers. Use
-- 'enumerateFromStepRealFloat' for numerical stability.
--
-- CAUTION: This will overflow or underflow and wrap around for bounded
-- types.
--
-- /Internal/
--
{-# INLINE enumerateFromStepNum #-}
enumerateFromStepNum :: (Applicative m, Num a) => Unfold m (a, a) a
enumerateFromStepNum = Unfold Producer.enumerateFromStep inject

    where

    inject (!from, !stride) = pure (from, stride)

-- | Same as 'enumerateFromStepNum (from, next)' using a stride of @next - from@:
--
-- @
-- >>> enumerateFromThenNum = lmap (\(from, next) -> (from, next - from)) Unfold.enumerateFromStepNum
--
-- @
--
-- Example:
-- @
-- >>> Stream.toList $ Stream.take 10 $ Stream.unfold enumerateFromThenNum (255::Word8,0)
-- [255,0,1,2,3,4,5,6,7,8]
--
-- @
--
-- CAUTION: This is NOT NUMERICALLY STABLE for floating point numbers.
--
-- CAUTION: This will overflow or underflow and wrap around for bounded
-- types.
--
-- /Internal/
--
{-# INLINE enumerateFromThenNum #-}
enumerateFromThenNum :: (Applicative m, Num a) => Unfold m (a, a) a
enumerateFromThenNum =
    lmap (\(from, next) -> (from, next - from)) enumerateFromStepNum

-- | Same as 'enumerateFromStepNum' using a stride of 1:
--
-- @
-- >>> enumerateFromNum = lmap (\from -> (from, 1)) Unfold.enumerateFromStepNum
-- >>> Stream.toList $ Stream.take 6 $ Stream.unfold enumerateFromNum (0.9)
-- [0.9,1.9,2.9,3.9,4.9,5.9]
--
-- @
--
-- /Internal/
--
{-# INLINE enumerateFromNum #-}
enumerateFromNum :: (Applicative m, Num a) => Unfold m a a
enumerateFromNum = lmap (\from -> (from, 1)) enumerateFromStepNum

{-# INLINE enumerateDownFromNum #-}
enumerateDownFromNum :: (Applicative m, Num a) => Unfold m a a
enumerateDownFromNum = lmap (\from -> (from, -1)) enumerateFromStepNum

-- | Unfolds @(from, next, to)@ generating a finite stream whose first
-- element is @from@, the second element is @next@ and the successive
-- elements are in increments of @next - from@ up to @to@.
--
-- This is overflow safe.
--
{-# INLINE enumerateFromThenToNum #-}
enumerateFromThenToNum :: (Applicative m, Num a, Ord a) => Unfold m (a, a, a) a
enumerateFromThenToNum = Unfold Producer.enumerateFromThenTo inject

    where

    inject (from, next, to) = pure (Producer.EnumInit from next to)

-- | Like 'enumerateFromThenToNum' but a simplified version that only works
-- in the upward direction. It returns an empty stream if @then < from@.
--
{-# INLINE enumerateUpFromThenToNum #-}
enumerateUpFromThenToNum :: (Applicative m, Num a, Ord a) => Unfold m (a, a, a) a
enumerateUpFromThenToNum = Unfold Producer.enumerateUpFromThenTo inject

    where

    inject (from, next, to) = pure (Producer.EnumUpInit from next to)

-- | Like 'enumerateUpFromThenToNum' but a simplified version that only
-- works in the downward direction. It returns an empty stream if
-- @then > from@.
--
{-# INLINE enumerateDownFromThenToNum #-}
enumerateDownFromThenToNum ::
    (Applicative m, Num a, Ord a) => Unfold m (a, a, a) a
enumerateDownFromThenToNum = Unfold Producer.enumerateDownFromThenTo inject

    where

    inject (from, next, to) =
        pure (Producer.EnumUpInit (Down from) (Down next) (Down to))

-- | Unfolds @(from, to)@ generating a finite stream whose first element is
-- @from@ and successive elements are in increments of @1@ up to @to@.
--
{-# INLINE enumerateFromToNum #-}
enumerateFromToNum :: (Monad m, Num a, Ord a) => Unfold m (a, a) a
enumerateFromToNum = Unfold Producer.enumerateFromTo inject

    where

    inject (from, to) = pure (Producer.EnumToInit from to)

-- NOTE: we can use the Down functor to implement this.
{-# INLINE enumerateDownFromToNum #-}
enumerateDownFromToNum :: (Monad m, Num a, Ord a) => Unfold m (a, a) a
enumerateDownFromToNum = Unfold Producer.enumerateDownFromTo inject

    where

    inject (from, to) = pure (Producer.EnumToInit from to)

------------------------------------------------------------------------------
-- Enumeration of Bounded Num
------------------------------------------------------------------------------

-- | Unfolds @(from, then)@ generating a stream whose first element is
-- @from@, the second element is @then@ and the successive elements are in
-- increments of @then - from@. The stream is bounded by the size of the
-- 'Integral' type.
--
-- /Internal/
--
{-# INLINE enumerateFromThenBoundedNum #-}
enumerateFromThenBoundedNum ::
    (Applicative m, Num a, Ord a, Bounded a) => Unfold m (a, a) a
enumerateFromThenBoundedNum = lmap adapt enumerateFromThenToNum

    where

    adapt (from, next) =
        (from, next, if next >= from then maxBound else minBound)

-- | Unfolds @from@ generating a stream whose first element is @from@ and
-- the successive elements are in increments of @1@. The stream is bounded
-- by the size of the type.
--
-- /Internal/
--
{-# INLINE enumerateFromBoundedNum #-}
enumerateFromBoundedNum :: (Monad m, Num a, Ord a, Bounded a) => Unfold m a a
enumerateFromBoundedNum = supplySecond maxBound enumerateFromToNum

{-# INLINE enumerateDownFromBoundedNum #-}
enumerateDownFromBoundedNum ::
    (Monad m, Num a, Ord a, Bounded a) => Unfold m a a
enumerateDownFromBoundedNum = supplySecond minBound enumerateDownFromToNum

------------------------------------------------------------------------------
-- Enumeration of Integrals (Bounded)
------------------------------------------------------------------------------

{-# INLINE takeWhileMWithInput #-}
takeWhileMWithInput :: Monad m =>
    (a -> b -> m Bool) -> Unfold m a b -> Unfold m a b
takeWhileMWithInput f = map snd . takeWhileM (uncurry f) . carryInput

{-# DEPRECATED enumerateFromStepIntegral "Please use enumerateFromStepNum instead." #-}
{-# INLINE enumerateFromStepIntegral #-}
enumerateFromStepIntegral :: (Monad m, Integral a) => Unfold m (a, a) a
enumerateFromStepIntegral = enumerateFromStepNum

{-# DEPRECATED enumerateFromThenToIntegral "Please use enumerateFromThenToNum instead." #-}
{-# INLINE enumerateFromThenToIntegral #-}
enumerateFromThenToIntegral, enumerateFromThenToIntegralBounded ::
    (Monad m, Integral a) => Unfold m (a, a, a) a
enumerateFromThenToIntegral = enumerateFromThenToNum

RENAME(enumerateFromThenToIntegralBounded,enumerateFromThenToIntegral)

{-# DEPRECATED enumerateFromThenIntegral "Please use enumerateFromThenNum instead." #-}
{-# INLINE enumerateFromThenIntegral #-}
enumerateFromThenIntegral :: (Monad m, Integral a) => Unfold m (a, a) a
enumerateFromThenIntegral = enumerateFromThenNum

{-# DEPRECATED enumerateFromThenIntegralBounded "Please use enumerateFromThenBoundedNum instead." #-}
{-# INLINE enumerateFromThenIntegralBounded #-}
enumerateFromThenIntegralBounded ::
    (Monad m, Integral a, Bounded a) => Unfold m (a, a) a
enumerateFromThenIntegralBounded = enumerateFromThenBoundedNum

{-# DEPRECATED enumerateFromToIntegral "Please use enumerateFromToNum instead." #-}
{-# INLINE enumerateFromToIntegral #-}
enumerateFromToIntegral, enumerateFromToIntegralBounded ::
    (Monad m, Integral a) => Unfold m (a, a) a
enumerateFromToIntegral = enumerateFromToNum

RENAME(enumerateFromToIntegralBounded,enumerateFromToIntegral)

{-# DEPRECATED enumerateFromIntegral "Please use enumerateFromNum instead." #-}
{-# INLINE enumerateFromIntegral #-}
enumerateFromIntegral :: (Monad m, Integral a) => Unfold m a a
enumerateFromIntegral = enumerateFromNum

{-# DEPRECATED enumerateFromIntegralBounded "Please use enumerateFromBoundedNum instead." #-}
{-# INLINE enumerateFromIntegralBounded #-}
enumerateFromIntegralBounded :: (Monad m, Integral a, Bounded a) => Unfold m a a
enumerateFromIntegralBounded = enumerateFromBoundedNum

------------------------------------------------------------------------------
-- Enumeration of RealFloat
------------------------------------------------------------------------------

-- | For floating point numbers if the increment is less than the precision
-- then it just gets lost. Therefore we cannot always increment it correctly
-- by just repeated addition.
--
-- Instead we accumulate the increment counter and compute the increment
-- every time before adding it to the starting number. This is numerically
-- stable but slower than 'enumerateFromStepNum'.
--
-- /Internal/
--
{-# INLINE enumerateFromStepRealFloat #-}
enumerateFromStepRealFloat :: (Applicative m, RealFloat a) => Unfold m (a, a) a
enumerateFromStepRealFloat = Unfold Producer.enumerateFromStepRealFloat inject

    where

    inject (!from, !stride) = pure (from, stride, 0)

{-# INLINE enumerateFromRealFloat #-}
enumerateFromRealFloat :: (Applicative m, RealFloat a) => Unfold m a a
enumerateFromRealFloat = lmap (\from -> (from, 1)) enumerateFromStepRealFloat

{-# INLINE enumerateFromThenRealFloat #-}
enumerateFromThenRealFloat :: (Applicative m, RealFloat a) => Unfold m (a, a) a
enumerateFromThenRealFloat =
    lmap (\(from, next) -> (from, next - from)) enumerateFromStepRealFloat

-- | Same as 'enumerateFromStepRealFloat' with a step of 1 and enumerating up
-- to the specified upper limit rounded to the nearest integral value:
--
-- @
-- >>> Stream.toList $ Stream.unfold Unfold.enumerateFromToRealFloat (0.1, 6.3)
-- [0.1,1.1,2.1,3.1,4.1,5.1,6.1]
--
-- @
--
-- /Internal/
--
{-# INLINE enumerateFromToRealFloat #-}
enumerateFromToRealFloat :: (Monad m, RealFloat a) => Unfold m (a, a) a
enumerateFromToRealFloat =
    takeWhileMWithInput (\(_, to) b -> return $ b <= to + 1 / 2)
        $ lmap (\(from, _) -> (from, 1)) enumerateFromStepRealFloat

{-# INLINE enumerateFromThenToRealFloat #-}
enumerateFromThenToRealFloat :: (Monad m, RealFloat a) => Unfold m (a, a, a) a
enumerateFromThenToRealFloat =
    takeWhileMWithInput cond $ lmap toFromStep enumerateFromStepRealFloat

    where

    toFromStep (from, next, _) = (from, next - from)

    cond (from, next, to) b =
        let stride = next - from
         in return
                $ if next >= from
                  then b <= to + stride / 2
                  else b >= to + stride / 2

------------------------------------------------------------------------------
-- Enumeration of Fractionals (Deprecated)
------------------------------------------------------------------------------

{-# INLINE enumerateFromStepFractional #-}
enumerateFromStepFractional :: (Monad m, Fractional a) => Unfold m (a, a) a
enumerateFromStepFractional = Unfold step inject

    where

    inject (!from, !stride) = return (from, stride, 0)

    step (from, stride, i) =
        return $ Yield (from + i * stride) (from, stride, i + 1)

{-# DEPRECATED enumerateFromFractional "Please use enumerateFromRealFloat instead." #-}
{-# INLINE_NORMAL enumerateFromFractional #-}
enumerateFromFractional :: (Monad m, Fractional a) => Unfold m a a
enumerateFromFractional =
    lmap (\from -> (from, 1)) enumerateFromStepFractional

{-# DEPRECATED enumerateFromThenFractional "Please use enumerateFromThenRealFloat instead." #-}
{-# INLINE_NORMAL enumerateFromThenFractional #-}
enumerateFromThenFractional :: (Monad m, Fractional a) => Unfold m (a, a) a
enumerateFromThenFractional =
    lmap (\(from, next) -> (from, next - from)) enumerateFromStepFractional

{-# DEPRECATED enumerateFromToFractional "Please use enumerateFromToRealFloat instead." #-}
{-# INLINE_NORMAL enumerateFromToFractional #-}
enumerateFromToFractional :: (Monad m, Fractional a, Ord a) =>
    Unfold m (a, a) a
enumerateFromToFractional =
    takeWhileMWithInput (\(_, to) b -> return $ b <= to + 1 / 2)
        $ lmap (\(from, _) -> (from, 1)) enumerateFromStepFractional

{-# DEPRECATED enumerateFromThenToFractional "Please use enumerateFromThenToRealFloat instead." #-}
{-# INLINE enumerateFromThenToFractional #-}
enumerateFromThenToFractional :: (Monad m, Fractional a, Ord a) =>
    Unfold m (a, a, a) a
enumerateFromThenToFractional =
    takeWhileMWithInput cond $ lmap toFromStep enumerateFromStepFractional

    where

    toFromStep (from, next, _) = (from, next - from)

    cond (from, next, to) b =
        let stride = next - from
         in return
                $ if next >= from
                  then b <= to + stride / 2
                  else b >= to + stride / 2

-------------------------------------------------------------------------------
-- Enumeration of Enum types not larger than Int
-------------------------------------------------------------------------------

-- | Enumerate from given starting Enum value 'from' and to Enum value 'to'
-- with stride of 1 till to value.
--
-- /Internal/
--
{-# INLINE enumerateFromToSmall #-}
enumerateFromToSmall :: (Monad m, Enum a) => Unfold m (a, a) a
enumerateFromToSmall =
    fmap toEnum (lmap (bimap fromEnum fromEnum) enumerateFromToNum)

-- | Enumerate from given starting Enum value 'from' and then Enum value 'next'
-- and to Enum value 'to' with stride of (fromEnum next - fromEnum from)
-- till to value.
--
-- /Internal/
--
{-# INLINE enumerateFromThenToSmall #-}
enumerateFromThenToSmall :: (Applicative m, Enum a) => Unfold m (a, a, a) a
enumerateFromThenToSmall =
    let toInts (x, y, z) = (fromEnum x, fromEnum y, fromEnum z)
     in fmap toEnum (lmap toInts enumerateFromThenToNum)

-------------------------------------------------------------------------------
-- Bounded Enumeration of Enum types not larger than Int
-------------------------------------------------------------------------------

-- | Enumerate from given starting Enum value 'from' with stride of 1 till
-- maxBound
--
-- /Internal/
--
{-# INLINE enumerateFromSmall #-}
enumerateFromSmall, enumerateFromSmallBounded ::
    (Monad m, Enum a, Bounded a) => Unfold m a a
enumerateFromSmall = supplySecond maxBound enumerateFromToSmall

RENAME(enumerateFromSmallBounded,enumerateFromSmall)

-- | Enumerate from given starting Enum value 'from' and next Enum value 'next'
-- with stride of (fromEnum next - fromEnum from) till maxBound.
--
-- /Internal/
--
{-# INLINE enumerateFromThenSmall #-}
enumerateFromThenSmall, enumerateFromThenSmallBounded
    :: forall m a. (Applicative m, Enum a, Bounded a) =>
    Unfold m (a, a) a
enumerateFromThenSmall =
    let adapt (from, next) =
            let frm = fromEnum from
                nxt = fromEnum next
                stride = nxt - frm
                to = if stride >= 0
                     then fromEnum (maxBound :: a)
                     else fromEnum (minBound :: a)
             in (frm, nxt, to)
     in fmap toEnum (lmap adapt enumerateFromThenToNum)

RENAME(enumerateFromThenSmallBounded,enumerateFromThenSmall)

-------------------------------------------------------------------------------
-- Enumerable type class
-------------------------------------------------------------------------------

-- | Types that can be enumerated as a stream. The operations in this type
-- class are equivalent to those in the 'Enum' type class, except that these
-- generate a stream instead of a list. Use the functions in
-- "Streamly.Internal.Data.Unfold.Enumeration" module to define new instances.
--
class Enumerable a where

    -- | Unfolds @from@ generating a stream starting with the element
    -- @from@, enumerating up to 'maxBound' when the type is 'Bounded' or
    -- generating an infinite stream when the type is not 'Bounded'.
    --
    -- >>> Stream.toList $ Stream.take 4 $ Stream.unfold Unfold.enumerateFrom (0 :: Int)
    -- [0,1,2,3]
    --
    -- For 'Fractional' types, enumeration is numerically stable. However, no
    -- overflow or underflow checks are performed.
    --
    -- >>> Stream.toList $ Stream.take 4 $ Stream.unfold Unfold.enumerateFrom 1.1
    -- [1.1,2.1,3.1,4.1]
    --
    enumerateFrom :: Monad m => Unfold m a a

    -- | Unfolds @(from, to)@ generating a finite stream starting with the element
    -- @from@, enumerating the type up to the value @to@. If @to@ is smaller than
    -- @from@ then an empty stream is returned.
    --
    -- >>> Stream.toList $ Stream.unfold Unfold.enumerateFromTo (0, 4)
    -- [0,1,2,3,4]
    --
    -- For 'Fractional' types, the last element is equal to the specified @to@
    -- value after rounding to the nearest integral value.
    --
    -- >>> Stream.toList $ Stream.unfold Unfold.enumerateFromTo (1.1, 4)
    -- [1.1,2.1,3.1,4.1]
    --
    -- >>> Stream.toList $ Stream.unfold Unfold.enumerateFromTo (1.1, 4.6)
    -- [1.1,2.1,3.1,4.1,5.1]
    --
    enumerateFromTo :: Monad m => Unfold m (a, a) a

    -- | Unfolds @(from, then)@ generating a stream whose first element is
    -- @from@ and the successive elements are in increments of @then@.  Enumeration
    -- can occur downwards or upwards depending on whether @then@ comes before or
    -- after @from@. For 'Bounded' types the stream ends when 'maxBound' is
    -- reached, for unbounded types it keeps enumerating infinitely.
    --
    -- >>> Stream.toList $ Stream.take 4 $ Stream.unfold Unfold.enumerateFromThen (0, 2)
    -- [0,2,4,6]
    --
    -- >>> Stream.toList $ Stream.take 4 $ Stream.unfold Unfold.enumerateFromThen (0,(-2))
    -- [0,-2,-4,-6]
    --
    enumerateFromThen :: Monad m => Unfold m (a, a) a

    -- | Unfolds @(from, then, to)@ generating a finite stream whose first element
    -- is @from@ and the successive elements are in increments of @then@ up to
    -- @to@. Enumeration can occur downwards or upwards depending on whether @then@
    -- comes before or after @from@.
    --
    -- >>> Stream.toList $ Stream.unfold Unfold.enumerateFromThenTo (0, 2, 6)
    -- [0,2,4,6]
    --
    -- >>> Stream.toList $ Stream.unfold Unfold.enumerateFromThenTo (0, (-2), (-6))
    -- [0,-2,-4,-6]
    --
    enumerateFromThenTo :: Monad m => Unfold m (a, a, a) a

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

-- For bounded Integral Enum types, may be larger than Int. 'enumerateFrom'
-- and 'enumerateFromThen' use the bounded Num functions so that they stop at
-- 'maxBound'/'minBound' instead of overflowing and wrapping around.
#define ENUMERABLE_BOUNDED_NUM(TYPE_NAME)                   \
instance Enumerable TYPE_NAME where {                       \
    {-# INLINE enumerateFrom #-};                           \
    enumerateFrom = enumerateFromBoundedNum;                \
    {-# INLINE enumerateFromThen #-};                       \
    enumerateFromThen = enumerateFromThenBoundedNum;        \
    {-# INLINE enumerateFromTo #-};                         \
    enumerateFromTo = enumerateFromToNum;                   \
    {-# INLINE enumerateFromThenTo #-};                     \
    enumerateFromThenTo = enumerateFromThenToNum }

ENUMERABLE_BOUNDED_NUM(Int)
ENUMERABLE_BOUNDED_NUM(Int8)
ENUMERABLE_BOUNDED_NUM(Int16)
ENUMERABLE_BOUNDED_NUM(Int32)
ENUMERABLE_BOUNDED_NUM(Int64)
ENUMERABLE_BOUNDED_NUM(Word)
ENUMERABLE_BOUNDED_NUM(Word8)
ENUMERABLE_BOUNDED_NUM(Word16)
ENUMERABLE_BOUNDED_NUM(Word32)
ENUMERABLE_BOUNDED_NUM(Word64)

-- For unbounded 'Num' types that are not 'RealFloat' (no numerical
-- stability concerns since these are either exact integrals or exact
-- rational/fixed-precision types).
#define ENUMERABLE_UNBOUNDED_NUM(TYPE_NAME,CONSTRAINT)      \
instance (CONSTRAINT) => Enumerable TYPE_NAME where {       \
    {-# INLINE enumerateFrom #-};                           \
    enumerateFrom = enumerateFromNum;                       \
    {-# INLINE enumerateFromThen #-};                       \
    enumerateFromThen = enumerateFromThenNum;               \
    {-# INLINE enumerateFromTo #-};                         \
    enumerateFromTo = enumerateFromToNum;                   \
    {-# INLINE enumerateFromThenTo #-};                     \
    enumerateFromThenTo = enumerateFromThenToNum }

ENUMERABLE_UNBOUNDED_NUM(Integer,)
ENUMERABLE_UNBOUNDED_NUM(Natural,)
ENUMERABLE_UNBOUNDED_NUM((Fixed a),HasResolution a)
ENUMERABLE_UNBOUNDED_NUM((Ratio a),Integral a)

#define ENUMERABLE_REAL_FLOAT(REALFLOAT_TYPE)                     \
instance Enumerable REALFLOAT_TYPE where {                        \
    {-# INLINE enumerateFrom #-};                                 \
    enumerateFrom = enumerateFromRealFloat;                       \
    {-# INLINE enumerateFromThen #-};                             \
    enumerateFromThen = enumerateFromThenRealFloat;                \
    {-# INLINE enumerateFromTo #-};                                \
    enumerateFromTo = enumerateFromToRealFloat;                    \
    {-# INLINE enumerateFromThenTo #-};                            \
    enumerateFromThenTo = enumerateFromThenToRealFloat }

ENUMERABLE_REAL_FLOAT(Float)
ENUMERABLE_REAL_FLOAT(Double)

instance Enumerable a => Enumerable (Identity a) where
    {-# INLINE enumerateFrom #-}
    enumerateFrom =
        map Identity $ lmap runIdentity enumerateFrom
    {-# INLINE enumerateFromThen #-}
    enumerateFromThen =
        map Identity $ lmap (bimap runIdentity runIdentity) enumerateFromThen
    {-# INLINE enumerateFromTo #-}
    enumerateFromTo  =
        map Identity $ lmap (bimap runIdentity runIdentity) enumerateFromTo
    {-# INLINE enumerateFromThenTo #-}
    enumerateFromThenTo  =
        map Identity $
            lmap
            (\(from, next, to) ->
                 (runIdentity from, runIdentity next, runIdentity to))
            enumerateFromThenTo
