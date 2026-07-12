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
    , EnumToState (..)
    , enumerateFromToNum
    , enumerateDownFromToNum
    , EnumState (..)
    , enumerateFromThenToNum
    , EnumStateUp (..)
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
    -- | For floating point numbers if the increment is less than the precision
    -- then it just gets lost. Therefore we cannot always increment it
    -- correctly by just repeated addition.
    -- 9007199254740992 + 1 + 1 :: Double => 9.007199254740992e15
    -- 9007199254740992 + 2     :: Double => 9.007199254740994e15
    --
    -- Instead we accumulate the increment counter and compute the increment
    -- every time before adding it to the starting number.
    --
    , enumerateFromRealFloat
    , enumerateFromToRealFloat
    , enumerateFromThenRealFloat
    , enumerateFromThenToRealFloat

    -- ** Convenient functions using 'Enumerable' type class
    , enumerate
    , enumerateTo

    -- * Deprecated
    , enumerateFromBounded
    , enumerateFromThenSmallBounded
    , enumerateFromIntegral
    , enumerateFromThenIntegral
    , enumerateFromToIntegral
    , enumerateFromThenToIntegral
    , enumerateFromStepIntegral
    , enumerateFromFractional
    , enumerateFromToFractional
    , enumerateFromThenFractional
    , enumerateFromThenToFractional
    )
where

#include "inline.hs"

import Data.Fixed
import Data.Functor.Identity (Identity(..))
import Data.Int
import Data.Ord (Down(..))
import Data.Ratio
import Data.Word
import Fusion.Plugin.Types (Fuse(..))
import Numeric.Natural
import Streamly.Internal.Data.Stream.Type

-- import qualified Streamly.Internal.Data.Producer as Producer
import Prelude hiding (takeWhile)

#include "DocTestDataStream.hs"

------------------------------------------------------------------------------
-- Enumeration of Num
------------------------------------------------------------------------------

-- | @enumerateFromStepNum from step@ generates an infinite stream whose first
-- element is @from@ and the successive elements are in increments of @step@.
--
-- >>> Stream.toList $ Stream.take 4 $ Stream.enumerateFromStepNum 0 2
-- [0,2,4,6]
--
-- >>> Stream.toList $ Stream.take 3 $ Stream.enumerateFromStepNum 0 (-2)
-- [0,-2,-4]
--
-- CAUTION: This is NOT NUMERICALLY STABLE for floating point numbers.
--
-- CAUTION: This will overflow or underflow and wrap around for bounded types.
{-# INLINE_NORMAL enumerateFromStepNum #-}
enumerateFromStepNum :: (Applicative m, Num a) => a -> a -> Stream m a
-- NOTE: Moving this to Producer causes regressions in many Stream benchmarks
-- I guess the problem was tuple not being reduced which is resolved now
{-
enumerateFromStepNum !from !stride =
    Stream (const Producer.enumerateFromStep) (from, stride)
-}
enumerateFromStepNum !from !stride = Stream step from

    where

    step _ x = pure $ Yield x $! (x + stride)

-- | @enumerateFromThenNum from then@ generates a stream whose first element is
-- @from@, the second element is @then@ and the successive elements are in
-- increments of @then - from@.
--
-- >>> Stream.toList $ Stream.take 4 $ Stream.enumerateFromThenNum (254 :: Word8) 255
-- [254,255,0,1]
--
-- >>> import Data.Int (Int8)
-- >>> Stream.toList $ Stream.take 4 $ Stream.enumerateFromThenNum (-126 :: Int8) (-127)
-- [-126,-127,-128,127]
--
-- CAUTION: This is NOT NUMERICALLY STABLE for floating point numbers.
--
-- CAUTION: This will overflow or underflow and wrap around for bounded types.
{-# INLINE_NORMAL enumerateFromThenNum #-}
enumerateFromThenNum :: (Applicative m, Num a) => a -> a -> Stream m a
enumerateFromThenNum from next = enumerateFromStepNum from $! (next - from)

-- | Same as:
--
-- >> enumerateFromThenNum from (from + 1)
--
{-# INLINE_NORMAL enumerateFromNum #-}
enumerateFromNum :: (Applicative m, Num a) => a -> Stream m a
enumerateFromNum from = enumerateFromStepNum from 1

{-# INLINE_NORMAL enumerateDownFromNum #-}
enumerateDownFromNum :: (Applicative m, Num a) => a -> Stream m a
enumerateDownFromNum from = enumerateFromStepNum from (-1)

{-# ANN type EnumState Fuse #-}
data EnumState a =
      EnumInit
    | EnumYieldUpward a a a
    | EnumYieldDownward a a a
    | EnumSingle a
    | EnumStop

-- | @enumerateFromThenToNum from then to@ generates a finite stream whose
-- first element is @from@, the second element is @then@ and the successive
-- elements are in increments of @then - from@ up to @to@.
--
-- >>> Stream.toList $ Stream.enumerateFromThenToNum 0 2 6
-- [0,2,4,6]
--
-- >>> Stream.toList $ Stream.enumerateFromThenToNum 0 (-2) (-6)
-- [0,-2,-4,-6]
--
{-# INLINE_NORMAL enumerateFromThenToNum #-}
enumerateFromThenToNum
    :: (Applicative m, Num a, Ord a)
    => a -> a -> a -> Stream m a
{-
-- This blows up build time memory consumption and compilation time of
-- Stream.Type.Logic benchmarks.
enumerateFromThenToNum from next to =
    Stream
        (const Producer.enumerateFromThenTo)
        (Producer.EnumInit from next to)
-}
enumerateFromThenToNum from next to = Stream step EnumInit

    where

    {-# INLINE_LATE step #-}
    step _ EnumInit =
        pure $
            if next >= from
            then
                if to < next
                then
                    if to < from
                    then Stop
                    else Skip (EnumSingle from)
                else -- from <= next <= to
                    let !stride = next - from
                    in Skip $ EnumYieldUpward from stride (to - stride)
            else
                if to > next
                then
                    if to > from
                    then Stop
                    else Skip (EnumSingle from)
                else -- from >= next >= to
                    let !stride = next - from
                    in Skip $ EnumYieldDownward from stride (to - stride)

    step _ (EnumYieldUpward x stride toMinus) =
        pure $
            if x <= toMinus
            then
                let !nxt = x + stride
                 in Yield x $ EnumYieldUpward nxt stride toMinus
            else Skip (EnumSingle x)

    step _ (EnumYieldDownward x stride toMinus) =
        pure $
            if x >= toMinus
            then
                let !nxt = x + stride
                 in Yield x $ EnumYieldDownward nxt stride toMinus
            else Skip (EnumSingle x)

    step _ (EnumSingle x) = pure $ Yield x EnumStop

    step _ EnumStop = pure Stop

{-# ANN type EnumStateUp Fuse #-}
data EnumStateUp a =
      EnumUpInit
    | EnumUpYield a a a
    | EnumUpStop

-- | Like 'enumerateFromThenToNum' but a simplified version that only works in
-- the upward direction. It returns an empty stream if @then < from@.
--
-- >>> Stream.toList $ Stream.enumerateUpFromThenToNum 0 2 6
-- [0,2,4,6]
--
{-# INLINE_NORMAL enumerateUpFromThenToNum #-}
enumerateUpFromThenToNum
    :: (Applicative m, Num a, Ord a) => a -> a -> a -> Stream m a
{-
enumerateUpFromThenToNum from next to =
    Stream
        (const Producer.enumerateUpFromThenTo)
        (Producer.EnumUpInit from next to)
-}
enumerateUpFromThenToNum from next to = Stream step EnumUpInit

    where

    {-# INLINE_LATE step #-}
    step _ EnumUpInit =
        pure $
            if next < from
            then Stop
            else
                if to < next
                then
                    if to < from
                    then Stop
                    else Yield from EnumUpStop
                else -- from <= next <= to
                    let !stride = next - from
                    in Skip $ EnumUpYield from stride (to - stride)

    step _ (EnumUpYield x stride toMinus) =
        pure $
            if x <= toMinus
            then
                let !nxt = x + stride
                 in Yield x $ EnumUpYield nxt stride toMinus
            else Yield x EnumUpStop

    step _ EnumUpStop = pure Stop

-- | Like 'enumerateFromThenToNum' but a simplified version that only works in
-- the downward direction. It returns an empty stream if @then > from@.
--
-- >>> Stream.toList $ Stream.enumerateDownFromThenToNum 6 4 0
-- [6,4,2,0]
--
{-# INLINE_NORMAL enumerateDownFromThenToNum #-}
enumerateDownFromThenToNum
    :: (Monad m, Num a, Ord a) => a -> a -> a -> Stream m a
enumerateDownFromThenToNum from next to =
    fmap getDown $ enumerateUpFromThenToNum (Down from) (Down next) (Down to)

{-# ANN type EnumToState Fuse #-}
data EnumToState a =
      EnumToInit
    | EnumToYield !a
    | EnumToStop

-- | @enumerateFromToNum from to@ generates a finite stream whose first element
-- is @from@ and successive elements are in increments of @1@ up to @to@.
--
-- >>> Stream.toList $ Stream.enumerateFromToNum (254 :: Word8) 255
-- [254,255]
--
-- Equivalent to the following but with better fusion:
--
-- >> enumerateUpFromThenToNum from (from + 1) to
--
{-# INLINE enumerateFromToNum #-}
enumerateFromToNum :: (Monad m, Num a, Ord a) => a -> a -> Stream m a
enumerateFromToNum from to = Stream step EnumToInit

    where

    -- Equivalent to the following but with better fusion:
    -- takeWhile (<= to) $ takeEndBy (== to) $ enumerateFromStepNum from 1

    {-# INLINE_LATE step #-}
    step _ EnumToInit =
        pure $
            if to < from
            then Stop
            else Skip $ EnumToYield from
    step _ (EnumToYield x) =
        pure $
            if to > x
            then Yield x (EnumToYield (x + 1))
            else Yield x EnumToStop
    step _ EnumToStop = pure Stop

{-# INLINE enumerateDownFromToNum #-}
enumerateDownFromToNum :: (Monad m, Num a, Ord a) => a -> a -> Stream m a
enumerateDownFromToNum from to = Stream step EnumToInit

    where

    {-# INLINE_LATE step #-}
    step _ EnumToInit =
        pure $
            if to > from
            then Stop
            else Skip $ EnumToYield from
    step _ (EnumToYield x) =
        pure $
            if to < x
            then Yield x (EnumToYield (x - 1))
            else Yield x EnumToStop
    step _ EnumToStop = pure Stop

------------------------------------------------------------------------------
-- Enumeration of Bounded Num
------------------------------------------------------------------------------

-- | @enumerateFromThenBoundedNum from then@ generates a stream whose first
-- element is @from@, the second element is @then@ and the successive elements
-- are in increments of @then - from@. The stream is bounded by the size of the
-- 'Integral' type.
--
-- >>> Stream.toList $ Stream.enumerateFromThenBoundedNum (254 :: Word8) 255
-- [254,255]
--
-- >>> import Data.Int (Int8)
-- >>> Stream.toList $ Stream.enumerateFromThenBoundedNum (-126 :: Int8) (-127)
-- [-126,-127,-128]
--
{-# INLINE_NORMAL enumerateFromThenBoundedNum #-}
enumerateFromThenBoundedNum :: (Applicative m, Num a, Ord a, Bounded a)
    => a -> a -> Stream m a
enumerateFromThenBoundedNum from next =
    enumerateFromThenToNum
        from next (if next >= from then maxBound else minBound)

-- | @enumerateFromBoundedNum from@ generates a stream whose first element is
-- @from@ and the successive elements are in increments of @1@. The stream is
-- bounded by the size of the type.
--
-- >>> Stream.toList $ Stream.enumerateFromBoundedNum (254 :: Word8)
-- [254,255]
--
{-# INLINE enumerateFromBoundedNum #-}
enumerateFromBoundedNum ::
    (Monad m, Num a, Ord a, Bounded a) => a -> Stream m a
enumerateFromBoundedNum from =
    enumerateFromToNum from maxBound

{-# INLINE enumerateDownFromBoundedNum #-}
enumerateDownFromBoundedNum ::
    (Monad m, Num a, Ord a, Bounded a) => a -> Stream m a
enumerateDownFromBoundedNum from =
    enumerateDownFromToNum from minBound

------------------------------------------------------------------------------
-- Enumeration of Integrals
------------------------------------------------------------------------------

{-# DEPRECATED enumerateFromStepIntegral "Please use enumerateFromStepNum instead." #-}
{-# INLINE enumerateFromStepIntegral #-}
enumerateFromStepIntegral :: (Integral a, Monad m) => a -> a -> Stream m a
enumerateFromStepIntegral = enumerateFromStepNum

{-# DEPRECATED enumerateFromThenToIntegral "Please use enumerateFromThenToNum instead." #-}
{-# INLINE_NORMAL enumerateFromThenToIntegral #-}
enumerateFromThenToIntegral
    :: (Monad m, Integral a)
    => a -> a -> a -> Stream m a
enumerateFromThenToIntegral = enumerateFromThenToNum

{-# DEPRECATED enumerateFromThenIntegral "Please use enumerateFromThenBoundedNum instead." #-}
{-# INLINE_NORMAL enumerateFromThenIntegral #-}
enumerateFromThenIntegral :: (Monad m, Integral a, Bounded a)
    => a -> a -> Stream m a
enumerateFromThenIntegral = enumerateFromThenBoundedNum

{-# DEPRECATED enumerateFromToIntegral "Please use enumerateFromToNum instead." #-}
{-# INLINE enumerateFromToIntegral #-}
enumerateFromToIntegral :: (Monad m, Integral a) => a -> a -> Stream m a
enumerateFromToIntegral = enumerateFromToNum

{-# DEPRECATED enumerateFromIntegral "Please use enumerateFromBoundedNum instead." #-}
{-# INLINE enumerateFromIntegral #-}
enumerateFromIntegral :: (Monad m, Integral a, Bounded a) => a -> Stream m a
enumerateFromIntegral = enumerateFromBoundedNum

------------------------------------------------------------------------------
-- Enumeration of RealFloat
------------------------------------------------------------------------------

-- We cannot write a general function for Num.  The only way to write code
-- portable between the two is to use a 'Real' constraint and convert between
-- Fractional and Integral using fromRational which is horribly slow.

-- Even though the underlying implementation of enumerateFromRealFloat and
-- enumerateFromThenFractional works for any 'Num' we have restricted these to
-- 'Fractional' because these do not perform any bounds check, in contrast to
-- integral versions and are therefore not equivalent substitutes for those.

-- | For floating point numbers if the increment is less than the precision then
-- it just gets lost. Therefore we cannot always increment it correctly by just
-- repeated addition.
-- 9007199254740992 + 1 + 1 :: Double => 9.007199254740992e15
-- 9007199254740992 + 2     :: Double => 9.007199254740994e15
--
-- Instead we accumulate the increment counter and compute the increment
-- every time before adding it to the starting number.
--
{-# INLINE_NORMAL enumerateFromStepRealFloat #-}
enumerateFromStepRealFloat :: (Applicative m, RealFloat a) => a -> a -> Stream m a
{-
enumerateFromStepRealFloat !from !stride =
    Stream (const Producer.enumerateFromStepRealFloat) (from, stride, 0)
-}
enumerateFromStepRealFloat !from !stride = Stream step 0

    where

    step _ i = pure $ (Yield $! (from + i * stride)) $! (i + 1)

-- | Numerically stable enumeration from a 'Fractional' number in steps of size
-- @1@. @enumerateFromRealFloat from@ generates a stream whose first element
-- is @from@ and the successive elements are in increments of @1@.  No overflow
-- or underflow checks are performed.
--
-- This is the equivalent to 'enumFrom' for 'Fractional' types. For example:
--
-- >>> Stream.toList $ Stream.take 4 $ Stream.enumerateFromRealFloat 1.1
-- [1.1,2.1,3.1,4.1]
--
{-# INLINE enumerateFromRealFloat #-}
enumerateFromRealFloat :: (Applicative m, RealFloat a) => a -> Stream m a
enumerateFromRealFloat from = enumerateFromStepRealFloat from 1

-- | Numerically stable enumeration from a 'RealFloat' number in steps.
-- @enumerateFromThenRealFloat from then@ generates a stream whose first
-- element is @from@, the second element is @then@ and the successive elements
-- are in increments of @then - from@.  No overflow or underflow checks are
-- performed.
--
-- This is the equivalent of 'enumFromThen' for 'RealFloat' types. For
-- example:
--
-- >>> Stream.toList $ Stream.take 4 $ Stream.enumerateFromThenRealFloat 1.1 2.1
-- [1.1,2.1,3.1,4.1]
--
-- >>> Stream.toList $ Stream.take 4 $ Stream.enumerateFromThenRealFloat 1.1 (-2.1)
-- [1.1,-2.1,-5.300000000000001,-8.500000000000002]
--
{-# INLINE enumerateFromThenRealFloat #-}
enumerateFromThenRealFloat
    :: (Applicative m, RealFloat a)
    => a -> a -> Stream m a
enumerateFromThenRealFloat from next =
    enumerateFromStepRealFloat from $! (next - from)

-- | Numerically stable enumeration from a 'RealFloat' number to a given
-- limit.  @enumerateFromToRealFloat from to@ generates a finite stream whose
-- first element is @from@ and successive elements are in increments of @1@ up
-- to @to@.
--
-- This is the equivalent of 'enumFromTo' for 'RealFloat' types. For
-- example:
--
-- >>> Stream.toList $ Stream.enumerateFromToRealFloat 1.1 4
-- [1.1,2.1,3.1,4.1]
--
-- >>> Stream.toList $ Stream.enumerateFromToRealFloat 1.1 4.6
-- [1.1,2.1,3.1,4.1,5.1]
--
-- Notice that the last element is equal to the specified @to@ value after
-- rounding to the nearest integer.
--
{-# INLINE_NORMAL enumerateFromToRealFloat #-}
enumerateFromToRealFloat
    :: (Monad m, RealFloat a)
    => a -> a -> Stream m a
enumerateFromToRealFloat from to =
    takeWhile (<= to + 1 / 2) $ enumerateFromStepRealFloat from 1

-- | Numerically stable enumeration from a 'RealFloat' number in steps up to a
-- given limit.  @enumerateFromThenToRealFloat from then to@ generates a
-- finite stream whose first element is @from@, the second element is @then@
-- and the successive elements are in increments of @then - from@ up to @to@.
--
-- This is the equivalent of 'enumFromThenTo' for 'RealFloat' types. For
-- example:
--
-- >>> Stream.toList $ Stream.enumerateFromThenToRealFloat 0.1 2 6
-- [0.1,2.0,3.9,5.799999999999999]
--
-- >>> Stream.toList $ Stream.enumerateFromThenToRealFloat 0.1 (-2) (-6)
-- [0.1,-2.0,-4.1000000000000005,-6.200000000000001]
--
{-# INLINE_NORMAL enumerateFromThenToRealFloat #-}
enumerateFromThenToRealFloat
    :: (Monad m, RealFloat a)
    => a -> a -> a -> Stream m a
enumerateFromThenToRealFloat from next to =
    takeWhile predicate $ enumerateFromThenRealFloat from next

    where

    mid = (next - from) / 2
    predicate | next >= from  = (<= to + mid)
              | otherwise     = (>= to + mid)

------------------------------------------------------------------------------
-- Enumeration of Fractionals (Deprecated)
------------------------------------------------------------------------------

{-# INLINE_NORMAL enumerateFromStepFractional #-}
enumerateFromStepFractional :: (Monad m, Fractional a) => a -> a -> Stream m a
enumerateFromStepFractional !from !stride = Stream step (from, stride, 0)

    where

    {-# INLINE_LATE step #-}
    step _ (from1, stride1, i) =
        pure $ Yield (from1 + i * stride1) (from1, stride1, i + 1)

{-# DEPRECATED enumerateFromFractional "Please use enumerateFromRealFloat instead." #-}
{-# INLINE enumerateFromFractional #-}
enumerateFromFractional :: (Monad m, Fractional a) => a -> Stream m a
enumerateFromFractional from = enumerateFromStepFractional from 1

{-# DEPRECATED enumerateFromThenFractional "Please use enumerateFromThenRealFloat instead." #-}
{-# INLINE enumerateFromThenFractional #-}
enumerateFromThenFractional
    :: (Monad m, Fractional a)
    => a -> a -> Stream m a
enumerateFromThenFractional from next =
    enumerateFromStepFractional from (next - from)

{-# DEPRECATED enumerateFromToFractional "Please use enumerateFromToRealFloat instead." #-}
{-# INLINE_NORMAL enumerateFromToFractional #-}
enumerateFromToFractional
    :: (Monad m, Fractional a, Ord a)
    => a -> a -> Stream m a
enumerateFromToFractional from to =
    takeWhile (<= to + 1 / 2) $ enumerateFromStepFractional from 1

{-# DEPRECATED enumerateFromThenToFractional "Please use enumerateFromThenToRealFloat instead." #-}
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
    $ enumerateFromToNum (fromEnum from) (fromEnum to)

-- | 'enumerateFromThenTo' for 'Enum' types not larger than 'Int'.
--
{-# INLINE enumerateFromThenToSmall #-}
enumerateFromThenToSmall :: (Monad m, Enum a)
    => a -> a -> a -> Stream m a
enumerateFromThenToSmall from next to =
          fmap toEnum
        $ enumerateFromThenToNum
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
class Enumerable a where

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

#define ENUMERABLE_REAL_FLOAT(FRACTIONAL_TYPE)                   \
instance Enumerable FRACTIONAL_TYPE where {                      \
    {-# INLINE enumerateFrom #-};                                \
    enumerateFrom = enumerateFromRealFloat;                      \
    {-# INLINE enumerateFromThen #-};                            \
    enumerateFromThen = enumerateFromThenRealFloat;              \
    {-# INLINE enumerateFromTo #-};                              \
    enumerateFromTo = enumerateFromToRealFloat;                  \
    {-# INLINE enumerateFromThenTo #-};                          \
    enumerateFromThenTo = enumerateFromThenToRealFloat }

ENUMERABLE_REAL_FLOAT(Float)
ENUMERABLE_REAL_FLOAT(Double)

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
