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

    -- ** Enumerating 'Num' Types
    , enumerateFromStepNum
    , enumerateFromNum
    , enumerateFromThenNum

    -- ** Enumerating unbounded 'Integral' Types
    , enumerateFromStepIntegral
    , enumerateFromIntegral
    , enumerateFromThenIntegral
    , enumerateFromToIntegral
    , enumerateFromThenToIntegral

    -- ** Enumerating 'Bounded' 'Integral' Types
    , enumerateFromIntegralBounded
    , enumerateFromThenIntegralBounded
    , enumerateFromToIntegralBounded
    , enumerateFromThenToIntegralBounded

    -- ** Enumerating small 'Integral' Types
    -- | Small types are always bounded.
    , enumerateFromSmallBounded
    , enumerateFromThenSmallBounded
    , enumerateFromToSmall
    , enumerateFromThenToSmall

    -- ** Enumerating 'Fractional' Types
    -- | Enumeration of 'Num' specialized to 'Fractional' types.
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
-- >>> Stream.fold Fold.toList $ Stream.take 10 $ Stream.unfold Unfold.enumerateFromStepNum (255::Word8,1)
-- [255,0,1,2,3,4,5,6,7,8]
--
-- @
--
-- The implementation is numerically stable for floating point values.
--
-- Note 'enumerateFromStepIntegral' is faster for integrals.
--
-- /Internal/
--
{-# INLINE enumerateFromStepNum #-}
enumerateFromStepNum :: (Monad m, Num a) => Unfold m (a, a) a
enumerateFromStepNum = Unfold step inject

    where

    inject (!from, !stride) = return (from, stride, 0)

    -- Note that the counter "i" is the same type as the type being enumerated.
    -- It may overflow, for example, if we are enumerating Word8, after 255 the
    -- counter will become 0, but the overflow does not affect the enumeration
    -- behavior.
    {-# INLINE_LATE step #-}
    step (from, stride, i) =
        return $
            (Yield $! (from + i * stride)) $! (from, stride, i + 1)

-- | Same as 'enumerateFromStepNum (from, next)' using a stride of @next - from@:
--
-- @
-- >>> enumerateFromThenNum = lmap (\(from, next) -> (from, next - from)) Unfold.enumerateFromStepNum
--
-- @
--
-- Example:
-- @
-- >>> Stream.fold Fold.toList $ Stream.take 10 $ Stream.unfold enumerateFromThenNum (255::Word8,0)
-- [255,0,1,2,3,4,5,6,7,8]
--
-- @
--
-- The implementation is numerically stable for floating point values.
--
-- Note that 'enumerateFromThenIntegral' is faster for integrals.
--
-- Note that in the strange world of floating point numbers, using
-- @enumerateFromThenNum (from, from + 1)@ is almost exactly the same as
-- @enumerateFromStepNum (from, 1) but not precisely the same. Because @(from +
-- 1) - from@ is not exactly 1, it may lose some precision, the loss may also
-- be aggregated in each step, if you want that precision then use
-- 'enumerateFromStepNum' instead.
--
-- /Internal/
--
{-# INLINE enumerateFromThenNum #-}
enumerateFromThenNum :: (Monad m, Num a) => Unfold m (a, a) a
enumerateFromThenNum =
    lmap (\(from, next) -> (from, next - from)) enumerateFromStepNum

-- | Same as 'enumerateFromStepNum' using a stride of 1:
--
-- @
-- >>> enumerateFromNum = lmap (\from -> (from, 1)) Unfold.enumerateFromStepNum
-- >>> Stream.fold Fold.toList $ Stream.take 6 $ Stream.unfold enumerateFromNum (0.9)
-- [0.9,1.9,2.9,3.9,4.9,5.9]
--
-- @
--
-- Also, same as 'enumerateFromThenNum' using a stride of 1 but see the note in
-- 'enumerateFromThenNum' about the loss of precision:
--
-- @
-- >>> enumerateFromNum = lmap (\from -> (from, from + 1)) Unfold.enumerateFromThenNum
-- >>> Stream.fold Fold.toList $ Stream.take 6 $ Stream.unfold enumerateFromNum (0.9)
-- [0.9,1.9,2.9,3.8999999999999995,4.8999999999999995,5.8999999999999995]
--
-- @
--
-- /Internal/
--
{-# INLINE enumerateFromNum #-}
enumerateFromNum :: (Monad m, Num a) => Unfold m a a
enumerateFromNum = lmap (\from -> (from, 1)) enumerateFromStepNum

------------------------------------------------------------------------------
-- Enumeration of Integrals
------------------------------------------------------------------------------

-- | Can be used to enumerate unbounded integrals. This does not check for
-- overflow or underflow for bounded integrals.
--
-- /Internal/
{-# INLINE_NORMAL enumerateFromStepIntegral #-}
enumerateFromStepIntegral :: (Monad m, Integral a) => Unfold m (a, a) a
enumerateFromStepIntegral = Unfold step inject

    where

    inject (from, stride) = from `seq` stride `seq` return (from, stride)

    {-# INLINE_LATE step #-}
    step (x, stride) = return $ Yield x $! (x + stride, stride)

-- Enumerate Unbounded Integrals ----------------------------------------------
{-# INLINE enumerateFromIntegral #-}
enumerateFromIntegral :: (Monad m, Integral a) => Unfold m a a
enumerateFromIntegral = lmap (\from -> (from, 1)) enumerateFromStepIntegral

{-# INLINE enumerateFromThenIntegral #-}
enumerateFromThenIntegral :: (Monad m, Integral a ) => Unfold m (a, a) a
enumerateFromThenIntegral =
    lmap (\(from, next) -> (from, next - from)) enumerateFromStepIntegral

{-# INLINE enumerateFromToIntegral #-}
enumerateFromToIntegral :: (Monad m, Integral a) => Unfold m (a, a) a
enumerateFromToIntegral =
    takeWhileMWithInput (\(_, to) b -> return $ b <= to)
        $ lmap (\(from, _) -> (from, 1)) enumerateFromStepIntegral

{-# INLINE enumerateFromThenToIntegral #-}
enumerateFromThenToIntegral :: (Monad m, Integral a) => Unfold m (a, a, a) a
enumerateFromThenToIntegral =
    takeWhileMWithInput cond $ lmap toFromStep enumerateFromStepIntegral

    where

    toFromStep (from, next, _) = (from, next - from)

    cond (from, next, to) b =
        return
            $ if next >= from
              then b <= to
              else b >= to

-- Enumerate Bounded Integrals ------------------------------------------------
{-# INLINE enumerateFromIntegralBounded #-}
enumerateFromIntegralBounded :: (Monad m, Integral a, Bounded a) =>
    Unfold m a a
enumerateFromIntegralBounded = second maxBound enumerateFromToIntegral

{-# INLINE enumerateFromThenIntegralBounded #-}
enumerateFromThenIntegralBounded :: (Monad m, Integral a, Bounded a ) =>
    Unfold m (a, a) a
enumerateFromThenIntegralBounded =
    takeWhileMWithInput cond $ lmap toFromStep enumerateFromStepIntegral

    where

    toFromStep (from, next) = (from, next - from)

    cond (from, next) b =
        return
            $ if next >= from
              then b <= maxBound
              else b >= minBound

{-# INLINE enumerateFromToIntegralBounded #-}
enumerateFromToIntegralBounded :: (Monad m, Integral a, Bounded a) =>
    Unfold m (a, a) a
enumerateFromToIntegralBounded =
    takeWhileMWithInput (\(_, to) b -> return $ b <= to)
        $ lmap fst enumerateFromIntegralBounded

{-# INLINE enumerateFromThenToIntegralBounded #-}
enumerateFromThenToIntegralBounded :: (Monad m, Integral a, Bounded a) =>
    Unfold m (a, a, a) a
enumerateFromThenToIntegralBounded =
    takeWhileMWithInput cond $ lmap toFromThen enumerateFromThenIntegralBounded

    where

    toFromThen (from, next, _) = (from, next)

    cond (from, next, to) b =
        return
            $ if next >= from
              then b <= to
              else b >= to

------------------------------------------------------------------------------
-- Enumeration of Fractionals
------------------------------------------------------------------------------

{-# INLINE_NORMAL enumerateFromFractional #-}
enumerateFromFractional :: (Monad m, Fractional a) => Unfold m a a
enumerateFromFractional = enumerateFromNum

{-# INLINE_NORMAL enumerateFromThenFractional #-}
enumerateFromThenFractional :: (Monad m, Fractional a) => Unfold m (a, a) a
enumerateFromThenFractional = enumerateFromThenNum

-- | Same as 'enumerateFromStepNum' with a step of 1 and enumerating up to the
-- specified upper limit rounded to the nearest integral value:
--
-- @
-- >>> Stream.fold Fold.toList $ Stream.unfold Unfold.enumerateFromToFractional (0.1, 6.3)
-- [0.1,1.1,2.1,3.1,4.1,5.1,6.1]
--
-- @
--
-- /Internal/
--
{-# INLINE_NORMAL enumerateFromToFractional #-}
enumerateFromToFractional :: (Monad m, Fractional a, Ord a) =>
    Unfold m (a, a) a
enumerateFromToFractional =
    takeWhileMWithInput (\(_, to) b -> return $ b <= to + 1 / 2)
        $ lmap (\(from, _) -> (from, 1)) enumerateFromStepNum

{-# INLINE enumerateFromThenToFractional #-}
enumerateFromThenToFractional :: (Monad m, Fractional a, Ord a) =>
    Unfold m (a, a, a) a
enumerateFromThenToFractional =
    takeWhileMWithInput cond $ lmap toFromStep enumerateFromStepNum

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
    fmap toEnum (lmap (bimap fromEnum fromEnum) enumerateFromToIntegral)

-- | Enumerate from given starting Enum value 'from' and then Enum value 'next'
-- and to Enum value 'to' with stride of (fromEnum next - fromEnum from)
-- till to value.
--
-- /Internal/
--
{-# INLINE enumerateFromThenToSmall #-}
enumerateFromThenToSmall :: (Monad m, Enum a) => Unfold m (a, a, a) a
enumerateFromThenToSmall =
    let toInts (x, y, z) = (fromEnum x, fromEnum y, fromEnum z)
     in fmap toEnum (lmap toInts enumerateFromThenToIntegral)

-------------------------------------------------------------------------------
-- Bounded Enumeration of Enum types not larger than Int
-------------------------------------------------------------------------------

-- | Enumerate from given starting Enum value 'from' with stride of 1 till
-- maxBound
--
-- /Internal/
--
{-# INLINE enumerateFromSmallBounded #-}
enumerateFromSmallBounded :: (Monad m, Enum a, Bounded a) => Unfold m a a
enumerateFromSmallBounded = second maxBound enumerateFromToSmall

-- | Enumerate from given starting Enum value 'from' and next Enum value 'next'
-- with stride of (fromEnum next - fromEnum from) till maxBound.
--
-- /Internal/
--
{-# INLINE enumerateFromThenSmallBounded #-}
enumerateFromThenSmallBounded :: forall m a. (Monad m, Enum a, Bounded a) =>
    Unfold m (a, a) a
enumerateFromThenSmallBounded =
    let adapt (from, next) =
            let frm = fromEnum from
                nxt = fromEnum next
                stride = nxt - frm
                to = if stride >= 0
                     then fromEnum (maxBound :: a)
                     else fromEnum (minBound :: a)
             in (frm, nxt, to)
     in fmap toEnum (lmap adapt enumerateFromThenToIntegral)

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
    -- >>> import qualified Streamly.Data.Stream as Stream
    -- >>> import qualified Streamly.Internal.Data.Unfold as Unfold
    --
    -- @
    -- >>> Stream.fold Fold.toList $ Stream.take 4 $ Stream.unfold Unfold.enumerateFrom (0 :: Int)
    -- [0,1,2,3]
    --
    -- @
    --
    -- For 'Fractional' types, enumeration is numerically stable. However, no
    -- overflow or underflow checks are performed.
    --
    -- @
    -- >>> Stream.fold Fold.toList $ Stream.take 4 $ Stream.unfold Unfold.enumerateFrom 1.1
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
    -- >>> import qualified Streamly.Data.Stream as Stream
    -- >>> import qualified Streamly.Internal.Data.Unfold as Unfold
    --
    -- @
    -- >>> Stream.fold Fold.toList $ Stream.unfold Unfold.enumerateFromTo (0, 4)
    -- [0,1,2,3,4]
    --
    -- @
    --
    -- For 'Fractional' types, the last element is equal to the specified @to@
    -- value after rounding to the nearest integral value.
    --
    -- @
    -- >>> Stream.fold Fold.toList $ Stream.unfold Unfold.enumerateFromTo (1.1, 4)
    -- [1.1,2.1,3.1,4.1]
    --
    -- >>> Stream.fold Fold.toList $ Stream.unfold Unfold.enumerateFromTo (1.1, 4.6)
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
    -- >>> import qualified Streamly.Data.Stream as Stream
    -- >>> import qualified Streamly.Internal.Data.Unfold as Unfold
    --
    -- @
    -- >>> Stream.fold Fold.toList $ Stream.take 4 $ Stream.unfold Unfold.enumerateFromThen (0, 2)
    -- [0,2,4,6]
    --
    -- >>> Stream.fold Fold.toList $ Stream.take 4 $ Stream.unfold Unfold.enumerateFromThen (0,(-2))
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
    -- >>> import qualified Streamly.Data.Stream as Stream
    -- >>> import qualified Streamly.Internal.Data.Unfold as Unfold
    --
    -- @
    -- >>> Stream.fold Fold.toList $ Stream.unfold Unfold.enumerateFromThenTo (0, 2, 6)
    -- [0,2,4,6]
    --
    -- >>> Stream.fold Fold.toList $ Stream.unfold Unfold.enumerateFromThenTo (0, (-2), (-6))
    -- [0,-2,-4,-6]
    --
    -- @
    --
    -- /Pre-release/
    enumerateFromThenTo :: Monad m => Unfold m (a, a, a) a

-------------------------------------------------------------------------------
-- Enumerable Instances
-------------------------------------------------------------------------------
--
-- For Enum types smaller than or equal to Int size.
#define ENUMERABLE_BOUNDED_SMALL(SMALL_TYPE)           \
instance Enumerable SMALL_TYPE where {                 \
    {-# INLINE enumerateFrom #-};                      \
    enumerateFrom = enumerateFromSmallBounded;         \
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
