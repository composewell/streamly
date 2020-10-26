#include "inline.hs"

-- |
-- Module      : Streamly.Internal.Data.Time.Units
-- Copyright   : (c) 2019 Composewell Technologies
--
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

module Streamly.Internal.Data.Time.Units
    (
    -- * Time Unit Conversions
      TimeUnit()
    -- , TimeUnitWide()
    , TimeUnit64()

    -- * Time Units
    , TimeSpec(..)
    , NanoSecond64(..)
    , MicroSecond64(..)
    , MilliSecond64(..)
    , showNanoSecond64

    -- * Absolute times (using TimeSpec)
    , AbsTime(..)
    , toAbsTime
    , fromAbsTime

    -- * Relative times (using TimeSpec)
    , RelTime
    , toRelTime
    , fromRelTime
    , diffAbsTime
    , addToAbsTime

    -- * Relative times (using NanoSecond64)
    , RelTime64
    , toRelTime64
    , fromRelTime64
    , diffAbsTime64
    , addToAbsTime64
    , showRelTime64
    )
where

import Text.Printf (printf)

import Data.Int
import Streamly.Internal.Data.Time.TimeSpec

-------------------------------------------------------------------------------
-- Some constants
-------------------------------------------------------------------------------

{-# INLINE tenPower3 #-}
tenPower3 :: Int64
tenPower3 = 1000

{-# INLINE tenPower6 #-}
tenPower6 :: Int64
tenPower6 = 1000000

{-# INLINE tenPower9 #-}
tenPower9 :: Int64
tenPower9 = 1000000000


-------------------------------------------------------------------------------
-- Time Unit Representations
-------------------------------------------------------------------------------

-- XXX We should be able to use type families to use different represenations
-- for a unit.
--
-- Second Rational
-- Second Double
-- Second Int64
-- Second Integer
-- NanoSecond Int64
-- ...

-- Double or Fixed would be a much better representation so that we do not lose
-- information between conversions. However, for faster arithmetic operations
-- we use an 'Int64' here. When we need convservation of values we can use a
-- different system of units with a Fixed precision.

-------------------------------------------------------------------------------
-- Integral Units
-------------------------------------------------------------------------------

-- | An 'Int64' time representation with a nanosecond resolution. It can
-- represent time up to ~292 years.
newtype NanoSecond64 = NanoSecond64 Int64
    deriving ( Eq
             , Read
             , Show
             , Enum
             , Bounded
             , Num
             , Real
             , Integral
             , Ord
             )

-- | An 'Int64' time representation with a microsecond resolution.
-- It can represent time up to ~292,000 years.
newtype MicroSecond64 = MicroSecond64 Int64
    deriving ( Eq
             , Read
             , Show
             , Enum
             , Bounded
             , Num
             , Real
             , Integral
             , Ord
             )

-- | An 'Int64' time representation with a millisecond resolution.
-- It can represent time up to ~292 million years.
newtype MilliSecond64 = MilliSecond64 Int64
    deriving ( Eq
             , Read
             , Show
             , Enum
             , Bounded
             , Num
             , Real
             , Integral
             , Ord
             )

-------------------------------------------------------------------------------
-- Fractional Units
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- Time unit conversions
-------------------------------------------------------------------------------

-- TODO: compare whether using TimeSpec instead of Integer provides significant
-- performance boost. If not then we can just use Integer nanoseconds and get
-- rid of TimeUnitWide.
--
-- | A type class for converting between time units using 'Integer' as the
-- intermediate and the widest representation with a nanosecond resolution.
-- This system of units can represent arbitrarily large times but provides
-- least efficient arithmetic operations due to 'Integer' arithmetic.
--
-- NOTE: Converting to and from units may truncate the value depending on the
-- original value and the size and resolution of the destination unit.
{-
class TimeUnitWide a where
    toTimeInteger   :: a -> Integer
    fromTimeInteger :: Integer -> a
-}

-- | A type class for converting between units of time using 'TimeSpec' as the
-- intermediate representation.  This system of units can represent up to ~292
-- billion years at nanosecond resolution with reasonably efficient arithmetic
-- operations.
--
-- NOTE: Converting to and from units may truncate the value depending on the
-- original value and the size and resolution of the destination unit.
class TimeUnit a where
    toTimeSpec   :: a -> TimeSpec
    fromTimeSpec :: TimeSpec -> a

-- XXX we can use a fromNanoSecond64 for conversion with overflow check and
-- fromNanoSecond64Unsafe for conversion without overflow check.
--
-- | A type class for converting between units of time using 'Int64' as the
-- intermediate representation with a nanosecond resolution.  This system of
-- units can represent up to ~292 years at nanosecond resolution with fast
-- arithmetic operations.
--
-- NOTE: Converting to and from units may truncate the value depending on the
-- original value and the size and resolution of the destination unit.
class TimeUnit64 a where
    toNanoSecond64   :: a -> NanoSecond64
    fromNanoSecond64 :: NanoSecond64 -> a

-------------------------------------------------------------------------------
-- Time units
-------------------------------------------------------------------------------

instance TimeUnit TimeSpec where
    toTimeSpec = id
    fromTimeSpec = id

instance TimeUnit NanoSecond64 where
    {-# INLINE toTimeSpec #-}
    toTimeSpec (NanoSecond64 t) = TimeSpec s ns
        where (s, ns) = t `divMod` tenPower9

    {-# INLINE fromTimeSpec #-}
    fromTimeSpec (TimeSpec s ns) =
        NanoSecond64 $ s * tenPower9 + ns

instance TimeUnit64 NanoSecond64 where
    {-# INLINE toNanoSecond64 #-}
    toNanoSecond64 = id

    {-# INLINE fromNanoSecond64 #-}
    fromNanoSecond64 = id

instance TimeUnit MicroSecond64 where
    {-# INLINE toTimeSpec #-}
    toTimeSpec (MicroSecond64 t) = TimeSpec s (us * tenPower3)
        where (s, us) = t `divMod` tenPower6

    {-# INLINE fromTimeSpec #-}
    fromTimeSpec (TimeSpec s ns) =
        -- XXX round ns to nearest microsecond?
        MicroSecond64 $ s * tenPower6 + (ns `div` tenPower3)

instance TimeUnit64 MicroSecond64 where
    {-# INLINE toNanoSecond64 #-}
    toNanoSecond64 (MicroSecond64 us) = NanoSecond64 $ us * tenPower3

    {-# INLINE fromNanoSecond64 #-}
    -- XXX round ns to nearest microsecond?
    fromNanoSecond64 (NanoSecond64 ns) = MicroSecond64 $ ns `div` tenPower3

instance TimeUnit MilliSecond64 where
    {-# INLINE toTimeSpec #-}
    toTimeSpec (MilliSecond64 t) = TimeSpec s (ms * tenPower6)
        where (s, ms) = t `divMod` tenPower3

    {-# INLINE fromTimeSpec #-}
    fromTimeSpec (TimeSpec s ns) =
        -- XXX round ns to nearest millisecond?
        MilliSecond64 $ s * tenPower3 + (ns `div` tenPower6)

instance TimeUnit64 MilliSecond64 where
    {-# INLINE toNanoSecond64 #-}
    toNanoSecond64 (MilliSecond64 ms) = NanoSecond64 $ ms * tenPower6

    {-# INLINE fromNanoSecond64 #-}
    -- XXX round ns to nearest millisecond?
    fromNanoSecond64 (NanoSecond64 ns) = MilliSecond64 $ ns `div` tenPower6

-------------------------------------------------------------------------------
-- Absolute time
-------------------------------------------------------------------------------

-- | Absolute times are relative to a predefined epoch in time. 'AbsTime'
-- represents times using 'TimeSpec' which can represent times up to ~292
-- billion years at a nanosecond resolution.
newtype AbsTime = AbsTime TimeSpec
    deriving (Eq, Ord, Show)

-- | Convert a 'TimeUnit' to an absolute time.
{-# INLINE_NORMAL toAbsTime #-}
toAbsTime :: TimeUnit a => a -> AbsTime
toAbsTime = AbsTime . toTimeSpec

-- | Convert absolute time to a 'TimeUnit'.
{-# INLINE_NORMAL fromAbsTime #-}
fromAbsTime :: TimeUnit a => AbsTime -> a
fromAbsTime (AbsTime t) = fromTimeSpec t

-- XXX We can also write rewrite rules to simplify divisions multiplications
-- and additions when manipulating units. Though, that might get simplified at
-- the assembly (llvm) level as well. Note to/from conversions may be lossy and
-- therefore this equation may not hold, but that's ok.
{-# RULES "fromAbsTime/toAbsTime" forall a. toAbsTime (fromAbsTime a) = a #-}
{-# RULES "toAbsTime/fromAbsTime" forall a. fromAbsTime (toAbsTime a) = a #-}

-------------------------------------------------------------------------------
-- Relative time using NaonoSecond64 as the underlying representation
-------------------------------------------------------------------------------

-- We use a separate type to represent relative time for safety and speed.
-- RelTime has a Num instance, absolute time doesn't.  Relative times are
-- usually shorter and for our purposes an Int64 nanoseconds can hold close to
-- thousand year duration. It is also faster to manipulate. We do not check for
-- overflows during manipulations so use it only when you know the time cannot
-- be too big. If you need a bigger RelTime representation then use RelTimeBig.

-- | Relative times are relative to some arbitrary point of time. Unlike
-- 'AbsTime' they are not relative to a predefined epoch.
newtype RelTime64 = RelTime64 NanoSecond64
    deriving ( Eq
             , Read
             , Show
             , Enum
             , Bounded
             , Num
             , Real
             , Integral
             , Ord
             )

-- | Convert a 'TimeUnit' to a relative time.
{-# INLINE_NORMAL toRelTime64 #-}
toRelTime64 :: TimeUnit64 a => a -> RelTime64
toRelTime64 = RelTime64 . toNanoSecond64

-- | Convert relative time to a 'TimeUnit'.
{-# INLINE_NORMAL fromRelTime64 #-}
fromRelTime64 :: TimeUnit64 a => RelTime64 -> a
fromRelTime64 (RelTime64 t) = fromNanoSecond64 t

{-# RULES "fromRelTime64/toRelTime64" forall a .
          toRelTime64 (fromRelTime64 a) = a #-}

{-# RULES "toRelTime64/fromRelTime64" forall a .
          fromRelTime64 (toRelTime64 a) = a #-}

-- | Difference between two absolute points of time.
{-# INLINE diffAbsTime64 #-}
diffAbsTime64 :: AbsTime -> AbsTime -> RelTime64
diffAbsTime64 (AbsTime (TimeSpec s1 ns1)) (AbsTime (TimeSpec s2 ns2)) =
    RelTime64 $ NanoSecond64 $ ((s1 - s2) * tenPower9) + (ns1 - ns2)

{-# INLINE addToAbsTime64 #-}
addToAbsTime64 :: AbsTime -> RelTime64 -> AbsTime
addToAbsTime64 (AbsTime (TimeSpec s1 ns1)) (RelTime64 (NanoSecond64 ns2)) =
    AbsTime $ TimeSpec (s1 + s) ns
    where (s, ns) = (ns1 + ns2) `divMod` tenPower9

-------------------------------------------------------------------------------
-- Relative time using TimeSpec as the underlying representation
-------------------------------------------------------------------------------

newtype RelTime = RelTime TimeSpec
    deriving ( Eq
             , Read
             , Show
             -- , Enum
             -- , Bounded
             , Num
             -- , Real
             -- , Integral
             , Ord
             )

{-# INLINE_NORMAL toRelTime #-}
toRelTime :: TimeUnit a => a -> RelTime
toRelTime = RelTime . toTimeSpec

{-# INLINE_NORMAL fromRelTime #-}
fromRelTime :: TimeUnit a => RelTime -> a
fromRelTime (RelTime t) = fromTimeSpec t

{-# RULES "fromRelTime/toRelTime" forall a. toRelTime (fromRelTime a) = a #-}
{-# RULES "toRelTime/fromRelTime" forall a. fromRelTime (toRelTime a) = a #-}

-- XXX rename to diffAbsTimes?
{-# INLINE diffAbsTime #-}
diffAbsTime :: AbsTime -> AbsTime -> RelTime
diffAbsTime (AbsTime t1) (AbsTime t2) = RelTime (t1 - t2)

{-# INLINE addToAbsTime #-}
addToAbsTime :: AbsTime -> RelTime -> AbsTime
addToAbsTime (AbsTime t1) (RelTime t2) = AbsTime $ t1 + t2

-------------------------------------------------------------------------------
-- Formatting and printing
-------------------------------------------------------------------------------

-- | Convert nanoseconds to a string showing time in an appropriate unit.
showNanoSecond64 :: NanoSecond64 -> String
showNanoSecond64 time@(NanoSecond64 ns)
    | time < 0    = '-' : showNanoSecond64 (-time)
    | ns < 1000 = fromIntegral ns `with` "ns"
#ifdef mingw32_HOST_OS
    | ns < 1000000 = (fromIntegral ns / 1000) `with` "us"
#else
    | ns < 1000000 = (fromIntegral ns / 1000) `with` "μs"
#endif
    | ns < 1000000000 = (fromIntegral ns / 1000000) `with` "ms"
    | ns < (60 * 1000000000) = (fromIntegral ns / 1000000000) `with` "s"
    | ns < (60 * 60 * 1000000000) =
        (fromIntegral ns / (60 * 1000000000)) `with` "min"
    | ns < (24 * 60 * 60 * 1000000000) =
        (fromIntegral ns / (60 * 60 * 1000000000)) `with` "hr"
    | ns < (365 * 24 * 60 * 60 * 1000000000) =
        (fromIntegral ns / (24 * 60 * 60 * 1000000000)) `with` "days"
    | otherwise =
        (fromIntegral ns / (365 * 24 * 60 * 60 * 1000000000)) `with` "years"
     where with (t :: Double) (u :: String)
               | t >= 1e9  = printf "%.4g %s" t u
               | t >= 1e3  = printf "%.0f %s" t u
               | t >= 1e2  = printf "%.1f %s" t u
               | t >= 1e1  = printf "%.2f %s" t u
               | otherwise = printf "%.3f %s" t u

-- In general we should be able to show the time in a specified unit, if we
-- omit the unit we can show it in an automatically chosen one.
{-
data UnitName =
      Nano
    | Micro
    | Milli
    | Sec
-}

showRelTime64 :: RelTime64 -> String
showRelTime64 = showNanoSecond64 . fromRelTime64
