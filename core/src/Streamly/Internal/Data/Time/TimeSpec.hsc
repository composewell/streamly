{-# OPTIONS_GHC -Wno-identities          #-}

-- |
-- Module      : Streamly.Internal.Data.Time.TimeSpec
-- Copyright   : (c) 2019 Composewell Technologies
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

module Streamly.Internal.Data.Time.TimeSpec
    (
      TimeSpec(..)
    )
where

#ifndef __GHCJS__
#include "config.h"
#endif

#include "Streamly/Internal/Data/Time/Clock/config-clock.h"

#include "MachDeps.h"

import Data.Int (Int64)
#if (WORD_SIZE_IN_BITS == 32)
import Data.Int (Int32)
#endif
import Foreign.Storable (Storable(..), peek)

#ifdef HS_CLOCK_GHCJS
import Foreign.C (CTime(..), CLong(..))
#endif

-------------------------------------------------------------------------------
-- Some constants
-------------------------------------------------------------------------------

{-# INLINE tenPower9 #-}
tenPower9 :: Int64
tenPower9 = 1000000000

-------------------------------------------------------------------------------
-- TimeSpec
-------------------------------------------------------------------------------

-- | 'TimeSpec' can hold time values up to ~292 billion years at nanosecond
-- precision.
--
-- Use 'fromInteger' from the Num instance to create 'TimeSpec' from
-- nanoseconds.  Use 'Eq' and 'Ord' instances for comparisons. Use the 'Num'
-- instance to perform arithmetic operations.
--
-- Note, we assume that 'nsec' is always less than 10^9. Also, when 'TimeSpec'
-- is negative then both 'sec' and 'nsec' must be negative.

-- XXX Use smart constructors to enforce these assumptions.
data TimeSpec = TimeSpec
  { sec  :: {-# UNPACK #-} !Int64 -- ^ seconds
  -- This could be Int32 instead but Int64 is as good.
  , nsec :: {-# UNPACK #-} !Int64 -- ^ nanoseconds
  } deriving (Eq, Read, Show)

-- We assume that nsec is always less than 10^9. When TimeSpec is negative then
-- both sec and nsec are negative.
instance Ord TimeSpec where
    compare (TimeSpec s1 ns1) (TimeSpec s2 ns2) =
        if s1 == s2
        then compare ns1 ns2
        else compare s1 s2

-- make sure nsec is less than 10^9
{-# INLINE addWithOverflow #-}
addWithOverflow :: TimeSpec -> TimeSpec -> TimeSpec
addWithOverflow (TimeSpec s1 ns1) (TimeSpec s2 ns2) =
    let nsum = ns1 + ns2
        (s', ns) = if nsum > tenPower9 || nsum < negate tenPower9
                    then nsum `divMod` tenPower9
                    else (0, nsum)
    in TimeSpec (s1 + s2 + s') ns

-- make sure both sec and nsec have the same sign
{-# INLINE adjustSign #-}
adjustSign :: TimeSpec -> TimeSpec
adjustSign t@(TimeSpec s ns)
    | s > 0 && ns < 0 = TimeSpec (s - 1) (ns + tenPower9)
    | s < 0 && ns > 0 = TimeSpec (s + 1) (ns - tenPower9)
    | otherwise = t

{-# INLINE timeSpecToInteger #-}
timeSpecToInteger :: TimeSpec -> Integer
timeSpecToInteger (TimeSpec s ns) = toInteger $ s * tenPower9 + ns

-- XXX Error on overflow?
-- | Note that the arithmetic operations may overflow silently.
instance Num TimeSpec where
    {-# INLINE (+) #-}
    t1 + t2 = adjustSign (addWithOverflow t1 t2)

    -- XXX will this be more optimal if implemented without "negate"?
    {-# INLINE (-) #-}
    t1 - t2 = t1 + negate t2
    t1 * t2 = fromInteger $ timeSpecToInteger t1 * timeSpecToInteger t2

    {-# INLINE negate #-}
    negate (TimeSpec s ns) = TimeSpec (negate s) (negate ns)
    {-# INLINE abs #-}
    abs    (TimeSpec s ns) = TimeSpec (abs s) (abs ns)
    {-# INLINE signum #-}
    signum (TimeSpec s ns) | s == 0    = TimeSpec (signum ns) 0
                           | otherwise = TimeSpec (signum s) 0
    -- | Convert 'Integer' nanoseconds to 'TimeSpec'.
    {-# INLINE fromInteger #-}
    fromInteger nanosec = TimeSpec (fromInteger s) (fromInteger ns)
        where (s, ns) = nanosec `divMod` toInteger tenPower9

#if HS_CLOCK_POSIX
#include <time.h>
#endif

#ifdef HS_CLOCK_GHCJS
instance Storable TimeSpec where
  sizeOf _ = 8
  alignment _ = 4
  peek p = do
    CTime  s <- peekByteOff p 0
    CLong ns <- peekByteOff p 4
    return (TimeSpec (fromIntegral s) (fromIntegral ns))
  poke p (TimeSpec s ns) = do
    pokeByteOff p 0 ((fromIntegral s) :: CTime)
    pokeByteOff p 4 ((fromIntegral ns) :: CLong)

#elif HS_CLOCK_WINDOWS
instance Storable TimeSpec where
  sizeOf _ = sizeOf (undefined :: Int64) * 2
  alignment _ = alignment (undefined :: Int64)
  peek ptr = do
    s <- peekByteOff ptr 0
    ns <- peekByteOff ptr (sizeOf (undefined :: Int64))
    return (TimeSpec s ns)
  poke ptr ts = do
      pokeByteOff ptr 0 (sec ts)
      pokeByteOff ptr (sizeOf (undefined :: Int64)) (nsec ts)
#else
instance Storable TimeSpec where
  sizeOf _ = #{size struct timespec}
  alignment _ = #{alignment struct timespec}
  peek ptr = do
      s :: #{type time_t} <- #{peek struct timespec, tv_sec} ptr
      ns :: #{type long} <- #{peek struct timespec, tv_nsec} ptr
      return $ TimeSpec (fromIntegral s) (fromIntegral ns)
  poke ptr ts = do
      let s :: #{type time_t} = fromIntegral $ sec ts
          ns :: #{type long} = fromIntegral $ nsec ts
      #{poke struct timespec, tv_sec} ptr (s)
      #{poke struct timespec, tv_nsec} ptr (ns)
#endif
