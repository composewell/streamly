{-# OPTIONS_GHC -Wno-identities          #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

#ifndef __GHCJS__
#include "config.h"
#endif

#include "Streamly/Internal/Data/Time/config-clock.h"

-- |
-- Module      : Streamly.Internal.Data.Time.Clock
-- Copyright   : (c) 2019 Composewell Technologies
--               (c) 2009-2012, Cetin Sert
--               (c) 2010, Eugene Kirpichov
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

-- A majority of the code below has been stolen from the "clock" package.

module Streamly.Internal.Data.Time.Clock
    (
    -- * get time from the system clock
      Clock(..)
    , getTime
    )
where

import Data.Int (Int32, Int64)
import Data.Typeable (Typeable)
import Data.Word (Word32)
import Foreign.C (CInt(..), throwErrnoIfMinus1_, CTime(..), CLong(..))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr)
import Foreign.Storable (Storable(..), peek)
import GHC.Generics (Generic)

import Streamly.Internal.Data.Time.Units (TimeSpec(..), AbsTime(..))

-------------------------------------------------------------------------------
-- Clock Types
-------------------------------------------------------------------------------

#if HS_CLOCK_POSIX
#include <time.h>

#if defined(CLOCK_MONOTONIC_RAW)
#define HAVE_CLOCK_MONOTONIC_RAW
#endif

-- XXX this may be RAW on apple not RAW on linux
#if __linux__ && defined(CLOCK_MONOTONIC_COARSE)
#define HAVE_CLOCK_MONOTONIC_COARSE
#endif

#if __APPLE__ && defined(CLOCK_MONOTONIC_RAW_APPROX)
#define HAVE_CLOCK_MONOTONIC_COARSE
#endif

#if __linux__ && defined(CLOCK_BOOTTIME)
#define HAVE_CLOCK_MONOTONIC_UPTIME
#endif

#if __APPLE__ && defined(CLOCK_UPTIME_RAW)
#define HAVE_CLOCK_MONOTONIC_UPTIME
#endif

#if __linux__ && defined(CLOCK_REALTIME_COARSE)
#define HAVE_CLOCK_REALTIME_COARSE
#endif

#endif

-- | Clock types. A clock may be system-wide (that is, visible to all processes)
--   or per-process (measuring time that is meaningful only within a process).
--   All implementations shall support CLOCK_REALTIME. (The only suspend-aware
--   monotonic is CLOCK_BOOTTIME on Linux.)
data Clock

    -- | The identifier for the system-wide monotonic clock, which is defined as
    --   a clock measuring real time, whose value cannot be set via
    --   @clock_settime@ and which cannot have negative clock jumps. The maximum
    --   possible clock jump shall be implementation defined. For this clock,
    --   the value returned by 'getTime' represents the amount of time (in
    --   seconds and nanoseconds) since an unspecified point in the past (for
    --   example, system start-up time, or the Epoch). This point does not
    --   change after system start-up time. Note that the absolute value of the
    --   monotonic clock is meaningless (because its origin is arbitrary), and
    --   thus there is no need to set it. Furthermore, realtime applications can
    --   rely on the fact that the value of this clock is never set.
  = Monotonic

    -- | The identifier of the system-wide clock measuring real time. For this
    --   clock, the value returned by 'getTime' represents the amount of time (in
    --   seconds and nanoseconds) since the Epoch.
  | Realtime

#ifndef HS_CLOCK_GHCJS
    -- | The identifier of the CPU-time clock associated with the calling
    --   process. For this clock, the value returned by 'getTime' represents the
    --   amount of execution time of the current process.
  | ProcessCPUTime

    -- | The identifier of the CPU-time clock associated with the calling OS
    --   thread. For this clock, the value returned by 'getTime' represents the
    --   amount of execution time of the current OS thread.
  | ThreadCPUTime
#endif

#if defined (HAVE_CLOCK_MONOTONIC_RAW)
    -- | (since Linux 2.6.28; Linux and Mac OSX)
    --   Similar to CLOCK_MONOTONIC, but provides access to a
    --   raw hardware-based time that is not subject to NTP
    --   adjustments or the incremental adjustments performed by
    --   adjtime(3).
  | MonotonicRaw
#endif

#if defined (HAVE_CLOCK_MONOTONIC_COARSE)
    -- | (since Linux 2.6.32; Linux and Mac OSX)
    --   A faster but less precise version of CLOCK_MONOTONIC.
    --   Use when you need very fast, but not fine-grained timestamps.
  | MonotonicCoarse
#endif

#if defined (HAVE_CLOCK_MONOTONIC_UPTIME)
    -- | (since Linux 2.6.39; Linux and Mac OSX)
    --   Identical to CLOCK_MONOTONIC, except it also includes
    --   any time that the system is suspended.  This allows
    --   applications to get a suspend-aware monotonic clock
    --   without having to deal with the complications of
    --   CLOCK_REALTIME, which may have discontinuities if the
    --   time is changed using settimeofday(2).
  | Uptime
#endif

#if defined (HAVE_CLOCK_REALTIME_COARSE)
    -- | (since Linux 2.6.32; Linux-specific)
    --   A faster but less precise version of CLOCK_REALTIME.
    --   Use when you need very fast, but not fine-grained timestamps.
  | RealtimeCoarse
#endif

  deriving (Eq, Enum, Generic, Read, Show, Typeable)

-------------------------------------------------------------------------------
-- Translate the Haskell "Clock" type to C
-------------------------------------------------------------------------------

#if HS_CLOCK_POSIX
-- Posix systems (Linux and Mac OSX 10.12 and later)
clockToPosixClockId :: Clock -> #{type clockid_t}
clockToPosixClockId Monotonic      = #const CLOCK_MONOTONIC
clockToPosixClockId Realtime       = #const CLOCK_REALTIME
clockToPosixClockId ProcessCPUTime = #const CLOCK_PROCESS_CPUTIME_ID
clockToPosixClockId ThreadCPUTime  = #const CLOCK_THREAD_CPUTIME_ID

#if defined(CLOCK_MONOTONIC_RAW)
clockToPosixClockId MonotonicRaw = #const CLOCK_MONOTONIC_RAW
#endif

#if __linux__ && defined (CLOCK_MONOTONIC_COARSE)
clockToPosixClockId MonotonicCoarse = #const CLOCK_MONOTONIC_COARSE
#elif __APPLE__ && defined(CLOCK_MONOTONIC_RAW_APPROX)
clockToPosixClockId MonotonicCoarse = #const CLOCK_MONOTONIC_RAW_APPROX
#endif

#if __linux__ && defined (CLOCK_REALTIME_COARSE)
clockToPosixClockId RealtimeCoarse = #const CLOCK_REALTIME_COARSE
#endif

#if __linux__ && defined(CLOCK_BOOTTIME)
clockToPosixClockId Uptime = #const CLOCK_BOOTTIME
#elif __APPLE__ && defined(CLOCK_UPTIME_RAW)
clockToPosixClockId Uptime = #const CLOCK_UPTIME_RAW
#endif

#elif HS_CLOCK_OSX
-- Mac OSX versions prior to 10.12
#include <time.h>
#include <mach/clock.h>

clockToOSXClockId :: Clock -> #{type clock_id_t}
clockToOSXClockId Monotonic      = #const SYSTEM_CLOCK
clockToOSXClockId Realtime       = #const CALENDAR_CLOCK
clockToOSXClockId ProcessCPUTime = #const SYSTEM_CLOCK
clockToOSXClockId ThreadCPUTime  = #const SYSTEM_CLOCK
#elif HS_CLOCK_GHCJS
-- XXX need to implement a monotonic clock for JS using performance.now()
clockToJSClockId :: Clock -> CInt
clockToJSClockId Monotonic      = 0
clockToJSClockId Realtime       = 0
#endif

-------------------------------------------------------------------------------
-- Clock time
-------------------------------------------------------------------------------

{-# INLINE getTimeWith #-}
getTimeWith :: (Ptr TimeSpec -> IO ()) -> IO AbsTime
getTimeWith f = do
    t <- alloca (\ptr -> f ptr >> peek ptr)
    return $ AbsTime t

#if HS_CLOCK_GHCJS

foreign import ccall unsafe "time.h clock_gettime_js"
    clock_gettime_js :: CInt -> Ptr TimeSpec -> IO CInt

{-# INLINABLE getTime #-}
getTime :: Clock -> IO AbsTime
getTime clock =
    getTimeWith (throwErrnoIfMinus1_ "clock_gettime" .
        clock_gettime_js (clockToJSClockId clock))

#elif HS_CLOCK_POSIX

foreign import ccall unsafe "time.h clock_gettime"
    clock_gettime :: #{type clockid_t} -> Ptr TimeSpec -> IO CInt

{-# INLINABLE getTime #-}
getTime :: Clock -> IO AbsTime
getTime clock =
    getTimeWith (throwErrnoIfMinus1_ "clock_gettime" .
        clock_gettime (clockToPosixClockId clock))

#elif HS_CLOCK_OSX

-- XXX perform error checks inside c implementation
foreign import ccall
    clock_gettime_darwin :: #{type clock_id_t} -> Ptr TimeSpec -> IO ()

{-# INLINABLE getTime #-}
getTime :: Clock -> IO AbsTime
getTime clock = getTimeWith $ clock_gettime_darwin (clockToOSXClockId clock)

#elif HS_CLOCK_WINDOWS

-- XXX perform error checks inside c implementation
foreign import ccall clock_gettime_win32_monotonic :: Ptr TimeSpec -> IO ()
foreign import ccall clock_gettime_win32_realtime :: Ptr TimeSpec -> IO ()
foreign import ccall clock_gettime_win32_processtime :: Ptr TimeSpec -> IO ()
foreign import ccall clock_gettime_win32_threadtime :: Ptr TimeSpec -> IO ()

{-# INLINABLE getTime #-}
getTime :: Clock -> IO AbsTime
getTime Monotonic = getTimeWith $ clock_gettime_win32_monotonic
getTime Realtime = getTimeWith $ clock_gettime_win32_realtime
getTime ProcessCPUTime = getTimeWith $ clock_gettime_win32_processtime
getTime ThreadCPUTime = getTimeWith $ clock_gettime_win32_threadtime
#endif
