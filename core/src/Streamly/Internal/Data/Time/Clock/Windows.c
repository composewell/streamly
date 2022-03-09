/*
 * Code taken from the Haskell "clock" package.
 *
 * Copyright (c) 2009-2012, Cetin Sert
 * Copyright (c) 2010, Eugene Kirpichov
 */

#ifdef _WIN32
#include <windows.h>

#if defined(_MSC_VER) || defined(_MSC_EXTENSIONS)
  #define U64(x) x##Ui64
#else
  #define U64(x) x##ULL
#endif

#define DELTA_EPOCH_IN_100NS  U64(116444736000000000)

static long ticks_to_nanos(LONGLONG subsecond_time, LONGLONG frequency)
{
    return (long)((1e9 * subsecond_time) / frequency);
}

static ULONGLONG to_quad_100ns(FILETIME ft)
{
    ULARGE_INTEGER li;
    li.LowPart = ft.dwLowDateTime;
    li.HighPart = ft.dwHighDateTime;
    return li.QuadPart;
}

static void to_timespec_from_100ns(ULONGLONG t_100ns, long long *t)
{
    t[0] = (long)(t_100ns / 10000000UL);
    t[1] = 100*(long)(t_100ns % 10000000UL);
}

void clock_gettime_win32_monotonic(long long* t)
{
   LARGE_INTEGER time;
   LARGE_INTEGER frequency;
   QueryPerformanceCounter(&time);
   QueryPerformanceFrequency(&frequency);
   // seconds
   t[0] = time.QuadPart / frequency.QuadPart;
   // nanos =
   t[1] = ticks_to_nanos(time.QuadPart % frequency.QuadPart, frequency.QuadPart);
}

void clock_gettime_win32_realtime(long long* t)
{
    FILETIME ft;
    ULONGLONG tmp;

    GetSystemTimeAsFileTime(&ft);

    tmp = to_quad_100ns(ft);
    tmp -= DELTA_EPOCH_IN_100NS;

    to_timespec_from_100ns(tmp, t);
}

void clock_gettime_win32_processtime(long long* t)
{
    FILETIME creation_time, exit_time, kernel_time, user_time;
    ULONGLONG time;

    GetProcessTimes(GetCurrentProcess(), &creation_time, &exit_time, &kernel_time, &user_time);
    // Both kernel and user, acc. to http://www.opengroup.org/onlinepubs/009695399/basedefs/xbd_chap03.html#tag_03_117

    time = to_quad_100ns(user_time) + to_quad_100ns(kernel_time);
    to_timespec_from_100ns(time, t);
}

void clock_gettime_win32_threadtime(long long* t)
{
    FILETIME creation_time, exit_time, kernel_time, user_time;
    ULONGLONG time;

    GetThreadTimes(GetCurrentThread(), &creation_time, &exit_time, &kernel_time, &user_time);
    // Both kernel and user, acc. to http://www.opengroup.org/onlinepubs/009695399/basedefs/xbd_chap03.html#tag_03_117

    time = to_quad_100ns(user_time) + to_quad_100ns(kernel_time);
    to_timespec_from_100ns(time, t);
}

void clock_getres_win32_monotonic(long long* t)
{
    LARGE_INTEGER frequency;
    QueryPerformanceFrequency(&frequency);

    ULONGLONG resolution = U64(1000000000)/frequency.QuadPart;
    t[0] = resolution / U64(1000000000);
    t[1] = resolution % U64(1000000000);
}

void clock_getres_win32_realtime(long long* t)
{
    t[0] = 0;
    t[1] = 100;
}

void clock_getres_win32_processtime(long long* t)
{
    t[0] = 0;
    t[1] = 100;
}

void clock_getres_win32_threadtime(long long* t)
{
    t[0] = 0;
    t[1] = 100;
}

#endif  /* _WIN32 */
