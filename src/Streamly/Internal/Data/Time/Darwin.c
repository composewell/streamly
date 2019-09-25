/*
 * Code taken from the Haskell "clock" package.
 *
 * Copyright (c) 2009-2012, Cetin Sert
 * Copyright (c) 2010, Eugene Kirpichov
 *
 * OS X code was contributed by Gerolf Seitz on 2013-10-15.
 */

#ifdef __MACH__
#include <time.h>
#include <mach/clock.h>
#include <mach/mach.h>

void clock_gettime_darwin(clock_id_t clock, struct timespec *ts)
{
    clock_serv_t cclock;
    mach_timespec_t mts;
    host_get_clock_service(mach_host_self(), clock, &cclock);
    clock_get_time(cclock, &mts);
    mach_port_deallocate(mach_task_self(), cclock);
    ts->tv_sec = mts.tv_sec;
    ts->tv_nsec = mts.tv_nsec;
}

void clock_getres_darwin(clock_id_t clock, struct timespec *ts)
{
    clock_serv_t cclock;
    int nsecs;
    mach_msg_type_number_t count;
    host_get_clock_service(mach_host_self(), clock, &cclock);
    clock_get_attributes(cclock, CLOCK_GET_TIME_RES, (clock_attr_t)&nsecs, &count);
    mach_port_deallocate(mach_task_self(), cclock);
}

#endif  /* __MACH__ */
