#if __GHCJS__
#define HS_CLOCK_GHCJS 1
#elif defined(_WIN32)
#define HS_CLOCK_WINDOWS 1
#elif HAVE_TIME_H && HAVE_CLOCK_GETTIME
#define HS_CLOCK_POSIX 1
#elif __APPLE__
#define HS_CLOCK_OSX 1
#else
#error "Time/Clock functionality not implemented for this system"
#endif
