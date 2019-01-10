# Input file for autoconf to generate the configure script.

# See https://www.gnu.org/software/autoconf/manual/autoconf.html for help on
# the macros used in this file.

AC_INIT([streamly], [0.6.0], [harendra.kumar@gmail.com], [streamly])

# To suppress "WARNING: unrecognized options: --with-compiler"
AC_ARG_WITH([compiler], [GHC])

# Check headers and functions required
AC_CHECK_HEADERS([time.h])
AC_CHECK_FUNCS([clock_gettime])

# Output
AC_CONFIG_HEADERS([src/Streamly/Time/config.h])
AC_OUTPUT
