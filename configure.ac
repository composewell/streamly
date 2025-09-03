# Input file for autoconf to generate the configure script.

# See https://www.gnu.org/software/autoconf/manual/autoconf.html for help on
# the macros used in this file.

AC_INIT([streamly], [0.12.0], [streamly@composewell.com], [streamly], [https://streamly.composewell.com])

# To suppress "WARNING: unrecognized options: --with-compiler"
AC_ARG_WITH([compiler], [GHC])

# -----------------------------------------------------------------------------
# Linux: check for inotify constants
# -----------------------------------------------------------------------------

# XXX Do this only for linux
AC_CHECK_DECLS([IN_MASK_CREATE],[],[],[#include <sys/inotify.h>])
AC_CHECK_DECLS([IN_EXCL_UNLINK],[],[],[#include <sys/inotify.h>])

# -----------------------------------------------------------------------------
# macOS: check for FSEvents constants
# -----------------------------------------------------------------------------

AC_DEFUN([AC_CHECK_FSEVENT_CONST],
[
  AC_MSG_CHECKING([for $1 usability])
  AC_LINK_IFELSE(
    [AC_LANG_PROGRAM(
      [[#include <CoreServices/CoreServices.h>]],
      [[unsigned int x = $1; return (x == 0);]]
    )],
    [AC_DEFINE([HAVE_DECL_]m4_toupper([$1]), [1], [Define to 1 if $1 is usable])
    AC_MSG_RESULT([yes])],
    [AC_DEFINE([HAVE_DECL_]m4_toupper([$1]), [0], [Define to 0 if $1 is unusable])
    AC_MSG_RESULT([no])]
  )
])

AC_CHECK_FSEVENT_CONST([kFSEventStreamCreateFlagFileEvents])
AC_CHECK_FSEVENT_CONST([kFSEventStreamCreateFlagFullHistory])
AC_CHECK_FSEVENT_CONST([kFSEventStreamEventFlagItemCloned])
AC_CHECK_FSEVENT_CONST([kFSEventStreamEventFlagItemIsHardlink])

# -----------------------------------------------------------------------------
# Output
# -----------------------------------------------------------------------------
AC_CONFIG_HEADERS([src/config.h])
AC_OUTPUT
