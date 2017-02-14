AC_INIT([pubstrate], [0.1.dev], [cwebber@dustycloud.org])

PKG_CHECK_MODULES([GUILE], [guile-2.2])

AM_INIT_AUTOMAKE([-Wall -Werror foreign])

AC_CONFIG_MACRO_DIR([m4])

GUILE_PROGS

AC_CONFIG_FILES([Makefile pubstrate/package-config.scm])
AC_CONFIG_FILES([scripts/pubstrate-web], [chmod +x scripts/pubstrate-web])
AC_CONFIG_FILES([env], [chmod +x env])
AC_CONFIG_FILES([pre-inst-env], [chmod +x pre-inst-env])

dnl Prepare a version of $datadir that does not contain references to
dnl shell variables.  (Borrowed from Sly, which borrowed from Guix...)
pubstrate_prefix="`eval echo $prefix | sed -e"s|NONE|/usr/local|g"`"
pubstrate_datadir="`eval eval echo $datadir | sed -e "s|NONE|$pubstrate_prefix|g"`"
AC_SUBST([pubstrate_datadir])


dnl Borrowed from guix: check for / find libgcrypt
dnl ==============================================

LIBGCRYPT="libgcrypt"
LIBGCRYPT_LIBDIR="no"
LIBGCRYPT_PREFIX="no"

AC_ARG_WITH([libgcrypt-prefix],
  [AS_HELP_STRING([--with-libgcrypt-prefix=DIR], [search for GNU libgcrypt in DIR])],
  [case "$withval" in
    yes|no)
      ;;
    *)
      LIBGCRYPT="$withval/lib/libgcrypt"
      LIBGCRYPT_PREFIX="$withval"
      LIBGCRYPT_LIBDIR="$withval/lib"
      ;;
   esac])

AC_ARG_WITH([libgcrypt-libdir],
  [AS_HELP_STRING([--with-libgcrypt-libdir=DIR],
     [search for GNU libgcrypt's shared library in DIR])],
  [case "$withval" in
    yes|no)
      LIBGCRYPT="libgcrypt"
      LIBGCRYPT_LIBDIR="no"
      ;;
    *)
      LIBGCRYPT="$withval/libgcrypt"
      LIBGCRYPT_LIBDIR="$withval"
      ;;
   esac])

dnl If none of the --with-libgcrypt-* options was used, try to determine the
dnl absolute file name of libgcrypt.so.
case "x$LIBGCRYPT_PREFIX$LIBGCRYPT_LIBDIR" in
  xnono)
    GUIX_LIBGCRYPT_LIBDIR([LIBGCRYPT_LIBDIR])
    if test "x$LIBGCRYPT_LIBDIR" != x; then
      LIBGCRYPT="$LIBGCRYPT_LIBDIR/libgcrypt"
    else
      dnl 'config-daemon.ac' expects "no" in this case.
      LIBGCRYPT_LIBDIR="no"
    fi
    ;;
esac

dnl Library name suitable for `dynamic-link'.
AC_MSG_CHECKING([for libgcrypt shared library name])
AC_MSG_RESULT([$LIBGCRYPT])
AC_SUBST([LIBGCRYPT])
AC_SUBST([LIBGCRYPT_PREFIX])
AC_SUBST([LIBGCRYPT_LIBDIR])

GUIX_ASSERT_LIBGCRYPT_USABLE

AC_OUTPUT
