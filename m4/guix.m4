dnl GNU Guix --- Functional package management for GNU
dnl Copyright © 2012, 2013, 2014, 2015, 2016 Ludovic Courtès <ludo@gnu.org>
dnl Copyright © 2014 Mark H Weaver <mhw@netris.org>
dnl
dnl This file is part of GNU Guix.
dnl
dnl GNU Guix is free software; you can redistribute it and/or modify it
dnl under the terms of the GNU General Public License as published by
dnl the Free Software Foundation; either version 3 of the License, or (at
dnl your option) any later version.
dnl
dnl GNU Guix is distributed in the hope that it will be useful, but
dnl WITHOUT ANY WARRANTY; without even the implied warranty of
dnl MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
dnl GNU General Public License for more details.
dnl
dnl You should have received a copy of the GNU General Public License
dnl along with GNU Guix.  If not, see <http://www.gnu.org/licenses/>.

dnl GUIX_ASSERT_LIBGCRYPT_USABLE
dnl
dnl Assert that GNU libgcrypt is usable from Guile.
AC_DEFUN([GUIX_ASSERT_LIBGCRYPT_USABLE],
  [AC_CACHE_CHECK([whether $LIBGCRYPT can be dynamically loaded],
    [guix_cv_libgcrypt_usable_p],
    [GUILE_CHECK([retval],
      [(dynamic-func \"gcry_md_hash_buffer\" (dynamic-link \"$LIBGCRYPT\"))])
     if test "$retval" = 0; then
       guix_cv_libgcrypt_usable_p="yes"
     else
       guix_cv_libgcrypt_usable_p="no"
     fi])

   if test "x$guix_cv_libgcrypt_usable_p" != "xyes"; then
     AC_MSG_ERROR([GNU libgcrypt does not appear to be usable; see `--with-libgcrypt-prefix' and `README'.])
   fi])

dnl GUIX_LIBGCRYPT_LIBDIR VAR
dnl
dnl Attempt to determine libgcrypt's LIBDIR; store the result in VAR.
AC_DEFUN([GUIX_LIBGCRYPT_LIBDIR], [
  AC_PATH_PROG([LIBGCRYPT_CONFIG], [libgcrypt-config])
  AC_CACHE_CHECK([libgcrypt's library directory],
    [guix_cv_libgcrypt_libdir],
    [if test "x$LIBGCRYPT_CONFIG" != "x"; then
       guix_cv_libgcrypt_libdir=`$LIBGCRYPT_CONFIG --libs | grep -e -L | sed -e "s/.*-L\([[^ ]]\+\)[[[:blank:]]]\+-lgcrypt.*/\1/g"`
     else
       guix_cv_libgcrypt_libdir=""
     fi])
  $1="$guix_cv_libgcrypt_libdir"
])
