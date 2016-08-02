AC_INIT([pubstrate], [0.1.dev], [cwebber@dustycloud.org])

PKG_CHECK_MODULES([GUILE], [guile-2.0])

AM_INIT_AUTOMAKE([-Wall -Werror foreign])

GUILE_PROGS

AC_CONFIG_FILES([Makefile pubstrate/paths.scm])
AC_CONFIG_FILES([env], [chmod +x env])
AC_CONFIG_FILES([pre-inst-env], [chmod +x pre-inst-env])

# Prepare a version of $datadir that does not contain references to
# shell variables.  (Borrowed from Sly, which borrowed from Guix...)
pubstrate_prefix="`eval echo $prefix | sed -e"s|NONE|/usr/local|g"`"
pubstrate_datadir="`eval eval echo $datadir | sed -e "s|NONE|$pubstrate_prefix|g"`"
AC_SUBST([pubstrate_datadir])


AC_OUTPUT
