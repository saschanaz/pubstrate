## Copyright (C) 2015 Christopher Allan Webber <cwebber@dustycloud.org>

## parts of this automake recipe borrowed from:

## GNU Guix --- Functional package management for GNU
## Copyright © 2012, 2013, 2014, 2015 Ludovic Courtès <ludo@gnu.org>
## Copyright © 2013 Andreas Enge <andreas@enge.fr>
## Copyright © 2015 Alex Kost <alezost@gmail.com>

## Sly
## Copyright (C) 2013, 2014 David Thompson <dthompson2@worcester.edu>

## This program is free software: you can redistribute it and/or
## modify it under the terms of the GNU General Public License as
## published by the Free Software Foundation, either version 3 of the
## License, or (at your option) any later version.
##
## This program is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with this program.  If not, see
## <http://www.gnu.org/licenses/>.

GOBJECTS = $(SOURCES:%.scm=%.go)
nobase_mod_DATA = $(SOURCES)
nobase_go_DATA = $(GOBJECTS)

# Make sure source files are installed first, so that the mtime of
# installed compiled files is greater than that of installed source
# files.  See
# <http://lists.gnu.org/archive/html/guile-devel/2010-07/msg00125.html>
# for details.
guile_install_go_files = install-nobase_goDATA
$(guile_install_go_files): install-nobase_modDATA

GUILE_WARNINGS = -Wunbound-variable -Warity-mismatch -Wformat
SUFFIXES = .scm .go
.scm.go:
	$(AM_V_GEN)$(top_builddir)/pre-inst-env $(GUILE_TOOLS) compile $(GUILE_WARNINGS) -o "$@" "$<"

moddir=$(prefix)/share/guile/site/2.0
godir=$(libdir)/guile/2.0/ccache

SOURCES =  \
	activitystuff/activities.scm			\
	activitystuff/json-utils.scm			\
	activitystuff/contrib/json.scm

TESTS =							\
	tests/test-json-ld.scm

TEST_EXTENSIONS = .scm

SCM_LOG_COMPILER = $(GUILE)
AM_SCM_LOG_FLAGS = --no-auto-compile -L $(top_srcdir)

CLEANFILES = 						\
	$(GOBJECTS)					\
	$(TESTS:tests/%.scm=%.log)			\
	*.log *.tar.gz


EXTRA_DIST = 						\
	$(SOURCES)					\
	$(TESTS)					\
	tests/utils.scm					\
	pre-inst-env.in