
####
#### Copyright (C) 2016 - 2020
#### Free Software Foundation, Inc.

#### This file is part of GNU G-Golf.

#### GNU G-Golf is free software; you can redistribute it and/or
#### modify it under the terms of the GNU General Public License as
#### published by the Free Software Foundation; either version 3 of
#### the License, or (at your option) any later version.

#### GNU G-Golf is distributed in the hope that it will be useful, but
#### WITHOUT ANY WARRANTY; without even the implied warranty of
#### MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
#### General Public License for more details.

#### You should have received a copy of the GNU General Public License
#### along with GNU G-Golf.  If not, see
#### <https://www.gnu.org/licenses/gpl.html>.
####


#include $(top_srcdir)/am/guile.mk


lib_LTLIBRARIES = \
	libg-golf.la


nodist_libg_golf_SOURCES = \
	libg-golf.c

libg_golf_SOURCES = \
	libg-golf.c		\
	libg-golf.h

# libg_golf_la_CFLAGS = \
#	$(AM_CFLAGS) $(GUILE_CFLAGS) $(GLIB_CFLAGS) $(GOBJECT_CFLAGS) $(GTK_CFLAGS)

# libg_golf_la_LIBADD = \
# 	$(GUILE_LIBS) $(GLIB_LIBS) $(GOBJECT_LIBS) $(GTK_LIBS)

libg_golf_la_CFLAGS = \
	$(AM_CFLAGS) $(GUILE_CFLAGS) $(GLIB_CFLAGS) $(GOBJECT_CFLAGS)

libg_golf_la_LIBADD = \
	$(GUILE_LIBS) $(GLIB_LIBS) $(GOBJECT_LIBS)

libg_golf_la_LDFLAGS = \
	 $(GUILE_LDFLAGS)

printenv:
	printf '$(libg_golf_la_CFLAGS)\n' \
	printf '$(libg_golf_la_LIBADD)\n'   \
	printf '$(libg_golf_la_LDFLAGS)\n'

# Makefile.am ends here
