
####
#### Copyright (C) 2016 - 2020
#### Free Software Foundation, Inc.

#### This file is part of GNU G-Golf

#### GNU G-Golf is free software; you can redistribute it and/or modify
#### it under the terms of the GNU Lesser General Public License as
#### published by the Free Software Foundation; either version 3 of the
#### License, or (at your option) any later version.

#### GNU G-Golf is distributed in the hope that it will be useful, but
#### WITHOUT ANY WARRANTY; without even the implied warranty of
#### MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
#### Lesser General Public License for more details.

#### You should have received a copy of the GNU Lesser General Public
#### License along with GNU G-Golf.  If not, see
#### <https://www.gnu.org/licenses/lgpl.html>.
####


include $(top_srcdir)/am/guile.mk


pkgconfigdir=$(libdir)/pkgconfig
pkgconfig_DATA=meta/g-golf-1.0.pc

SOURCES = \
	g-golf/support/libg-golf.scm		\
	g-golf/support/float.scm		\
	g-golf/support/module.scm		\
	g-golf/support/goops.scm		\
	g-golf/support/g-export.scm		\
	g-golf/support/utils.scm		\
	g-golf/support/push.scm			\
	g-golf/support/keyword.scm		\
	g-golf/support/enum.scm			\
	g-golf/support/flag.scm			\
	g-golf/support/struct.scm		\
	g-golf/support/union.scm		\
	g-golf/support/bytevector.scm		\
	g-golf/support.scm			\
	g-golf/init.scm				\
	g-golf/gdk/events.scm			\
	g-golf/gdk/key-values.scm		\
	g-golf/gdk.scm				\
	g-golf/glib/mem-alloc.scm		\
	g-golf/glib/main-event-loop.scm		\
	g-golf/glib/io-channels.scm		\
	g-golf/glib/unix-utils.scm		\
	g-golf/glib/glist.scm			\
	g-golf/glib/gslist.scm			\
	g-golf/glib/quarks.scm			\
	g-golf/glib.scm				\
	g-golf/gobject/type-info.scm		\
	g-golf/gobject/gobject.scm		\
	g-golf/gobject/enum-flags.scm		\
	g-golf/gobject/generic-values.scm	\
	g-golf/gobject/params-vals.scm		\
	g-golf/gobject/param-spec.scm		\
	g-golf/gobject/closures.scm		\
	g-golf/gobject/signals.scm		\
	g-golf/gobject.scm			\
	g-golf/gi/utils.scm			\
	g-golf/gi/cache.scm			\
	g-golf/gi/repository.scm		\
	g-golf/gi/common-types.scm		\
	g-golf/gi/base-info.scm			\
	g-golf/gi/callable-info.scm		\
	g-golf/gi/signal-info.scm		\
	g-golf/gi/function-info.scm		\
	g-golf/gi/registered-type-info.scm	\
	g-golf/gi/enum-info.scm			\
	g-golf/gi/struct-info.scm		\
	g-golf/gi/union-info.scm		\
	g-golf/gi/object-info.scm		\
	g-golf/gi/interface-info.scm		\
	g-golf/gi/arg-info.scm			\
	g-golf/gi/constant-info.scm		\
	g-golf/gi/field-info.scm		\
	g-golf/gi/property-info.scm		\
	g-golf/gi/type-info.scm			\
	g-golf/gi/typelib.scm			\
	g-golf/gi.scm				\
	g-golf/override/override.scm		\
	g-golf/override/gtk.scm			\
	g-golf/override.scm			\
	g-golf/hl-api/gtype.scm			\
	g-golf/hl-api/gobject.scm		\
	g-golf/hl-api/function.scm		\
	g-golf/hl-api/object.scm		\
	g-golf/hl-api/closure.scm		\
	g-golf/hl-api/signal.scm		\
	g-golf/hl-api/glib.scm			\
	g-golf/hl-api/callback.scm		\
	g-golf/hl-api/import.scm		\
	g-golf/hl-api/utils.scm			\
	g-golf/hl-api.scm			\
	g-golf.scm

# NOCOMP_SOURCES= \
# 	$(wildcard g-golf/glade/*)	\
# 	$(wildcard g-golf/icons/*)	\
# 	$(wildcard g-golf/latex/*)	\
# 	$(wildcard g-golf/gtkrc/*)	\
# 	$(shell find $(top_srcdir)/g-golf/pof -type f)

EXTRA_DIST = \
	AUTHORS				\
	ChangeLog			\
	COPYING				\
	INSTALL				\
	LICENSE				\
	NEWS				\
	README				\
	pre-inst-env.in			\
	meta/g-golf-1.0.pc.in		\
	$(wildcard am/*)		\
	$(wildcard m4/*)		\
	$(SOURCES)
# 	$(NOCOMP_SOURCES)

# bin_SCRIPTS = g-golf/g-golf

CLEANFILES = \
	meta/g-golf-1.0.pc		\
	$(GOBJECTS)
# 	$(bin_SCRIPTS)

ETAGS_ARGS = \
	$(SOURCES)
# g-golf

SUBDIRS = \
	libg-golf			\
	test-suite			\
	doc


###
### Removing old mod and go files if any
###

SOURCES_TO_REMOVE = \
	g-golf/gdk/event-structures.scm

GOBJECTS_TO_REMOVE = $(SOURCES_TO_REMOVE:%.scm=%.go)

install-data-local:
	for f in $(SOURCES_TO_REMOVE); do	\
          if [ -e $(moddir)/$$f ]; then		\
	    rm $(moddir)/$$f;			\
	  fi;					\
	done
	for f in $(GOBJECTS_TO_REMOVE); do 	\
	  if [ -e $(godir)/$$f ]; then		\
	    rm $(godir)/$$f;			\
	  fi;					\
	done

uninstall-local:
	for f in $(SOURCES_TO_REMOVE); do	\
          if [ -e $(moddir)/$$f ]; then		\
	    rm $(moddir)/$$f;			\
	  fi;					\
	done
	for f in $(GOBJECTS_TO_REMOVE); do 	\
	  if [ -e $(godir)/$$f ]; then		\
	    rm $(godir)/$$f;			\
	  fi;					\
	done

install-html-local:
	htmldir="$(datadir)/doc/@PACKAGE@/@PACKAGE@.html";	\
	if ! [[ -e $$htmldir ]]; then				\
	      mkdir -p "$$htmldir";				\
	      cd "$$htmldir";					\
	      ln -s ../images .;				\
	else							\
	      cd "$$htmldir";					\
	      if ! [[ -L images ]]; then			\
	            ln -s ../images .;				\
	      fi;						\
	fi


###
### Distclean
###

distclean-local:
	rm -f Makefile
	rm -f Makefile.in
	rm -f aclocal.m4
	rm -rf autom4te.cache
	rm -rf build-aux
	rm -f *~
	rm -f g-golf-*.tar.gz
	rm -f g-golf-*.tar.gz.sig
	rm -f libg-golf/*~
	rm -f libg-golf/Makefile
	rm -f libg-golf/Makefile.in
	rm -f libg-golf/libg-golf.la
	rm -f libg-golf/libg-golf.lo
	rm -f libg-golf/libg-golf.so
	rm -f libg-golf/libg_golf_la-libg-golf.lo
	rm -rf doc/g-golf
	rm -f doc/*.info
	rm -f doc/*.pdf
	rm -f doc/*~
	rm -f doc/*.aux doc/*.cp doc/*.cps doc/*.fn doc/*.fns doc/*.ky
	rm -f doc/*.log doc/*.pg doc/*.toc doc/*.tp doc/*.vr doc/*.vrs
	rm -f doc/Makefile
	rm -f doc/Makefile.in
	rm -f doc/stamp-vti
	rm -f g-golf/TAGS
	rm -f g-golf/*~
	rm -f test-suite/Makefile
	rm -f test-suite/Makefile.in
	rm -f m4/libtool.m4
	rm -f m4/ltoptions.m4
	rm -f m4/ltsugar.m4
	rm -f m4/ltversion.m4
	rm -f m4/lt~obsolete.m4
	rm -f m4/*~


ACLOCAL_AMFLAGS = -I m4

# * .PHONY: gen-pof:

# Makefile.am ends here
