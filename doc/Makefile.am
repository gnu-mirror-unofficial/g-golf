
####
#### Copyright (C) 2016 - 2021
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


# AUTOMAKE_OPTIONS = gnu

info_TEXINFOS = g-golf.texi

DOC_IMAGES = \
	$(wildcard images/*)

DOC_CSS = \
	$(wildcard css/*)

g_golf_TEXINFOS = \
  version.texi			\
  variables.texi		\
  preface.texi     		\
  introduction.texi		\
  ug.texi			\
  ug-bys.texi			\
  merging-generics.texi		\
  ug-bys-naming-conventions.texi		\
  ug-bys-goops-notes-and-conventions.texi	\
  ug-bys-configuring-guile-for-g-golf.texi	\
  ug-bys-customizing-g-golf.texi		\
  ug-g-golf-cache.texi		\
  ug-gswg.texi			\
  ug-gswg-hello-world.texi	\
  ug-gswg-selective-import.texi	\
  ug-gswg-scripting.texi	\
  ug-wwg-import.texi		\
  ug-wwg-events.texi		\
  ug-wwg-gobject.texi		\
  ug-wwg.texi			\
  cr.texi			\
  cr-overview.texi		\
  cr-ov-structure-and-naming-conventions.texi	\
  cr-glib.texi			\
  cr-glib-mem-alloc.texi	\
  cr-glib-main-event-loop.texi	\
  cr-glib-io-channels.texi	\
  cr-glib-misc-util-funcs.texi	\
  cr-glib-unix-utils.texi	\
  cr-glib-glist.texi		\
  cr-glib-gslist.texi		\
  cr-glib-quarks.texi		\
  cr-gobject.texi		\
  cr-gobject-type-info.texi	\
  cr-gobject-gobject.texi	\
  cr-gobject-enum-flags.texi	\
  cr-gobject-boxed-types.texi	\
  cr-gobject-gen-vals.texi	\
  cr-gobject-params-vals.texi	\
  cr-gobject-param-spec.texi	\
  cr-gobject-closures.texi	\
  cr-gobject-signals.texi	\
  cr-gi.texi			\
  cr-gi-repository.texi		\
  cr-gi-common-types.texi	\
  cr-gi-base-info.texi		\
  cr-gi-callable-info.texi	\
  cr-gi-signal-info.texi	\
  cr-gi-function-info.texi	\
  cr-gi-registered-type-info.texi	\
  cr-gi-enum-info.texi		\
  cr-gi-struct-info.texi	\
  cr-gi-union-info.texi		\
  cr-gi-object-info.texi	\
  cr-gi-interface-info.texi	\
  cr-gi-arg-info.texi		\
  cr-gi-constant-info.texi	\
  cr-gi-field-info.texi		\
  cr-gi-property-info.texi	\
  cr-gi-type-info.texi		\
  cr-gi-typelib.texi		\
  cr-gi-utilities.texi		\
  cr-support.texi		\
  cr-support-module.texi	\
  cr-support-goops.texi		\
  cr-support-enum.texi		\
  cr-support-flags.texi		\
  cr-support-struct.texi	\
  cr-support-union.texi		\
  cr-support-utils.texi		\
  cr-hl-api.texi		\
  cr-hl-api-closure.texi	\
  cr-hl-api-function.texi	\
  cr-hl-api-import.texi		\
  cr-hl-api-utils.texi		\
  indices.texi			\
  fdl.texi			\
  $(DOC_IMAGES)			\
  $(DOC_CSS)

nobase_doc_DATA = \
	$(DOC_IMAGES)		\
	$(DOC_CSS)

EXTRA_DIST = \
	$(DOC_IMAGES)		\
	$(DOC_CSS)

AM_MAKEINFOHTMLFLAGS = \
	-c DOCTYPE='<!DOCTYPE html>' --css-ref=../css/manual.css
