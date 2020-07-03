
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


# AUTOMAKE_OPTIONS = gnu

info_TEXINFOS = g-golf.texi

DOC_CSS = \
	$(wildcard css/*)

g_golf_TEXINFOS = \
  version.texi			\
  variables.texi		\
  preface.texi     		\
  introduction.texi		\
  ug.texi			\
  ug-bys.texi			\
  ug-bys-naming-conventions.texi		\
  ug-bys-goops-notes-and-conventions.texi	\
  cr.texi			\
  cr-overview.texi		\
  cr-ov-project-and-doc-struct.texi \
  cr-glib.texi			\
  cr-glib-mem-alloc.texi	\
  cr-glib-main-event-loop.texi \
  cr-glib-io-channels.texi	\
  cr-glib-unix-utils.texi	\
  cr-glib-glist.texi		\
  cr-glib-gslist.texi		\
  cr-glib-quarks.texi		\
  cr-gobject.texi		\
  cr-gobject-type-info.texi	\
  cr-gobject-gobject.texi	\
  cr-gobject-enum-flags.texi	\
  cr-gobject-gen-vals.texi	\
  cr-gobject-params-vals.texi  \
  cr-gobject-param-spec.texi	\
  cr-gobject-closures.texi	\
  cr-gobject-signals.texi	\
  cr-gdk.texi			\
  cr-gdk-events.texi		\
  cr-gdk-key-values.texi	\
  cr-gir.texi			\
  cr-gir-repository.texi	\
  cr-gir-common-types.texi	\
  cr-gir-base-info.texi		\
  cr-gir-callable-info.texi	\
  cr-gir-signal-info.texi	\
  cr-gir-function-info.texi	\
  cr-gir-registered-type-info.texi	\
  cr-gir-enum-info.texi		\
  cr-gir-struct-info.texi	\
  cr-gir-union-info.texi	\
  cr-gir-object-info.texi	\
  cr-gir-interface-info.texi	\
  cr-gir-arg-info.texi		\
  cr-gir-constant-info.texi	\
  cr-gir-field-info.texi	\
  cr-gir-property-info.texi	\
  cr-gir-type-info.texi		\
  cr-gir-typelib.texi		\
  cr-gir-utilities.texi		\
  cr-support.texi		\
  cr-support-module.texi	\
  cr-support-goops.texi		\
  cr-support-enum.texi		\
  cr-support-flag.texi		\
  cr-support-struct.texi	\
  cr-support-union.texi		\
  cr-support-utils.texi		\
  cr-hl-api.texi		\
  cr-hl-api-events.texi		\
  cr-hl-api-gtype.texi		\
  cr-hl-api-gobject.texi	\
  cr-hl-api-closure.texi	\
  cr-hl-api-function.texi	\
  cr-hl-api-object.texi		\
  cr-hl-api-import.texi		\
  cr-hl-api-utils.texi		\
  indices.texi			\
  fdl.texi			\
  $(DOC_CSS)

nobase_doc_DATA = \
	$(DOC_CSS)

EXTRA_DIST = \
	$(DOC_CSS)

AM_MAKEINFOHTMLFLAGS = \
	-c DOCTYPE='<!DOCTYPE html>' --css-ref=../css/manual.css
