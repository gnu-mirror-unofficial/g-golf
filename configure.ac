dnl -*- Autoconf -*-
dnl
dnl   configuration script for GNU G-Golf
dnl   Process this file with autoconf to produce configure.
dnl

define(G_GOLF_CONFIGURE_COPYRIGHT,[[

Copyright (C) 2016 - 2020
Free Software Foundation, Inc.

This file is part of GNU G-Golf

GNU G-Golf is free software; you can redistribute it and/or modify
it under the terms of the GNU Lesser General Public License as
published by the Free Software Foundation; either version 3 of the
License, or (at your option) any later version.

GNU G-Golf is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with GNU G-Golf.  If not, see
<https://www.gnu.org/licenses/lgpl.html>.

]])

AC_PREREQ(2.69)

AC_INIT(
  [g-golf],
  [0.1.0],
  [bug-g-golf@gnu.org])

AC_CONFIG_AUX_DIR([build-aux])
AC_CONFIG_MACRO_DIR([m4])
dnl Use `serial-tests' so the output `check-guile' is not hidden
dnl (`parallel-tests' is the default in Automake 1.13.)
dnl `serial-tests' was introduced in Automake 1.12.
AM_INIT_AUTOMAKE([1.14 gnu -Wall -Werror -Wno-portability -Wno-syntax \
  color-tests serial-tests foreign])
AM_MAINTAINER_MODE([enable])
AM_SILENT_RULES([yes])

AC_PROG_CC
AC_STDC_HEADERS
LT_INIT([disable-static])

AC_COPYRIGHT(G_GOLF_CONFIGURE_COPYRIGHT)
AC_CONFIG_SRCDIR([g-golf])

MAKEINFO_PROG([6.6])

GUILE_PKG([2.2 2.0])
GUILE_PROGS([2.0.14])
GUILE_SITE_DIR
GUILE_GLOBAL_SITE_DIR
GUILE_SITE_CCACHE_DIR
GUILE_FLAGS


AC_ARG_WITH(
  [guile-site],
  [AS_HELP_STRING(
     [--with-guile-site],
     [install source modules and compiled files using Guile's site and site-ccache dirs])],
  [case "x$withval" in
     xyes)
     guile_site="yes";;
     *)
     guile_site="no";;
   esac],
  [guile_site="no"])
AC_SUBST([guile_site])

if test "x$guile_site" = "xyes"; then
   SITEDIR="$GUILE_GLOBAL_SITE";
   SITECCACHEDIR="$GUILE_SITE_CCACHE";
else
   SITEDIR="$datadir/g-golf";
   SITECCACHEDIR="$libdir/g-golf/guile/$GUILE_EFFECTIVE_VERSION/site-ccache";
fi
AC_SUBST([SITEDIR])
AC_SUBST([SITECCACHEDIR])


PKG_CHECK_MODULES(GI, gobject-introspection-1.0 >= 1.48.0)

PKG_CHECK_MODULES(GLIB, glib-2.0 >= 2.48.0)
AC_SUBST(GLIB_CFLAGS)
AC_SUBST(GLIB_LIBS)

PKG_CHECK_MODULES(GOBJECT, gobject-2.0 >= 2.48.0)
AC_SUBST(GOBJECT_CFLAGS)
AC_SUBST(GOBJECT_LIBS)

PKG_CHECK_MODULES(GTK, gtk+-3.0 >= 3.24.0)
AC_SUBST(GTK_CFLAGS)
AC_SUBST(GTK_LIBS)

PKG_CHECK_MODULES(GUILE_LIB, guile-lib-1.0 >= 0.2.5)


AC_CONFIG_FILES(
  [pre-inst-env],
  [chmod +x pre-inst-env])

AC_CONFIG_FILES([
  meta/g-golf-1.0.pc
  Makefile
  libg-golf/Makefile
  doc/Makefile
  test-suite/Makefile
])

AC_OUTPUT


AC_MSG_NOTICE(
  []
  []
  [prefix=$prefix]
  [exec_prefix=$exec_prefix]
  [libdir=$libdir]
  [datarootdir=$datarootdir]
  [datadir=$datadir]
  []
  [--with-guile-site="$guile_site"]
  []
  [sitedir (source code): ${SITEDIR}]
  [siteccachedir (compiled modules): ${SITECCACHEDIR}]
  [])

if test "x$guile_site" = "xno"; then
  AC_MSG_WARN(
    []
    []
    [Since --with-guile-site="no", sitedir and siteccachedir differ from]
    [the one used by Guile itself. You have to augment both Guile's %load-path]
    [and %load-compiled-path variables before to use Guile-CV's modules (see]
    ['Environment Variables' and 'Load Paths' in the Guile Reference Manual).]
    []
    [Otherwise, consider using --with-guile-site=yes])
fi
