
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


TESTS = \
	tests/support.scm	\
	tests/glib.scm		\
	tests/gobject.scm	\
	tests/gi.scm		\
	tests/hl-api.scm	\
	tests/override.scm

TEST_EXTENSIONS = .scm

TESTS_ENVIRONMENT = \
	$(abs_top_builddir)/pre-inst-env		\
	$(GUILE) -L $(abs_top_builddir)/test-suite	\
		 --no-auto-compile

EXTRA_DIST = \
	$(TESTS)
