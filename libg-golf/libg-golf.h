/*  -*- mode: C; coding: utf-8 -*-

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

*/


/*
 * misc.
 *
*/

size_t
pointer_address_size ();


/*
 * floats
 *
*/

int
float_to_int (float f);


/*
 * Glib
 *
*/

uint
g_source_ref_count (GSource *source);


/*
 * GObject
 *
*/

size_t
g_value_size ();

GType
g_object_type (GObject *obj);

const gchar*
g_object_type_name (GObject *obj);

uint
g_object_ref_count (GObject *obj);

size_t
g_closure_size ();

uint
g_closure_ref_count (GClosure *closure);


/*
 * Gdk
 *
*/

GdkWindowState
gdk_event_get_changed_mask (GdkEvent *event);

GdkWindowState
gdk_event_get_new_window_state (GdkEvent *event);


/*
 * Test suite
 *
*/

char**
test_suite_n_string_ptr ();

char**
test_suite_strings_ptr ();
