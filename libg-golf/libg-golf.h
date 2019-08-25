/*  -*- mode: C; coding: utf-8 -*-

####
#### Copyright (C) 2016 - 2019
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

size_t pointer_address_size_c ();


/*
 * floats
 *
*/

int float_to_int_c (float f);


/*
 * GObject
 *
*/

GType g_object_type_c (GObject *obj);
const gchar * g_object_type_name_c (GObject *obj);
uint g_object_ref_count_c (GObject *obj);
