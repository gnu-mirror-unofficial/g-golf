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

#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <limits.h>
#include <float.h>
#include <math.h>

#include <glib.h>
#include <glib-object.h>
/* #include <girepository.h> */

/*
 * make distcheck fails, I'll solve this later
 * #include "libg-golf.h"
 *
 */


/*
 * misc.
 *
*/

size_t
pointer_address_size ()
{
  size_t n = sizeof(float *) * CHAR_BIT;

  return n;
}


/*
 * floats
 *
*/

int
float_to_int (float f)
{
  int i;

  i = (int)f;
  return (i);
}


/*
 * Glib
 *
*/


/*
 * GObject
 *
*/

size_t
g_value_size ()
{
  size_t n = sizeof(GValue);

  return n;
}

GType
g_object_type (GObject *obj)
{
    GType type;

    type = G_OBJECT_TYPE (obj);

    return (type);
}

const gchar *
g_object_type_name (GObject *obj)
{
    const gchar *name;

    name = G_OBJECT_TYPE_NAME (obj);

    return (name);
}

uint
g_object_ref_count (GObject *obj)
{
    return (obj->ref_count);
}

size_t
g_closure_size ()
{
  size_t n = sizeof(GClosure);

  return n;
}
