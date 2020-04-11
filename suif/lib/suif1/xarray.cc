/*  Extensible Array Implementation */

/*  Copyright (c) 1994 Stanford University

    All rights reserved.

    This software is provided under the terms described in
    the "suif_copyright.h" include file. */

#include <suif_copyright.h>

#define _MODULE_ "libsuif.a"

#pragma implementation "xarray.h"

#define RCS_BASE_FILE xarray_cc

#include "misc.h"
#include "xarray.h"

RCS_BASE(
    "$Id$")


// private copy constructor and assignment op
x_array::x_array(const x_array &)  { assert(FALSE); }
x_array &x_array::operator=(const x_array &)  { assert(FALSE); return *this; }

x_array::x_array (int sz)
    : size(sz), hi(0), data(new void *[sz]), next(0)
{
    /* set the "hi" counter to show that there are no entries in the array */
}


x_array::~x_array ()
{
    if (next) delete next;
    delete [] data;
}


/*
 *  Reference an element of the array.  The element must have already been
 *  appended to the end of the array using the "extend" method.
 */

void *&
x_array::operator[] (int i)
{
    assert_msg(i >= 0, ("x_array::[] - index < 0"));
    if (i < hi) return data[i];
    assert_msg(next != NULL, ("x_array::[] - next field is NULL"));
    return (*next)[i - hi];
}


/*
 *  Append an element to the end of the array and update the element count.
 *  If necessary, allocate an additional chunk of data.
 */

int
x_array::extend (void *e)
{
    if (hi >= size) {
	if (!next) next = new x_array(size);
	return next->extend(e) + hi;
    }
    data[hi] = e;
    hi++;
    return hi - 1;
}


/*
 *  Return the number of elements that are currently in the array.
 */

int
x_array::ub ()
{
    return next ? next->ub() + hi : hi;
}


