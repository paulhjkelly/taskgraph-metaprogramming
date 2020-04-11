/*  Extensible Array Definitions */

/*  Copyright (c) 1994 Stanford University

    All rights reserved.

    This software is provided under the terms described in
    the "suif_copyright.h" include file. */

#include <suif_copyright.h>

#ifndef XARRAY_H
#define XARRAY_H

#pragma interface

RCS_HEADER(xarray_h,
    "$Id$")


/*
 *  An x_array is an extensible array of void* objects.  An initial size is
 *  specified when the array is created, but the array elements cannot be
 *  referenced until they are initialized using the "extend" method.  In this
 *  way an x_array behaves like a list -- the array elements must be appended
 *  (with "extend") to the end of the array before they can be used.  If the
 *  number of elements exceed the initial size additional chunks of data are
 *  automatically allocated.  The "ub" method returns the number of elements
 *  that are currently in the array.
 */

class x_array {
private:
    /* We make explicit copy constructor and assignment operator and
     * make them private to foil C++'s automatic default versions. */
    x_array(const x_array &);
    x_array &operator=(const x_array &);

    int size;
    int hi;
    void **data;
    x_array *next;

public:
    x_array(int sz);
    ~x_array();

    void *& operator[](int i);
    int extend(void *e);
    int ub();
};


/*
 *  The following macro definition is used to automatically generate
 *  subclasses of x_array that contain pointers to various data types.
 *  This is similar to the macros for declaring new list classes.
 */

#define DECLARE_X_ARRAY(CLASS, PTRTYPE, DEFAULT_SIZE)			      \
class CLASS: public x_array						      \
{									      \
public:									      \
    CLASS(int sz=DEFAULT_SIZE) : x_array(sz) { }			      \
    ~CLASS() { }							      \
    PTRTYPE *&operator[](int i)						      \
      { return (PTRTYPE *&)(x_array::operator[](i)); }			      \
}

#endif /* XARRAY_H */
