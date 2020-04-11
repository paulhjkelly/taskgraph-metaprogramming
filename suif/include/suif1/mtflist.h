/*  Move-to-Front List Definitions */

/*  Copyright (c) 1994 Stanford University

    All rights reserved.

    This software is provided under the terms described in
    the "suif_copyright.h" include file. */

#include <suif_copyright.h>

#ifndef MTFLIST_H
#define MTFLIST_H

#pragma interface

RCS_HEADER(mtflist_h,
    "$Id$")

/*
 *  This class is just like glist with the additional feature of a
 *  search function on the list.  The user must provide a function that
 *  compares a glist_e with some other unspecified object.  After every
 *  successful lookup, the returned element will be moved to the front
 *  of the list.  For many applications, this provides pretty good
 *  performance.
 */

typedef boolean (*mtflist_test_f)(glist_e *e, void *k);

class mtflist : public glist {
public:
    glist_e *get(mtflist_test_f check, void *key);
    glist_e *lookup(mtflist_test_f check, void *k);
};


/*
 *  The following macro definitions are used to automatically generate
 *  subclasses of mtflist that contain various data types.  See glist.h
 *  for more details.
 */

#define DECLARE_MTFLIST_CLASS(CLASS, TYPE)				      \
    DECLARE_MTFLIST_CLASSES(CLASS, CLASS##_e, CLASS##_iter, TYPE)

#define DECLARE_MTFLIST_CLASSES(CLASS, CLASS_E, CLASS_ITER, TYPE)	      \
DECLARE_LIST_CLASS_E(CLASS_E, TYPE, glist_e, ;);			      \
DECLARE_LIST_CLASS_LIST(CLASS, CLASS_E, TYPE, mtflist,			      \
    CLASS_E *get(mtflist_test_f check, void *key)			      \
      { return (CLASS_E*)mtflist::get(check, key); }			      \
    CLASS_E *lookup(mtflist_test_f check, void *key)			      \
      { return (CLASS_E*)mtflist::lookup(check, key); });		      \
DECLARE_LIST_CLASS_ITER(CLASS, CLASS_E, CLASS_ITER, TYPE, glist_iter)

#endif /* MTFLIST_H */
