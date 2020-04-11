/*  Doubly-Linked List Definitions */

/*  Copyright (c) 1994 Stanford University

    All rights reserved.

    This software is provided under the terms described in
    the "suif_copyright.h" include file. */

#include <suif_copyright.h>

#ifndef DLIST_H
#define DLIST_H

#pragma interface

RCS_HEADER(dlist_h,
    "$Id$")


/*
 *  Doubly-linked lists.  In addition to the standard glist_e fields, each
 *  element of these lists contains a pointer to the previous list element.
 *  This makes certain list operations much more efficient, but otherwise
 *  the interfaces are identical to the standard glist classes.
 */

class dlist_e : public glist_e {
    friend class dlist;

private:
    /* We make explicit copy constructor and assignment operator and
     * make them private to foil C++'s automatic default versions. */
    dlist_e(const dlist_e &);
    dlist_e &operator=(const dlist_e &);

protected:
    dlist_e *prev_e;

public:
    dlist_e() : prev_e(0)		{ }
    dlist_e *next() const		{ return static_cast<dlist_e *>(glist_e::next()); }
    dlist_e *prev() const		{ return prev_e; }
};


class dlist : public glist {

public:
    dlist_e *head() const		{ return static_cast<dlist_e*>(glist::head()); }
    dlist_e *tail() const		{ return static_cast<dlist_e*>(glist::tail()); }
    dlist_e *push(dlist_e *e);
    dlist_e *pop();
    dlist_e *append(dlist_e *e);
    dlist_e *insert_before(dlist_e *e, dlist_e *pos);
    dlist_e *insert_after(dlist_e *e, dlist_e *pos);
    dlist_e *remove(dlist_e *e);
    dlist_e *operator[](int ndx) const
	{ return static_cast<dlist_e*>(glist::operator[](ndx)); }
    void push(dlist *l);
    void append(dlist *l);
    void insert_before(dlist *l, dlist_e *pos);
    void insert_after(dlist *l, dlist_e *pos);
};


class dlist_iter : public glist_iter {
private:
    /* We make explicit copy constructor and assignment operator and
     * make them private to foil C++'s automatic default versions. */
    dlist_iter(const dlist_iter &);
    dlist_iter &operator=(const dlist_iter &);

public:
    dlist_iter()			{ }
    dlist_iter(const dlist *dl) : glist_iter(dl) { }

    dlist_e *step()			{ return static_cast<dlist_e*>(glist_iter::step());}
    dlist_e *peek() const		{ return static_cast<dlist_e*>(glist_iter::peek());}
};


/*
 *  The following macro definitions are used to automatically generate
 *  subclasses of dlist that contain various data types.  See glist.h for
 *  more details.
 */

#define DECLARE_DLIST_CLASS(CLASS,TYPE)					      \
DECLARE_DLIST_CLASSES(CLASS, CLASS##_e, CLASS##_iter, TYPE)

#define DECLARE_DLIST_CLASSES(CLASS, CLASS_E, CLASS_ITER, TYPE)		      \
DECLARE_LIST_CLASS_E(CLASS_E, TYPE, dlist_e,				      \
    CLASS_E *prev() { return static_cast<CLASS_E*>(dlist_e::prev()); });		      \
DECLARE_LIST_CLASS_LIST(CLASS, CLASS_E, TYPE, dlist, ;);		      \
DECLARE_LIST_CLASS_ITER(CLASS, CLASS_E, CLASS_ITER, TYPE, dlist_iter)

#endif /* DLIST_H */
