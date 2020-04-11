/*  Association List Definitions */

/*  Copyright (c) 1994 Stanford University

    All rights reserved.

    This software is provided under the terms described in
    the "suif_copyright.h" include file. */

#include <suif_copyright.h>

#ifndef ALIST_H
#define ALIST_H

#pragma interface

RCS_HEADER(alist_h,
    "$Id$")

/*
 *  Association list elements include both a key and a data pointer.
 *  This allows access to a particular list element specified by a key.
 */

class alist_e : public glist_e {

private:
    /* We make explicit copy constructor and assignment operator and
     * make them private to foil C++'s automatic default versions. */
    alist_e(const alist_e &);
    alist_e &operator=(const alist_e &);

public:
    void *key, *info;

    alist_e(void *k, void *i) : key(k), info(i) { }
    alist_e *next()			{ return static_cast<alist_e *>(glist_e::next()); }
};


/*
 *  The alist class defines several functions to search for a
 *  particular key and return either the list element or the info
 *  pointer from that element.  Several wrapper function also provide
 *  type-correct access to the underlying glist functions.
 */

class alist : public glist {

public:
    alist() { }
    alist_e *head() const		{ return static_cast<alist_e *>(glist::head()); }
    alist_e *tail() const		{ return static_cast<alist_e *>(glist::tail()); }
    alist_e *push(alist_e *e)		{ return static_cast<alist_e *>(glist::push(e)); }
    alist_e *pop()			{ return static_cast<alist_e *>(glist::pop()); }
    alist_e *remove(alist_e *e)		{ return static_cast<alist_e *>(glist::remove(e)); }

    alist_e *enter(void *k, void *i)	{ return push(new alist_e(k, i)); }
    alist_e *search(void *k) const;
    void *lookup(void *k) const;
    boolean exists(void *k, void **i = NULL) const;
};


/*
 *  The amtflist class combines the features of the association lists
 *  with those of the move-to-front lists.  The interface is equivalent
 *  to the alist interface.
 */

class amtflist : public mtflist {
public:
    alist_e *head()			{ return static_cast<alist_e*>(mtflist::head()); }
    alist_e *tail()			{ return static_cast<alist_e*>(mtflist::tail()); }
    alist_e *push(alist_e *e)		{ return static_cast<alist_e*>(mtflist::push(e)); }
    alist_e *pop()			{ return static_cast<alist_e*>(mtflist::pop()); }
    alist_e *remove(alist_e *e)		{ return static_cast<alist_e*>(mtflist::remove(e));}

    alist_e *enter(void *k, void *i)	{ return push(new alist_e(k, i)); }
    alist_e *search(void *k);
    void *lookup(void *k);
    boolean exists(void *k, void **i = NULL);
};


/*
 *  This iterator works for both alists and amtflists.
 */

class alist_iter : public glist_iter {

private:
    /* We make explicit copy constructor and assignment operator and
     * make them private to foil C++'s automatic default versions. */
    alist_iter(const alist_iter &);
    alist_iter &operator=(const alist_iter &);

public:
    alist_iter()			{ }
    alist_iter(const alist *l)		{ reset(l); }
    alist_iter(const amtflist *l)	{ reset(l); }

    alist_e *step()		{ return static_cast<alist_e *>(glist_iter::step()); }
    alist_e *peek()		{ return static_cast<alist_e *>(glist_iter::peek()); }
    alist_e *cur_elem()		{ return static_cast<alist_e *>(glist_iter::cur_elem()); }
};

#endif /* ALIST_H */
