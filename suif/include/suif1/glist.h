/*  Generic List Definitions */

/*  Copyright (c) 1994 Stanford University

    All rights reserved.

    This software is provided under the terms described in
    the "suif_copyright.h" include file. */

#include <suif_copyright.h>

#ifndef GLIST_H
#define GLIST_H

#pragma interface

RCS_HEADER(glist_h,
    "$Id$")

/*
 *  Class glist_e: List elements for generic lists.  A glist_e list
 *  element only contains a pointer to the next element in the list.
 *  Other fields are added in derived classes.
 */

class glist_e {
    friend class glist;
    friend class mtflist;
    friend class dlist;

private:
    /* We make explicit copy constructor and assignment operator and
     * make them private to foil C++'s automatic default versions. */
    glist_e(const glist_e &);
    glist_e &operator=(const glist_e &);

protected:
    glist_e *next_e;

public:
    glist_e() : next_e(0)		{ }
    glist_e *next() const		{ return next_e; }
};


/*
 *  Class glist: Generic lists are used as base classes throughout the
 *  SUIF system.  The glist class contains pointers to the glist_e list
 *  elements at the head and tail of a list.  An empty list is
 *  indicated by a NULL head pointer.
 *
 *  The push and pop functions add and remove elements from the front
 *  of the list.  The append function adds a list element on the end of
 *  the list.  The remove function removes the given element from the list.
 *  The grab_from function takes all of the elements from another glist and
 *  then clears that list.  The count function returns the number of
 *  elements in a list.
 *
 *  There is no storage management in glist.  Since deallocation
 *  is intimately related to the contents and usage patterns of the
 *  derived classes, such behavior should be specified by them.
 */

class glist {
private:
    /* We make explicit copy constructor and assignment operator and
     * make them private to foil C++'s automatic default versions. */
    glist(const glist &);
    glist &operator=(const glist &);

protected:
    glist_e *head_e, *tail_e;

public:
    glist() : head_e(0), tail_e(0) { }
    virtual ~glist();

    boolean is_empty() const		{ return head_e == NULL; }
    glist_e *head() const		{ return head_e; }
    glist_e *tail() const		{ return tail_e; }
    glist_e *push(glist_e *e);
    glist_e *pop();
    glist_e *append(glist_e *e);
    glist_e *insert_before(glist_e *e, glist_e *pos);
    glist_e *insert_after(glist_e *e, glist_e *pos);
    glist_e *remove(glist_e *e);
    void clear()			{ head_e = tail_e = NULL; }
    void erase();
    void grab_from(glist *l);
    void push(glist *l);
    void append(glist *l);
    void insert_before(glist *l, glist_e *pos);
    void insert_after(glist *l, glist_e *pos);
    int count() const;
    boolean contains(const glist_e *e) const;
    glist_e *operator[](int ndx) const;
};


/*
 *  Class glist_iter:  Iterator for generic lists.  This provides an easy
 *  way to traverse the elements of a glist.  The reset function initializes
 *  the iterator to point to the beginning of a list.  The is_empty function
 *  can then be used as the exit condition for a loop.  Within the loop,
 *  a call to the step function will return the current list element and
 *  advance the iterator.  The peek function returns the current list
 *  element without advancing to the next element.
 */

class glist_iter {
private:
    /* We make explicit copy constructor and assignment operator and
     * make them private to foil C++'s automatic default versions. */
    glist_iter(const glist_iter &);
    glist_iter &operator=(const glist_iter &);

protected:
    glist_e *cur, *nxt;

public:
    glist_iter() : cur(0), nxt(0)	{ }
    glist_iter(const glist *gl) : cur(0), nxt(0) { reset(gl); }

    void reset(const glist *gl);
    void set(glist_e *e);		/* set the next element */

    boolean is_empty() const		{ return nxt == NULL; }
    glist_e *step();
    glist_e *peek() const		{ return nxt; }
    glist_e *cur_elem() const		{ return cur; }
};


/*
 *  The following macro definitions are used to automatically generate
 *  subclasses of glist that contain various data types.  (This is an
 *  alternative to using C++ templates, which seem to be plagued with bugs
 *  in the GNU compiler.)  The set_elem() virtual function can be overridden
 *  in a derived class to automatically update a list element when it is
 *  added to a list.  For example, set_elem() may set back pointers in the
 *  TYPE object to the CLASS list and/or to the CLASS_E list element.
 */

#define DECLARE_LIST_CLASS(CLASS, TYPE)					      \
    DECLARE_LIST_CLASSES(CLASS, CLASS##_e, CLASS##_iter, TYPE)

#define DECLARE_LIST_CLASSES(CLASS, CLASS_E, CLASS_ITER, TYPE)		      \
DECLARE_LIST_CLASS_E(CLASS_E, TYPE, glist_e, ;);			      \
DECLARE_LIST_CLASS_LIST(CLASS, CLASS_E, TYPE, glist, ;);		      \
DECLARE_LIST_CLASS_ITER(CLASS, CLASS_E, CLASS_ITER, TYPE, glist_iter)


#define DECLARE_LIST_CLASS_E(CLASS_E, TYPE, BASE_E, EXTRA)		      \
class CLASS_E: public BASE_E {						      \
private: \
    CLASS_E(const CLASS_E &o) : contents(o.contents) { assert(0); } \
    CLASS_E &operator=(const CLASS_E &) { assert(0); return *this; } \
public:									      \
    TYPE contents;							      \
    CLASS_E(TYPE t) : contents(t) { }					\
    CLASS_E *next() const { return static_cast<CLASS_E*>(BASE_E::next()); }		      \
    EXTRA								      \
}


#define DECLARE_LIST_CLASS_LIST(CLASS, CLASS_E, TYPE, BASE_CLASS, EXTRA)      \
class CLASS: public BASE_CLASS {					      \
protected:								      \
    virtual void set_elem(CLASS_E *) { }				      \
    void set_elems(CLASS *l)						      \
	{ CLASS_E *i = l->head(); while (i) { set_elem(i); i = i->next(); } } \
private: \
    CLASS(const CLASS &) { assert(0); } \
    CLASS &operator=(const CLASS &) { assert(0); return *this; } \
public:									      \
    CLASS() { }								      \
    CLASS(TYPE t) { append(t); }					      \
    CLASS(TYPE t1, TYPE t2) { append(t1); append(t2); }			      \
    CLASS(TYPE t1, TYPE t2, TYPE t3) { append(t1); append(t2); append(t3); }  \
    virtual ~CLASS() { } \
    CLASS_E *head() const { return static_cast<CLASS_E*>(BASE_CLASS::head()); }	      \
    CLASS_E *tail() const { return static_cast<CLASS_E*>(BASE_CLASS::tail()); }	      \
    TYPE push(TYPE t) { return push(new CLASS_E(t))->contents; }	      \
    CLASS_E *push(CLASS_E *e)						      \
	{ set_elem(e); return static_cast<CLASS_E*>(BASE_CLASS::push(e)); }		      \
    TYPE pop() { CLASS_E *e = static_cast<CLASS_E*>(BASE_CLASS::pop());		      \
		 TYPE t = e->contents; delete e; return t; }		      \
    TYPE append(TYPE t) { return append(new CLASS_E(t))->contents; }	      \
    TYPE remove(TYPE t) { CLASS_E *tmp  = new CLASS_E(t);                     \
                          TYPE tmp_type = remove(tmp)->contents;              \
                          delete tmp; return tmp_type; }	              \
    CLASS_E *append(CLASS_E *e)						      \
	{ set_elem(e); return static_cast<CLASS_E*>(BASE_CLASS::append(e)); }	      \
    TYPE insert_before(TYPE t, CLASS_E *pos)				      \
	{ return insert_before(new CLASS_E(t), pos)->contents; }	      \
    CLASS_E *insert_before(CLASS_E *e, CLASS_E *pos)			      \
	{ set_elem(e); return static_cast<CLASS_E*>(BASE_CLASS::insert_before(e, pos)); }  \
    TYPE insert_after(TYPE t, CLASS_E *pos)				      \
	{ return insert_after(new CLASS_E(t), pos)->contents; }		      \
    CLASS_E *insert_after(CLASS_E *e, CLASS_E *pos)			      \
	{ set_elem(e); return static_cast<CLASS_E*>(BASE_CLASS::insert_after(e, pos)); }   \
    CLASS_E *remove(CLASS_E *e) { return static_cast<CLASS_E*>(BASE_CLASS::remove(e)); }   \
    void copy(const CLASS *l) {						      \
	CLASS_E *i = l->head();						      \
	while (i) { append(i->contents); i = i->next(); } }		      \
    void grab_from(CLASS *l) { set_elems(l); BASE_CLASS::grab_from(l); }      \
    void push(CLASS *l) { set_elems(l); BASE_CLASS::push(l); }		      \
    void append(CLASS *l) { set_elems(l); BASE_CLASS::append(l); }	      \
    void insert_before(CLASS *l, CLASS_E *pos)				      \
	{ set_elems(l); BASE_CLASS::insert_before(l, pos); }		      \
    void insert_after(CLASS *l, CLASS_E *pos)				      \
	{ set_elems(l); BASE_CLASS::insert_after(l, pos); }		      \
    TYPE operator[](int ndx) const					      \
	{ return static_cast<CLASS_E*>(BASE_CLASS::operator[](ndx))->contents; }	      \
    CLASS_E *lookup(const TYPE t) const {				      \
	CLASS_E *i = head();						      \
	while (i) { if (i->contents == t) break; i = i->next(); }	      \
	return i; }							      \
    EXTRA								      \
}


#define DECLARE_LIST_CLASS_ITER(CLASS, CLASS_E, CLASS_ITER, TYPE, BASE_ITER)  \
class CLASS_ITER: public BASE_ITER {					      \
public:									      \
    CLASS_ITER() : BASE_ITER() { }					      \
    CLASS_ITER(const CLASS *l) : BASE_ITER(l) { }			      \
    TYPE step() { return static_cast<CLASS_E*>(BASE_ITER::step())->contents; }	      \
    TYPE peek() const { assert_msg(!is_empty(),				      \
			     ("%s_iter::peek - no next element", #CLASS));    \
			return static_cast<CLASS_E*>(BASE_ITER::peek())->contents; }     \
    CLASS_E *cur_elem() const { return static_cast<CLASS_E*>(BASE_ITER::cur_elem()); }     \
}									      \

#endif /* GLIST_H */


