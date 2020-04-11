/* Generic List Implementation */

/*  Copyright (c) 1994 Stanford University

    All rights reserved.

    This software is provided under the terms described in
    the "suif_copyright.h" include file. */

#include <suif_copyright.h>

#define _MODULE_ "libsuif.a"

#pragma implementation "glist.h"

#define RCS_BASE_FILE glist_cc

#include "misc.h"
#include "glist.h"

RCS_BASE(
    "$Id$")


// override default copy constructor and assignment op
glist_e::glist_e(const glist_e &) : next_e(0)  { assert(FALSE); }
glist_e &glist_e::operator=(const glist_e &)  { assert(FALSE); return *this; }

glist::glist(const glist &) : head_e(0), tail_e(0) { assert(FALSE); }
glist &glist::operator=(const glist &)  { assert(FALSE); return *this; }

/*
 *  Delete a glist.
 */

glist::~glist ()
{
    erase();
}


/*
 *  Add a glist_e element at the start of a glist.
 */

glist_e *
glist::push (glist_e *e)
{
    e->next_e = head_e;
    head_e = e;
    if (!tail_e)
	tail_e = head_e;
    return e;
}


/*
 *  Remove and return the element at the start of a glist.
 */

glist_e *
glist::pop ()
{
    assert_msg(head_e, ("glist::pop -- list is empty"));

    glist_e *e = head_e;
    head_e = e->next_e;
    if (!head_e)
	tail_e = NULL;
    return e;
}


/*
 *  Add a glist_e element at the end of a glist.
 */

glist_e *
glist::append (glist_e *e)
{
    if (tail_e) {
	tail_e->next_e = e;
	tail_e = e;
    } else {
	head_e = tail_e = e;
    }
    e->next_e = NULL;
    return e;
}


/*
 *  Insert the list element e before the element pos.  pos==NULL means
 *  insert at the end of the list.
 */

glist_e *
glist::insert_before (glist_e *e, glist_e *pos)
{
    if (!pos) {
	append(e);
    } else if (pos == head_e) {
	push(e);
    } else {
	glist_e *el = head_e, *prev_e = NULL;
	while (el) {
	    if (el->next_e == pos) {
		e->next_e = el->next_e;
		el->next_e = e;
		return e;
	    }
	    prev_e = el;
	    el = el->next_e;
	}
	assert_msg(FALSE,
		   ("glist::insert_before - element not found in list"));
    }
    return e;
}


/*
 *  Insert the list element e after the element pos.  pos==NULL means
 *  insert at the beginning of the list.
 */

glist_e *
glist::insert_after (glist_e *e, glist_e *pos)
{
    if (!pos) {
	push(e);
    } else {
	e->next_e = pos->next_e;
	pos->next_e = e;
	if (!e->next_e)
	    tail_e = e;
    }
    return e;
}


/*
 *  Remove the specified glist_e element from the list.
 */

glist_e *
glist::remove (glist_e *e)
{
    glist_e *el = head_e, *prev_e = NULL;

    while (el) {
	if (el == e) {
	    if (prev_e)
		prev_e->next_e = el->next_e;
	    if (head_e == el)
		head_e = el->next_e;
	    if (tail_e == el)
		tail_e = prev_e;
	    return e;
	}
	prev_e = el;
	el = el->next_e;
    }

    return e;
}


/*
 *  Delete all the entries in the list.  NOTE this will not call the
 *  destructor of any data contained in the elements!
 */

void
glist::erase ()
{
    glist_iter gli(this);
    while (!gli.is_empty())
	delete gli.step();
    head_e = NULL;
    tail_e = NULL;
}


/*
 *  Functions to combine two lists.  All of the elements of the parameter list
 *  are removed and combined with the current list.
 */

void
glist::push (glist *l)
{
    if (!l->tail_e) return;
    l->tail_e->next_e = head_e;
    head_e = l->head_e;
    if (!tail_e)
	tail_e = l->tail_e;
    l->clear();
}


void
glist::append (glist *l)
{
    if (!tail_e) {
	grab_from(l);
	return;
    }
    tail_e->next_e = l->head_e;
    if (l->tail_e)
	tail_e = l->tail_e;
    l->clear();
}


/*
 *  Insert the list l before the element pos.  pos==NULL means
 *  insert at the end of the list.
 */

void
glist::insert_before (glist *l, glist_e *pos)
{
    if (!l->tail_e) return;

    if (!pos) {
	append(l);
    } else if (pos == head_e) {
	push(l);
    } else {
	glist_e *el = head_e, *prev_e = NULL;
	while (el) {
	    if (el->next_e == pos) {
		l->tail_e->next_e = el->next_e;
		el->next_e = l->head_e;
		l->clear();
		return;
	    }
	    prev_e = el;
	    el = el->next_e;
	}
	assert_msg(FALSE,
		   ("glist::insert_before - element not found in list"));
    }
}


/*
 *  Insert the list l after the element pos.  pos==NULL means
 *  insert at the beginning of the list.
 */

void
glist::insert_after (glist *l, glist_e *pos)
{
    if (!l->tail_e) return;

    if (!pos) {
	push(l);
    } else {
	l->tail_e->next_e = pos->next_e;
	pos->next_e = l->head_e;
	if (!l->tail_e->next_e)
	    tail_e = l->tail_e;
    }
}


/*
 *  Steal all of the elements from another glist and clear that list.
 *  The current list must be empty when this function is called.
 */

void
glist::grab_from (glist *l)
{
    assert_msg(is_empty(), ("glist::grab_from - list not empty"));
    head_e = l->head_e;
    tail_e = l->tail_e;
    l->clear();
}


/*
 *  Count the number of elements in the list.
 */

int
glist::count () const
{
    int cnt;
    glist_iter gi(this);

    for (cnt = 0; !gi.is_empty(); gi.step()) {
	cnt++;
    }

    return cnt;
}


/*
 *  Determine if the specified element is in the list.
 */

boolean
glist::contains (const glist_e *e) const
{
    glist_e *el = head_e;

    while (el) {
	if (el == e)
	    return TRUE;
	el = el->next_e;
    }
    return FALSE;
}


/*
 *  Access an element a particular distance from the front of the list.
 */

glist_e *
glist::operator[] (int ndx) const
{
    int origndx = ndx;
    
    glist_e *el = head_e;
    while (el) {
	if (!ndx)
	    return el;
	ndx--;
	el = el->next();
    }
    assert_msg(FALSE,
	       ("glist::[] - Attempt to extract element %d out of range %d-%d",
		origndx, 0, count()-1));
    return NULL;
}


/*****************************************************************************/

// override default copy constructor and assignment op
glist_iter::glist_iter(const glist_iter &) : cur(0), nxt(0) { assert(FALSE); }
glist_iter &glist_iter::operator=(const glist_iter &)  { assert(FALSE); return *this; }


void
glist_iter::reset (const glist *gl)
{
    cur = NULL;
    nxt = gl ? gl->head() : NULL;
}


void
glist_iter::set (glist_e *e)
{
    nxt = e;
}


glist_e *
glist_iter::step ()
{
    cur = nxt;
    if (nxt) nxt = nxt->next();
    return cur;
}


