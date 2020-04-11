/*  Doubly-Linked Lists Implementation */

/*  Copyright (c) 1994 Stanford University

    All rights reserved.

    This software is provided under the terms described in
    the "suif_copyright.h" include file. */

#include <suif_copyright.h>

#define _MODULE_ "libsuif.a"

#pragma implementation "dlist.h"

#define RCS_BASE_FILE dlist_cc

#include "misc.h"
#include "glist.h"
#include "dlist.h"

RCS_BASE(
    "$Id$")


// override default copy constructor and assignment op
dlist_e::dlist_e(const dlist_e &) : prev_e(0) { assert(FALSE); }
dlist_e &dlist_e::operator=(const dlist_e &)  { assert(FALSE); return *this; }

dlist_iter::dlist_iter(const dlist_iter &)  { assert(FALSE); }
dlist_iter &dlist_iter::operator=(const dlist_iter &)  { assert(FALSE); return *this; }

/*
 *  Add a dlist_e element at the start of a dlist.
 */

dlist_e *
dlist::push (dlist_e *e)
{
    e->next_e = head_e;
    e->prev_e = NULL;
    if (head_e)
	((dlist_e *)head_e)->prev_e = e;
    head_e = e;
    if (tail_e == NULL)
	tail_e = head_e;
    return e;
}


/*
 *  Remove and return the list element at the start of the list.
 */

dlist_e *
dlist::pop ()
{
    dlist_e *result = (dlist_e*)glist::pop();
    if (head_e) ((dlist_e *)head_e)->prev_e = NULL;
    return result;
}


/*
 *  Add a dlist_e element at the end of a dlist.
 */

dlist_e *
dlist::append (dlist_e *e)
{
    if (tail_e) {
	tail_e->next_e = e;
	e->prev_e = (dlist_e *)tail_e;
	tail_e = e;
    } else {
	head_e = tail_e = e;
	e->prev_e = NULL;
    }
    e->next_e = NULL;
    return e;
}


/*
 *  Insert the list element e before the element pos.  pos==NULL means
 *  insert at end of the list.
 */

dlist_e *
dlist::insert_before (dlist_e *e, dlist_e *pos)
{
    if (pos == NULL) {
	append(e);
    } else {
	e->next_e = pos;
	e->prev_e = pos->prev_e;
	if (pos == head_e) {
	    head_e = e;
	} else {
	    pos->prev_e->next_e = e;
	}
	pos->prev_e = e;
    }
    return e;
}


/*
 *  Insert the list element e after the element pos.  pos==NULL means
 *  insert at the beginning of the list.
 */

dlist_e *
dlist::insert_after (dlist_e *e, dlist_e *pos)
{
    if (pos == NULL) {
	push(e);
    } else {
	e->next_e = pos->next_e;
	e->prev_e = pos;
	if (pos == tail_e) {
	    tail_e = e;
	} else {
	    ((dlist_e *)pos->next_e)->prev_e = e;
	}
	pos->next_e = e;
    }
    return e;
}


/*
 *  Remove the specified dlist_e element from the list.
 */

dlist_e *
dlist::remove (dlist_e *e)
{
    assert(e != NULL);
    if (e == head_e)
	head_e = e->next_e;
    if (e->prev_e)
	e->prev_e->next_e = e->next_e;
    if (e == tail_e)
	tail_e = e->prev_e;
    if (e->next_e)
	e->next()->prev_e = e->prev_e;
    return e;
}


/*
 *  Functions to combine two lists.  All of the elements of the parameter list
 *  are removed and combined with the current list.
 */

void
dlist::push (dlist *l)
{
    if (!l->tail_e) return;
    l->tail_e->next_e = head_e;
    if (head_e)
	((dlist_e *)head_e)->prev_e = (dlist_e *)l->tail_e;
    head_e = l->head_e;
    if (!tail_e)
	tail_e = l->tail_e;
    l->clear();
}


void
dlist::append (dlist *l)
{
    if (!tail_e) {
	grab_from(l);
	return;
    }
    tail_e->next_e = l->head_e;
    if (l->head_e)
	((dlist_e *)l->head_e)->prev_e = (dlist_e *)tail_e;
    if (l->tail_e)
	tail_e = l->tail_e;
    l->clear();
}


/*
 *  Insert the list l before the element pos.  pos==NULL means
 *  insert at the end of the list.
 */

void
dlist::insert_before (dlist *l, dlist_e *pos)
{
    if (!l->tail_e) return;

    if (!pos) {
	append(l);
    } else if (pos == head_e) {
	push(l);
    } else {
	pos->prev_e->next_e = l->head_e;
	((dlist_e *)l->head_e)->prev_e = pos->prev_e;
	l->tail_e->next_e = pos;
	pos->prev_e = (dlist_e *)l->tail_e;
	l->clear();
    }
}


/*
 *  Insert the list l after the element pos.  pos==NULL means
 *  insert at the beginning of the list.
 */

void
dlist::insert_after (dlist *l, dlist_e *pos)
{
    if (!l->tail_e) return;

    if (!pos) {
	push(l);
    } else {
	l->tail_e->next_e = pos->next_e;
	if (pos->next_e)
	    ((dlist_e *)pos->next_e)->prev_e = (dlist_e *)l->tail_e;
	((dlist_e *)l->head_e)->prev_e = pos;
	pos->next_e = l->head_e;
	if (!l->tail_e->next_e)
	    tail_e = l->tail_e;
	l->clear();
    }
}

