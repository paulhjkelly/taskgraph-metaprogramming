/*  Move-to-Front List Implementation */

/*  Copyright (c) 1994 Stanford University

    All rights reserved.

    This software is provided under the terms described in
    the "suif_copyright.h" include file. */

#include <suif_copyright.h>

#define _MODULE_ "libsuif.a"

#pragma implementation "mtflist.h"

#define RCS_BASE_FILE mtflist_cc

#include "misc.h"
#include "glist.h"
#include "mtflist.h"

RCS_BASE(
    "$Id$")


/*
 *  This function searches through the list for an element for which
 *  the check function returns TRUE.  If found, the element is removed
 *  from the list and returned.  Otherwise, the function returns NULL.
 */

glist_e *
mtflist::get (mtflist_test_f check, void *key)
{
    glist_e *current = head();
    glist_e *prev = NULL;
    
    while (current) {
	if (check(current, key)) {
	    if (prev == NULL)
		return pop();
	    prev->next_e = current->next_e;
	    current->next_e = NULL;
	    if (tail_e == current)
		tail_e = prev;
	    return current;
	}
	prev = current;
	current = current->next_e;
    }
    return NULL;
}


/*
 *  This function simply uses the get function to find the element.  If
 *  an element is found, it pushes it back on the list and returns a
 *  pointer to it.
 */

glist_e *
mtflist::lookup (mtflist_test_f check, void *k)
{
    glist_e *e = get(check, k);
    if (e)
	push(e);
    return e;
}


