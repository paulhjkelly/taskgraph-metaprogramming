/*  Association List Implementation */

/*  Copyright (c) 1994 Stanford University

    All rights reserved.

    This software is provided under the terms described in
    the "suif_copyright.h" include file. */

#include <suif_copyright.h>

#define _MODULE_ "libsuif.a"

#pragma implementation "alist.h"

#define RCS_BASE_FILE alist_cc

#include "misc.h"
#include "glist.h"
#include "mtflist.h"
#include "alist.h"

RCS_BASE(
    "$Id$")

// override default copy constructor and assignment op
alist_e::alist_e(const alist_e &)  { assert(FALSE); }
alist_e &alist_e::operator=(const alist_e &)  { assert(FALSE); return *this; }

alist_iter::alist_iter(const alist_iter &)  { assert(FALSE); }
alist_iter &alist_iter::operator=(const alist_iter &)  { assert(FALSE); return *this; }

/*
 *  The following two functions search through the elements of an
 *  alist to find a match for the specified key.  The return value
 *  varies.
 */

alist_e *
alist::search (void *k) const
{
    alist_e *ap = (alist_e *)head();
    while (ap) {
	if (ap->key == k)
	    return ap;
	ap = ap->next();
    }
    return NULL;
}


void *
alist::lookup (void *k) const
{
    alist_e *ap = search(k);
    assert_msg(ap, ("alist::lookup - attempt to lookup %x failed", k));
    return ap->info;
}


/*
 *  Check if the list contains the specified key.  If so, optionally
 *  return the associated info pointer through the second argument.
 */

boolean alist::exists(void *k, void **i) const
{
    alist_e *ap = search(k);
    if (ap) {
	if (i) *i = ap->info;
	return TRUE;
    }
    return FALSE;
}


/*****************************************************************************/


/*
 *  The search functions in amtflist use the same search function as
 *  mtflist by providing a separate function to compare the keys.
 */

static boolean
amtflist_pred (alist_e *a, void *k)
{
    return a->key == k;
}


alist_e *
amtflist::search (void *k)
{
    return (alist_e *)mtflist::lookup((mtflist_test_f)amtflist_pred, k);
}


void *
amtflist::lookup (void *k)
{
    alist_e *ap = search(k);
    assert_msg(ap, ("amtflist::lookup - attempt to lookup %x failed", k));
    return ap->info;
}


boolean
amtflist::exists (void *k, void **i)
{
    alist_e *ap = search(k);
    if (ap) {
	if (i) *i = ap->info;
	return TRUE;
    }
    return FALSE;
}


