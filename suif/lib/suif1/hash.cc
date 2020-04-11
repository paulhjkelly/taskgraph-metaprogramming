/*  Generic Hash Table Implementation */

/*  Copyright (c) 1994 Stanford University

    All rights reserved.

    This software is provided under the terms described in
    the "suif_copyright.h" include file. */

#include <suif_copyright.h>

#define _MODULE_ "libsuif.a"

#pragma implementation "hash.h"

#define RCS_BASE_FILE hash_cc

#include "misc.h"
#include "glist.h"
#include "mtflist.h"
#include "hash.h"

RCS_BASE(
    "$Id$")

// override default copy constructor and assignment op
hash_table::hash_table(const hash_table &)  { assert(FALSE); }
hash_table &hash_table::operator=(const hash_table &)  { assert(FALSE); return *this;}

hash_table::hash_table (hash_compare f, unsigned sz)
{
    compare = f;
    buckets = new hash_chain[sz];
    size = sz;
}


hash_table::~hash_table ()
{
    delete[] buckets;
}


/*
 *  This function tries to add a new entry to the hash table.  It returns
 *  TRUE if the element was already there.
 */

boolean
hash_table::enter (hash_e *e)
{
    /* find the appropriate bucket */
    hash_chain *b = &buckets[e->signature % size];

    /* check if the entry is already there */
    if (b->lookup(compare, e))
	return TRUE;

    /* add it as a new entry */
    b->push(e);
    return FALSE;
}


