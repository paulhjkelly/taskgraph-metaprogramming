/*  Generic Hash Table Definitions */

/*  Copyright (c) 1994 Stanford University

    All rights reserved.

    This software is provided under the terms described in
    the "suif_copyright.h" include file. */

#include <suif_copyright.h>

#ifndef HASH_H
#define HASH_H

#pragma interface

RCS_HEADER(hash_h,
    "$Id$")


/*
 *  Hash table entries.  This base class contains only the signature for an
 *  entry; other fields can be added in derived classes.  The signature is
 *  an unsigned value (not necessarily unique) that is used to index into the
 *  hash table.  In many cases, a pointer can be used as the signature; in
 *  other situations a more complex hash function may be necessary.
 */

class hash_e : public glist_e {
public:
    unsigned signature;

    hash_e() : signature(0)		{ }
    hash_e(unsigned s) : signature(s)	{ }
};


/*
 *  Type for functions to compare two hash table entries.  Return TRUE if the
 *  two entries are equal.  This function is necessary to distinguish distinct
 *  entries that happen to have the same signature.
 */

typedef boolean (*hash_compare)(hash_e *, hash_e *);


/*
 *  Hash table buckets (hash_chains) are move-to-front list of hash_e's.
 *  By using mtflist, we are implicitly assuming that the references to
 *  the hash table exhibit temporal locality.
 */

class hash_chain : public mtflist {
public:
    hash_e *lookup(hash_compare check, hash_e *k)
	{ return static_cast<hash_e *>(mtflist::lookup(reinterpret_cast<mtflist_test_f>(check), k)); }
    hash_e *remove(hash_compare check, hash_e *k)
	{ return static_cast<hash_e *>(mtflist::get(reinterpret_cast<mtflist_test_f>(check), k)); }
};


/*
 *  Generic hash tables contain fixed-size arrays of hash_chain buckets.
 *  The user must provide a function to compare hash table entries.  The
 *  "lookup" method checks if the given entry is in the table.  The "enter"
 *  method adds a new entry to the table; it returns TRUE if the entry was
 *  already there.
 */

class hash_table {
private:
    /* We make explicit copy constructor and assignment operator and
     * make them private to foil C++'s automatic default versions. */
    hash_table(const hash_table &);
    hash_table &operator=(const hash_table &);

public:
    hash_compare compare;
    unsigned size;
    hash_chain *buckets;

    hash_table(hash_compare f, unsigned sz);
    ~hash_table();

    boolean enter(hash_e *e);
    hash_e *lookup(hash_e *e)
	{ return buckets[e->signature % size].lookup(compare, e); }
    hash_e *remove(hash_e *e)
	{ return buckets[e->signature % size].remove(compare, e); }
};

#endif /* HASH_H */
