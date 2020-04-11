/*  String Table Definitions */

/*  Copyright (c) 1994 Stanford University

    All rights reserved.

    This software is provided under the terms described in
    the "suif_copyright.h" include file. */

#include <suif_copyright.h>

#ifndef STRINGTABLE_H
#define STRINGTABLE_H

#pragma interface

RCS_HEADER(stringtable_h,
    "$Id$")

#define string_table_size 512


/*
 *  The string table is a hash table.  Each entry in the hash table is a
 *  linked list of string_e's that point to the string value.  Every time
 *  enter() is called it will search the table for the string and if found
 *  it will simply return the stored pointer value.  In this way, we have
 *  the nice property that pointer equality implies string equality, a
 *  property that can lead to very efficient string comparisions.  When
 *  entering a new string in the table, the "fixed" flag should be set to
 *  TRUE only if the string is known to be allocated on the heap and will
 *  not be modified or deallocated.
 */

class string_e {
    friend class string_table;

private:
    /* We make explicit copy constructor and assignment operator and
     * make them private to foil C++'s automatic default versions. */
    string_e(const string_e &);
    string_e &operator=(const string_e &);

    string_e *next;

    string_e(string_e *n, unsigned sig, const char *s) :
	next(n), signature(sig), sp(s) { }

public:
    unsigned signature;
    const char *sp;				/* always points to heap */

    ~string_e();
};


class string_table {
private:
    /* We make explicit copy constructor and assignment operator and
     * make them private to foil C++'s automatic default versions. */
    string_table(const string_table &);
    string_table & operator=(const string_table &);

    string_e *table[string_table_size];

public:
    string_table();
    ~string_table();

    string_e * enter(const char *s, boolean fixed = FALSE);
};


EXPORTED_BY_SUIF string_table *lexicon;

#endif /* STRINGTABLE_H */
