/*  String Table Implementation */

/*  Copyright (c) 1994 Stanford University

    All rights reserved.

    This software is provided under the terms described in
    the "suif_copyright.h" include file. */

#include <suif_copyright.h>

#define _MODULE_ "libsuif.a"

#pragma implementation "stringtable.h"

#define RCS_BASE_FILE stringtable_cc

#include "misc.h"
#include "stringtable.h"
#include <cstring>

RCS_BASE(
    "$Id$")


static unsigned
RawHash (const char *s)
{
    unsigned sum = 0;
    int i = 0;
    while (*s) {
	sum += (++i) * (0377 & *s++);
    }
    return sum;
}

// override default copy constructor and assignment op
string_table::string_table(const string_table &)  { assert(FALSE); }
string_table &string_table::operator=(const string_table &)  { assert(FALSE); return *this; }

string_table::string_table ()
{
    /* clear all the hash buckets */
    for (int i = 0; i < string_table_size; i++) {
	table[i] = NULL;
    }
}


string_table::~string_table ()
{
    for (int i = 0; i < string_table_size; i++) {
	delete table[i];
    }
}


/*
 *  The enter function scans through the linked list of string_e's to
 *  find the given string.  The non-equality test used in the loop has
 *  been found to be very effective at removing unnecessary string
 *  comparisons.  If the optional argument, fixed, is set then s is
 *  guaranteed to be an uncorruptible string stored on the heap and
 *  enter will simply use its value rather than making a copy of it.
 *
 *  A potential improvement to this function is to use a move-to-front
 *  list to improve the search time.  This should cut down on the number
 *  of iterations because of locality in the string accesses.  However,
 *  the time of each iteration will be longer.  Therefore, if the lists
 *  are short anyways one could easily lose.
 */

string_e *
string_table::enter (const char *s, boolean fixed)
{
    unsigned h = RawHash(s);
    unsigned h2 = h % string_table_size;
    string_e *ep = table[h2];

    /* look through the existing entries */
    while (ep) {
	if (ep->signature == h && !strcmp(ep->sp,s))
	    return ep;
	ep = ep->next;
    }

    /* if not found, create a new entry */
    string_e *e = new string_e(table[h2], h, fixed ? s : const_string(s));
    return table[h2] = e;
}

// override default copy constructor and assignment op;
string_e::string_e(const string_e &)  { assert(FALSE); }
string_e & string_e::operator=(const string_e &)  { assert(FALSE); return *this; }

string_e::~string_e ()
{
    delete next;
    delete[] sp;
}
