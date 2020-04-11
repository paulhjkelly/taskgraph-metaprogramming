/*  Bit Set Implementation */

/*  Copyright (c) 1994 Stanford University

    All rights reserved.

    This software is provided under the terms described in
    the "suif_copyright.h" include file. */

#include <suif_copyright.h>

#define _MODULE_ "libsuif.a"

#pragma implementation "bitset.h"

#define RCS_BASE_FILE bitset_cc

#include "misc.h"
#include "bitset.h"

RCS_BASE(
    "$Id$")

/*  number of bits in a word */
const int w_size = sizeof(long) * suif_byte_size;


inline int
words (int i)
{
    return i / w_size;
}

// private default constructor and assignment op
bit_set::bit_set(const bit_set &) { assert(FALSE); }
bit_set &bit_set::operator=(const bit_set &)  { assert(FALSE); return *this; }

/*
 *  This constructor builds a bit vector capable of representing sets
 *  with elements that are greater than or equal to "f" but strictly
 *  less than "l".  By default, it will clear the bit vector but this
 *  can be disabled using the no_clear flag.
 */

bit_set::bit_set (int f, int l, boolean no_clear)
    : first(0), last(0), bits(0)
{
    expand(f, l, no_clear);
}


/*
 *  Create a new bitset and optionally clear all the bits.
 */

void
bit_set::expand (int f, int l, boolean no_clear)
{
    int sz;

    assert_msg(l >= f, ("bit_set::expand - length is negative"));
    delete[] bits;

    first = words(f) * w_size;
    sz = words(l - first - 1) + 1;
    last = first + (sz * w_size);

    bits = new long[sz];

    /* clear all the bits */
    if (!no_clear)
	clear();
}


void
bit_set::clear ()
{
    int i;
    long *ip = bits;

    for (i = first; i < last; i += w_size) {
	*ip++ = 0;
    }
}


/*
 *  Set all bits to 1.
 */

void
bit_set::universal ()
{
    int i;
    long *ip = bits;

    for (i = first; i < last; i += w_size) {
	*ip++ = ~0;
    }
}


/*
 *  Set one of the bits to 1.  If it exceeds the current range, extend
 *  the range.
 */

void
bit_set::add (int e)
{
   int i;
   int nf, nl, nsize;
   long *nbits;
   long *sp, *dp;

   if (bits == NULL)
       expand(e, e + 1);

   if (e >= first && e < last) {
       e -= first;
       bits[e / w_size] |= 1L << (e % w_size);
       return;
   }
   else if (e < first) {
       nf = words(e) * w_size;
       nsize = words(last - nf);
       nl = last;
   }
   else {
       nf = first;
       nsize = words(e - nf) + 1;
       nl = nf + (nsize * w_size);
   }

   nbits = new long[nsize];
   dp = nbits;
   for (i = nf; i < first; dp++, i += w_size)
       *dp = 0;

   sp = bits;
   for (i = first; i < last; dp++, sp++, i += w_size)
       *dp = *sp;

   for (i = last; i < nl; dp++, i += w_size)
       *dp = 0;

   e -= nf;
   nbits[e / w_size] |= 1L << ( e % w_size);

   if (bits) delete[] bits;
   bits = nbits;
   first = nf;
   last = nl;
}


/*
 *  Clear one of the bits to 0.
 */

void
bit_set::remove (int e)
{
    assert_msg(e >= first && e < last,
	       ("bit_set::add - bit %d not in range %d-%d", e, first, last));

    e -= first;
    bits[e / w_size] &= ~(1L << (e % w_size));
}


/*
 *  Invert all the bits.
 */

void
bit_set::invert ()
{
    int i;
    long *ip = bits;

    for (i = first; i < last; i += w_size) {
	*ip = ~*ip;
	ip++;
    }
}


/*
 *  Check if one of the bits is set.
 */

boolean
bit_set::contains (int e)
{
    if (e < first || e >= last)
	return FALSE;

    e -= first;
    long r = bits[e / w_size] & (1L << (e % w_size));
    return r != 0;
}


/*
 *  Create a bit-wise OR of two bitsets.
 */

void
bit_set::set_union (bit_set *l, bit_set *r)
{
    first = (l->first < r->first) ? l->first : r->first;
    last = (l->last > r->last) ? l->last : r->last;

    delete[] bits;
    bits = new long[words(last - first)];

    int i;
    long *ip = bits;
    for (i = first; i < last; i += w_size) {
	*ip++ = (*l)[i] | (*r)[i];
    }
}


/*
 *  Create a bit-wise AND of two bitsets.
 */

void
bit_set::set_intersect (bit_set *l, bit_set *r)
{
    first = (r->first < l->first) ? l->first : r->first;
    last  = (r->last > l->last)   ? l->last  : r->last;

    delete[] bits;

    if (last <= first) {
	first = 0; last = 0; bits = NULL;
	return;
    }
    bits = new long[words(last - first)];

    int i;
    long *ip = bits;
    for (i = first; i < last; i += w_size) {
	*ip++ = (*l)[i] & (*r)[i];
    }
}


/*
 *  Copy a bit_set.
 */

void
bit_set::copy (bit_set *s)
{
    /* if not the same size, reallocate this bitset */
    if (first != s->first || last != s->last) {
	first = s->first;
	last = s->last;
	delete[] bits;
	bits = new long[words(last - first)];
    }

    /* copy the bits one word at a time */
    int i;
    long *ip = bits;
    long *jp = s->bits;
    for (i = first; i < last; i += w_size) {
	*ip++ = *jp++;
    }
}


/*
 *  Do a structure copy from a bit_set and optionally delete it.
 */

void
bit_set::transfer (bit_set *src, boolean del)
{
    if (bits)
	delete[] bits;
    *this = *src;
    src->bits = NULL;
    if (del) delete src;
}


/*
 *  Bit-wise OR with this bitset, expanding if necessary.
 */

void
bit_set::operator+= (bit_set &r)
{
    if (r.first >= r.last)
	return;

    if (bits == NULL)
	expand(r.first, r.last, TRUE);

    else if (r.first < first || r.last > last) {

	int nf = (r.first < first) ? r.first : first;
	int nl = (r.last  > last)  ? r.last  : last;

	long *nbits = new long[words(nl - nf)];
	long *dp = nbits;
	int i;

	for (i = nf; i < first; dp++, i += w_size)
	    *dp = 0;
	long *sp = bits;
	for (i = first; i < last; sp++, dp++, i += w_size)
	    *dp = *sp;
	for (i = last; i < nl; dp++, i += w_size)
	    *dp = 0;

	delete[] bits;
	bits = nbits;
	first = nf;
	last = nl;
    }

    long *dp = bits + words(r.first - first);
    for (int i = r.first; i < r.last; dp++, i += w_size)
	*dp |= r[i];
}


/*
 *  Bit-wise AND with this bitset.
 */

void
bit_set::operator*= (bit_set &r)
{
    int i;
    if (first == r.first && last == r.last) {
	long *lp = bits;
	long *rp = r.bits;
	for (i = first; i < last; i += w_size) {
	    *lp++ &= *rp++;
	}
    } else {
	long *ip = bits;
	for (i = first; i < last; i += w_size) {
	    *ip++ &= r[i];
	}
    }
}


/*
 *  Bit-wise ANDNOT with this bitset.
 */

void
bit_set::operator-= (bit_set &r)
{
    int i;
    long *ip = bits;
    for (i = first; i < last; i += w_size) {
	*ip++ &= ~r[i];
    }
}


/*
 *  Check if all the bits are the same.
 */

boolean
bit_set::operator== (bit_set &r)
{
    int start = (first < r.first) ? first : r.first;
    int end = (last > r.last) ? last : r.last;

    for ( ; start < end; start += w_size) {
	if ((*this)[start] != r[start]) return FALSE;
    }
    return TRUE;
}


/*
 *  Check if r contains at least the bits in this set.
 */

boolean
bit_set::operator<= (bit_set &r)
{
    int start = (first < r.first) ? first : r.first;
    int end = (last > r.last) ? last : r.last;

    for ( ; start < end; start += w_size) {
	long w = (*this)[start];
	if ((w & r[start]) != w) return FALSE;
    }
    return TRUE;
}


/*
 *  Test for non-empty intersection.
 */

boolean
bit_set::operator^ (bit_set &r)
{
    int i;
    if (first == r.first && last == r.last) {
	long *lp = bits;
	long *rp = r.bits;
	for (i = first; i < last; i += w_size) {
	    if (*lp++ & *rp++) return TRUE;
	}
    } else {
	long *ip = bits;
	for (i = first; i < last; i += w_size) {
	    if (*ip++ & r[i]) return TRUE;
	}
    }
    return FALSE;
}


/*
 *  Check if all bits are 0.
 */

boolean
bit_set::is_empty ()
{
    long *ip;
    int sz;
    sz = (last - first) / w_size;
    for (ip = bits; sz > 0; sz--) {
	if (*ip++ != 0) return FALSE;
    }
    return TRUE;
}


/*
 *  Check if all bits are 1.
 */

boolean
bit_set::is_universal ()
{
    long *ip;
    int sz;
    sz = (last - first) / w_size;
    for (ip = bits; sz > 0; sz--) {
	if (*ip++ != ~0) return FALSE;
    }
    return TRUE;
}


/*
 *  Count the number of one bits in this set.
 */

int
bit_set::count ()
{
    int cnt = 0;

    bit_set_iter bi(this);
    while (!bi.is_empty()) {
	bi.step();
	cnt++;
    }
    return cnt;
}


/*
 *  Print the bit_set.
 */

void
bit_set::print (FILE *fp, const char *fmt)
{
    int i;
    fprintf(fp, "(%d:%d){", first, last);
    for (i = first; i < last; i++) {
	if (contains(i))
	    fprintf(fp, fmt, i);
    }
    putc('}', fp);
}


/*
 *  Retrieve an entire word of the bits.
 */

long
bit_set::operator[] (int i)
{
    if (i < first || i >= last)
	return 0;
    i -= first;
    return bits[i / w_size];
}


/*****************************************************************************/


/*
 *  Set up a bitset iterator.
 */

bit_set_iter::bit_set_iter (bit_set *st)
    : s(0), w_offset(0), b_offset(0), w_first(0)
{
    s = st;
    reset();
}


void
bit_set_iter::reset ()
{
    assert_msg(s, ("bit_set_iter::reset - no bit sit given"));

    w_first = s->first;
    w_offset = b_offset = 0;
}


/*
 *  Check if there are any more bits in this set.  Note: This function
 *  must be called once before each call to step().
 */

boolean
bit_set_iter::is_empty ()
{
    if (b_offset >= w_size) {		/* stepped beyond word boundary? */
	w_first += w_size;
	w_offset++;
	b_offset = 0;
    }
    while (w_first < s->last) {
	register unsigned long e = s->bits[w_offset];

	if (e >>= b_offset) {
	    while (!(e & 1)) {
		e >>= 1;
		b_offset++;
	    }
	    return FALSE;
	}
	w_first += w_size;
	w_offset++;
	b_offset = 0;
    }
    return TRUE;
}
