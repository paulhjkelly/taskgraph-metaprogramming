/* file "fract.cc" */

/*  Copyright (c) 1994 Stanford University

    All rights reserved.

    This software is provided under the terms described in
    the "suif_copyright.h" include file. */

#include <suif_copyright.h>

/*  Fraction Implementation */
#define _MODULE_ "libsuifmath.a"
#pragma implementation "fract.h"

#include "suif1.h"
#include "fract.h"


/*  Reduce the fraction.  */

void fract::reduce()
{
    if (n == 0) d = 1;
    else if (d == 1) ;
    else if (n == 1 && d > 0) ;
    else reduce_aux();
}



void fract::reduce_aux()
{
    /* convert to absolute values */
    int mm = (n < 0) ? -n : n;
    int nn = (d < 0) ? -d : d;

    /* find gcd and divide thru by it */
    while (TRUE) {
	int rr = mm % nn;
	if (rr == 0) break;		/* nn is the answer */
	mm = nn;
	nn = rr;
    }
    if (d < 0) nn = -nn;
    n /= nn;
    d /= nn;
}



fract fract::operator/(const fract &a) const
{
    assert(a.num() != 0);

    /* get the reciprocal */
    fract rcp;
    if (a.num() < 0) {
	rcp = fract(fneg(a.denom()), fneg(a.num()));
    } else {
	rcp = fract(a.denom(), a.num());
    }

    /* multiply by the reciprocal */
    return *this * rcp;
}



void fract::print(FILE *f) const
{
    if (d == 1) {
	fprintf(f, " %d ", n);
    } else {
	fprintf(f, "%d/%d", n, d);
    }
    fflush(f);
}
