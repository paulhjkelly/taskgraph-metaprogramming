/* file "fract.h" */

/*  Copyright (c) 1994 Stanford University

    All rights reserved.

    This software is provided under the terms described in
    the "suif_copyright.h" include file. */

#include <suif_copyright.h>

/*  Fraction Definitions */
#pragma interface

#ifndef FRACT_H
#define FRACT_H

#pragma interface

/*  the following files must be included before this point:
#include <cassert>
#include <cstdio>
#include <useful.h>
*/
#include <climits>

class fract;

inline int fmul(int a, int b)
{
    /* make sure the result will fit in an int */
    double x = static_cast<double>(a) * static_cast<double>(b);
    assert((x < static_cast<double>(INT_MAX)) && (x > static_cast<double>(INT_MIN)));
    return a * b;
}

inline int fadd(int a, int b)
{
    /* make sure the result will fit in an int */
    double x = static_cast<double>(a) + static_cast<double>(b);
    assert((x < static_cast<double>(INT_MAX)) && (x > static_cast<double>(INT_MIN)));
    return a + b;
}

inline int fsub(int a, int b)
{
    /* make sure the result will fit in an int */
    double x = static_cast<double>(a) - static_cast<double>(b);
    assert((x < static_cast<double>(INT_MAX)) && (x > static_cast<double>(INT_MIN)));
    return a - b;
}

inline int fneg(int a)
{
    /* make sure the result will fit in an int */
    double x = -static_cast<double>(a);
    assert((x < static_cast<double>(INT_MAX)) && (x > static_cast<double>(INT_MIN)));
    return -a;
}

class fract {
    friend class vector;
    int n, d;
    void reduce_aux();
    void reduce();

public:
    fract()				{ n = 0; d = 1; }
    fract(const fract &f)		{ n = f.n; d = f.d; }
    fract(int nn)			{ n = nn; d = 1; }
    fract(int nn, int dd)		{ n = nn; d = dd; reduce(); }
    ~fract()				{ }
    int num() const			{ return n; }
    int denom() const			{ return d; }
    boolean is_zero() const             { return (n == 0); }
    double real() const			{ return double(n) / double(d); }
    boolean is_int() const		{ return ((d == 1) || (d == -1)); }
    int integer() const                 { assert(is_int());
					  return ((d < 0) ? fneg(n) : n); }
    void operator=(const fract &f)	{ n = f.n; d = f.d; }
    fract operator-() const		{ return fract(fneg(n), d); }
    fract operator+(const fract &a) const
					{ return fract(fadd(fmul(n, a.d),
							    fmul(d, a.n)),
						       fmul(d, a.d)); }
    fract operator-(const fract &a) const
					{ return *this + (-a); }
    fract operator*(const fract &a) const
					{ return fract(fmul(n, a.n),
						       fmul(d, a.d)); }
    fract operator/(const fract &a) const;
    void operator+=(const fract &a)	{ int nn = fadd(fmul(n, a.d),
							fmul(d, a.n));
					  int dd = fmul(d, a.d);
					  n = nn; d = dd; reduce(); }
    void operator-=(const fract &a)	{ int nn = fsub(fmul(n, a.d),
							fmul(d, a.n));
    					  int dd = fmul(d,a.d);
					  n = nn; d = dd; reduce(); }
    void operator*=(const fract &a)	{ n = fmul(n, a.n); d = fmul(d, a.d);
					  reduce(); }
    void operator/=(const fract &a)	{ assert(a.n != 0); n = fmul(n, a.d);
					  d = fmul(d, a.n); reduce(); }
    boolean operator==(const fract &a) const
					{ return (n == a.n) && (d == a.d); }
    boolean operator!=(const fract &a) const
					{ return (n != a.n) || (d != a.d); }
    boolean operator<=(const fract &a) const
					{ return fmul(n, a.d) <= fmul(d, a.n); }
    boolean operator<(const fract &a) const
					{ return fmul(n, a.d) < fmul(d, a.n); }
    boolean operator>=(const fract &a) const
					{ return fmul(n, a.d) >= fmul(d, a.n); }
    boolean operator>(const fract &a) const
					{ return fmul(n, a.d) > fmul(d, a.n); }
    fract abs() const			{ return fract((n < 0) ? fneg(n) : n,
						       d); }
    void print(FILE *f = stdout) const;
};

#endif /* FRACT_H */
