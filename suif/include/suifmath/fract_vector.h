/* file "fract_vector.h" */

/*  Copyright (c) 1994,95 Stanford University

    All rights reserved.

    This software is provided under the terms described in
    the "suif_copyright.h" include file. */

#include <suif_copyright.h>

/* 
   Vectors (with fractional values)
*/

#ifndef FRACT_VECTOR_H
#define FRACT_VECTOR_H

#pragma interface

#include <suif1.h>
#include "int_matrix.h"
#include "fract.h"

RCS_HEADER(fract_vector_h, "$Id$")

class fract_vector {
    friend class fract_vector_list;

private:
    fract *fract_list;
    int size;

    void copy_vector(const fract_vector &vec);  // copy vec into this
    void copy_row(const integer_row &rw);       // copy integer_row into this

public:
    fract_vector();
    fract_vector(const integer_row &rw);
    fract_vector(const integer_row &rw, int div);
    fract_vector(const fract_vector &vec);
    fract_vector(const int *ilist, int s);  // sets elems to ints on ilist
    fract_vector(const fract *flist, int s);// sets elems to fracts on flist
    fract_vector(int s);                    // sets all 's' elements to 0
    ~fract_vector()   { clear(); }

    int n() const   { return size; }

    fract &operator[](int i) 
	{ assert((i >= 0) && (i < size)); 
	  return fract_list[i]; }

    fract &elt(int i) { return (*this)[i]; }

    const fract &c(int i) const
	{ assert((i >= 0) && (i < size)); 
	  return fract_list[i]; }

    //
    // Equality and assignment operators:
    //
    boolean operator==(const fract_vector &a) const;
    boolean operator!=(const fract_vector &a) const { return !((*this) == a); }
    fract_vector & operator=(const fract_vector &vec);

    //
    // Element-wise operations:
    //
    fract_vector operator+(const fract_vector &vec) const;
    fract_vector operator-(const fract_vector &vec) const;
    fract_vector operator*(const fract_vector &vec) const;
    fract_vector operator/(const fract_vector &vec) const;
    fract_vector inverse() const;        // return inverse of each elem
	                                 // does not modify 'this'.
 
    fract_vector & operator+=(const fract_vector &vec);
    fract_vector & operator-=(const fract_vector &vec);
    fract_vector & operator*=(const fract_vector &vec);
    fract_vector & operator/=(const fract_vector &vec);

    fract_vector & operator=(fract val);
    fract_vector & operator+=(fract val);
    fract_vector & operator-=(fract val);
    fract_vector & operator*=(fract val);
    fract_vector & operator/=(fract val);

    fract_vector & operator=(int val)   { return (*this = fract(val)); }
    fract_vector & operator+=(int val)  { return (*this += fract(val)); }
    fract_vector & operator-=(int val)  { return (*this -= fract(val)); }
    fract_vector & operator*=(int val)  { return (*this *= fract(val)); }
    fract_vector & operator/=(int val)  { return (*this /= fract(val)); }

    //
    // Other useful functions:
    //
    fract dot(const fract_vector & vec) const;
    friend fract_vector operator *(const fract &, const fract_vector &);
    boolean proportional(const fract_vector &vec) const;
    fract_vector projection(const fract_vector &vec) const;
                                                // proj of this onto line
                                                // spanned by vec
    boolean is_zero() const;
    void reduce_magnitude();   // used if only direction important

    //
    // General utilities.  
    //
    void clear();
    void operator^=(fract_vector &vec);       // swap vectors

    fract_vector operator%(const integer_row &rw);
                                              // shuffle operator: 
                                              //   ret[x] = this[rw[x]]

    fract_vector del_pos(int i, int j) const; // removes elements from 
                                              //   positions i to j, inclusive

    fract_vector del_pos(int i) const    { return del_pos(i, i); }

    fract_vector insert_pos(int i) const;      // insert new element with 
	                                       //   value 0 at positition i
    fract_vector swap_pos(int i, int j) const; // switch the elems at pos i & j

    // Create a vector from elems n1..n2 of 'this'. Extra elems are appended
    // and set to 'fill'.
    fract_vector resize(int n1, int n2, fract fill = 0) const;

    immed_list *cvt_immed_list() const;
    friend fract_vector cvt_fract_vector(immed_list *il);

    void print(FILE *fp = stdout) const;
};


DECLARE_LIST_CLASSES(fract_vector_list, fract_vector_list_e, 
		     fract_vector_list_iter, fract_vector*);

#endif /* FRACT_VECTOR_H */
