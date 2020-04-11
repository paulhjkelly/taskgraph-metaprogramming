/* file "fract_vector.cc" */

/*  Copyright (c) 1994,95 Stanford University

    All rights reserved.

    This software is provided under the terms described in
    the "suif_copyright.h" include file. */

#include <suif_copyright.h>

#pragma implementation "fract_vector.h"

#define RCS_BASE_FILE fract_vector_cc

#include "fract_vector.h"
#include <cstring>

RCS_BASE("$Id$")


/*
 * Private member functions
 */

void fract_vector::copy_vector(const fract_vector &vec)
{
    assert(size == vec.size);

/*
    for (int i = 0; i < size; i++) {
	fract_list[i] = vec.c(i);
    }
*/

    memcpy(fract_list, vec.fract_list, size*sizeof(fract));
}


void fract_vector::copy_row(const integer_row &rw)
{
    assert(size == rw.n());

    for (int i = 0; i < size; i++) {
	fract_list[i] = fract(rw.c(i));
    }
}


/*
 * Public member functions
 */


//
// Constructors
//

fract_vector::fract_vector()
{
    fract_list = NULL;
    size = 0;
}

fract_vector::fract_vector(const integer_row &rw)
{
    size = rw.n();
    fract_list = new fract[size];
    copy_row(rw);
}

fract_vector::fract_vector(const integer_row &rw, int div)
{
    size = rw.n();
    fract_list = new fract[size];
    copy_row(rw);
    *this /= fract(div);
}

fract_vector::fract_vector(const fract_vector &vec)
{
    size = vec.n();
    fract_list = new fract[size];
    copy_vector(vec);
}


fract_vector::fract_vector(int s)
{
    size = s;
    fract_list = new fract[size];
}

fract_vector::fract_vector(const int *ilist, int s) 
{
    size = s;
    fract_list = new fract[size];

    for (int i = 0; i < size; i++) {
	fract_list[i] = fract(ilist[i]);
    }
}

fract_vector::fract_vector(const fract *flist, int s)
{
    size = s;
    fract_list = new fract[size];

/*
    for (int i = 0; i < size; i++) {
	fract_list[i] = flist[i];
    }
*/

    memcpy(fract_list, flist, size*sizeof(fract));
}


//
//  Equality and assignment operators:
//

boolean fract_vector::operator==(const fract_vector &a) const
{
    if (size != a.size) return FALSE;

/*
    for(int i = 0; i < size; i++) {
        if (fract_list[i] != a[i])  return FALSE;
    }
*/

    return (memcmp(a.fract_list, fract_list, size*sizeof(fract)) == 0);
}

fract_vector &fract_vector::operator=(const fract_vector &vec)
{
    if (&vec == this) return *this;

    if (size != vec.size) {
        clear();
        size = vec.size; 
        fract_list = new fract[size]; 
    } 

    copy_vector(vec);
    return *this;
}


//
//   Element-wise operations:
//

fract_vector fract_vector::operator+(const fract_vector & vec) const
{
    fract_vector result = *this;
    return result += vec;
}

fract_vector fract_vector::operator-(const fract_vector & vec) const
{
    fract_vector result = *this;
    result -= vec;

    return result;
}

fract_vector fract_vector::operator*(const fract_vector & vec) const
{
    fract_vector result = *this;
    result *= vec;

    return result;
}

fract_vector fract_vector::operator/(const fract_vector & vec) const
{
    fract_vector result = *this;
    result /= vec;

    return result;
}

fract_vector fract_vector::inverse() const
{
    fract_vector result(size);

    for (int i = 0; i < size; i++) {
	fract cur_fract = fract_list[i];
        result[i] = fract(cur_fract.denom(), cur_fract.num());
    }

    return (result);
}

fract_vector & fract_vector::operator+=(const fract_vector & vec)
{
    assert(size == vec.size);
/*
    for (int i = 0; i < size; i++) {
        fract_list[i] += vec[i];
    }
*/
    fract *my_ptr = fract_list;
    fract *vec_ptr = vec.fract_list;
    fract *last_ptr = &fract_list[size-1];

    while (my_ptr <= last_ptr) *my_ptr++ += *vec_ptr++;
    return *this;
}

fract_vector & fract_vector::operator-=(const fract_vector & vec)
{
    assert(size == vec.size);
/* 
    for (int i = 0; i < size; i++) {
        fract_list[i] -= vec[i];
    }
*/
    fract *my_ptr = fract_list;
    fract *vec_ptr = vec.fract_list;
    fract *last_ptr = &fract_list[size-1];

    while (my_ptr <= last_ptr) *my_ptr++ -= *vec_ptr++;
    return *this;
}

fract_vector & fract_vector::operator*=(const fract_vector & vec)
{
    assert(size == vec.size);
/*
    for (int i = 0; i < size; i++) {
        fract_list[i] *= vec[i];
    }
*/
    fract *my_ptr = fract_list;
    fract *vec_ptr = vec.fract_list;
    fract *last_ptr = &fract_list[size-1];

    while (my_ptr <= last_ptr) *my_ptr++ *= *vec_ptr++;
    return *this;
}

fract_vector & fract_vector::operator/=(const fract_vector & vec)
{
    assert(size == vec.size);
 /*
    for (int i = 0; i < size; i++) {
        fract_list[i] /= vec[i];
    }

*/
    fract *my_ptr = fract_list;
    fract *vec_ptr = vec.fract_list;
    fract *last_ptr = &fract_list[size-1];

    while (my_ptr <= last_ptr) *my_ptr++ /= *vec_ptr++;
    return *this;
}

fract_vector & fract_vector::operator=(fract val)
{
    for (int i = 0; i < size; i++) {
	fract_list[i] = val;
    }

    return *this;
}

fract_vector & fract_vector::operator+=(fract val)
{
    for (int i = 0; i < size; i++) {
	fract_list[i] += val;
    }

    return *this;
}

fract_vector & fract_vector::operator-=(fract val)
{
    for (int i = 0; i < size; i++) {
	fract_list[i] -= val;
    }

    return *this;

}

fract_vector & fract_vector::operator*=(fract val)
{
    for (int i = 0; i < size; i++) {
	fract_list[i] *= val;
    }

    return *this;
}

fract_vector & fract_vector::operator/=(fract val)
{
    for (int i = 0; i < size; i++) {
	fract_list[i] /= val;
    }

    return *this;
}


//
// Other useful functions:
//

fract fract_vector::dot(const fract_vector & vec) const
{

    assert(size == vec.size);
    fract result(0);

    for (int i = 0; i < n(); i++) {
         result += fract_list[i] * vec.c(i);
    }

    return result;
}

fract_vector operator *(const fract &f, const fract_vector &vec)
{
    fract_vector result(vec.n());

    for (int i = 0; i < vec.n(); i++) {
        result[i] = f * vec.c(i);
    }

    return result;
}

boolean fract_vector::proportional(const fract_vector &vec) const
{
    assert(size == vec.size);

    fract cur_ratio = fract(0);
    for (int i = 0; i < size; i++) {
        boolean is_zero1 = (fract_list[i] == 0);
        boolean is_zero2 = (vec.c(i) == 0);

        if (is_zero1 || is_zero2 ) {
            if (!is_zero1 || !is_zero2) return FALSE;
        } 
        else {
            fract ratio = fract_list[i]/vec.c(i);
            if (cur_ratio.num() == 0) cur_ratio = ratio;
            else if (cur_ratio != ratio) return FALSE;
        }
    }

    return TRUE;
}

//
// Returns the point that's the projection of this onto line spanned by vec.
// Let a = vec, b = this and resulting point = p
//    p = a*(a^T*b / a^T*a), where a^T means a transposed.
// See Strang, Third Edition, pg. 148
//
fract_vector fract_vector::projection(const fract_vector & vec) const
{
    fract f = this->dot(vec) / vec.dot(vec);
    return (f * vec);
}

boolean fract_vector::is_zero() const
{
    for (int i = 0; i < n(); i++) {
	if (fract_list[i].num() != 0) return FALSE;
    }

    return TRUE;
}


//
// Try to reduce numerator maximally for all fractions in the vector.
//
void fract_vector::reduce_magnitude()
{
    if (size == 0) return;

    int cur_num = 0, cur_denom = 0;
 
    int i;
    for (i = 0; i < size; i++) {
        int num = fract_list[i].num();
        if (num < 0) num = -num;
 
        cur_num = gcd(cur_num, num);
	cur_denom = gcd(cur_denom, fract_list[i].denom());
    }

    assert(cur_denom != 0);
    assert(cur_num != 0 || cur_denom == 1);

    if (cur_num > 1 || cur_denom > 1) {
        for(i = 0; i < size; i++) {
	    fract cur_fract = fract_list[i];
	    fract_list[i] = fract(cur_fract.num() / cur_num, 
				  cur_fract.denom() / cur_denom);
        }
    }
}


//
// General utilities
//

void fract_vector::operator^=(fract_vector &vec)
{
    assert(size == vec.size);

    for (int i = 0; i < size; i++) {
        fract temp_fract = fract_list[i];
        fract_list[i] = vec[i];
        vec[i] = temp_fract;
    }
}

fract_vector fract_vector::operator%(const integer_row &rw)
{
    assert(size == rw.n());

    fract_vector result(size);
    for(int i = 0; i < size; i++) {
        assert((rw.c(i) >= 0) && (rw.c(i) < size));
        result[i] = fract_list[rw.c(i)];
    }

    return result;
}


fract_vector fract_vector::del_pos(int i, int j) const
{
    assert(i <= j);
    assert((i >= 0) && (j < size));
    fract_vector result(size - (j - i + 1));

    int count = 0;
    int k;
    for (k = 0; k < i; k++) 
        result[count++] = fract_list[k];

    for (k = j+1; k < size; k++)
        result[count++] = fract_list[k];

    assert(count == result.size);
    return result;
}

fract_vector fract_vector::insert_pos(int i) const
{
    assert((i >= 0) && (i <= size));
    fract_vector result(size + 1);

    int count = 0;
    int k;
    for (k = 0; k < i; k++)
        result[count++] = fract_list[k];

    result[count++] = fract(0);

    for (k = i; k < size; k++)
        result[count++] = fract_list[k];

    assert(count == result.size);
    return result;
}


fract_vector fract_vector::swap_pos(int i, int j) const
{
    assert((i >= 0) && (i < size));
    assert((j >= 0) && (j < size));

    fract_vector result = *this;

    result[i] = fract_list[j];
    result[j] = fract_list[i];

    return result;
}


fract_vector fract_vector::resize(int n1, int n2, fract fill) const
{
    fract_vector result(n2-n1);
    result = fill;
  
    for (int i = MAX(0,n1); i < MIN(size,n2); i++) {
	result[i-n1] = this->c(i);
    }

    return result;
}


immed_list *fract_vector::cvt_immed_list() const
{
    immed_list *il = new immed_list;

    il->append(immed("size"));
    il->append(immed(size));

    // Find lcm of denoms
    int curr_lcm = 1;
    int i;
    for (i = 0; i < size; i++) {
	curr_lcm = lcm(curr_lcm, c(i).denom());
    }

    if (curr_lcm != 1) {
	il->append(immed("divisor"));
	il->append(immed(curr_lcm));
    }

    il->append(immed("vector"));
    for (i = 0; i < size; i++) {
	fract temp = c(i);
   	temp *= curr_lcm;              // fract "*=" operator reduces fraction
	assert(temp.denom() == 1);
        il->append(immed(temp.num()));
    }

    return il;
}

fract_vector cvt_fract_vector(immed_list *il)
{
    assert(il->count() >= 3);  // at least: "size" #elems "vector"

    immed temp = il->pop();
    assert(temp.is_string() && !strcmp(temp.string(), "size"));
    immed sz = il->pop();
    assert(sz.is_integer());

    fract_vector result(sz.integer());
    temp = il->pop();
    assert(temp.is_string());

    // Grab divisor, if any
    //
    int divisor = 1;
    if (!strcmp(temp.string(), "divisor")) {
	assert(il->count() >= 2);
	immed div = il->pop();
	assert(div.is_integer());
	divisor = div.integer();
	temp = il->pop();
	assert(temp.is_string());
    }
    
    assert(!strcmp(temp.string(), "vector"));
    assert(il->count() >= result.n());

    for (int i = 0; i < result.n(); i++) {
	immed val = il->pop();
	assert(val.is_integer());
	result[i] = fract(val.integer(), divisor);
    }

    return result;
}


void fract_vector::print(FILE *fp) const
{
    fprintf(fp, "(");
    for (int i = 0; i < n(); i++) {
       fract_list[i].print(fp);
       fprintf(fp, " ");
    }

    fprintf(fp, ")"); 
}

void fract_vector::clear()
{
    if (fract_list) {
        delete[] fract_list;
        fract_list = NULL;
    }
    size = 0;
}
