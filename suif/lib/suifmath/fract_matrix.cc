/* file "fract_matrix.cc" */

/*  Copyright (c) 1994,95 Stanford University

    All rights reserved.

    This software is provided under the terms described in
    the "suif_copyright.h" include file. */

#include <suif_copyright.h>

#pragma implementation "fract_matrix.h"

#define RCS_BASE_FILE fract_matrix_cc

#include "fract_matrix.h"
#include <cstring>

RCS_BASE("$Id$")

/*
 * Private member functions
 */

void fract_matrix::create_columns()
{
    col_list = new fract_vector[cols];

    for (int i = 0; i < cols; i++) {
        col_list[i] = fract_vector(rows);
    }
}


void fract_matrix::copy_fract_matrix(const fract_matrix &mat)
{
    assert(rows == mat.m() && cols == mat.n());

    for (int j = 0; j < cols; j++) {
	col_list[j] = mat.col_list[j];
    }
}

void fract_matrix::copy_int_matrix(const integer_matrix &mat)
{
    assert(rows == mat.m() && cols == mat.n());

    for (int j = 0; j < cols; j++)
        for (int i = 0; i < rows; i++) {
	    elt(i,j) = mat.r(i).c(j);
        }
}


/*
 * Public member functions
 */

//
// Constructors
//

fract_matrix::fract_matrix()
{
    rows = cols = 0;
    col_list = NULL;
}


fract_matrix::fract_matrix(int r, int c)
{
    assert(r >= 0 && c >= 0);

    rows = r;
    cols = c;
    create_columns();
}

fract_matrix::fract_matrix(const integer_matrix &mat)
{
    assert(mat.m() >= 0 && mat.n() >= 0);

    rows = mat.m();
    cols = mat.n();
    create_columns();
    copy_int_matrix(mat);
}

fract_matrix::fract_matrix(const integer_matrix &mat, int div)
{
    assert(mat.m() >= 0 && mat.n() >= 0);

    rows = mat.m();
    cols = mat.n();
    create_columns();
    copy_int_matrix(mat);

    *this /= fract(div);
}

fract_matrix::fract_matrix(const fract_matrix &mat)
{
    rows = mat.rows;
    cols = mat.cols;
    create_columns();
    copy_fract_matrix(mat);
}

fract_matrix::fract_matrix(const fract_matrix &mat, int c)
{
    assert(c >= 0);

    rows = mat.rows;
    cols = c;

    col_list = new fract_vector[cols];

    if (cols <= mat.cols) {
        for (int i = 0; i < cols; i++) col_list[i] = mat.col_list[i];
    }
    else {
        int i;
        for (i = 0; i< mat.cols; i++) col_list[i] = mat.col_list[i];
        for (i = mat.cols; i< cols; i++) col_list[i] = fract_vector(rows);
    }
}

//
// Equality and assignment operators:
//

boolean fract_matrix::operator==(const fract_matrix &mat) const
{
    if ((rows != mat.rows) || (cols != mat.cols)) return FALSE;

    for (int i = 0; i < cols; i++) {
        if (mat.col_list[i] != col_list[i]) return FALSE;
    }

    return TRUE;
}

fract_matrix & fract_matrix::operator=(const fract_matrix &mat)
{
    if (&mat == this) return *this;

    if ((rows != mat.rows) || (cols != mat.cols)) {
	clear();
	rows = mat.rows;
	cols = mat.cols;
	create_columns();
    }

    copy_fract_matrix(mat);
    return *this;
}


//
// Matrix-Matrix operations:
//

fract_matrix fract_matrix::operator*(const fract_matrix &mat) const
{
    assert(cols == mat.rows);

    fract_matrix result(rows, mat.cols);

    for (int i = 0; i < rows; i++) {
        for (int j = 0; j < mat.cols; j++) {
	    fract temp = fract(0);
      	    for (int k = 0; k < cols; k++) {
	        temp += rc(i,k) * mat.rc(k,j);
	    }
	    result.elt(i,j) = temp;
        }
    }

    return result;
}

fract_matrix fract_matrix::operator+(const fract_matrix &mat) const
{
    fract_matrix result = *this;
    result += mat;

    return result;
}

fract_matrix fract_matrix::operator-(const fract_matrix &mat) const
{
    fract_matrix result = *this;
    result -= mat;

    return result;
}

fract_matrix & fract_matrix::operator+=(const fract_matrix &mat)
{
    assert(rows == mat.rows);
    assert(cols == mat.cols);

    for (int j = 0; j < cols; j++)
        for (int i = 0; i < rows; i++)
            elt(i,j) += mat.rc(i,j);

    return *this;
}

fract_matrix & fract_matrix::operator-=(const fract_matrix &mat)
{
    assert(rows == mat.rows);
    assert(cols == mat.cols);

    for (int j = 0; j < cols; j++)
        for (int i = 0; i < rows; i++)
            elt(i,j) -= mat.rc(i,j);

    return *this;
}

//
// Element-wise operations:
//

fract_matrix fract_matrix::operator*(fract val) const
{
    fract_matrix result = *this;

    result *= val;
    return result;
}

fract_matrix fract_matrix::operator/(fract val) const
{
    fract_matrix result = *this;

    result /= val;
    return result;
}

fract_matrix fract_matrix::operator+(fract val) const
{
    fract_matrix result = *this;

    result += val;
    return result;
}

fract_matrix fract_matrix::operator-(fract val) const
{
    fract_matrix result = *this;

    result -= val;
    return result;
}

fract_matrix &fract_matrix::operator+=(fract val)
{
    for (int j = 0; j < cols; j++)
	for (int i = 0; i < rows; i++)
	    elt(i,j) += val;

    return *this;
}

fract_matrix &fract_matrix::operator-=(fract val)
{
    for (int j = 0; j < cols; j++)
        for (int i = 0; i < rows; i++)
	    elt(i,j) -= val;

    return *this;
}

fract_matrix &fract_matrix::operator*=(fract val)
{
    for (int j = 0; j < cols; j++)
	for (int i = 0; i < rows; i++)
	    elt(i,j) *= val;

    return *this;
}

fract_matrix &fract_matrix::operator/=(fract val)
{
    for (int j = 0; j < cols; j++)
        for (int i = 0; i < rows; i++)
	    elt(i,j) /= val;

    return *this;
}

//
// Matrix-vector
//

fract_vector fract_matrix::operator*(const fract_vector &vec) const
{
    fract_vector result(rows);
    assert(cols == vec.n());

    for (int i = 0; i < rows; i++) {
	fract f(0);
	for (int j = 0; j < cols; j++) {
	    f += rc(i,j) * vec.c(j);
	}
	result[i] = f;
    }

    return result;
}

//
// Other useful functions:
//
void fract_matrix::ident(int n)
{
    if ((rows != n) || (cols != n)) {
	clear();
	rows = n;
	cols = n;
	create_columns();
    }

    for (int j = 0; j < cols; j++) {
        elt(j,j) = fract(1);
    }
}

fract_matrix fract_matrix::transpose() const
{
    fract_matrix result(cols, rows);

    for (int j = 0; j < cols; j++)
	for (int i = 0; i < rows; i++)
            result.elt(j,i) = rc(i,j);

    return result;
}

fract_matrix fract_matrix::operator%(const integer_row &rw) const
{
    fract_matrix result(rows, cols);

    for (int j = 0; j < cols; j++) {
        result.col_list[j] = col_list[j] % rw;
    }

    return result;
}

fract_matrix fract_matrix::del_row(int i, int j) const
{
    assert (i <= j);
    assert((i >= 0) && (j < rows));

    fract_matrix result(rows - (j - i + 1), cols);

    for (int k = 0; k < cols; k++)
        result.col_list[k] = col_list[k].del_pos(i, j);

    return result;
}


fract_matrix fract_matrix::del_col(int i, int j) const
{
    assert (i <= j);
    assert((i >= 0) && (j < cols));

    fract_matrix result(rows, cols - (j - i + 1));

    int count = 0;
    int k;
    for (k = 0; k < i; k++)
        result.col_list[count++] = col_list[k];

    for (k = j+1; k < cols; k++)
        result.col_list[count++] = col_list[k];

    assert(count == result.cols);
    return result;
}


fract_matrix fract_matrix::del_columns(const integer_row &rw) const
{
    assert(rw.n() == cols);

    int col_count = 0;

    for (int i = 0; i < cols; i++) {
        if (rw.c(i)) col_count++;
    }

    fract_matrix result(rows, col_count);
    int count = 0;

    for (int j = 0; j < cols; j++)
        if (rw.c(j)) result.col_list[count++] = col_list[j];

    assert(count == result.cols);
    return result;
}

fract_matrix fract_matrix::insert_col(int i) const
{
    assert(i >= 0 && i <= cols);
    fract_matrix result(rows, cols+1);

    int count = 0;
    int j;
    for (j = 0; j < i; j++)
	result.col_list[count++] = col_list[j];

    result.col_list[count++] = fract_vector(rows);

    for (j = i+1; j < cols; j++)
	result.col_list[count++] = col_list[j];

    assert(count == result.cols);
    return result;
}

fract_matrix fract_matrix::swap_col(int i, int j) const
{
    assert(i >= 0 && i < cols);
    assert(j >= 0 && j < cols);
    fract_matrix result = *this;

    result.col_list[i] = col_list[j];
    result.col_list[j] = col_list[i];

    return result;
}

fract_matrix fract_matrix::swap_row(int i, int j) const
{
    assert(i >= 0 && i < rows);
    assert(j >= 0 && j < rows);
    fract_matrix result(rows, cols);

    for(int k = 0; k < cols; k++)
        result.col_list[j] = col_list[j].swap_pos(i, j);

    return result;
}


fract_vector fract_matrix::get_row(int i) const
{
    assert(i >= 0 && i < rows);
    fract_vector the_row(cols);

    for (int j = 0; j < cols; j++) {
	the_row[j] = rc(i,j);
    }

    return the_row;
}


fract_vector fract_matrix::get_col(int i) const
{
    assert(i >= 0 && i < cols);
    return col_list[i];
}


void fract_matrix::set_col(int i, const fract_vector &vec)
{
    assert(i >= 0 && i < cols);
    assert(vec.n() == rows);

    col_list[i] = vec;
}


void fract_matrix::set_row(int i, const fract_vector &vec)
{
    assert(i >= 0 && i < rows);
    assert(vec.n() == cols);

    for (int j = 0; j < cols; j++) {
    	elt(i,j) = vec.c(j);
    }
}


fract_matrix fract_matrix::resize_offset(int r1, int r2, int c1, int c2,
					 int fill) const
{
    assert(rows-r1+r2 >=0);
    assert(cols-c1+c2 >=0);

    fract_matrix result(rows-r1+r2, cols-c1+c2);

    int j;
    for (j = 0; j < result.cols; j++) result.col_list[j] = fill;

    for (j = MAX(0,c1); j < cols+MIN(0,c2); j++)
        for (int i = MAX(0,r1); i < rows+MIN(0,r2); i++)
            result.elt(i-r1,j-c1) = rc(i,j);

    return result;
}

fract_matrix fract_matrix::r_merge(const fract_matrix &mat) const
{
    assert(cols == mat.cols);

    return (resize_offset(0, mat.rows, 0, 0) +
            mat.resize_offset(-rows, 0, 0, 0));
}


fract_matrix fract_matrix::c_merge(const fract_matrix &mat) const
{
    assert(rows == mat.rows);

    return (resize_offset(0, 0, 0, mat.cols) +
            mat.resize_offset(0, 0, -cols, 0));
}

immed_list *fract_matrix::cvt_immed_list() const
{
    immed_list *il = new immed_list;

    il->append(immed("rows"));
    il->append(immed(rows));
    il->append(immed("cols"));
    il->append(immed(cols));

    // Find lcm of denoms
    int curr_lcm = 1;
    for (int j = 0; j < cols; j++) {
        for (int i = 0; i < rows; i++) {
	    curr_lcm = lcm(curr_lcm, rc(i,j).denom());
	}
    }

    if (curr_lcm != 1) {
	il->append(immed("divisor"));
	il->append(immed(curr_lcm));
    }

    il->append(immed("matrix"));
    for (int i = 0; i < rows; i++) {
	for (int j = 0; j < cols; j++) {
	    fract temp = rc(i,j);
    	    temp *= curr_lcm;         // fract "*=" operator reduces fraction
	    assert(temp.denom() == 1);
            il->append(immed(temp.num()));
        }
    }

    return il;
}

fract_matrix cvt_fract_matrix(immed_list *il)
{
    assert(il->count() >= 5);  // at least: "rows" #rows "cols" #cols "matrix"

    immed temp = il->pop();
    assert(temp.is_string() && !strcmp(temp.string(), "rows"));
    immed r = il->pop();

    temp = il->pop();
    assert(temp.is_string() && !strcmp(temp.string(), "cols"));
    immed c = il->pop();

    assert(r.is_integer() && c.is_integer());

    fract_matrix result(r.integer(), c.integer());
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

    assert(!strcmp(temp.string(), "matrix"));
    assert(il->count() >= result.m() * result.n());

    for (int i = 0; i < result.m(); i++) {
	for (int j = 0; j < result.n(); j++) {
	    immed val = il->pop();
	    assert(val.is_integer());
	    result.elt(i,j) = fract(val.integer(), divisor);
	}
    }

    return result;
}

void fract_matrix::print(FILE *fp) const
{
    for (int i = 0; i < rows; i++) {
        for (int j = 0; j < cols; j++) {
	    rc(i,j).print(fp);
	    fprintf(fp, " ");
        }
	fprintf(fp, "\n");
    }
}

void fract_matrix::clear()
{
    if (col_list) {
        delete[] col_list;
	col_list = NULL;
    }

    rows = cols = 0;
}
