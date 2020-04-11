/* file "matrix.cc" */

/*  Copyright (c) 1994,95 Stanford University

    All rights reserved.

    This software is provided under the terms described in
    the "suif_copyright.h" include file. */

#include <suif_copyright.h>

#pragma implementation "matrix.h"

#define RCS_BASE_FILE matrix_cc

#include "matrix.h"

RCS_BASE("$Id$")

/*
 * Private member functions
 */ 

void matrix::init_decomp()
{
    lu_decomp = fract_matrix();
    factored = FALSE;
    interchanges = NULL;
    pivot_cols = NULL;
}

void matrix::clear_decomp()
{
    lu_decomp.clear();
    factored = FALSE;

    if (interchanges) {
	delete[] interchanges;
	interchanges = NULL;
    }

    if (pivot_cols) {
	delete[] pivot_cols;
	pivot_cols = NULL;
    }
}

void matrix::copy_decomp(const matrix &mat)
{
    assert(factored && mat.factored);

    lu_decomp = mat.lu_decomp;

    int rows = lu_decomp.m();
    int cols = lu_decomp.n();

    assert(mat.interchanges);
    interchanges = new int[rows];
    int i;
    for (i = 0; i < rows; i++)
	interchanges[i] = mat.interchanges[i];

    assert(mat.pivot_cols);
    pivot_cols = new boolean[cols];
    for (i = 0; i < cols; i++)
	pivot_cols[i] = mat.pivot_cols[i];
}

void matrix::copy_matrix(const matrix &mat)
{
    elems = mat.elems;

    if (mat.factored) {
	clear_decomp();
	factored = TRUE;
	copy_decomp(mat);
    }
    else {
	init_decomp();
    }
}


//
// Linear Algebra support
//

// Factors column 'j' in elems and places it in lu_decomp 
//
void matrix::factor_col_and_insert(int j)
{
    assert(j < elems.n());

    int rows = elems.m();   // number of rows in resulting decomp
    int cols = j;           // column being factored = number of prev columns

    fract_vector new_col(elems.col_list[j]);

    int cur_pivot_row = 0;
    for (int i = 0; i < cols; i++)        // pivot row = number of pivot cols
        cur_pivot_row += pivot_cols[i];

    assert(cur_pivot_row <= rows);

    int new_pivot_row = factor_col(&new_col, cols, cur_pivot_row);
    assert((cur_pivot_row <= new_pivot_row && new_pivot_row < rows) || 
	   (new_pivot_row == rows && cur_pivot_row == new_pivot_row));

    if (cur_pivot_row < rows) {
	interchanges[cur_pivot_row] = new_pivot_row;

 	if (new_pivot_row != cur_pivot_row) {

      	    // new_col has swapped rows, now swap prev cols
	    for(int i = 0; i < cols; i++) {
	      fract temp = lu_decomp.elt(new_pivot_row, i);
	      lu_decomp.elt(new_pivot_row, i) = lu_decomp.elt(cur_pivot_row,i);
 	      lu_decomp.elt(cur_pivot_row, i) = temp;
	    }
	}

	pivot_cols[cols] = (new_col[cur_pivot_row] != fract(0));
    } 
    else pivot_cols[cols] = FALSE;

    lu_decomp.col_list[j] = new_col;
}


// Factor the given fract_vector column. Returns the row that the pivot row
// is exchanged with.
//
int matrix::factor_col(fract_vector *vec, int cols, int pivot_row)
{
    int rows = elems.m();

    apply_L(vec, cols);   // Apply the L we have so far to the column vec

    if (pivot_row == rows) return rows;
    assert(pivot_row < rows);

    int new_pivot_row = pivot_row;

    fract max = vec->elt(pivot_row);

    if (max == fract(0)) {  // Find new pivot row to interchange
        for (int i = pivot_row+1; i < rows; i++) {
  	    max = vec->elt(i).abs();

  	    if (max > fract(0)) {
    	        new_pivot_row = i;
		break;
	    }
	}
    }

    assert(pivot_row <= new_pivot_row && new_pivot_row < rows);

    if (new_pivot_row != pivot_row) {  // swap
        fract temp = vec->elt(pivot_row);
	vec->elt(pivot_row) = vec->elt(new_pivot_row);
	vec->elt(new_pivot_row) = temp;
    }

    if (vec->elt(pivot_row) != fract(0)) {

	if (pivot_row == cols) {
	    for (int i = pivot_row+1; i < rows; i++) {  
		vec->elt(i) /= vec->elt(pivot_row);
	    }
	}
	else {
	    assert(pivot_row < cols);
	    for (int i = pivot_row+1; i < rows; i++) {
		lu_decomp.elt(i,pivot_row) = vec->elt(i) / vec->elt(pivot_row);
		vec->elt(i) = 0;
	    }
	}
    }

    return new_pivot_row;
}

// Apply the L matrix to the column.
//
void matrix::apply_L(fract_vector *vec, int cols)
{
    int rows = elems.m();

    // Perform interchanges we've done so far
    //
    for (int i = 0; i < rows; i++) {
        if (interchanges[i] != i) {
  	    assert(interchanges[i] > i);
     	    fract temp = vec->elt(i);
   	    vec->elt(i) = vec->elt(interchanges[i]);
	    vec->elt(interchanges[i]) = temp;
	}
    }

    // Operations on column as dictated by L.
    //
    for (int j = 0; j < cols; j++) {
        for (int i = j+1; i < rows; i++) {
  	    vec->elt(i) -= vec->elt(j) * lu_decomp.elt(i,j);
	}
    }
}


// Backsolve the column 'vec' using U.
// All free variables are assumed to be zero except the var specified by
// 'free_var'.
//
fract_vector matrix::backsolve_U(fract_vector &vec, int free_var, 
                                 boolean *valid)
{
    assert(factored);

    int rows = lu_decomp.m();
    int cols = lu_decomp.n();

    fract_vector result(cols);

    // First examine all zero rows:
    int last_row = last_nonzero_Urow();

    for (int ii = last_row + 1; ii < rows; ii++) {
       if (vec[ii] != fract(0)) {
	   *valid = FALSE;
           return result;  // All zeros
       }
    }

    int i = last_row;
    for (int j = cols-1; j >= 0; j--) {
	if (!pivot_cols[j]) {
	    if(j == free_var) result[j] = fract(1);
     	    else result[j] = fract(0);
	}

	else {
	    fract temp = vec[i];

	    for (int jj = j+1; jj < cols; jj++) {
	        temp -= lu_decomp.elt(i,jj) * result[jj];
	    }
	    result[j] = temp/lu_decomp.elt(i,j);
	    i--;
	}
    }

    *valid = TRUE;
    return result;
}

int matrix::last_nonzero_Urow() 
{
    assert(factored);

    int result = -1;
    for (int i = 0; i < lu_decomp.n(); i++)
        result += pivot_cols[i];

    return result;
}


/*
 * Public member functions
 */

//
// Constructors
//

matrix::matrix()
{
    elems = fract_matrix();
    init_decomp();
}

matrix::matrix(int r, int c)
{
    elems = fract_matrix(r, c);
    init_decomp();
}

matrix::matrix(const integer_matrix &mat)
{
    elems = fract_matrix(mat);
    init_decomp();
} 

matrix::matrix(const integer_matrix &mat, int div)
{
    elems = fract_matrix(mat, div);
    init_decomp();
}

matrix::matrix(const fract_matrix &mat)
{
    elems = mat;
    init_decomp();
}

matrix::matrix(const fract_matrix &mat, int c)
{
    elems = fract_matrix(mat, c);
    init_decomp();
}


matrix::matrix(const matrix &mat)
{
    init_decomp();
    copy_matrix(mat);
}

matrix::matrix(const matrix &mat, int c)
{
    elems = fract_matrix(mat.elems, c);
    init_decomp();
}


//
// Equality and assignment operators:
//

boolean matrix::operator==(const matrix &mat) const
{
    return (elems == mat.elems);
}

matrix &matrix::operator=(const matrix &mat)
{
    copy_matrix(mat);
    return *this;
}


//
// Matrix-Matrix operations:
//

matrix matrix::operator*(matrix &mat)
{
    return matrix(elems * mat.elems);
}

matrix matrix::operator+(matrix &mat)
{
    return matrix(elems + mat.elems);
}

matrix matrix::operator-(matrix &mat)
{
    return matrix(elems - mat.elems);
}

matrix &matrix::operator+=(matrix &mat)
{
    elems += mat.elems;
    factored = FALSE;

    return *this;
}

matrix &matrix::operator-=(matrix &mat)
{
    elems -= mat.elems;
    factored = FALSE;

    return *this;
}


// 
// Element-wise operations:
//
matrix matrix::operator*(fract val)
{
    return matrix(elems * val);
}

matrix matrix::operator/(fract val)
{
    return matrix(elems / val);
}

matrix matrix::operator+(fract val)
{
    return matrix(elems + val);
}

matrix matrix::operator-(fract val)
{
    return matrix(elems - val);
}

matrix &matrix::operator+=(fract val)
{
    elems += val;
    factored = FALSE;

    return *this;
}

matrix &matrix::operator-=(fract val)
{
    elems -= val;
    factored = FALSE;

    return *this;
}

matrix &matrix::operator*=(fract val)
{
    elems *= val;
    factored = FALSE;

    return *this;
}

matrix &matrix::operator/=(fract val)
{
    elems /= val;
    factored = FALSE;

    return *this;
}


// 
// Matrix-vector
//
fract_vector matrix::operator*(fract_vector &vec)
{
    return (elems * vec);
}


//
// Other useful functions:
//    
void matrix::ident(int n)
{
    elems.ident(n);
    factored = FALSE;
}

matrix matrix::transpose()
{
    return matrix(elems.transpose());
}


//
// Linear Algebra Operations
//

boolean matrix::is_pivot(int i)
{ 
    factor();
    return pivot_cols[i];
}


int matrix::rank()
{
    int val = 0;
    int cols = elems.n();

    factor();
    for (int i = 0; i < cols; i++) 
        if (pivot_cols[i]) val++;

    return (val);
}


void matrix::factor()
{
    if (factored) return;

    int rows = m();
    int cols = n();

    clear_decomp();
    lu_decomp = fract_matrix(0, cols);  // Say rows are size 0 so no storage 
    lu_decomp.rows = rows;              // is allocated.

    interchanges = new int[rows];
    pivot_cols = new boolean[cols];

    for (int r = 0; r < rows; r++)
        interchanges[r] = r;

    for (int j = 0; j < cols; j++) {
	factor_col_and_insert(j);
    }

    factored = TRUE;
}

matrix matrix::inverse()
{
    int rows = elems.m();
    int cols = elems.n();

    assert_msg(rows == cols, 
        ("Matrix of size %d x %d is not square", rows, cols));

    factor();

    int i;
    for (i = 0; i < m(); i++) {
        assert_msg(pivot_cols[i], 
            ("Matrix is singular, col %d is not a pivot col", i));
    }

    fract_matrix result(0, cols);         // Say rows are size 0 so no storage 
    result.rows = rows;                   // is allocated.

    for (i = 0; i < rows; i++) {
        fract_vector vec(rows);
	vec[i] = fract(1);

	apply_L(&vec, cols);
	boolean valid;
	fract_vector solve_col = backsolve_U(vec, -1, &valid);
	assert(valid);

        result.col_list[i] = solve_col;
    }

    return matrix(result);
}

vector_space matrix::kernel()
{
    int rows = m();
    int cols = n();

    factor();

    vector_space result(cols);
    fract_vector zeros(rows);
    
    for (int free_var = 0; free_var < cols; free_var++) {
        // the kernel has a dimension for each non-pivot column
        //
        if (!pivot_cols[free_var]) {
	    boolean valid;
            fract_vector solve_col = backsolve_U(zeros, free_var, &valid);
            assert(valid);
            valid = result.insert(solve_col);
            assert(valid);
        }
    }

    return result;
}

fract_vector matrix::particular_solution(const fract_vector &vec,
                                         boolean *valid)
{
    int cols = n();
    factor();

    fract_vector new_vec(vec);

    apply_L(&new_vec, cols);
    fract_vector result = backsolve_U(new_vec, -1, valid);

    return result;
}


vector_space matrix::range()
{
    return (vector_space(*this));
}


vector_space matrix::domain()
{
    matrix trans = transpose();
    return (vector_space(trans));
}


matrix matrix::operator%(integer_row &rw)
{
    return matrix(elems % rw);
}

matrix matrix::del_row(int i, int j)
{
    return matrix(elems.del_row(i,j));
}


matrix matrix::del_col(int i, int j)
{
    return matrix(elems.del_col(i,j));
}


matrix matrix::del_columns(integer_row &rw)
{
    return matrix(elems.del_columns(rw));
}


matrix matrix::insert_col(int i)
{
    return matrix(elems.insert_col(i));
}

matrix matrix::swap_col(int i, int j)
{
    return matrix(elems.swap_col(i,j));
}

matrix matrix::swap_row(int i, int j)
{
    return matrix(elems.swap_row(i,j));
}

fract_vector matrix::get_row(int i)
{
    return elems.get_row(i);
}

fract_vector matrix::get_col(int i)
{
    return elems.get_col(i);
}

void matrix::set_col(int i, fract_vector &vec)
{
    elems.set_col(i, vec);
    factored = FALSE;
}

void matrix::set_row(int i, fract_vector &vec)
{
    elems.set_row(i, vec);
    factored = FALSE;
}

matrix matrix::resize_offset(int r1, int r2, int c1, int c2, int fill)
{
    return matrix(elems.resize_offset(r1, r2, c1, c2, fill));
}

matrix matrix::r_merge(matrix &mat)
{
    return matrix(elems.r_merge(mat.elems));
}


matrix matrix::c_merge(matrix &mat)
{
    return matrix(elems.c_merge(mat.elems));
}

immed_list *matrix::cvt_immed_list()
{
    return elems.cvt_immed_list();
}

matrix cvt_matrix(immed_list *il)
{
    return matrix(cvt_fract_matrix(il));
}

void matrix::print(FILE *fp)
{
    elems.print(fp);
}

void matrix::print_full(FILE *fp)
{
    fprintf(fp, "Elements:\n");
    elems.print(fp);

    if (factored) {
	fprintf(fp, "LU decomp:\n");
	lu_decomp.print(fp);

	assert(interchanges);
	fprintf(fp, "Interchanges [");
	int i;
	for (i = 0; i < lu_decomp.m(); i++)
	    fprintf(fp, "%s%d", (i == 0 ? "" : " "), interchanges[i]);
	fprintf(fp, "]\n");

	assert(pivot_cols);
	fprintf(fp, "Pivot Columns [");
	for (i = 0; i < lu_decomp.n(); i++)
	    fprintf(fp, "%s%d", (i == 0 ? "" : " "), pivot_cols[i]);
	fprintf(fp, "]\n");
    }

    else {
	fprintf(fp, "Not factored\n");
    }
}

void matrix::clear()
{
    elems.clear();
    clear_decomp();
}
