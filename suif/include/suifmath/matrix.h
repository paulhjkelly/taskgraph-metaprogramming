/* file "matrix.h" */

/*  Copyright (c) 1994,95 Stanford University

    All rights reserved.

    This software is provided under the terms described in
    the "suif_copyright.h" include file. */

#include <suif_copyright.h>

#ifndef MATRIX_H
#define MATRIX_H

#pragma interface

#include <suif1.h>
#include "int_matrix.h"
#include "fract.h"
#include "fract_vector.h"
#include "fract_matrix.h"
#include "vector_space.h"

RCS_HEADER(matrix_h, "$Id$")

class matrix {
friend class vector_space;
private:
    fract_matrix elems;      // The matrix itself 
    fract_matrix lu_decomp;  // LU decomp of the matrix
    int *interchanges;       // Interchanged columns of the LU decomp
    boolean *pivot_cols;     // Is the column a pivot column?
    boolean factored;        // Is the LU decomp current?

    void clear_decomp();
    void init_decomp();
    void copy_decomp(const matrix &mat);
    void copy_matrix(const matrix &mat);  // copy mat into this

    // Linear algebra support
    //
    void factor_col_and_insert(int j);
    int factor_col(fract_vector *vec, int cols, int pivot_row);
    void apply_L(fract_vector *vec, int cols);
    fract_vector backsolve_U(fract_vector &vec, int free_var, boolean *valid);
    int last_nonzero_Urow();
    
public:
    matrix();
    matrix(int r, int c);
    matrix(const integer_matrix &mat); 
    matrix(const integer_matrix &mat, int div);
    matrix(const fract_matrix &mat); 
    matrix(const matrix &mat); 
    matrix(const fract_matrix &mat, int c);  
    matrix(const matrix &mat, int c); 
      // (c <= mat.n()) result is first c cols of mat
      // (c > mat.n()) result is mat, with (c - mat.m()) cols of 0's appended

    ~matrix() { clear(); }

    int m() { return elems.m(); } 
    int n() { return elems.n(); } 

    fract &elt(int r, int c)  { return elems.elt(r,c); }

    //
    // Equality and assignment operators:
    //
    boolean operator==(const matrix &mat) const;
                                        // only checks if elems matrices equal
    boolean operator!=(const matrix &mat) const
      { return (!((*this) == mat)); }
    matrix & operator=(const matrix &mat);

    //
    // Matrix-Matrix operations:
    //
    matrix operator+(matrix &mat);
    matrix operator-(matrix &mat);
    matrix operator*(matrix &mat);
    matrix & operator+=(matrix &mat);
    matrix & operator-=(matrix &mat);

    // 
    // Element-wise operations:
    // 
    matrix operator*(fract val);
    matrix operator/(fract val);
    matrix operator+(fract val);
    matrix operator-(fract val);

    matrix & operator+=(fract val);
    matrix & operator-=(fract val);
    matrix & operator*=(fract val);
    matrix & operator/=(fract val);

    // 
    // Matrix-vector
    //
    fract_vector operator*(fract_vector &vec);

    //
    // Other useful functions:
    //    
    void ident(int n);     // overwrites 'this' with an nxn identity matrix
    void ident()               { assert(m() == n()); ident(m()); }   

    matrix transpose();    // returns transpose, does not modify 'this'

    //
    // Linear Algebra Operations
    //
    boolean is_factored()        { return factored; }
    boolean is_pivot(int i);
    int rank();
    boolean full_row_rank()      { return (m() == rank()); }
    boolean full_col_rank()      { return (n() == rank()); }

    void factor();
    matrix inverse();
    vector_space kernel();
    fract_vector particular_solution(const fract_vector &vec, boolean *valid);
    vector_space range();
    vector_space domain();

    //
    // General utilities
    //
    matrix operator%(integer_row &rw);   // shuffle each column:
                                         //   ret[x,i] = this[rw[x],i]

    matrix del_row(int i, int j);    // deletes rows i to j, inclusive
    matrix del_row(int i)   { return del_row(i, i); }
 

    matrix del_col(int i, int j);    // deletes cols i to j, inclusive
    matrix del_col(int i)   { return del_col(i, i); }

    matrix del_columns(integer_row &rw);  // delete cols w/ rw as mask
    matrix insert_col(int i);             // insert new col at pos i
    matrix swap_col(int i, int j);        // switches cols i and j
    matrix swap_row(int i, int j);        // switches rows i and j
    fract_vector get_row(int i);
    fract_vector get_col(int i);
    void set_row(int i, fract_vector &vec);
    void set_col(int i, fract_vector &vec);

        // resize_offset returns a (this->m()-r1+r2 x this->n()-c1+c2) matrix.
        // If (r2 > 0) the matrix is formed with rows r1..m()-1, with r2 extra
        //   rows appended.  The elements in the appended rows are set
        //   to 'fill'.  
        // If (r2 < 0) the matrix is formed with rows r1..m()-r2. 
        // The columns are set in the same way as the rows.
        // 
        // resize returns a (r1+r2 x c1+c2) matrix.
        // The matrix is formed with rows r1..r2.  If (r2 > m()) then 
        //   r2-mm rows are appended.   The elements in the appended rows 
        //   are set to 'fill'.  
        // The columns are set in the same way as the rows.
        //
    matrix resize_offset(int r1, int r2, int c1, int c2, int fill = 0);
    matrix resize(int r1, int r2, int c1, int c2, int fill = 0) 
        { return resize_offset(r1, r2-m(), c1, c2-n(), fill); }

        // c_merge returns the (this->m() x (this->n() + mat.n()))
        //   matrix formed by merging 'this' and mat, rowwise 
        // r_merge returns the ((this->m() + mat.m()) x this->n())
        //   matrix formed by merging m1 and m2, columnwise.
        // 
    matrix r_merge(matrix &mat);
    matrix c_merge(matrix &mat);

    matrix operator|(matrix &mat) { return c_merge(mat); }
    matrix operator&(matrix &mat) { return r_merge(mat); }

    immed_list *cvt_immed_list();
    friend matrix cvt_matrix(immed_list *il);
    void print(FILE *fp = stdout);            // print elems
    void print_full(FILE *fp = stdout);       // print elems and LU decomp
    void clear();
};


#endif /* MATRIX_H */
