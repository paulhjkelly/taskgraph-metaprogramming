/* file "fract_matrix.h" */

/*  Copyright (c) 1994,95 Stanford University

    All rights reserved.

    This software is provided under the terms described in
    the "suif_copyright.h" include file. */

#include <suif_copyright.h>

//
// Two dimensional fractional matrix class.
//
// Each matrix is made up of an array of fract_vectors, where each
// fract_vector is a column of the matrix.
//

#ifndef FRACT_MATRIX_H
#define FRACT_MATRIX_H

#pragma interface

#include <suif1.h>
#include "int_matrix.h"
#include "fract.h"
#include "fract_vector.h"

RCS_HEADER(fract_matrix_h, "$Id$")

class matrix;

class fract_matrix {
friend class matrix;
private:
    int rows;
    int cols;
    fract_vector *col_list;

    void create_columns();                     // create fract_vectors for cols
    void copy_fract_matrix(const fract_matrix &mat); // copy mat into this
    void copy_int_matrix(const integer_matrix &mat);
                                               // copy integer_matrix into this

public:
    fract_matrix();
    fract_matrix(int r, int c);
    fract_matrix(const integer_matrix &mat);
    fract_matrix(const integer_matrix &mat, int div);
    fract_matrix(const fract_matrix &mat);
    fract_matrix(const fract_matrix &mat, int c);
      // (c <= mat.n()) result is first c cols of mat
      // (c > mat.n()) result is mat, with (c - mat.m()) cols of 0's appended

    ~fract_matrix() { clear(); }

    int m() const { return rows; }
    int n() const { return cols; }

    fract &elt(int r, int c)
        { assert (r >= 0 && r < rows && c >= 0 && c < cols);
          return col_list[c][r];  }

    const fract &rc(int r, int c) const
        { assert (r >= 0 && r < rows && c >= 0 && c < cols);
          return col_list[c][r];  }

    //
    // Equality and assignment operators:
    //
    boolean operator==(const fract_matrix &mat) const;
    boolean operator!=(const fract_matrix &mat) const
      { return (!((*this) == mat)); }
    fract_matrix & operator=(const fract_matrix &mat);

    //
    // Matrix-Matrix operations:
    //
    fract_matrix operator+(const fract_matrix &mat) const;
    fract_matrix operator-(const fract_matrix &mat) const;
    fract_matrix operator*(const fract_matrix &mat) const;
    fract_matrix & operator+=(const fract_matrix &mat);
    fract_matrix & operator-=(const fract_matrix &mat);

    //
    // Element-wise operations:
    //
    fract_matrix operator*(fract val) const;
    fract_matrix operator/(fract val) const;
    fract_matrix operator+(fract val) const;
    fract_matrix operator-(fract val) const;

    fract_matrix & operator+=(fract val);
    fract_matrix & operator-=(fract val);
    fract_matrix & operator*=(fract val);
    fract_matrix & operator/=(fract val);

    //
    // Matrix-vector
    //
    fract_vector operator*(const fract_vector &vec) const;

    //
    // Other useful functions:
    //
    void ident(int n);     // overwrites 'this' with an nxn identity matrix
    void ident()                 { assert(rows == cols); ident(rows); }

    fract_matrix transpose() const;
                           // returns transpose, does not modify 'this'

    //
    // General utilities
    //
    fract_matrix  operator%(const integer_row &rw) const;
                                               // shuffle each column:
                                               //   ret[x,i] = this[rw[x],i]

    fract_matrix del_row(int i, int j) const; // deletes rows i to j, inclusive
    fract_matrix del_row(int i) const   { return del_row(i, i); }


    fract_matrix del_col(int i, int j) const; // deletes cols i to j, inclusive
    fract_matrix del_col(int i) const   { return del_col(i, i); }

    fract_matrix del_columns(const integer_row &rw) const;
                                                // delete cols w/ rw as mask
    fract_matrix insert_col(int i) const;       // insert new col at pos i
    fract_matrix swap_col(int i, int j) const;  // switches cols i and j
    fract_matrix swap_row(int i, int j) const;  // switches rows i and j
    fract_vector get_row(int i) const;
    fract_vector get_col(int i) const;
    void set_row(int i, const fract_vector &vec);
    void set_col(int i, const fract_vector &vec);

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
    fract_matrix resize_offset(int r1, int r2, int c1, int c2, int fill = 0)
            const;
    fract_matrix resize(int r1, int r2, int c1, int c2, int fill = 0)
        { return resize_offset(r1, r2-rows, c1, c2-cols, fill); }

        // c_merge returns the (this->m() x (this->n() + mat.n()))
        //   matrix formed by merging 'this' and mat, rowwise
        // r_merge returns the ((this->m() + mat.m()) x this->n())
        //   matrix formed by merging m1 and m2, columnwise.
        //
    fract_matrix r_merge(const fract_matrix &mat) const;
    fract_matrix c_merge(const fract_matrix &mat) const;

    fract_matrix operator|(const fract_matrix &mat) const
      { return c_merge(mat); }
    fract_matrix operator&(const fract_matrix &mat) const
      { return r_merge(mat); }

    immed_list *cvt_immed_list() const;
    void print(FILE *fp = stdout) const;
    void clear();
};

fract_matrix cvt_fract_matrix(immed_list *il);

#endif /* FRACT_MATRIX_H */
