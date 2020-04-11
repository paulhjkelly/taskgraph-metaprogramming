/* file "vector_space.h" */

/*  Copyright (c) 1994,95 Stanford University

    All rights reserved.

    This software is provided under the terms described in
    the "suif_copyright.h" include file. */

#include <suif_copyright.h>

/* 
   Vector spaces (with fractional values)
*/

#ifndef VECTOR_SPACE_H
#define VECTOR_SPACE_H

#pragma interface

#include <suif1.h>
#include "fract_vector.h"

class matrix;   // from "matrix.h"

RCS_HEADER(vector_space_h, "$Id$")

// A vector space is represented as a list of independent vectors, or 
// the column vectors in a matrix.

class vector_space {
private:
    int space;                          // dimension of basis_vectors
    fract_vector_list *basis_vectors;
    matrix *col_matrix;
    void make_basis();
    void make_matrix();

    void clear_basis();

public:
    vector_space();
    vector_space(int dims);
    vector_space(const vector_space &vs);
    vector_space(matrix &m);
    ~vector_space()            { clear(); }

    fract_vector_list *basis() { make_basis(); return basis_vectors; }
    matrix *get_matrix()       { make_matrix(); return col_matrix; }
    int dimensionality()       { make_basis(); return basis_vectors->count(); }
    int space_dimension() const { return space; }

    //
    // Operators
    //
    // Addition:  Given two spaces V and W, then X = V + W is the vector 
    // space made up of all possible combinations x = v + w, 
    // where v is an element of V and w is an element of W.
    // Both V and W must be subspaces of the same space (i.e. both V
    // and W must have space_dimension() equal.)
    // e.g. {(1,0)} + {(1,1)} = {(1,0),(0,1)}
    // 
    // Subtraction:  Given two spaces V and W, then X = V - W is the 
    // vector space V with the common directions removed.
    // Both V and W must be subspaces of the same space (i.e. both V
    // and W must have space_dimension() equal.)
    // e.g. {(1,0),(0,1)} - {(1,1)} = {(1,-1)}
    // 
    // Multiplication (Intersection): Given two spaces V and W, then 
    // X = V * W is the space whose vectors are in both V and W.
    // Both V and W must be subspaces of the same space (i.e. both V
    // and W must have space_dimension() equal.)
    // e.g. {(1,0)} * {(0,1)} = {}.
    //
    void operator=(const vector_space & vs);
    boolean operator==(const vector_space &vs) const;
    boolean operator!=(const vector_space &vs) const
      { return !((*this) == vs); }

    int operator+=(vector_space &vs);   // returns num vectors inserted
    void operator-=(vector_space &vs); 
    void operator*=(vector_space &vs);
    vector_space operator+(vector_space &vs);
    vector_space operator-(vector_space &vs);
    vector_space operator*(vector_space &vs);
    boolean operator <= (vector_space &vs);   // subset
    boolean operator >= (vector_space &vs);   // superset

    //
    // Utilities:
    //
    void clear();
    boolean perpendicular(fract_vector &vec);
    boolean space_full();    // Does the vector_space span the entire space?
    matrix proj_matrix();
    void beautify();
    void reduce_magnitude();
    vector_space &fill_space(int dim);   // increase space dimension to 'dim'
    vector_space &shrink_space(int dim); // shrink space dimension to 'dim'

    boolean in(fract_vector &vec);	 // returns TRUE if vec in vector_space
    boolean insert(fract_vector &vec);	 // returns TRUE if vec is inserted
	                                 //   inserts a copy of vec

    // Resize all basis vector using elems n1..n2 of the vectors. Extra elems 
    // are appended and set to 'fill'.
    vector_space resize(int n1, int n2, fract fill = 0);

    void print(FILE *fp = stdout);
};

#endif /* VECTOR_SPACE_H */
