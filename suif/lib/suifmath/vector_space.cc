/* file "vector_space.cc" */

/*  Copyright (c) 1994,95 Stanford University

    All rights reserved.

    This software is provided under the terms described in
    the "suif_copyright.h" include file. */

#include <suif_copyright.h>

#pragma implementation "vector_space.cc"

#define RCS_BASE_FILE vector_space_cc

#include "vector_space.h"
#include "matrix.h"

RCS_BASE("$Id$")


/* Private member functions */


void vector_space::clear_basis()
{
    if (!basis_vectors) return;

    while (!basis_vectors->is_empty()) {
	delete (basis_vectors->pop());
    }

    delete basis_vectors;
    basis_vectors = NULL;
}


/* Public member functions */

//
// Constructors
//

vector_space::vector_space()
{
    basis_vectors = new fract_vector_list(); 
    col_matrix = NULL;
    space = 0;
}

vector_space::vector_space(int dim)
{
    basis_vectors = new fract_vector_list(); 
    col_matrix = NULL;
    space = dim;
}

vector_space::vector_space(matrix &m)
{
    basis_vectors = NULL;
    col_matrix = new matrix(m);
    space = m.m();  // space is the number of rows

    beautify();
}

vector_space::vector_space(const vector_space &vs)
{
    space = vs.space_dimension();
    if (vs.basis_vectors) {
        assert(!vs.col_matrix);
	col_matrix = NULL;
	basis_vectors = new fract_vector_list();

        fract_vector_list_iter it(vs.basis_vectors); 
	while (!it.is_empty()) {
	    fract_vector *vec = it.step();
	    basis_vectors->append(new fract_vector(*vec));
	}
    } 

    else {
        assert(vs.col_matrix);
	basis_vectors = NULL;
	col_matrix = new matrix(*(vs.col_matrix));
    }
}


void vector_space::make_basis()
{
    if (basis_vectors != NULL) return;
    assert(col_matrix);
    assert(col_matrix->m() == space);

    // Use matrix elems

    basis_vectors = new fract_vector_list();

    for (int j = 0; j < col_matrix->n(); j++) {
	fract_vector *vec = new fract_vector(space);

	for (int i = 0; i < col_matrix->m(); i++) {
   	    vec->elt(i) = col_matrix->elt(i,j);
	}
        basis_vectors->append(vec);
    }

    delete col_matrix;
    col_matrix = NULL;
}

void vector_space::make_matrix()
{
    if (col_matrix != NULL) return;
    assert(basis_vectors);

    int count = dimensionality();
    assert(count <= space);

    col_matrix = new matrix(space, count);
    fract_vector_list_iter it(basis_vectors);

    for(int j = 0; j < count; j++) {
	assert(!it.is_empty());
	fract_vector *vec = it.step();
	for (int i = 0; i < space; i++) {
	   col_matrix->elt(i,j) = vec->elt(i);
	}
    }
    col_matrix->factor();

    clear_basis();
}


//
// Operators
//

void vector_space::operator=(const vector_space & vs)
{
    clear();

    space = vs.space_dimension();
    if (vs.basis_vectors) {
        assert(!vs.col_matrix);
	col_matrix = NULL;
	basis_vectors = new fract_vector_list();

        fract_vector_list_iter it(vs.basis_vectors); 
	while (!it.is_empty()) {
	    fract_vector *vec = it.step();
	    basis_vectors->append(new fract_vector(*vec));
	}
    } 

    else {
        assert(vs.col_matrix);
	basis_vectors = NULL;
	col_matrix = new matrix(*(vs.col_matrix));
    }
}


boolean vector_space::operator==(const vector_space &vs) const
{
    if (space != vs.space) return FALSE;

    vector_space op1(*this);
    vector_space op2(vs);

    op1.beautify();  // makes basis
    op2.beautify();
    assert(op1.basis_vectors && op2.basis_vectors);

    if (op1.dimensionality() != op2.dimensionality()) return FALSE;

    fract_vector_list_iter it(op1.basis_vectors);
    while (!it.is_empty()) {
	fract_vector *vec = it.step();
        if (!op2.in(*vec)) return FALSE;
    }

    return TRUE;
}


int vector_space::operator+=(vector_space & vs)
{
    assert(space == vs.space_dimension());
    int result = 0;

    vs.make_basis();
    fract_vector_list_iter it(vs.basis());

    while(!it.is_empty()) {
        fract_vector *vec = it.step();
	result += insert(*vec);
    }

    return result;
}

void vector_space::operator-=(vector_space & vs)
{
    assert(space == vs.space_dimension());
    make_basis();
    vs.make_basis();
    vector_space temp_vs(vs);

    fract_vector_list temp_basis;
    fract_vector_list_iter it(basis_vectors);
    while(!it.is_empty()) {
	fract_vector *vec = it.step();
        temp_basis.append(new fract_vector(*vec));
    }

    matrix proj = temp_vs.proj_matrix();

    vector_space result(space);
    while (!temp_basis.is_empty()) {
        fract_vector *vec = temp_basis.pop();
	*vec -= proj * (*vec);
        vec->reduce_magnitude();

	if(!vec->is_zero()) {
	    result.insert(*vec);
	}

	delete vec;
    }
    *this = result;
}

//
// Let *this = V, and vs = W.  V is the column space of some matrix A,
// and W is the column space of some matrix B.  Then V + W is the column
// space of D = [A B]. 
// First form the matrix D, and find it's kernel.  
// The basis of ker D leads to a basis for V * W:
//
// Given a vector x in ker D, v in V, w in W and dim(V) = k, dim(W) = l,
// we can write Dx = 0 as follows:
// 
// x_1v_1 + ... + x_kv_k + x_(k+1)w_1 + ... + x_(k+l)w_l = 0
// x_1v_1 + ... + x_kv_k = -x_(k+1)w_1 - ... - x_(k+l)w_l
//
// Since the left-side is in V and the right-side is in W, and they are equal
// this represents a vector in V * W.
//
// See Strang, Third Edition, pp. 199-201.
//
void vector_space::operator*=(vector_space & vs)
{
    assert(space == vs.space_dimension());
    int count1 = dimensionality();
    int count2 = vs.dimensionality();

    // Form the matrix D, and find its kernel:
    matrix D = get_matrix()->c_merge(*(vs.get_matrix()));
    vector_space kernel_D = D.kernel();

    vector_space result(space);
    fract_vector new_vec(space);

    // For each vector in the kernel of D find corresponding vector that's
    // in both V and W:
     while (!kernel_D.basis()->is_empty()) {
 	fract_vector *cur_vec = kernel_D.basis()->pop();
	assert(cur_vec);
	fract_vector_list_iter it(vs.basis());
	for (int i = 0; i < count2; i++) {
	    assert(!it.is_empty());
	    fract f = cur_vec->elt(count1+i);
	    fract_vector vec = f * *(it.step());
	    new_vec += vec;
	}
	assert(it.is_empty());
	assert(!result.in(new_vec));
	result.insert(new_vec);
        delete cur_vec;
    }
    *this = result;
}

vector_space vector_space::operator+(vector_space & vs)
{
    vector_space result = *this;
    result += vs;
    return result;
}

vector_space vector_space::operator-(vector_space & vs)
{
    vector_space result = *this;
    result -= vs;
    return result;
}

vector_space vector_space::operator*(vector_space & vs)
{
    vector_space result = *this;
    result *= vs;
    return result;
}

// Is "vs" a subset of this?
//
boolean vector_space::operator>=(vector_space &vs)
{
    return (vs <= *this);
}

// Is this a subset of "vs"?
//
boolean vector_space::operator<=(vector_space &vs)
{
    make_basis();
    fract_vector_list_iter fvli(basis_vectors);
    while (!fvli.is_empty()) {
	fract_vector *cur_vec = fvli.step();
	if (!vs.in(*cur_vec)) return FALSE;
    }

    return TRUE;
}


//
// Utilities:
//

void vector_space::clear()
{
    if (basis_vectors) 	clear_basis();

    if (col_matrix) delete col_matrix;
    col_matrix = NULL;
}


boolean vector_space::perpendicular(fract_vector & vec)
{
    make_basis();
    fract_vector_list_iter it(basis_vectors);
    while(!it.is_empty()) {
	fract_vector *cur_vec = it.step();
	if (cur_vec->dot(vec) != 0) return FALSE;
    }

    return TRUE;
}

boolean vector_space::space_full() 
{
    beautify();
    return (dimensionality() == space);
}

//
// The projection matrix for the vector space.
// Let this be the column space for a matrix A.  Then the projection 
// matrix P is: A*((A^T*A)-1)*A^T
// See Strang, Third Edition, p. 158
//
matrix vector_space::proj_matrix()
{
    make_matrix();

    // Uses matrix elems, *not* lu_decomp
    matrix A(*col_matrix);
    
    matrix A_trans = A.transpose();
    matrix m = A_trans * A;

    matrix m_inverse = m.inverse();
    matrix A_m_inverse = A * m_inverse;
    matrix result = A_m_inverse * A_trans;
    return result;
}

void vector_space::beautify()
{
    make_matrix();

    assert(col_matrix->m() == space);

    fract_vector elem_vec(space);

    vector_space temp_space(space);
    for (int j = 0; j < space; j++) {
        for(int i = 0; i < space; i++) {
 	    elem_vec[i] = fract(i == j);
	}
	if (in(elem_vec)) temp_space.insert(elem_vec);  // insert copies vec
    }

    *this -= temp_space;

    temp_space.make_basis();
    while (!temp_space.basis_vectors->is_empty()) {
	fract_vector *vec = temp_space.basis_vectors->pop();
	insert(*vec);
	delete vec;
    }
}


void vector_space::reduce_magnitude()
{
    make_basis();
    fract_vector_list_iter it(basis_vectors);
    while(!it.is_empty()) {
        it.step()->reduce_magnitude();
    }
}


vector_space &vector_space::fill_space(int dim)
{
    if (dim <= space) return *this;

    if (col_matrix) {
	matrix temp_matrix = col_matrix->resize(0, dim, 0, col_matrix->n());
	matrix *new_matrix = new matrix(temp_matrix);
	delete col_matrix;
	col_matrix = new_matrix;
    }

    else {
	fract_vector_list *new_basis = new fract_vector_list();
	int num_vectors = basis_vectors->count();

	for (int i = 0; i < num_vectors; i++) {
	    fract_vector *old_v = basis_vectors->pop();
	    fract_vector *new_v = new fract_vector(dim);

	    int j;
	    for (j = 0; j < space; j++) new_v->elt(j) = old_v->elt(j);
	    for (j = space; j < dim; j++)   new_v->elt(j) = fract(0);
	    new_basis->append(new_v);
	    delete old_v;
	}
	clear_basis();
	basis_vectors = new_basis;
    }

    space = dim;
    return *this;
}

vector_space &vector_space::shrink_space(int dim)
{
    if (dim >= space) return *this;

    if (col_matrix) {
	matrix temp_matrix = col_matrix->resize(0, dim, 0, col_matrix->n());
	matrix *new_matrix = new matrix(temp_matrix);
	delete col_matrix;
	col_matrix = new_matrix;
    }

    else {
	fract_vector_list *old_basis = basis_vectors;
	int num_vectors = basis_vectors->count();
	clear_basis();

	for (int i = 0; i < num_vectors; i++) {
	    fract_vector *old_v = old_basis->pop();
	    fract_vector *new_v = new fract_vector(old_v->resize(0, dim));
	    insert(*new_v);
	    delete old_v;
	}
    }

    space = dim;
    return *this;
}


// Factors column 'vec' with lu_decomp of vector_space's matrix.  
// If the resulting factored column is dependent (i.e. has zero
// in the cur_pivot_row) then it's in the vector_space
//
boolean vector_space::in(fract_vector & vec)
{
    make_matrix();  
    int rows = col_matrix->m();
    int cols = col_matrix->n();
    assert(rows == space);

    // Uses lu_decomp of matrix

    // Check for null
    int i;
    for(i = 0; i < rows; i++) {
        if (vec[i] != fract(0)) break;
    }
    if (i == rows) return TRUE; // Null vector in all vector_spaces

    fract_vector new_vec = vec;
    int cur_pivot_row = 0;

    for(i = 0; i < cols; i++) {
        cur_pivot_row += (int) col_matrix->is_pivot(i);
    }
    if (cur_pivot_row == space) return TRUE;  // Vector space spans full space

    col_matrix->factor();
    col_matrix->factor_col(&new_vec, cols, cur_pivot_row);

    return (new_vec[cur_pivot_row] == fract(0));
}

boolean vector_space::insert(fract_vector & vec)
{
    assert_msg(vec.n() == space, 
    ("Cannot insert vector of length %d into vector space of dim %d",
     vec.n(), space));

    if (!in(vec)) {
	make_basis();
	fract_vector *new_vec = new fract_vector(vec);
	basis_vectors->append(new_vec);
	return TRUE;
    }

    return FALSE;
}

vector_space vector_space::resize(int n1, int n2, fract fill)
{
    vector_space result(n2-n1);
    fract_vector_list *vectors = basis();
    fract_vector_list_iter fvli(vectors);
    while (!fvli.is_empty()) {
	fract_vector *vec = fvli.step();
	fract_vector new_vec = vec->resize(n1, n2, fill);
	result.insert(new_vec);
    }

    return result;
}

void vector_space::print(FILE * fp)
{
    make_basis();

    int count=0;
    fract_vector_list_iter it(basis_vectors);

    fprintf(fp, "{");
    while(!it.is_empty()) {
        if (count++) fprintf(fp, ",");
	it.step()->print(fp);
    }
    fprintf(fp, "}\n");
}
