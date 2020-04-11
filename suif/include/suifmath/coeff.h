/* file "coeff.h" */

/*  Copyright (c) 1994 Stanford University

    All rights reserved.

    This software is provided under the terms described in
    the "suif_copyright.h" include file. */

#include <suif_copyright.h>

#pragma interface
/*
 * this represents a set of expressions of the form vars[i][0]*x1 + ... + 
 * vars[i][n-1]*xn + constant[i] 
 */
class coeff {
public:
    int m,n;
    int *constant;
    int *vars;  
    coeff *copy() const;
    void print(FILE * fp=stdout) const;
} ;
