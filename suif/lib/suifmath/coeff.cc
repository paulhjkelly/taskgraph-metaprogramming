/* file "coeff.cc" */

/*  Copyright (c) 1994 Stanford University

    All rights reserved.

    This software is provided under the terms described in
    the "suif_copyright.h" include file. */

#include <suif_copyright.h>

#define _MODULE_ "libsuifmath.a"
#pragma implementation "coeff.h"

#include <suif1.h>
#include "suifmath.h"
#include <cstring>

coeff *coeff::copy() const {
    coeff *new_coeff = new coeff;
    
    new_coeff->constant = new int[m];
    new_coeff->vars = new int[n*m];
    new_coeff->n = n; new_coeff->m = m;
    memcpy(new_coeff->constant, constant, m*sizeof(int));
    memcpy(new_coeff->vars, vars, n*m*sizeof(int));
    
    return new_coeff;
}

void coeff::print(FILE * fp) const
{
    for(int i=0; i<m; i++) {
        fprintf(fp, "%3d", constant[i]);
        for(int j=0; j<n; j++) 
            fprintf(fp, "%3d", vars[i*n+j]);
        fprintf(fp,"\n");
    }
}

