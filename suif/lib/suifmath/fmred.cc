/* file "fmred.cc" */

/*  Copyright (c) 1994 Stanford University

    All rights reserved.

    This software is provided under the terms described in
    the "suif_copyright.h" include file. */

#include <suif_copyright.h>

#define _MODULE_ "libsuifmath.a"
#pragma implementation "fmred.h"
#pragma implementation "nametable.h"

#include <cstdio>
#include <cstring>
#include <suif1.h>
#include "suifmath.h"

int saman_flag_f = 0;


poly_iterator * Iterator = 0;
name_store * nmstore = 0;


fmred::fmred()
{
    Iterator = 0;
}


void fmred::set(lin_ineq & in)
{
    Iterator = new poly_iterator(in);

}


void fmred::set(column_stat in[])
{
    assert(Iterator);
    Iterator->set_sort_order(in);

}


lin_ineq fmred::reduce()
{
    assert(Iterator);
    const lin_ineq * T;
    T = Iterator->get_iterator();

    T = Iterator->delete_irrelevant_variables();

    T = Iterator->reduce_extra_constraints_ptr();

    return *T;
}




name_store::~name_store() 
{ 
    int i;
    for (i=0; i<num_vars; i++) 
        delete vars[i];
    for (i=0; i<num_params; i++) 
        delete params[i];
}

void name_store::init(name_store & nm) 
{ 
    num_vars   = nm.num_vars;
    num_params = nm.num_params;
    aux        = nm.aux; 
    int i;
    for (i=0; i<num_vars; i++) {
        vars[i] = new char[strlen(nm.vars[i])+1];
        strcpy(vars[i], nm.vars[i]);
        varkind[i] = nm.varkind[i];
    }
    for(i=0; i<num_params; i++) {
        params[i] = new char[strlen(nm.params[i])+1];
        strcpy(params[i], nm.params[i]);
        paramkind[i] = nm.paramkind[i];
    }
}
