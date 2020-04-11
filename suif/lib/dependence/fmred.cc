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
#include <suif.h>
#include <suifmath.h>
#include "dependence.h"

int debug;
int saman_flag_f = 0;


poly_iterator * Iterator;
name_store * nmstore;


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
    //BUGBUG Had to leak memory on T otherwise delete crashes 2/18/92
    lin_ineq * T;
    T = new lin_ineq(Iterator->get_iterator());

    T = new lin_ineq(Iterator->delete_irravalent_variables());

    T = new lin_ineq(Iterator->reduce_extra_constraints());
    

    return *T;
}




name_store::~name_store() 
{ 
    for (int i=0; i<num_vars; i++) 
        delete vars[i];
    for (i=0; i<num_params; i++) 
        delete params[i];
}

void name_store::init(name_store & nm) 
{ 
    num_vars   = nm.num_vars;
    num_params = nm.num_params;
    aux        = nm.aux; 
    for (int i=0; i<num_vars; i++) {
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
