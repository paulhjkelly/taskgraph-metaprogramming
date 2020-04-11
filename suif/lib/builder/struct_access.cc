/* file "struct_access.cc" */

/*  Copyright (c) 1994 Stanford University

    All rights reserved.

    This software is provided under the terms described in
    the "suif_copyright.h" include file. */

#include <suif_copyright.h>

#define _MODULE_ "libbuilder.a"
#define BUILDERLIB

/***************************************************************************
 ***************************************************************************
 *
 *                     S U I F   C o m p i l e r  S y s t e m
 *
 *                                B U I L D E R
 *
 ***************************************************************************
 ***************************************************************************/


#include <suif1.h>
#include "builder.h"
#include "bldtypes.h"
#include <cstdlib>
#include <cstring>

struct_access::struct_access()
{
    var = NULL;
    offset = 0;
    type = NULL;
    dref_ptr = FALSE;
}


struct_access::struct_access(struct_access * sa)
{
    assert(sa);
    var = sa->var;
    offset = sa->offset;
    type = sa->type;
    dref_ptr = sa->dref_ptr;
    cp_field_list(&sa->field_list);
}


struct_access::struct_access(var_sym * base, int field)
{
    assert(base);
    type_node * t = base->type()->unqual();
    mk_st_ac(base, t, 0, field);
}


struct_access::struct_access(var_sym * base, char * fld)
{
    assert(base);
    type_node * t = base->type()->unqual();
    mk_st_ac(base, t, 0, get_field(t, fld));
}


struct_access::struct_access(struct_access * base, int field)
{
    assert(base);
    cp_field_list(&base->field_list);
    mk_st_ac(base->var, base->type->unqual(), base->offset, field);
}


struct_access::struct_access(struct_access * base, char * fld)
{
    assert(base);
    type_node * t = base->type->unqual();
    cp_field_list(&base->field_list);
    mk_st_ac(base->var, t, base->offset, get_field(t, fld));
}

struct_access::~struct_access()
{

}


int struct_access::get_field(type_node * tp,
                             char * fld)
{
    assert(fld);
    assert(tp);
    if(!tp->is_struct())
        error(et_type, "Accessing a structure of a non-structure type");

    struct_type * st = (struct_type *)tp;

    for(int i=0; ((unsigned)i)<st->num_fields(); i++)
        if(strcmp(fld, st->field_name(i)) == 0)
            return i;

    error(et_type, "Field %s is not a part of the structure", fld);
    return -1;
}



void struct_access::mk_st_ac(var_sym * v,
                             type_node * tp,
                             int off,
                             int field_nm)
{
    assert(v);
    assert(tp);

    struct_type * st = NULL;
    if(tp->is_ptr()) {
      if(!((ptr_type *)tp)->ref_type()->is_struct()) {
        error(et_type, "Accessing a structure of a non-structure type");
      } else {
	st = (struct_type *)((ptr_type *)tp)->ref_type();
	dref_ptr = TRUE;
      }
    } else if(!tp->is_struct()) {
      error(et_type, "Accessing a structure of a non-structure type");
    }  else {
      st = (struct_type *)tp;
      dref_ptr = FALSE;
    }


    if((field_nm < 0)||(((unsigned)field_nm) >= st->num_fields()))
        error(et_type, "Field %d is not a valid field\n", field_nm);

    var = v;
    type = st->field_type(field_nm);
    offset = st->offset(field_nm)+off;
    field_list.append(immed(st->field_name(field_nm)));
}


void struct_access::cp_field_list(immed_list * lst)
{
    assert(lst);
    immed_list_iter iter(lst);
    while(!iter.is_empty()) {
        immed im(iter.step());
        field_list.append(im);
    }
}



void struct_access::print() const
{
    printf("<");
    var->print(stdout);
    printf(":%d>", offset);
}
