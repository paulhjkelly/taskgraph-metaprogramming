/*  Variable Definition Implementation */

/*  Copyright (c) 1994 Stanford University

    All rights reserved.

    This software is provided under the terms described in
    the "suif_copyright.h" include file. */

#include <suif_copyright.h>

#define _MODULE_ "libsuif.a"

#pragma implementation "vardef.h"

#define RCS_BASE_FILE vardef_cc

#include "suif1.h"

RCS_BASE(
    "$Id$")


/*
 *  Create a new variable definition.
 */

var_def::var_def (var_sym *v, int a)
    : align(a), var(v), table(0)
{
}


/*
 *  Write the var_def fields onto an annotation.  This method is used when
 *  writing out symbol tables.
 */

annote *
var_def::cvt_to_annote ()
{
    annote *an = new annote(k_var_def);
    an->immeds()->append(immed(variable()->sym_id()));
    an->immeds()->append(immed(alignment()));
    return an;
}


/*
 *  Create a var_def and read its fields from an annotation.  (This doesn't
 *  really have to be separate from the "cvt_from_annote" method, but this
 *  way it works just like sym_nodes.)
 */

var_def *
var_def::scan_from_annote (annote *an, base_symtab *symtab)
{
    var_def *result = new var_def;
    result->cvt_from_annote(an, symtab);
    return result;
}


/*
 *  Read the fields of a var_def from an annotation.
 */

void
var_def::cvt_from_annote (annote *an, base_symtab *symtab)
{
    var = (var_sym *)symtab->lookup_sym_id(an->immeds()->pop().unsigned_int());
    align = an->immeds()->pop().integer();
}


void
var_def::set_variable (var_sym *v)
{
    if (parent() != NULL)
        parent()->register_def_sym_change(this, v);
    var = v;
}


/*
 *  Print a variable definition.
 */

void
var_def::print (FILE *fp, int depth)
{
    suif_indent(fp, depth);
    variable()->print(fp);
    fprintf(fp, " align=%d ", alignment());
    print_annotes(fp, depth+1);
    putc('\n', fp);
}


/*
 *  Since var_defs cannot be cloned by themselves and since they are never
 *  referenced by other objects, this method is only used when cloning symbol
 *  tables.  It doesn't make much sense in the current implementation, but it
 *  allows var_defs to be handled in the same way as symbols and types.
 */

var_def *
var_def::clone_helper (replacements *r)
{
    /* check if this var_def has been replaced */
    var_def_list_iter old_vdli(&r->olddefs);
    var_def_list_iter new_vdli(&r->newdefs);
    while (!old_vdli.is_empty()) {
	assert(!new_vdli.is_empty());

	var_def *old_def = old_vdli.step();
	var_def *new_def = new_vdli.step();

	/* return the replacement */
	if (old_def == this) return new_def;
    }

    return this;
}

