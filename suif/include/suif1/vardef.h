/*  Variable Definitions */

/*  Copyright (c) 1994 Stanford University

    All rights reserved.

    This software is provided under the terms described in
    the "suif_copyright.h" include file. */

#include <suif_copyright.h>

#ifndef VARDEF_H
#define VARDEF_H

#pragma interface

RCS_HEADER(vardef_h,
    "$Id$")


/*
 *  If a variable is not extern and its storage class is not automatic, it
 *  must have a separate var_def object in the symbol table for the scope
 *  where it is defined.  Note that a global variable may be declared in the
 *  global symtab, but only defined in one of the file symtabs.  The var_def
 *  contains the alignment in bits, and any initial data is stored in
 *  annotations attached to the var_def.
 */

class var_def : public suif_object {
    friend class base_symtab;

private:
    int align;
    var_sym *var;
    base_symtab *table;

    void set_parent(base_symtab *st)	{ table = st; }

protected:
    var_def() : align(0), var(0), table(0)		{ }

    static var_def *scan_from_annote(annote *an, base_symtab *symtab);
    void cvt_from_annote(annote *an, base_symtab *symtab);
    annote *cvt_to_annote();

public:
    var_def(var_sym *v,			/* the variable */
	    int a);			/* alignment in bits */

    object_kinds object_kind()		{ return DEF_OBJ; }

    int alignment()			{ return align; }
    var_sym *variable()			{ return var; }
    base_symtab *parent()		{ return table; }

    void set_alignment(int a)		{ align = a; }
    void set_variable(var_sym *v);

    var_def *clone_helper(replacements *r);

    void print(FILE *fp = stdout, int depth = 0);
};

DECLARE_DLIST_CLASS(var_def_list, var_def*);

#endif /* VARDEF_H */
