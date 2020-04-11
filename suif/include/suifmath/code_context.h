/* file "code_context.h" */

/*  Copyright (c) 1994 Stanford University

    All rights reserved.

    This software is provided under the terms described in
    the "suif_copyright.h" include file. */

#include <suif_copyright.h>

#pragma interface

#ifndef CODE_CONTEXT_H
#define CODE_CONTEXT_H

class code_context_annote;


/************************************************************************
 **** Methods to add context to a SUIF program
 ************************************************************************/
code_context_annote * include_context(tree_node * tn, char * nm, named_symcoeff_ineq & ctxt);
code_context_annote * new_context(tree_node * tn, char * nm, named_symcoeff_ineq & ctxt);
void clear_context(tree_node * tn, char * nm = NULL, boolean recur = TRUE);


#define MAX_CUSTOM           128


/************************************************************************
 **** Driver to extract the context out of a suif program
 ************************************************************************/
class code_context {
    boolean inc_for;
    boolean inc_if;
    boolean inc_all_custom;
    const char * custom[MAX_CUSTOM];
    int ncustom;

public:
    code_context(boolean include_for=TRUE, boolean include_if=FALSE, boolean include_all_custom=FALSE);

    void for_ctxt(boolean b)                        { inc_for = b; }
    void if_ctxt(boolean  b)                        { inc_if = b; }

    void custom_ctxt_all();
    void custom_ctxt_none();
    void custom_ctxt_use(const char * nm);
    void custom_ctxt_dont_use(const char * nm);

    named_symcoeff_ineq get_context(tree_node *);
    named_symcoeff_ineq get_context(tree_node *, named_symcoeff_ineq &);
    named_symcoeff_ineq get_context(tree_node *, named_symcoeff_ineq *);
private:
    boolean is_custom(const char * nm);
    
};



/************************************************************************
 **** The annotation that'll keep the contexts 
 ************************************************************************/
struct code_context_annote {
    static const char * k_annote;
    named_symcoeff_ineq ineq;
    const char * nm;

    code_context_annote()                        { nm = NULL; }
    code_context_annote(const char * n, named_symcoeff_ineq & i);

    const char * name()                                 { return nm; }
    named_symcoeff_ineq & context()               { return ineq; }

    void set_context(named_symcoeff_ineq & i)     { ineq = i; }
    void print(FILE *fp = stdout);

    void init(immed_list * il);
    immed_list * create();
};

    
void code_context_annote_print(FILE * fp, const char * /* name */, void * data);
void code_context_annote_free(void *data);
immed_list * code_context_annote_to(const char *name, void *data);
void * code_context_annote_from(const char *name, immed_list *il, suif_object *);


#endif
