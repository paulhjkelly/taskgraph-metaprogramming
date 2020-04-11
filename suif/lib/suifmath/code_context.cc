/* file "code_context.cc" */
 
/*  Copyright (c) 1994 Stanford University

    All rights reserved.

    This software is provided under the terms described in
    the "suif_copyright.h" include file. */

#include <suif_copyright.h>

#define _MODULE_ "libsuifmath.a"
#pragma implementation "code_context.h"

#include <suif1.h>
#include <builder.h>
#include "suifmath.h"
#include <cstdio>
#include <cstring>

const char * code_context_annote::k_annote;

/* ################################################
   ####    code_context_annote                 ####
   ################################################ */




void * code_context_annote_from(const char *name, immed_list *il, suif_object *)
{
    code_context_annote * ret = new code_context_annote;
    
    assert(name == code_context_annote::k_annote);
    assert(il); 
    ret->init(il);
    return (void *)ret;
}

void code_context_annote::init(immed_list *il)
{
    assert(il);
    assert(il->count() >= 2);
    immed im_name((*il)[0]);
    assert(im_name.is_string());
    nm = lexicon->enter(im_name.string())->sp;
    ineq.init(*il, 1);
}


immed_list * code_context_annote_to(const char *name, void *data)
{
    assert(name ==  code_context_annote::k_annote);
    assert(data);
    code_context_annote * cca = (code_context_annote *) data;

    immed_list * im = cca->create();
    return im;
}

immed_list * code_context_annote::create()
{
    immed_list * im = ineq.cvt_immed_list();
    im->push(immed(nm));
    return im;
}
void code_context_annote_free(void *data)
{
    assert(data);
    code_context_annote * cca = (code_context_annote *) data;
    delete cca;
}


void code_context_annote_print(FILE * fp, const char * /* name */, void * data)
{
    assert(data);
    code_context_annote * cca = (code_context_annote *) data;
    cca->print(fp);
}

void code_context_annote::print(FILE *fp)
{
    fprintf(fp, "---%s---\n", nm);
    ineq.print_exp(pet_system_nl, fp);
}


code_context_annote::code_context_annote(const char * n, named_symcoeff_ineq & i)
{
    nm = lexicon->enter(n)->sp;
    ineq = i;
}


/* ################################################ 
   ################################################ */


/************************************************************************
 * 
 ************************************************************************/
code_context_annote * add_context(tree_node * tn, char * nm, named_symcoeff_ineq & ctxt)
{
    assert(tn);
    assert(nm);

    code_context_annote * cca = NULL;
    boolean found = FALSE;
    annote_list_iter iter(tn->annotes());
    while((!iter.is_empty())&&(!found)) {
	annote * an = iter.step();
	if(strcmp(code_context_annote::k_annote, an->name()) == 0) {
	    cca = (code_context_annote *)an->data();
	    if(strcmp(nm, cca->name()) == 0) {
		tn->annotes()->remove(iter.cur_elem());
		found = TRUE;
	    }
	}
    }

    if(cca == NULL) 
	cca = new code_context_annote(nm, ctxt);
    else
	cca->context() &= ctxt;

    tn->append_annote(code_context_annote::k_annote, cca);
    return cca;
}

code_context_annote * new_context(tree_node * tn, char * nm, named_symcoeff_ineq & ctxt)
{
    clear_context(tn, nm);
    return add_context(tn, nm, ctxt);
}



/************************************************************************
 * 
 ************************************************************************/
void clear_context(tree_node_list * tnl, char * nm, boolean recur)
{
    tree_node_list_iter iter(tnl);
    while(!iter.is_empty()) {
	tree_node * tn = iter.step();
	clear_context(tn, nm, recur);
    }
}


/************************************************************************
 * 
 ************************************************************************/
void clear_context(tree_node * tn, char * nm, boolean recur)
{
    annote_list_iter iter(tn->annotes());
    while(!iter.is_empty()) {
	annote * an = iter.step();
	if(strcmp(code_context_annote::k_annote, an->name()) == 0) {
	    code_context_annote * cca = (code_context_annote *)an->data();
	    if(nm == NULL)
		tn->annotes()->remove(iter.cur_elem());
	    else if(strcmp(nm, cca->name()) == 0)
		tn->annotes()->remove(iter.cur_elem());
	}
    }
    
    if(recur) {
	switch (tn->kind()) {
        case TREE_BLOCK: 
            clear_context(((tree_block *) tn)->body(), nm, recur);
            break;
            
        case TREE_LOOP: 
            clear_context(((tree_loop *) tn)->body(), nm, recur);
            break;
            
        case TREE_IF:
            clear_context(((tree_if *) tn)->then_part(), nm, recur);
            clear_context(((tree_if *) tn)->else_part(), nm, recur);
            break;
            
        case TREE_INSTR: 
	    break;
            
        case TREE_FOR: 
            clear_context(((tree_for *) tn)->landing_pad(), nm, recur);
	    clear_context(((tree_for *) tn)->body(), nm, recur);
            break;
	}
    }
    
}





/* ################################################
   ####    code_context                        ####
   ################################################ */


/************************************************************************
 * 
 ************************************************************************/
code_context::code_context(boolean include_for, boolean include_if, boolean include_all_custom)
{
    inc_for = include_for;
    inc_if = include_if;
    inc_all_custom = include_all_custom;
    ncustom = 0;
}


/************************************************************************
 * 
 ************************************************************************/
void code_context::custom_ctxt_all()
{
    inc_all_custom = TRUE;
    ncustom = 0;
}


/************************************************************************
 * 
 ************************************************************************/
void code_context::custom_ctxt_none()
{
    inc_all_custom = FALSE;
    ncustom = 0;
}


/************************************************************************
 * 
 ************************************************************************/
void code_context::custom_ctxt_use(const char * nm)
{
    nm = lexicon->enter(nm)->sp;
    assert(ncustom < MAX_CUSTOM-1);
    for(int i=0; i<ncustom; i++)
	if(nm == custom[i]) return;

    custom[ncustom++] = nm;
}


/************************************************************************
 * 
 ************************************************************************/
void code_context::custom_ctxt_dont_use(const char * nm)
{
    int num = -1;
    nm = lexicon->enter(nm)->sp;
    assert(ncustom < MAX_CUSTOM-1);
    int i;
    for(i=0; ((i<ncustom)&&(num==-1)); i++)
	if(nm == custom[i])
	    num = i;

    if(num == -1) return;
    
    for(i=num; i<ncustom-1; i++)
	custom[i] = custom[i+1];
    ncustom--;
}



/************************************************************************
 * 
 * 
 ************************************************************************/
boolean code_context::is_custom(const char * nm)
{
    if(inc_all_custom) return TRUE;

    for(int i=0; i<ncustom; i++)
	if(nm == custom[i])
	    return TRUE;

    return FALSE;
}

/************************************************************************
 * 
 * 
 ************************************************************************/
named_symcoeff_ineq code_context::get_context(tree_node * tn)
{
    named_symcoeff_ineq curr;

    annote_list_iter iter(tn->annotes());
    while(!iter.is_empty()) {
	annote * an = iter.step();
	if(strcmp(code_context_annote::k_annote, an->name()) == 0) {
	    code_context_annote * cca = (code_context_annote *)an->data();
	    if(is_custom(cca->name()))
		curr &= cca->context();
	}
    }

    if(inc_for && tn->is_for()) {
	tree_for * tnf = (tree_for *)tn;
	named_symcoeff_ineq * sc = include_sc_for(tnf);
	if(sc) {
	    curr &= *sc;
	    delete sc;
	}
    } else if(inc_if && tn->is_if()) {
	assert_msg(0, ("Not implemented"));
    }

    return curr;
}


/************************************************************************
 * 
 * 
 ************************************************************************/
named_symcoeff_ineq code_context::get_context(tree_node * tn, named_symcoeff_ineq & in)
{
    named_symcoeff_ineq sc = get_context(tn);
    return (sc & in);
}

named_symcoeff_ineq code_context::get_context(tree_node * tn, named_symcoeff_ineq * in)
{
    named_symcoeff_ineq sc = get_context(tn);
    if(in)
	return (sc & *in);
    else
    	return sc;
}

    

