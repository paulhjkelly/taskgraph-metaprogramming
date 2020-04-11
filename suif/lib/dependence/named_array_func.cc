/* file "named_lin_ineq.cc" */

/*  Copyright (c) 1994 Stanford University

    All rights reserved.

    This software is provided under the terms described in
    the "suif_copyright.h" include file. */

#include <suif_copyright.h>

#define _MODULE_ "libdependence.a"


#include <cstdio>
#include <suif1.h>
#include <builder.h>
#include <suifmath.h>
#include "dependence.h"
#include <cstring>




dvlist * find_extra(in_array * ia1, 
                    in_array * ia2, 
                    int minnest, 
                    tn_list * tnl, 
                    boolean lexpos);


void convert_const(named_lin_ineq * c)
{
    const char * st;

    if(c == NULL) return;
    for(int i=1; i<c->n(); i++) {
        immed v(c->names()[i].name());

        if (v.is_symbol())
            st = v.symbol()->name();
        else if (v.is_string())
            st = v.string();
        else      // an aux
            continue;

        if(strncmp(st, "c__", 3) == 0) {
            int val;
            sscanf(&st[3], "%d", &val);
            for(int j=0; j<c->ineqs().m(); j++) {
                c->ineqs()[j][0] += c->ineqs()[j][i]*val;
                c->ineqs()[j][i] = 0;
            }
        }
    }
    c->cleanup();
}


/***************************************************************************
 *                                                                         *
 ***************************************************************************/
named_lin_ineq * named_lin_ineq::mk_named_lin_ineq(array_info & ai,     
                                                   immed ind, 
                                                   boolean lb)
{
    named_lin_ineq * ret = NULL;
    array_info_iter iter(&ai);
    while(!iter.is_empty()) {
        access_vector * av = iter.step();
        if(av->too_messy) 
            return NULL;

        named_lin_ineq * curr = mk_named_lin_ineq(*av, ind, lb);
	if (curr) {
	    if (ret) {
		*ret &= *curr;
		delete curr;
	    } else
		ret = curr;
	}
    }
    convert_const(ret);
    if (ret && (ret->m() == 0)) {
	delete ret;
	ret = 0;
    }
    return ret;
}
 


/***************************************************************************
 *                                                                         *
 ***************************************************************************/
named_lin_ineq * named_lin_ineq::mk_named_lin_ineq(access_vector & av, 
                                                   immed ind, 
                                                   boolean lb)
{
    if(av.too_messy) return NULL;

    named_lin_ineq *ret = new named_lin_ineq;

    int ncol = 1 + av.conregs.count() + av.elts.count() + 1;

    ret->lq.init(1,ncol);
    ret->nt.init(ncol);
    ret->lq[0][0] = av.con;

    int cnt = 1;
    access_list_iter iter1(&av.conregs);
    while(!iter1.is_empty()) {
        access_list_e *e = iter1.step();
        ret->lq[0][cnt] = e->val();
        var_sym * v = (var_sym *) e->var();
        ret->nt[cnt++].init(immed(v));
    }
    
    access_list_iter iter2(&av.elts);
    while(!iter2.is_empty()) {
        access_list_e *e = iter2.step();
        ret->lq[0][cnt] = e->val();
        tree_node *n = (tree_node *) e->var();
        var_sym * v = ((tree_for *)n)->index();
        ret->nt[cnt].init(immed(v));
        ret->nt[cnt++].mark_sym();
    }

    ret->lq[0][cnt] = -1;
    ret->nt[cnt].init(ind);
    ret->nt[cnt].mark_sym();
    if(lb) ret->lq[0] *= -1;

    convert_const(ret);
    if (ret && (ret->m()==0)) {
	delete ret;
	ret = 0;
    }
    return ret;
}






named_lin_ineq * include_for(tree_for * tf, named_lin_ineq * c)
{
    assert(tf);

    boolean forward = TRUE;
    boolean ub_neq = FALSE;
    boolean lb_neq = FALSE;
    switch(tf->test()) {
    case FOR_SLT:
    case FOR_ULT:
	ub_neq = TRUE;
	break;

    case FOR_SLTE:
    case FOR_ULTE:
	break;

    case FOR_SGT:
    case FOR_UGT:
	forward = FALSE;
	lb_neq = TRUE;
	break;
	
    case FOR_SGTE:
    case FOR_UGTE:
	forward = FALSE;
	break;
	
    default:
	assert_msg(0, ("Test should not happen outside frontend\n"));
    }
    
    dep_for_annote *dfa = 
        (dep_for_annote *) tf->peek_annote(k_dep_for_annote);
    assert(dfa);

    immed idx(tf->index());
    named_lin_ineq * cl = named_lin_ineq::mk_named_lin_ineq((forward)?*dfa->lb:*dfa->ub, idx, TRUE); 
    if(cl == NULL) {
        printf("Unknown lower bound\n");
        if(c) return new named_lin_ineq(c);
        return NULL;
    }
    named_lin_ineq * cu = named_lin_ineq::mk_named_lin_ineq((forward)?*dfa->ub:*dfa->lb, idx, FALSE);
    if(cu == NULL) {
        printf("Unknown upper bound\n");
        delete cl;
        if(c) return new named_lin_ineq(c);
        return NULL;
    }

    if(ub_neq) {
	for(int i=0; i < cu->m(); i++)
	    cu->ineqs()[i][0] -= 1;
    }

    if(lb_neq) {
	for(int i=0; i < cl->m(); i++)
	    cl->ineqs()[i][0] += 1;
    }
    
    
    *cu &= *cl;
    
    named_lin_ineq * stp = named_lin_ineq::mk_named_lin_ineq(*dfa->stp, idx, FALSE);
    boolean stpbad = TRUE;
    if(stp)
        if((stp->n() == 2)&&                            // no sym variables
           (stp->ineqs().m() == 1)) {                   // no max or mins
            stpbad = FALSE;
            if(stp->ineqs()[0][0] != 1) {               // non-unit step
                int stride = stp->ineqs()[0][0];
                named_lin_ineq str(cl);        
                name_table_entry aux;
                aux.mark_aux();
                str.add_col(aux, str.n());
                str.ineqs()[0][str.n()-1] = stride;
                str &= str.ineqs()*-1;
		*cu &= str;
            }
        }
    if(stpbad) {
        printf("Unknown or symbolic step size\n");
        delete cl;
        delete cu;
        if(stp) delete stp;
        if(c) return new named_lin_ineq(c);
        return NULL;
    }
    
    if (c)
	*cu &= *c;
    cu->cleanup();
    
    delete cl;
    delete stp;

    return cu;
}



