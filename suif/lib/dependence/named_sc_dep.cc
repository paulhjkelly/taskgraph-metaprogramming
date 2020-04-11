/* file "named_sc_dep.cc" */

/*  Copyright (c) 1994 Stanford University

    All rights reserved.

    This software is provided under the terms described in
    the "suif_copyright.h" include file. */

#include <suif_copyright.h>

#define _MODULE_ "libdependence.a"

#include <cstdio>
#include <cstdarg>
#include <csetjmp>
#include <suif1.h>
#include <suifmath.h>
#include "dependence.h"
#include <builder.h>

const char * k_depset_is_run;
const char * k_depset_symconst_ok;

static boolean sign_ok(named_symcoeff_ineq & nsi)
{
    for(int im = 0; im < nsi.m(); im++)
        for(int in = 0; in < nsi.n(); in++) {
            int sign = 0;
            for(int ip=0; ip<nsi.p(); ip++) {
                int c = nsi[ip][im][in];
                if(c*sign < 0) return FALSE;
                if(sign == 0) sign = c;
            }
        }
    return TRUE;
}



static named_lin_ineq * get_context(tree_node * tn)
{       
    named_lin_ineq * ret = NULL;        
    while(tn) {
        if(tn->is_for()) {
            ret = include_for((tree_for *)tn, ret);
        }
        tn = (tn->parent())?tn->parent()->parent():NULL;
    }
    return ret;
}


static void new_col(named_symcoeff_ineq & nsi, immed from, immed to)
{
    int pos = nsi.cols().find(from);
    if(pos == -1) {
        nsi.add_col(to, 1);
    } else 
        nsi.cols()[pos].init(to);
        
    name_table_entry nte(from);
    nsi.add_col(nte, 1);
}

extern jmp_buf * _named_sc_fm_longjmp_env;

static distance_vector * mk_matching(int i, 
                                     named_symcoeff_ineq * context,
                                     named_symcoeff_ineq & nsi1, 
                                     named_symcoeff_ineq & nsi2,
                                     int minnest,
                                     tn_list * tnl,
                                     boolean less)
{
    distance_vector * dv = new distance_vector;

    named_symcoeff_ineq tnsi1(nsi1);
    named_symcoeff_ineq tnsi2(nsi2);


    named_symcoeff_ineq context1;
    if(context) context1.init(context);

    distance d;
    char nm[128];
    var_sym * vs = NULL;
    int j;
    for(j=0; j<=i; j++) {
        tree_for * tf = (tree_for *)(*tnl)[j];  
        vs = tf->index();
        sprintf(nm, "%s_x", vs->name());

        new_col(tnsi1, immed(vs), immed(nm));
        if(context)
            new_col(context1, immed(vs), immed(nm));

        if(j<i)  
            d.set_direction(d_star);
        else if(less)
            d.set_direction(d_lt);
        else
            d.set_direction(d_gt);
        dv->push(new distance_vector_e(d));
    }

    d.set_direction(d_eq);
    for(; j<minnest; j++) 
        dv->push(new distance_vector_e(d));
    
    tnsi1 || tnsi2;
    
    named_symcoeff_ineq chknsi;
    chknsi.init(tnsi1.p(), tnsi1.m()*2, tnsi1.n());
    chknsi.planes().init(tnsi1.planes());
    chknsi.cols().init(tnsi1.cols());
    for(int xp = 0; xp<tnsi1.p(); xp++) 
        for(int xm = 0; xm<tnsi1.m(); xm++) {
            chknsi[xp][2*xm] = tnsi1[xp][xm] - tnsi2[xp][xm];
            chknsi[xp][2*xm+1] = tnsi2[xp][xm] - tnsi1[xp][xm];
        }

    if(i >= 0) {
        named_symcoeff_ineq half;
        half.init(1, 1, 3);
        name_table nt(new immed(vs), new immed(nm));
        half.cols().init(nt);
        half[0][0][0] = -1;
        half[0][0][1] =  (less)?1:-1;
        half[0][0][2] =  (less)?-1:1;
        chknsi &= half;
    }

    if(context) {
        chknsi &= *context;
        chknsi &= context1;
    }

    boolean not_valid;
    jmp_buf Env;
    _named_sc_fm_longjmp_env = &Env;
    if(setjmp(Env)) {
        not_valid = FALSE;
    }else {
        if(sign_ok(chknsi))
            not_valid = ~chknsi;
        else
            not_valid = FALSE;
    }
    _named_sc_fm_longjmp_env = NULL;
    
/*
    printf("chknsi::\n");
    chknsi.print_exp();
    dv->print(stdout);
    printf("%sAnswer is %s\n", 
           sign_ok(chknsi)?"":"Sign is not ok, ", 
           not_valid?"not valid":"valid");
           */

    if(not_valid) {
	delete dv;
        dv = NULL;
    }

    return dv;
}


static boolean is_immed_in_list(immed & im, immed_list & iml)
{
    immed_list_iter iter(&iml);
    while(!iter.is_empty()) {
        immed tim(iter.step());
        if(tim == im) return TRUE;
    }
    return FALSE;
}

static boolean is_names_in_immed_list(name_table & nt, immed_list & iml)
{
    for(int i=1; i<nt.n(); i++) {
        immed im(nt[i].name());
        if(is_immed_in_list(im, iml) == FALSE)
            return FALSE;
    }
    return TRUE;
}

tree_for *find_inner(tree_node *n, var_sym * v);
tree_for * is_index(var_sym * v, tree_node *tn);

static boolean is_var_modified_but_not_by_index(in_array * ia, immed im)
{
    if(!im.is_symbol()) return FALSE;
    if(!im.symbol()->is_var()) return FALSE;
    if(is_index((var_sym *)im.symbol(), ia->parent())) return FALSE;
    tree_for * tf = find_inner(ia->parent(), (var_sym *)im.symbol());
    if(tf == NULL) return FALSE;
    return TRUE;
}

static boolean is_modified_but_not_by_index(in_array * ia, name_table & nt)
{
    for(int i=1; i<nt.n(); i++) {
        immed im(nt[i].name());
        if(is_var_modified_but_not_by_index(ia, im)) return TRUE;
    }
    return FALSE;
}

static boolean is_var_ok(in_array * ia, named_symcoeff_ineq * ineq)
{
    immed_list * iml = (immed_list *) ia->peek_annote(k_depset_symconst_ok);
    if(iml == NULL) return FALSE;
    if(is_names_in_immed_list(ineq->planes(), *iml) == FALSE)
        return FALSE;

    if(is_modified_but_not_by_index(ia, ineq->planes())) return FALSE;
    if(is_modified_but_not_by_index(ia, ineq->cols())) return FALSE;
    return TRUE;
}


dvlist * do_extra_dep(in_array * ia1, in_array * ia2, int minnest, tn_list * tnl, boolean lexpos)
{
    assert(ia1);
    assert(ia2);
    assert(ia1->dims() == ia2->dims());

    named_symcoeff_ineq nsi1;
    named_symcoeff_ineq nsi2;
    int i;
    for(i=0; ((unsigned)i) < ia1->dims(); i++) {
        named_symcoeff_ineq * t1 = named_symcoeff_ineq::convert_exp(ia1->index(i)); 
        named_symcoeff_ineq * t2 = named_symcoeff_ineq::convert_exp(ia2->index(i)); 
        if(t1)
            if(is_var_ok(ia1, t1) == FALSE) {
                if(t2) delete t2;
                return NULL;
            }
        if(t2)
            if(is_var_ok(ia2, t2) == FALSE) {
                if(t1) delete t1;
                return NULL;
            }
        if(t1 && t2) {
            nsi1 || *t1;
            nsi2 || *t2;
            nsi1.add_ineq(0);
            nsi2.add_ineq(0);
            nsi1.set_m(0, t1->get_m(0));
            nsi2.set_m(0, t2->get_m(0));
        }

        if(t1) delete t1;
        if(t2) delete t2;
                 
    }

//    printf("access 1\n");    nsi1.print_exp();
//    printf("access 2\n");    nsi2.print_exp();
//    printf("-----\n");

    named_lin_ineq * tcon = get_context((*tnl)[0]);
    named_symcoeff_ineq * con = NULL;
    if(tcon) {
        con = new named_symcoeff_ineq(tcon);
        *con || nsi1;
        *con || nsi2;
    }


    dvlist * retlist = new dvlist;

    distance_vector * dv;
    dv =  mk_matching(-1, con, nsi1, nsi2, minnest, tnl, TRUE);
    if(dv) retlist->append(new dvlist_e(dv));

    for(i=0; i<minnest; i++) {        
//        tree_for * tf = (tree_for *)(*tnl)[i];
//        printf("%d: %s\n", i, tf->index()->name());
        dv =  mk_matching(i, con, nsi1, nsi2, minnest, tnl, TRUE);
        if(dv) retlist->append(new dvlist_e(dv));
        dv =  mk_matching(i, con, nsi1, nsi2, minnest, tnl, FALSE);
        if(dv) retlist->append(new dvlist_e(dv));
    }

    if(lexpos) retlist->make_lexpos();
    return retlist;
}




dvlist * find_extra(in_array * ia1, in_array * ia2, int minnest, tn_list * tnl, boolean lexpos)
{
    tree_proc * tp = ia1->parent()->proc()->block();
    if(tp->peek_annote(k_depset_is_run) == NULL)
        return NULL;

    array_info ai1(ia1, TRUE);
    array_info ai2(ia2, TRUE);

    boolean too_messy = FALSE;
    array_info_iter aii1(&ai1);
    array_info_iter aii2(&ai2);
    while(!aii1.is_empty()) {
        assert(!aii2.is_empty());
        access_vector *av1 = aii1.step();
        access_vector *av2 = aii2.step();
        if(av1->too_messy || av2->too_messy)
            too_messy = TRUE;
    }
        
    if(too_messy) {
//        print_array_access(ia1);
//        print_array_access(ia2);
        dvlist * dv = do_extra_dep(ia1, ia2, minnest, tnl, lexpos);

        return dv;
    }
    return NULL;
}
