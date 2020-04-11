/* file "dodep.cc" */

/*  Copyright (c) 1994 Stanford University

    All rights reserved.

    This software is provided under the terms described in
    the "suif_copyright.h" include file. */

#include <suif_copyright.h>

#define _MODULE_ "libdependence.a"
#pragma implementation "dodep.h"

#include <cstdio>
#include <suif1.h>
#include <useful.h>
#include <builder.h>
#include <suifmath.h>
#include "dependence.h"

dvlist * find_extra(in_array * ia1, in_array * ia2, int minnest, tn_list * tnl, boolean lexpos);


static void lex_half(dvlist & ret, direction d, int expand, int minnest)
{
    distance eq;
    eq.set_direction(d_eq);
    distance star;
    star.set_direction(d_star);
    distance din;
    din.set_direction(d);

    for(int i=0; i<expand; i++) {
        distance_vector * ndv = new distance_vector;
	int j;
        for(j=0; j<i; j++)
            ndv->append(eq);
        ndv->append(din);
        for(j++; j<minnest; j++)
            ndv->append(star);
        ret.push(new dvlist_e(ndv));
    }
}


dvlist * DependenceTest(in_array *w, in_array *r, 
                        int lexpos,
                        deptest_result * res,
                        int arraysaresame)
{
    dependence_test D;
    D.do_test(w, r, FALSE, lexpos, arraysaresame);
    if(res) *res = D.result();
    return D.get_dv();
}

dvlist * FlowDependenceTest(in_array *w, in_array *r, 
                            int lexpos,
                            deptest_result * res)
{
    dependence_test D;
    D.do_test(w, r, TRUE, lexpos);
    if(res) *res = D.result();
    return D.get_dv();
}


static void rm_mod_this(array_info * ai)
{
    assert(ai);
    array_info_iter iter(ai);
    while(!iter.is_empty()) {
        access_vector * av = iter.step();
        av->mod_this = NULL;
    }
}


static in_ldc * find_ldc(instruction * in)
{
    assert(in);
    if(in->opcode() == io_ldc) return (in_ldc *)in;
    else if(in->opcode() == io_cvt) {
        in_rrr * rrr = (in_rrr *)in;
        assert(rrr->src1_op().is_instr());
        return find_ldc(rrr->src1_op().instr());
    }
    assert(0);
    return NULL;
}

void dependence_test::mk_star_vec(int minnest, int lexpos)
{
    dv = new dvlist;
    lex_half(*dv, d_lt, minnest, minnest);
    if(!lexpos)
        lex_half(*dv, d_gt, minnest, minnest);

    distance eq;
    eq.set_direction(d_eq);
    distance_vector * ndv = new distance_vector;
    for(int j=0; j<minnest; j++)
        ndv->append(eq);
    dv->push(new dvlist_e(ndv));
}

static dvlist * elem_and_add(dvlist * lel, int n, dvlist * gl)
{
    dvlist * ret = new dvlist;

    dvlist_iter iter1(lel);
    while(!iter1.is_empty()) {
        distance_vector * dv = iter1.step()->dv;
        if(dv->level() <= n)
            ret->push(new dvlist_e(dv));
    }

    dvlist_iter iter2(gl);
    while(!iter2.is_empty()) {
        distance_vector * dv = iter2.step()->dv;
        if(dv->level() > n)
            ret->push(new dvlist_e(dv));
    }

    return ret;
}


// compare a read to a write 
void dependence_test::do_test(in_array *w, in_array *r, int flow, int lexpos, int arraysaresame)
{
    if(!arraysaresame) {
        // check if the same array
        sym_node *sr = operand_address_root_symbol(r->base_op());
        sym_node *sw = operand_address_root_symbol(w->base_op());

	if((sr == NULL)||(sw == NULL)) {
	    dv = new dvlist;
	    res = dt_too_messy; 
	    return;
	}
	

        var_sym *vr = (sr->is_var() ? (var_sym *)sr : NULL);
        var_sym *vw = (sw->is_var() ? (var_sym *)sw : NULL);

	if ((vr != NULL) && (vw != NULL) && (vr->overlaps(vw) == FALSE)) {
            // not the same array
	    dv = new dvlist;
	    dv->set_indep();
	    res = dt_indep; 
	    return; 
	}

	// BUGBUG: Currently not set to handle arrays that overlap but
	// not the same starting point.
	if(!operands_are_same_expr(r->base_op(), w->base_op())) {
	    dv = new dvlist;
	    res = dt_too_messy; 
	    return;
	}
    }

    // BUGBUG fancy is always set to TRUE
    array_info *ai1 = new array_info(w, TRUE);
    array_info *ai2 = new array_info(r, TRUE);
    
    int minnest = find_minnest(w->parent(), r->parent());
    int wnest = num_loops(w->parent());
    int rnest = num_loops(r->parent());
    
    int wmodnest = num_mod_nest(w->parent(), *ai1);
    int rmodnest = num_mod_nest(r->parent(), *ai2);

    if (!minnest) { 
        res = dt_no_common_nest; 
        mk_star_vec(minnest, lexpos);
        delete ai1;
        delete ai2;
        return; 
    }
    
    if((wnest-wmodnest > 0)||(rnest-rmodnest > 0)) {
        int mmod = MAX(wnest-wmodnest, rnest-rmodnest);
        if(mmod < minnest) {
            tn_list * tnl = find_astlist(w->parent(),
                                         r->parent(), 
                                         minnest);
            dvlist * dv1 = new dvlist(ai1,ai2, tnl, lexpos, flow);
            
            rm_mod_this(ai1);
            rm_mod_this(ai2);
            dvlist * dv2 = new dvlist(ai1,ai2, tnl, lexpos, flow);

            dv = elem_and_add(dv1, mmod, dv2);
            
/*            printf("mmod = %d minnest=%d\n", mmod, minnest);
            printf("normal      dv1=\n"); dv1->print(stdout);
            printf("rm modthis  dv2=\n"); dv2->print(stdout);
            printf("elem/insert dv=\n"); dv->print(stdout);
            */

            res = (dv->indep())?dt_indep:dt_ok;

            delete dv1;
            delete dv2;
            delete ai1;
            delete ai2;
            delete tnl;
            return;
        }
    }

    tn_list * tnl = find_astlist(w->parent(),r->parent(), minnest);
    dv = new dvlist(ai1,ai2, tnl, lexpos, flow);
    dvlist * more_dv = find_extra(w, r, minnest, tnl, lexpos);


/*    printf("*******DV lists***************\n");
    dv->print(stdout);
    if(more_dv) {
        printf("******\n");
        more_dv->print(stdout);
    }
    printf("**********************\n"); */

        
    if(more_dv) {
        delete dv;
        dv = more_dv;
    }
    res = (dv->indep())?dt_indep:dt_ok;
    
    delete ai1;
    delete ai2;
    delete tnl;
}


dvlist * dependence_test::make_vec(dvlist * dvl, 
                                   int n, 
                                   int minnest, 
                                   int lexpos)
{
    dvlist * ret = new dvlist;

    lex_half(*ret, d_lt, n, minnest);
    if(!lexpos)
        lex_half(*ret, d_gt, n, minnest);
    
//    printf("sides dv\n"); ret->print(stdout);
    

    distance eq;
    eq.set_direction(d_eq);
    
    assert(dvl);
    dvlist_iter iter(dvl);
    while(!iter.is_empty()) {
        distance_vector * dv = iter.step()->dv;
//        printf("got: "); dv->print(stdout);

        distance_vector * ndv = new distance_vector(dv);
        for(int i=0; i<n; i++) {
            ndv->push(new distance_vector_e(eq));
        }
        ret->push(new dvlist_e(ndv));
    }

    return ret;
}



// how many enclosing for loops does a reference have
int dependence_test::num_loops(tree_node *i, tree_node *top)
{
    int sum=0;
    for (tree_node *an = (tree_node *) i; an; an=an->parent()->parent()) {
        if (an->kind() == TREE_FOR) {
            if(an == top) return sum;
            sum++;
        }
        if(an->parent()==NULL) break;
    }
    return sum;
}

int dependence_test::num_mod_nest(tree_node *i, array_info & ai)
{
    int m = num_loops(i);
    array_info_iter iter(&ai);
    while(!iter.is_empty()) {
        access_vector * av = iter.step();
        if(av->mod_this) {
            int tm = num_loops(i, av->mod_this);
            m = MIN(tm, m);
        }
    }
    return m;
}

// how many enclosing common loops do two references have
int dependence_test::find_minnest(tree_node *i1, tree_node *i2)
{
    int num1 = num_loops(i1);
    int num2 = num_loops(i2);
    
    // throw out bottom loops of larger one
    tree_node *an1 = i1;
    tree_node *an2 = i2;
    if (num1 > num2) {
        assert(an1->kind() == TREE_INSTR);
        while (num1 > num2) {
            if (an1->kind() == TREE_FOR) num1--;
            assert(an1->parent());
            an1 = an1->parent()->parent();
        }
    } else if (num2 > num1) {
        assert(an2->kind() == TREE_INSTR);
        while (num2 > num1) {
            if (an2->kind() == TREE_FOR) num2--;
            assert(an2->parent());
            an2 = an2->parent()->parent();
        }
    }
    assert (num1 == num2);
    assert(num1 >= 0);
    
    // set each one to deepest for they have in common
    while (num1 && an1 && an2 && (an1 != an2)) {
        while (an1->kind() != TREE_FOR) { 
            if(an1->parent()==NULL) 
                an1 = NULL;
            else 
                an1=an1->parent()->parent();
        }
        while (an2->kind() != TREE_FOR) {
            if(an2->parent()==NULL) 
                an2 = NULL;
            else
                an2=an2->parent()->parent();
        }
        if (an1 != an2) {
            num1--;
            num2--;

            if(an1->parent()==NULL) 
                an1 = NULL;
            else 
                an1=an1->parent()->parent();

            if(an2->parent()==NULL) 
                an2 = NULL;
            else
                an2=an2->parent()->parent();
        }
    }
    return num1;
}


// set astlist so that first element is innermost loop that encloses both	
tn_list *dependence_test::find_astlist(tree_node *i1)
{
    tn_list *al = new tn_list();
    tree_node *an1 = i1;
    int num1 = num_loops(i1);
    
    for (int i = 0; i<num1; i++, an1=an1->parent()->parent()) {
        while (an1->kind() != TREE_FOR) {
            assert(an1->parent());
            an1=an1->parent()->parent();
        }
        al->append(an1);
    }
    
    return al;
}


tn_list *make_al(tree_node *s1, tree_node *s2);
// set astlist so that first element is innermost loop that encloses both	
// also sets the number of nests both loops have in common
tn_list *dependence_test::find_astlist(tree_node *i1, tree_node *i2, int /* minnest */)
{
    // need to do the goofy part
    tn_list * tnl = make_al(i1, i2);
    return tnl;

/*    tn_list *al = new tn_list();
    tree_node *an1 =  i1;
    tree_node *an2 =  i2;
    int num1 = num_loops(i1);
    int num2 = num_loops(i2);
    
    while (num1 > minnest) {
        if (an1->kind() == TREE_FOR) num1--;
        an1 = an1->parent()->parent();
    }
    while (num2 > minnest) {
        if (an2->kind() == TREE_FOR) num2--;
        an2 = an2->parent()->parent();
    }
    for (int i = minnest; i>0; i--) {
        while (an1->kind() != TREE_FOR) an1=an1->parent()->parent();
        while (an2->kind() != TREE_FOR) an2=an2->parent()->parent();
        al->append(an1);

        if(an1->parent()==NULL) 
            an1 = NULL;
        else 
            an1=an1->parent()->parent();

        if(an2->parent()==NULL) 
            an2 = NULL;
        else 
            an2=an2->parent()->parent();
    }
    
    return al; */
}


void print_array_access(in_array * in, FILE * fs)
{
    var_sym * vs = get_sym_of_array(in);
    fprintf(fs, "%s", ((vs != NULL) ? vs->name() : "??"));
    array_info ai(in, TRUE);
    ai.print(fs);
    fprintf(fs, "\n");
}
