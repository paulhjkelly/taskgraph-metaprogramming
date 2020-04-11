/* file "normalize_code.cc" */

/*  Copyright (c) 1994 Stanford University

    All rights reserved.

    This software is provided under the terms described in
    the "suif_copyright.h" include file. */

#include <suif_copyright.h>

#include <suif1.h>
#include <suifmath.h>
#include "dependence.h"
#include <useful.h>
#include <builder.h>

//
// normalize_code.cc
//
// The routine normalize_code normalizes the given tree_for by modifying
// all array references and loop definitions so that the step 
// is always 1 and the test is always <= 
//
//  for i = a to b by c  (where a and b may be funcs of other induction vars)
//      [xi + ...]
// is transformed to:
// for i = 0 to (b-a)div c by 1
//      [xci + xa + ...]
//

//
// sub_uses_{tree,src} recursively replace index of the tree_for tf
// with the access vector av -- Note: it doesn't change the dep annotations.
//
//
struct sub_uses_params {
    tree_for *tf;
    access_vector *av;
};

static boolean sub_uses_src(instruction *, operand *r, void *x);
static void sub_uses_tree(tree_node *tn, void *x);

boolean sub_uses_src(instruction *, operand *r, void *x)
{
    if (r->is_instr()) {
        r->instr()->src_map(sub_uses_src, x);
    }
    else if (r->is_symbol()) {
	sym_node *sn = r->symbol();
        sub_uses_params *params = (sub_uses_params *) x;
	if (sn == params->tf->index()) {
	    r->remove();
	    *r = params->av->generate_code(params->tf->proc()->block());
	    return TRUE;
	}
    }
    return FALSE;
}


void sub_uses_tree(tree_node *tn, void *x)
{
    if (tn->kind() == TREE_INSTR) {
        instruction *ins = ((tree_instr *) tn)->instr();
        sub_uses_params *params = (sub_uses_params *) x;

        operand dst = ins->dst_op();
        assert(!dst.is_symbol() || dst.symbol() != params->tf->index());

	ins->src_map(sub_uses_src, x);
    }
}


// Normalizes the tree_for.  Returns whether normalization was successful.
// Changes code so that step is always 1 and the test is less-than-or-equal
//
boolean normalize_code(tree_for *tf)
{
    tree_proc *p = tf->proc()->block();
    block bldr;
    bldr.set_proc(p);
    
    // Check if we can do the normalization:
    // 
    int true_step;
    if (!tf->step_is_constant(&true_step) || true_step == 0) return FALSE;

    type_node * stype = tf->index()->type();    
    if(stype->op() != TYPE_INT)
	return FALSE;

    if (true_step > 0) {
        if (!((base_type *)stype)->is_signed()) {
            if (tf->test() != FOR_ULTE && tf->test() != FOR_ULT) return FALSE;
        } else {
            if (tf->test() != FOR_SLTE && tf->test() != FOR_SLT) return FALSE;
        }
    } else {
        if (!((base_type *)stype)->is_signed()) {
            if (tf->test() != FOR_UGTE && tf->test() != FOR_UGT) return FALSE;
        } else { 
            if (tf->test() != FOR_SGTE && tf->test() != FOR_SGT) return FALSE;
        }
    }

    // Now do the normalization:
    // Make bound less-than-or-equal or greater-than-or-equal

    if (tf->test() == FOR_ULT || tf->test() == FOR_SLT || 
	tf->test() == FOR_UGT || tf->test() == FOR_SGT) {

        int diff = (true_step < 0) ? 1 : -1;

        block up(tf->ub_op());
        block up_new(up + block(diff));
	instruction *inst = up_new.make_instruction();
	tf->ub_op().remove();
	tf->set_ub_op(operand(inst));

        tree_for_test test = (tf->test() == FOR_ULT) ? FOR_ULTE :
	                     (tf->test() == FOR_SLT) ? FOR_SLTE :
			     (tf->test() == FOR_UGT) ? FOR_UGTE : FOR_SGTE;
	tf->set_test(test);
    }

    dep_for_annote *dfa = (dep_for_annote *) tf->peek_annote(k_dep_for_annote);
    assert(dfa);


    // We don't need access vectors to normalize a loop with -1 step:
    //
    if (true_step == -1) {

        // Negate lower and upper bounds
        block up(-block(tf->ub_op()));
        block low(-block(tf->lb_op()));

        tf->ub_op().remove();
        tf->lb_op().remove();
        tf->set_ub_op(operand(up.make_instruction()));
        tf->set_lb_op(operand(low.make_instruction()));

        access_vector av;
        av.add(tf, -1);

        sub_uses_params *params = new sub_uses_params();
        params->tf = tf;
        params->av = &av;

        tf->body()->map(sub_uses_tree, params);

        if (tf->test() == FOR_UGTE) tf->set_test(FOR_ULTE);
        else if (tf->test() == FOR_SGTE) tf->set_test(FOR_SLTE);
        else assert(FALSE);

        // Set step to unity
        tf->step_op().remove();
        tf->set_step_op(const_op(immed(1), tf->index()->type()->unqual()));

        dfa->fill_in_access(tf, TRUE, TRUE);
        true_step = 1;
    }

    else if (true_step != 1) {
        // Set step to unity
        tf->step_op().remove();
        tf->set_step_op(const_op(immed(1), tf->index()->type()->unqual()));

        // Fill in access vectors again with normalization for procedure
        dfa->fill_in_access(tf, TRUE, TRUE);  

        access_vector *av_lb = NULL, *av_ub = NULL;   // don't delete these!
        if (dfa->lb->count() == 1 && dfa->ub->count() == 1) {
            av_lb = dfa->lb->first();
            av_ub = dfa->ub->first();
            if (av_lb == NULL || av_lb->too_messy || 
		av_ub == NULL || av_ub->too_messy) {

                if (av_lb) av_lb = NULL;
                if (av_ub) av_ub = NULL;
            }
        }

        // Give up -- but first restore step
        if (av_lb == NULL || av_ub == NULL) {
            assert(av_lb == NULL && av_ub == NULL);
	    tf->step_op().remove();
            tf->set_step_op(const_op(immed(true_step),
                                     tf->index()->type()->unqual()));
            return FALSE; 
        }

        // Copy av_lb and av_ub 
        access_vector lbav = *av_lb;
        access_vector ubav = *av_ub;

        access_vector zero; 
        zero.con = 0;
        tf->lb_op().remove();
        tf->set_lb_op(zero.generate_code(p));

        int ub_con = ubav.con - lbav.con;
        av_compare_info ci(&lbav, &ubav);

        if (ci.identical_excluding_const()) {
            // compute floor of ub_con / true_step

            int step = true_step;
            if (step < 0) { step = -step; ub_con = -ub_con; }

            access_vector av;
            av.con = ub_con >= 0 ? ub_con/step : -1;
	    tf->ub_op().remove();
            tf->set_ub_op(av.generate_code(p));
        } 
	else {
            access_vector av = ubav - lbav;
            int step = true_step;
            av.con = ub_con;
            if (step < 0) { step = -step; av = -av; }

            block up = bldr.op(block(av.generate_code(p)), bop_divfloor, 
			       block(step));
	    tf->ub_op().remove();
            tf->set_ub_op(operand(up.make_instruction()));

            dfa->fill_in_access(tf, 1, TRUE);  // modifies av_lb, av_ub
        }

        // replace each tf->index by true_step*(tf->index) + lb;
        lbav.add(tf, true_step);

        sub_uses_params *params = new sub_uses_params();
        params->tf = tf;
        params->av = &lbav;

        tf->body()->map(sub_uses_tree, params);

        // Set test to less-than-or-equal
        if (tf->test() == FOR_UGTE) tf->set_test(FOR_ULTE);
        else if (tf->test() == FOR_SGTE) tf->set_test(FOR_SLTE);

        dfa->fill_in_access(tf, TRUE, TRUE);
    }

    return TRUE;
}

