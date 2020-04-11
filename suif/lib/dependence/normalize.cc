/* file "normalize.cc" */

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

//
// normalize.c
//
//  Normalize the access vectors for all array references and loop
//    definitoins so that the loop increment is always 1
//
//  for i = a to b by c  (where a and b may be funcs of other induc vars)
//      [xi + ...]
// has it's access vectors transformed to
// for i = 0 to (b-a)div c by 1
//      [xci + xa + ...]
// (unless c is 1 in which case no normalization is done)
// Two sets of routines:
//	ref routines change i->xci+xa for all references (including more deeply
//						  nested loop definintions)
//      loop definition routines change lb<-0, ub<- (b-a)divc, step<-1
// to work correctly, all references must be changed before enclosing defs
//    and the references in a loop definition must be changed before
//    statements inside the loop,
//    ie for i = .  . .
//          for j = a*i b c
//              [i,j]
//
// Also routine to normalize access vectors and test so test is <=
//
//    must change for j = a' b' c' ref first (to take into account changes in i)
//    then change ref [i,j] and finally change loop definitions (a'<-0, etc)
//


// nomalize the reference in an array_info
void array_info::normalize_step_ref()
{
    array_info *temp_info = new array_info;

    array_info_iter iter1(this);
    while (!iter1.is_empty()) {  // normalize each dimension
        access_vector *temp_vector = iter1.step();
        temp_vector->normalize_step_ref();
        temp_info->append(temp_vector);
    }

    clear();
    // copy normalized versions back onto this array_info
    array_info_iter iter2(temp_info);
    while (!iter2.is_empty()) {
        access_vector *temp_vector = iter2.step();
        append(temp_vector);
	temp_info->remove(iter2.cur_elem());
    }
    delete temp_info;
}


// normalize an acess_vector for a reference
// ie ai+bj+... b => a*step1*i+a*lb1 + b*step2*j+b*lb2 + ...
void access_vector::normalize_step_ref()
{
    if (too_messy) return;  // cannot undo a mess

    access_list_iter ai(&elts);
    while(!ai.is_empty()) {   // for each induction variable referenced
        normalize_step_ref(ai.step());
        if (too_messy) return;  // cannot undo a mess
    }
}


// normalize one variable of an access variable referenced
// ie a*i => a*step*i + a*lb
void access_vector::normalize_step_ref(access_list_e *induct)
{
    tree_node * tn = (tree_node *)induct->var();
    dep_for_annote * ann  =
        (dep_for_annote *) tn->peek_annote(k_dep_for_annote);
    array_info *low = ann->lb;
    access_vector *step = ann->stp;

    if (step->too_messy || !step->elts.is_empty()) {
        too_messy++;
        return;
    }
    if (step->con == 1) return;  // already normalized

    /* keep only the first lower bound */
    if (low->count() > 1) {
        array_info *ai = new array_info();
        ai->append(new access_vector(low->first()));
        delete low;
        ann->lb = low = ai;
    }
    assert(low->count() == 1);
    access_vector *lb = low->first();


    if (lb->too_messy || (step->con == 0)) {
        too_messy++;
        return;
    }


    int a = (long) induct->info;
    induct->info = (void *) (a*step->con);  // a*i => a*step*i

    // now add a*lb to access vector
    access_list_iter ai(&lb->elts);
    while (!ai.is_empty()) {
        access_list_e *lb_element = ai.step();
        access_list_e *this_element = elts.search(lb_element->var());
        if (this_element) {
            this_element->info = (void *) (this_element->val() +
                                           a*(lb_element->val()));
        } else {
            add((tree_node *) lb_element->var(),a*lb_element->val());
        }
    }
    con += a*lb->con;

    // now add it to conregs and conpaths and memregs
    ai.reset(&lb->conregs);
    while (!ai.is_empty()) {
        access_list_e *lb_element = ai.step();
        access_list_e *this_element = conregs.search(lb_element->var());
        if (this_element) {
            this_element->info = (void *) (this_element->val() +
                                           a*(lb_element->val()));
        } else {
            add((var_sym *) lb_element->var(),0,a*lb_element->val());
        }
    }
    ai.reset(&lb->memregs);
    while (!ai.is_empty()) {
        access_list_e *lb_element = ai.step();
        access_list_e *this_element = memregs.search(lb_element->var());
        if (this_element) {
            this_element->info = (void *) (this_element->val() +
                                           a*(lb_element->val()));
        } else {
            add((var_sym *) lb_element->var(),1,a*lb_element->val());
        }
    }
}




// normalize the access vectors for the bounds and steps of a loop
void normalize_step_loops(array_info *low, array_info *up,
			  access_vector *step, tree_for_test test)
{
    if (step->too_messy) return;
    if (step->is_const() &&
        step ->con == 1) return;   // loop already step_normalized

    /* keep only the first lower/upper bound */
    access_vector *lb=low->pop();
    while (low->count()) delete low->pop();
    low->push(lb);
    assert(low->count() == 1);

    access_vector *ub=up->pop();
    while (up->count()) delete up->pop();
    up->push(ub);
    assert(up->count() == 1);

    if (!step->is_const()) {  // non constant step, give up
        step->too_messy++;
        lb->too_messy++;
        ub->too_messy++;
        return;
    }
    if (step->con == 0) {  // perverse case, die
        step->too_messy++;
        lb->too_messy++;
        ub->too_messy++;
        return;
    }
    if (lb->too_messy) {  // can't normalize so give up
        step->too_messy++;
        lb->too_messy++;
        ub->too_messy++;
        return;
    }
    int ub_offset = 0;
    if (step->con < 0) {  // reverse sign of test
        if (test == FOR_SLT) {
            ub_offset = -1;
        } else if (test == FOR_ULT) {
            ub_offset = -1;
        } else if (test == FOR_SLTE) {
            ;
        } else if (test == FOR_ULTE) {
            ;
        } else if (test == FOR_SGT) {
            ub_offset = -1;
        } else if (test == FOR_UGT) {
            ub_offset = -1;
        } else if (test == FOR_SGTE) {
            ;
        } else if (test == FOR_UGTE) {
            ;
        } else {
            step->too_messy++;
            lb->too_messy++;
            ub->too_messy++;
            return;
        }
    }

    if (ub->too_messy) {  // can normalize, but ub stays messy
    } else {
        // ub <- ub - lb
        access_list_iter ai(&lb->elts);
        while(!ai.is_empty()) {  // subtract off each lower bound component
            access_list_e *lb_element = ai.step();
            access_list_e *ub_element = ub->elts.search(lb_element->var());
            if (ub_element) {
                ub_element->info = (void *) (ub_element->val() -
                                             lb->val((tree_node *) lb_element->var()));
            } else {
                ub->add((tree_node *) lb_element->key,
                        -lb->val((tree_node *) lb_element->var()));
            }
        }
        ub->con -= lb->con;
        ai.reset(&lb->conregs);
        while(!ai.is_empty()) {  // subtract off each lower bound component
            access_list_e *lb_element = ai.step();
            access_list_e *ub_element = ub->conregs.search(lb_element->var());
            if (ub_element) {
                ub_element->info = (void *) (ub_element->val() -
                                             lb->val((var_sym *)lb_element->var(),0));
            } else {
                ub->add((var_sym *) lb_element->key,0,
                        -lb->val((var_sym *)lb_element->var(),0));
            }
        }
        ai.reset(&lb->memregs);
        while(!ai.is_empty()) {  // subtract off each lower bound component
            access_list_e *lb_element = ai.step();
            access_list_e *ub_element = ub->memregs.search(lb_element->var());
            if (ub_element) {
                ub_element->info = (void *) (ub_element->val() -
                                             lb->val((var_sym *)lb_element->var(),1));
            } else {
                ub->add((var_sym *) lb_element->key,1,
                        -lb->val((var_sym *)lb_element->var(),1));
            }
        }

        // ub <- ub / step
        ai.reset(&ub->elts);
        while(!ai.is_empty()) {
            access_list_e *ub_element = ai.step();
            if ((ub_element->val() % step->con) != 0)  {
                /*
                  lb->too_messy++;
                  ub->too_messy++;
                  step->too_messy++;
                  return;
                  */
                ub->too_messy++;
                lb->con = 0;
                while(!lb->elts.is_empty())
                    lb->elts.pop();// erase lb vars
                while (!lb->conregs.is_empty())
                    lb->conregs.pop();
                while (!lb->memregs.is_empty())
                    lb->memregs.pop();
                step->con = 1;
                return;
            } else {
                ub_element->info = (void *)
                    (ub->val((tree_node *)ub_element->key) / step->con);
            }
        }
        if ((ub->con % step->con) != 0)  {
            ub->too_messy++;
            lb->con = 0;
            while(!lb->elts.is_empty()) lb->elts.pop();// erase lb vars
            while (!lb->conregs.is_empty()) lb->conregs.pop();
            while (!lb->memregs.is_empty()) lb->memregs.pop();
            step->con = 1;
            return;
        } else {
            ub->con /= step->con;
            ub->con += ub_offset;
        }
        ai.reset(&ub->conregs);
        while(!ai.is_empty()) {
            access_list_e *ub_element = ai.step();
            if ((ub_element->val() % step->con) != 0)  {
                ub->too_messy++;
                lb->con = 0;
                while(!lb->elts.is_empty())
                    lb->elts.pop();// erase lb vars
                while (!lb->conregs.is_empty())
                    lb->conregs.pop();
                while (!lb->memregs.is_empty())
                    lb->memregs.pop();
                step->con = 1;
                return;
/*
  lb->too_messy++;
  ub->too_messy++;
  step->too_messy++;
  return;
  */
            } else {
                ub_element->info = (void *)
                    (ub->val((var_sym *)ub_element->key,0) / step->con);
            }
        }
        ai.reset(&ub->memregs);
        while(!ai.is_empty()) {
            access_list_e *ub_element = ai.step();
            if ((ub_element->val() % step->con) != 0)  {
/*
  lb->too_messy++;
  ub->too_messy++;
  step->too_messy++;
  */
                ub->too_messy++;
                lb->con = 0;
                while(!lb->elts.is_empty())
                    lb->elts.pop();// erase lb vars
                while (!lb->conregs.is_empty())
                    lb->conregs.pop();
                while (!lb->memregs.is_empty())
                    lb->memregs.pop();
                step->con = 1;
                return;
            } else {
                ub_element->info = (void *)
                    (ub->val((var_sym *)ub_element->key,1) / step->con);
            }
        }
    }

    lb->con = 0;
    while (!lb->elts.is_empty()) lb->elts.pop();  // erase lb vars
    while (!lb->conregs.is_empty()) lb->conregs.pop();
    while (!lb->memregs.is_empty()) lb->memregs.pop();
    step->con = 1;
}


void normalize_test(array_info *low,array_info *up,
		    access_vector *step, tree_for_test test)
{
    array_info_iter ai(up);
    while (!ai.is_empty()) {
        access_vector *ub = ai.step();

        if (test == FOR_SLT) {
            ub->con--;
            if (ub->min) (*ub->min) --;
        } else if (test == FOR_ULT) {
            ub->con--;
            if (ub->min) (*ub->min) --;
        } else if ((test == FOR_SLTE) || (test == FOR_ULTE)) {
        } else {
            ub->too_messy++;
            step->too_messy++;
        }
    }
    array_info_iter ai2(low);
    while (!ai2.is_empty()) {
        access_vector *lb = ai2.step();

        if (test == FOR_SLT) {
        } else if (test == FOR_ULT) {
        } else if ((test == FOR_SLTE) || (test == FOR_ULTE)) {
        } else {
            lb->too_messy++;
        }
    }

}
