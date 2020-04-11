/* file "estimate.cc" */

/*  Copyright (c) 1994 Stanford University

    All rights reserved.

    This software is provided under the terms described in
    the "suif_copyright.h" include file. */

#include <suif_copyright.h>

/*
 * This is the implementation of the rough execution time estimating
 * routines for the SUIF ``useful'' library of miscellaneous useful
 * routines.
 */

#define _MODULE_ "libuseful.a"

#pragma implementation "basic.h"

#define RCS_BASE_FILE estimate_cc

#include "useful_internal.h"

RCS_BASE(
    "$Id$")

/*----------------------------------------------------------------------*
    Begin Documentation
 *----------------------------------------------------------------------*

                Summary
                -------

        This file is a place to put general estimation routines for
        SUIF.


 *----------------------------------------------------------------------*
    End Documentation
 *----------------------------------------------------------------------*/

#define LOAD_TIME_ESTIMATE           2.0
#define STORE_TIME_ESTIMATE          2.0
#define BRANCH_TIME_ESTIMATE         2.0
#define INT_ALU_ESTIMATE             1.0
#define FP_ADD_ESTIMATE              2.0
#define FP_DIV_ESTIMATE              8.0

#define FUNC_CALL_ESTIMATE        1000.0
#define INTRINSIC_CALL_ESTIMATE     50.0
#define GENERIC_ESTIMATE           100.0

#define LOOP_ITERATIONS_ESTIMATE    100

extern double rough_time_estimate(tree_node_list *the_list)
  {
    double cost = 0.0;
    tree_node_list_iter the_iter(the_list);
    while (!the_iter.is_empty())
      {
        tree_node *this_node = the_iter.step();
        cost += rough_time_estimate(this_node);
      }
    return cost;
  }

extern double rough_time_estimate(tree_node *the_node)
  {
    switch (the_node->kind())
      {
        case TREE_INSTR:
          {
            tree_instr *the_tree_instr = (tree_instr *)the_node;
            return rough_time_estimate_instr_tree(the_tree_instr->instr());
          }
        case TREE_LOOP:
          {
            tree_loop *the_loop = (tree_loop *)the_node;
            double body_estimate = rough_time_estimate(the_loop->body());
            double test_estimate = rough_time_estimate(the_loop->test());
            return ((double) LOOP_ITERATIONS_ESTIMATE) * 
		             (body_estimate + test_estimate);
          }
        case TREE_FOR:
          {
            tree_for *the_for = (tree_for *)the_node;
            double landing_pad_est = 
		rough_time_estimate(the_for->landing_pad());
            double body_estimate = rough_time_estimate(the_for->body());
            double init_estimate = rough_time_estimate(the_for->lb_op());
            init_estimate += rough_time_estimate(the_for->ub_op());
            init_estimate += rough_time_estimate(the_for->step_op());

            int lb_int, ub_int, step_int;
            boolean lb_is_int, ub_is_int, step_is_int;
            lb_is_int = operand_is_int_const(the_for->lb_op(), &lb_int);
            ub_is_int = operand_is_int_const(the_for->ub_op(), &ub_int);
            step_is_int = operand_is_int_const(the_for->step_op(), &step_int);
            int factor = LOOP_ITERATIONS_ESTIMATE;
            if (lb_is_int && ub_is_int && step_is_int && (step_int != 0))
              {
                factor = (ub_int - lb_int) / step_int;
                if (factor < 0)
                    factor = 0;
              }
            double cost = landing_pad_est + init_estimate + 
	                  (body_estimate * (double) factor);
	    return (cost);
          }
        case TREE_IF:
          {
            tree_if *the_if = (tree_if *)the_node;
            double header_estimate = rough_time_estimate(the_if->header());
            double then_estimate = rough_time_estimate(the_if->then_part());
            double else_estimate = rough_time_estimate(the_if->else_part());
            return header_estimate + ((then_estimate + else_estimate) / 2.0);
          }
        case TREE_BLOCK:
          {
            tree_block *the_block = (tree_block *)the_node;
            return rough_time_estimate(the_block->body());
          }
        default:
            assert(FALSE);
            return (0.0);
      }
  }

extern double rough_time_estimate(operand the_operand)
  {
    if (the_operand.is_expr())
        return rough_time_estimate_instr_tree(the_operand.instr());
    else
        return (0.0);
  }

extern double rough_time_estimate_instr_tree(instruction *the_instr)
  {
    double cost = 0.0;

    unsigned num_srcs = the_instr->num_srcs();
    for (unsigned src_num = 0; src_num < num_srcs; ++src_num)
        cost += rough_time_estimate(the_instr->src_op(src_num));
    return cost + rough_time_estimate_one_instr(the_instr);
  }

extern double rough_time_estimate_one_instr(instruction *the_instr)
  {
    switch (the_instr->opcode())
      {
        case io_lod:
            return LOAD_TIME_ESTIMATE;
        case io_str:
            return STORE_TIME_ESTIMATE;
        case io_seq:
        case io_sne:
        case io_sl:
        case io_sle:
          {
            in_rrr *the_rrr = (in_rrr *)the_instr;
            if (the_rrr->src1_op().type()->unqual()->op() == TYPE_FLOAT)
                return FP_ADD_ESTIMATE;
            else
                return INT_ALU_ESTIMATE;
          }
        case io_btrue:
        case io_bfalse:
        case io_jmp:
        case io_ret:
            return BRANCH_TIME_ESTIMATE;
        case io_div:
        case io_divfloor:
        case io_divceil:
        case io_rem:
        case io_mod:
            if (the_instr->result_type()->unqual()->op() == TYPE_FLOAT)
                return FP_DIV_ESTIMATE;
            else
                return INT_ALU_ESTIMATE;
        case io_lab:
        case io_mrk:
        case io_nop:
            return (0.0);
        case io_mbr:
            return BRANCH_TIME_ESTIMATE + INT_ALU_ESTIMATE;
        case io_cal:
          {
            in_cal *the_call = (in_cal *)the_instr;
            proc_sym *the_proc = proc_for_call(the_call);
            if ((the_proc != NULL) &&
                (the_proc->annotes()->peek_annote(k_fortran_intrinsic) !=
                 NULL))
              {
                return INTRINSIC_CALL_ESTIMATE;
              }
            return FUNC_CALL_ESTIMATE;
          }
        case io_gen:
            return GENERIC_ESTIMATE;
        case io_array:
          {
            in_array *the_array = (in_array *)the_instr;
            if (the_instr->result_type()->unqual()->op() == TYPE_FLOAT)
                return ((double) the_array->dims()) * FP_ADD_ESTIMATE * 2.0;
            else
                return ((double) the_array->dims()) * INT_ALU_ESTIMATE * 2.0;
          }
        case io_memcpy:
            return LOAD_TIME_ESTIMATE + STORE_TIME_ESTIMATE;
        default:
            if (the_instr->result_type()->unqual()->op() == TYPE_FLOAT)
                return FP_ADD_ESTIMATE;
            else
                return INT_ALU_ESTIMATE;
      }
  }


