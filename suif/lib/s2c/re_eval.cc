/* file "re_eval.cc" */

/*  Copyright (c) 1994 Stanford University

    All rights reserved.

    This software is provided under the terms described in
    the "suif_copyright.h" include file. */

#include <suif_copyright.h>

/*
 *  This file implements some pre-processing of the SUIF code to make
 *  sure nothing is re-evaluated when it should be, for the s2c
 *  program for the SUIF system.
 */

#define RCS_BASE_FILE re_eval_cc

#include "s2c.h"

RCS_BASE(
    "$Id$")

/*----------------------------------------------------------------------*
    Begin Documentation
 *----------------------------------------------------------------------*

 *----------------------------------------------------------------------*
    End Documentation
 *----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*
    Begin Private Type Definitions
 *----------------------------------------------------------------------*/

/*----------------------------------------------------------------------*
    End Private Type Definitions
 *----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*
    Begin Public Global Variables
 *----------------------------------------------------------------------*/

/*----------------------------------------------------------------------*
    End Public Global Variables
 *----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*
    Begin Private Global Variables
 *----------------------------------------------------------------------*/

/*----------------------------------------------------------------------*
    End Private Global Variables
 *----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*
    Begin Private Function Declarations
 *----------------------------------------------------------------------*/

static void make_operands_re_eval_ok(instruction *the_instr);
static boolean ok_for_re_evaluation(operand the_operand);
static operand make_temporary(operand the_operand, tree_node *owner);
static boolean op_contains_spilled_macro_func(operand the_op);

/*----------------------------------------------------------------------*
    End Private Function Declarations
 *----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*
    Begin Public Function Implementations
 *----------------------------------------------------------------------*/

/*
 *  In SUIF, to evaluate an instruction, each of its operands is
 *  evaluated exactly once.  If the instruction has to be represented
 *  by a macro, though, each operand might be evaluated more than
 *  once.  This is true for all the macros we use.  So the operands of
 *  such instructions are changed so that it will be harmless to
 *  re-evaluate them by using a temporary variable if necessary.
 */
extern void fix_re_eval_instr(instruction *the_instr)
  {
    assert(the_instr != NULL);

    if_ops opcode = the_instr->opcode();

    for (int index = 0; macro_table[index].replacement != NULL; ++index)
      {
        if (macro_table[index].opcode == opcode)
          {
            make_operands_re_eval_ok(the_instr);
            break;
          }
      }
  }

/*
 *  In SUIF, the semantics of a TREE_FOR are that the upper bound and
 *  step are evaluated exactly once, before any iterations of the
 *  body.  In a C ``for'' loop, these are re-evaluated at every
 *  iteration.  So we make sure it is harmless to re-evaluate the
 *  upper bound and step by using a temporary variable if necessary.
 */
extern void fix_re_eval_for(tree_for *the_for)
  {
    tree_node_list *lpad = the_for->landing_pad();
    tree_node_list *body = the_for->body();

    if (!the_for->landing_pad()->is_empty())
      {
        operand lb_operand = the_for->lb_op();
        if ((!ok_for_re_evaluation(lb_operand)) ||
            might_modify(lb_operand, lpad))
          {
            the_for->set_lb_op(make_temporary(lb_operand, the_for));
          }
      }

    operand ub_operand = the_for->ub_op();
    if ((!ok_for_re_evaluation(ub_operand)) ||
        might_modify(ub_operand, lpad) || might_modify(ub_operand, body) ||
        (drop_bounds && op_contains_spilled_macro_func(ub_operand)))
      {
        the_for->set_ub_op(make_temporary(ub_operand, the_for));
      }

    operand step_operand = the_for->step_op();
    if ((!ok_for_re_evaluation(step_operand)) ||
        might_modify(step_operand, lpad) || might_modify(step_operand, body) ||
        (drop_bounds && op_contains_spilled_macro_func(step_operand)))
      {
        the_for->set_step_op(make_temporary(step_operand, the_for));
      }
  }

/*----------------------------------------------------------------------*
    End Public Function Implementations
 *----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*
    Begin Private Function Implementations
 *----------------------------------------------------------------------*/

static void make_operands_re_eval_ok(instruction *the_instr)
  {
    unsigned num_srcs = the_instr->num_srcs();
    for (unsigned src_num = 0; src_num < num_srcs; ++src_num)
      {
        operand this_src = the_instr->src_op(src_num);
        if (!ok_for_re_evaluation(this_src))
          {
            operand new_src = make_temporary(this_src, the_instr->owner());
            the_instr->set_src_op(src_num, new_src);
          }
      }
  }

static boolean ok_for_re_evaluation(operand the_operand)
  {
    switch (the_operand.kind())
      {
        case OPER_NULL:
            return TRUE;
        case OPER_SYM:
            return !the_operand.symbol()->type()->is_volatile();
        case OPER_INSTR:
          {
            instruction *the_instr = the_operand.instr();

            if_ops opcode = the_instr->opcode();
            if ((opcode == io_lod) || (opcode == io_cal))
                return FALSE;

            unsigned num_srcs = the_instr->num_srcs();
            for (unsigned src_num = 0; src_num < num_srcs; ++src_num)
              {
                if (!ok_for_re_evaluation(the_instr->src_op(src_num)))
                    return FALSE;
              }
            return TRUE;
          }
        default:
            assert(FALSE);
            return FALSE;
      }
  }

static operand make_temporary(operand the_operand, tree_node *owner)
  {
    assert(owner != NULL);

    the_operand.remove();

    instruction *assignment_instr = NULL;
    if (the_operand.is_expr())
      {
        assignment_instr = the_operand.instr();
      }
    else if (the_operand.is_symbol())
      {
        var_sym *the_var = the_operand.symbol();
        assignment_instr =
                new in_rrr(io_cpy, the_var->type(), operand(), the_operand);
      }
    else
      {
        assert(FALSE);
      }

    type_node *the_type = assignment_instr->result_type();
    var_sym *new_var = owner->scope()->new_unique_var(the_type, "__s2c_tmp");
    assignment_instr->set_dst(operand(new_var));

    tree_instr *new_node = new tree_instr(assignment_instr);
    owner->parent()->insert_before(new_node, owner->list_e());

    return operand(new_var);
  }

static boolean op_contains_spilled_macro_func(operand the_op)
  {
    if (the_op.is_symbol())
      {
        var_sym *the_var = the_op.symbol();
        return (the_var->is_addr_taken() || the_var->is_global());
      }
    else if (the_op.is_expr())
      {
        instruction *this_instr = the_op.instr();
        switch (this_instr->opcode())
          {
            case io_min:
            case io_max:
            case io_abs:
            case io_lod:
            case io_divfloor:
            case io_divceil:
            case io_gen:
                return TRUE;
            default:
                break;
          }

        unsigned num_srcs = this_instr->num_srcs();
        for (unsigned src_num = 0; src_num < num_srcs; ++src_num)
          {
            if (op_contains_spilled_macro_func(this_instr->src_op(src_num)))
                return TRUE;
          }
        return FALSE;
      }
    else
      {
        return FALSE;
      }
  }

/*----------------------------------------------------------------------*
    End Private Function Implementations
 *----------------------------------------------------------------------*/
