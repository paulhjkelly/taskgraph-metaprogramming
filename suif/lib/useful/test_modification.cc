/* file "test_modification.cc" */

/*  Copyright (c) 1994,1995 Stanford University

    All rights reserved.

    This software is provided under the terms described in
    the "suif_copyright.h" include file. */

#include <suif_copyright.h>

/*
 *  This is the implementation of routines to test for potential
 *  modification of variables or expressions by SUIF code, for the
 *  SUIF library of miscellaneous useful routines.
 */

#define _MODULE_ "libsueful.a"

#define RCS_BASE_FILE test_modification_cc

#include "useful_internal.h"

RCS_BASE(
    "$Id$")

/*----------------------------------------------------------------------*
    Begin Type Declarations
 *----------------------------------------------------------------------*/

/*----------------------------------------------------------------------*
    End Type Declarations
 *----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*
    Begin Private Function Declarations
 *----------------------------------------------------------------------*/

static boolean node_might_modify(var_sym *the_var, tree_node *the_node);
static boolean expr_might_modify(var_sym *the_var, instruction *the_instr);
static boolean op_might_modify(var_sym *the_var, operand the_op);

/*----------------------------------------------------------------------*
    End Private Function Declarations
 *----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*
    Begin Public Function Implementations
 *----------------------------------------------------------------------*/

extern boolean might_modify(var_sym *the_variable, tree_node_list *node_list)
  {
    if (the_variable->type()->is_volatile())
        return TRUE;

    tree_node_list_iter the_iter(node_list);
    while (!the_iter.is_empty())
      {
        tree_node *this_node = the_iter.step();
        if (node_might_modify(the_variable, this_node))
            return TRUE;
      }

    return FALSE;
  }

extern boolean might_modify(operand the_operand, tree_node_list *node_list)
  {
    switch (the_operand.kind())
      {
        case OPER_NULL:
            return FALSE;
        case OPER_SYM:
          {
            var_sym *the_var = the_operand.symbol();
            if ((node_list->parent() != NULL) &&
                (node_list->parent()->scope() != NULL) &&
                (!node_list->parent()->scope()->is_ancestor(
                          the_var->parent())))
              {
                return TRUE;
              }
            return might_modify(the_var, node_list);
          }
        case OPER_INSTR:
          {
            instruction *instr = the_operand.instr();
            return might_modify(instr, node_list);
          }
        default:
            assert(FALSE);
            return TRUE;
      }
  }

extern boolean might_modify(instruction *the_instr, tree_node_list *node_list)
  {
    if (instr_is_impure_call(the_instr) || (the_instr->opcode() == io_gen))
        return TRUE;

    if ((the_instr->opcode() == io_lod) || (the_instr->opcode() == io_cal))
      {
        unsigned num_srcs = the_instr->num_srcs();
        unsigned src_num = 0;
        if (the_instr->opcode() == io_cal)
            src_num = 1;
        for (; src_num < num_srcs; ++src_num)
          {
            operand this_op = the_instr->src_op(src_num);
            if (this_op.type()->unqual()->is_ptr())
              {
                sym_node *addr_symbol = operand_address_root_symbol(this_op);
                if (addr_symbol == NULL)
                    return TRUE;
                if ((node_list->parent() != NULL) &&
                    (node_list->parent()->scope() != NULL) &&
                    (!node_list->parent()->scope()->is_ancestor(
                              addr_symbol->parent())))
                  {
                    return TRUE;
                  }
                if (addr_symbol->is_var())
                  {
                    var_sym *addr_var = (var_sym *)addr_symbol;
                    if (might_modify(addr_var, node_list))
                        return TRUE;
                  }
              }
          }
      }

    unsigned num_srcs = the_instr->num_srcs();
    for (unsigned src_num = 0; src_num < num_srcs; ++src_num)
      {
        if (might_modify(the_instr->src_op(src_num), node_list))
            return TRUE;
      }

    return FALSE;
  }

/*----------------------------------------------------------------------*
    End Public Function Implementations
 *----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*
    Begin Private Function Implementations
 *----------------------------------------------------------------------*/

static boolean node_might_modify(var_sym *the_var, tree_node *the_node)
  {
    switch (the_node->kind())
      {
        case TREE_INSTR:
          {
            tree_instr *the_tree_instr = (tree_instr *)the_node;
            return expr_might_modify(the_var, the_tree_instr->instr());
          }
        case TREE_LOOP:
          {
            tree_loop *the_loop = (tree_loop *)the_node;
            if (might_modify(the_var, the_loop->body()))
                return TRUE;
            return might_modify(the_var, the_loop->test());
          }
        case TREE_FOR:
          {
            tree_for *the_for = (tree_for *)the_node;
            if (the_var->overlaps(the_for->index()))
                return TRUE;
            if (op_might_modify(the_var, the_for->lb_op()))
                return TRUE;
            if (op_might_modify(the_var, the_for->ub_op()))
                return TRUE;
            if (op_might_modify(the_var, the_for->step_op()))
                return TRUE;
            if (might_modify(the_var, the_for->body()))
                return TRUE;
            return might_modify(the_var, the_for->landing_pad());
          }
        case TREE_IF:
          {
            tree_if *the_if = (tree_if *)the_node;
            if (might_modify(the_var, the_if->header()))
                return TRUE;
            if (might_modify(the_var, the_if->then_part()))
                return TRUE;
            return might_modify(the_var, the_if->else_part());
          }
        case TREE_BLOCK:
          {
            tree_block *the_block = (tree_block *)the_node;
            return might_modify(the_var, the_block->body());
          }
        default:
            assert(FALSE);
            return TRUE;
      }
  }

static boolean expr_might_modify(var_sym *the_var, instruction *the_instr)
  {
    operand dest_op = the_instr->dst_op();
    if (dest_op.is_symbol() && (dest_op.symbol() == the_var))
        return TRUE;

    switch (the_instr->opcode())
      {
        case io_cal:
            if (!instr_is_impure_call(the_instr))
                break;
            /* fall through */
        case io_gen:
          {
            if (!the_var->is_auto())
                return TRUE;

            boolean is_fortran = FALSE;
            if ((the_instr->owner() != NULL) &&
                (the_instr->owner()->proc()->src_lang() == src_fortran))
              {
                is_fortran = TRUE;
              }
            if (!is_fortran)
              {
                if (the_var->root_ancestor()->is_addr_taken())
                    return TRUE;
              }

            unsigned num_srcs = the_instr->num_srcs();
            for (unsigned src_num = 0; src_num < num_srcs; ++src_num)
              {
                operand this_op = the_instr->src_op(src_num);
                if (this_op.type()->unqual()->is_ptr())
                  {
                    sym_node *modified_sym =
                            operand_address_root_symbol(this_op);
                    if ((modified_sym != NULL) && (modified_sym->is_var()))
                      {
                        var_sym *modified_var = (var_sym *)modified_sym;
                        if (modified_var->overlaps(the_var))
                            return TRUE;
                      }
                  }
              }
            break;
          }
        case io_str:
        case io_memcpy:
          {
            in_rrr *the_rrr = (in_rrr *)the_instr;
            operand dest_addr = the_rrr->dst_addr_op();
            sym_node *modified_sym = operand_address_root_symbol(dest_addr);
            if (modified_sym != NULL)
              {
                if (modified_sym->is_var())
                  {
                    var_sym *modified_var = (var_sym *)modified_sym;
                    if (modified_var->overlaps(the_var))
                        return TRUE;
                  }
              }
            else
              {
                if (the_var->root_ancestor()->is_addr_taken())
                    return TRUE;
              }
            break;
          }
        default:
            break;
      }

    unsigned num_srcs = the_instr->num_srcs();
    for (unsigned src_num = 0; src_num < num_srcs; ++src_num)
      {
        if (op_might_modify(the_var, the_instr->src_op(src_num)))
            return TRUE;
      }

    return FALSE;
  }

static boolean op_might_modify(var_sym *the_var, operand the_op)
  {
    if (the_op.is_expr())
        return expr_might_modify(the_var, the_op.instr());
    else
        return FALSE;
  }

/*----------------------------------------------------------------------*
    End Private Function Implementations
 *----------------------------------------------------------------------*/
