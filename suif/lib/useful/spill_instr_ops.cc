/* file "spill_instr_ops.cc" */

/*  Copyright (c) 1995 Stanford University

    All rights reserved.

    This software is provided under the terms described in
    the "suif_copyright.h" include file. */

#include <suif_copyright.h>

/*
 *  This is the implementation of routines to spill instruction
 *  operands and expression trees to use variables instead.
 */

#define _MODULE_ "libsueful.a"

#define RCS_BASE_FILE spill_instr_ops_cc

#include "useful_internal.h"

RCS_BASE(
    "$Id$")


static void spill_all_srcs(tree_instr *the_tree_instr);
static void spill_internal_srcs(tree_instr *the_tree_instr);
static void fix_for_op(tree_for *the_for, tree_node_list *op_list);


const char *spill_prefix = "_spill";


extern void spill_instr_ops(tree_node_list *the_list)
  {
    tree_node_list_iter the_iter(the_list);
    while (!the_iter.is_empty())
      {
        tree_node *this_node = the_iter.step();
        if (this_node->is_instr())
          {
            tree_instr *this_tree_instr = (tree_instr *)this_node;
            spill_all_srcs(this_tree_instr);
          }
      }
  }

extern void recursive_spill_instr_ops(tree_node *the_node)
  {
    switch (the_node->kind())
      {
        case TREE_INSTR:
          {
            tree_instr *the_tree_instr = (tree_instr *)the_node;
            spill_internal_srcs(the_tree_instr);
            break;
          }
        case TREE_FOR:
          {
            tree_for *the_for = (tree_for *)the_node;
            recursive_spill_instr_ops(the_for->lb_list());
            fix_for_op(the_for, the_for->lb_list());
            recursive_spill_instr_ops(the_for->ub_list());
            fix_for_op(the_for, the_for->ub_list());
            recursive_spill_instr_ops(the_for->step_list());
            fix_for_op(the_for, the_for->step_list());
            recursive_spill_instr_ops(the_for->body());
            recursive_spill_instr_ops(the_for->landing_pad());
            break;
          }
        case TREE_LOOP:
        case TREE_IF:
        case TREE_BLOCK:
          {
            unsigned num_children = the_node->num_child_lists();
            for (unsigned long child_num = 0; child_num < num_children;
                 ++child_num)
              {
                recursive_spill_instr_ops(the_node->child_list_num(child_num));
              }
            break;
          }
        default:
            assert(FALSE);
      }
  }

extern void recursive_spill_instr_ops(tree_node_list *the_list)
  {
    spill_instr_ops(the_list);
    tree_node_list_iter the_iter(the_list);
    while (!the_iter.is_empty())
      {
        tree_node *this_node = the_iter.step();
        recursive_spill_instr_ops(this_node);
      }
  }


static void spill_all_srcs(tree_instr *the_tree_instr)
  {
    instruction *this_instr = the_tree_instr->instr();
    unsigned long num_srcs = this_instr->num_srcs();
    for (unsigned long src_num = 0; src_num < num_srcs; ++src_num)
      {
        operand src_op = this_instr->src_op(src_num);
        if (src_op.is_instr())
          {
            instruction *src_instr = src_op.instr();
            tree_instr *src_parent = src_instr->parent();
            if (src_parent == the_tree_instr)
              {
                src_instr->remove();
                tree_instr *new_node = new tree_instr(src_instr);
                the_tree_instr->parent()->insert_before(new_node,
                        the_tree_instr->list_e());
                spill_all_srcs(new_node);
              }
            else
              {
                assert(src_parent->instr() == src_instr);
              }
            assert(src_instr->dst_op() == operand(this_instr));
            var_sym *spill_var =
                    the_tree_instr->scope()->new_unique_var(src_op.type(),
                                                            spill_prefix);
            src_instr->set_dst(operand());
            this_instr->set_src_op(src_num, operand());
            src_instr->set_dst(operand(spill_var));
            this_instr->set_src_op(src_num, operand(spill_var));
          }
      }
  }

static void spill_internal_srcs(tree_instr *the_tree_instr)
  {
    instruction *this_instr = the_tree_instr->instr();
    unsigned long num_srcs = this_instr->num_srcs();
    for (unsigned long src_num = 0; src_num < num_srcs; ++src_num)
      {
        operand src_op = this_instr->src_op(src_num);
        if (src_op.is_instr())
          {
            instruction *src_instr = src_op.instr();
            tree_instr *src_parent = src_instr->parent();
            if (src_parent == the_tree_instr)
              {
                src_instr->remove();
                tree_instr *new_node = new tree_instr(src_instr);
                the_tree_instr->parent()->insert_before(new_node,
                        the_tree_instr->list_e());
                spill_internal_srcs(new_node);
                assert(src_instr->dst_op() == operand(this_instr));
                var_sym *spill_var =
                        the_tree_instr->scope()->new_unique_var(src_op.type(),
                                                                spill_prefix);
                src_instr->set_dst(operand());
                this_instr->set_src_op(src_num, operand());
                src_instr->set_dst(operand(spill_var));
                this_instr->set_src_op(src_num, operand(spill_var));
              }
            else
              {
                assert(src_parent->instr() == src_instr);
              }
          }
      }
  }

static void fix_for_op(tree_for *the_for, tree_node_list *op_list)
  {
    tree_node_list_e *follow = op_list->head();
    while (TRUE)
      {
        tree_node_list_e *next = follow->next();
        if (next == NULL)
            return;
        tree_node *this_node = follow->contents;
        remove_node(this_node);
        the_for->parent()->insert_before(this_node, the_for->list_e());
        follow = next;
      }
  }
