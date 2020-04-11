/* file "inline.cc" */

/*  Copyright (c) 1996 Stanford University

    All rights reserved.

    This software is provided under the terms described in
    the "suif_copyright.h" include file. */

#include <suif_copyright.h>

/*
 * This is the implementation of inlining routines for the SUIF
 * library of miscellaneous useful routines.
 */

#define _MODULE_ "libuseful.a"

#pragma implementation "basic.h"

#define RCS_BASE_FILE inline_cc

#include "useful_internal.h"

RCS_BASE(
    "$Id$")


static var_sym *result_var = NULL;
static label_sym *return_label = NULL;


static boolean unnest_returns_on_node(tree_node *the_node);
static boolean unnest_returns_on_list(tree_node_list *the_list);
static void fix_returns_on_object(suif_object *the_object);


extern void inline_call(in_cal *the_call)
  {
    proc_sym *the_proc = proc_for_call(the_call);
    assert(the_proc != NULL);
    assert(the_proc->is_in_memory());
    force_dest_not_expr(the_call);
    base_symtab *scope = the_call->owner()->scope();
    if (the_call->result_type()->op() != TYPE_VOID)
        result_var = scope->new_unique_var(the_call->result_type(), "result");
    tree_proc *new_tree_proc = the_proc->block()->clone();
    proc_symtab *old_symtab = new_tree_proc->proc_syms();
    tree_node_list *new_body = new_tree_proc->body();

    sym_node_list_iter param_iter(old_symtab->params());
    unsigned param_num = 0;
    while (!param_iter.is_empty())
      {
        sym_node *this_sym = param_iter.step();
        assert(this_sym->is_var());
        var_sym *this_param = (var_sym *)this_sym;
        if (param_num < the_call->num_args())
          {
            operand this_actual = the_call->argument(param_num);
            this_actual.remove();
            new_body->push(assign(this_param, this_actual));
          }
        ++param_num;
      }

    new_tree_proc->set_body(new tree_node_list);
    block_symtab *new_symtab = new block_symtab(the_proc->name());
    scope->add_child(new_symtab);
    tree_block *new_block = new tree_block(new_body, new_symtab);

    while (!old_symtab->children()->is_empty())
      {
        base_symtab *this_child = old_symtab->children()->head()->contents;
        old_symtab->remove_child(this_child);
        new_symtab->add_child(this_child);
      }

    while (!old_symtab->types()->is_empty())
      {
        type_node *this_type = old_symtab->types()->head()->contents;
        old_symtab->remove_type(this_type);
        new_symtab->add_type(this_type);
      }

    while (!old_symtab->symbols()->is_empty())
      {
        sym_node *this_symbol = old_symtab->symbols()->head()->contents;
        old_symtab->remove_sym(this_symbol);
        if (this_symbol->is_var())
          {
            var_sym *this_var = (var_sym *)this_symbol;
            if (this_var->is_param())
                this_var->reset_param();
            assert(!this_var->type()->is_call_by_ref());
          }
        new_symtab->add_sym(this_symbol);
      }

    while (!old_symtab->var_defs()->is_empty())
      {
        var_def *this_def = old_symtab->var_defs()->head()->contents;
        old_symtab->remove_def(this_def);
        new_symtab->add_def(this_def);
      }

    delete new_tree_proc;
    insert_before(new_block, the_call->owner());

    if (result_var != NULL)
      {
        operand dst_op = the_call->dst_op();
        if (dst_op.is_symbol())
          {
            insert_after(assign(dst_op.symbol(), operand(result_var)),
                         new_block);
          }
      }

    return_label = new_symtab->new_unique_label();
    new_body->append(new tree_instr(new in_lab(return_label)));

    unnest_returns_on_list(new_body);

    so_walker the_walker;
    the_walker.set_post_function(fix_returns_on_object);
    the_walker.walk(new_block);

    kill_node(the_call->owner());
    result_var = NULL;
    return_label = NULL;
  }


static boolean unnest_returns_on_node(tree_node *the_node)
  {
    if (the_node->is_instr())
      {
        tree_instr *the_tree_instr = (tree_instr *)the_node;
        return (the_tree_instr->instr()->opcode() == io_ret);
      }

    boolean result = FALSE;
    unsigned num_child_lists = the_node->num_child_lists();
    for (unsigned child_num = 0; child_num < num_child_lists; ++child_num)
      {
        boolean list_result =
                unnest_returns_on_list(the_node->child_list_num(child_num));
        if (list_result)
            result = TRUE;
      }
    return result;
  }

static boolean unnest_returns_on_list(tree_node_list *the_list)
  {
    boolean result = FALSE;

    while (!the_list->is_empty())
      {
        tree_node *first_node = the_list->head()->contents;
        boolean head_result = unnest_returns_on_node(first_node);
        if (!head_result)
            break;
        result = TRUE;
        if (first_node->is_instr() || first_node->is_block())
            break;
        dismantle(first_node);
      }

    if (the_list->is_empty())
        return result;
    tree_node *previous_node = the_list->head()->contents;

    while (TRUE)
      {
        tree_node_list_e *next_e = previous_node->list_e()->next();
        if (next_e == NULL)
            return result;
        tree_node *next_node = next_e->contents;
        boolean node_result = unnest_returns_on_node(next_node);
        if (node_result)
          {
            result = TRUE;
            if (next_node->is_instr() || next_node->is_block())
                previous_node = next_node;
            else
                dismantle(next_node);
          }
        else
          {
            previous_node = next_node;
          }
      }
  }

static void fix_returns_on_object(suif_object *the_object)
  {
    if (!the_object->is_tree_obj())
        return;
    tree_node *the_node = (tree_node *)the_object;
    if (!the_node->is_instr())
        return;
    tree_instr *the_tree_instr = (tree_instr *)the_node;
    instruction *old_instr = the_tree_instr->instr();
    if (old_instr->opcode() != io_ret)
        return;
    insert_after(new tree_instr(new in_bj(io_jmp, return_label)),
                 the_tree_instr);
    if (result_var != NULL)
      {
        in_rrr *old_rrr = (in_rrr *)old_instr;
        operand result_value = old_rrr->src_op();
        instruction *new_instr;
        if (result_value.is_expr())
          {
            result_value.remove();
            new_instr = result_value.instr();
            new_instr->set_dst(operand(result_var));
          }
        else
          {
            new_instr =
                    new in_rrr(io_cpy, result_var->type()->unqual(),
                               operand(result_var), result_value);
          }
        the_tree_instr->remove_instr(old_rrr);
        the_tree_instr->set_instr(new_instr);
        delete old_rrr;
      }
    else
      {
        kill_node(the_tree_instr);
      }
  }
