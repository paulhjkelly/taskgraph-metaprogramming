/* file "dismantle.cc" */

/*  Copyright (c) 1995 Stanford University

    All rights reserved.

    This software is provided under the terms described in
    the "suif_copyright.h" include file. */

#include <suif_copyright.h>

/*
 *  This file implements routines to dismantle tree_node's into
 *  lower-level constructs.
 */

#define _MODULE_ "libuseful.a"

#define RCS_BASE_FILE useful_dismantle_cc

#include "useful_internal.h"

RCS_BASE(
    "$Id$")

/*----------------------------------------------------------------------*
    Begin Constant Declarations
 *----------------------------------------------------------------------*/

/*----------------------------------------------------------------------*
    End Constant Declarations
 *----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*
    Begin Type Declarations
 *----------------------------------------------------------------------*/

/*----------------------------------------------------------------------*
    End Type Declarations
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

static void dismantle_for(tree_for *the_for);
static void dismantle_loop(tree_loop *the_loop);
static void dismantle_if(tree_if *the_if);
static void dismantle_block(tree_block *the_block);

/*----------------------------------------------------------------------*
    End Private Function Declarations
 *----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*
    Begin Public Function Implementations
 *----------------------------------------------------------------------*/

extern void dismantle(tree_node *the_node)
  {
    switch (the_node->kind())
      {
        case TREE_INSTR:
            error_line(1, the_node, "attempt to dismantle tree_instr node");
            break;
        case TREE_LOOP:
          {
            tree_loop *the_loop = (tree_loop *)the_node;
            dismantle_loop(the_loop);
            break;
          }
        case TREE_FOR:
          {
            tree_for *the_for = (tree_for *)the_node;
            dismantle_for(the_for);
            break;
          }
        case TREE_IF:
          {
            tree_if *the_if = (tree_if *)the_node;
            dismantle_if(the_if);
            break;
          }
        case TREE_BLOCK:
          {
            if (the_node->is_proc())
                error_line(1, the_node, "attempt to dismantle tree_proc node");
            tree_block *the_block = (tree_block *)the_node;
            dismantle_block(the_block);
            break;
          }
        default:
            assert(FALSE);
      }
  }

/*----------------------------------------------------------------------*
    End Public Function Implementations
 *----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*
    Begin Internal Function Implementations
 *----------------------------------------------------------------------*/

/*----------------------------------------------------------------------*
    End Internal Function Implementations
 *----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*
    Begin Private Function Implementations
 *----------------------------------------------------------------------*/

static void dismantle_for(tree_for *the_for)
  {
    guard_for(the_for);

    tree_node_list *parent_list = the_for->parent();
    assert(parent_list != NULL);
    tree_node_list_e *list_e = the_for->list_e();
    assert(list_e != NULL);

    parent_list->insert_before(the_for->landing_pad(), list_e);

    var_sym *index = the_for->index();

    if (no_effects(the_for->body()) && !any_part_volatile(index->type()))
      {
        tree_node *assignment = assign(index, final_index_value(the_for));
        parent_list->insert_before(assignment, list_e);
      }
    else
      {
        operand lb_op = the_for->lb_op();
        lb_op.remove();
        parent_list->insert_before(assign(index, lb_op), list_e);

        base_symtab *base_scope = the_for->scope();
        assert(base_scope->is_block());
        block_symtab *block_scope = (block_symtab *)base_scope;

        label_sym *top_label = block_scope->new_unique_label("__top");

        tree_node_list *body = the_for->body();
        the_for->set_body(new tree_node_list);

        tree_node_list *test_list = new tree_node_list;

        tree_loop *do_loop =
                new tree_loop(body, test_list, the_for->contlab(),
                              the_for->brklab(), top_label);
        parent_list->insert_before(do_loop, list_e);

        operand step_op = the_for->step_op();
        step_op.remove();
        test_list->append(assign(index, operand(index) + step_op));

        in_bj *the_bj =
                new in_bj(io_bfalse, top_label,
                          for_test_done(the_for, operand(index)));
        test_list->append(new tree_instr(the_bj));
      }

    kill_node(the_for);
  }

static void dismantle_loop(tree_loop *the_loop)
  {
    tree_node_list *parent_list = the_loop->parent();
    assert(parent_list != NULL);

    parent_list->insert_before(label_node(the_loop->toplab()),
                               the_loop->list_e());
    parent_list->insert_before(the_loop->body(), the_loop->list_e());
    parent_list->insert_before(label_node(the_loop->contlab()),
                               the_loop->list_e());
    parent_list->insert_before(the_loop->test(), the_loop->list_e());
    parent_list->insert_before(label_node(the_loop->brklab()),
                               the_loop->list_e());

    kill_node(the_loop);
  }

static void dismantle_if(tree_if *the_if)
  {
    tree_node_list *parent_list = the_if->parent();
    assert(parent_list != NULL);

    if (!no_effects(the_if->else_part()))
      {
        base_symtab *base_scope = the_if->scope();
        assert(base_scope->is_block());
        block_symtab *block_scope = (block_symtab *)base_scope;

        label_sym *done_label = block_scope->new_unique_label("__done");
        the_if->then_part()->append(jump_node(done_label));
        the_if->else_part()->append(label_node(done_label));
      }

    parent_list->insert_before(the_if->header(), the_if->list_e());
    parent_list->insert_before(the_if->then_part(), the_if->list_e());
    parent_list->insert_before(label_node(the_if->jumpto()), the_if->list_e());
    parent_list->insert_before(the_if->else_part(), the_if->list_e());

    kill_node(the_if);
  }

static void dismantle_block(tree_block *the_block)
  {
    assert(!the_block->is_proc());

    block_symtab *the_symtab = the_block->symtab();

    base_symtab *parent_symtab = the_symtab->parent();
    assert(parent_symtab != NULL);

    type_node_list *type_list = the_symtab->types();
    while (!type_list->is_empty())
      {
        type_node *this_type = type_list->head()->contents;
        the_symtab->remove_type(this_type);
        parent_symtab->add_type(this_type);
      }

    sym_node_list *sym_list = the_symtab->symbols();
    while (!sym_list->is_empty())
      {
        sym_node *this_sym = sym_list->head()->contents;
        the_symtab->remove_sym(this_sym);
        this_sym->set_name(deconflict_sym_name(this_sym->name(), parent_symtab,
                                               this_sym->kind()));
        parent_symtab->add_sym(this_sym);
      }

    var_def_list *def_list = the_symtab->var_defs();
    while (!def_list->is_empty())
      {
        var_def *this_def = def_list->head()->contents;
        the_symtab->remove_def(this_def);
        parent_symtab->add_def(this_def);
      }

    base_symtab_list *child_list = the_symtab->children();
    while (!child_list->is_empty())
      {
        base_symtab *this_child = child_list->head()->contents;
        the_symtab->remove_child(this_child);
        parent_symtab->add_child(this_child);
      }

    tree_node_list *parent_list = the_block->parent();
    tree_node_list_e *list_e = the_block->list_e();
    assert(parent_list != NULL);
    parent_list->insert_after(the_block->body(), list_e);

    kill_node(the_block);
  }

/*----------------------------------------------------------------------*
    End Private Function Implementations
 *----------------------------------------------------------------------*/
