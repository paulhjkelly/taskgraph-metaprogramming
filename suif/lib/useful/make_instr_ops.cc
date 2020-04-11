/* file "make_instr_ops.cc" */

/*  Copyright (c) 1995 Stanford University

    All rights reserved.

    This software is provided under the terms described in
    the "suif_copyright.h" include file. */

#include <suif_copyright.h>

/*
 *  This is the implementation of routines to build instruction
 *  operands out of single-use, single-def variables in the same basic
 *  block and scope.
 */

#define _MODULE_ "libsueful.a"

#define RCS_BASE_FILE make_instr_ops_cc

#include "useful_internal.h"

RCS_BASE(
    "$Id$")


enum bound_kind {BK_LB, BK_UB, BK_STEP};

struct use_def_record
  {
    instruction *def;
    instruction *use;
    unsigned use_src_num;
    tree_for *for_use;
    bound_kind use_bound;

    use_def_record(void)  { def = NULL; use = NULL; for_use = NULL; }
  };


boolean make_instr_for_bounds = FALSE;


static const char *k_useful_mio_use_def = NULL;

static var_sym_list *pending_vars = NULL;


static void start_symtab(block_symtab *the_symtab);
static void finish_symtab(block_symtab *the_symtab);
static void handle_instr(instruction *the_instr);
static void handle_for_bounds(tree_for *the_for);
static void handle_a_bound(tree_for *the_for, operand bound_op,
                           bound_kind which_bound);
static void handle_tree_node_list(tree_node_list *node_list);
static void handle_tree_node(tree_node *the_node);
static void kill_live(void);


extern void init_make_instr_ops(void)
  {
    k_useful_mio_use_def = lexicon->enter("useful mio use def")->sp;
  }

extern void make_instr_ops(tree_node *the_node)
  {
    assert(pending_vars == NULL);
    pending_vars = new var_sym_list;
    handle_tree_node(the_node);
    assert(pending_vars->is_empty());
    delete pending_vars;
    pending_vars = NULL;
  }


static void start_symtab(block_symtab *the_symtab)
  {
    sym_node_list_iter sym_iter(the_symtab->symbols());
    while (!sym_iter.is_empty())
      {
        sym_node *this_sym = sym_iter.step();
        if (!this_sym->is_var())
            continue;
        var_sym *this_var = (var_sym *)this_sym;
        if (this_var->is_addr_taken() || (!this_var->is_auto()) ||
            this_var->is_param() || (this_var->parent_var() != NULL) ||
            (this_var->num_children() != 0))
          {
            continue;
          }
        this_var->append_annote(k_useful_mio_use_def, new use_def_record);
      }
  }

static void finish_symtab(block_symtab *the_symtab)
  {
    sym_node_list_iter sym_iter(the_symtab->symbols());
    while (!sym_iter.is_empty())
      {
        sym_node *this_sym = sym_iter.step();
        use_def_record *the_usedef =
                (use_def_record *)(this_sym->get_annote(k_useful_mio_use_def));
        if (the_usedef == NULL)
            continue;
        assert(this_sym->is_var());
        operand var_op = operand((var_sym *)this_sym);
        if (the_usedef->def != NULL)
          {
            assert(the_usedef->def->dst_op() == var_op);
            operand new_src = operand(the_usedef->def);
            if (the_usedef->use != NULL)
              {
                assert(the_usedef->for_use == NULL);
                assert(the_usedef->use->src_op(the_usedef->use_src_num) ==
                       var_op);
                the_usedef->use->set_src_op(the_usedef->use_src_num, new_src);
              }
            else
              {
                assert(the_usedef->for_use != NULL);
                tree_instr *def_parent = the_usedef->def->parent();
                assert(def_parent->instr() == the_usedef->def);
                the_usedef->def->remove();
                kill_node(def_parent);
                switch (the_usedef->use_bound)
                  {
                    case BK_LB:
                        assert(the_usedef->for_use->lb_op() == var_op);
                        the_usedef->for_use->set_lb_op(new_src);
                        break;
                    case BK_UB:
                        assert(the_usedef->for_use->ub_op() == var_op);
                        the_usedef->for_use->set_ub_op(new_src);
                        break;
                    case BK_STEP:
                        assert(the_usedef->for_use->step_op() == var_op);
                        the_usedef->for_use->set_step_op(new_src);
                        break;
                    default:
                        assert(FALSE);
                  }
              }
          }
        delete the_usedef;
      }
  }

static void handle_instr(instruction *the_instr)
  {
    unsigned num_srcs = the_instr->num_srcs();
    for (unsigned src_num = 0; src_num < num_srcs; ++src_num)
      {
        operand this_src = the_instr->src_op(src_num);
        if (this_src.is_symbol())
          {
            var_sym *this_var = this_src.symbol();
            annote *usedef_annote =
                    this_var->annotes()->peek_annote(k_useful_mio_use_def);
            if (usedef_annote != NULL)
              {
                use_def_record *the_usedef =
                        (use_def_record *)(usedef_annote->data());
                assert(the_usedef != NULL);
                if ((the_usedef->def == NULL) || (the_usedef->use != NULL) ||
                    (the_usedef->for_use != NULL))
                  {
                    annote *removed_annote =
                            this_var->annotes()->get_annote(
                                    k_useful_mio_use_def);
                    assert(removed_annote == usedef_annote);
                    delete usedef_annote;
                    delete the_usedef;
                  }
                else
                  {
                    the_usedef->use = the_instr;
                    the_usedef->use_src_num = src_num;
                  }
              }
          }
        else if (this_src.is_expr())
          {
            handle_instr(this_src.instr());
          }
      }

    operand dest_op = the_instr->dst_op();
    if (dest_op.is_symbol())
      {
        var_sym *dest_var = dest_op.symbol();
        annote *usedef_annote =
                dest_var->annotes()->peek_annote(k_useful_mio_use_def);
        if (usedef_annote != NULL)
          {
            use_def_record *the_usedef =
                    (use_def_record *)(usedef_annote->data());
            assert(the_usedef != NULL);
            if (the_usedef->def != NULL)
              {
                annote *removed_annote =
                        dest_var->annotes()->get_annote(k_useful_mio_use_def);
                assert(removed_annote == usedef_annote);
                delete usedef_annote;
                delete the_usedef;
              }
            else
              {
                the_usedef->def = the_instr;
                assert(pending_vars != NULL);
                pending_vars->append(dest_var);
              }
          }
      }

    switch (the_instr->opcode())
      {
        case io_btrue:
        case io_bfalse:
        case io_jmp:
        case io_cal:
        case io_ret:
        case io_lab:
        case io_mbr:
        case io_gen:
            kill_live();
            break;
        default:
            break;
      }
  }

static void handle_for_bounds(tree_for *the_for)
  {
    handle_a_bound(the_for, the_for->lb_op(), BK_LB);
    handle_a_bound(the_for, the_for->ub_op(), BK_UB);
    handle_a_bound(the_for, the_for->step_op(), BK_STEP);
  }

static void handle_a_bound(tree_for *the_for, operand bound_op,
                           bound_kind which_bound)
  {
    if (bound_op.is_symbol())
      {
        var_sym *this_var = bound_op.symbol();
        annote *usedef_annote =
                this_var->annotes()->peek_annote(k_useful_mio_use_def);
        if (usedef_annote != NULL)
          {
            use_def_record *the_usedef =
                    (use_def_record *)(usedef_annote->data());
            assert(the_usedef != NULL);
            if ((the_usedef->def == NULL) || (the_usedef->use != NULL) ||
                (the_usedef->for_use != NULL))
              {
                annote *removed_annote =
                        this_var->annotes()->get_annote(k_useful_mio_use_def);
                assert(removed_annote == usedef_annote);
                delete usedef_annote;
                delete the_usedef;
              }
            else
              {
                the_usedef->for_use = the_for;
                the_usedef->use_bound = which_bound;
              }
          }
      }
    else if (bound_op.is_expr())
      {
        handle_instr(bound_op.instr());
      }
  }

static void handle_tree_node_list(tree_node_list *node_list)
  {
    kill_live();

    tree_node_list_iter node_iter(node_list);
    while (!node_iter.is_empty())
      {
        tree_node *this_node = node_iter.step();
        handle_tree_node(this_node);
      }

    kill_live();
  }

static void handle_tree_node(tree_node *the_node)
  {
    switch (the_node->kind())
      {
        case TREE_INSTR:
          {
            tree_instr *the_tree_instr = (tree_instr *)the_node;
            handle_instr(the_tree_instr->instr());
            break;
          }
        case TREE_FOR:
          {
            tree_for *the_for = (tree_for *)the_node;
            handle_for_bounds(the_for);
            handle_tree_node_list(the_for->body());
            handle_tree_node_list(the_for->landing_pad());
            break;
          }
        case TREE_BLOCK:
          {
            tree_block *the_block = (tree_block *)the_node;
            start_symtab(the_block->symtab());
            handle_tree_node_list(the_block->body());
            finish_symtab(the_block->symtab());
            break;
          }
        default:
          {
            unsigned num_children = the_node->num_child_lists();
            for (unsigned child_num = 0; child_num < num_children; ++child_num)
                handle_tree_node_list(the_node->child_list_num(child_num));
            break;
          }
      }
  }

static void kill_live(void)
  {
    while (!pending_vars->is_empty())
      {
        var_sym *this_var = pending_vars->pop();
        annote *usedef_annote =
                this_var->annotes()->peek_annote(k_useful_mio_use_def);
        if (usedef_annote != NULL)
          {
            use_def_record *the_usedef =
                    (use_def_record *)(usedef_annote->data());
            assert(the_usedef != NULL);
            assert(the_usedef->def != NULL);
            if ((the_usedef->use == NULL) && (the_usedef->for_use == NULL))
              {
                annote *removed_annote =
                        this_var->annotes()->get_annote(k_useful_mio_use_def);
                assert(removed_annote == usedef_annote);
                delete usedef_annote;
                delete the_usedef;
              }
          }
      }
  }
