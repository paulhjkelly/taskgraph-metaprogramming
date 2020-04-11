/* file "pass1.cc" */

/*  Copyright (c) 1994 Stanford University

    All rights reserved.

    This software is provided under the terms described in
    the "suif_copyright.h" include file. */

#include <suif_copyright.h>

/*
 *  This file implements the first pass over the SUIF code for the s2c
 *  program for the SUIF system.  The first pass reads and then
 *  flushes each procedure to gather global information so that the
 *  entire program doesn't have to be in memory at once.
 */

#define RCS_BASE_FILE pass1_cc

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
    Begin Private Constants
 *----------------------------------------------------------------------*/

/*----------------------------------------------------------------------*
    End Private Constants
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

static void pass1_on_node(tree_node *the_node, void *);
static void pass1_on_instr(instruction *the_instr, void *);
static void pass1_immed_ref(immed the_immed);
static void pass1_var_ref(var_sym *the_var);

/*----------------------------------------------------------------------*
    End Private Function Declarations
 *----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*
    Begin Public Function Implementations
 *----------------------------------------------------------------------*/

extern void pass1_on_proc(tree_proc *the_proc)
  {
    assert(the_proc != NULL);
    the_proc->map(&pass1_on_node, NULL);
    pass1_on_node(the_proc, NULL);
  }

extern void pass1_on_symtab(base_symtab *the_symtab)
  {
    init_summaries_for_symtab(the_symtab, TRUE);

    if (!array_exprs)
        pass1_array_copy_on_symtab(the_symtab);

    var_def_list_iter def_iter(the_symtab->var_defs());
    while (!def_iter.is_empty())
      {
        var_def *this_def = def_iter.step();
        base_init_struct_list *init_list = read_init_data(this_def);
        base_init_struct_list_iter init_iter(init_list);
        while (!init_iter.is_empty())
          {
            base_init_struct *this_init = init_iter.step();
            if (this_init->the_multi_init() != NULL)
              {
                immed_list_iter data_iter(this_init->the_multi_init()->data);
                while (!data_iter.is_empty())
                  {
                    immed this_immed = data_iter.step();
                    pass1_immed_ref(this_immed);
                  }
              }
            else if (this_init->the_repeat_init() != NULL)
              {
                pass1_immed_ref(this_init->the_repeat_init()->data);
              }
          }
        deallocate_init_data(init_list);
      }
  }

/*----------------------------------------------------------------------*
    End Public Function Implementations
 *----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*
    Begin Private Function Implementations
 *----------------------------------------------------------------------*/

static void pass1_on_node(tree_node *the_node, void *)
  {
    switch (the_node->kind())
      {
        case TREE_INSTR:
          {
            tree_instr *the_tree_instr = (tree_instr *)the_node;
            the_tree_instr->instr_map(&pass1_on_instr, NULL);
            break;
          }
        case TREE_BLOCK:
          {
            tree_block *the_block = (tree_block *)the_node;
            pass1_on_symtab(the_block->symtab());
            break;
          }
        default:
            break;
      }
  }

static void pass1_on_instr(instruction *the_instr, void *)
  {
    if_ops opcode = the_instr->opcode();
    for (int index = 0; macro_table[index].replacement != NULL; ++index)
      {
        if (macro_table[index].opcode == opcode)
          {
            macro_table[index].used = TRUE;
            break;
          }
      }

    if (!array_exprs)
        pass1_array_copy_on_instr(the_instr);

    if (the_instr->dst_op().is_symbol())
      {
        var_sym *dest_var = the_instr->dst_op().symbol();
        /* two references will exclude it as a choice for substitution */
        pass1_var_ref(dest_var);
        pass1_var_ref(dest_var);
      }

    unsigned num_srcs = the_instr->num_srcs();
    for (unsigned src_num = 0; src_num < num_srcs; ++src_num)
      {
        operand this_src = the_instr->src_op(src_num);
        if (this_src.is_symbol())
          {
            /* two references will exclude it as a choice for substitution */
            pass1_var_ref(this_src.symbol());
            pass1_var_ref(this_src.symbol());
          }
      }

    if (opcode == io_ldc)
      {
        in_ldc *the_ldc = (in_ldc *)the_instr;
        pass1_immed_ref(the_ldc->value());
      }
  }

static void pass1_immed_ref(immed the_immed)
  {
    if (the_immed.is_symbol())
      {
        sym_node *the_sym = the_immed.symbol();
        if (the_sym->is_var())
          {
            var_sym *the_var = (var_sym *)the_sym;
            pass1_var_ref(the_var);

            /*
             *  A second reference will exclude this as a choice for
             *  substitution -- we need to exclude it if the offset is
             *  not zero.
             */
            if (the_immed.offset() != 0)
                pass1_var_ref(the_var);
          }
      }
  }

static void pass1_var_ref(var_sym *the_var)
  {
    if ((!the_var->is_private()) || (!the_var->is_global()) ||
        (!the_var->has_var_def()) || the_var->is_userdef())
      {
        return;
      }

    type_node *var_type = the_var->type()->unqual();
    if (!var_type->is_array())
        return;
    array_type *the_array = (array_type *)var_type;

    if ((the_array->elem_type()->op() != TYPE_INT) ||
        (c_int_type(the_array->elem_type()) != C_char))
      {
        return;
      }

    base_init_struct_list *init_list = read_init_data(the_var->definition());
    ctree *init_tree = build_initializers(the_var->type(), init_list);
    deallocate_init_data(init_list);

    if (init_tree == NULL)
        return;

    if (init_tree->getop() == ctree_strconst)
      {
        annote *the_annote = the_var->annotes()->peek_annote(k_s2c_multi_use);
        if (the_annote == NULL)
          {
            the_annote = the_var->annotes()->peek_annote(k_s2c_one_use);
            if (the_annote == NULL)
              {
                immed_list *new_data = new immed_list;
                new_data->append(immed(init_tree->string_val()));
                the_var->append_annote(k_s2c_one_use, new_data);
              }
            else
              {
                the_annote = the_var->annotes()->get_annote(k_s2c_one_use);
                delete the_annote;
                the_var->append_annote(k_s2c_multi_use);
              }
          }
      }

    delete init_tree;
  }

/*----------------------------------------------------------------------*
    End Private Function Implementations
 *----------------------------------------------------------------------*/
