/* file "symbol.cc" */

/*  Copyright (c) 1994 Stanford University

    All rights reserved.

    This software is provided under the terms described in
    the "suif_copyright.h" include file. */

#include <suif_copyright.h>

/*
 *  This file implements miscellaneous symbol routines for the SUIF
 *  library of miscellaneous useful routines.
 */

#define _MODULE_ "libuseful.a"

#pragma implementation "basic.h"

#define RCS_BASE_FILE symbol_cc

#include "useful_internal.h"

RCS_BASE(
    "$Id$")


extern void globalize(var_sym *the_var, glob_type the_glob_type)
  {
    var_sym *moving_var = the_var->root_ancestor();
    assert(!moving_var->is_param());
    base_symtab *var_symtab = moving_var->parent();

    base_symtab *first_global_symtab = var_symtab;
    while (!first_global_symtab->is_global())
        first_global_symtab = first_global_symtab->parent();

    base_symtab *new_symtab = first_global_symtab;
    if ((the_glob_type == INTER_FILE) && new_symtab->is_file())
        new_symtab = new_symtab->parent();
    if (new_symtab == var_symtab)
        return;

    if (!moving_var->has_var_def())
        var_symtab->define_var(moving_var, get_alignment(moving_var->type()));

    /*
     * At this point we know the new symtab is higher than the
     * original symtab; hence the original symtab couldn't have
     * been the inter-file global symtab, so first_global_symtab
     * should point to a file symtab.
     */
    assert(first_global_symtab->is_file());
    var_def *the_def = moving_var->definition();
    assert(the_def != NULL);
    the_def->parent()->remove_def(the_def);
    first_global_symtab->add_def(the_def);

    moving_var->remove_from_table();
    moving_var->add_to_table(new_symtab);

    global_symtab *new_globals = (global_symtab *)new_symtab;
    new_globals->number_globals();
  }

extern boolean is_common(var_sym *the_var)
  {
    return (the_var->annotes()->peek_annote(k_common_block) != NULL);
  }

extern base_symtab *joint_symtab(base_symtab *symtab_1, base_symtab *symtab_2)
  {
    assert((symtab_1 != NULL) && (symtab_2 != NULL));

    base_symtab *follow = symtab_1;
    while (follow != NULL)
      {
        if (follow == symtab_2)
            return symtab_1;
        follow = follow->parent();
      }

    follow = symtab_2;
    while (follow != NULL)
      {
        if (follow == symtab_1)
            return symtab_2;
        follow = follow->parent();
      }

    return NULL;
  }

extern base_symtab *common_symtab(base_symtab *symtab_1,
                                  base_symtab *symtab_2)
  {
    assert((symtab_1 != NULL) && (symtab_2 != NULL));

    base_symtab *follow = symtab_1;
    while (follow != NULL)
      {
        if (symtab_2->is_ancestor(follow))
            return follow;
        follow = follow->parent();
      }

    return NULL;
  }

extern immed_list *children_touching_region(var_sym *base_var, int offset,
                                            int length)
  {
    immed_list *result_list = new immed_list();
    unsigned num_children = base_var->num_children();
    for (unsigned child_num = 0; child_num < num_children; ++child_num)
      {
        var_sym *this_child = base_var->child_var(child_num);
        int this_offset = this_child->offset();
        int this_size = this_child->type()->size();
        if ((this_offset < offset + length) &&
            (offset < this_offset + this_size))
          {
            result_list->append(immed(this_child));
          }
      }
    return result_list;
  }

extern type_node *addr_type(sym_node *the_sym)
  {
    switch (the_sym->kind())
      {
        case SYM_PROC:
          {
            proc_sym *the_proc = (proc_sym *)the_sym;
            return the_proc->type()->ptr_to();
          }
        case SYM_LABEL:
            return type_void->ptr_to();
        case SYM_VAR:
          {
            var_sym *the_var = (var_sym *)the_sym;
            the_var->set_addr_taken();
            return the_var->type()->ptr_to();
          }
        default:
            assert(FALSE);
            return NULL;
      }
  }
