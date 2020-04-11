/* file "no_sub_vars.cc" */

/*  Copyright (c) 1994 Stanford University

    All rights reserved.

    This software is provided under the terms described in
    the "suif_copyright.h" include file. */

#include <suif_copyright.h>

/*
 *  This file implements routines to remove sub-variables for the SUIF
 *  library of miscellaneous useful routines.
 */

#define _MODULE_ "libuseful.a"

#define RCS_BASE_FILE useful_no_sub_vars_cc

#include "useful_internal.h"
#include <cstring>

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

static sym_addr root_symaddr(sym_addr orig_sym_addr,
                             immed_list **field_immeds);
static void delete_children_and_symbol(var_sym *the_var);

/*----------------------------------------------------------------------*
    End Private Function Declarations
 *----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*
    Begin Public Function Implementations
 *----------------------------------------------------------------------*/

extern void symtab_no_sub_vars(base_symtab *the_symtab)
  {
    no_annote_sub_vars(the_symtab);

    sym_node_list_iter sym_iter(the_symtab->symbols());
    while (!sym_iter.is_empty())
      {
        sym_node *this_sym = sym_iter.step();
        no_annote_sub_vars(this_sym);
      }

    var_def_list_iter def_iter(the_symtab->var_defs());
    while (!def_iter.is_empty())
      {
        var_def *this_def = def_iter.step();
        no_annote_sub_vars(this_def);
      }

    type_node_list_iter type_iter(the_symtab->types());
    while (!type_iter.is_empty())
      {
        type_node *this_type = type_iter.step();
        no_annote_sub_vars(this_type);
        if (this_type->is_array())
          {
            array_type *this_array = (array_type *)this_type;

            if (this_array->lower_bound().is_variable())
              {
                if (this_array->lower_bound().variable()->parent_var() != NULL)
                  {
                    error_line(1, NULL, "sub-variable `%s' in array bound",
                               this_array->lower_bound().variable()->name());
                  }
              }

            if (this_array->upper_bound().is_variable())
              {
                if (this_array->upper_bound().variable()->parent_var() != NULL)
                  {
                    error_line(1, NULL, "sub-variable `%s' in array bound",
                               this_array->upper_bound().variable()->name());
                  }
              }
          }
      }

    sym_node_list_iter final_iter(the_symtab->symbols());
    while (!final_iter.is_empty())
      {
        sym_node *this_sym = final_iter.step();

        if (!this_sym->is_var())
            continue;
        var_sym *this_var = (var_sym *)this_sym;

        if (this_var->parent_var() != NULL)
          {
            this_var->parent_var()->remove_child(this_var);
            this_var->remove_from_table();
            delete_children_and_symbol(this_var);
          }
      }
  }

extern void no_annote_sub_vars(suif_object *the_object)
  {
    annote_list_iter annote_iter(the_object->annotes());
    while (!annote_iter.is_empty())
      {
        annote *this_annote = annote_iter.step();
        immed_list *the_immeds = this_annote->immeds();
        if (the_immeds != NULL)
          {
            immed_list_e *follow = the_immeds->head();
            while (follow != NULL)
              {
                if (follow->contents.is_symbol())
                  {
                    follow->contents =
                            immed(root_symaddr(follow->contents.addr(), NULL));
                  }
                else if (follow->contents.is_op())
                  {
                    operand this_op = follow->contents.op();
                    if (this_op.is_instr())
                        instr_no_sub_vars(this_op.instr());
                  }
                else if (follow->contents.is_instr())
                  {
                    instr_no_sub_vars(follow->contents.instr());
                  }
                follow = follow->next();
              }
          }
      }
  }

extern void instr_no_sub_vars(instruction *the_instr)
  {
    no_annote_sub_vars(the_instr);

    if (the_instr->opcode() == io_ldc)
      {
        in_ldc *the_ldc = (in_ldc *)the_instr;
        if (the_ldc->value().is_symbol())
          {
            sym_addr old_addr = the_ldc->value().addr();
            if (old_addr.symbol()->is_var())
              {
                var_sym *old_var = (var_sym *)(old_addr.symbol());
                if (old_var->parent_var() != NULL)
                  {
                    immed_list *field_immeds =
                            (immed_list *)(the_instr->get_annote(k_fields));
                    sym_addr new_addr = root_symaddr(old_addr, &field_immeds);
                    the_ldc->set_value(immed(new_addr));
                    if (field_immeds != NULL)
                        the_instr->append_annote(k_fields, field_immeds);
                  }
              }
          }
      }

    unsigned num_srcs = the_instr->num_srcs();
    for (unsigned src_num = 0; src_num < num_srcs; ++src_num)
      {
        operand this_op = the_instr->src_op(src_num);
        if (this_op.is_symbol())
          {
            var_sym *this_var = this_op.symbol();
            if (this_var->parent_var() != NULL)
              {
                immed_list *field_immeds = NULL;
                sym_addr new_addr =
                        root_symaddr(sym_addr(this_var, 0), &field_immeds);
                in_ldc *new_ldc =
                        new in_ldc(this_var->type()->ptr_to(), operand(),
                                   immed(new_addr));
                if (field_immeds != NULL)
                    new_ldc->append_annote(k_fields, field_immeds);
                in_rrr *new_load =
                        new in_rrr(io_lod, this_var->type(), operand(),
                                   operand(new_ldc));
                the_instr->set_src_op(src_num, operand(new_load));
              }
          }
        else if (this_op.is_expr())
          {
            instr_no_sub_vars(this_op.instr());
          }
      }

    operand destination = the_instr->dst_op();
    if (destination.is_symbol())
      {
        var_sym *dest_var = destination.symbol();
        if (dest_var->parent_var() != NULL)
          {
            immed_list *field_immeds = NULL;
            sym_addr new_addr =
                    root_symaddr(sym_addr(dest_var, 0), &field_immeds);
            in_ldc *new_ldc =
                    new in_ldc(dest_var->type()->ptr_to(), operand(),
                               immed(new_addr));
            if (field_immeds != NULL)
                new_ldc->append_annote(k_fields, field_immeds);
            in_rrr *new_store =
                    new in_rrr(io_str, type_void, operand(), operand(new_ldc));
            the_instr->set_dst(operand());
            replace_instruction(the_instr, new_store);
            new_store->set_src2(operand(the_instr));
          }
      }
  }

/*----------------------------------------------------------------------*
    End Public Function Implementations
 *----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*
    Begin Private Function Implementations
 *----------------------------------------------------------------------*/

static sym_addr root_symaddr(sym_addr orig_sym_addr,
                             immed_list **field_immeds)
  {
    if (!orig_sym_addr.symbol()->is_var())
        return orig_sym_addr;
    var_sym *orig_var = (var_sym *)(orig_sym_addr.symbol());

    var_sym *parent_var = orig_var->parent_var();
    if (parent_var == NULL)
        return orig_sym_addr;

    if (field_immeds != NULL)
      {
        type_node *parent_type = parent_var->type()->unqual();
        boolean add_field = FALSE;
        if (parent_type->is_struct())
          {
            struct_type *parent_struct = (struct_type *)parent_type;
            unsigned num_fields = parent_struct->num_fields();
            for (unsigned field_num = 0; field_num < num_fields; ++field_num)
              {
                if ((parent_struct->field_type(field_num) ==
                     orig_var->type()) &&
                    (strcmp(parent_struct->field_name(field_num),
                            orig_var->name()) == 0) &&
                    (parent_struct->offset(field_num) == orig_var->offset()))
                  {
                    add_field = TRUE;
                    break;
                  }
              }
          }

        if (add_field)
          {
            if (*field_immeds == NULL)
                *field_immeds = new immed_list;
            (*field_immeds)->push(orig_var->name());
          }
        else
          {
            if (*field_immeds != NULL)
              {
                delete *field_immeds;
                *field_immeds = NULL;
              }
          }
      }

    return root_symaddr(sym_addr(parent_var,
                                 orig_var->offset() + orig_sym_addr.offset()),
                        field_immeds);
  }

static void delete_children_and_symbol(var_sym *the_var)
  {
    if (the_var->parent_var() != NULL)
        the_var->parent_var()->remove_child(the_var);

    int num_children = the_var->num_children();
    for (int child_num = num_children - 1; child_num >= 0; --child_num)
        delete_children_and_symbol(the_var->child_var(child_num));
    delete the_var;
  }

/*----------------------------------------------------------------------*
    End Private Function Implementations
 *----------------------------------------------------------------------*/
