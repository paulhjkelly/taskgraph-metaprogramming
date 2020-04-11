/* file "positioning.cc" */

/*  Copyright (c) 1995 Stanford University

    All rights reserved.

    This software is provided under the terms described in
    the "suif_copyright.h" include file. */

#include <suif_copyright.h>

/*
 * This is the implementation of variable positioning routines for the
 * SUIF library of miscellaneous useful routines.
 */

#define _MODULE_ "libuseful.a"

#pragma implementation "basic.h"

#define RCS_BASE_FILE positioning_cc

#include "useful_internal.h"
#include <cstring>

RCS_BASE(
    "$Id$")


#define COMBO_NAME_PREFIX "__suif_combo_"


static char *generate_combination_name(void);


extern boolean set_relative_positions(var_sym *var1, var_sym *var2,
                                      i_integer offset,
                                      offset_kind which_offset)
  {
    base_symtab *parent = var1->parent();
    if (parent != var2->parent())
        return FALSE;
    if (var1->is_param() || var2->is_param())
        return FALSE;
    if (var1->is_auto() != var2->is_auto())
        return FALSE;
    if (var1->is_reg() != var2->is_reg())
        return FALSE;
    var_sym *root1 = var1->root_ancestor();
    var_sym *root2 = var2->root_ancestor();
    if (parent->kind() == SYMTAB_GLOBAL)
      {
        if ((!root1->has_var_def()) || (!root2->has_var_def()))
            return FALSE;
      }
    i_integer start_to_start_offset;
    switch (which_offset)
      {
        case OFFSET_START_TO_START:
            start_to_start_offset = offset;
            break;
        case OFFSET_START_TO_END:
            start_to_start_offset = offset - var2->type()->size();
            break;
        case OFFSET_END_TO_START:
            start_to_start_offset = offset + var1->type()->size();
            break;
        case OFFSET_END_TO_END:
            start_to_start_offset =
                    (offset + var1->type()->size()) - var2->type()->size();
            break;
        default:
            assert(FALSE);
      }
    start_to_start_offset =
            (start_to_start_offset + var1->root_offset()) -
            var2->root_offset();
    if (root1 == root2)
        return (start_to_start_offset == 0);
    if (start_to_start_offset < 0)
      {
        start_to_start_offset = -start_to_start_offset;
        var_sym *temp_root = root1;
        root1 = root2;
        root2 = temp_root;
      }
    base_init_struct_list *initialization = NULL;
    if (root1->has_var_def())
      {
        assert(root2->has_var_def());
        var_def *def1 = root1->definition();
        var_def *def2 = root2->definition();
        assert((def1 != NULL) && (def2 != NULL));
        base_init_struct_list *init1 = read_init_data(def1);
        base_init_struct_list *init2 = read_init_data(def2);
        base_init_struct_list *leftovers;
        boolean split_ok =
                split_init_data(init1, start_to_start_offset.c_int(),
                                &initialization, &leftovers);
        if (!split_ok)
          {
            deallocate_init_data(init1);
            deallocate_init_data(init2);
            return FALSE;
          }
        if (!leftovers->is_empty())
          {
            deallocate_init_data(initialization);
            deallocate_init_data(leftovers);
            deallocate_init_data(init2);
            return FALSE;
          }
        deallocate_init_data(leftovers);
        i_integer skip_total = 0;
        base_init_struct_list_iter init_iter(initialization);
        while (!init_iter.is_empty())
          {
            base_init_struct *this_init_struct = init_iter.step();
            skip_total += this_init_struct->total_size();
          }
        assert(skip_total <= start_to_start_offset);
        if (skip_total < start_to_start_offset)
          {
            int fill_size = (start_to_start_offset - skip_total).c_int();
            initialization->append(new fill_init_struct(fill_size, 0));
          }
        initialization->append(init2);
        delete init2;
      }
    i_integer alignment1 = get_alignment(root1->type());
    i_integer alignment2 = get_alignment(root2->type());
    i_integer coeff_n;
    i_integer coeff_m;
    i_integer gcd = ii_gcd(alignment1, alignment2, &coeff_n, &coeff_m);
    i_integer lcm = (alignment1 / gcd) * alignment2;
    if ((start_to_start_offset % gcd) != 0)
      {
        error_line(1, NULL,
                   "set_relative_positions(): requested relative offset "
                   "violates alignment restrictions");
      }
    i_integer first_offset =
            (- alignment1 * coeff_n * (start_to_start_offset / gcd)) % lcm;
    if (first_offset < 0)
        first_offset += lcm;
    i_integer second_offset = first_offset + start_to_start_offset;
    assert(first_offset >= 0);
    assert(second_offset >= 0);
    i_integer end1 = first_offset + root1->type()->size();
    i_integer end2 = second_offset + root2->type()->size();
    i_integer total_size;
    if (end1 >= end2)
        total_size = end1;
    else
        total_size = end2;
    assert(total_size >= 0);
    if (!total_size.is_c_int())
      {
        error_line(1, NULL,
                   "set_relative_positions(): overflow in total size of "
                   "requested combined entity");
      }
    assert(first_offset.is_c_int());
    assert(second_offset.is_c_int());
    char *new_name = generate_combination_name();
    struct_type *new_type =
            new struct_type(TYPE_GROUP, total_size.c_int(), new_name,  2);
    new_type->set_field_name(0, root1->name());
    new_type->set_field_type(0, root1->type());
    new_type->set_offset(0, first_offset.c_int());
    new_type->set_field_name(1, root2->name());
    new_type->set_field_type(1, root2->type());
    new_type->set_offset(1, second_offset.c_int());
    new_type = (struct_type *)(parent->install_type(new_type));

    var_sym *combined_var = parent->new_var(new_type, new_name);

    if (root1->has_var_def())
      {
        assert(root2->has_var_def());
        var_def *def1 = root1->definition();
        var_def *def2 = root2->definition();
        assert((def1 != NULL) && (def2 != NULL));
        if (!lcm.is_c_int())
          {
            error_line(1, NULL,
                       "set_relative_positions(): overflow in alignment of "
                       "requested combined entity");
          }
        var_def *new_def =
                def1->parent()->define_var(combined_var, lcm.c_int());
        def1->parent()->remove_def(def1);
        def2->parent()->remove_def(def2);
        delete def1;
        delete def2;
        if (initialization != NULL)
          {
            if (first_offset > 0)
              {
                int fill_size = first_offset.c_int();
                initialization->push(new fill_init_struct(fill_size, 0));
              }
            write_init_data(new_def, initialization);
            deallocate_init_data(initialization);
          }
      }

    if (root1->is_addr_taken() || root2->is_addr_taken())
        combined_var->set_addr_taken();
    else
        combined_var->reset_addr_taken();
    combined_var->add_child(root1, first_offset.c_int());
    combined_var->add_child(root2, second_offset.c_int());
    return TRUE;
  }


static char *generate_combination_name(void)
  {
    static i_integer *counter = NULL;
    static char *buffer = NULL;
    static unsigned long buffer_size = 0;

    if (buffer == NULL)
      {
        buffer_size = (strlen(COMBO_NAME_PREFIX) + 20);
        buffer = new char[buffer_size];
        strcpy(buffer, COMBO_NAME_PREFIX);
      }
    if (counter == NULL)
        counter = new i_integer(0);
    if (counter->written_length() + strlen(COMBO_NAME_PREFIX) + 1 >
        buffer_size)
      {
        buffer_size *= 2;
        delete[] buffer;
        buffer = new char[buffer_size];
        strcpy(buffer, COMBO_NAME_PREFIX);
      }
    counter->write(&(buffer[strlen(COMBO_NAME_PREFIX)]));
    ++(*counter);
    return buffer;
  }
