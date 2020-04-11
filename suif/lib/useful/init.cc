/* file "init.cc" */

/*  Copyright (c) 1994 Stanford University

    All rights reserved.

    This software is provided under the terms described in
    the "suif_copyright.h" include file. */

#include <suif_copyright.h>

/*
 * This is the implementation of the routines dealing with
 * initialization annotations that go on vardefs.
 */

#define _MODULE_ "libuseful.a"

#pragma implementation "basic.h"

#define RCS_BASE_FILE libuseful_init_cc

#include "useful_internal.h"

RCS_BASE(
    "$Id$")

/*----------------------------------------------------------------------*
    Begin Documentation
 *----------------------------------------------------------------------*

                Summary
                -------

        This file is a place to put general routines to deal with the
        initialization annotations that go on vardefs.


 *----------------------------------------------------------------------*
    End Documentation
 *----------------------------------------------------------------------*/

/*
 *  Return the contents of a k_multi_init, k_repeat_init, or
 *  k_fill_init annote, respectively, in a convenient form.  The
 *  original data list is unchanged.
 */
static multi_init_struct translate_multi_init_annote(immed_list *data);
static repeat_init_struct translate_repeat_init_annote(immed_list *data);
static fill_init_struct translate_fill_init_annote(immed_list *data);
static boolean int_repetitions_at_offset_is_ok(type_node *the_type,
                                              int int_size, int repetitions,
                                              int offset);
static boolean float_repetitions_at_offset_is_ok(type_node *the_type,
                                                int float_size,
                                                int repetitions, int offset);
static int num_int_repetitions_at_offset(type_node *the_type, int int_size,
                                         int offset);
static int num_float_repetitions_at_offset(type_node *the_type, int float_size,
                                           int offset);

extern base_init_struct_list *read_init_data(var_def *the_def)
  {
    base_init_struct_list *new_list = new base_init_struct_list;

    annote_list_iter the_iter(the_def->annotes());
    while (!the_iter.is_empty())
      {
        annote *this_annote = the_iter.step();
        if (this_annote->name() == k_multi_init)
          {
            multi_init_struct *new_init_struct = new multi_init_struct;
            *new_init_struct =
                    translate_multi_init_annote(this_annote->immeds());
            new_list->append(new_init_struct);
          }
        else if (this_annote->name() == k_repeat_init)
          {
            repeat_init_struct *new_init_struct = new repeat_init_struct;
            *new_init_struct =
                    translate_repeat_init_annote(this_annote->immeds());
            new_list->append(new_init_struct);
          }
        else if (this_annote->name() == k_fill)
          {
            fill_init_struct *new_init_struct = new fill_init_struct;
            *new_init_struct =
                    translate_fill_init_annote(this_annote->immeds());
            new_list->append(new_init_struct);
          }
      }

    return new_list;
  }

extern void deallocate_init_data(base_init_struct_list *initializers)
  {
    while (!initializers->is_empty())
      {
        base_init_struct *this_init_struct = initializers->pop();
        if (this_init_struct->the_multi_init() != NULL)
            delete this_init_struct->the_multi_init()->data;
        delete this_init_struct;
      }
    delete initializers;
  }

extern void write_init_data(var_def *the_def, base_init_struct_list *data)
  {
    base_init_struct_list_iter data_iter(data);
    while (!data_iter.is_empty())
      {
        base_init_struct *this_init_struct = data_iter.step();
        if (this_init_struct->the_multi_init() != NULL)
          {
            multi_init_struct *the_multi_init =
                    this_init_struct->the_multi_init();

            immed_list *new_data = new immed_list;
            new_data->append(immed(the_multi_init->size));

            immed_list_iter the_immed_iter(the_multi_init->data);
            while (!the_immed_iter.is_empty())
              {
                immed this_immed = the_immed_iter.step();
                new_data->append(this_immed);
              }
            the_def->append_annote(k_multi_init, new_data);
          }
        else if (this_init_struct->the_repeat_init() != NULL)
          {
            repeat_init_struct *the_repeat_init =
                    this_init_struct->the_repeat_init();

            immed_list *new_data = new immed_list;
            new_data->append(immed(the_repeat_init->repetitions));
            new_data->append(immed(the_repeat_init->size));
            new_data->append(the_repeat_init->data);
            the_def->append_annote(k_repeat_init, new_data);
          }
        else if (this_init_struct->the_fill_init() != NULL)
          {
            fill_init_struct *the_fill_init =
                    this_init_struct->the_fill_init();

            immed_list *new_data = new immed_list;
            new_data->append(immed(the_fill_init->size));
            new_data->append(immed(the_fill_init->data));
            the_def->append_annote(k_fill, new_data);
          }
        else
          {
            assert(FALSE);
          }
      }
  }

extern boolean split_init_data(base_init_struct_list *original, int offset,
                               base_init_struct_list **chunk1,
                               base_init_struct_list **chunk2)
  {
    *chunk1 = new base_init_struct_list();

    int remaining_offset = offset;
    while (!original->is_empty())
      {
        base_init_struct *this_bis = original->head()->contents;
        int this_size = this_bis->total_size();
        if (this_size > remaining_offset)
            break;

        remaining_offset -= this_size;
        this_bis = original->pop();
        (*chunk1)->append(this_bis);
      }

    if (original->is_empty() || (remaining_offset == 0))
      {
        *chunk2 = original;
        return TRUE;
      }

    base_init_struct *this_bis = original->head()->contents;

    base_init_struct *new_init_struct = NULL;
    if (this_bis->the_multi_init() != NULL)
      {
        multi_init_struct *old_multi = this_bis->the_multi_init();
        if (remaining_offset % old_multi->size == 0)
          {
            remaining_offset /= old_multi->size;
            immed_list *new_list = new immed_list;
            while (remaining_offset > 0)
              {
                new_list->append(old_multi->data->pop());
                --remaining_offset;
              }
            new_init_struct = new multi_init_struct(old_multi->size, new_list);
          }
      }
    else if (this_bis->the_repeat_init() != NULL)
      {
        repeat_init_struct *old_repeat = this_bis->the_repeat_init();
        if (remaining_offset % old_repeat->size == 0)
          {
            remaining_offset /= old_repeat->size;
            new_init_struct =
                    new repeat_init_struct(remaining_offset, old_repeat->size,
                                           old_repeat->data);
            old_repeat->repetitions -= remaining_offset;
          }
      }
    else if (this_bis->the_fill_init() != NULL)
      {
        fill_init_struct *old_fill = this_bis->the_fill_init();
        new_init_struct =
                new fill_init_struct(remaining_offset, old_fill->data);
        old_fill->size -= remaining_offset;
      }
    else
      {
        assert(FALSE);
      }

    if (new_init_struct != NULL)
      {
        (*chunk1)->append(new_init_struct);
        *chunk2 = original;
        return TRUE;
      }
    else
      {
        original->push(*chunk1);
        delete *chunk1;
        *chunk1 = NULL;
        *chunk2 = NULL;
        return FALSE;
      }
  }

extern char *string_from_init_data(var_def *the_def)
  {
    base_init_struct_list *init_data = read_init_data(the_def);
    if (init_data == NULL)
        return NULL;
    if (init_data->is_empty())
      {
        deallocate_init_data(init_data);
        return NULL;
      }
    base_init_struct *head_init_struct = init_data->head()->contents;
    multi_init_struct *the_multi_init = head_init_struct->the_multi_init();
    if (the_multi_init == NULL)
      {
        deallocate_init_data(init_data);
        return NULL;
      }
    if (the_multi_init->size != target.size[C_char])
      {
        deallocate_init_data(init_data);
        return NULL;
      }
    immed_list *data = the_multi_init->data;
    assert(data != NULL);
    char *result = new char[data->count()];
    char *current_position = result;
    immed_list_iter data_iter(data);
    while (!data_iter.is_empty())
      {
        immed data_item = data_iter.step();
        if (!data_item.is_int_const())
          {
            deallocate_init_data(init_data);
            delete[] result;
            return NULL;
          }
        i_integer data_ii = immed_to_ii(data_item);
        if (!data_ii.is_c_char())
          {
            deallocate_init_data(init_data);
            delete[] result;
            return NULL;
          }
        char this_char = data_ii.c_char();
        *current_position = this_char;
        if (this_char == 0)
          {
            deallocate_init_data(init_data);
            return result;
          }
        ++current_position;
      }
    deallocate_init_data(init_data);
    delete[] result;
    return NULL;
  }

extern boolean type_is_compatible_with_initializations(type_node *the_type,
                                                       var_def *the_def)
  {
    base_init_struct_list *init_data = read_init_data(the_def);
    int offset = 0;
    while (!init_data->is_empty())
      {
        base_init_struct *this_init = init_data->pop();
        if (offset + this_init->total_size() > the_type->size())
            return FALSE;
        if (this_init->the_multi_init() != NULL)
          {
            multi_init_struct *the_multi = this_init->the_multi_init();
            if (the_multi->data->is_empty())
                continue;
            int size = the_multi->size;
            int repetitions = the_multi->data->count();
            switch (the_multi->data->head()->contents.kind())
              {
                case im_int:
                case im_extended_int:
                case im_symbol:
                    if (!int_repetitions_at_offset_is_ok(the_type, size,
                                                         repetitions, offset))
                      {
                        return FALSE;
                      }
                    break;
                case im_float:
                case im_extended_float:
                    if (!float_repetitions_at_offset_is_ok(the_type, size,
                                                           repetitions,
                                                           offset))
                      {
                        return FALSE;
                      }
                    break;
                default:
                    return FALSE;
              }
          }
        else if (this_init->the_repeat_init() != NULL)
          {
            repeat_init_struct *the_repeat = this_init->the_repeat_init();
            int size = the_repeat->size;
            int repetitions = the_repeat->repetitions;
            if (repetitions == 0)
                continue;
            switch (the_repeat->data.kind())
              {
                case im_int:
                case im_extended_int:
                case im_symbol:
                    if (!int_repetitions_at_offset_is_ok(the_type, size,
                                                         repetitions, offset))
                      {
                        return FALSE;
                      }
                    break;
                case im_float:
                case im_extended_float:
                    if (!float_repetitions_at_offset_is_ok(the_type, size,
                                                           repetitions,
                                                           offset))
                      {
                        return FALSE;
                      }
                    break;
                default:
                    return FALSE;
              }
          }
        else
          {
            fill_init_struct *the_fill = this_init->the_fill_init();
            assert(the_fill != NULL);
            if (the_fill->data != 0)
                return FALSE;
          }
        offset += this_init->total_size();
      }
    delete init_data;
    return TRUE;
  }

extern void fix_group_type_for_initializations(struct_type *the_group,
                                               var_def *the_def)
  {
    base_init_struct_list *init_data = read_init_data(the_def);
    int offset = 0;
    while (!init_data->is_empty())
      {
        base_init_struct *this_init = init_data->pop();
        if (offset + this_init->total_size() > the_group->size())
            the_group->set_size(offset + this_init->total_size());
        if (this_init->the_multi_init() != NULL)
          {
            multi_init_struct *the_multi = this_init->the_multi_init();
            if (the_multi->data->is_empty())
                continue;
            int size = the_multi->size;
            int repetitions = the_multi->data->count();
            switch (the_multi->data->head()->contents.kind())
              {
                case im_int:
                case im_extended_int:
                case im_symbol:
                    if (!int_repetitions_at_offset_is_ok(the_group, size,
                                                         repetitions, offset))
                      {
                        type_node *field_type =
                                new base_type(TYPE_INT, size, TRUE);
                        if (repetitions > 1)
                          {
                            field_type =
                                    new array_type(field_type, 0,
                                                   repetitions - 1);
                          }
                        field_type =
                                fileset->globals()->install_type(field_type);
                        unsigned field_num = the_group->num_fields();
                        the_group->set_num_fields(field_num + 1);
                        the_group->set_field_name(field_num, "_init");
                        the_group->set_field_type(field_num, field_type);
                        the_group->set_offset(field_num, offset);
                      }
                    break;
                case im_float:
                case im_extended_float:
                    if (!float_repetitions_at_offset_is_ok(the_group, size,
                                                           repetitions,
                                                           offset))
                      {
                        type_node *field_type =
                                new base_type(TYPE_FLOAT, size);
                        if (repetitions > 1)
                          {
                            field_type =
                                    new array_type(field_type, 0,
                                                   repetitions - 1);
                          }
                        field_type =
                                fileset->globals()->install_type(field_type);
                        unsigned field_num = the_group->num_fields();
                        the_group->set_num_fields(field_num + 1);
                        the_group->set_field_name(field_num, "_init");
                        the_group->set_field_type(field_num, field_type);
                        the_group->set_offset(field_num, offset);
                      }
                    break;
                default:
                    assert(FALSE);
              }
          }
        else if (this_init->the_repeat_init() != NULL)
          {
            repeat_init_struct *the_repeat = this_init->the_repeat_init();
            int size = the_repeat->size;
            int repetitions = the_repeat->repetitions;
            if (repetitions == 0)
                continue;
            switch (the_repeat->data.kind())
              {
                case im_int:
                case im_extended_int:
                case im_symbol:
                    if (!int_repetitions_at_offset_is_ok(the_group, size,
                                                         repetitions, offset))
                      {
                        type_node *field_type =
                                new base_type(TYPE_INT, size, TRUE);
                        if (repetitions > 1)
                          {
                            field_type =
                                    new array_type(field_type, 0,
                                                   repetitions - 1);
                          }
                        field_type =
                                fileset->globals()->install_type(field_type);
                        unsigned field_num = the_group->num_fields();
                        the_group->set_num_fields(field_num + 1);
                        the_group->set_field_name(field_num, "_init");
                        the_group->set_field_type(field_num, field_type);
                        the_group->set_offset(field_num, offset);
                      }
                    break;
                case im_float:
                case im_extended_float:
                    if (!float_repetitions_at_offset_is_ok(the_group, size,
                                                           repetitions,
                                                           offset))
                      {
                        type_node *field_type =
                                new base_type(TYPE_FLOAT, size);
                        if (repetitions > 1)
                          {
                            field_type =
                                    new array_type(field_type, 0,
                                                   repetitions - 1);
                          }
                        field_type =
                                fileset->globals()->install_type(field_type);
                        unsigned field_num = the_group->num_fields();
                        the_group->set_num_fields(field_num + 1);
                        the_group->set_field_name(field_num, "_init");
                        the_group->set_field_type(field_num, field_type);
                        the_group->set_offset(field_num, offset);
                      }
                    break;
                default:
                    assert(FALSE);
              }
          }
        else
          {
            fill_init_struct *the_fill = this_init->the_fill_init();
            assert(the_fill != NULL);
            assert(the_fill->data == 0);
          }
        offset += this_init->total_size();
      }
    delete init_data;
  }


static multi_init_struct translate_multi_init_annote(immed_list *data)
  {
    if ((data->count() < 1) || (!((*data)[0].is_integer())))
        error_line(1, NULL, "bad format for \"%s\" annote", k_multi_init);

    immed_list_iter data_iter(data);

    immed first = data_iter.step();
    int size = first.integer();

    immed_list *new_data = new immed_list;
    while (!data_iter.is_empty())
      {
        immed this_data = data_iter.step();
        new_data->append(this_data);
      }

    return multi_init_struct(size, new_data);
  }

static repeat_init_struct translate_repeat_init_annote(immed_list *data)
  {
    if ((data->count() != 3) || (!((*data)[0].is_integer())) ||
        (!((*data)[1].is_integer())))
      {
        error_line(1, NULL, "bad format for \"%s\" annote", k_repeat_init);
      }

    int repetitions = (*data)[0].integer();
    int size = (*data)[1].integer();
    immed new_data = (*data)[2];

    return repeat_init_struct(repetitions, size, new_data);
  }

static fill_init_struct translate_fill_init_annote(immed_list *data)
  {
    if ((data->count() != 2) || (!((*data)[0].is_integer())) ||
        (!((*data)[1].is_integer())))
      {
        error_line(1, NULL, "bad format for \"%s\" annote", k_fill);
      }

    int size = (*data)[0].integer();
    int new_data = (*data)[1].integer();

    return fill_init_struct(size, new_data);
  }

static boolean int_repetitions_at_offset_is_ok(type_node *the_type,
                                               int int_size, int repetitions,
                                               int offset)
  {
    int repetitions_remaining = repetitions;
    int offset_remaining = offset;
    while (repetitions_remaining > 0)
      {
        int chunk_size =
                num_int_repetitions_at_offset(the_type, int_size,
                                              offset_remaining);
        if (chunk_size == 0)
            return FALSE;
        if (chunk_size >= repetitions_remaining)
            return TRUE;
        repetitions_remaining -= chunk_size;
        offset_remaining += (chunk_size * int_size);
      }
    return TRUE;
  }

static boolean float_repetitions_at_offset_is_ok(type_node *the_type,
                                                 int float_size,
                                                 int repetitions, int offset)
  {
    int repetitions_remaining = repetitions;
    int offset_remaining = offset;
    while (repetitions_remaining > 0)
      {
        int chunk_size =
                num_float_repetitions_at_offset(the_type, float_size,
                                                offset_remaining);
        if (chunk_size == 0)
            return FALSE;
        if (chunk_size >= repetitions_remaining)
            return TRUE;
        repetitions_remaining -= chunk_size;
        offset_remaining += (chunk_size * float_size);
      }
    return TRUE;
  }

static int num_int_repetitions_at_offset(type_node *the_type, int int_size,
                                         int offset)
  {
    if (the_type->size() < offset + int_size)
        return 0;
    type_node *unqual = the_type->unqual();
    switch (unqual->op())
      {
        case TYPE_INT:
            if ((offset == 0) && (unqual->size() == int_size))
                return 1;
            else
                return 0;
        case TYPE_PTR:
            if ((offset == 0) && (unqual->size() == int_size))
                return 1;
            else
                return 0;
        case TYPE_ARRAY:
          {
            array_type *the_array = (array_type *)unqual;
            if (the_array->size() == 0)
                return 0;
            type_node *elem_type = the_array->elem_type();
            if (elem_type->size() == 0)
                return 0;
            int elem_offset = offset % (elem_type->size());
            int elem_result =
                    num_int_repetitions_at_offset(elem_type, int_size,
                                                  elem_offset);
            if (elem_result == 0)
                return 0;
            if ((elem_offset != 0) ||
                (elem_result * int_size != elem_type->size()))
              {
                return elem_result;
              }
            return elem_result *
                   ((the_array->size() / elem_type->size()) -
                    (offset / elem_type->size()));
          }
        case TYPE_GROUP:
        case TYPE_STRUCT:
        case TYPE_UNION:
          {
            struct_type *the_struct = (struct_type *)unqual;
            unsigned num_fields = the_struct->num_fields();
            for (unsigned field_num = 0; field_num < num_fields; ++field_num)
              {
                type_node *field_type = the_struct->field_type(field_num);
                int field_offset = the_struct->offset(field_num);
                if (field_offset <= offset)
                  {
                    int field_result =
                            num_int_repetitions_at_offset(field_type, int_size,
                                    offset - field_offset);
                    if (field_result > 0)
                      {
                        return field_result +
                               num_int_repetitions_at_offset(the_struct,
                                       int_size,
                                       offset + (field_result * int_size));
                      }
                  }
              }
            return 0;
          }
        case TYPE_ENUM:
            if ((offset == 0) && (unqual->size() == int_size))
                return 1;
            else
                return 0;
        default:
            return 0;
      }
  }

static int num_float_repetitions_at_offset(type_node *the_type, int float_size,
                                           int offset)
  {
    type_node *unqual = the_type->unqual();
    switch (unqual->op())
      {
        case TYPE_FLOAT:
            if ((offset == 0) && (unqual->size() == float_size))
                return 1;
            else
                return 0;
        case TYPE_ARRAY:
          {
            array_type *the_array = (array_type *)unqual;
            if (the_array->size() == 0)
                return 0;
            type_node *elem_type = the_array->elem_type();
            if (elem_type->size() == 0)
                return 0;
            int elem_offset = offset % (elem_type->size());
            int elem_result =
                    num_float_repetitions_at_offset(elem_type, float_size,
                                                    elem_offset);
            if (elem_result == 0)
                return 0;
            if ((elem_offset != 0) ||
                (elem_result * float_size != elem_type->size()))
              {
                return elem_result;
              }
            return elem_result *
                   ((the_array->size() / elem_type->size()) -
                    (offset / elem_type->size()));
          }
        case TYPE_GROUP:
        case TYPE_STRUCT:
        case TYPE_UNION:
          {
            struct_type *the_struct = (struct_type *)unqual;
            unsigned num_fields = the_struct->num_fields();
            for (unsigned field_num = 0; field_num < num_fields; ++field_num)
              {
                type_node *field_type = the_struct->field_type(field_num);
                int field_offset = the_struct->offset(field_num);
                if (field_offset <= offset)
                  {
                    int field_result =
                            num_float_repetitions_at_offset(field_type,
                                    float_size, offset - field_offset);
                    if (field_result > 0)
                      {
                        return field_result +
                               num_float_repetitions_at_offset(the_struct,
                                       float_size,
                                       offset + (field_result * float_size));
                      }
                  }
              }
            return 0;
          }
        default:
            return 0;
      }
  }
