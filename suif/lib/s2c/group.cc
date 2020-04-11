/* file "group.cc" */

/*  Copyright (c) 1994 Stanford University

    All rights reserved.

    This software is provided under the terms described in
    the "suif_copyright.h" include file. */

#include <suif_copyright.h>

/*
 *  This file contains the implementation of routines to handle group
 *  types in s2c, the SUIF to C converter.
 */

#define RCS_BASE_FILE group_cc

#include "s2c.h"

RCS_BASE("$Id$")

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

class group_layout
  {
public:
    struct_type *the_group;
    int num_panes;
    int *pane_for_field;

    void print_fields(io_class *out, int nindent);
    struct_type *struct_for_pane(int pane_num);
  };

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

static const char *k_s2c_group_translation;

/*----------------------------------------------------------------------*
    End Private Global Variables
 *----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*
    Begin Private Function Declarations
 *----------------------------------------------------------------------*/

static void layout_one_group(struct_type *the_struct);
static void print_with_fields(struct_type *the_struct, io_class *out,
                              int nindent, boolean with_name);
static void ctree_indent(io_class *out, int amount);
static void fill_field_space(io_class *out, int amount, int nindent,
        boolean indent_before, boolean name_needed, int start_offset);
static void free_group_layout(void *data);

/*----------------------------------------------------------------------*
    End Private Function Declarations
 *----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*
    Begin Public Function Implementations
 *----------------------------------------------------------------------*/

extern void init_group_layout(void)
  {
    STRUCT_ANNOTE(k_s2c_group_translation, "s2c group translation", FALSE,
                  NULL, NULL, &free_group_layout, NULL);
  }

extern void layout_groups_for_symtab(base_symtab *the_symtab)
  {
    type_node_list_iter type_iter(the_symtab->types());
    while (!type_iter.is_empty())
      {
        type_node *this_type = type_iter.step();
        if (this_type->op() == TYPE_GROUP)
            layout_one_group((struct_type *)this_type);
      }
  }

extern void print_struct_with_fields(struct_type *the_struct, io_class *out,
                                     int nindent)
  {
    switch (the_struct->op())
      {
        case TYPE_STRUCT:
        case TYPE_UNION:
            print_with_fields(the_struct, out, nindent, TRUE);
            return;
        case TYPE_GROUP:
          {
            group_layout *the_layout =
                    (group_layout *)(the_struct->peek_annote(
                                             k_s2c_group_translation));
            assert(the_layout != NULL);
            the_layout->print_fields(out, nindent);
            return;
          }
        default:
            assert(FALSE);
      }
  }

extern boolean struct_is_union(struct_type *the_struct)
  {
    switch (the_struct->op())
      {
        case TYPE_STRUCT:
            return FALSE;
        case TYPE_UNION:
            return TRUE;
        case TYPE_GROUP:
          {
            group_layout *the_layout =
                    (group_layout *)(the_struct->peek_annote(
                                             k_s2c_group_translation));
            assert(the_layout != NULL);
            if (the_layout->num_panes == 1)
                return FALSE;
            else
                return TRUE;
          }
        default:
            assert(FALSE);
            return FALSE;
      }
  }

extern void print_struct_short(struct_type *the_struct, io_class *out)
  {
    out->printf("%s %s", (struct_is_union(the_struct) ? "union" : "struct"),
                the_struct->name());
  }

extern struct_type *init_view(struct_type *the_struct)
  {
    assert(the_struct->op() == TYPE_GROUP);
    group_layout *the_layout =
            (group_layout *)(the_struct->peek_annote(k_s2c_group_translation));
    assert(the_layout != NULL);
    return the_layout->struct_for_pane(0);
  }

extern boolean init_is_union(struct_type *the_struct)
  {
    assert(the_struct->op() == TYPE_GROUP);
    group_layout *the_layout =
            (group_layout *)(the_struct->peek_annote(k_s2c_group_translation));
    assert(the_layout != NULL);
    return (the_layout->num_panes > 1);
  }

extern const char *output_field_name(struct_type *the_struct, unsigned field_num)
  {
    const char *field_name = the_struct->field_name(field_num);
    if (the_struct->op() == TYPE_GROUP)
      {
        group_layout *the_layout =
                (group_layout *)(the_struct->peek_annote(
                                         k_s2c_group_translation));
        assert(the_layout != NULL);
        if (the_layout->num_panes != 1)
          {
            char *result;
            string_io *the_io = new string_io(&result);
            the_io->printf("_%d.%s", the_layout->pane_for_field[field_num],
                           field_name);
            delete the_io;
            field_name = lexicon->enter(result)->sp;
            delete[] result;
          }
      }
    return field_name;
  }

/*----------------------------------------------------------------------*
    End Public Function Implementations
 *----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*
    Begin Private Function Implementations
 *----------------------------------------------------------------------*/

struct_type *group_layout::struct_for_pane(int pane_num)
  {
    unsigned fields_in_pane = 0;
    unsigned num_fields = the_group->num_fields();
    unsigned field_num;
    for (field_num = 0; field_num < num_fields; ++field_num)
      {
        if (pane_for_field[field_num] == pane_num)
            ++fields_in_pane;
      }

    struct_type *new_struct =
            new struct_type(TYPE_STRUCT, the_group->size(), the_group->name(),
                            fields_in_pane);

    unsigned which_field = 0;
    for (field_num = 0; field_num < num_fields; ++field_num)
      {
        if (pane_for_field[field_num] == pane_num)
          {
            new_struct->set_field_name(which_field,
                                       the_group->field_name(field_num));
            new_struct->set_field_type(which_field,
                                       the_group->field_type(field_num));
            new_struct->set_offset(which_field, the_group->offset(field_num));
            ++which_field;
          }
      }

    compact_a_struct(new_struct);
    return new_struct;
  }

void group_layout::print_fields(io_class *out, int nindent)
  {
    assert(num_panes >= 1);
    if (num_panes == 1)
      {
        struct_type *this_struct = struct_for_pane(0);
        print_with_fields(this_struct, out, nindent, TRUE);
        delete this_struct;
      }
    else
      {
        int added_indent = out->printf("union %s { ", the_group->name());
        for (int pane_num = 0; pane_num < num_panes; ++pane_num)
          {
            if (pane_num > 0)
              {
                out->printf("\n");
                ctree_indent(out, nindent + added_indent);
              }

            struct_type *this_struct = struct_for_pane(pane_num);
            print_with_fields(this_struct, out, nindent + added_indent, FALSE);
            delete this_struct;

            out->printf(" _%d;", pane_num);
          }

        out->printf(" }");
      }
  }


static void layout_one_group(struct_type *the_struct)
  {
    assert(the_struct->op() == TYPE_GROUP);

    add_and_mark_init_fields(the_struct);
    sort_fields_by_offset(the_struct);

    unsigned num_fields = the_struct->num_fields();
    int *fill_points = new int[num_fields + 1];
    group_layout *the_layout = new group_layout;
    the_layout->the_group = the_struct;
    the_layout->num_panes = 1;
    the_layout->pane_for_field = new int[num_fields];
    fill_points[0] = 0;
    unsigned next_init_field_num = 0;
    while ((next_init_field_num < num_fields) &&
           !is_init_field(the_struct->field_type(next_init_field_num)))
      {
        ++next_init_field_num;
      }
    for (unsigned field_num = 0; field_num < num_fields; ++field_num)
      {
        int pane_num;
        int tail = the_struct->offset(field_num) +
                   the_struct->field_type(field_num)->size();
        if (field_num == next_init_field_num)
          {
            pane_num = 0;
            ++next_init_field_num;
            while ((next_init_field_num < num_fields) &&
                   !is_init_field(the_struct->field_type(next_init_field_num)))
              {
                ++next_init_field_num;
              }
          }
        else
          {
            if ((next_init_field_num < num_fields) &&
                (tail > the_struct->offset(next_init_field_num)))
              {
                pane_num = 1;
              }
            else
              {
                pane_num = 0;
              }

            for (; pane_num < the_layout->num_panes; ++pane_num)
              {
                if (fill_points[pane_num] <= the_struct->offset(field_num))
                    break;
              }
            if (pane_num == the_layout->num_panes)
                ++the_layout->num_panes;
          }
        fill_points[pane_num] = tail;
        the_layout->pane_for_field[field_num] = pane_num;
      }
    delete[] fill_points;

    the_struct->append_annote(k_s2c_group_translation, the_layout);
  }

static void print_with_fields(struct_type *the_struct, io_class *out,
                              int nindent, boolean with_name)
  {
    assert((the_struct->op() == TYPE_STRUCT) ||
           (the_struct->op() == TYPE_UNION));
    char *pre_name = ((the_struct->op() == TYPE_STRUCT) ? (char*)"struct" : (char*)"union");
    int added_indent = out->printf("%s", pre_name);
    if (with_name)
        added_indent += out->printf(" %s", the_struct->name());

    unsigned num_fields = the_struct->num_fields();

    if ((num_fields == 0) && (the_struct->size() == 0))
        return;

    added_indent += out->printf(" { ");

    int next_offset = 0;

    for (unsigned field_num = 0; field_num < num_fields; ++field_num)
      {
        if (field_num > 0)
          {
            out->printf("\n");
            ctree_indent(out, nindent + added_indent);
          }

        if (the_struct->op() == TYPE_UNION)
          {
            if (the_struct->offset(field_num) != 0)
              {
                error_line(1, NULL,
                           "field %u of `union %s' does not have offset zero",
                           field_num, the_struct->name());
              }
          }
        else
          {
            int new_offset = the_struct->offset(field_num);
            if (new_offset < 0)
              {
                error_line(1, NULL,
                           "field %u of `struct %s' has negative offset",
                           field_num, the_struct->name());
              }
            if (new_offset < next_offset)
              {
                error_line(1, NULL,
                           "field %u of `struct %s' overlaps previous field",
                           field_num, the_struct->name());
              }

            int field_alignment =
                    get_alignment(the_struct->field_type(field_num));
            if (field_alignment != 0)
              {
                if ((new_offset % field_alignment) != 0)
                  {
                    error_line(1, NULL,
                               "field %u of `struct %s' violates alignment "
                               "restrictions", field_num, the_struct->name());
                  }
                int mod = (next_offset % field_alignment);
                if (mod != 0)
                    next_offset += field_alignment - mod;
              }

            fill_field_space(out, new_offset - next_offset,
                    nindent + added_indent, FALSE, FALSE, next_offset);

            next_offset =
                    new_offset + the_struct->field_type(field_num)->size();
          }

        out->printf("%s;", make_c_agg_type(the_struct->field_type(field_num),
                                           the_struct->field_name(field_num)));
      }

    if (the_struct->op() == TYPE_STRUCT)
      {
        int struct_alignment = get_alignment(the_struct);
        if (struct_alignment != 0)
          {
            int mod = (next_offset % struct_alignment);
            if ((mod != 0) &&
                (the_struct->size() == next_offset + struct_alignment - mod))
              {
                next_offset = the_struct->size();
              }
          }
        fill_field_space(out, the_struct->size() - next_offset,
                nindent + added_indent, TRUE, (num_fields == 0), next_offset);
      }

    out->printf(" }");
  }

static void ctree_indent(io_class *out, int amount)
  {
    for (int count = 0; count < amount; ++count)
        out->printf(" ");
  }

static void fill_field_space(io_class *out, int amount, int nindent,
        boolean indent_before, boolean name_needed, int start_offset)
  {
    int remainder = amount;
    boolean name_here = name_needed;

    int block_alignment = target.align[C_int];
    if (block_alignment > 0)
      {
        int mod = (start_offset % block_alignment);
        if ((mod > 0) && ((block_alignment - mod) <= remainder))
          {
            if (indent_before)
              {
                out->printf("\n");
                ctree_indent(out, nindent);
              }
            out->printf("int : 0;");
            if (!indent_before)
              {
                out->printf("\n");
                ctree_indent(out, nindent);
              }
            remainder -= (block_alignment - mod);
          }
      }

    if (fill_field_space_with_char_arrays && (remainder > 0) &&
        ((remainder % target.addressable_size) == 0))
      {
        static unsigned long field_num = 0;

        if (indent_before)
          {
            out->printf("\n");
            ctree_indent(out, nindent);
          }
        out->printf("char dummy_chars%ld[%ld];", field_num,
                    (unsigned long)(remainder / target.addressable_size));
        if (!indent_before)
          {
            out->printf("\n");
            ctree_indent(out, nindent);
          }
        ++field_num;
        return;
      }

    while (remainder > 0)
      {
        int chunk =
                ((target.size[C_int] > 0) && (remainder > target.size[C_int]) ?
                 target.size[C_int] : remainder);

        if (indent_before)
          {
            out->printf("\n");
            ctree_indent(out, nindent);
          }
        out->printf("int ");
        if (name_here)
          {
            out->printf("dummy ");
            name_here = FALSE;
          }
        out->printf(": %d;", chunk);
        if (!indent_before)
          {
            out->printf("\n");
            ctree_indent(out, nindent);
          }
        remainder -= chunk;
      }
  }

static void free_group_layout(void *data)
  {
    group_layout *the_layout = (group_layout *)data;
    delete[] the_layout->pane_for_field;
    delete the_layout;
  }

/*----------------------------------------------------------------------*
    End Private Function Implementations
 *----------------------------------------------------------------------*/
