/* file "ldc.cc" */

/*  Copyright (c) 1994 Stanford University

    All rights reserved.

    This software is provided under the terms described in
    the "suif_copyright.h" include file. */

#include <suif_copyright.h>

/*
 *  This file implements the conversion of loaded constants (immeds) to ctrees
 *  for the s2c program for the SUIF system.
 */

#define RCS_BASE_FILE ldc_cc

#include "s2c.h"
#include <cstring>

RCS_BASE(
    "$Id$")

/*----------------------------------------------------------------------*
    Begin Documentation
 *----------------------------------------------------------------------*

 *----------------------------------------------------------------------*
    End Documentation
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

enum_type *watch_enum_type = NULL;
unsigned watch_enum_member;
boolean watch_enum_used;

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

static boolean has_member_offset_type(type_node *base_type,
                                      type_node *member_type, int offset,
                                      boolean arrays_allowed);
static ctree *get_member_offset_type(type_node *base_type,
                                     type_node *member_type, int offset,
                                     ctree *base_object);
static boolean has_member_offset_size(type_node *base_type, int member_size,
                                      int offset, boolean arrays_allowed);
static ctree *get_member_offset_size(type_node *base_type, int member_size,
                                     int offset, ctree *base_object,
                                     type_node **result_type);
static boolean has_member_at_offset(type_node *base_type, int offset);
static ctree *get_member_at_offset(type_node *base_type, int offset,
                                   ctree *base_object,
                                   type_node **result_type);

/*----------------------------------------------------------------------*
    End Private Function Declarations
 *----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*
    Begin Public Function Implementations
 *----------------------------------------------------------------------*/

extern ctree *ldc_tree(type_node *the_type, immed value,
                       immed_list *field_immeds)
  {
    switch (value.kind())
      {
        case im_int:
        case im_extended_int:
          {
            i_integer value_ii = immed_to_ii(value);

            if (field_immeds != NULL)
              {
                error_line(1, NULL, "\"%s\" annotation on integer ldc",
                           k_fields);
              }

            if (the_type->op() == TYPE_ENUM)
              {
                enum_type *the_enum = (enum_type *)the_type;

                type_node *enum_int_type =
                        new base_type(TYPE_INT, the_enum->size(),
                                      the_enum->is_signed());
                enum_int_type =
                        the_enum->parent()->install_type(enum_int_type);

                unsigned member;
                if (value_ii.is_c_int())
                    member = the_enum->find_member_by_value(value_ii.c_int());
                else
                    member = the_enum->num_values();
                if (member < the_enum->num_values())
                  {
                    if (watch_enum_type != NULL)
                      {
                        if ((watch_enum_type == the_enum) &&
                            (watch_enum_member == member))
                          {
                            watch_enum_used = TRUE;
                          }
                      }

                    ctree *enum_const =
                            new ctree(ctree_enumconst,
                                      the_enum->member(member));
                    if (the_enum->is_signed() &&
                        (the_enum->size() == target.size[C_int]))
                      {
                        return enum_const;
                      }

                    return force_type(enum_const, enum_int_type);
                  }
                else
                  {
                    return ldc_tree(enum_int_type, value);
                  }
              }

            if ((the_type->op() == TYPE_FLOAT) && (value_ii == 0))
                return new ctree(the_type, immed(0.0));

            if (the_type->op() == TYPE_INT)
              {
                base_type *the_base = (base_type *)the_type;

                if (always_intnum)
                    return new ctree(ctree_intconst, the_type, value_ii);

                /*
                 * First, make sure this value fits in the destination
                 * type -- otherwise the SUIF code is illegal.
                 */
                if (!immed_fits(value, the_type))
                  {
                    static boolean warned = FALSE;
                    mistake(&warned, NULL, "constant too large for type");
                  }

                immed new_value = value;
                fit_immed(&new_value, the_type);

                enum C_types the_c_type = c_int_type(the_type);
                if ((the_c_type == C_longlong) && !ll_suffix)
                  {
                    type_node *intermediate_type =
                            (the_base->is_signed() ? type_signed_long :
                                                     type_unsigned_long);
                    ctree *const_tree =
                            new ctree(ctree_intconst, intermediate_type,
                                      (value_ii < 0) ? -value_ii : value_ii);
                    if (value_ii < 0)
                      {
                        ctree *neg_tree = new ctree(ctree_neg);
                        neg_tree->addchild(const_tree);
                        const_tree = neg_tree;
                      }
                    return force_type(const_tree, the_type);
                  }

                if (((the_c_type == C_int) || (the_c_type == C_long) ||
                     (the_c_type == C_longlong)) && the_base->is_signed())
                  {
                    /*
                     * If the value is positive, it can be represented
                     * directly by an integer constant token.
                     */
                    if (value_ii >= 0)
                        return new ctree(ctree_intconst, the_type, value_ii);

                    /*
                     * Otherwise, see if we can represent this as the unary
                     * negation operator applied to a positive int constant
                     * token.
                     */
                    if (immed_fits(ii_to_immed(-value_ii), the_base))
                      {
                        ctree *const_tree =
                                new ctree(ctree_intconst, the_type, -value_ii);

                        ctree *neg_tree = new ctree(ctree_neg);
                        neg_tree->addchild(const_tree);
                        return neg_tree;
                      }

                    /*
                     * If we got here, the value of the constant is a negative
                     * value whose negation is too big to fit in the target
                     * integer type (i.e. a one in the sign bit and all other
                     * bits zero in two's complement).  We'll have to use an
                     * unsigned integer constant, cast it to a signed value
                     * and negate it.
                     */
                    type_node *this_unsigned_type =
                            new base_type(TYPE_INT, the_base->size(), FALSE);
                    this_unsigned_type =
                            the_base->parent()->install_type(
                                    this_unsigned_type);

                    ctree *const_tree =
                            ldc_tree(this_unsigned_type,
                                     ii_to_immed(-value_ii));

                    ctree *conv_tree = force_type(const_tree, the_base);

                    ctree *neg_tree = new ctree(ctree_neg);
                    neg_tree->addchild(conv_tree);

                    return neg_tree;
                  }

                if (((the_c_type == C_int) || (the_c_type == C_long) ||
                     (the_c_type == C_longlong)) && !the_base->is_signed())
                  {
                    return new ctree(ctree_intconst, the_type, value_ii);
                  }

                ctree *const_tree;
                if ((the_c_type == C_char) &&
                    expressable_as_char_const(value_ii) &&
                    immed_fits(value, type_char))
                  {
                    const_tree =
                            new ctree(ctree_charconst, type_signed, value_ii);
                  }
                else
                  {
                    const_tree = ldc_tree(type_signed, value);
                  }

                return force_type(const_tree, the_base);
              }

            ctree *const_tree;
            if (immed_fits(value, type_signed))
                const_tree = ldc_tree(type_signed, value);
            else if (immed_fits(value, type_unsigned))
                const_tree = ldc_tree(type_unsigned, value);
            else if (immed_fits(value, type_signed_long))
                const_tree = ldc_tree(type_signed_long, value);
            else if (immed_fits(value, type_unsigned_long))
                const_tree = ldc_tree(type_unsigned_long, value);
            else if (immed_fits(value, type_signed_longlong))
                const_tree = ldc_tree(type_signed_longlong, value);
            else
                const_tree = ldc_tree(type_unsigned_longlong, value);
            return force_type(const_tree, the_type);
          }
        case im_float:
        case im_extended_float:
          {
            if (field_immeds != NULL)
              {
                error_line(1, NULL, "\"%s\" annotation on floating point ldc",
                           k_fields);
              }

            base_type *float_type = (base_type *)type_double;
            if (the_type->op() == TYPE_FLOAT)
              {
                enum C_types the_c_type = c_float_type(the_type);
                if ((the_c_type == C_float) || (the_c_type == C_double) ||
                    (the_c_type == C_longdouble))
                  {
                    float_type = (base_type *)the_type;
                  }
              }

            ctree *const_tree =
                    new ctree(float_type, value);

            if (float_type == the_type)
                return const_tree;

            return force_type(const_tree, the_type);
          }
        case im_symbol:
          {
            sym_node *symbol = value.symbol();
            int offset = value.offset();
            type_node *sym_type = NULL;

            if (symbol->is_var())
              {
                var_sym *the_var = (var_sym *)symbol;
                sym_type = the_var->type()->unqual();
                annote *string_annote =
                        the_var->annotes()->peek_annote(k_s2c_one_use);
                if (string_annote != NULL)
                  {
                    assert(offset == 0);
                    immed_list *string_immeds = string_annote->immeds();
                    assert(string_immeds->count() == 1);
                    immed string_value = (*string_immeds)[0];
                    assert(string_value.is_string());
                    ctree *string_tree =
                            new ctree(ctree_strconst, string_value.string());
                    return get_address_with_offset(string_tree, sym_type,
                                                   the_type, 0, field_immeds);
                  }
              }
            else if (symbol->is_proc())
              {
                proc_sym *the_proc = (proc_sym *)symbol;
                sym_type = the_proc->type()->unqual();
              }
            else
              {
                error_line(1, NULL, "illegal symbol type in symbolic address");
              }

            ctree *symbol_tree =
                    new ctree(ctree_symconst, symbol, sym_type);

            return get_address_with_offset(symbol_tree, sym_type, the_type,
                                           offset, field_immeds);
          }
        case im_string:
          {
            static boolean warned = FALSE;
            mistake(&warned, NULL,
                    "string immed is illegal as an immediate value");

            return new ctree(ctree_strconst, value.string());
          }
        case im_type:
          {
            static boolean warned = FALSE;
            mistake(&warned, NULL,
                    "type immed is illegal as an immediate value");

            ctree *cast_tree = new ctree(ctree_conv, FALSE, value.type());
            cast_tree->addchild(new ctree(ctree_macro, ""));
            return cast_tree;
          }
        case im_op:
          {
            static boolean warned = FALSE;
            mistake(&warned, NULL,
                    "operand immed is illegal as an immediate value");

            return operand_to_tree(value.op());
          }
        case im_instr:
          {
            static boolean warned = FALSE;
            mistake(&warned, NULL,
                    "instruction immed is illegal as an immediate value");

            return process_solo_instr(value.instr());
          }
        case im_undef:
          {
            static boolean warned = FALSE;
            mistake(&warned, NULL,
                    "undefined immed is illegal as an immediate value");

            return new ctree(ctree_macro, "__undefined__");
          }
        default:
            assert(FALSE);
            return NULL;
      }
  }

extern ctree *get_address_with_offset(ctree *object_tree,
                                      type_node *object_type,
                                      type_node *result_type, int offset,
                                      immed_list *field_immeds)
  {
    ctree *current_object = object_tree;
    type_node *current_obj_type = object_type->unqual();
    int remaining_offset = offset;

    if (field_immeds != NULL)
      {
        immed_list_iter field_iter(field_immeds);
        while (!field_iter.is_empty())
          {
            immed this_field = field_iter.step();

            if (!this_field.is_string())
              {
                error_line(1, NULL, "bad format for \"%s\" annotation",
                           k_fields);
              }
            const char *field_name = this_field.string();

            if (!current_obj_type->is_struct())
              {
                error_line(1, NULL,
                           "\"%s\" annotation on non-struct-or-union type",
                           k_fields);
              }
            struct_type *obj_struct = (struct_type *)current_obj_type;

            unsigned field_num = obj_struct->num_fields();

            annote *orig_names_annote =
                    obj_struct->annotes()->peek_annote(
                            k_s2c_original_field_names);
            if (orig_names_annote != NULL)
              {
                immed_list_iter names_iter(orig_names_annote->immeds());
                while (!names_iter.is_empty())
                  {
                    immed first_immed = names_iter.step();
                    assert(!names_iter.is_empty());
                    immed second_immed = names_iter.step();
                    assert(first_immed.is_string());
                    assert(second_immed.is_integer());

                    if (strcmp(first_immed.string(), field_name) == 0)
                      {
                        field_num = second_immed.integer();
                        break;
                      }
                  }
              }

            if (field_num >= obj_struct->num_fields())
                field_num = obj_struct->find_field_by_name(field_name);

            if (field_num >= obj_struct->num_fields())
              {
                error_line(1, NULL,
                           "\"%s\" annotation name `%s' does not correspond "
                           "to any field name in structure `%s'", k_fields,
                           field_name, obj_struct->name());
              }

            int field_offset = obj_struct->offset(field_num);
            remaining_offset -= field_offset;

            current_obj_type = obj_struct->field_type(field_num)->unqual();

            ctree *field_tree =
                    new ctree(ctree_strsel,
                              output_field_name(obj_struct, field_num));
            field_tree->addchild(current_object);
            current_object = field_tree;
          }
      }

    if (result_type->unqual()->is_ptr())
      {
        ptr_type *the_ptr = (ptr_type *)(result_type->unqual());
        type_node *ref_type = the_ptr->ref_type()->unqual();

        if (has_member_offset_type(current_obj_type, ref_type,
                                   remaining_offset, TRUE))
          {
            ctree *new_object =
                    get_member_offset_type(current_obj_type, ref_type,
                                           remaining_offset, current_object);

            ctree *address_tree;
            if (ref_type->is_array() || ref_type->is_func())
              {
                address_tree = new_object;
              }
            else
              {
                address_tree = new ctree(ctree_addrof);
                address_tree->addchild(new_object);
              }

            return force_type(address_tree, result_type);
          }

        if (has_member_offset_size(current_obj_type, ref_type->size(),
                                   remaining_offset, TRUE))
          {
            type_node *new_obj_type;
            ctree *new_object =
                    get_member_offset_size(current_obj_type, ref_type->size(),
                                           remaining_offset, current_object,
                                           &new_obj_type);

            ctree *address_tree;
            if (new_obj_type->is_array() || new_obj_type->is_func())
              {
                address_tree = new_object;
              }
            else
              {
                address_tree = new ctree(ctree_addrof);
                address_tree->addchild(new_object);
              }

            return force_type(address_tree, result_type);
          }
      }

    ctree *new_object;
    if (has_member_at_offset(current_obj_type, remaining_offset))
      {
        new_object =
                get_member_at_offset(current_obj_type, remaining_offset,
                                     current_object, &current_obj_type);
      }
    else
      {
        ctree *obj_address;
        if (current_obj_type->is_array() || current_obj_type->is_func())
          {
            obj_address = current_object;
          }
        else
          {
            obj_address = new ctree(ctree_addrof);
            obj_address->addchild(current_object);
          }

        type_node *char_pointer = type_char->ptr_to();
        ctree *obj_conv = force_type(obj_address, char_pointer);

        new_object = new ctree(ctree_subscr);
        new_object->addchild(obj_conv);
        new_object->addchild(ldc_tree(type_ptr_diff,
                                      immed(remaining_offset /
                                            target.addressable_size)));

        current_obj_type = type_char;
      }

    ctree *address_tree;
    if (current_obj_type->is_array() || current_obj_type->is_func())
      {
        address_tree = new_object;
      }
    else
      {
        address_tree = new ctree(ctree_addrof);
        address_tree->addchild(new_object);
      }

    return force_type(address_tree, result_type);
  }

/*----------------------------------------------------------------------*
    End Public Function Implementations
 *----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*
    Begin Private Function Implementations
 *----------------------------------------------------------------------*/


static boolean has_member_offset_type(type_node *base_type,
                                      type_node *member_type, int offset,
                                      boolean arrays_allowed)
  {
    assert((base_type != NULL) && (member_type != NULL));

    if (base_type->is_same(member_type) && (offset == 0))
        return TRUE;
        
    switch (base_type->op())
      {
        case TYPE_ARRAY:
          {
            if (!arrays_allowed)
                break;

            array_type *the_array = (array_type *)base_type;
            type_node *elem_type = the_array->elem_type()->unqual();

            if (elem_type->size() == 0)
                break;

            int elem_offset = (offset % elem_type->size());
            return has_member_offset_type(elem_type, member_type, elem_offset,
                                          TRUE);
          }
        case TYPE_GROUP:
        case TYPE_STRUCT:
        case TYPE_UNION:
          {
            struct_type *the_struct = (struct_type *)base_type;

            unsigned num_fields = the_struct->num_fields();
            for (unsigned field_num = 0; field_num < num_fields; ++field_num)
              {
                type_node *field_type =
                        the_struct->field_type(field_num)->unqual();
                int field_offset = the_struct->offset(field_num);
                if ((offset >= field_offset) &&
                    ((offset + member_type->size()) <=
                     (field_offset + field_type->size())) &&
                    has_member_offset_type(field_type, member_type,
                                           offset - field_offset,
                                           arrays_allowed))
                  {
                    return TRUE;
                  }
              }
            break;
          }
        default:
            break;
      }

    return FALSE;
  }

static ctree *get_member_offset_type(type_node *base_type,
                                     type_node *member_type, int offset,
                                     ctree *base_object)
  {
    assert((base_type != NULL) && (member_type != NULL));

    if (base_type->is_same(member_type) && (offset == 0))
        return base_object;
        
    switch (base_type->op())
      {
        case TYPE_ARRAY:
          {
            array_type *the_array = (array_type *)base_type;
            type_node *elem_type = the_array->elem_type()->unqual();

            assert(elem_type->size() != 0);

            int elem_offset = (offset % elem_type->size());

            ctree *aref_tree = new ctree(ctree_subscr);
            aref_tree->addchild(base_object);
            aref_tree->addchild(ldc_tree(type_ptr_diff,
                                         immed(offset / elem_type->size())));

            return get_member_offset_type(elem_type, member_type, elem_offset,
                                          aref_tree);
          }
        case TYPE_GROUP:
        case TYPE_STRUCT:
        case TYPE_UNION:
          {
            struct_type *the_struct = (struct_type *)base_type;

            unsigned num_fields = the_struct->num_fields();
            unsigned field_num;
            for (field_num = 0; field_num < num_fields; ++field_num)
              {
                type_node *field_type =
                        the_struct->field_type(field_num)->unqual();
                int field_offset = the_struct->offset(field_num);
                if ((offset >= field_offset) &&
                    ((offset + member_type->size()) <=
                     (field_offset + field_type->size())) &&
                    has_member_offset_type(field_type, member_type,
                                           offset - field_offset, FALSE))
                  {
                    ctree *field_ref_tree =
                            new ctree(ctree_strsel,
                                      output_field_name(the_struct,
                                                        field_num));
                    field_ref_tree->addchild(base_object);
                    return get_member_offset_type(field_type, member_type,
                                                  offset - field_offset,
                                                  field_ref_tree);
                  }
              }

            for (field_num = 0; field_num < num_fields; ++field_num)
              {
                type_node *field_type =
                        the_struct->field_type(field_num)->unqual();
                int field_offset = the_struct->offset(field_num);
                if ((offset >= field_offset) &&
                    ((offset + member_type->size()) <=
                     (field_offset + field_type->size())) &&
                    has_member_offset_type(field_type, member_type,
                                           offset - field_offset, TRUE))
                  {
                    ctree *field_ref_tree =
                            new ctree(ctree_strsel,
                                      output_field_name(the_struct,
                                                        field_num));
                    field_ref_tree->addchild(base_object);
                    return get_member_offset_type(field_type, member_type,
                                                  offset - field_offset,
                                                  field_ref_tree);
                  }
              }
            break;
          }
        default:
            break;
      }

    assert(FALSE);
    return NULL;
  }

static boolean has_member_offset_size(type_node *base_type, int member_size,
                                      int offset, boolean arrays_allowed)
  {
    assert(base_type != NULL);

    if ((base_type->size() == member_size) && (offset == 0))
        return TRUE;
        
    switch (base_type->op())
      {
        case TYPE_ARRAY:
          {
            if (!arrays_allowed)
                break;

            array_type *the_array = (array_type *)base_type;
            type_node *elem_type = the_array->elem_type()->unqual();

            if (elem_type->size() == 0)
                break;

            int elem_offset = (offset % elem_type->size());
            return has_member_offset_size(elem_type, member_size, elem_offset,
                                          TRUE);
          }
        case TYPE_GROUP:
        case TYPE_STRUCT:
        case TYPE_UNION:
          {
            struct_type *the_struct = (struct_type *)base_type;

            unsigned num_fields = the_struct->num_fields();
            for (unsigned field_num = 0; field_num < num_fields; ++field_num)
              {
                type_node *field_type =
                        the_struct->field_type(field_num)->unqual();
                int field_offset = the_struct->offset(field_num);
                if ((offset >= field_offset) &&
                    ((offset + member_size) <=
                     (field_offset + field_type->size())) &&
                    has_member_offset_size(field_type, member_size,
                                           offset - field_offset,
                                           arrays_allowed))
                  {
                    return TRUE;
                  }
              }
            break;
          }
        default:
            break;
      }

    return FALSE;
  }

static ctree *get_member_offset_size(type_node *base_type, int member_size,
                                     int offset, ctree *base_object,
                                     type_node **result_type)
  {
    assert(base_type != NULL);

    if ((base_type->size() == member_size) && (offset == 0))
      {
        *result_type = base_type;
        return base_object;
      }
        
    switch (base_type->op())
      {
        case TYPE_ARRAY:
          {
            array_type *the_array = (array_type *)base_type;
            type_node *elem_type = the_array->elem_type()->unqual();

            assert(elem_type->size() != 0);

            int elem_offset = (offset % elem_type->size());

            ctree *aref_tree = new ctree(ctree_subscr);
            aref_tree->addchild(base_object);
            aref_tree->addchild(ldc_tree(type_ptr_diff,
                                         immed(offset / elem_type->size())));

            return get_member_offset_size(elem_type, member_size, elem_offset,
                                          aref_tree, result_type);
          }
        case TYPE_GROUP:
        case TYPE_STRUCT:
        case TYPE_UNION:
          {
            struct_type *the_struct = (struct_type *)base_type;

            unsigned num_fields = the_struct->num_fields();
            unsigned field_num;
            for (field_num = 0; field_num < num_fields; ++field_num)
              {
                type_node *field_type =
                        the_struct->field_type(field_num)->unqual();
                int field_offset = the_struct->offset(field_num);
                if ((offset >= field_offset) &&
                    ((offset + member_size) <=
                     (field_offset + field_type->size())) &&
                    has_member_offset_size(field_type, member_size,
                                           offset - field_offset, FALSE))
                  {
                    ctree *field_ref_tree =
                            new ctree(ctree_strsel,
                                      output_field_name(the_struct,
                                                        field_num));
                    field_ref_tree->addchild(base_object);
                    return get_member_offset_size(field_type, member_size,
                                                  offset - field_offset,
                                                  field_ref_tree, result_type);
                  }
              }

            for (field_num = 0; field_num < num_fields; ++field_num)
              {
                type_node *field_type =
                        the_struct->field_type(field_num)->unqual();
                int field_offset = the_struct->offset(field_num);
                if ((offset >= field_offset) &&
                    ((offset + member_size) <=
                     (field_offset + field_type->size())) &&
                    has_member_offset_size(field_type, member_size,
                                           offset - field_offset, TRUE))
                  {
                    ctree *field_ref_tree =
                            new ctree(ctree_strsel,
                                      output_field_name(the_struct,
                                                        field_num));
                    field_ref_tree->addchild(base_object);
                    return get_member_offset_size(field_type, member_size,
                                                  offset - field_offset,
                                                  field_ref_tree, result_type);
                  }
              }
            break;
          }
        default:
            break;
      }

    assert(FALSE);
    return NULL;
  }

static boolean has_member_at_offset(type_node *base_type, int offset)
  {
    assert(base_type != NULL);

    if (offset == 0)
        return TRUE;
        
    switch (base_type->op())
      {
        case TYPE_ARRAY:
          {
            array_type *the_array = (array_type *)base_type;
            type_node *elem_type = the_array->elem_type()->unqual();

            if (elem_type->size() == 0)
                break;

            int elem_offset = (offset % elem_type->size());
            return has_member_at_offset(elem_type, elem_offset);
          }
        case TYPE_GROUP:
        case TYPE_STRUCT:
        case TYPE_UNION:
          {
            struct_type *the_struct = (struct_type *)base_type;

            unsigned num_fields = the_struct->num_fields();
            for (unsigned field_num = 0; field_num < num_fields; ++field_num)
              {
                type_node *field_type =
                        the_struct->field_type(field_num)->unqual();
                int field_offset = the_struct->offset(field_num);
                if ((offset >= field_offset) &&
                    (offset < (field_offset + field_type->size())) &&
                    has_member_at_offset(field_type, offset - field_offset))
                  {
                    return TRUE;
                  }
              }
            break;
          }
        default:
            break;
      }

    return FALSE;
  }

static ctree *get_member_at_offset(type_node *base_type, int offset,
                                   ctree *base_object,
                                   type_node **result_type)
  {
    assert(base_type != NULL);

    if (offset == 0)
      {
        *result_type = base_type;
        return base_object;
      }
        
    switch (base_type->op())
      {
        case TYPE_ARRAY:
          {
            array_type *the_array = (array_type *)base_type;
            type_node *elem_type = the_array->elem_type()->unqual();

            assert(elem_type->size() != 0);

            int elem_offset = (offset % elem_type->size());

            ctree *aref_tree = new ctree(ctree_subscr);
            aref_tree->addchild(base_object);
            aref_tree->addchild(ldc_tree(type_ptr_diff,
                                         immed(offset / elem_type->size())));

            return get_member_at_offset(elem_type, elem_offset, aref_tree,
                                        result_type);
          }
        case TYPE_GROUP:
        case TYPE_STRUCT:
        case TYPE_UNION:
          {
            struct_type *the_struct = (struct_type *)base_type;

            unsigned num_fields = the_struct->num_fields();
            for (unsigned field_num = 0; field_num < num_fields; ++field_num)
              {
                type_node *field_type =
                        the_struct->field_type(field_num)->unqual();
                int field_offset = the_struct->offset(field_num);
                if ((offset >= field_offset) &&
                    (offset < (field_offset + field_type->size())) &&
                    has_member_at_offset(field_type, offset - field_offset))
                  {
                    ctree *field_ref_tree =
                            new ctree(ctree_strsel,
                                      output_field_name(the_struct,
                                                        field_num));
                    field_ref_tree->addchild(base_object);
                    return get_member_at_offset(field_type,
                                                offset - field_offset,
                                                field_ref_tree, result_type);
                  }
              }
            break;
          }
        default:
            break;
      }

    assert(FALSE);
    return NULL;
  }

/*----------------------------------------------------------------------*
    End Private Function Implementations
 *----------------------------------------------------------------------*/
