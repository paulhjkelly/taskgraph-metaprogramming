/*  Copyright (c) 1994, 1995, 1996 Stanford University

    All rights reserved.

    This software is provided under the terms described in
    the "suif_copyright.h" include file. */

#include <suif_copyright.h>

/*
 * This is the main program for the SUIF library of miscellaneous
 * useful routines.
 */

#define _MODULE_ "libuseful.a"

#pragma implementation "basic.h"

#define RCS_BASE_FILE useful_cc

#include "useful_internal.h"
#include <cstring>
#include <climits>

RCS_BASE(
    "$Id$")

/*----------------------------------------------------------------------*
    Begin Documentation
 *----------------------------------------------------------------------*

                Summary
                -------

        The SUIF ``useful'' library provides a place to put various
        sorts of functions that are useful in various passes and
        libraries.  These are built on top of the basic SUIF library
        and may or may not be included in future versions of the SUIF
        system.


 *----------------------------------------------------------------------*
    End Documentation
 *----------------------------------------------------------------------*/

class field_struct
  {
public:
    type_node *type;
    const char *name;
    int offset;
  };


static void merge_sort_fields(field_struct *start_array,
                              field_struct *end_array,
                              field_struct *work_array, unsigned num_fields);
static boolean instr_may_ref_location_internal(instruction *the_instr,
                                               operand address,
                                               var_sym *base_var);
static boolean operand_may_ref_location_internal(operand the_operand,
                                                 operand address,
                                                 var_sym *base_var);
static void find_label_uses_on_object(suif_object *the_object,
                                      so_walker *the_walker);


const char *k_pure_function;
const char *k_dead;
const char *k_addr_not_stored;
const char *k_guarded;
const char *k_globalize;
const char *k_filize;
const char *k_replacement;
const char *k_not_fortran_reason;
const char *k_no_parameter_aliasing;
const char *k_direct_calls_only;
const char *k_fortran_intrinsic;
const char *k_fred;
const char *k_fortran_unfixables_called;
const char *k_no_recursion;
const char *k_fortran_power_op;
const char *k_form_array;
const char *k_is_constant;
const char *k_no_break;
const char *k_potential_break;


extern void init_useful(int & /* argc */, char * /* argv */ [])
  {
    ANNOTE(k_pure_function,              "pure function",              TRUE);
    ANNOTE(k_dead,                       "dead",                       TRUE);
    ANNOTE(k_addr_not_stored,            "addr not stored",            TRUE);
    ANNOTE(k_guarded,                    "guarded",                    TRUE);
    ANNOTE(k_globalize,                  "globalize",                  TRUE);
    ANNOTE(k_filize,                     "filize",                     TRUE);
    ANNOTE(k_replacement,                "replacement",                FALSE);
    ANNOTE(k_not_fortran_reason,         "not Fortran reason",         TRUE);
    ANNOTE(k_no_parameter_aliasing,      "no parameter aliasing",      TRUE);
    ANNOTE(k_direct_calls_only,          "direct_calls_only",          TRUE);
    ANNOTE(k_fortran_intrinsic,          "Fortran intrinsic",          TRUE);
    ANNOTE(k_fred,                       "Fred",                       TRUE);
    ANNOTE(k_fortran_unfixables_called,  "Fortran unfixables called",  TRUE);
    ANNOTE(k_no_recursion,               "no recursion",               TRUE);
    ANNOTE(k_fortran_power_op,           "Fortran power op",           TRUE);
    ANNOTE(k_form_array,                 "form array",                 TRUE);
    ANNOTE(k_is_constant,                "is constant",                TRUE);
    ANNOTE(k_no_break,                   "no break",                   TRUE);
    ANNOTE(k_potential_break,            "potential break",            TRUE);
    init_labinfo();
    init_linkinfo();
    init_make_instr_ops();
    init_transform();
  }

extern void exit_useful(void)
  {
    return;
  }

extern int align_up(int offset, type_node *the_type)
  {
    int alignment = get_alignment(the_type);
    if (alignment == 0)
        return offset;
    int remainder = offset % alignment;
    if (remainder == 0)
        return offset;
    return offset + alignment - remainder;
  }

extern int get_alignment(type_node *the_type)
  {
    switch (the_type->op())
      {
        case TYPE_INT:
        case TYPE_ENUM:
          {
            enum C_types the_c_type = c_int_type(the_type);
            if (the_c_type == num_C_types)
                return target.addressable_size;
            else
                return target.align[the_c_type];
          }
        case TYPE_FLOAT:
          {
            enum C_types the_c_type = c_float_type(the_type);
            if (the_c_type == num_C_types)
                return target.addressable_size;
            else
                return target.align[the_c_type];
          }
        case TYPE_VOID:
            return target.addressable_size;
        case TYPE_PTR:
            return target.align[C_ptr];
        case TYPE_ARRAY:
          {
            array_type *the_array = (array_type *)the_type;
            i_integer e_align = get_alignment(the_array->elem_type());
            i_integer a_align = target.array_align;
            i_integer lcm = (e_align / ii_gcd(e_align, a_align)) * a_align;
            if (!lcm.is_c_int())
                error_line(1, NULL, "overflow in alignment calculation");
            return lcm.c_int();
          }
        case TYPE_FUNC:
            return target.addressable_size;
        case TYPE_GROUP:
        case TYPE_STRUCT:
        case TYPE_UNION:
          {
            struct_type *the_struct = (struct_type *)the_type;
            i_integer total = target.struct_align;
            unsigned num_fields = the_struct->num_fields();
            for (unsigned field_num = 0; field_num < num_fields; ++field_num)
              {
                i_integer field_align =
                        get_alignment(the_struct->field_type(field_num));
                total = (total / ii_gcd(total, field_align)) * field_align;
              }
            if (!total.is_c_int())
                error_line(1, NULL, "overflow in alignment calculation");
            return total.c_int();
          }
        case TYPE_CONST:
        case TYPE_VOLATILE:
        case TYPE_CALL_BY_REF:
	case TYPE_RESTRICT:
        case TYPE_NULL:
          {
            modifier_type *the_modifier = (modifier_type *)the_type;
            return get_alignment(the_modifier->base());
          }
        default:
            assert(FALSE);
            return 0;
      }
  }

extern enum C_types c_int_type(type_node *the_type)
  {
    assert((the_type->op() == TYPE_INT) || (the_type->op() == TYPE_ENUM));

    if (the_type->size() == target.size[C_int])
        return C_int;
    else if (the_type->size() == target.size[C_char])
        return C_char;
    else  if (the_type->size() == target.size[C_short])
        return C_short;
    else  if (the_type->size() == target.size[C_long])
        return C_long;
    else  if (the_type->size() == target.size[C_longlong])
        return C_longlong;
    return num_C_types;
  }

extern enum C_types c_float_type(type_node *the_type)
  {
    assert(the_type->op() == TYPE_FLOAT);

    if (the_type->size() == target.size[C_float])
        return C_float;
    else if (the_type->size() == target.size[C_double])
        return C_double;
    else  if (the_type->size() == target.size[C_longdouble])
        return C_longdouble;
    return num_C_types;
  }

extern base_type *c_type_to_suif(enum C_types c_type,
                                 boolean is_signed)
  {
    base_type *suif_type;

    switch (c_type)
      {
        case C_char:
        case C_short:
        case C_int:
        case C_long:
        case C_longlong:
            suif_type = new base_type(TYPE_INT, target.size[c_type],
                                      is_signed);
            break;
        case C_float:
        case C_double:
        case C_longdouble:
            suif_type = new base_type(TYPE_FLOAT, target.size[c_type]);
            break;
        default:
            suif_type = NULL;
            assert(FALSE);
      }
    return (base_type *)(fileset->globals()->install_type(suif_type));
  }

extern boolean non_negative(type_node *the_type)
  {
    if ((the_type->unqual()->op() == TYPE_INT) ||
        (the_type->unqual()->op() == TYPE_ENUM))
      {
        base_type *the_base_type = (base_type *)(the_type->unqual());
        return !(the_base_type->is_signed());
      }
    else if (the_type->unqual()->op() == TYPE_PTR)
      {
        return TRUE;
      }
    return FALSE;
  }

extern operand operand_from_array_bound(array_bound the_bound)
  {
    if (the_bound.is_constant())
        return const_op(immed(the_bound.constant()), type_ptr_diff);
    else if (the_bound.is_variable())
        return operand(the_bound.variable());
    else
        return const_op(immed(0), type_ptr_diff);
  }

extern boolean any_part_volatile(type_node *the_type)
  {
    if (the_type->is_volatile())
        return TRUE;

    type_node *unqual = the_type->unqual();
    switch (unqual->op())
      {
        case TYPE_ARRAY:
          {
            array_type *the_array_type = (array_type *)unqual;
            return any_part_volatile(the_array_type->elem_type());
          }
        case TYPE_GROUP:
        case TYPE_STRUCT:
        case TYPE_UNION:
          {
            struct_type *the_struct_type = (struct_type *)unqual;

            unsigned num_fields = the_struct_type->num_fields();
            for (unsigned field_num = 0; field_num < num_fields; ++field_num)
              {
                if (any_part_volatile(the_struct_type->field_type(field_num)))
                    return TRUE;
              }

            break;
          }
        default:
            break;
      }

    return FALSE;
  }

extern type_node_list *cast_sequence(type_node *source_type,
                                     type_node *target_type)
  {
    type_node *unqual_source = source_type->unqual();
    type_node *unqual_target = target_type->unqual();

    type_ops source_op = unqual_source->op();
    type_ops target_op = unqual_target->op();

    if ((source_op == TYPE_VOID) || (source_op == TYPE_ARRAY) ||
        (source_op == TYPE_FUNC) || (source_op == TYPE_GROUP) ||
        (source_op == TYPE_STRUCT) || (source_op == TYPE_UNION) ||
        (target_op == TYPE_VOID) || (target_op == TYPE_ARRAY) ||
        (target_op == TYPE_FUNC) || (target_op == TYPE_GROUP) ||
        (target_op == TYPE_STRUCT) || (target_op == TYPE_UNION))
      {
        return NULL;
      }

    type_node_list *result = new type_node_list;

    switch (source_op)
      {
        case TYPE_INT:
        case TYPE_ENUM:
          {
            base_type *source_base = (base_type *)unqual_source;

            switch (target_op)
              {
                case TYPE_INT:
                case TYPE_ENUM:
                  {
                    base_type *target_base = (base_type *)unqual_target;
                    if (source_base->is_signed() != target_base->is_signed())
                      {
                        if (source_base->size() < target_base->size())
                          {
                            type_node *new_type =
                                    new base_type(TYPE_INT,
                                                  target_base->size(),
                                                  source_base->is_signed());
                            new_type =
                                    fileset->globals()->install_type(new_type);
                            result->append(new_type);
                          }
                        else if (source_base->size() > target_base->size())
                          {
                            type_node *new_type =
                                    new base_type(TYPE_INT,
                                                  source_base->size(),
                                                  target_base->is_signed());
                            new_type =
                                    fileset->globals()->install_type(new_type);
                            result->append(new_type);
                          }
                      }
                    break;
                  }
                case TYPE_FLOAT:
                  {
                    if (source_base->size() < target.size[C_int])
                      {
                        type_node *new_type =
                                new base_type(TYPE_INT, target.size[C_int],
                                              source_base->is_signed());
                        new_type = fileset->globals()->install_type(new_type);
                        result->append(new_type);
                      }
                    break;
                  }
                case TYPE_PTR:
                  {
                    int ptr_int_size = target.size[target.ptr_diff_type];
                    if (source_base->size() != ptr_int_size)
                      {
                        type_node *new_type =
                                new base_type(TYPE_INT, ptr_int_size,
                                              source_base->is_signed());
                        new_type = fileset->globals()->install_type(new_type);
                        result->append(new_type);
                      }
                    break;
                  }
                default:
                    assert(FALSE);
              }

            break;
          }
        case TYPE_FLOAT:
          {
            switch (target_op)
              {
                case TYPE_INT:
                case TYPE_ENUM:
                  {
                    base_type *target_base = (base_type *)unqual_target;
                    if (target_base->size() < target.size[C_int])
                      {
                        type_node *new_type =
                                new base_type(TYPE_INT, target.size[C_int],
                                              target_base->is_signed());
                        new_type = fileset->globals()->install_type(new_type);
                        result->append(new_type);
                      }
                    break;
                  }
                case TYPE_FLOAT:
                    break;
                case TYPE_PTR:
                  {
                    int ptr_int_size = target.size[target.ptr_diff_type];
                    type_node *new_type =
                            new base_type(TYPE_INT, ptr_int_size, TRUE);
                    new_type = fileset->globals()->install_type(new_type);

                    delete result;
                    result = cast_sequence(source_type, new_type);

                    break;
                  }
                default:
                    assert(FALSE);
              }

            break;
          }
        case TYPE_PTR:
          {
            switch (target_op)
              {
                case TYPE_INT:
                case TYPE_ENUM:
                  {
                    base_type *target_base = (base_type *)unqual_target;
                    int ptr_int_size = target.size[target.ptr_diff_type];
                    if (target_base->size() != ptr_int_size)
                      {
                        type_node *new_type =
                                new base_type(TYPE_INT, ptr_int_size,
                                              target_base->is_signed());
                        new_type = fileset->globals()->install_type(new_type);
                        result->append(new_type);
                      }
                    break;
                  }
                case TYPE_FLOAT:
                  {
                    int ptr_int_size = target.size[target.ptr_diff_type];
                    type_node *new_type =
                            new base_type(TYPE_INT, ptr_int_size, TRUE);
                    new_type = fileset->globals()->install_type(new_type);

                    delete result;
                    result = cast_sequence(new_type, target_type);
                    result->push(new_type);
                    return result;
                  }
                case TYPE_PTR:
                    break;
                default:
                    assert(FALSE);
              }

            break;
          }
        default:
            assert(FALSE);
      }

    result->append(target_type);
    return result;
  }

extern void sort_fields_by_offset(struct_type *the_struct)
  {
    unsigned num_fields = the_struct->num_fields();
    if (num_fields == 0)
        return;

    field_struct *field_array = new field_struct[num_fields];
    field_struct *end_array = new field_struct[num_fields];
    field_struct *work_array = new field_struct[num_fields];

    unsigned field_num;
    for (field_num = 0; field_num < num_fields; ++field_num)
      {
        field_array[field_num].type = the_struct->field_type(field_num);
        field_array[field_num].name = the_struct->field_name(field_num);
        field_array[field_num].offset = the_struct->offset(field_num);
      }

    merge_sort_fields(field_array, end_array, work_array, num_fields);

    for (field_num = 0; field_num < num_fields; ++field_num)
      {
        the_struct->set_field_type(field_num, end_array[field_num].type);
        the_struct->set_field_name(field_num, end_array[field_num].name);
        the_struct->set_offset(field_num, end_array[field_num].offset);
      }

    delete[] field_array;
    delete[] end_array;
    delete[] work_array;
  }

extern boolean native_floating_arithmetic_ok(base_type *the_type)
  {
    assert(the_type->op() == TYPE_FLOAT);

    /*
     *  Currently, we assume that if ``double'' on the compiling
     *  machine is at least as bit as the given type, using
     *  compiling-machine arithmetic is ok.  This may not always be
     *  what's needed, for example if we want to stay strictly with
     *  the target floating point representation and arithmetic and
     *  that differs from that on the compiling machine.  If so, only
     *  this function needs to be changed to be conservative about
     *  using compiler arithmetic.
     */
    return ((size_t)(the_type->size()) <= (sizeof(double) * suif_byte_size));
  }

extern type_node *diff_type(type_node *original_type)
  {
    switch (original_type->unqual()->op())
      {
        case TYPE_INT:
        case TYPE_FLOAT:
            return original_type->unqual();
        case TYPE_PTR:
            return type_ptr_diff;
        case TYPE_ENUM:
          {
            enum_type *the_enum = (enum_type *)original_type;
            base_type *result =
                    new base_type(TYPE_INT, the_enum->size(),
                                  the_enum->is_signed());
            return the_enum->parent()->install_type(result);
          }
        default:
            assert(FALSE);
            return NULL;
      }
  }

extern type_node *cast_up(type_node *type1, type_node *type2)
  {
    type_node *unqual1 = type1->unqual();
    type_node *unqual2 = type2->unqual();

    if (unqual1->op() == TYPE_FLOAT)
      {
        if (unqual2->op() == TYPE_FLOAT)
          {
            if (unqual1->size() >= unqual2->size())
                return unqual1;
            else
                return unqual2;
          }
        else
          {
            return unqual1;
          }
      }

    if (unqual2->op() == TYPE_FLOAT)
        return unqual2;

    assert((unqual1->op() == TYPE_INT) || (unqual1->op() == TYPE_ENUM));

    base_type *base1 = (base_type *)unqual1;
    base_type *base2 = (base_type *)unqual2;
    int new_size;
    if (base1->size() >= base2->size())
        new_size = base1->size();
    else
        new_size = base2->size();
    boolean new_is_signed;
    if (base1->is_signed() == base2->is_signed())
      {
        new_is_signed = base1->is_signed();
      }
    else
      {
        int the_c_type;
        for (the_c_type = (int)C_char; the_c_type <= (int)C_longlong;
             ++the_c_type)
          {
            if (target.size[the_c_type] > new_size)
              {
                new_size = target.size[the_c_type];
                new_is_signed = TRUE;
                break;
              }
          }
        if (the_c_type > C_longlong)
          {
            new_is_signed = FALSE;
          }
      }
    base_type *new_base = new base_type(TYPE_INT, new_size, new_is_signed);
    return fileset->globals()->install_type(new_base);
  }

extern boolean instr_references_var(instruction *the_instr, var_sym *the_var)
  {
    unsigned num_srcs = the_instr->num_srcs();
    for (unsigned src_num = 0; src_num < num_srcs; ++src_num)
      {
        if (operand_references_var(the_instr->src_op(src_num), the_var))
            return TRUE;
      }
    return FALSE;
  }

extern boolean operand_references_var(operand the_operand, var_sym *the_var)
  {
    switch (the_operand.kind())
      {
        case OPER_NULL:
            return FALSE;
        case OPER_SYM:
            return (the_var->overlaps(the_operand.symbol()));
        case OPER_INSTR:
            return instr_references_var(the_operand.instr(), the_var);
        default:
            assert(FALSE);
            return FALSE;
      }
  }

extern boolean instr_may_reference_var(instruction *the_instr,
                                       var_sym *the_var)
  {
    if (instr_is_impure_call(the_instr) &&
        (the_var->is_addr_taken() || (!(the_var->is_auto()))))
      {
        return TRUE;
      }

    if (the_var->is_addr_taken() &&
        ((the_instr->opcode() == io_cal) || (the_instr->opcode() == io_lod) ||
         (the_instr->opcode() == io_memcpy) ||
         (the_instr->opcode() == io_gen)))
      {
        unsigned num_srcs = the_instr->num_srcs();
        unsigned src_num = 0;
        if ((the_instr->opcode() == io_cal) ||
            (the_instr->opcode() == io_memcpy))
          {
            src_num = 1;
          }
        for (; src_num < num_srcs; ++src_num)
          {
            operand this_op = the_instr->src_op(src_num);
            if (this_op.type()->unqual()->is_ptr())
              {
                sym_node *addr_root_symbol =
                        operand_address_root_symbol(this_op);
                if ((addr_root_symbol == NULL) ||
                    (the_var->root_ancestor() == addr_root_symbol))
                  {
                    return TRUE;
                  }
              }
          }
      }

    unsigned num_srcs = the_instr->num_srcs();
    for (unsigned src_num = 0; src_num < num_srcs; ++src_num)
      {
        if (operand_may_reference_var(the_instr->src_op(src_num), the_var))
            return TRUE;
      }
    return FALSE;
  }

extern boolean operand_may_reference_var(operand the_operand,
                                         var_sym *the_var)
  {
    switch (the_operand.kind())
      {
        case OPER_NULL:
            return FALSE;
        case OPER_SYM:
            return (the_var->overlaps(the_operand.symbol()));
        case OPER_INSTR:
            return instr_may_reference_var(the_operand.instr(), the_var);
        default:
            assert(FALSE);
            return FALSE;
      }
  }

extern boolean node_may_reference_var(tree_node *the_node, var_sym *the_var)
  {
    switch (the_node->kind())
      {
        case TREE_INSTR:
          {
            tree_instr *the_tree_instr = (tree_instr *)the_node;
            return instr_may_reference_var(the_tree_instr->instr(), the_var);
          }
        case TREE_LOOP:
          {
            tree_loop *the_loop = (tree_loop *)the_node;
            return (node_list_may_reference_var(the_loop->body(), the_var) ||
                    node_list_may_reference_var(the_loop->test(), the_var));
          }
        case TREE_FOR:
          {
            tree_for *the_for = (tree_for *)the_node;
            return (the_var->overlaps(the_for->index()) ||
                    operand_may_reference_var(the_for->lb_op(), the_var) ||
                    operand_may_reference_var(the_for->ub_op(), the_var) ||
                    operand_may_reference_var(the_for->step_op(), the_var) ||
                    node_list_may_reference_var(the_for->landing_pad(),
                                                the_var) ||
                    node_list_may_reference_var(the_for->body(), the_var));
          }
        case TREE_IF:
          {
            tree_if *the_if = (tree_if *)the_node;
            return (node_list_may_reference_var(the_if->header(), the_var) ||
                    node_list_may_reference_var(the_if->then_part(),
                                                the_var) ||
                    node_list_may_reference_var(the_if->else_part(), the_var));
          }
        case TREE_BLOCK:
          {
            tree_block *the_block = (tree_block *)the_node;
            return node_list_may_reference_var(the_block->body(), the_var);
          }
        default:
            assert(FALSE);
            return FALSE;
      }
  }

extern boolean node_list_may_reference_var(tree_node_list *node_list,
                                           var_sym *the_var)
  {
    tree_node_list_iter node_iter(node_list);
    while (!node_iter.is_empty())
      {
        tree_node *this_node = node_iter.step();
        if (node_may_reference_var(this_node, the_var))
            return TRUE;
      }

    return FALSE;
  }

extern boolean instr_may_reference_location(instruction *the_instr,
                                            operand address)
  {
    sym_node *base_sym = operand_address_root_symbol(address);
    var_sym *base_var = NULL;
    if ((base_sym != NULL) && base_sym->is_var())
        base_var = (var_sym *)base_sym;
    return instr_may_ref_location_internal(the_instr, address, base_var);
  }

extern boolean operand_may_reference_location(operand the_operand,
                                              operand address)
  {
    sym_node *base_sym = operand_address_root_symbol(address);
    var_sym *base_var = NULL;
    if ((base_sym != NULL) && base_sym->is_var())
        base_var = (var_sym *)base_sym;
    return operand_may_ref_location_internal(the_operand, address, base_var);
  }

extern boolean node_may_reference_location(tree_node *the_node,
                                           operand address)
  {
    switch (the_node->kind())
      {
        case TREE_INSTR:
          {
            tree_instr *the_tree_instr = (tree_instr *)the_node;
            return instr_may_reference_location(the_tree_instr->instr(),
                                                address);
          }
        case TREE_LOOP:
          {
            tree_loop *the_loop = (tree_loop *)the_node;
            return (node_list_may_reference_location(the_loop->body(),
                                                     address) ||
                    node_list_may_reference_location(the_loop->test(),
                                                     address));
          }
        case TREE_FOR:
          {
            tree_for *the_for = (tree_for *)the_node;
            return (operand_may_reference_location(operand(the_for->index()),
                                                   address) ||
                    operand_may_reference_location(the_for->lb_op(),
                                                   address) ||
                    operand_may_reference_location(the_for->ub_op(),
                                                   address) ||
                    operand_may_reference_location(the_for->step_op(),
                                                   address) ||
                    node_list_may_reference_location(the_for->landing_pad(),
                                                     address) ||
                    node_list_may_reference_location(the_for->body(),
                                                     address));
          }
        case TREE_IF:
          {
            tree_if *the_if = (tree_if *)the_node;
            return (node_list_may_reference_location(the_if->header(),
                                                     address) ||
                    node_list_may_reference_location(the_if->then_part(),
                                                     address) ||
                    node_list_may_reference_location(the_if->else_part(),
                                                     address));
          }
        case TREE_BLOCK:
          {
            tree_block *the_block = (tree_block *)the_node;
            return node_list_may_reference_location(the_block->body(),
                                                    address);
          }
        default:
            assert(FALSE);
            return FALSE;
      }
  }

extern boolean node_list_may_reference_location(tree_node_list *node_list,
                                                operand address)
  {
    tree_node_list_iter node_iter(node_list);
    while (!node_iter.is_empty())
      {
        tree_node *this_node = node_iter.step();
        if (node_may_reference_location(this_node, address))
            return TRUE;
      }

    return FALSE;
  }

extern boolean instr_reads_addressed_var(instruction *the_instr)
  {
    unsigned num_srcs = the_instr->num_srcs();
    for (unsigned src_num = 0; src_num < num_srcs; ++src_num)
      {
        if (operand_reads_addressed_var(the_instr->src_op(src_num)))
            return TRUE;
      }
    return FALSE;
  }

extern boolean operand_reads_addressed_var(operand the_operand)
  {
    switch (the_operand.kind())
      {
        case OPER_NULL:
            return FALSE;
        case OPER_SYM:
            return the_operand.symbol()->is_addr_taken();
        case OPER_INSTR:
            return instr_reads_addressed_var(the_operand.instr());
        default:
            assert(FALSE);
            return FALSE;
      }
  }

extern boolean instr_contains_load(instruction *the_instr)
  {
    if (the_instr->opcode() == io_lod)
        return TRUE;

    unsigned num_srcs = the_instr->num_srcs();
    for (unsigned src_num = 0; src_num < num_srcs; ++src_num)
      {
        if (operand_contains_load(the_instr->src_op(src_num)))
            return TRUE;
      }
    return FALSE;
  }

extern boolean operand_contains_load(operand the_operand)
  {
    if (the_operand.is_expr())
        return instr_contains_load(the_operand.instr());
    else
        return FALSE;
  }

extern boolean instr_may_read_global(instruction *the_instr)
  {
    if ((the_instr->opcode() == io_lod) || (the_instr->opcode() == io_cal) ||
        (the_instr->opcode() == io_gen))
      {
        if (instr_is_impure_call(the_instr))
            return TRUE;

        unsigned num_srcs = the_instr->num_srcs();
        for (unsigned src_num = 0; src_num < num_srcs; ++src_num)
          {
            operand this_op = the_instr->src_op(src_num);
            if (this_op.type()->is_ptr())
              {
                sym_node *root_sym = operand_address_root_symbol(this_op);
                if (root_sym == NULL)
                    return TRUE;
                else if (root_sym->is_var() && root_sym->is_global())
                    return TRUE;
              }
          }
      }

    unsigned num_srcs = the_instr->num_srcs();
    for (unsigned src_num = 0; src_num < num_srcs; ++src_num)
      {
        if (operand_may_read_global(the_instr->src_op(src_num)))
            return TRUE;
      }
    return FALSE;
  }

extern boolean operand_may_read_global(operand the_operand)
  {
    if (the_operand.is_symbol())
        return the_operand.symbol()->is_global();
    else if (the_operand.is_expr())
        return instr_may_read_global(the_operand.instr());
    else
        return FALSE;
  }

extern boolean instr_may_read_statically_allocated_var(instruction *the_instr)
  {
    if ((the_instr->opcode() == io_lod) || (the_instr->opcode() == io_cal) ||
        (the_instr->opcode() == io_gen))
      {
        if (instr_is_impure_call(the_instr))
            return TRUE;

        unsigned num_srcs = the_instr->num_srcs();
        for (unsigned src_num = 0; src_num < num_srcs; ++src_num)
          {
            operand this_op = the_instr->src_op(src_num);
            if (this_op.type()->is_ptr())
              {
                sym_node *root_sym = operand_address_root_symbol(this_op);
                if (root_sym == NULL)
                  {
                    return TRUE;
                  }
                else if (root_sym->is_var() &&
                         !(((var_sym *)root_sym)->is_auto()))
                  {
                    return TRUE;
                  }
              }
          }
      }

    unsigned num_srcs = the_instr->num_srcs();
    for (unsigned src_num = 0; src_num < num_srcs; ++src_num)
      {
        if (operand_may_read_statically_allocated_var(the_instr->src_op(
                    src_num)))
          {
            return TRUE;
          }
      }
    return FALSE;
  }

extern boolean operand_may_read_statically_allocated_var(operand the_operand)
  {
    if (the_operand.is_symbol())
        return !(the_operand.symbol()->is_auto());
    else if (the_operand.is_expr())
        return instr_may_read_statically_allocated_var(the_operand.instr());
    else
        return FALSE;
  }

extern boolean instr_uses_scope(instruction *the_instr, base_symtab *scope)
  {
    if (the_instr->opcode() == io_ldc)
      {
        in_ldc *the_ldc = (in_ldc *)the_instr;
        immed value = the_ldc->value();
        if (value.is_symbol() && (value.symbol()->parent() == scope))
            return TRUE;
      }

    unsigned num_srcs = the_instr->num_srcs();
    for (unsigned src_num = 0; src_num < num_srcs; ++src_num)
      {
        if (operand_uses_scope(the_instr->src_op(src_num), scope))
            return TRUE;
      }
    return FALSE;
  }

extern boolean operand_uses_scope(operand the_operand, base_symtab *scope)
  {
    if (the_operand.is_expr())
        return instr_uses_scope(the_operand.instr(), scope);
    else if (the_operand.is_symbol())
        return (the_operand.symbol()->parent() == scope);
    else
        return FALSE;
  }

extern sym_node *instr_address_root_symbol(instruction *the_instr)
  {
    switch (the_instr->opcode())
      {
        case io_ldc:
          {
            in_ldc *the_ldc = (in_ldc *)the_instr;
            immed value = the_ldc->value();
            if (!value.is_symbol())
                return NULL;
            if (value.symbol()->is_var())
              {
                var_sym *the_var = (var_sym *)(value.symbol());
                return the_var->root_ancestor();
              }
            return value.symbol();
          }
        case io_cvt:
        case io_cpy:
          {
            in_rrr *the_rrr = (in_rrr *)the_instr;
            operand src_op = the_rrr->src_op();
            if (!src_op.type()->unqual()->is_ptr())
                return NULL;
            return operand_address_root_symbol(src_op);
          }
        case io_add:
        case io_sub:
          {
            in_rrr *the_rrr = (in_rrr *)the_instr;
            operand src1_op = the_rrr->src1_op();
            operand src2_op = the_rrr->src2_op();
            if ((!src1_op.type()->unqual()->is_ptr()) &&
                (the_instr->opcode() == io_add))
              {
                operand temp_op = src1_op;
                src1_op = src2_op;
                src2_op = temp_op;
              }

            if (src1_op.type()->unqual()->is_ptr() &&
                !src2_op.type()->is_ptr())
              {
                return operand_address_root_symbol(src1_op);
              }
            else
              {
                return NULL;
              }
          }
        case io_array:
          {
            in_array *the_aref = (in_array *)the_instr;
            return operand_address_root_symbol(the_aref->base_op());
          }
        default:
            return NULL;
      }
  }

extern sym_node *operand_address_root_symbol(operand the_operand)
  {
    if (the_operand.is_instr())
        return instr_address_root_symbol(the_operand.instr());
    else
        return NULL;
  }

extern boolean operands_are_same_expr(operand op_1, operand op_2)
  {
    if (op_1 == op_2)
        return TRUE;
    if (op_1.is_expr() && op_2.is_expr())
        return instrs_are_same_expr(op_1.instr(), op_2.instr());
    return FALSE;
  }

extern boolean instrs_are_same_expr(instruction *instr_1,
                                    instruction *instr_2)
  {
    if (instr_1 == instr_2)
        return TRUE;

    if (instr_1->opcode() == io_cpy)
      {
        in_rrr *the_copy = (in_rrr *)instr_1;
        return operands_are_same_expr(the_copy->src_op(), operand(instr_2));
      }

    if (instr_2->opcode() == io_cpy)
      {
        in_rrr *the_copy = (in_rrr *)instr_2;
        return operands_are_same_expr(operand(instr_1), the_copy->src_op());
      }

    if (instr_1->opcode() != instr_2->opcode())
        return FALSE;

    if (instr_1->result_type() != instr_2->result_type())
        return FALSE;

    boolean is_commutative = FALSE;
    switch (instr_1->format())
      {
        case inf_rrr:
          {
            in_rrr *rrr_1 = (in_rrr *)instr_1;
            in_rrr *rrr_2 = (in_rrr *)instr_2;

            if (rrr_1->is_commutative() && rrr_2->is_commutative())
                is_commutative = TRUE;
            break;
          }
        case inf_bj:
          {
            in_bj *bj_1 = (in_bj *)instr_1;
            in_bj *bj_2 = (in_bj *)instr_2;

            if (bj_1->target() != bj_2->target())
                return FALSE;
            break;
          }
        case inf_ldc:
          {
            in_ldc *ldc_1 = (in_ldc *)instr_1;
            in_ldc *ldc_2 = (in_ldc *)instr_2;

            if (ldc_1->value().is_symbol() && ldc_2->value().is_symbol())
              {
                if (root_address(ldc_1->value().addr()) !=
                    root_address(ldc_2->value().addr()))
                  {
                    return FALSE;
                  }
              }
            else
              {
                if (ldc_1->value() != ldc_2->value())
                    return FALSE;
              }
            break;
          }
        case inf_cal:
            break;
        case inf_array:
          {
            in_array *array_1 = (in_array *)instr_1;
            in_array *array_2 = (in_array *)instr_2;

            if (array_1->offset() != array_2->offset())
                return FALSE;
            if (array_1->elem_size() != array_2->elem_size())
                return FALSE;
            if (array_1->dims() != array_2->dims())
                return FALSE;
            break;
          }
        case inf_mbr:
          {
            in_mbr *mbr_1 = (in_mbr *)instr_1;
            in_mbr *mbr_2 = (in_mbr *)instr_2;

            if (mbr_1->default_lab() != mbr_2->default_lab())
                return FALSE;
            if (mbr_1->lower() != mbr_2->lower())
                return FALSE;

            unsigned num_labs = mbr_1->num_labs();
            if (num_labs != mbr_2->num_labs())
                return FALSE;
            for (unsigned lab_num = 0; lab_num < num_labs; ++lab_num)
              {
                if (mbr_1->label(lab_num) != mbr_2->label(lab_num))
                    return FALSE;
              }
            break;
          }
        case inf_lab:
          {
            in_lab *lab_1 = (in_lab *)instr_1;
            in_lab *lab_2 = (in_lab *)instr_2;

            if (lab_1->label() != lab_2->label())
                return FALSE;
            break;
          }
        case inf_gen:
          {
            in_gen *gen_1 = (in_gen *)instr_1;
            in_gen *gen_2 = (in_gen *)instr_2;

            if (strcmp(gen_1->name(), gen_2->name()) != 0)
                return FALSE;
            break;
          }
        default:
            return FALSE;
      }

    unsigned num_srcs = instr_1->num_srcs();
    if (num_srcs != instr_2->num_srcs())
        return FALSE;
    boolean operands_same = TRUE;
    for (unsigned src_num = 0; src_num < num_srcs; ++src_num)
      {
        if (!operands_are_same_expr(instr_1->src_op(src_num),
                                    instr_2->src_op(src_num)))
          {
            operands_same = FALSE;
            break;
          }
      }
    if ((!operands_same) && is_commutative && (num_srcs == 2))
      {
        if (operands_are_same_expr(instr_1->src_op(0), instr_2->src_op(1)) &&
            operands_are_same_expr(instr_1->src_op(1), instr_2->src_op(0)))
          {
            operands_same = TRUE;
          }
      }
    if (!operands_same)
        return FALSE;
    return TRUE;
  }

extern boolean instr_reevaluation_ok(instruction *the_instr)
  {
    if (instr_is_impure_call(the_instr))
        return FALSE;

    switch (the_instr->opcode())
      {
        case io_lod:
          {
            in_rrr *the_load = (in_rrr *)the_instr;
            type_node *src_addr_type = the_load->src_addr_op().type();
            assert(src_addr_type->is_ptr());
            ptr_type *src_ptr = (ptr_type *)src_addr_type;
            if (src_ptr->ref_type()->is_volatile())
                return FALSE;
            break;
          }
        case io_gen:
            return FALSE;
        default:
            break;
      }

    unsigned num_srcs = the_instr->num_srcs();
    for (unsigned src_num = 0; src_num < num_srcs; ++src_num)
      {
        if (!operand_reevaluation_ok(the_instr->src_op(src_num)))
            return FALSE;
      }
    return TRUE;
  }

extern boolean operand_reevaluation_ok(operand the_operand)
  {
    if (the_operand.is_symbol())
        return (!the_operand.symbol()->type()->is_volatile());
    else if (the_operand.is_expr())
        return instr_reevaluation_ok(the_operand.instr());
    else
        return TRUE;
  }

extern boolean instr_contains_impure_call(instruction *the_instr)
  {
    if (instr_is_impure_call(the_instr))
        return TRUE;

    unsigned num_srcs = the_instr->num_srcs();
    for (unsigned src_num = 0; src_num < num_srcs; ++src_num)
      {
        if (operand_contains_impure_call(the_instr->src_op(src_num)))
            return TRUE;
      }
    return FALSE;
  }

extern boolean operand_contains_impure_call(operand the_operand)
  {
    if (the_operand.is_expr())
        return instr_contains_impure_call(the_operand.instr());
    return FALSE;
  }

extern boolean instr_is_impure_call(instruction *the_instr)
  {
    if (the_instr->opcode() != io_cal)
        return FALSE;
    in_cal *the_call = (in_cal *)the_instr;

    proc_sym *the_proc_sym = proc_for_call(the_call);
    if (the_proc_sym == NULL)
        return TRUE;

    return (the_proc_sym->peek_annote(k_pure_function) == NULL);
  }

extern proc_sym *proc_for_call(in_cal *the_call)
  {
    operand function_address = the_call->addr_op();
    if (!function_address.is_expr())
        return NULL;
    instruction *addr_instr = function_address.instr();

    if (addr_instr->opcode() != io_ldc)
        return NULL;
    in_ldc *addr_ldc = (in_ldc *)addr_instr;

    immed addr_value = addr_ldc->value();
    if (!addr_value.is_symbol())
        return NULL;
    sym_node *addr_sym = addr_value.symbol();

    if (!addr_sym->is_proc())
        return NULL;
    return (proc_sym *)addr_sym;
  }

extern sym_addr root_address(sym_addr old_addr)
  {
    if (old_addr.symbol()->is_var())
      {
        var_sym *old_var = (var_sym *)(old_addr.symbol());
        return sym_addr(old_var->root_ancestor(),
                        old_addr.offset() + old_var->root_offset());
      }
    else
      {
        return old_addr;
      }
  }

extern boolean locations_dont_overlap(operand addr1, operand addr2)
  {
    sym_node *root_sym1 = operand_address_root_symbol(addr1);
    sym_node *root_sym2 = operand_address_root_symbol(addr2);
    if ((root_sym1 != NULL) && (root_sym2 != NULL) && (root_sym1 != root_sym2))
        return TRUE;

    type_node *addr_type1 = addr1.type()->unqual();
    type_node *addr_type2 = addr2.type()->unqual();
    assert(addr_type1->is_ptr());
    assert(addr_type2->is_ptr());
    ptr_type *ptr_type1 = (ptr_type *)addr_type1;
    ptr_type *ptr_type2 = (ptr_type *)addr_type2;

    int min_separation = min_addr_separation(addr1, addr2);
    if ((min_separation > 0) &&
        (min_separation >= ptr_type1->ref_type()->size()) &&
        (min_separation >= ptr_type2->ref_type()->size()))
      {
        return TRUE;
      }

    return FALSE;
  }

extern int min_addr_separation(operand addr1, operand addr2)
  {
    if (!addr1.is_expr())
        return 0;
    if (!addr2.is_expr())
        return 0;

    instruction *instr1 = addr1.instr();
    instruction *instr2 = addr2.instr();

    while ((instr1->opcode() == io_cpy) || (instr1->opcode() == io_cvt))
      {
        in_rrr *the_rrr = (in_rrr *)instr1;
        operand src_op = the_rrr->src_op();
        if (!src_op.type()->unqual()->is_ptr())
            return 0;
        if (!src_op.is_expr())
            return 0;
        instr1 = src_op.instr();
      }

    while ((instr2->opcode() == io_cpy) || (instr2->opcode() == io_cvt))
      {
        in_rrr *the_rrr = (in_rrr *)instr2;
        operand src_op = the_rrr->src_op();
        if (!src_op.type()->unqual()->is_ptr())
            return 0;
        if (!src_op.is_expr())
            return 0;
        instr2 = src_op.instr();
      }

    if ((instr1->opcode() != io_array) || (instr2->opcode() != io_array))
        return 0;
    in_array *array1 = (in_array *)instr1;
    in_array *array2 = (in_array *)instr2;
    operand base1 = array1->base_op();
    operand base2 = array2->base_op();

    if (!base1.type()->unqual()->is_same(base2.type()->unqual()))
        return 0;
    if (!operands_are_same_expr(base1, base2))
        return 0;
    if (array1->elem_size() != array2->elem_size())
        return 0;

    unsigned num_dims = array1->dims();
    if (num_dims != array2->dims())
        return 0;
    for (unsigned dim_num = 0; dim_num < num_dims; ++dim_num)
      {
        immed diff;
        boolean const_diff =
                constant_difference(array1->index(dim_num),
                                    array2->index(dim_num), &diff);
        if (const_diff && diff.is_int_const())
          {
            i_integer diff_int = immed_to_ii(diff);
            if (diff_int != 0)
              {
                if (diff_int < 0)
                    diff_int = -diff_int;
                diff_int *= array1->elem_size();
                i_integer offset_diff =
                        i_integer(array1->offset()) - array2->offset();
                if (offset_diff < 0)
                    offset_diff = - offset_diff;
                if ((diff_int > offset_diff) &&
                    (diff_int - offset_diff).is_c_int())
                  {
                    return (diff_int - offset_diff).c_int();
                  }
              }
          }
      }
    return 0;
  }

extern boolean constant_difference(operand op1, operand op2, immed *diff)
  {
    if (!op1.type()->unqual()->is_same(op2.type()->unqual()))
        return FALSE;

    immed immed1;
    eval_status status1 = evaluate_const_expr(op1, &immed1);
    if (status1 != EVAL_OK)
        return FALSE;

    immed immed2;
    eval_status status2 = evaluate_const_expr(op2, &immed2);
    if (status2 != EVAL_OK)
        return FALSE;

    immed result;
    eval_status diff_status =
            calc_real_2op_rrr(io_sub, &result, op1.type()->unqual(), immed1,
                              immed2);
    if (diff_status != EVAL_OK)
        return FALSE;

    *diff = result;
    return TRUE;
  }

extern boolean op_divisible_by(operand the_op, i_integer divisor)
  {
    if ((divisor == 1) || (divisor == -1))
        return TRUE;

    if (the_op.is_expr())
      {
        instruction *the_instr = the_op.instr();
        switch (the_instr->opcode())
          {
            case io_cpy:
            case io_neg:
            case io_abs:
              {
                in_rrr *the_rrr = (in_rrr *)the_instr;
                return op_divisible_by(the_rrr->src_op(), divisor);
              }
            case io_add:
            case io_sub:
              {
                in_rrr *the_rrr = (in_rrr *)the_instr;
                return (op_divisible_by(the_rrr->src1_op(), divisor) &&
                        op_divisible_by(the_rrr->src2_op(), divisor));
              }
            case io_mul:
              {
                in_rrr *the_mul = (in_rrr *)the_instr;
                return (op_divisible_by(the_mul->src1_op(), divisor) ||
                        op_divisible_by(the_mul->src2_op(), divisor));
              }
            case io_cvt:
              {
                in_rrr *the_cvt = (in_rrr *)the_instr;
                operand src_op = the_cvt->src_op();
                type_node *src_type = src_op.type();
                type_node *dst_type = the_instr->result_type();
                if (((src_type->op() == TYPE_INT) ||
                     (src_type->op() == TYPE_ENUM)) &&
                    ((dst_type->op() == TYPE_INT) ||
                     (dst_type->op() == TYPE_ENUM)) &&
                    immed_fits(ii_to_immed(divisor), src_type))
                  {
                    base_type *src_base = (base_type *)src_type;
                    base_type *dst_base = (base_type *)dst_type;
                    if (src_base->is_signed())
                      {
                        if (dst_base->is_signed() &&
                            (src_base->size() <= dst_base->size()))
                          {
                            return op_divisible_by(src_op, divisor);
                          }
                      }
                    else
                      {
                        if (dst_base->is_signed())
                          {
                            if (src_base->size() < dst_base->size())
                                return op_divisible_by(src_op, divisor);
                          }
                        else
                          {
                            if (src_base->size() <= dst_base->size())
                                return op_divisible_by(src_op, divisor);
                          }
                      }
                  }
                break;
              }
            case io_ldc:
              {
                in_ldc *the_ldc = (in_ldc *)the_instr;
                immed value = the_ldc->value();
                if (value.is_int_const())
                    return ((immed_to_ii(value) % divisor) == 0);
                break;
              }
            default:
                break;
          }
      }

    return FALSE;
  }

extern operand coefficient(operand the_op, var_sym *the_var)
  {
    type_node *type_out = diff_type(the_op.type());
    switch (the_op.kind())
      {
        case OPER_NULL:
            return the_op;
        case OPER_SYM:
          {
            var_sym *test_var = the_op.symbol();
            if (test_var == the_var)
                return const_op(immed(1), type_out);
            else
                return const_op(immed(0), type_out);
          }
        case OPER_INSTR:
          {
            if (!the_op.is_expr())
                return const_op(immed(0), type_out);
            instruction *the_instr = the_op.instr();

            switch (the_instr->opcode())
              {
                case io_cpy:
                case io_cvt:
                  {
                    in_rrr *the_rrr = (in_rrr *)the_instr;
                    return cast_op(coefficient(the_rrr->src_op(), the_var),
                                   type_out);
                  }
                case io_add:
                case io_sub:
                  {
                    in_rrr *the_rrr = (in_rrr *)the_instr;
                    return fold_real_2op_rrr(the_rrr->opcode(), type_out,
                                             coefficient(the_rrr->src1_op(),
                                                         the_var),
                                             coefficient(the_rrr->src2_op(),
                                                         the_var));
                  }
                case io_neg:
                  {
                    in_rrr *the_rrr = (in_rrr *)the_instr;
                    return fold_neg(coefficient(the_rrr->src_op(), the_var));
                  }
                case io_mul:
                  {
                    in_rrr *the_rrr = (in_rrr *)the_instr;
                    operand op1 = coefficient(the_rrr->src1_op(), the_var);
                    operand op2 = coefficient(the_rrr->src2_op(), the_var);
                    if (matches_const(op1, immed(0)))
                      {
                        kill_op(op1);
                        return fold_mul(the_rrr->src1_op().clone(), op2);
                      }
                    else
                      {
                        assert(matches_const(op2, immed(0)));
                        kill_op(op2);
                        return fold_mul(op1, the_rrr->src2_op().clone());
                      }
                  }
                case io_array:
                    /* @@@ */
                default:
                    assert(!instr_references_var(the_instr, the_var));
                    return const_op(immed(0), type_out);
              }
          }
        default:
            assert(FALSE);
            return operand();
      }
  }

extern operand const_op(immed the_const, type_node *the_type)
  {
    type_node *intermediate_type = NULL;
    switch (the_const.kind())
      {
        case im_int:
        case im_extended_int:
            switch (the_type->op())
              {
                case TYPE_INT:
                    break;
                case TYPE_FLOAT:
                    intermediate_type = type_signed;
                    break;
                case TYPE_PTR:
                    break;
                case TYPE_ENUM:
                    break;
                default:
                    assert(FALSE);
                    break;
              }
            break;
        case im_float:
        case im_extended_float:
            switch (the_type->op())
              {
                case TYPE_INT:
                    intermediate_type = type_double;
                    break;
                case TYPE_FLOAT:
                    break;
                case TYPE_PTR:
                    intermediate_type = type_double;
                    break;
                case TYPE_ENUM:
                    intermediate_type = type_double;
                    break;
                default:
                    assert(FALSE);
                    break;
              }
            break;
        case im_symbol:
            switch (the_type->op())
              {
                case TYPE_INT:
                    intermediate_type = type_ptr;
                    break;
                case TYPE_FLOAT:
                    intermediate_type = type_ptr;
                    break;
                case TYPE_PTR:
                    break;
                case TYPE_ENUM:
                    intermediate_type = type_ptr;
                    break;
                default:
                    assert(FALSE);
                    break;
              }
            break;
        default:
            assert(FALSE);
            break;
      }
    if (intermediate_type == NULL)
        return operand(new in_ldc(the_type, operand(), the_const));
    in_ldc *new_ldc = new in_ldc(intermediate_type, operand(), the_const);
    return operand(new in_rrr(io_cvt, the_type, operand(), operand(new_ldc)));
  }

extern operand addr_op(sym_node *the_sym)
  {
    return const_op(immed(the_sym), addr_type(the_sym));
  }

extern boolean addr_might_be_stored(var_sym *the_var)
  {
    return (the_var->is_addr_taken() &&
            (the_var->annotes()->peek_annote(k_addr_not_stored) == NULL));
  }

extern operand get_address(operand the_op)
  {
    if (the_op.is_symbol())
      {
        return addr_op(the_op.symbol());
      }
    else if (the_op.is_expr())
      {
        instruction *the_instr = the_op.instr();
        if (the_instr->opcode() == io_lod)
          {
            in_rrr *this_load = (in_rrr *)the_instr;
            return this_load->src_addr_op().clone();
          }
      }
    return operand();
  }

extern boolean no_effects(tree_node_list *node_list)
  {
    tree_node_list_iter node_iter(node_list);
    while (!node_iter.is_empty())
      {
        tree_node *this_node = node_iter.step();
        if (!this_node->is_instr())
            return FALSE;
        tree_instr *this_tree_instr = (tree_instr *)this_node;
        instruction *this_instr = this_tree_instr->instr();
        switch (this_instr->opcode())
          {
            case io_mrk:
            case io_nop:
                break;
            default:
                return FALSE;
          }
      }

    return TRUE;
  }

extern tree_node *single_effect(tree_node_list *node_list)
  {
    tree_node *result = NULL;

    tree_node_list_iter node_iter(node_list);
    while (!node_iter.is_empty())
      {
        tree_node *this_node = node_iter.step();
        if (!this_node->is_instr())
          {
            if (result != NULL)
                return NULL;
            result = this_node;
            continue;
          }
        tree_instr *this_tree_instr = (tree_instr *)this_node;
        instruction *this_instr = this_tree_instr->instr();
        switch (this_instr->opcode())
          {
            case io_mrk:
            case io_nop:
                break;
            default:
              {
                if (result != NULL)
                    return NULL;
                result = this_node;
                break;
              }
          }
      }

    return result;
  }

/*
 *  Note that the next two functions are implemented so that that if
 *  the step is either the integer constant 1 or -1, the expression
 *  will reduce to the upper bound alone, or the upper bound plus or
 *  minus the integer constant one, since the routines used to build
 *  the expressions optimize away mods or divides by plus or minus
 *  one.
 */
extern operand final_index_value(tree_for *the_for)
  {
    operand ub_op = the_for->ub_op().clone();
    operand step1 = the_for->step_op().clone();
    operand step2 = step1.clone();
    operand diff = ub_op.clone() - the_for->lb_op().clone();
    switch (the_for->test())
      {
        case FOR_SGT:
        case FOR_UGT:
            return ub_op + (((-(diff + 1)) % (-step1)) + step2 + 1);
        case FOR_SGTE:
        case FOR_UGTE:
            return ub_op + (((-diff) % (-step1)) + step2);
        case FOR_SLT:
        case FOR_ULT:
            return ub_op + ((-((diff - 1) % step1)) + step2 - 1);
        case FOR_SLTE:
        case FOR_ULTE:
            return ub_op + (-(diff % step1)) + step2;
        default:
            assert(FALSE);
            return operand();
      }
  }

extern operand iteration_count(tree_for *the_for)
  {
    operand step = the_for->step_op().clone();
    operand diff = the_for->ub_op().clone() - the_for->lb_op().clone();
    switch (the_for->test())
      {
        case FOR_SGT:
        case FOR_UGT:
            return (((-(diff + 1))) / (-step)) + 1;
        case FOR_SGTE:
        case FOR_UGTE:
            return ((-diff) / (-step)) + 1;
        case FOR_SLT:
        case FOR_ULT:
            return ((diff - 1) / step) + 1;
        case FOR_SLTE:
        case FOR_ULTE:
            return (diff / step) + 1;
        default:
            assert(FALSE);
            return operand();
      }
  }

extern boolean potential_break(tree_for *the_for)
  {
    if (the_for->peek_annote(k_no_break) != NULL)
        return FALSE;
    if (the_for->peek_annote(k_potential_break) != NULL)
        return TRUE;
    so_walker the_walker;
    the_walker.set_data(0, the_for->brklab());
    boolean result =
            the_walker.walk(the_for, &find_label_uses_on_object, FALSE).si;
    if (result)
        the_for->append_annote(k_potential_break);
    else
        the_for->append_annote(k_no_break);
    return result;
  }

extern boolean annotes_scope_ok(suif_object *the_object, base_symtab *scope)
  {
    annote_list_iter annote_iter(the_object->annotes());
    while (!annote_iter.is_empty())
      {
        annote *this_annote = annote_iter.step();
        immed_list *these_immeds = this_annote->immeds();
        immed_list_iter data_iter(these_immeds);
        while (!data_iter.is_empty())
          {
            immed this_immed = data_iter.step();
            switch (this_immed.kind())
              {
                case im_symbol:
                    if (!scope->is_visible(this_immed.symbol()))
                        return FALSE;
                    break;
                case im_type:
                    if (!scope->is_visible(this_immed.type()))
                        return FALSE;
                    break;
                case im_op:
                    if (!op_scope_ok(this_immed.op(), scope))
                        return FALSE;
                    break;
                case im_instr:
                    if (!instr_scope_ok(this_immed.instr(), scope))
                        return FALSE;
                    break;
                case im_int:
                case im_extended_int:
                case im_string:
                case im_float:
                case im_extended_float:
                case im_undef:
                    break;
                default:
                    assert(FALSE);
              }
          }
      }
    return TRUE;
  }

extern boolean op_scope_ok(operand the_op, base_symtab *scope)
  {
    switch (the_op.kind())
      {
        case OPER_NULL:
            return TRUE;
        case OPER_SYM:
            return scope->is_visible(the_op.symbol());
        case OPER_INSTR:
            return instr_scope_ok(the_op.instr(), scope);
        default:
            assert(FALSE);
            return FALSE;
      }
  }

extern boolean instr_scope_ok(instruction *the_instr, base_symtab *scope)
  {
    if (the_instr->opcode() == io_ldc)
      {
        in_ldc *the_ldc = (in_ldc *)the_instr;
        immed value = the_ldc->value();
        if (value.is_symbol() && (!scope->is_visible(value.symbol())))
            return FALSE;
      }

    if (!the_instr->dst_op().is_instr())
      {
        if (!op_scope_ok(the_instr->dst_op(), scope))
            return FALSE;
      }

    if (!scope->is_visible(the_instr->result_type()))
        return FALSE;

    unsigned num_srcs = the_instr->num_srcs();
    for (unsigned src_num = 0; src_num < num_srcs; ++src_num)
      {
        if (!op_scope_ok(the_instr->src_op(src_num), scope))
            return FALSE;
      }

    return TRUE;
  }

extern const char *deconflict_sym_name(const char *original_name, base_symtab *the_symtab,
                                 sym_kinds the_kind)
  {
    if (the_symtab->lookup_sym(original_name, the_kind) == NULL)
        return original_name;
    size_t original_length = strlen(original_name);
    char *name_buf =
            new char[original_length + sizeof(unsigned long) * CHAR_BIT];
    strcpy(name_buf, original_name);
    char *name_place = &(name_buf[original_length]);
    unsigned long suffix_num = 0;
    while (TRUE)
      {
        sprintf(name_place, "%lu", suffix_num);
        if (the_symtab->lookup_sym(name_buf,  the_kind) == NULL)
          {
            const char *result = lexicon->enter(name_buf)->sp;
            delete[] name_buf;
            return result;
          }
        if (suffix_num == ULONG_MAX)
            error_line(1, NULL, "overflow in suffix attempt");
        ++suffix_num;
      }
  }

static void merge_sort_fields(field_struct *start_array,
                              field_struct *end_array,
                              field_struct *work_array, unsigned num_fields)
  {
    if (num_fields == 1)
      {
        end_array[0] = start_array[0];
        return;
      }
    else if (num_fields == 2)
      {
        if (start_array[0].offset > start_array[1].offset)
          {
            end_array[0] = start_array[1];
            end_array[1] = start_array[0];
          }
        else
          {
            end_array[0] = start_array[0];
            end_array[1] = start_array[1];
          }
        return;
      }

    unsigned first_chunk_size = num_fields / 2;
    merge_sort_fields(start_array, work_array, end_array, first_chunk_size);
    merge_sort_fields(&(start_array[first_chunk_size]),
                      &(work_array[first_chunk_size]),
                      &(end_array[first_chunk_size]),
                      num_fields - first_chunk_size);

    unsigned first_position = 0;
    unsigned second_position = first_chunk_size;
    unsigned end_position = 0;
    while (TRUE)
      {
        if (work_array[first_position].offset >
            work_array[second_position].offset)
          {
            end_array[end_position] = work_array[second_position];
            ++end_position;
            ++second_position;
            if (second_position >= num_fields)
              {
                do
                  {
                    end_array[end_position] = work_array[first_position];
                    ++end_position;
                    ++first_position;
                  } while (first_position < first_chunk_size);
                break;
              }
          }
        else
          {
            end_array[end_position] = work_array[first_position];
            ++end_position;
            ++first_position;
            if (first_position >= first_chunk_size)
              {
                do
                  {
                    end_array[end_position] = work_array[second_position];
                    ++end_position;
                    ++second_position;
                  } while (second_position < num_fields);
                break;
              }
          }
      }

    assert(end_position == num_fields);
  }

static boolean instr_may_ref_location_internal(instruction *the_instr,
                                               operand address,
                                               var_sym *base_var)
  {
    if (instr_is_impure_call(the_instr) || (the_instr->opcode() == io_gen))
      {
        if ((base_var == NULL) || base_var->is_addr_taken() ||
            (!(base_var->is_auto())))
          {
            return TRUE;
          }
      }

    if (((base_var == NULL) || base_var->is_addr_taken()) &&
        ((the_instr->opcode() == io_cal) || (the_instr->opcode() == io_lod) ||
         (the_instr->opcode() == io_memcpy) ||
         (the_instr->opcode() == io_gen)))
      {
        unsigned num_srcs = the_instr->num_srcs();
        unsigned src_num = 0;
        if (the_instr->opcode() == io_memcpy)
            src_num = 1;
        for (; src_num < num_srcs; ++src_num)
          {
            operand this_op = the_instr->src_op(src_num);
            if (this_op.type()->unqual()->is_ptr())
              {
                if (base_var == NULL)
                  {
                    if (!locations_dont_overlap(address, this_op))
                        return TRUE;
                  }
                else
                  {
                    sym_node *addr_root_symbol =
                            operand_address_root_symbol(this_op);
                    if ((addr_root_symbol == NULL) ||
                        (base_var->root_ancestor() == addr_root_symbol))
                      {
                        if (!locations_dont_overlap(address, this_op))
                            return TRUE;
                      }
                  }
              }
          }
      }

    unsigned num_srcs = the_instr->num_srcs();
    for (unsigned src_num = 0; src_num < num_srcs; ++src_num)
      {
        if (operand_may_ref_location_internal(the_instr->src_op(src_num),
                                              address, base_var))
          {
            return TRUE;
          }
      }
    return FALSE;
  }

static boolean operand_may_ref_location_internal(operand the_operand,
                                                 operand address,
                                                 var_sym *base_var)
  {
    switch (the_operand.kind())
      {
        case OPER_NULL:
            return FALSE;
        case OPER_SYM:
          {
            if (base_var != NULL)
                return base_var->overlaps(the_operand.symbol());
            else
                return the_operand.symbol()->is_addr_taken();
          }
        case OPER_INSTR:
            return instr_may_ref_location_internal(the_operand.instr(),
                                                   address, base_var);
        default:
            assert(FALSE);
            return FALSE;
      }
  }

static void find_label_uses_on_object(suif_object *the_object,
                                      so_walker *the_walker)
  {
    if (!the_object->is_instr_obj())
        return;
    instruction *the_instr = (instruction *)the_object;
    label_sym *the_label = (label_sym *)(the_walker->get_data(0).ptr);
    switch (the_instr->opcode())
      {
        case io_btrue:
        case io_bfalse:
        case io_jmp:
          {
            in_bj *the_bj = (in_bj *)the_instr;
            if (the_bj->target() == the_label)
              {
                the_walker->set_result(TRUE);
                the_walker->set_break();
              }
            break;
          }
        case io_mbr:
          {
            in_mbr *the_mbr = (in_mbr *)the_instr;
            if (the_mbr->default_lab() == the_label)
              {
                the_walker->set_result(TRUE);
                the_walker->set_break();
                return;
              }
            unsigned num_labs = the_mbr->num_labs();
            for (unsigned lab_num = 0; lab_num < num_labs; ++lab_num)
              {
                if (the_mbr->label(lab_num) == the_label)
                  {
                    the_walker->set_result(TRUE);
                    the_walker->set_break();
                    return;
                  }
              }
            break;
          }
        default:
            break;
      }
  }
