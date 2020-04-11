/* file "eval.cc" */

/*  Copyright (c) 1994,95 Stanford University

    All rights reserved.

    This software is provided under the terms described in
    the "suif_copyright.h" include file. */

#include <suif_copyright.h>

/*
 * This is the implementation of the constant folding and expression
 * evaluation routines for the SUIF ``useful'' library of
 * miscellaneous useful routines.
 */

#define _MODULE_ "libuseful.a"

#pragma implementation "basic.h"

#define RCS_BASE_FILE eval_cc

#include "useful_internal.h"
#include <climits>

RCS_BASE(
    "$Id$")

/*----------------------------------------------------------------------*
    Begin Documentation
 *----------------------------------------------------------------------*

                Summary
                -------

        This file is a place to put general expression evaluation and
        constant folding routines for SUIF.


 *----------------------------------------------------------------------*
    End Documentation
 *----------------------------------------------------------------------*/

boolean fold_had_effect = FALSE;
boolean overflow_in_folding = FALSE;
boolean suppress_array_folding = TRUE;

struct summand
  {
    boolean is_negated;
    operand the_op;

    summand& operator=(summand &other)
      {
        is_negated = other.is_negated;
        the_op = other.the_op;
        return other;
      }
    boolean operator==(const summand &) const  { return FALSE; }
  };

DECLARE_DLIST_CLASS(summand_list, summand);

/*
 *  This is either the place to put code that needs to be evaluated
 *  for side-effects only (for example, if the result of a function
 *  call is multiplied by zero, the function call will be put here),
 *  or NULL if there is noplace to put such side-effect code.
 */
static tree_node *spill_node = NULL;

static boolean immed_is_zero(immed the_immed);
static boolean fold_constants_internal(instruction *the_instr,
                                       operand *replacement,
                                       boolean recursively);
static instruction *spill_side_effects(operand the_operand);
static operand new_zero_op(type_node *the_type);
static summand_list *sum_form(operand the_op);
static void cancel_in_sum(summand_list *the_sum);
static void negate_sum(summand_list *the_sum);
static void merge_sum_consts(summand_list *the_sum);
static void free_sum(summand_list *the_sum);
static operand push_division_down(operand the_op, i_integer divisor);
static operand int_op(type_node *the_type, i_integer the_int);
static boolean same_modulo_call_by_ref(type_node *type1, type_node *type2);

extern immed ii_to_immed(const i_integer &the_ii)
  {
    static char *buffer = NULL;
    static unsigned long buffer_size = 0;

    if (the_ii.is_c_int())
        return immed(the_ii.c_int());

    i_integer ii_length = the_ii.written_length();
    if (!ii_length.is_c_unsigned_long())
        error_line(1, NULL, "out of memory address space");
    unsigned long ulong_length = ii_length.c_unsigned_long();
    if (ulong_length == ULONG_MAX)
        error_line(1, NULL, "out of memory address space");
    ++ulong_length;
    if (ulong_length > buffer_size)
      {
        if (buffer != NULL)
            delete[] buffer;
        buffer_size = ulong_length;
        buffer = new char[buffer_size];
      }
    the_ii.write(buffer);
    return immed(im_extended_int, buffer);
  }

extern i_integer immed_to_ii(const immed &the_immed)
  {
    switch (the_immed.kind())
      {
        case im_int:
            return i_integer(the_immed.integer());
        case im_extended_int:
            return i_integer(the_immed.ext_integer());
        default:
            assert(FALSE);
            return i_integer();
      }
  }

extern eval_status evaluate_const_expr(operand the_operand, immed *result)
  {
    switch (the_operand.kind())
      {
        case OPER_NULL:
            return EVAL_UNDEFINED;
        case OPER_SYM:
            return EVAL_NOT_CONST;
        case OPER_INSTR:
            break;
        default:
            assert(FALSE);
      }

    instruction *the_instr = the_operand.instr();
    return evaluate_const_instr(the_instr, result);
  }

extern eval_status evaluate_const_instr(instruction *the_instr, immed *result)
  {
    assert(the_instr != NULL);
    switch (the_instr->format())
      {
        case inf_none:
            return EVAL_ERROR;
        case inf_rrr:
          {
            switch (the_instr->opcode())
              {
                case io_mrk:
                case io_nop:
                case io_ret:
                    return EVAL_ERROR;
                case io_lod:
                case io_str:
                case io_memcpy:
                    return EVAL_NOT_CONST;
                default:
                    break;
              }

            in_rrr *the_rrr = (in_rrr *)the_instr;

            type_node *result_type = the_instr->result_type();
            assert(result_type != NULL);

            eval_status good_result = EVAL_OK;

            immed result1;
            eval_status status =
                    evaluate_const_expr(the_rrr->src1_op(), &result1);
            if (status == EVAL_OVERFLOW)
                good_result = EVAL_OVERFLOW;
            else if (status != EVAL_OK)
                return status;

            if (is_real_2op_rrr(the_instr->opcode()))
              {
                immed result2;
                status = evaluate_const_expr(the_rrr->src2_op(), &result2);
                if (status == EVAL_OVERFLOW)
                    good_result = EVAL_OVERFLOW;
                else if (status != EVAL_OK)
                    return status;

                return calc_real_2op_rrr(the_instr->opcode(), result,
                                         result_type, result1, result2);
              }

            assert(is_real_1op_rrr(the_instr->opcode()));
            return calc_real_1op_rrr(the_instr->opcode(), result, result_type,
                                     result1);
          }
        case inf_bj:
            return EVAL_ERROR;
        case inf_ldc:
          {
            in_ldc *the_ldc = (in_ldc *)the_instr;
            immed value = the_ldc->value();
            if ((!value.is_int_const()) && (!value.is_float_const()) &&
                (!value.is_symbol()))
              {
                return EVAL_ERROR;
              }
            *result = value;
            return EVAL_OK;
          }
        case inf_cal:
            return EVAL_NOT_CONST;
        case inf_array:
          {
            if (suppress_array_folding)
                return EVAL_NOT_CONST;

            in_array *the_array = (in_array *)the_instr;
            eval_status good_result = EVAL_OK;

            immed base_value;
            eval_status status =
                    evaluate_const_expr(the_array->base_op(), &base_value);
            if (status == EVAL_OVERFLOW)
                good_result = EVAL_OVERFLOW;
            else if (status != EVAL_OK)
                return status;

            unsigned num_dims = the_array->dims();
            i_integer elem_num = 0;
            for (unsigned dim_num = 0; dim_num < num_dims; ++dim_num)
              {
                if (dim_num != 0)
                  {
                    immed bound_value;
                    status =
                            evaluate_const_expr(the_array->bound(dim_num),
                                                &bound_value);
                    if (status == EVAL_OVERFLOW)
                        good_result = EVAL_OVERFLOW;
                    else if (status != EVAL_OK)
                        return status;

                    if (!bound_value.is_int_const())
                        return EVAL_ERROR;
                    elem_num *= immed_to_ii(bound_value);
                  }

                immed index_value;
                status =
                        evaluate_const_expr(the_array->index(dim_num),
                                            &index_value);
                if (status == EVAL_OVERFLOW)
                    good_result = EVAL_OVERFLOW;
                else if (status != EVAL_OK)
                    return status;

                if (!index_value.is_int_const())
                    return EVAL_ERROR;
                elem_num += immed_to_ii(index_value);
              }

            if (!the_array->offset_op().is_null())
              {
                immed offset_value;
                status =
                        evaluate_const_expr(the_array->offset_op(),
                                            &offset_value);
                if (status == EVAL_OVERFLOW)
                    good_result = EVAL_OVERFLOW;
                else if (status != EVAL_OK)
                    return status;

                if (!offset_value.is_int_const())
                    return EVAL_ERROR;
                elem_num -= immed_to_ii(offset_value);
              }

            i_integer elem_size = the_array->elem_size();
            elem_size =
                    (elem_size + target.addressable_size - 1) /
                    target.addressable_size;

            i_integer offset = elem_num * elem_size;
            offset += (i_integer(the_array->offset()) /
                       target.addressable_size);

            if (base_value.is_int_const())
              {
                *result = ii_to_immed(immed_to_ii(base_value) + offset);
              }
            else if (base_value.is_symbol())
              {
                i_integer new_bit_offset =
                        offset * target.addressable_size + base_value.offset();
                if (!new_bit_offset.is_c_int())
                  {
                    good_result = EVAL_OVERFLOW;
                    new_bit_offset = 0;
                  }
                *result = immed(base_value.symbol(), new_bit_offset.c_int());
              }
            else
              {
                return EVAL_ERROR;
              }

            return good_result;
          }
        case inf_mbr:
            return EVAL_ERROR;
        case inf_lab:
            return EVAL_ERROR;
        case inf_gen:
            return EVAL_NOT_CONST;
        default:
            assert(FALSE);
      }
    return EVAL_ERROR;
  }

extern eval_status evaluate_const_int_expr(operand the_operand, int *result)
  {
    immed immed_value;
    eval_status status = evaluate_const_expr(the_operand, &immed_value);
    if ((status != EVAL_OK) && (status != EVAL_OVERFLOW))
        return status;
    if (!immed_value.is_integer())
        return EVAL_UNKNOWN_AT_LINK;
    *result = immed_value.integer();
    return status;
  }

extern eval_status evaluate_const_int_instr(instruction *the_instr,
                                            int *result)
  {
    immed immed_value;
    eval_status status = evaluate_const_instr(the_instr, &immed_value);
    if ((status != EVAL_OK) && (status != EVAL_OVERFLOW))
        return status;
    if (!immed_value.is_integer())
        return EVAL_UNKNOWN_AT_LINK;
    *result = immed_value.integer();
    return status;
  }

extern eval_status fit_immed(immed *value, type_node *the_type)
  {
    type_node *base_type = the_type;

    switch (base_type->op())
      {
        case TYPE_INT:
        case TYPE_ENUM:
        case TYPE_PTR:
            if (value->is_int_const())
              {
                int size = base_type->size();
                i_integer mask = i_integer(-1) << size;
                i_integer new_value = immed_to_ii(*value);
                if (non_negative(base_type))
                  {
                    if ((new_value & mask) != 0)
                      {
                        new_value = new_value & ~mask;
                        *value = ii_to_immed(new_value);
                        return EVAL_OVERFLOW;
                      }
                  }
                else
                  {
                    i_integer magnitude_mask = mask >> 1;
                    if (new_value < 0)
                      {
                        if ((new_value & magnitude_mask) != magnitude_mask)
                          {
                            i_integer sign_mask = i_integer(1) << (size - 1);
                            if ((new_value & sign_mask) == 0)
                                new_value = new_value & ~magnitude_mask;
                            else
                                new_value = new_value | magnitude_mask;
                            *value = ii_to_immed(new_value);
                            return EVAL_OVERFLOW;
                          }
                      }
                    else
                      {
                        if ((new_value & magnitude_mask) != 0)
                          {
                            i_integer sign_mask = i_integer(1) << (size - 1);
                            if ((new_value & sign_mask) == 0)
                                new_value = new_value & ~magnitude_mask;
                            else
                                new_value = new_value | magnitude_mask;
                            *value = ii_to_immed(new_value);
                            return EVAL_OVERFLOW;
                          }
                      }
                  }
              }
            return EVAL_OK;
        case TYPE_FLOAT:
            /* Someday we should check the size of result here, but
               right now we can't since we don't know the floating
               point representation of the target machine. */
            return EVAL_OK;
        default:
            return EVAL_ERROR;
      }
  }

extern boolean immed_fits(immed value, type_node *the_type)
  {
    immed test_immed = value;
    return (fit_immed(&test_immed, the_type) == EVAL_OK);
  }

extern boolean operand_is_int_const(operand the_operand, int *result)
  {
    immed value;
    eval_status status = evaluate_const_expr(the_operand, &value);
    if (status != EVAL_OK)
        return FALSE;
    if (!value.is_integer())
        return FALSE;
    *result = value.integer();
    return TRUE;
  }

extern boolean matches_const(operand the_op, immed the_const)
  {
    immed value;
    eval_status status = evaluate_const_expr(the_op, &value);
    if (status != EVAL_OK)
        return FALSE;
    return (value == the_const);
  }

extern boolean is_real_2op_rrr(if_ops opcode)
  {
    switch (opcode)
      {
        case io_add:
        case io_sub:
        case io_mul:
        case io_div:
        case io_rem:
        case io_mod:
        case io_min:
        case io_max:
        case io_and:
        case io_ior:
        case io_xor:
        case io_asr:
        case io_lsl:
        case io_lsr:
        case io_rot:
        case io_seq:
        case io_sne:
        case io_sl:
        case io_sle:
        case io_divfloor:
        case io_divceil:
            return TRUE;
        default:
            return FALSE;
      }
  }

extern boolean is_real_1op_rrr(if_ops opcode)
  {
    switch (opcode)
      {
        case io_cpy:
        case io_neg:
        case io_not:
        case io_abs:
        case io_cvt:
            return TRUE;
        default:
            return FALSE;
      }
  }

extern eval_status calc_real_2op_rrr(if_ops opcode, immed *result_val,
                                     type_node *result_type, immed src1_val,
                                     immed src2_val)
  {
    assert(is_real_2op_rrr(opcode));

    switch (opcode)
      {
        case io_div:
        case io_rem:
        case io_mod:
        case io_divfloor:
        case io_divceil:
	    /* NEW: allow floating point division by zero */
	    if (immed_is_zero(src2_val) && !src2_val.is_flt())
                return EVAL_DIV_BY_ZERO;
        default:
            break;
      }

    eval_status good_result = EVAL_OK;

    switch (opcode)
      {
        case io_add:
            if (src2_val.is_symbol())
              {
                immed temp_immed = src2_val;
                src2_val = src1_val;
                src1_val = temp_immed;
              }
            if (src1_val.is_symbol())
              {
                if (src2_val.is_int_const())
                  {
                    i_integer new_offset =
                            i_integer(src1_val.offset()) +
                            immed_to_ii(src2_val) * target.addressable_size;
                    if (!new_offset.is_c_int())
                      {
                        good_result = EVAL_OVERFLOW;
                        new_offset = 0;
                      }
                    *result_val = immed(src1_val.symbol(), new_offset.c_int());
                  }
                else
                  {
                    return EVAL_ERROR;
                  }
              }
            else if (src1_val.is_int_const() && src2_val.is_int_const())
              {
                *result_val = ii_to_immed(immed_to_ii(src1_val) +
                                          immed_to_ii(src2_val));
              }
            else if (src1_val.is_flt() && src2_val.is_flt())
              {
                double double_src1 = src1_val.flt();
                double double_src2 = src2_val.flt();
                double double_result = double_src1 + double_src2;
                if (((double_src2 < 0) && (double_result > double_src1)) ||
                    ((double_src2 > 0) && (double_result < double_src1)))
                  {
                    good_result = EVAL_OVERFLOW;
                  }
                *result_val = immed(double_result);
              }
            else
              {
                return EVAL_ERROR;
              }
            break;
        case io_sub:
            if (src1_val.is_symbol())
              {
                if (src2_val.is_int_const())
                  {
                    i_integer new_offset =
                            i_integer(src1_val.offset()) -
                            immed_to_ii(src2_val) * target.addressable_size;
                    if (!new_offset.is_c_int())
                      {
                        good_result = EVAL_OVERFLOW;
                        new_offset = 0;
                      }
                    *result_val = immed(src1_val.symbol(), new_offset.c_int());
                  }
                else if (src2_val.is_symbol())
                  {
                    sym_addr addr1 = root_address(src1_val.addr());
                    sym_addr addr2 = root_address(src2_val.addr());

                    if (addr1.symbol() != addr2.symbol())
                        return EVAL_UNKNOWN_AT_LINK;
                    int difference = addr1.offset() - addr2.offset();
                    *result_val = immed(difference / target.addressable_size);
                  }
                else
                  {
                    return EVAL_UNKNOWN_AT_LINK;
                  }
              }
            else if (src2_val.is_symbol())
              {
                return EVAL_UNKNOWN_AT_LINK;
              }
            else if (src1_val.is_int_const() && src2_val.is_int_const())
              {
                *result_val = ii_to_immed(immed_to_ii(src1_val) -
                                          immed_to_ii(src2_val));
              }
            else if (src1_val.is_flt() && src2_val.is_flt())
              {
                double double_src1 = src1_val.flt();
                double double_src2 = src2_val.flt();
                double double_result = double_src1 - double_src2;
                if (((double_src2 > 0) && (double_result > double_src1)) ||
                    ((double_src2 < 0) && (double_result < double_src1)))
                  {
                    good_result = EVAL_OVERFLOW;
                  }
                *result_val = immed(double_result);
              }
            else
              {
                return EVAL_ERROR;
              }
            break;
        case io_mul:
            if (src1_val.is_int_const() && src2_val.is_int_const())
              {
                *result_val = ii_to_immed(immed_to_ii(src1_val) *
                                          immed_to_ii(src2_val));
              }
            else if (src1_val.is_flt() && src2_val.is_flt())
              {
                double double_src1 = src1_val.flt();
                double double_src2 = src2_val.flt();
                double double_result = double_src1 * double_src2;
                *result_val = immed(double_result);
              }
            else
              {
                return EVAL_ERROR;
              }
            break;
        case io_div:
            if (src1_val.is_int_const() && src2_val.is_int_const())
              {
                *result_val = ii_to_immed(immed_to_ii(src1_val) /
                                          immed_to_ii(src2_val));
              }
            else if (src1_val.is_flt() && src2_val.is_flt())
              {
                *result_val = immed(src1_val.flt() / src2_val.flt());
              }
            else
              {
                return EVAL_ERROR;
              }
            break;
        case io_rem:
            if (src1_val.is_int_const() && src2_val.is_int_const())
              {
                *result_val =
                        ii_to_immed(immed_to_ii(src1_val).mod(
                                immed_to_ii(src2_val)));
              }
            else if (src1_val.is_flt() && src2_val.is_flt())
              {
                *result_val = immed((double)0.0);
              }
            else
              {
                return EVAL_ERROR;
              }
            break;
        case io_mod:
            if (src1_val.is_int_const() && src2_val.is_int_const())
              {
                *result_val =
                        ii_to_immed(immed_to_ii(src1_val).mod(
                                immed_to_ii(src2_val)));
              }
            else if (src1_val.is_flt() && src2_val.is_flt())
              {
                *result_val = immed((double)0.0);
              }
            else
              {
                return EVAL_ERROR;
              }
            break;
        case io_min:
            if (src1_val.is_symbol() || src2_val.is_symbol())
              {
                return EVAL_UNKNOWN_AT_LINK;
              }
            else if (src1_val.is_int_const() && src2_val.is_int_const())
              {
                i_integer ii1 = immed_to_ii(src1_val);
                i_integer ii2 = immed_to_ii(src2_val);
                i_integer ii_result;
                if (ii1 >= ii2)
                    ii_result = ii2;
                else
                    ii_result = ii1;
                *result_val = ii_to_immed(ii_result);
              }
            else if (src1_val.is_flt() && src2_val.is_flt())
              {
                double double_src1 = src1_val.flt();
                double double_src2 = src2_val.flt();
                double double_result;
                if (double_src1 >= double_src2)
                    double_result = double_src2;
                else
                    double_result = double_src1;
                *result_val = immed(double_result);
              }
            else
              {
                return EVAL_ERROR;
              }
            break;
        case io_max:
            if (src1_val.is_symbol() || src2_val.is_symbol())
              {
                return EVAL_UNKNOWN_AT_LINK;
              }
            else if (src1_val.is_int_const() && src2_val.is_int_const())
              {
                i_integer ii1 = immed_to_ii(src1_val);
                i_integer ii2 = immed_to_ii(src2_val);
                i_integer ii_result;
                if (ii1 >= ii2)
                    ii_result = ii1;
                else
                    ii_result = ii2;
                *result_val = ii_to_immed(ii_result);
              }
            else if (src1_val.is_flt() && src2_val.is_flt())
              {
                double double_src1 = src1_val.flt();
                double double_src2 = src2_val.flt();
                double double_result;
                if (double_src1 >= double_src2)
                    double_result = double_src1;
                else
                    double_result = double_src2;
                *result_val = immed(double_result);
              }
            else
              {
                return EVAL_ERROR;
              }
            break;
        case io_and:
            if ((!src1_val.is_int_const()) || (!src2_val.is_int_const()))
                return EVAL_ERROR;
            *result_val =
                    ii_to_immed(immed_to_ii(src1_val) & immed_to_ii(src2_val));
            break;
        case io_ior:
            if ((!src1_val.is_int_const()) || (!src2_val.is_int_const()))
                return EVAL_ERROR;
            *result_val =
                    ii_to_immed(immed_to_ii(src1_val) | immed_to_ii(src2_val));
            break;
        case io_xor:
            if ((!src1_val.is_int_const()) || (!src2_val.is_int_const()))
                return EVAL_ERROR;
            *result_val =
                    ii_to_immed(immed_to_ii(src1_val) ^ immed_to_ii(src2_val));
            break;
        case io_asr:
        case io_lsr:
            if ((!src1_val.is_int_const()) || (!src2_val.is_int_const()))
                return EVAL_ERROR;
            if (immed_to_ii(src2_val) < 0)
                return EVAL_ERROR;
            *result_val =
                    ii_to_immed(immed_to_ii(src1_val) >>
                                immed_to_ii(src2_val));
            break;
        case io_lsl:
          {
            if ((!src1_val.is_int_const()) || (!src2_val.is_int_const()))
                return EVAL_ERROR;
            if (immed_to_ii(src2_val) < 0)
                return EVAL_ERROR;
            *result_val =
                    ii_to_immed(immed_to_ii(src1_val) <<
                                immed_to_ii(src2_val));
            break;
          }
        case io_rot:
          {
            if ((!src1_val.is_int_const()) || (!src2_val.is_int_const()))
                return EVAL_ERROR;
            i_integer size = result_type->size();
            if (size == 0)
              {
                *result_val = src1_val;
              }
            else
              {
                i_integer remainder = immed_to_ii(src2_val) % size;
                if (remainder < 0)
                    remainder += size;
                assert((remainder >= 0) && (remainder < size));

                i_integer top_mask =
                        (~(i_integer(-1) << remainder)) << (size - remainder);
                i_integer bottom_mask = ~(i_integer(-1) << (size - remainder));

                i_integer base = immed_to_ii(src1_val);
                i_integer top_part = (top_mask & base) >> (size - remainder);
                i_integer bottom_part = (bottom_mask & base) << remainder;

                i_integer final = top_part | bottom_part;
                *result_val = ii_to_immed(final);
              }
            break;
          }
        case io_seq:
            if (src1_val.is_int_const() && src2_val.is_int_const())
              {
                *result_val =
                        immed((immed_to_ii(src1_val) ==
                               immed_to_ii(src2_val)) ? 1 : 0);
              }
            else if (src1_val.is_flt() && src2_val.is_flt())
              {
                *result_val =
                        immed((src1_val.flt() == src2_val.flt()) ? 1 : 0);
              }
            else if (src1_val.is_symbol())
              {
                if (src2_val.is_symbol())
                  {
                    *result_val =
                            immed((root_address(src1_val.addr()) ==
                                   root_address(src2_val.addr())) ? 1 : 0);
                  }
                else
                  {
                    return EVAL_UNKNOWN_AT_LINK;
                  }
              }
            else if (src2_val.is_symbol())
              {
                return EVAL_UNKNOWN_AT_LINK;
              }
            else
              {
                return EVAL_ERROR;
              }
            break;
        case io_sne:
            if (src1_val.is_int_const() && src2_val.is_int_const())
              {
                *result_val =
                        immed((immed_to_ii(src1_val) !=
                               immed_to_ii(src2_val)) ? 1 : 0);
              }
            else if (src1_val.is_flt() && src2_val.is_flt())
              {
                *result_val =
                        immed((src1_val.flt() != src2_val.flt()) ? 1 : 0);
              }
            else if (src1_val.is_symbol())
              {
                if (src2_val.is_symbol())
                  {
                    *result_val =
                            immed((root_address(src1_val.addr()) !=
                                   root_address(src2_val.addr())) ? 1 : 0);
                  }
                else
                  {
                    return EVAL_UNKNOWN_AT_LINK;
                  }
              }
            else if (src2_val.is_symbol())
              {
                return EVAL_UNKNOWN_AT_LINK;
              }
            else
              {
                return EVAL_ERROR;
              }
            break;
        case io_sl:
            if (src1_val.is_int_const() && src2_val.is_int_const())
              {
                *result_val =
                        immed((immed_to_ii(src1_val) <
                               immed_to_ii(src2_val)) ? 1 : 0);
              }
            else if (src1_val.is_flt() && src2_val.is_flt())
              {
                *result_val = immed((src1_val.flt() < src2_val.flt()) ? 1 : 0);
              }
            else if (src1_val.is_symbol())
              {
                if (src2_val.is_symbol())
                  {
                    sym_addr addr1 = root_address(src1_val.addr());
                    sym_addr addr2 = root_address(src2_val.addr());

                    if (addr1.symbol() == addr2.symbol())
                      {
                        *result_val =
                                immed((addr1.offset() < addr2.offset()) ?
                                      1 : 0);
                      }
                    else
                      {
                        return EVAL_UNKNOWN_AT_LINK;
                      }
                  }
                else
                  {
                    return EVAL_UNKNOWN_AT_LINK;
                  }
              }
            else if (src2_val.is_symbol())
              {
                return EVAL_UNKNOWN_AT_LINK;
              }
            else
              {
                return EVAL_ERROR;
              }
            break;
        case io_sle:
            if (src1_val.is_int_const() && src2_val.is_int_const())
              {
                *result_val =
                        immed((immed_to_ii(src1_val) <=
                               immed_to_ii(src2_val)) ? 1 : 0);
              }
            else if (src1_val.is_flt() && src2_val.is_flt())
              {
                *result_val =
                        immed((src1_val.flt() <= src2_val.flt()) ? 1 : 0);
              }
            else if (src1_val.is_symbol())
              {
                if (src2_val.is_symbol())
                  {
                    sym_addr addr1 = root_address(src1_val.addr());
                    sym_addr addr2 = root_address(src2_val.addr());

                    if (addr1.symbol() == addr2.symbol())
                      {
                        *result_val =
                                immed((addr1.offset() <= addr2.offset()) ?
                                      1 : 0);
                      }
                    else
                      {
                        return EVAL_UNKNOWN_AT_LINK;
                      }
                  }
                else
                  {
                    return EVAL_UNKNOWN_AT_LINK;
                  }
              }
            else if (src2_val.is_symbol())
              {
                return EVAL_UNKNOWN_AT_LINK;
              }
            else
              {
                return EVAL_ERROR;
              }
            break;
        case io_divfloor:
            if (src1_val.is_int_const() && src2_val.is_int_const())
              {
                i_integer ii1 = immed_to_ii(src1_val);
                i_integer ii2 = immed_to_ii(src2_val);
                i_integer ii_result = ii1 / ii2;
                if (((ii1 < 0) != (ii2 < 0)) && ((ii1 % ii2) != 0))
                    --ii_result;
                *result_val = ii_to_immed(ii_result);
              }
            else if (src1_val.is_flt() && src2_val.is_flt())
              {
                *result_val = immed(src1_val.flt() / src2_val.flt());
              }
            else
              {
                return EVAL_ERROR;
              }
            break;
        case io_divceil:
            if (src1_val.is_int_const() && src2_val.is_int_const())
              {
                i_integer ii1 = immed_to_ii(src1_val);
                i_integer ii2 = immed_to_ii(src2_val);
                i_integer ii_result = ii1 / ii2;
                if (((ii1 < 0) == (ii2 < 0)) && ((ii1 % ii2) != 0))
                    ++ii_result;
                *result_val = ii_to_immed(ii_result);
              }
            else if (src1_val.is_flt() && src2_val.is_flt())
              {
                *result_val = immed(src1_val.flt() / src2_val.flt());
              }
            else
              {
                return EVAL_ERROR;
              }
            break;
        default:
            assert(FALSE);
      }

    eval_status fit_status = fit_immed(result_val, result_type);
    if ((good_result == EVAL_OK) && (fit_status == EVAL_OVERFLOW) &&
        (opcode == io_lsl))
      {
        /* empty */
      }
    else if (fit_status != EVAL_OK)
      {
        good_result = fit_status;
      }

    return good_result;
  }

extern eval_status calc_real_1op_rrr(if_ops opcode, immed *result_val,
                                     type_node *result_type, immed src_val)
  {
    assert(is_real_1op_rrr(opcode));

    eval_status good_result = EVAL_OK;

    switch (opcode)
      {
        case io_cpy:
            *result_val = src_val;
            break;
        case io_neg:
            if (src_val.is_int_const())
                *result_val = ii_to_immed(-immed_to_ii(src_val));
            else if (src_val.is_flt())
                *result_val = immed(-(src_val.flt()));
            else
                return EVAL_ERROR;
            break;
        case io_not:
          {
            if (!src_val.is_int_const())
                return EVAL_ERROR;
            *result_val = ii_to_immed(~immed_to_ii(src_val));

            /*
             *  The high-order bits will be changed to ones by this
             *  but this shouldn't be interpreted as overflow.  In
             *  fact, overflow can't occur.  So here we want to
             *  truncate the thing and ignore overflow warnings.
             */
            eval_status fit_status = fit_immed(result_val, result_type);
            if ((fit_status != EVAL_OK) && (fit_status != EVAL_OVERFLOW))
                good_result = fit_status;

            return good_result;
          }
        case io_abs:
            if (src_val.is_int_const())
              {
                i_integer ii_result = immed_to_ii(src_val);
                if (ii_result < 0)
                    ii_result = -ii_result;
                *result_val = ii_to_immed(ii_result);
              }
            else if (src_val.is_flt())
              {
                double double_result = src_val.flt();
                if (double_result < 0.0)
                    double_result = -double_result;
                *result_val = immed(double_result);
              }
            else
              {
                return EVAL_ERROR;
              }
            break;
        case io_cvt:
            if (src_val.is_int_const())
              {
                if ((result_type->op() == TYPE_INT) ||
                    (result_type->op() == TYPE_PTR) ||
                    (result_type->op() == TYPE_ENUM))
                  {
                    *result_val = ii_to_immed(immed_to_ii(src_val));
                  }
                else if (result_type->op() == TYPE_FLOAT)
                  {
                    double double_value;
                    if (src_val.is_integer())
                      {
                        double_value = (double)(src_val.integer());
                      }
                    else
                      {
                        errno = 0;
                        assert(src_val.is_ext_integer());
                        double_value = strtod(src_val.ext_integer(), NULL);
                        if (errno == ERANGE)
                          {
                            *result_val = immed(double_value);
                            return EVAL_OVERFLOW;
                          }
                      }
                    *result_val = immed(double_value);
                  }
                else
                  {
                    return EVAL_UNKNOWN_AT_LINK;
                  }
              }
            else if (src_val.is_flt())
              {
                if (result_type->op() == TYPE_INT)
		  {
                    /* @@@ */
		    base_type *t = (base_type*)result_type;
		    if (t->is_signed()) {
		      if (t->size() <= (int)sizeof(long)) {
			*result_val = immed((long)(src_val.flt()));
		      } else {
			return EVAL_ERROR;
		      }
		    } else {
		      if (t->size() <= (int)sizeof(unsigned long)) {
			*result_val = immed((unsigned long)(src_val.flt()));
		      } else {
			return EVAL_ERROR;
		      }
		    }
		    /* @@@ */
		  }
		else if (result_type->op() == TYPE_PTR)
                  {
                    /* @@@ */
		    // This should convert based on size_t
		      *result_val = immed((long)(src_val.flt()));
                    /* @@@ */
                  }
                else if (result_type->op() == TYPE_FLOAT)
                  {
                    *result_val = immed(src_val.flt());
                  }
                else
                  {
                    return EVAL_UNKNOWN_AT_LINK;
                  }
              }
            else if (src_val.is_symbol())
              {
                if (result_type->op() == TYPE_PTR)
                    *result_val = src_val;
                else
                    return EVAL_UNKNOWN_AT_LINK;
              }
            else
              {
                return EVAL_UNKNOWN_AT_LINK;
              }
            break;
        default:
            assert(FALSE);
      }

    eval_status fit_status = fit_immed(result_val, result_type);
    if (fit_status != EVAL_OK)
        good_result = fit_status;

    return good_result;
  }

extern operand fold_constants(operand the_operand)
  {
    if (!the_operand.is_expr())
        return the_operand;

    instruction *the_instr = the_operand.instr();
    operand replacement;
    boolean is_new = fold_constants_internal(the_instr, &replacement, TRUE);
    if (is_new)
      {
        delete the_instr;
        return replacement;
      }
    return the_operand;
  }

extern void fold_constants(instruction *the_instr)
  {
    assert(the_instr != NULL);

    tree_node *owner_node = the_instr->owner();
    if ((owner_node != NULL) && (owner_node->is_instr()))
        spill_node = owner_node;

    if (owner_node != NULL)
      {
        if ((the_instr->opcode() == io_str) ||
            (the_instr->opcode() == io_memcpy))
          {
            in_rrr *the_rrr = (in_rrr *)the_instr;
            operand address_op = the_rrr->dst_addr_op();

            immed result_value;
            eval_status status =
                    evaluate_const_expr(address_op, &result_value);
            if ((status == EVAL_OK) && result_value.is_symbol() &&
                (result_value.offset() == 0) &&
                result_value.symbol()->is_var())
              {
                var_sym *the_var = (var_sym *)(result_value.symbol());
                if (the_var->type()->ptr_to()->is_same(address_op.type()))
                  {
                    operand data_op = the_rrr->src2_op();
                    data_op.remove();
                    if (the_rrr->opcode() == io_memcpy)
                        data_op = fold_load(data_op);
                    the_instr = new in_rrr(io_cpy, the_var->type()->unqual(),
                                           operand(the_var), data_op);
                    tree_instr *parent = the_rrr->parent();
                    parent->remove_instr(the_rrr);
                    parent->set_instr(the_instr);
                    delete the_rrr;
                    fold_had_effect = TRUE;
                  }
              }
          }
      }

    operand replacement;
    boolean is_new = fold_constants_internal(the_instr, &replacement, TRUE);
    if (is_new)
      {
        tree_instr *parent = the_instr->parent();
        assert(parent != NULL);
        if (parent->instr() == the_instr)
          {
            instruction *new_instruction;
            if (replacement.is_expr())
              {
                new_instruction = replacement.instr();
                new_instruction->set_dst(the_instr->dst_op());
              }
            else
              {
                new_instruction =
                        new in_rrr(io_cpy, the_instr->result_type(),
                                   the_instr->dst_op(), replacement);
              }
            parent->remove_instr(the_instr);
            parent->set_instr(new_instruction);
          }
        else
          {
            assert(the_instr->dst_op().is_instr());
            instruction *parent_instr = the_instr->dst_op().instr();

            unsigned num_srcs = parent_instr->num_srcs();
            for (unsigned src_num = 0; src_num < num_srcs; ++src_num)
              {
                operand this_src = parent_instr->src_op(src_num);
                if (this_src.is_expr() && (this_src.instr() == the_instr))
                  {
                    this_src.remove();
                    parent_instr->set_src_op(src_num, replacement);
                    break;
                  }
                assert(src_num < num_srcs);
              }
          }

        delete the_instr;
      }

    spill_node = NULL;
  }

extern operand fold_real_2op_rrr(if_ops opcode, type_node *result_type,
                                 operand src1_op, operand src2_op)
  {
    assert(is_real_2op_rrr(opcode));
    in_rrr *new_rrr =
            new in_rrr(opcode, result_type, operand(), src1_op, src2_op);

    operand new_result;
    boolean changed = fold_constants_internal(new_rrr, &new_result, FALSE);
    if (changed)
      {
        delete new_rrr;
        return new_result;
      }
    return operand(new_rrr);
  }

extern operand fold_real_1op_rrr(if_ops opcode, type_node *result_type,
                                 operand src_op)
  {
    assert(is_real_1op_rrr(opcode));
    in_rrr *new_rrr = new in_rrr(opcode, result_type, operand(), src_op);

    operand new_result;
    boolean changed = fold_constants_internal(new_rrr, &new_result, FALSE);
    if (changed)
      {
        delete new_rrr;
        return new_result;
      }
    return operand(new_rrr);
  }

extern operand fold_add(type_node *result_type, operand src1_op,
                        operand src2_op)
  {
    return fold_real_2op_rrr(io_add, result_type, src1_op, src2_op);
  }

extern operand fold_add(operand src1_op, operand src2_op)
  {
    operand new_src1 = src1_op;
    operand new_src2 = src2_op;

    type_node *unqual1 = src1_op.type()->unqual();
    type_node *unqual2 = src2_op.type()->unqual();

    type_node *the_type;
    if (unqual1->is_ptr())
      {
        assert(!unqual2->is_ptr());
        the_type = unqual1;
      }
    else if (unqual2->is_ptr())
      {
        the_type = unqual2;
      }
    else
      {
        the_type = cast_up(src1_op.type(), src2_op.type());
        new_src1 = cast_op(new_src1, the_type);
        new_src2 = cast_op(new_src2, the_type);
      }
    return fold_real_2op_rrr(io_add, the_type, new_src1, new_src2);
  }

extern operand fold_sub(type_node *result_type, operand src1_op,
                        operand src2_op)
  {
    return fold_real_2op_rrr(io_sub, result_type, src1_op, src2_op);
  }

extern operand fold_sub(operand src1_op, operand src2_op)
  {
    operand new_src1 = src1_op;
    operand new_src2 = src2_op;

    type_node *unqual1 = src1_op.type()->unqual();
    type_node *unqual2 = src2_op.type()->unqual();

    type_node *the_type;
    if (unqual1->is_ptr())
      {
        if (unqual2->is_ptr())
            the_type = type_ptr_diff;
        else
            the_type = unqual1;
      }
    else
      {
        assert(!unqual2->is_ptr());
        the_type = cast_up(src1_op.type(), src2_op.type());
        new_src1 = cast_op(new_src1, the_type);
        new_src2 = cast_op(new_src2, the_type);
      }
    return fold_real_2op_rrr(io_sub, the_type, new_src1, new_src2);
  }

extern operand fold_mul(operand src1_op, operand src2_op)
  {
    type_node *the_type = cast_up(src1_op.type(), src2_op.type());
    return fold_real_2op_rrr(io_mul, the_type, cast_op(src1_op, the_type),
                             cast_op(src2_op, the_type));
  }

extern operand fold_div(operand src1_op, operand src2_op)
  {
    type_node *the_type = cast_up(src1_op.type(), src2_op.type());
    return fold_real_2op_rrr(io_div, the_type, cast_op(src1_op, the_type),
                             cast_op(src2_op, the_type));
  }

extern operand fold_rem(operand src1_op, operand src2_op)
  {
    type_node *the_type = cast_up(src1_op.type(), src2_op.type());
    return fold_real_2op_rrr(io_rem, the_type, cast_op(src1_op, the_type),
                             cast_op(src2_op, the_type));
  }

extern operand fold_mod(operand src1_op, operand src2_op)
  {
    type_node *the_type = cast_up(src1_op.type(), src2_op.type());
    return fold_real_2op_rrr(io_mod, the_type, cast_op(src1_op, the_type),
                             cast_op(src2_op, the_type));
  }

extern operand fold_min(operand src1_op, operand src2_op)
  {
    type_node *the_type = cast_up(src1_op.type(), src2_op.type());
    return fold_real_2op_rrr(io_min, the_type, cast_op(src1_op, the_type),
                             cast_op(src2_op, the_type));
  }

extern operand fold_max(operand src1_op, operand src2_op)
  {
    type_node *the_type = cast_up(src1_op.type(), src2_op.type());
    return fold_real_2op_rrr(io_max, the_type, cast_op(src1_op, the_type),
                             cast_op(src2_op, the_type));
  }

extern operand fold_and(operand src1_op, operand src2_op)
  {
    type_node *the_type = cast_up(src1_op.type(), src2_op.type());
    return fold_real_2op_rrr(io_and, the_type, cast_op(src1_op, the_type),
                             cast_op(src2_op, the_type));
  }

extern operand fold_ior(operand src1_op, operand src2_op)
  {
    type_node *the_type = cast_up(src1_op.type(), src2_op.type());
    return fold_real_2op_rrr(io_ior, the_type, cast_op(src1_op, the_type),
                             cast_op(src2_op, the_type));
  }

extern operand fold_xor(operand src1_op, operand src2_op)
  {
    type_node *the_type = cast_up(src1_op.type(), src2_op.type());
    return fold_real_2op_rrr(io_xor, the_type, cast_op(src1_op, the_type),
                             cast_op(src2_op, the_type));
  }

extern operand fold_asr(operand src1_op, operand src2_op)
  {
    return fold_real_2op_rrr(io_asr, src1_op.type(), src1_op, src2_op);
  }

extern operand fold_lsl(operand src1_op, operand src2_op)
  {
    return fold_real_2op_rrr(io_lsl, src1_op.type(), src1_op, src2_op);
  }

extern operand fold_lsr(operand src1_op, operand src2_op)
  {
    return fold_real_2op_rrr(io_lsr, src1_op.type(), src1_op, src2_op);
  }

extern operand fold_rot(operand src1_op, operand src2_op)
  {
    return fold_real_2op_rrr(io_rot, src1_op.type(), src1_op, src2_op);
  }

extern operand fold_seq(operand src1_op, operand src2_op)
  {
    type_node *the_type = cast_up(src1_op.type(), src2_op.type());
    return fold_real_2op_rrr(io_seq, type_signed, cast_op(src1_op, the_type),
                             cast_op(src2_op, the_type));
  }

extern operand fold_sne(operand src1_op, operand src2_op)
  {
    type_node *the_type = cast_up(src1_op.type(), src2_op.type());
    return fold_real_2op_rrr(io_sne, type_signed, cast_op(src1_op, the_type),
                             cast_op(src2_op, the_type));
  }

extern operand fold_sl(operand src1_op, operand src2_op)
  {
    type_node *the_type = cast_up(src1_op.type(), src2_op.type());
    return fold_real_2op_rrr(io_sl, type_signed, cast_op(src1_op, the_type),
                             cast_op(src2_op, the_type));
  }

extern operand fold_sle(operand src1_op, operand src2_op)
  {
    type_node *the_type = cast_up(src1_op.type(), src2_op.type());
    return fold_real_2op_rrr(io_sle, type_signed, cast_op(src1_op, the_type),
                             cast_op(src2_op, the_type));
  }

extern operand fold_divfloor(operand src1_op, operand src2_op)
  {
    type_node *the_type = cast_up(src1_op.type(), src2_op.type());
    return fold_real_2op_rrr(io_divfloor, the_type, cast_op(src1_op, the_type),
                             cast_op(src2_op, the_type));
  }

extern operand fold_divceil(operand src1_op, operand src2_op)
  {
    type_node *the_type = cast_up(src1_op.type(), src2_op.type());
    return fold_real_2op_rrr(io_divceil, the_type, cast_op(src1_op, the_type),
                             cast_op(src2_op, the_type));
  }

extern operand fold_neg(operand src_op)
  {
    return fold_real_1op_rrr(io_neg, src_op.type(), src_op);
  }

extern operand fold_not(operand src_op)
  {
    return fold_real_1op_rrr(io_not, src_op.type(), src_op);
  }

extern operand fold_abs(operand src_op)
  {
    return fold_real_1op_rrr(io_abs, src_op.type(), src_op);
  }

extern operand fold_load(operand address)
  {
    type_node *address_type = address.type()->unqual();
    assert(address_type->is_ptr());
    ptr_type *address_ptr = (ptr_type *)address_type;
    type_node *data_type = address_ptr->ref_type();

    in_rrr *new_load = new in_rrr(io_lod, data_type, operand(), address);

    operand new_result;
    boolean changed = fold_constants_internal(new_load, &new_result, FALSE);
    if (changed)
      {
        delete new_load;
        return new_result;
      }
    return operand(new_load);
  }

extern operand fold_logical_not(operand the_op)
  {
    if (the_op.is_expr())
      {
        instruction *the_instr = the_op.instr();
        switch (the_instr->opcode())
          {
            case io_seq:
                the_instr->set_opcode(io_sne);
                return the_op;
            case io_sne:
                the_instr->set_opcode(io_seq);
                return the_op;
            case io_sl:
            case io_sle:
              {
                in_rrr *the_rrr = (in_rrr *)the_instr;
                if (the_rrr->opcode() == io_sl)
                    the_rrr->set_opcode(io_sle);
                else
                    the_rrr->set_opcode(io_sl);
                operand op1 = the_rrr->src1_op();
                operand op2 = the_rrr->src2_op();
                op1.remove();
                op2.remove();
                the_rrr->set_src1(op2);
                the_rrr->set_src2(op1);
                return the_op;
              }
            default:
                break;
          }
      }

    return fold_seq(the_op, const_op(immed(0), the_op.type()));
  }

extern tree_node_list *reduce_to_side_effects(operand the_op)
  {
    tree_node_list *result = NULL;

    if (the_op.is_symbol())
      {
        var_sym *the_var = the_op.symbol();
        if (any_part_volatile(the_var->type()))
          {
            in_rrr *the_copy =
                    new in_rrr(io_cpy, the_var->type()->unqual(), operand(),
                               operand(the_var));
            if (result == NULL)
                result = new tree_node_list;
            result->append(new tree_instr(the_copy));
          }
      }

    if (!the_op.is_expr())
        return result;

    instruction *the_instr = the_op.instr();

    switch (the_instr->opcode())
      {
        case io_lod:
          {
            in_rrr *the_load = (in_rrr *)the_instr;
            type_node *the_type = the_load->src_addr_op().type()->unqual();
            assert(the_type->is_ptr());
            ptr_type *the_ptr = (ptr_type *)the_type;
            if (!any_part_volatile(the_ptr->ref_type()))
                break;

            /* fall through */
          }
        case io_cal:
        case io_gen:
            if ((the_instr->opcode() == io_cal) &&
                !instr_is_impure_call(the_instr))
              {
                break;
              }
            if (result == NULL)
                result = new tree_node_list;
            result->append(new tree_instr(the_instr));
            return result;
        default:
            break;
      }

    unsigned num_srcs = the_instr->num_srcs();
    for (unsigned src_num = 0; src_num < num_srcs; ++src_num)
      {
        operand this_src = the_instr->src_op(src_num);
        this_src.remove();
        tree_node_list *this_effect = reduce_to_side_effects(this_src);
        if (this_effect != NULL)
          {
            if (result == NULL)
              {
                result = this_effect;
              }
            else
              {
                result->append(this_effect);
                delete this_effect;
              }
          }
      }

    delete the_instr;
    return result;
  }

extern operand cast_op(operand the_op, type_node *new_type)
  {
    if (new_type->is_modifier())
      {
        error_line(1, new_type,
                   "operand type may not be made a qualified type");
      }

    if (the_op.type() == new_type)
        return the_op;

    if (the_op.is_expr() && (the_op.instr()->opcode() == io_cvt) &&
        the_op.type()->is_ptr())
      {
        the_op.instr()->set_result_type(new_type);
        return the_op;
      }

    return operand(new in_rrr(io_cvt, new_type, operand(), the_op));
  }

static boolean immed_is_zero(immed the_immed)
  {
    return ((the_immed == immed(0)) || (the_immed == immed(0.0)));
  }

/*
 *  Attempt to fold constants in the expression rooted with the_instr.
 *  If the_instr should be replaced, TRUE is returned and *replacement
 *  is given the operand that should replace the_instr.  In that case,
 *  the_instr should be deleted by the calling function and replaced
 *  with *replacement; the_instr is no longer a valid expression tree
 *  for that value -- some of its descendants may have been removed
 *  and replaced with null operands.  In the other case, when
 *  the_instr does not need to be replaced, FALSE is returned and
 *  the_instr is still a valid expression tree, though its children
 *  may have changed.
 */
static boolean fold_constants_internal(instruction *the_instr,
                                       operand *replacement,
                                       boolean recursively)
  {
    if (the_instr->opcode() == io_ldc)
        return FALSE;

    immed immed_result;
    eval_status const_status = evaluate_const_instr(the_instr, &immed_result);
    switch (const_status)
      {
        case EVAL_OVERFLOW:
            warning_line(spill_node, "overflow in constant expression");
            overflow_in_folding = TRUE;
            /* fall through */
        case EVAL_OK:
          {
            *replacement = const_op(immed_result, the_instr->result_type());
            fold_had_effect = TRUE;
            return TRUE;
          }
        default:
            break;
      }
    
    if (recursively)
      {
        unsigned num_srcs = the_instr->num_srcs();
        for (unsigned src_num = 0; src_num < num_srcs; ++src_num)
          {
            operand this_src = the_instr->src_op(src_num);
            if (this_src.is_expr())
              {
                operand new_src;
                boolean replace =
                        fold_constants_internal(this_src.instr(), &new_src,
                                                TRUE);
                if (replace)
                  {
                    this_src.remove();
                    delete this_src.instr();
                    the_instr->set_src_op(src_num, new_src);
                  }
              }
          }
      }

    if (the_instr->peek_annote(k_fields) != NULL)
        return FALSE;

    if (is_real_2op_rrr(the_instr->opcode()))
      {
        in_rrr *the_rrr = (in_rrr *)the_instr;
        operand source_op1 = the_rrr->src1_op();
        operand source_op2 = the_rrr->src2_op();

        boolean source1_is_const = FALSE;
        boolean source2_is_const = FALSE;
        immed source1_value, source2_value;

        if (source_op1.is_expr() &&
            (source_op1.instr()->opcode() == io_ldc) &&
            (source_op1.instr()->peek_annote(k_fields) == NULL))
          {
            in_ldc *the_ldc = (in_ldc *)(source_op1.instr());
            source1_is_const = TRUE;
            source1_value = the_ldc->value();
          }

        if (source_op2.is_expr() &&
            (source_op2.instr()->opcode() == io_ldc) &&
            (source_op2.instr()->peek_annote(k_fields) == NULL))
          {
            in_ldc *the_ldc = (in_ldc *)(source_op2.instr());
            source2_is_const = TRUE;
            source2_value = the_ldc->value();
          }

        if (source1_is_const && source2_is_const)
          {
            immed result_value;
            eval_status status =
                    calc_real_2op_rrr(the_instr->opcode(), &result_value,
                                      the_instr->result_type(), source1_value,
                                      source2_value);

            switch (status)
              {
                case EVAL_OK:
                    break;
                case EVAL_OVERFLOW:
                    warning_line(spill_node,
                                 "overflow in constant expression");
                    overflow_in_folding = TRUE;
                    break;
                case EVAL_DIV_BY_ZERO:
                    error_line(1, spill_node, "constant division by zero");
                    break;
                case EVAL_UNKNOWN_AT_LINK:
                case EVAL_ERROR:
                    return FALSE;
                case EVAL_NOT_CONST:
                case EVAL_UNDEFINED:
                default:
                    assert(FALSE);
              }

            *replacement = const_op(result_value, the_instr->result_type());
            fold_had_effect = TRUE;
            return TRUE;
          }

        switch (the_instr->opcode())
          {
            case io_add:
            case io_sub:
              {
                summand_list *src1_sum = sum_form(source_op1);
                summand_list *src2_sum = sum_form(source_op2);
                boolean first_level =
                        ((src1_sum->count() == 1) && (src2_sum->count() == 1));
                if (the_instr->opcode() == io_sub)
                    negate_sum(src2_sum);
                src1_sum->append(src2_sum);
                delete src2_sum;

                cancel_in_sum(src1_sum);
                merge_sum_consts(src1_sum);

                if (src1_sum->count() == 0)
                  {
                    delete src1_sum;
                    *replacement = new_zero_op(the_instr->result_type());
                    fold_had_effect = TRUE;
                    return TRUE;
                  }
                else if (src1_sum->count() == 1)
                  {
                    summand this_summand = src1_sum->pop();
                    if ((!this_summand.is_negated) ||
                        (!this_summand.the_op.type()->is_ptr()))
                      {
                        delete src1_sum;

                        if_ops opcode =
                                (this_summand.is_negated ? io_neg : io_cvt);

                        *replacement =
                                fold_real_1op_rrr(opcode,
                                                  the_instr->result_type(),
                                                  this_summand.the_op);
                        fold_had_effect = TRUE;
                        return TRUE;
                      }
                  }
                else if ((src1_sum->count() == 2) && (!first_level))
                  {
                    summand summand1 = src1_sum->pop();
                    summand summand2 = src1_sum->pop();
                    delete src1_sum;

                    operand op1 = summand1.the_op;
                    operand op2 = summand2.the_op;

                    boolean switched = FALSE;
                    if_ops opcode;
                    boolean result_negated = FALSE;
                    if (summand1.is_negated)
                      {
                        if (summand2.is_negated)
                          {
                            opcode = io_add;
                            result_negated = TRUE;
                          }
                        else
                          {
                            opcode = io_sub;
                            switched = TRUE;
                          }
                      }
                    else
                      {
                        if (summand2.is_negated)
                          {
                            opcode = io_sub;
                          }
                        else
                          {
                            opcode = io_add;
                          }
                      }

                    if (switched)
                      {
                        operand temp_op = op1;
                        op1 = op2;
                        op2 = temp_op;
                      }

                    in_rrr *new_rrr =
                            new in_rrr(opcode, the_instr->result_type(),
                                       operand(), op1, op2);

                    if (result_negated)
                      {
                        new_rrr =
                                new in_rrr(io_neg, the_instr->result_type(),
                                           operand(), operand(new_rrr));
                      }

                    *replacement = operand(new_rrr);

                    fold_had_effect = TRUE;
                    return TRUE;
                  }

                free_sum(src1_sum);
                delete src1_sum;

                break;
              }
            case io_mul:
              {
                if (source1_is_const || source2_is_const)
                  {
                    operand the_op;
                    immed the_value;
                    if (source1_is_const)
                      {
                        the_op = source_op2;
                        the_value = source1_value;
                      }
                    else
                      {
                        the_op = source_op1;
                        the_value = source2_value;
                      }

                    if ((the_value == immed(0)) || (the_value == immed(0.0)))
                      {
                        the_op.remove();
                        instruction *effects = spill_side_effects(the_op);
                        operand zero_op =
                                new_zero_op(the_instr->result_type());

                        if (effects != NULL)
                          {
                            operand effect_op = operand(effects);
                            effect_op = cast_op(effect_op, zero_op.type());
                            zero_op =
                                    operand(new in_rrr(io_mul, zero_op.type(),
                                                       operand(), zero_op,
                                                       effect_op));
                          }

                        *replacement = zero_op;
                        fold_had_effect = TRUE;
                        return TRUE;
                      }

                    if ((the_value == immed(1)) || (the_value == immed(1.0)))
                      {
                        the_op.remove();
                        the_op = cast_op(the_op, the_instr->result_type());
                        *replacement = the_op;
                        fold_had_effect = TRUE;
                        return TRUE;
                      }
                  }
                break;
              }
            case io_div:
            case io_divfloor:
            case io_divceil:
              {
                if (source2_is_const && source2_value.is_int_const() &&
                    (source2_value != immed(0)))
                  {
                    if (op_divisible_by(source_op1,
                                        immed_to_ii(source2_value)))
                      {
                        source_op1.remove();
                        *replacement =
                                push_division_down(source_op1,
                                                   immed_to_ii(source2_value));
                        fold_had_effect = TRUE;
                        return TRUE;
                      }
                  }
                break;
              }
            case io_mod:
            case io_rem:
              {
                if (source2_is_const &&
                    ((source2_value == immed(1)) ||
                     (source2_value == immed(-1))))
                  {
                    source_op1.remove();
                    instruction *effects = spill_side_effects(source_op1);
                    operand zero_op = new_zero_op(the_instr->result_type());

                    if (effects != NULL)
                      {
                        operand effect_op = operand(effects);
                        effect_op = cast_op(effect_op, zero_op.type());
                        zero_op =
                                operand(new in_rrr(io_mul, zero_op.type(),
                                                   operand(), zero_op,
                                                   effect_op));
                      }

                    *replacement = zero_op;
                    fold_had_effect = TRUE;
                    return TRUE;
                  }
                break;
              }
            case io_and:
            case io_ior:
            case io_xor:
              {
                if (source1_is_const || source2_is_const)
                  {
                    operand the_op;
                    immed the_value;
                    if (source1_is_const)
                      {
                        the_op = source_op2;
                        the_value = source1_value;
                      }
                    else
                      {
                        the_op = source_op1;
                        the_value = source2_value;
                      }

                    if (immed_is_zero(the_value))
                      {
                        if (the_instr->opcode() == io_and)
                          {
                            the_op.remove();
                            instruction *effects = spill_side_effects(the_op);
                            operand zero_op =
                                    new_zero_op(the_instr->result_type());

                            if (effects != NULL)
                              {
                                operand effect_op = operand(effects);
                                effect_op = cast_op(effect_op, zero_op.type());
                                zero_op =
                                        operand(new in_rrr(io_mul,
                                                           zero_op.type(),
                                                           operand(), zero_op,
                                                           effect_op));
                              }

                            *replacement = zero_op;
                          }
                        else
                          {
                            the_op.remove();
                            if (the_op.type() != the_instr->result_type())
                              {
                                the_op = cast_op(the_op,
                                                 the_instr->result_type());
                              }

                            *replacement = the_op;
                          }
                        fold_had_effect = TRUE;
                        return TRUE;
                      }
                  }
                break;
              }
            case io_seq:
            case io_sne:
            case io_sl:
            case io_sle:
              {
                if ((source_op1.type()->unqual()->op() == TYPE_INT) &&
                    (source_op2.type()->unqual()->op() == TYPE_INT) &&
                    source_op1.is_expr() &&
                    (source_op1.instr()->opcode() == io_cvt) &&
                    source_op2.is_expr() &&
                    (source_op2.instr()->opcode() == io_cvt))
                  {
                    in_rrr *cvt1 = (in_rrr *)(source_op1.instr());
                    in_rrr *cvt2 = (in_rrr *)(source_op2.instr());
                    base_type *base1 =
                            (base_type *)(source_op1.type()->unqual());
                    base_type *base2 =
                            (base_type *)(source_op2.type()->unqual());
                    type_node *src_type1 = cvt1->src_op().type()->unqual();
                    type_node *src_type2 = cvt2->src_op().type()->unqual();
                    if ((!base1->is_signed()) && (!base2->is_signed()) &&
                        src_type1->is_ptr() && src_type2->is_ptr() &&
                        (base1->size() == src_type1->size()) &&
                        (base2->size() == src_type2->size()))
                      {
                        operand new_src1 = cvt1->src_op();
                        operand new_src2 = cvt2->src_op();
                        new_src1.remove();
                        new_src2.remove();
                        source_op1.remove();
                        source_op2.remove();
                        delete cvt1;
                        delete cvt2;
                        the_rrr->set_src1(new_src1);
                        the_rrr->set_src2(new_src2);
                        fold_had_effect = TRUE;
                        return fold_constants_internal(the_rrr, replacement,
                                                       recursively);
                      }
                  }

                summand_list *src1_sum = sum_form(source_op1);
                summand_list *src2_sum = sum_form(source_op2);
                negate_sum(src2_sum);
                src1_sum->append(src2_sum);
                delete src2_sum;

                cancel_in_sum(src1_sum);
                merge_sum_consts(src1_sum);

                if (src1_sum->count() == 1)
                  {
                    summand this_summand = src1_sum->head()->contents;
                    operand this_op = this_summand.the_op;
                    if (this_op.is_expr() &&
                        (this_op.instr()->opcode() == io_ldc))
                      {
                        in_ldc *the_ldc = (in_ldc *)(this_op.instr());
                        immed value = the_ldc->value();
                        boolean is_negated = this_summand.is_negated;
                        boolean success = TRUE;
                        int result_int;
                        if (value.is_int_const())
                          {
                            i_integer value_ii = immed_to_ii(value);
                            switch (the_instr->opcode())
                              {
                                case io_seq:
                                    result_int = (value_ii == 0);
                                    break;
                                case io_sne:
                                    result_int = (value_ii != 0);
                                    break;
                                case io_sl:
                                    if (is_negated)
                                        result_int = (value_ii > 0);
                                    else
                                        result_int = (value_ii < 0);
                                    break;
                                case io_sle:
                                    if (is_negated)
                                        result_int = (value_ii >= 0);
                                    else
                                        result_int = (value_ii <= 0);
                                    break;
                                default:
                                    result_int = 0;
                                    assert(FALSE);
                              }
                          }
                        else if (value.is_flt())
                          {
                            double value_double = value.flt();
                            switch (the_instr->opcode())
                              {
                                case io_seq:
                                    result_int = (value_double == 0.0);
                                    break;
                                case io_sne:
                                    result_int = (value_double != 0.0);
                                    break;
                                case io_sl:
                                    if (is_negated)
                                        result_int = (value_double > 0.0);
                                    else
                                        result_int = (value_double < 0.0);
                                    break;
                                case io_sle:
                                    if (is_negated)
                                        result_int = (value_double >= 0.0);
                                    else
                                        result_int = (value_double <= 0.0);
                                    break;
                                default:
                                    result_int = 0;
                                    assert(FALSE);
                              }
                          }
                        else
                          {
                            success = FALSE;
                          }

                        if (success)
                          {
                            free_sum(src1_sum);
                            delete src1_sum;

                            *replacement =
                                    const_op(immed(result_int),
                                             the_instr->result_type());
                            fold_had_effect = TRUE;
                            return TRUE;
                          }
                      }
                  }

                free_sum(src1_sum);
                delete src1_sum;

                break;
              }
            default:
                break;
          }
      }
    else if (is_real_1op_rrr(the_instr->opcode()))
      {
        in_rrr *the_rrr = (in_rrr *)the_instr;
        operand source_op = the_rrr->src_op();

        if (source_op.is_expr() && (source_op.instr()->opcode() == io_ldc) &&
            (source_op.instr()->peek_annote(k_fields) == NULL))
          {
            in_ldc *the_ldc = (in_ldc *)(source_op.instr());
            immed value = the_ldc->value();

            immed result_value;
            eval_status status =
                    calc_real_1op_rrr(the_instr->opcode(), &result_value,
                                      the_instr->result_type(), value);

            switch (status)
              {
                case EVAL_OK:
                    break;
                case EVAL_OVERFLOW:
                    warning_line(spill_node,
                                 "overflow in constant expression");
                    overflow_in_folding = TRUE;
                    break;
                case EVAL_DIV_BY_ZERO:
                    error_line(1, spill_node, "constant division by zero");
                    break;
                case EVAL_UNKNOWN_AT_LINK:
                case EVAL_ERROR:
                    return FALSE;
                case EVAL_NOT_CONST:
                case EVAL_UNDEFINED:
                default:
                    assert(FALSE);
              }

            *replacement = const_op(result_value, the_instr->result_type());
            fold_had_effect = TRUE;
            return TRUE;
          }

        switch (the_instr->opcode())
          {
            case io_cpy:
                if ((the_instr->result_type() == source_op.type()) &&
                    ((!the_instr->dst_op().is_symbol()) ||
                     (!source_op.is_symbol())))
                  {
                    source_op.remove();
                    *replacement = source_op;
                    fold_had_effect = TRUE;
                    return TRUE;
                  }
                break;
            case io_cvt:
                if (the_instr->result_type() == source_op.type())
                  {
                    source_op.remove();
                    *replacement = source_op;
                    fold_had_effect = TRUE;
                    return TRUE;
                  }

                if (the_instr->result_type()->is_same(source_op.type()))
                  {
                    source_op.remove();
                    source_op = fold_real_1op_rrr(io_cpy,
                                                  the_instr->result_type(),
                                                  source_op);
                    *replacement = source_op;
                    fold_had_effect = TRUE;
                    return TRUE;
                  }

                if (source_op.is_expr() &&
                    (source_op.instr()->opcode() == io_cvt))
                  {
                    if (the_instr->result_type()->unqual()->is_ptr() &&
                        source_op.type()->unqual()->is_ptr())
                      {
                        source_op.remove();
                        source_op.instr()->set_result_type(
                                the_instr->result_type());
                        *replacement = source_op;
                        fold_had_effect = TRUE;
                        return TRUE;
                      }
                  }

                break;
            default:
                break;
          }
      }
    else if (the_instr->opcode() == io_lod)
      {
        in_rrr *the_load = (in_rrr *)the_instr;
        operand address_op = the_load->src_addr_op();

        immed result_value;
        eval_status status = evaluate_const_expr(address_op, &result_value);
        if ((status == EVAL_OK) && result_value.is_symbol() &&
            (result_value.offset() == 0) && result_value.symbol()->is_var())
          {
            var_sym *the_var = (var_sym *)(result_value.symbol());
            if (same_modulo_call_by_ref(the_var->type(),
                                        the_load->result_type()))
              {
                *replacement = operand(the_var);
                fold_had_effect = TRUE;
                return TRUE;
              }
          }
      }
    else if (the_instr->opcode() == io_array)
      {
        in_array *the_aref = (in_array *)the_instr;
        unsigned dims = the_aref->dims();
        for (unsigned dim_num = 0; dim_num < dims; ++dim_num)
          {
            operand this_index = the_aref->index(dim_num);
            boolean changed = FALSE;
            while (this_index.is_expr())
              {
                instruction *this_instr = this_index.instr();
                if ((this_instr->opcode() != io_cvt) &&
                    (this_instr->opcode() != io_cpy))
                  {
                    break;
                  }
                in_rrr *the_rrr = (in_rrr *)this_instr;
                type_node *source_type = the_rrr->src_op().type();
                type_node *dest_type = this_index.type();
                if ((dest_type->op() != TYPE_INT) ||
                    (source_type->op() != TYPE_INT))
                  {
                    break;
                  }
                base_type *source_base = (base_type *)source_type;
                base_type *dest_base = (base_type *)dest_type;
                if ((source_base->size() > dest_base->size()) ||
                    (source_base->is_signed() != dest_base->is_signed()))
                  {
                    break;
                  }
                if (!changed)
                  {
                    changed = TRUE;
                    this_index.remove();
                  }
                this_index = the_rrr->src_op();
                this_index.remove();
                delete the_rrr;
              }
            if (changed)
              {
                the_aref->set_index(dim_num, this_index);
                fold_had_effect = TRUE;
              }
          }
      }

    return FALSE;
  }

/*
 *  Get rid of the operand, moving any side effects to just before the
 *  node pointed to by ``spill_node'', if possible.  If there are side
 *  effects and ``spill_node'' is NULL, return an expression
 *  containing the side effects, otherwise return NULL.
 */
static instruction *spill_side_effects(operand the_operand)
  {
    tree_node_list *side_effects = reduce_to_side_effects(the_operand);
    if (side_effects != NULL)
      {
        assert(!side_effects->is_empty());
        tree_node *top_node = side_effects->pop();
        assert(top_node->is_instr());
        tree_instr *top_tree_instr = (tree_instr *)top_node;
        instruction *result = top_tree_instr->instr();
        top_tree_instr->remove_instr(result);
        delete top_tree_instr;

        while (!side_effects->is_empty())
          {
            tree_node *this_node = side_effects->pop();
            assert(this_node->is_instr());
            tree_instr *this_tree_instr = (tree_instr *)this_node;
            instruction *this_instr = this_tree_instr->instr();
            this_tree_instr->remove_instr(this_instr);
            delete this_tree_instr;

            operand op1 = cast_op(operand(result), type_unsigned);
            operand op2 = cast_op(operand(this_instr), type_unsigned);
            result = new in_rrr(io_add, type_unsigned, operand(), op1, op2);
          }

        delete side_effects;
        return result;
      }
    else
      {
        return NULL;
      }
  }

static operand new_zero_op(type_node *the_type)
  {
    immed zero_immed;
    if (the_type->unqual()->op() == TYPE_FLOAT)
        zero_immed = immed(0.0);
    else
        zero_immed = immed(0);

    return const_op(zero_immed, the_type);
  }

static summand_list *sum_form(operand the_op)
  {
    if (the_op.is_instr())
      {
        instruction *the_instr = the_op.instr();
        switch (the_instr->opcode())
          {
            case io_cpy:
              {
                in_rrr *the_cpy = (in_rrr *)the_instr;
                return sum_form(the_cpy->src_op());
              }
            case io_add:
            case io_sub:
              {
                in_rrr *the_rrr = (in_rrr *)the_instr;
                summand_list *sum1 = sum_form(the_rrr->src1_op());
                summand_list *sum2 = sum_form(the_rrr->src2_op());
                if (the_instr->opcode() == io_sub)
                    negate_sum(sum2);
                sum1->append(sum2);
                delete sum2;
                return sum1;
              }
            case io_neg:
              {
                in_rrr *the_rrr = (in_rrr *)the_instr;
                summand_list *result = sum_form(the_rrr->src_op());
                negate_sum(result);
                return result;
              }
            case io_cvt:
              {
                in_rrr *the_rrr = (in_rrr *)the_instr;
                if (the_rrr->result_type()->unqual()->is_ptr() &&
                    the_rrr->src_op().type()->unqual()->is_ptr())
                  {
                    return sum_form(the_rrr->src_op());
                  }
                break;
              }
            case io_array:
              {
                in_array *the_array = (in_array *)the_instr;

                operand elem_num;
                if (the_array->dims() > 0)
                  {
                    elem_num = the_array->index(0).clone();
                    elem_num = cast_op(elem_num, type_ptr_diff);

                    for (unsigned index_num = 1; index_num < the_array->dims();
                         ++index_num)
                      {
                        operand bound_op = the_array->bound(index_num).clone();
                        operand index_op = the_array->index(index_num).clone();
                        bound_op = cast_op(bound_op, type_ptr_diff);
                        index_op = cast_op(index_op, type_ptr_diff);
                        elem_num = fold_mul(elem_num, bound_op);
                        elem_num = fold_add(elem_num, index_op);
                      }
                  }
                else
                  {
                    elem_num = new_zero_op(type_ptr_diff);
                  }

                if (!the_array->offset_op().is_null())
                  {
                    operand offset_clone = the_array->offset_op().clone();
                    offset_clone = cast_op(offset_clone, type_ptr_diff);
                    elem_num = fold_sub(elem_num, offset_clone);
                  }

                unsigned elem_size = the_array->elem_size();
                elem_size =
                        (elem_size + target.addressable_size - 1) /
                        target.addressable_size;
                operand elem_size_op =
                        const_op(immed(elem_size), type_ptr_diff);

                operand elem_offset = fold_mul(elem_num, elem_size_op);

                if (the_array->offset() != 0)
                  {
                    operand next_offset_op =
                            const_op(immed(the_array->offset()),
                                     type_ptr_diff);
                    elem_offset = fold_add(elem_offset, next_offset_op);
                  }

                operand result_op =
                        fold_add( the_array->result_type(),
                                          the_array->base_op().clone(),
                                          elem_offset);

                summand_list *result = sum_form(result_op);
                kill_op(result_op);
                return result;
              }
            default:
                break;
          }
      }

    summand_list *result = new summand_list;
    summand new_summand;
    new_summand.is_negated = FALSE;
    new_summand.the_op = the_op.clone();
    result->append(new_summand);
    return result;
  }

static void cancel_in_sum(summand_list *the_sum)
  {
    summand_list_e *first_e = the_sum->head();
    while (first_e != NULL)
      {
        summand_list_e *next_e = first_e->next();
        summand_list_e *second_e = next_e;
        while (second_e != NULL)
          {
            if ((first_e->contents.is_negated !=
                 second_e->contents.is_negated) &&
                operands_are_same_expr(first_e->contents.the_op,
                                       second_e->contents.the_op))
              {
                if (next_e == second_e)
                    next_e = second_e->next();

                kill_op(first_e->contents.the_op);
                kill_op(second_e->contents.the_op);

                the_sum->remove(first_e);
                the_sum->remove(second_e);

                delete first_e;
                delete second_e;
                break;
              }
            second_e = second_e->next();
          }
        first_e = next_e;
      }
  }

static void negate_sum(summand_list *the_sum)
  {
    summand_list_e *sum_e = the_sum->head();
    while (sum_e != NULL)
      {
        sum_e->contents.is_negated = !(sum_e->contents.is_negated);
        sum_e = sum_e->next();
      }
  }

static void merge_sum_consts(summand_list *the_sum)
  {
    summand_list_e *first_e = the_sum->head();
    while (first_e != NULL)
      {
        summand_list_e *next_e = first_e->next();
        immed first_immed;
        eval_status first_status =
                evaluate_const_expr(first_e->contents.the_op, &first_immed);
        if (first_status == EVAL_OK)
          {
            if ((first_immed == immed(0)) || (first_immed == immed(0.0)))
              {
                kill_op(first_e->contents.the_op);
                the_sum->remove(first_e);
                delete first_e;
                first_e = next_e;
                continue;
              }
            summand_list_e *second_e = next_e;
            while (second_e != NULL)
              {
                summand_list_e *next_second = second_e->next();
                immed second_immed;
                eval_status second_status =
                        evaluate_const_expr(second_e->contents.the_op,
                                            &second_immed);
                if (second_status == EVAL_OK)
                  {
                    type_node *first_type =
                            first_e->contents.the_op.type()->unqual();
                    type_node *second_type =
                            second_e->contents.the_op.type()->unqual();
                    boolean switched = FALSE;
                    if_ops opcode;
                    boolean result_negated = FALSE;
                    if (first_e->contents.is_negated)
                      {
                        if (second_e->contents.is_negated)
                          {
                            opcode = io_add;
                            result_negated = TRUE;
                          }
                        else
                          {
                            opcode = io_sub;
                            switched = TRUE;
                          }
                      }
                    else
                      {
                        if (second_e->contents.is_negated)
                          {
                            opcode = io_sub;
                          }
                        else
                          {
                            opcode = io_add;
                          }
                      }

                    if (switched)
                      {
                        immed temp_immed = first_immed;
                        first_immed = second_immed;
                        second_immed = temp_immed;

                        type_node *temp_type = first_type;
                        first_type = second_type;
                        second_type = temp_type;
                      }

                    type_node *result_type;
                    if (first_type->is_ptr() && second_type->is_ptr())
                        result_type = type_ptr_diff;
                    else if (second_type->is_ptr())
                        result_type = second_type;
                    else
                        result_type = first_type;

                    immed merge_immed;
                    eval_status merge_status =
                            calc_real_2op_rrr(opcode, &merge_immed,
                                              result_type, first_immed,
                                              second_immed);
                    if (switched)
                      {
                        first_immed = second_immed;
                      }

                    if (merge_status == EVAL_OK)
                      {
                        first_immed = merge_immed;
                        kill_op(first_e->contents.the_op);
                        first_e->contents.the_op =
                                const_op(merge_immed, result_type);
                        first_e->contents.is_negated = result_negated;

                        if (next_e == second_e)
                            next_e = second_e->next();

                        kill_op(second_e->contents.the_op);
                        the_sum->remove(second_e);
                        delete second_e;
                      }
                  }
                second_e = next_second;
              }
          }
        first_e = next_e;
      }
  }

static void free_sum(summand_list *the_sum)
  {
    while (!the_sum->is_empty())
      {
        summand this_summand = the_sum->pop();
        kill_op(this_summand.the_op);
      }
  }

static operand push_division_down(operand the_op, i_integer divisor)
  {
    type_node *result_type = the_op.type()->unqual();

    if (divisor == 1)
        return the_op;
    else if (divisor == -1)
        return fold_neg(the_op);

    if (the_op.is_expr())
      {
        instruction *the_instr = the_op.instr();
        switch (the_instr->opcode())
          {
            case io_cpy:
            case io_neg:
              {
                in_rrr *the_rrr = (in_rrr *)the_instr;
                operand src_op = the_rrr->src_op();
                src_op.remove();
                i_integer new_divisor;
                if (the_rrr->opcode() == io_neg)
                    new_divisor = -divisor;
                else
                    new_divisor = divisor;
                delete the_rrr;
                return push_division_down(src_op, new_divisor);
              }
            case io_abs:
              {
                in_rrr *the_rrr = (in_rrr *)the_instr;
                if (op_divisible_by(the_rrr->src_op(), divisor))
                  {
                    operand src_op = the_rrr->src_op();
                    src_op.remove();
                    delete the_rrr;
                    operand result_op = push_division_down(src_op, divisor);
                    if (divisor >= 0)
                        return result_op;
                    return fold_neg(result_op);
                  }
                break;
              }
            case io_add:
            case io_sub:
              {
                in_rrr *the_rrr = (in_rrr *)the_instr;
                if (op_divisible_by(the_rrr->src1_op(), divisor) &&
                    op_divisible_by(the_rrr->src2_op(), divisor))
                  {
                    operand old_src1 = the_rrr->src1_op();
                    operand old_src2 = the_rrr->src2_op();

                    old_src1.remove();
                    old_src2.remove();

                    the_rrr->set_src1(push_division_down(old_src1, divisor));
                    the_rrr->set_src2(push_division_down(old_src2, divisor));

                    return operand(the_rrr);
                  }
                break;
              }
            case io_mul:
              {
                in_rrr *the_mul = (in_rrr *)the_instr;

                if (op_divisible_by(the_mul->src1_op(), divisor))
                  {
                    operand old_src1 = the_mul->src1_op();
                    old_src1.remove();
                    the_mul->set_src1(push_division_down(old_src1, divisor));
                    return operand(the_mul);
                  }

                if (op_divisible_by(the_mul->src2_op(), divisor))
                  {
                    operand old_src2 = the_mul->src2_op();
                    old_src2.remove();
                    the_mul->set_src2(push_division_down(old_src2, divisor));
                    return operand(the_mul);
                  }

                break;
              }
            case io_cvt:
              {
                in_rrr *the_cvt = (in_rrr *)the_instr;
                if (op_divisible_by(the_cvt->src_op(), divisor))
                  {
                    operand old_src = the_cvt->src_op();
                    old_src.remove();
                    the_cvt->set_src(push_division_down(old_src, divisor));
                    return operand(the_cvt);
                  }
                break;
              }
            case io_ldc:
              {
                in_ldc *the_ldc = (in_ldc *)the_instr;
                immed value = the_ldc->value();
                if (value.is_int_const())
                  {
                    the_ldc->set_value(ii_to_immed(immed_to_ii(value) /
                                                   divisor));
                    return operand(the_ldc);
                  }
                break;
              }
            default:
                break;
          }
      }

    return operand(new in_rrr(io_div, result_type, operand(), the_op,
                              int_op(result_type, divisor)));
  }

static operand int_op(type_node *the_type, i_integer the_int)
  {
    return const_op(ii_to_immed(the_int), the_type);
  }

static boolean same_modulo_call_by_ref(type_node *type1, type_node *type2)
  {
    type_node *unqual1 = type1->unqual();
    type_node *unqual2 = type2->unqual();
    if (!unqual1->is_same(unqual2))
        return FALSE;

    type_node *follow1 = type1;
    while (follow1 != unqual1)
      {
        assert(follow1->is_modifier());
        modifier_type *mod1 = (modifier_type *)follow1;
        follow1 = mod1->base();
        if (mod1->op() == TYPE_CALL_BY_REF)
            continue;
        type_node *follow2 = type2;
        while (TRUE)
          {
            if (follow2 == mod1)
                break;
            if (follow2 == unqual2)
                return FALSE;
            assert(follow2->is_modifier());
            modifier_type *mod2 = (modifier_type *)follow2;
            if ((mod2->op() == mod1->op()) && (!mod2->are_annotations()) &&
                (!mod1->are_annotations()))
              {
                break;
              }
            follow2 = mod2->base();
          }
      }

    follow1 = type2;
    while (follow1 != unqual2)
      {
        assert(follow1->is_modifier());
        modifier_type *mod1 = (modifier_type *)follow1;
        follow1 = mod1->base();
        if (mod1->op() == TYPE_CALL_BY_REF)
            continue;
        type_node *follow2 = type1;
        while (TRUE)
          {
            if (follow2 == mod1)
                break;
            if (follow2 == unqual1)
                return FALSE;
            assert(follow2->is_modifier());
            modifier_type *mod2 = (modifier_type *)follow2;
            if ((mod2->op() == mod1->op()) && (!mod2->are_annotations()) &&
                (!mod1->are_annotations()))
              {
                break;
              }
            follow2 = mod2->base();
          }
      }

    return TRUE;
  }
