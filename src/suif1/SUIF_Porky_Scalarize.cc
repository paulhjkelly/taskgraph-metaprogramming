// Contains modified porky.h codegen.cc utils.cc scalarize.cc

/* file "porky.h" */

/*  Copyright (c) 1994,95 Stanford University

    All rights reserved.

    This software is provided under the terms described in
    the "suif_copyright.h" include file. */

#include <suif_copyright.h>

/* include file for the porky program for SUIF */

#include <cstring>
#include <suif1.h>
#include <useful.h>

namespace tg
{

/*----------------------------------------------------------------------*
    Begin Global Variables
 *----------------------------------------------------------------------*/

int verbosity_level = 0;
boolean strength_reduce = FALSE;

/*----------------------------------------------------------------------*
    End Global Variables
 *----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*
    Begin Type Declarations
 *----------------------------------------------------------------------*/

typedef enum
  {
    NO_CALLS,
    CALLS_TOTALLY_ORDERED,
    CALLS_NOT_TOTALLY_ORDERED
  } call_order;

typedef struct goto_data goto_data;

/*----------------------------------------------------------------------*
    End Type Declarations
 *----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*
    Begin Public Function Declarations
 *----------------------------------------------------------------------*/

/*----------------------------------------------------------------------*
    in file "utils.cc":
 *----------------------------------------------------------------------*/

 void insert_tree_node_list_before(tree_node_list *to_insert,
                                         tree_node *before);
 void append_tree_node_to_list(tree_node_list *the_list,
                                     tree_node *the_node);
 tree_node *last_action_node(tree_node_list *the_node_list);
 tree_node *last_action_before(tree_node_list *the_node_list,
                                     tree_node *the_node);

/*----------------------------------------------------------------------*
    in file "codegen.cc":
 *----------------------------------------------------------------------*/

 operand operand_multiply(type_node *the_type, operand arg1,
                                operand arg2);
 operand operand_add(type_node *the_type, operand arg1, operand arg2);
 operand operand_sub(type_node *the_type, operand arg1, operand arg2);
 operand operand_lsl(operand arg1, operand arg2);
 operand operand_neg(type_node *the_type, operand arg);
 operand operand_div(type_node *the_type, operand arg1, operand arg2);
 operand operand_rrr(type_node *the_type, if_ops opcode, operand arg1,
                           operand arg2);
 operand operand_mul_by_const(type_node *the_type, operand arg1,
                                    int arg2);
 operand operand_add_const(type_node *the_type, operand arg1, int arg2);
 operand operand_sub_const(type_node *the_type, operand arg1, int arg2);
 operand operand_div_by_const(type_node *the_type, operand arg1,
                                    int arg2);
 operand operand_int(type_node *the_type, int the_int);
 tree_if *create_if(operand test, tree_node_list *then_part,
                          tree_node_list *else_part, base_symtab *scope);
 boolean try_constant_operation(type_node *the_type, if_ops opcode,
                                      int source_1, int source_2, int *result);
 boolean operand_is_zero(operand the_operand);
 boolean operand_is_int_const(operand the_operand);
 boolean int_const_from_operand(operand the_operand, int *value);
 void replace_instruction_with_operand(instruction *old,
                                             operand new_operand);

/*----------------------------------------------------------------------*
    in file "scalarize.cc":
 *----------------------------------------------------------------------*/

 void init_scalarization(void);
 void scalarize_proc(tree_proc *the_proc);

/*----------------------------------------------------------------------*
    End Public Function Declarations
 *----------------------------------------------------------------------*/

/* file "codegen.cc" */

/*  Copyright (c) 1994 Stanford University

    All rights reserved.

    This software is provided under the terms described in
    the "suif_copyright.h" include file. */

/* for the porky program for SUIF */


/*----------------------------------------------------------------------*
    Begin Type Declarations
 *----------------------------------------------------------------------*/

/*----------------------------------------------------------------------*
    End Type Declarations
 *----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*
    Begin Private Function Declarations
 *----------------------------------------------------------------------*/

static operand operand_dumb_mul(type_node *the_type, operand arg1,
                                operand arg2);
static operand operand_dumb_add(type_node *the_type, operand arg1,
                                operand arg2);
static operand operand_dumb_sub(type_node *the_type, operand arg1,
                                operand arg2);
static operand operand_dumb_lsl(operand arg1, operand arg2);
static boolean try_to_fold_addition(operand original_operand, int constant);
static boolean try_to_fold_multiply(type_node* the_type,
                                    operand *original_operand, int constant);
static boolean will_fold_multiply(operand original_operand, int constant);
static boolean log_base_two(int to_test, int *the_log);
static void dispose_of_operand(operand to_go);

/*----------------------------------------------------------------------*
    End Private Function Declarations
 *----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*
    Begin Public Function Implementations
 *----------------------------------------------------------------------*/

 operand operand_multiply(type_node *the_type, operand arg1,
                                operand arg2)
  {
    assert(!the_type->is_modifier());

    int constant;

    operand new_arg1 = fold_constants(arg1);
    operand new_arg2 = fold_constants(arg2);

    if (int_const_from_operand(new_arg1, &constant))
      {
        dispose_of_operand(new_arg1);
        return operand_mul_by_const(the_type, new_arg2, constant);
      }
    if (int_const_from_operand(new_arg2, &constant))
      {
        dispose_of_operand(new_arg2);
        return operand_mul_by_const(the_type, new_arg1, constant);
      }
    return operand_dumb_mul(the_type, new_arg1, new_arg2);
  }

 operand operand_add(type_node *the_type, operand arg1, operand arg2)
  {
    assert(!the_type->is_modifier());

    int constant;

    operand new_arg1 = fold_constants(arg1);
    operand new_arg2 = fold_constants(arg2);

    if (int_const_from_operand(new_arg1, &constant))
      {
        dispose_of_operand(new_arg1);
        return operand_add_const(the_type, new_arg2, constant);
      }
    if (int_const_from_operand(new_arg2, &constant))
      {
        dispose_of_operand(new_arg2);
        return operand_add_const(the_type, new_arg1, constant);
      }
    return operand_dumb_add(the_type, new_arg1, new_arg2);
  }

 operand operand_sub(type_node *the_type, operand arg1, operand arg2)
  {
    assert(!the_type->is_modifier());

    int constant;

    operand new_arg1 = fold_constants(arg1);
    operand new_arg2 = fold_constants(arg2);

    if (int_const_from_operand(new_arg2, &constant))
      {
        dispose_of_operand(new_arg2);
        return operand_sub_const(the_type, new_arg1, constant);
      }
    if (int_const_from_operand(new_arg1, &constant))
      {
        if (constant == 0)
          {
            dispose_of_operand(new_arg1);
            return operand_neg(the_type, new_arg2);
          }
      }
    return operand_dumb_sub(the_type, new_arg1, new_arg2);
  }

 operand operand_lsl(operand arg1, operand arg2)
  {
    int constant;

    operand new_arg1 = fold_constants(arg1);
    operand new_arg2 = fold_constants(arg2);
    if (int_const_from_operand(new_arg1, &constant))
      {
        if (constant == 0)
          {
            dispose_of_operand(new_arg2);
            return new_arg1;
          }
      }
    if (int_const_from_operand(new_arg2, &constant))
      {
        if (constant == 0)
          {
            dispose_of_operand(new_arg2);
            return new_arg1;
          }
      }
    return operand_dumb_lsl(new_arg1, new_arg2);
  }

 operand operand_neg(type_node *the_type, operand arg)
  {
    assert(!the_type->is_modifier());

    int constant;

    operand new_arg = fold_constants(arg);

    if (int_const_from_operand(new_arg, &constant))
      {
        int negation = -constant;
        if (-negation == constant)
          {
            dispose_of_operand(new_arg);
            return operand_int(the_type, negation);
          }
      }

    return operand_rrr(the_type, io_neg, new_arg, operand());
  }

 operand operand_div(type_node *the_type, operand arg1, operand arg2)
  {
    assert(!the_type->is_modifier());

    int constant;

    operand new_arg1 = fold_constants(arg1);
    operand new_arg2 = fold_constants(arg2);

    if (int_const_from_operand(new_arg2, &constant))
      {
        dispose_of_operand(new_arg2);
        return operand_div_by_const(the_type, new_arg1, constant);
      }
    return operand_rrr(the_type, io_div, new_arg1, new_arg2);
  }

 operand operand_rrr(type_node *the_type, if_ops opcode, operand arg1,
                           operand arg2)
  {
    assert(which_format(opcode) == inf_rrr);
    assert(!the_type->is_modifier());
    in_rrr *new_rrr = new in_rrr(opcode, the_type, operand(), arg1, arg2);
    operand new_operand(new_rrr);
    return new_operand;
  }

/*
 *  If the constant can't be folded but it's a power of two, use reduction of
 *  strength to make it a shift
 */
 operand operand_mul_by_const(type_node *the_type, operand arg1,
                                    int arg2)
  {
    assert(!the_type->is_modifier());

    operand new_operand = arg1;
    if (try_to_fold_multiply(the_type, &new_operand, arg2))
        return cast_op(new_operand, the_type);
    int the_log;
    type_node *source_type = new_operand.type();
    assert(source_type != NULL);
    if (log_base_two(arg2, &the_log) && (source_type->op() == TYPE_INT) &&
        strength_reduce)
      {
        operand source = new_operand;

        base_type *the_base_type = (base_type *)source_type;
        if (the_base_type->is_signed())
          {
            type_node *new_type =
                    new base_type(TYPE_INT, the_base_type->size(), FALSE);
            new_type = the_base_type->parent()->install_type(new_type);
            source = operand(new in_rrr(io_cvt, new_type, operand(), source));
          }

        operand result =
                operand_lsl(source, operand_int(type_unsigned, the_log));

        if (result.type() != the_type)
            result = operand(new in_rrr(io_cvt, the_type, operand(), result));
        return result;
      }
    else
      {
        return operand_dumb_mul(the_type, new_operand,
                                operand_int(the_type, arg2));
      }
  }

 operand operand_add_const(type_node *the_type, operand arg1, int arg2)
  {
    assert(!the_type->is_modifier());

    operand new_operand = arg1;
    if (try_to_fold_addition(new_operand, arg2))
        return cast_op(new_operand, the_type);
    type_node *const_type = (the_type->is_ptr() ? type_ptr_diff : the_type);
    return operand_dumb_add(the_type, arg1, operand_int(const_type, arg2));
  }

 operand operand_sub_const(type_node *the_type, operand arg1, int arg2)
  {
    assert(!the_type->is_modifier());

    int negation = -arg2;
    if (-negation == arg2)
        return operand_add_const(the_type, arg1, negation);
    type_node *const_type =
            (the_type->unqual()->is_ptr()) ? type_ptr_diff : the_type;
    return operand_dumb_sub(the_type, arg1, operand_int(const_type, arg2));
  }

 operand operand_div_by_const(type_node *the_type, operand arg1,
                                    int arg2)
  {
    assert(!the_type->is_modifier());

    if (arg2 == 1)
        return arg1;
    if (arg2 != 0)
      {
        int constant;
        if (int_const_from_operand(arg1, &constant))
          {
            dispose_of_operand(arg1);
            return operand_int(the_type, constant / arg2);
          }
      }
    return operand_rrr(the_type, io_div, arg1, operand_int(the_type, arg2));
  }

 operand operand_int(type_node *the_type, int the_int)
  {
    assert(!the_type->is_modifier());

    immed the_immed(the_int);
    instruction *the_instr;
    if ((the_type->op() == TYPE_INT) || (the_type->op() == TYPE_PTR))
      {
        the_instr = new in_ldc(the_type, operand(), the_immed);
      }
    else
      {
        in_ldc *the_ldc = new in_ldc(type_signed, operand(), the_immed);
        operand ldc_operand(the_ldc);
        the_instr = new in_rrr(io_cvt, the_type, operand(), ldc_operand,
                               operand());
      }
    operand new_operand(the_instr);
    return new_operand;
  }

 tree_if *create_if(operand test, tree_node_list *then_part,
                          tree_node_list *else_part, base_symtab *scope)
  {
    assert(scope->is_block());
    block_symtab *the_block_symtab = (block_symtab *)scope;
    label_sym *else_label = the_block_symtab->new_unique_label(NULL);
    tree_node_list *test_part = new tree_node_list;
    instruction *test_instr = new in_bj(io_bfalse, else_label, test);
    tree_node *test_node = new tree_instr(test_instr);
    append_tree_node_to_list(test_part, test_node);
    return new tree_if(else_label, test_part, then_part, else_part);
  }

 boolean try_constant_operation(type_node *the_type, if_ops opcode,
                                      int source_1, int source_2, int *result)
  {
    assert(!the_type->is_modifier());

    if (is_real_2op_rrr(opcode))
      {
        immed result_val;
        eval_status return_code =
                calc_real_2op_rrr(opcode, &result_val, the_type,
                                  immed(source_1), immed(source_2));
        if (return_code != EVAL_OK)
            return FALSE;
        if (!result_val.is_integer())
            return FALSE;
        *result = result_val.integer();
        return TRUE;
      }
    else if (is_real_1op_rrr(opcode))
      {
        immed result_val;
        eval_status return_code =
                calc_real_1op_rrr(opcode, &result_val, the_type,
                                  immed(source_1));
        if (return_code != EVAL_OK)
            return FALSE;
        if (!result_val.is_integer())
            return FALSE;
        *result = result_val.integer();
        return TRUE;
      }
    else
      {
        return FALSE;
      }
  }

 boolean operand_is_zero(operand the_operand)
  {
    int value;
    if (!int_const_from_operand(the_operand, &value))
        return FALSE;
    return (value == 0);
  }


 boolean operand_is_int_const(operand the_operand)
  {
    int value;
    return int_const_from_operand(the_operand, &value);
  }


 boolean int_const_from_operand(operand the_operand, int *value)
  {
    if (the_operand.kind() == OPER_NULL)
      {
        *value = 0;
        return TRUE;
      }
    if (the_operand.kind() != OPER_INSTR)
        return FALSE;
    instruction *instr = the_operand.instr();
    if (instr->opcode() != io_ldc)
        return FALSE;
    in_ldc *the_ldc = (in_ldc *)instr;
    immed immed_value = the_ldc->value();
    if (immed_value.kind() != im_int)
        return FALSE;
    *value = immed_value.integer();
    return TRUE;
  }

 void replace_instruction_with_operand(instruction *old,
                                             operand new_operand)
  {
    operand original_destination = old->dst_op();
    boolean in_tree = (original_destination.kind() == OPER_INSTR);

    if (in_tree)
      {
        instruction *parent_instr = original_destination.instr();
        assert(parent_instr != NULL);

        unsigned num_srcs = parent_instr->num_srcs();
        for (unsigned src_num = 0; src_num < num_srcs; ++src_num)
          {
            if (parent_instr->src_op(src_num) == operand(old))
              {
                old->remove();
                parent_instr->set_src_op(src_num, new_operand);
                return;
              }
          }
        assert(FALSE);
      }
    else
      {
        tree_node *owner = old->owner();
        assert(owner != NULL);
        switch (owner->kind())
          {
            case TREE_INSTR:
              {
                tree_instr *owner_instr_node = (tree_instr *)owner;
                owner_instr_node->remove_instr(old);

                instruction *new_instr;
                if (new_operand.is_expr())
                  {
                    new_instr = new_operand.instr();
                    new_instr->set_dst(original_destination);
                  }
                else
                  {
                    new_instr =
                            new in_rrr(io_cpy, old->result_type(),
                                       original_destination, new_operand);
                  }

                owner_instr_node->set_instr(new_instr);
                break;
              }
            case TREE_FOR:
              {
                tree_for *the_for = (tree_for *)owner;

                operand lb_op = the_for->lb_op();
                if ((lb_op.kind() == OPER_INSTR) && (lb_op.instr() == old))
                  {
                    old->remove();
                    the_for->set_lb_op(new_operand);
                    return;
                  }

                operand ub_op = the_for->ub_op();
                if ((ub_op.kind() == OPER_INSTR) && (ub_op.instr() == old))
                  {
                    old->remove();
                    the_for->set_ub_op(new_operand);
                    return;
                  }

                operand step_op = the_for->step_op();
                if ((step_op.kind() == OPER_INSTR) && (step_op.instr() == old))
                  {
                    old->remove();
                    the_for->set_step_op(new_operand);
                    return;
                  }

                assert(FALSE);
                break;
              }
            default:
                assert(FALSE);
          }
      }
  }

/*----------------------------------------------------------------------*
    End Public Function Implementations
 *----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*
    Begin Private Function Implementations
 *----------------------------------------------------------------------*/

static operand operand_dumb_mul(type_node *the_type, operand arg1,
                                operand arg2)
  {
    return operand_rrr(the_type, io_mul, arg1, arg2);
  }

static operand operand_dumb_add(type_node *the_type, operand arg1,
                                operand arg2)
  {
    return operand_rrr(the_type, io_add, arg1, arg2);
  }

static operand operand_dumb_sub(type_node *the_type, operand arg1,
                                operand arg2)
  {
    return operand_rrr(the_type, io_sub, arg1, arg2);
  }

static operand operand_dumb_lsl(operand arg1, operand arg2)
  {
    return operand_rrr(arg1.type()->unqual(), io_lsl, arg1, arg2);
  }

static boolean try_to_fold_addition(operand original_operand, int constant)
  {
    if (constant == 0)
        return TRUE;
    if (original_operand.kind() == OPER_INSTR)
      {
        instruction *the_instr = original_operand.instr();
        switch (the_instr->opcode())
          {
            case io_cpy:
              {
                in_rrr *the_rrr = (in_rrr *)the_instr;
                return try_to_fold_addition(the_rrr->src1_op(), constant);
	      }
            case io_add:
              {
                in_rrr *the_rrr = (in_rrr *)the_instr;
                boolean result =
                        try_to_fold_addition(the_rrr->src1_op(), constant);
                if (!result)
                  {
                    result = try_to_fold_addition(the_rrr->src2_op(),
                                                  constant);
                  }
                return result;
	      }
            case io_sub:
              {
                in_rrr *the_rrr = (in_rrr *)the_instr;
                boolean result =
                        try_to_fold_addition(the_rrr->src1_op(), constant);
                if (!result)
                  {
                    int negation = -constant;
                    if (-negation != constant)
                        return FALSE;
                    result = try_to_fold_addition(the_rrr->src2_op(),
                                                  negation);
                  }
                return result;
	      }
            case io_neg:
              {
                in_rrr *the_rrr = (in_rrr *)the_instr;
                int negation = -constant;
                if (-negation != constant)
                    return FALSE;
                return try_to_fold_addition(the_rrr->src1_op(), negation);
	      }
            case io_ldc:
              {
                in_ldc *the_in_ldc = (in_ldc *)the_instr;
                immed the_immediate = the_in_ldc->value();
                if (the_immediate.kind() != im_int)
                    return FALSE;
                int old_value = the_immediate.integer();
                int new_value;
                if (!try_constant_operation(the_in_ldc->result_type(), io_add,
                                            old_value, constant, &new_value))
                  {
                    return FALSE;
                  }
                immed new_immed(new_value);
                the_in_ldc->set_value(new_immed);
                return TRUE;
              }
            default:
                return FALSE;
          }
      }
    else
      {
        return FALSE;
      }
  }

static boolean try_to_fold_multiply(type_node *the_type,
                                    operand *original_operand, int constant)
  {
    assert(!the_type->is_modifier());

    if (constant == 0)
      {
        if (original_operand->kind() == OPER_INSTR)
          {
            instruction *to_go = original_operand->instr();
            delete to_go;
          }
        *original_operand = operand_int(the_type, 0);
        return TRUE;
      }
    if (constant == 1)
        return TRUE;

    if (original_operand->kind() == OPER_INSTR)
      {
        instruction *the_instr = original_operand->instr();
        switch (the_instr->opcode())
          {
            case io_cpy:
              {
                in_rrr *the_rrr = (in_rrr *)the_instr;
                operand source_1 = the_rrr->src1_op();
                source_1.remove();
                boolean success =
                        try_to_fold_multiply(the_type, &source_1, constant);
                the_rrr->set_src1(source_1);
                return success;
	      }
            case io_add:
            case io_sub:
              {
                in_rrr *the_rrr = (in_rrr *)the_instr;
                operand source_1 = the_rrr->src1_op();
                operand source_2 = the_rrr->src2_op();

                boolean success = (will_fold_multiply(source_1, constant) ||
                                   will_fold_multiply(source_2, constant));
                if (!success)
                    return FALSE;

                source_1.remove();
                source_2.remove();

                type_node *result_type = the_rrr->result_type();

                boolean result =
                        try_to_fold_multiply(the_type, &source_1, constant);
                if (!result)
                  {
                    source_1 = operand_mul_by_const(result_type, source_1,
                                                   constant);
                  }

                result = try_to_fold_multiply(the_type, &source_2, constant);

                if (!result)
                  {
                    source_2 = operand_mul_by_const(result_type, source_2,
                                                   constant);
                  }

                the_rrr->set_src1(source_1);
                the_rrr->set_src2(source_2);
                return TRUE;
              }
            case io_neg:
              {
                in_rrr *the_rrr = (in_rrr *)the_instr;
                operand source_1 = the_rrr->src1_op();
                source_1.remove();
                boolean success =
                        try_to_fold_multiply(the_type, &source_1, constant);
                the_rrr->set_src1(source_1);
                return success;
	      }
            case io_mul:
              {
                in_rrr *the_rrr = (in_rrr *)the_instr;

                operand source_1 = the_rrr->src1_op();
                source_1.remove();
                boolean success =
                        try_to_fold_multiply(the_type, &source_1, constant);
                the_rrr->set_src1(source_1);
                if (success)
                    return TRUE;

                operand source_2 = the_rrr->src2_op();
                source_2.remove();
                success = try_to_fold_multiply(the_type, &source_2, constant);
                the_rrr->set_src2(source_2);
                return success;
	      }
            case io_ldc:
              {
                in_ldc *the_in_ldc = (in_ldc *)the_instr;
                immed the_immediate = the_in_ldc->value();
                if (the_immediate.kind() != im_int)
                    return FALSE;
                int old_value = the_immediate.integer();
                int new_value;
                if (!try_constant_operation(the_in_ldc->result_type(), io_mul,
                                            old_value, constant, &new_value))
                  {
                    return FALSE;
                  }
                immed new_immed(new_value);
                the_in_ldc->set_value(new_immed);
                return TRUE;
              }
            default:
                return FALSE;
          }
      }
    else
      {
        return FALSE;
      }
  }

static boolean will_fold_multiply(operand original_operand, int constant)
  {
    if (constant == 0)
        return TRUE;
    if (constant == 1)
        return TRUE;

    if (original_operand.kind() == OPER_INSTR)
      {
        instruction *the_instr = original_operand.instr();
        switch (the_instr->opcode())
          {
            case io_cpy:
              {
                in_rrr *the_rrr = (in_rrr *)the_instr;
                return will_fold_multiply(the_rrr->src1_op(), constant);
	      }
            case io_add:
            case io_sub:
              {
                in_rrr *the_rrr = (in_rrr *)the_instr;
                if (!will_fold_multiply(the_rrr->src1_op(), constant))
                    return FALSE;
                return will_fold_multiply(the_rrr->src2_op(), constant);
              }
            case io_neg:
              {
                in_rrr *the_rrr = (in_rrr *)the_instr;
                return will_fold_multiply(the_rrr->src1_op(), constant);
	      }
            case io_mul:
              {
                in_rrr *the_rrr = (in_rrr *)the_instr;
                if (will_fold_multiply(the_rrr->src1_op(), constant))
                    return TRUE;
                return will_fold_multiply(the_rrr->src2_op(), constant);
	      }
            case io_ldc:
              {
                in_ldc *the_in_ldc = (in_ldc *)the_instr;
                immed the_immediate = the_in_ldc->value();
                if (the_immediate.kind() == im_int)
                  {
                    int old_value = the_immediate.integer();
                    int new_value = old_value * constant;
                    boolean overflow =
                            ((constant != 0) &&
                             ((new_value / constant) != old_value));
                    return !overflow;
                  }
                else
                  {
                    return FALSE;
                  }
              }
            default:
                return FALSE;
          }
      }
    else
      {
        return FALSE;
      }
  }

static boolean log_base_two(int to_test, int *the_log)
  {
    int test = to_test;
    int result = 0;

    if (test == 0)
        return FALSE;
    while (TRUE)
      {
        if (test == 1)
          {
            *the_log = result;
            return TRUE;
          }
        if (test & 1)
            return FALSE;
        test >>= 1;
        ++result;
      }
  }

static void dispose_of_operand(operand to_go)
  {
    if (to_go.kind() == OPER_INSTR)
        delete to_go.instr();
  }

/*----------------------------------------------------------------------*
    End Private Function Implementations
 *----------------------------------------------------------------------*/
/* file "utils.cc" */

/*  Copyright (c) 1994 Stanford University

    All rights reserved.

    This software is provided under the terms described in
    the "suif_copyright.h" include file. */

/* for the porky program for SUIF */


/*----------------------------------------------------------------------*
    Begin Type Declarations
 *----------------------------------------------------------------------*/

/*----------------------------------------------------------------------*
    End Type Declarations
 *----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*
    Begin Private Function Declarations
 *----------------------------------------------------------------------*/

/*----------------------------------------------------------------------*
    End Private Function Declarations
 *----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*
    Begin Public Function Implementations
 *----------------------------------------------------------------------*/

 void insert_tree_node_list_before(tree_node_list *to_insert,
                                         tree_node *before)
  {
    assert(to_insert != NULL);
    assert(before != NULL);

    tree_node_list *parent_list = before->parent();
    tree_node_list_e *list_place = before->list_e();
    parent_list->insert_before(to_insert, list_place);
  }

 void append_tree_node_to_list(tree_node_list *the_list,
                                     tree_node *the_node)
  {
    assert(the_list != NULL);
    assert(the_node != NULL);

    the_list->append(the_node);
  }

/*
 *  This function returns the last node in a list that might have any effect
 *  when executed.  Mark instructions never have an effect, so they aren't
 *  counted here.  If there is no node that might have an effect, NULL is
 *  returned.
 */
 tree_node *last_action_node(tree_node_list *the_node_list)
  {
    return last_action_before(the_node_list, NULL);
  }

/*
 *  This function is the same as last_action_node() except that it only
 *  considers nodes that come before the given node.  If the given node is
 *  NULL, that is considered the end of the list, so the result is the same as
 *  that of last_action_node().  If the given node is not on the list, NULL is
 *  returned.
 */
 tree_node *last_action_before(tree_node_list *the_node_list,
                                     tree_node *the_node)
  {
    if (the_node_list == NULL)
        return NULL;

    if (the_node_list->is_empty())
        return NULL;

    tree_node_list_e *current_element = the_node_list->tail();

    if (the_node != NULL)
      {
        while (current_element != NULL)
          {
            tree_node_list_e *old_element = current_element;
            current_element = current_element->prev();
            if (old_element->contents == the_node)
                break;
          }
      }

    while (current_element != NULL)
      {
        tree_node *this_node = current_element->contents;
        switch (this_node->kind())
          {
            case TREE_INSTR:
              {
                tree_instr *the_tree_instr = (tree_instr *)this_node;
                instruction *the_instr = the_tree_instr->instr();
                assert(the_instr != NULL);
                if ((the_instr->opcode() != io_mrk) &&
                    (the_instr->opcode() != io_nop))
                  {
                    return this_node;
                  }
                break;
              }
            case TREE_BLOCK:
              {
                tree_block *the_block = (tree_block *)this_node;
                tree_node *last_in_block = last_action_node(the_block->body());
                if (last_in_block != NULL)
                    return last_in_block;
              }
            default:
                return this_node;
          }
        current_element = current_element->prev();
      }
    return NULL;
  }

/*----------------------------------------------------------------------*
    End Public Function Implementations
 *----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*
    Begin Private Function Implementations
 *----------------------------------------------------------------------*/

/*----------------------------------------------------------------------*
    End Private Function Implementations
 *----------------------------------------------------------------------*/
/* file "scalarize.cc" */

/*  Copyright (c) 1994 Stanford University

    All rights reserved.

    This software is provided under the terms described in
    the "suif_copyright.h" include file. */

/* for the porky program for SUIF */


/*----------------------------------------------------------------------*
    Begin Type Declarations
 *----------------------------------------------------------------------*/

/*----------------------------------------------------------------------*
    End Type Declarations
 *----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*
    Begin Private Global Variables
 *----------------------------------------------------------------------*/

const char *k_eligibility_level;
const char *k_scalar_replacements;

/*----------------------------------------------------------------------*
    End Private Global Variables
 *----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*
    Begin Private Function Declarations
 *----------------------------------------------------------------------*/

static void find_eligible_on_tree_node(tree_node *the_node, void *);
static void add_eligible_vars(block_symtab *the_symtab);
static void kill_init_referenced(block_symtab *the_symtab);
static void kill_ineligible_on_instr(instruction *the_instr);
static void kill_ineligible_on_operand(operand the_operand);
static void scalarize_on_tree_node(tree_node *the_node, void *);
static void build_replacements(block_symtab *the_symtab);
static void build_replacement_syms(const char *base_name, type_node *the_type,
                                   immed_list *sym_list, unsigned level,
                                   var_sym *old_sym,
                                   base_init_struct_list *inits_left);
static void replace_arrays_on_instr(instruction *the_instr);
static void replace_arrays_on_operand(operand the_operand);
static var_sym *replace_arrays_on_addr(operand address);
static void make_eligible(var_sym *the_var);
static void make_ineligible(var_sym *the_var);
static unsigned eligibility_level(var_sym *the_var);
static void limit_eligibility_level(var_sym *the_var, unsigned level);
static void set_replacement_list(var_sym *the_var, immed_list *replacements);
static immed_list *get_replacement_list(var_sym *the_var);
static void move_initialization_prefix(base_init_struct_list *new_list,
                                       base_init_struct_list *old_list,
                                       int num_bits);
static boolean base_types_ok_for_scalarization(type_node *type1,
                                               type_node *type2);

/*----------------------------------------------------------------------*
    End Private Function Declarations
 *----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*
    Begin Public Function Implementations
 *----------------------------------------------------------------------*/

 void init_scalarization(void)
  {
    k_eligibility_level = lexicon->enter("eligibility level")->sp;
    k_scalar_replacements = lexicon->enter("scalar replacements")->sp;
  }

 void scalarize_proc(tree_proc *the_proc)
  {
    find_eligible_on_tree_node(the_proc, NULL);
    the_proc->map(&find_eligible_on_tree_node, NULL);
    scalarize_on_tree_node(the_proc, NULL);
    the_proc->map(&scalarize_on_tree_node, NULL, TRUE);
  }

/*----------------------------------------------------------------------*
    End Public Function Implementations
 *----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*
    Begin Private Function Implementations
 *----------------------------------------------------------------------*/

static void find_eligible_on_tree_node(tree_node *the_node, void *)
  {
    switch (the_node->kind())
      {
        case TREE_INSTR:
          {
            tree_instr *the_tree_instr = (tree_instr *)the_node;
            instruction *the_instr = the_tree_instr->instr();
            kill_ineligible_on_instr(the_instr);
            if (the_instr->dst_op().is_symbol())
                make_ineligible(the_instr->dst_op().symbol());
            break;
          }
        case TREE_FOR:
          {
            tree_for *the_for = (tree_for *)the_node;
            kill_ineligible_on_operand(the_for->lb_op());
            kill_ineligible_on_operand(the_for->ub_op());
            kill_ineligible_on_operand(the_for->step_op());
            break;
          }
        case TREE_BLOCK:
          {
            tree_block *the_block = (tree_block *)the_node;
            add_eligible_vars(the_block->symtab());
            kill_init_referenced(the_block->symtab());
            break;
          }
        default:
            break;
      }
  }

static void add_eligible_vars(block_symtab *the_symtab)
  {
    sym_node_list_iter the_iter(the_symtab->symbols());
    while (!the_iter.is_empty())
      {
        sym_node *this_symbol = the_iter.step();

        if (!this_symbol->is_var())
            continue;
        var_sym *this_var = (var_sym *)this_symbol;

        if (this_var->is_param())
            continue;
        if ((this_var->parent_var() != NULL) ||
            (this_var->num_children() != 0))
          {
            continue;
          }
        if (!this_var->type()->unqual()->is_array())
            continue;
        if (this_var->type()->is_volatile())
            continue;
        if (this_var->type()->is_call_by_ref())
            continue;
        if ((this_var->parent_var() != NULL) ||
            (this_var->num_children() != 0))
          {
            continue;
          }

        make_eligible(this_var);
      }
  }

/*
 *  Kill any variables whose addresses are used in the static
 *  initialization of other variables.
 */
static void kill_init_referenced(block_symtab *the_symtab)
  {
    var_def_list_iter def_iter(the_symtab->var_defs());
    while (!def_iter.is_empty())
      {
        var_def *this_def = def_iter.step();

        annote_list_iter annote_iter(this_def->annotes());
        while (!annote_iter.is_empty())
          {
            annote *this_annote = annote_iter.step();
            const char *this_name = this_annote->name();
            if ((this_name == k_multi_init) || (this_name == k_repeat_init) ||
                (this_name == k_fill))
              {
                immed_list_iter immed_iter(this_annote->immeds());
                while (!immed_iter.is_empty())
                  {
                    immed this_immed = immed_iter.step();
                    if (this_immed.is_symbol())
                      {
                        sym_node *this_symbol = this_immed.symbol();
                        if (this_symbol->is_var())
                          {
                            var_sym *this_var = (var_sym *)this_symbol;
                            make_ineligible(this_var);
                          }
                      }
                  }
              }
          }
      }
  }

static void kill_ineligible_on_instr(instruction *the_instr)
  {
    unsigned num_srcs = the_instr->num_srcs();
    for (unsigned src_num = 0; src_num < num_srcs; ++src_num)
        kill_ineligible_on_operand(the_instr->src_op(src_num));

    if (the_instr->opcode() != io_ldc)
        return;
    in_ldc *the_ldc = (in_ldc *)the_instr;

    immed value = the_ldc->value();
    if (!value.is_symbol())
        return;
    sym_node *the_symbol = value.symbol();

    if (!the_symbol->is_var())
        return;
    var_sym *the_var = (var_sym *)the_symbol;
    if (eligibility_level(the_var) == 0)
        return;

    if (value.offset() != 0)
      {
        make_ineligible(the_var);
        return;
      }

    type_node *ldc_type = the_ldc->result_type();
    if (!ldc_type->unqual()->is_ptr())
      {
        make_ineligible(the_var);
        return;
      }
    ptr_type *the_ptr_type = (ptr_type *)(ldc_type->unqual());
    if (!base_types_ok_for_scalarization(the_ptr_type->ref_type(),
                                         the_var->type()))
      {
        make_ineligible(the_var);
        return;
      }

    operand ldc_dest = the_ldc->dst_op();
    if (!ldc_dest.is_instr())
      {
        make_ineligible(the_var);
        return;
      }
    instruction *parent_instr = ldc_dest.instr();

    if (parent_instr->opcode() != io_array)
      {
        make_ineligible(the_var);
        return;
      }
    in_array *the_aref = (in_array *)parent_instr;

    if (the_aref->base_op() != operand(the_ldc))
      {
        make_ineligible(the_var);
        return;
      }

    operand aref_dest = the_aref->dst_op();
    if (!aref_dest.is_instr())
      {
        make_ineligible(the_var);
        return;
      }
    instruction *aref_parent = aref_dest.instr();

    switch (aref_parent->opcode())
      {
        case io_lod:
          {
            in_rrr *the_load = (in_rrr *)aref_parent;
            if (the_load->src_addr_op() != operand(the_aref))
              {
                make_ineligible(the_var);
                return;
              }
            break;
          }
        case io_str:
          {
            in_rrr *the_store = (in_rrr *)aref_parent;
            if (the_store->dst_addr_op() != operand(the_aref))
              {
                make_ineligible(the_var);
                return;
              }
            break;
          }
        case io_memcpy:
            break;
        default:
            make_ineligible(the_var);
            return;
      }

    if (the_aref->offset() + the_aref->result_type()->size() >
        the_aref->elem_size())
      {
        make_ineligible(the_var);
        return;
      }

    unsigned num_dims = the_aref->dims();
    unsigned dim_num;
    for (dim_num = 0; dim_num < num_dims; ++dim_num)
      {
        int dummy;
        eval_status status =
                evaluate_const_int_expr(the_aref->index(dim_num), &dummy);
        if (status != EVAL_OK)
            break;
      }

    if (dim_num == 0)
      {
        make_ineligible(the_var);
        return;
      }
    limit_eligibility_level(the_var, dim_num);
  }

static void kill_ineligible_on_operand(operand the_operand)
  {
    if (the_operand.is_symbol())
        make_ineligible(the_operand.symbol());
    else if (the_operand.is_expr())
        kill_ineligible_on_instr(the_operand.instr());
  }

static void scalarize_on_tree_node(tree_node *the_node, void *)
  {
    switch (the_node->kind())
      {
        case TREE_INSTR:
          {
            tree_instr *the_tree_instr = (tree_instr *)the_node;
            instruction *the_instr = the_tree_instr->instr();
            replace_arrays_on_instr(the_instr);
            break;
          }
        case TREE_FOR:
          {
            tree_for *the_for = (tree_for *)the_node;
            replace_arrays_on_operand(the_for->lb_op());
            replace_arrays_on_operand(the_for->ub_op());
            replace_arrays_on_operand(the_for->step_op());
            break;
          }
        case TREE_BLOCK:
          {
            tree_block *the_block = (tree_block *)the_node;
            build_replacements(the_block->symtab());
            break;
          }
        default:
            break;
      }
  }

static void build_replacements(block_symtab *the_symtab)
  {
    sym_node_list_iter the_iter(the_symtab->symbols());
    while (!the_iter.is_empty())
      {
        sym_node *this_symbol = the_iter.step();

        if (!this_symbol->is_var())
            continue;
        var_sym *this_var = (var_sym *)this_symbol;

        unsigned level = eligibility_level(this_var);
        if (level == 0)
            continue;

        if (verbosity_level > 0)
          {
            fprintf(stderr,
                    "in function %s, local array variable %s replaced with "
                    "scalars\n", the_symtab->block()->proc()->name(),
                    this_var->name());
          }

        immed_list *replacements = new immed_list;
        base_init_struct_list *old_inits;
        if (this_var->is_static())
            old_inits = read_init_data(this_var->definition());
        else
            old_inits = NULL;

        build_replacement_syms(this_var->name(), this_var->type(),
                               replacements, level, this_var, old_inits);

        if (old_inits != NULL)
          {
            if (!old_inits->is_empty())
                error_line(1, NULL, "misalignment of initialization data");
            delete old_inits;
          }

        set_replacement_list(this_var, replacements);
      }
  }

static void build_replacement_syms(const char *base_name, type_node *the_type,
                                   immed_list *sym_list, unsigned level,
                                   var_sym *old_sym,
                                   base_init_struct_list *inits_left)
  {
    assert(the_type->unqual()->is_array());
    array_type *the_array_type = (array_type *)(the_type->unqual());
    type_node *elem_type = the_array_type->elem_type();
    assert(the_array_type->lower_bound().is_constant());
    assert(the_array_type->upper_bound().is_constant());

    int lower_bound = the_array_type->lower_bound().constant();
    int upper_bound = the_array_type->upper_bound().constant();

    char *new_base_name = new char[strlen(base_name) + 20];
    sprintf(new_base_name, "%s_", base_name);
    char *num_place = new_base_name + strlen(base_name) + 1;

    if (lower_bound < 0)
      {
        upper_bound = upper_bound - lower_bound;
        lower_bound = 0;
      }
    for (int index = lower_bound; index <= upper_bound; ++index)
      {
        sprintf(num_place, "%d", index);
        if (level > 1)
          {
            build_replacement_syms(new_base_name, elem_type, sym_list,
                                   level - 1, old_sym, inits_left);
          }
        else
          {
            var_sym *new_var =
                    old_sym->parent()->new_var(elem_type->unqual(),
                                               new_base_name);
            new_var->reset_userdef();
            new_var->reset_addr_taken();
            if (old_sym->is_reg())
                new_var->set_reg();
            if (old_sym->is_static())
              {
                assert(inits_left != NULL);
                int alignment = get_alignment(elem_type);
                var_def *new_def =
                        old_sym->parent()->define_var(new_var, alignment);

                base_init_struct_list *new_initializations =
                        new base_init_struct_list;
                move_initialization_prefix(new_initializations, inits_left,
                                           new_var->type()->size());
                write_init_data(new_def, new_initializations);
                deallocate_init_data(new_initializations);
              }

            sym_list->append(immed(new_var));
          }
      }

    delete[] new_base_name;
  }

static void replace_arrays_on_instr(instruction *the_instr)
  {
    switch (the_instr->opcode())
      {
        case io_lod:
          {
            in_rrr *the_load = (in_rrr *)the_instr;
            var_sym *new_var = replace_arrays_on_addr(the_load->src_addr_op());
            if (new_var != NULL)
              {
                operand dest_op = the_load->dst_op();
                if (dest_op.is_instr())
                  {
                    instruction *parent_instr = dest_op.instr();

                    unsigned num_srcs = parent_instr->num_srcs();
                    for (unsigned src_num = 0; src_num < num_srcs; ++src_num)
                      {
                        if (parent_instr->src_op(src_num) == operand(the_load))
                          {
                            assert(parent_instr->src_op(src_num).is_expr());
                            the_load->remove();
                            delete the_load;
                            parent_instr->set_src_op(src_num,
                                                     operand(new_var));
                            return;
                          }
                      }
                    assert(FALSE);
                  }
                else
                  {
                    in_rrr *new_cpy =
                            new in_rrr(io_cpy, new_var->type()->unqual(),
                                       dest_op, operand(new_var));
                    tree_instr *the_tree_instr = the_load->parent();
                    assert(the_tree_instr->instr() == the_load);
                    the_tree_instr->remove_instr(the_load);
                    delete the_load;
                    the_tree_instr->set_instr(new_cpy);
                    return;
                  }
              }
            else
              {
                replace_arrays_on_operand(the_load->src_addr_op());
              }
            break;
          }
        case io_str:
          {
            in_rrr *the_store = (in_rrr *)the_instr;
            replace_arrays_on_operand(the_store->src2_op());
            var_sym *new_var =
                    replace_arrays_on_addr(the_store->dst_addr_op());
            if (new_var != NULL)
              {
                operand data_op = the_store->src2_op();
                data_op.remove();
                instruction *copy_instr;
                if (data_op.is_expr())
                  {
                    copy_instr = data_op.instr();
                    copy_instr->set_dst(operand(new_var));
                  }
                else
                  {
                    copy_instr =
                            new in_rrr(io_cpy, new_var->type()->unqual(),
                                       operand(new_var), data_op);
                  }

                tree_instr *the_tree_instr = the_store->parent();
                assert(the_tree_instr->instr() == the_store);
                the_tree_instr->remove_instr(the_store);
                delete the_store;
                the_tree_instr->set_instr(copy_instr);
              }
            else
              {
                replace_arrays_on_operand(the_store->dst_addr_op());
              }
            break;
          }
        case io_memcpy:
          {
            in_rrr *the_memcpy = (in_rrr *)the_instr;
            var_sym *new_var =
                    replace_arrays_on_addr(the_memcpy->src_addr_op());
            if (new_var != NULL)
              {
                operand dst_addr = the_memcpy->dst_addr_op();
                dst_addr.remove();

                in_rrr *new_store =
                        new in_rrr(io_str, type_void, operand(), dst_addr,
                                   operand(new_var));

                tree_instr *the_tree_instr = the_memcpy->parent();
                assert(the_tree_instr->instr() == the_memcpy);
                the_tree_instr->remove_instr(the_memcpy);
                delete the_memcpy;
                the_tree_instr->set_instr(new_store);

                replace_arrays_on_instr(new_store);
                return;
              }

            new_var = replace_arrays_on_addr(the_memcpy->dst_addr_op());
            if (new_var != NULL)
              {
                operand src_addr = the_memcpy->src_addr_op();
                src_addr.remove();

                in_rrr *new_load =
                        new in_rrr(io_lod, new_var->type()->unqual(),
                                   operand(new_var), src_addr);

                tree_instr *the_tree_instr = the_memcpy->parent();
                assert(the_tree_instr->instr() == the_memcpy);
                the_tree_instr->remove_instr(the_memcpy);
                delete the_memcpy;
                the_tree_instr->set_instr(new_load);

                replace_arrays_on_instr(new_load);
                return;
              }

            replace_arrays_on_operand(the_memcpy->src_addr_op());
            replace_arrays_on_operand(the_memcpy->dst_addr_op());
            break;
          }
        default:
          {
            unsigned num_srcs = the_instr->num_srcs();
            for (unsigned src_num = 0; src_num < num_srcs; ++src_num)
                replace_arrays_on_operand(the_instr->src_op(src_num));
          }
      }
  }

static void replace_arrays_on_operand(operand the_operand)
  {
    if (the_operand.is_expr())
        replace_arrays_on_instr(the_operand.instr());
  }

static var_sym *replace_arrays_on_addr(operand address)
  {
    if (!address.is_expr())
        return NULL;
    instruction *the_instr = address.instr();

    if (the_instr->opcode() != io_array)
        return NULL;
    in_array *the_aref = (in_array *)the_instr;

    operand base_op = the_aref->base_op();
    if (!base_op.is_expr())
        return NULL;
    instruction *base_instr = base_op.instr();

    if (base_instr->opcode() != io_ldc)
        return NULL;
    in_ldc *base_ldc = (in_ldc *)base_instr;

    immed base_value = base_ldc->value();
    if (!base_value.is_symbol())
        return NULL;
    sym_node *base_sym = base_value.symbol();

    if (!base_sym->is_var())
        return NULL;
    var_sym *old_var = (var_sym *)base_sym;

    unsigned level = eligibility_level(old_var);
    if (level == 0)
        return NULL;

    type_node *current_type = old_var->type();
    int elem_num = 0;
    for (unsigned dim_num = 0; dim_num < level; ++dim_num)
      {
        if (!current_type->unqual()->is_array())
          {
            error_line(1, the_aref->parent(),
                       "array reference instruction mismatches base operand "
                       "type");
            return NULL;
          }

        array_type *this_array_type = (array_type *)(current_type->unqual());
        assert(this_array_type->lower_bound().is_constant() &&
               this_array_type->upper_bound().is_constant());

        int lower_bound = this_array_type->lower_bound().constant();
        int upper_bound = this_array_type->upper_bound().constant();

        elem_num *= (upper_bound - lower_bound) + 1;

        int index_int;
        eval_status status =
                evaluate_const_int_expr(the_aref->index(dim_num), &index_int);
        assert(status == EVAL_OK);
        elem_num += index_int - lower_bound;

        current_type = this_array_type->elem_type();
      }

    immed_list *replacements = get_replacement_list(old_var);

    immed_list_iter replacement_iter(replacements);
    while (elem_num > 0)
      {
        if (replacement_iter.is_empty())
          {
            error_line(1, the_aref->parent(), "array index out of bounds");
            return NULL;
          }
        (void)replacement_iter.step();
        --elem_num;
      }

    if (replacement_iter.is_empty())
      {
        error_line(1, the_aref->parent(), "array index out of bounds");
        return NULL;
      }
    immed replacement_value = replacement_iter.step();

    assert(replacement_value.is_symbol());
    sym_node *replacement_sym = replacement_value.symbol();

    assert(replacement_sym->is_var());
    var_sym *new_var = (var_sym *)replacement_sym;

    if (level < the_aref->dims())
      {
        type_node *new_pointer = new_var->type()->ptr_to();
        in_ldc *new_ldc = new in_ldc(new_pointer, operand(), immed(new_var));
        new_var->set_addr_taken();

        in_array *new_aref =
                new in_array(the_aref->result_type(), operand(),
                             operand(new_ldc), the_aref->elem_size(),
                             the_aref->dims() - level, the_aref->offset());

        operand offset_op = operand_int(type_ptr_diff, 0);
        type_node *follow_type = new_var->type();
        unsigned num_dims = the_aref->dims() - level;
        for (unsigned dim_num = 0; dim_num < num_dims; ++dim_num)
          {
            operand index = the_aref->index(dim_num + level);
            operand bound = the_aref->bound(dim_num + level);
            index.remove();
            bound.remove();

            offset_op =
                    operand_multiply(type_ptr_diff, offset_op, bound.clone());

            new_aref->set_index(dim_num, index);
            new_aref->set_bound(dim_num, bound);

            if (!follow_type->unqual()->is_array())
              {
                error_line(1, the_aref->parent(),
                           "array reference instruction mismatches base "
                           "operand type");
                return NULL;
              }
            array_type *follow_array_type =
                    (array_type *)(follow_type->unqual());

            operand lower_bound =
                    operand_from_array_bound(follow_array_type->lower_bound());
            offset_op = operand_add(type_ptr_diff, offset_op, lower_bound);

            follow_type = follow_array_type->elem_type();
          }

        new_aref->set_offset_op(offset_op);

        replace_instruction(the_aref, new_aref);
        return NULL;
      }
    else
      {
        type_node *result_type = the_aref->result_type()->unqual();
        if (result_type->is_ptr())
          {
            ptr_type *result_ptr = (ptr_type *)result_type;
            result_type = result_ptr->ref_type();
          }
        else
          {
            result_type = NULL;
          }

        if ((the_aref->offset() == 0) &&
            (new_var->type()->is_same(result_type)))
          {
            return new_var;
          }
        else
          {
            in_ldc *new_ldc =
                    new in_ldc(the_aref->result_type(), operand(),
                               immed(new_var, the_aref->offset()));
            new_var->set_addr_taken();

            replace_instruction(the_aref, new_ldc);
            return NULL;
          }
      }
  }

static void make_eligible(var_sym *the_var)
  {
    type_node *the_type = the_var->type()->unqual();
    unsigned num_dims = 0;
    while (the_type->is_array())
      {
        array_type *the_array_type = (array_type *)the_type;
        if ((!the_array_type->lower_bound().is_constant()) ||
            (!the_array_type->upper_bound().is_constant()))
          {
            break;
          }
        the_type = the_array_type->elem_type()->unqual();
        ++num_dims;
      }

    immed_list *the_immeds = new immed_list;
    the_immeds->append(immed(num_dims));
    the_var->append_annote(k_eligibility_level, the_immeds);
  }

static void make_ineligible(var_sym *the_var)
  {
    immed_list *the_immeds =
            (immed_list *)(the_var->get_annote(k_eligibility_level));
    if (the_immeds != NULL)
        delete the_immeds;
  }

static unsigned eligibility_level(var_sym *the_var)
  {
    immed_list *the_immeds =
            (immed_list *)(the_var->peek_annote(k_eligibility_level));
    if (the_immeds == NULL)
        return 0;
    assert(!the_immeds->is_empty());
    immed value = the_immeds->head()->contents;
    assert(value.is_unsigned_int());
    return value.unsigned_int();
  }

static void limit_eligibility_level(var_sym *the_var, unsigned level)
  {
    if (level == 0)
      {
        make_ineligible(the_var);
        return;
      }

    immed_list *the_immeds =
            (immed_list *)(the_var->get_annote(k_eligibility_level));
    if (the_immeds == NULL)
        return;

    assert(!the_immeds->is_empty());
    immed value = the_immeds->head()->contents;
    assert(value.is_unsigned_int());

    unsigned new_level = value.unsigned_int();
    if (level < new_level)
        new_level = level;

    delete the_immeds;
    
    the_immeds = new immed_list;
    the_immeds->append(immed(new_level));
    the_var->append_annote(k_eligibility_level, the_immeds);
  }

static void set_replacement_list(var_sym *the_var, immed_list *replacements)
  {
    the_var->append_annote(k_scalar_replacements, replacements);
  }

static immed_list *get_replacement_list(var_sym *the_var)
  {
    return (immed_list *)(the_var->peek_annote(k_scalar_replacements));
  }

/*
 *  Move the first num_bits worth of initialization data from old_list
 *  to new_list.
 */
static void move_initialization_prefix(base_init_struct_list *new_list,
                                       base_init_struct_list *old_list,
                                       int num_bits)
  {
    int bits_remaining = num_bits;
    while ((bits_remaining > 0) && !old_list->is_empty())
      {
        base_init_struct *this_init_struct = old_list->head()->contents;

        if (this_init_struct->the_multi_init() != NULL)
          {
            multi_init_struct *the_multi_init =
                    this_init_struct->the_multi_init();

            immed_list *new_data = new immed_list;
            immed_list *old_data = the_multi_init->data;
            int size = the_multi_init->size;
            while ((bits_remaining > 0) && !old_data->is_empty())
              {
                if (bits_remaining < size)
                    error_line(1, NULL, "misalignment of initialization data");
                bits_remaining -= size;
                immed this_immed = old_data->pop();
                new_data->append(this_immed);
              }

            multi_init_struct *new_multi_init =
                    new multi_init_struct(size, new_data);
            new_list->append(new_multi_init);

            if (old_data->is_empty())
              {
                this_init_struct = old_list->pop();
                delete old_data;
                delete this_init_struct;
              }
          }
        else if (this_init_struct->the_repeat_init() != NULL)
          {
            repeat_init_struct *the_repeat_init =
                    this_init_struct->the_repeat_init();

            int size = the_repeat_init->size;
            immed data = the_repeat_init->data;

            int new_repetitions;
            if (bits_remaining < size * the_repeat_init->repetitions)
              {
                if (bits_remaining % size != 0)
                    error_line(1, NULL, "misalignment of initialization data");
                new_repetitions = bits_remaining / size;
                the_repeat_init->repetitions -= new_repetitions;
                bits_remaining = 0;
              }
            else
              {
                new_repetitions = the_repeat_init->repetitions;
                bits_remaining -= new_repetitions * size;

                this_init_struct = old_list->pop();
                delete this_init_struct;
              }

            repeat_init_struct *new_repeat_init =
                    new repeat_init_struct(new_repetitions, size, data);
            new_list->append(new_repeat_init);
          }
        else if (this_init_struct->the_fill_init() != NULL)
          {
            fill_init_struct *the_fill_init =
                    this_init_struct->the_fill_init();

            int data = the_fill_init->data;

            int fill_bits;
            if (bits_remaining < the_fill_init->size)
              {
                fill_bits = bits_remaining;
                the_fill_init->size -= fill_bits;
                bits_remaining = 0;
              }
            else
              {
                fill_bits = the_fill_init->size;
                bits_remaining -= fill_bits;

                this_init_struct = old_list->pop();
                delete this_init_struct;
              }

            fill_init_struct *new_fill_init =
                    new fill_init_struct(fill_bits, data);
            new_list->append(new_fill_init);
          }
      }
  }

static boolean base_types_ok_for_scalarization(type_node *type1,
                                               type_node *type2)
  {
    type_node *unqual1 = type1->unqual();
    type_node *unqual2 = type2->unqual();

    if (unqual1->is_same(unqual2))
        return TRUE;
    if (!unqual1->is_array())
        return FALSE;
    if (!unqual2->is_array())
        return FALSE;
    array_type *array1 = (array_type *)unqual1;
    array_type *array2 = (array_type *)unqual2;
    return (array1->elem_type()->is_same(array2->elem_type()) &&
            (array1->lower_bound() == array2->lower_bound()));
  }

/*----------------------------------------------------------------------*
    End Private Function Implementations
 *----------------------------------------------------------------------*/

}
