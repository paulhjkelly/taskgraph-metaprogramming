/* file "array_copy.cc" */

/*  Copyright (c) 1994 Stanford University

    All rights reserved.

    This software is provided under the terms described in
    the "suif_copyright.h" include file. */

#include <suif_copyright.h>

/*
 *  This file implements some pre-processing of the SUIF code to make
 *  sure there are no array copies, for the s2c program for the SUIF
 *  system.
 */

#define RCS_BASE_FILE array_copy_cc

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

#define TMP_STRUCT_PREFIX "__s2c_tmp_struct_"

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

static alist dummy_structs;

/*----------------------------------------------------------------------*
    End Private Global Variables
 *----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*
    Begin Private Function Declarations
 *----------------------------------------------------------------------*/

static void pass1_copied_type(type_node *the_type);
static void fix_array_memcopy(in_rrr *the_memcopy);
static ptr_type *array_ptr_to_struct_ptr(ptr_type *array_ptr);
static void add_dummy_struct(int size);
static struct_type *array_to_struct(array_type *the_array);

/*----------------------------------------------------------------------*
    End Private Function Declarations
 *----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*
    Begin Public Function Implementations
 *----------------------------------------------------------------------*/

/*
 *  This is called to install all needed dummy structures before
 *  fix_array_copy_on_instr() is called.  We do this in two steps so
 *  that all changes to the global symbol table can be made first.
 */
extern void pass1_array_copy_on_instr(instruction *the_instr)
  {
    if ((the_instr->opcode() == io_memcpy) || (the_instr->opcode() == io_str))
      {
        in_rrr *the_memop = (in_rrr *)the_instr;
        type_node *src1_type = the_memop->src1_op().type()->unqual();
        assert(src1_type->is_ptr());
        ptr_type *src1_ptr = (ptr_type *)src1_type;
        pass1_copied_type(src1_ptr->ref_type());
        return;
      }

    unsigned num_srcs = the_instr->num_srcs();
    for (unsigned src_num = 0; src_num < num_srcs; ++src_num)
      {
        operand this_op = the_instr->src_op(src_num);
        if (this_op.is_symbol())
            pass1_copied_type(this_op.type());
      }

    pass1_copied_type(the_instr->result_type());
  }

/*
 *  This is called to install dummy structures before
 *  fix_function_array_types() is called.
 */
extern void pass1_array_copy_on_symtab(base_symtab *the_symtab)
  {
    type_node_list_iter type_iter(the_symtab->types());
    while (!type_iter.is_empty())
      {
        type_node *this_type = type_iter.step();
        if (this_type->is_func())
          {
            func_type *this_func_type = (func_type *)this_type;

            pass1_copied_type(this_func_type->return_type());

            unsigned num_args = this_func_type->num_args();
            for (unsigned arg_num = 0; arg_num < num_args; ++arg_num)
                pass1_copied_type(this_func_type->arg_type(arg_num));
          }
      }

    if (the_symtab->is_proc())
      {
        proc_symtab *the_proc_symtab = (proc_symtab *)the_symtab;
        sym_node_list_iter param_iter(the_proc_symtab->params());
        while (!param_iter.is_empty())
          {
            sym_node *this_symbol = param_iter.step();

            assert(this_symbol->is_var());
            var_sym *this_var = (var_sym *)this_symbol;
            pass1_copied_type(this_var->type());
          }
      }
  }

/*
 *  In SUIF it is legal to have copy operations on arrays with known
 *  size, but in C it is not.  So we want to translate such array
 *  copies into constructs that are legal in C.  We've chosen to do so
 *  by taking the locations of the two objects, casting them to
 *  pointers to dummy structures of the correct size, and doing a
 *  structure copy.  One advantage of this approach over copying the
 *  array explicitly part-by-part is that the back-end C compiler will
 *  see the copy as a unit and do it as efficiently as it does
 *  structure copies.  Another is that the C code is easier to read
 *  and closer to the original SUIF code.
 */
extern void fix_array_copy_on_instr(instruction *the_instr)
  {
    if (the_instr->opcode() == io_memcpy)
      {
        in_rrr *the_memcopy = (in_rrr *)the_instr;
        type_node *src1_type = the_memcopy->src1_op().type()->unqual();
        assert(src1_type->is_ptr());
        ptr_type *src1_ptr = (ptr_type *)src1_type;
        if (src1_ptr->ref_type()->is_array())
            fix_array_memcopy(the_memcopy);
        return;
      }
    if (the_instr->opcode() == io_str)
      {
        in_rrr *the_store = (in_rrr *)the_instr;
        type_node *src1_type = the_store->src1_op().type()->unqual();
        assert(src1_type->is_ptr());
        ptr_type *src1_ptr = (ptr_type *)src1_type;
        if (src1_ptr->ref_type()->is_array())
          {
            operand dst_addr = the_store->dst_addr_op();
            dst_addr.remove();

            assert(dst_addr.type()->unqual()->is_ptr());
            ptr_type *old_ptr = (ptr_type *)(dst_addr.type()->unqual());
            ptr_type *new_ptr = array_ptr_to_struct_ptr(old_ptr);

            dst_addr = fold_real_1op_rrr(io_cvt, new_ptr, dst_addr);
            the_store->set_dst_addr_op(dst_addr);
          }
      }

    unsigned num_srcs = the_instr->num_srcs();
    for (unsigned src_num = 0; src_num < num_srcs; ++src_num)
      {
        operand this_op = the_instr->src_op(src_num);
        if (this_op.is_symbol() && this_op.type()->is_array())
          {
            var_sym *the_var = this_op.symbol();
            ptr_type *array_ptr = the_var->type()->ptr_to();
            in_ldc *the_ldc = new in_ldc(array_ptr, operand(), immed(the_var));
            ptr_type *struct_ptr = array_ptr_to_struct_ptr(array_ptr);
            this_op = fold_real_1op_rrr(io_cvt, struct_ptr, operand(the_ldc));
            this_op = operand(new in_rrr(io_lod, struct_ptr->ref_type(),
                                         operand(), this_op));
            the_instr->set_src_op(src_num, this_op);
          }
      }

    if (the_instr->result_type()->is_array())
      {
        array_type *the_array = (array_type *)(the_instr->result_type());
        struct_type *the_struct = array_to_struct(the_array);

        if (the_instr->opcode() == io_lod)
          {
            type_node *new_ptr = the_struct->ptr_to();
            in_rrr *the_load = (in_rrr *)the_instr;
            operand addr_op = the_load->src_op();
            addr_op.remove();
            addr_op = fold_real_1op_rrr(io_cvt, new_ptr, addr_op);
            the_load->set_src(addr_op);
          }

        the_instr->set_result_type(the_struct);

        if (the_instr->dst_op().is_symbol())
          {
            var_sym *dest_var = the_instr->dst_op().symbol();
            type_node *old_ptr = dest_var->type()->ptr_to();
            type_node *new_ptr = the_struct->ptr_to();
            in_ldc *dest_addr =
                    new in_ldc(old_ptr, operand(), immed(dest_var));
            operand dest_op =
                    fold_real_1op_rrr(io_cvt, new_ptr, operand(dest_addr));
            in_rrr *the_store =
                    new in_rrr(io_str, type_void, operand(), dest_op,
                               operand());
            the_instr->set_dst(operand());
            replace_instruction(the_instr, the_store);
            the_store->set_src2(operand(the_instr));
          }
      }
  }

/*
 *  In C it is also illegal to pass arrays to functions or to have
 *  functions return array types.  The previous function changed all
 *  the instructions for these cases to use structures of the same
 *  size, but the function types have to be changed too; that is what
 *  is done here.  We must also change the type of any parameter
 *  symbols that are arrays.
 */
extern void fix_function_array_types(base_symtab *the_symtab)
  {
    type_node_list_iter type_iter(the_symtab->types());
    while (!type_iter.is_empty())
      {
        type_node *this_type = type_iter.step();
        if (this_type->is_func())
          {
            func_type *this_func_type = (func_type *)this_type;

            if (this_func_type->return_type()->is_array())
              {
                array_type *the_array =
                        (array_type *)(this_func_type->return_type());
                this_func_type->set_return_type(array_to_struct(the_array));
              }

            unsigned num_args = this_func_type->num_args();
            for (unsigned arg_num = 0; arg_num < num_args; ++arg_num)
              {
                type_node *this_arg = this_func_type->arg_type(arg_num);
                if (this_arg->is_array())
                  {
                    array_type *the_array = (array_type *)this_arg;
                    this_func_type->set_arg_type(arg_num,
                                                 array_to_struct(the_array));
                  }
              }
          }
      }

    if (the_symtab->is_proc())
      {
        proc_symtab *the_proc_symtab = (proc_symtab *)the_symtab;
        sym_node_list_iter param_iter(the_proc_symtab->params());
        while (!param_iter.is_empty())
          {
            sym_node *this_symbol = param_iter.step();

            assert(this_symbol->is_var());
            var_sym *this_var = (var_sym *)this_symbol;
            if (this_var->type()->is_array())
              {
                array_type *the_array = (array_type *)(this_var->type());
                struct_type *the_struct = array_to_struct(the_array);
                this_var->set_type(the_struct);
              }
          }
      }
  }

/*----------------------------------------------------------------------*
    End Public Function Implementations
 *----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*
    Begin Private Function Implementations
 *----------------------------------------------------------------------*/

static void pass1_copied_type(type_node *the_type)
  {
    if (the_type->unqual()->is_array())
        add_dummy_struct(the_type->size());
  }

static void fix_array_memcopy(in_rrr *the_memcopy)
  {
    operand op1 = the_memcopy->src1_op();
    operand op2 = the_memcopy->src2_op();

    op1.remove();
    op2.remove();

    assert(op1.type()->unqual()->is_ptr());
    ptr_type *old_ptr = (ptr_type *)(op1.type()->unqual());
    ptr_type *new_ptr = array_ptr_to_struct_ptr(old_ptr);

    op1 = fold_real_1op_rrr(io_cvt, new_ptr, op1);
    op2 = fold_real_1op_rrr(io_cvt, new_ptr, op2);

    the_memcopy->set_src1(op1);
    the_memcopy->set_src2(op2);
  }

static ptr_type *array_ptr_to_struct_ptr(ptr_type *array_ptr)
  {
    assert(array_ptr->ref_type()->is_array());
    array_type *the_array = (array_type *)(array_ptr->ref_type());
    struct_type *the_struct = array_to_struct(the_array);
    return the_struct->ptr_to();
  }

static void add_dummy_struct(int size)
  {
    static char name_buffer[sizeof(TMP_STRUCT_PREFIX) + sizeof(int) * 3 + 1];

    assert(size != 0);

    struct_type *existing_type;
    boolean type_exists =
            dummy_structs.exists((void *)(((char *)0) + size),
                                 (void**)&existing_type);
    if (type_exists)
        return;

    sprintf(name_buffer, "%s%d", TMP_STRUCT_PREFIX, size);
    struct_type *the_struct =
            new struct_type(TYPE_STRUCT, size, name_buffer, 0);
    type_node *installed_struct = fileset->globals()->install_type(the_struct);
    the_struct = (struct_type *)installed_struct;

    dummy_structs.enter((void *)(((char *)0) + size), the_struct);
  }

static struct_type *array_to_struct(array_type *the_array)
  {
    int size = the_array->size();
    assert(size != 0);

    struct_type *existing_type;
    boolean type_exists =
            dummy_structs.exists((void *)(((char *)0) + size),
                                 (void **)&existing_type);
    assert(type_exists);
    return existing_type;
  }

/*----------------------------------------------------------------------*
    End Private Function Implementations
 *----------------------------------------------------------------------*/
