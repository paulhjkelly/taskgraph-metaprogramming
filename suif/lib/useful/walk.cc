/* file "walk.cc" */

/*  Copyright (c) 1995 Stanford University

    All rights reserved.

    This software is provided under the terms described in
    the "suif_copyright.h" include file. */

#include <suif_copyright.h>

/*
 *  This file implements routines to walk over SUIF code.
 */

#define _MODULE_ "libuseful.a"

#define RCS_BASE_FILE useful_walk_cc

#include "useful_internal.h"
#include <climits>
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

enum walk_frame_tag
  {
    WALK_SUIF_OBJ, WALK_ANNOTE_LIST, WALK_AN_ANNOTE, WALK_IMMED_LIST,
    WALK_AN_IMMED, WALK_OPERAND, WALK_NODE_LIST, WALK_FILE_SET_ENTRY,
    WALK_TREE_NODE, WALK_TREE_INSTR, WALK_TREE_LOOP, WALK_TREE_FOR,
    WALK_TREE_IF, WALK_TREE_BLOCK, WALK_INSTRUCTION, WALK_LDC_INSTR,
    WALK_BJ_INSTR, WALK_MBR_INSTR, WALK_LAB_INSTR, WALK_A_SYMTAB,
    WALK_A_SYMBOL, WALK_A_VARIABLE, WALK_PROC_SYM, WALK_VAR_DEF, WALK_A_TYPE,
    WALK_ARRAY_TYPE
  };

enum walk_so_tag
  { WALK_SO_PRE, WALK_SO_ANNOTES, WALK_SO_CHILDREN, WALK_SO_POST,
    WALK_SO_DONE };

enum walk_an_annote_tag
  { WALK_ANNOTE_PRE, WALK_ANNOTE_IMMEDS, WALK_ANNOTE_POST, WALK_ANNOTE_DONE };

enum walk_an_immed_tag
  { WALK_IMMED_CHILDREN, WALK_IMMED_DONE };

enum walk_operand_tag
  { WALK_OP_PRE, WALK_OP_CHILDREN, WALK_OP_POST, WALK_OP_DONE };

enum walk_file_set_entry_tag
  { WALK_FSE_SYMTAB, WALK_FSE_DONE };

enum walk_tree_instr_tag
  { WALK_TI_INSTR, WALK_TI_DONE };

enum walk_tree_loop_tag
  { WALK_LOOP_CONTINUE, WALK_LOOP_BREAK, WALK_LOOP_TOP, WALK_LOOP_DONE };

enum walk_tree_for_tag
  { WALK_FOR_INDEX, WALK_FOR_CONTINUE, WALK_FOR_BREAK, WALK_FOR_DONE };

enum walk_tree_if_tag
  { WALK_IF_JUMPTO, WALK_IF_DONE };

enum walk_tree_block_tag
  { WALK_TB_SYMTAB, WALK_TB_PROC_SYM, WALK_TB_DONE };

enum walk_instruction_tag
  { WALK_INST_SRCS, WALK_INST_DST, WALK_INST_RESULT_TYPE, WALK_INST_CHILDREN,
    WALK_INST_DONE };

enum walk_ldc_instr_tag
  { WALK_LDC_IMMED, WALK_LDC_DONE };

enum walk_bj_instr_tag
  { WALK_BJ_TARGET, WALK_BJ_DONE };

enum walk_mbr_instr_tag
  { WALK_MBR_LABELS, WALK_MBR_DEFAULT, WALK_MBR_DONE };

enum walk_lab_instr_tag
  { WALK_LAB_SYMBOL, WALK_LAB_DONE };

enum walk_a_symtab_tag
  { WALK_SYMTAB_SYMBOLS, WALK_SYMTAB_TYPES, WALK_SYMTAB_DEFS,
    WALK_SYMTAB_DONE };

enum walk_a_symbol_tag
  { WALK_SYMBOL_CHILDREN, WALK_SYMBOL_DONE };

enum walk_a_variable_tag
  { WALK_VAR_TYPE, WALK_VAR_PARENT, WALK_VAR_CHILDREN, WALK_VAR_DONE };

enum walk_proc_sym_tag
  { WALK_PROC_TYPE, WALK_PROC_DONE };

enum walk_var_def_tag
  { WALK_DEF_VAR, WALK_DEF_DONE };

enum walk_a_type_tag
  { WALK_TYPE_REF_TYPES, WALK_TYPE_CHILDREN, WALK_TYPE_DONE };

enum walk_array_type_tag
  { WALK_ARRAY_LOWER, WALK_ARRAY_UPPER, WALK_ARRAY_DONE };


class walk_frame_so_data
  {
public:
    walk_so_tag tag;
    suif_object *the_object;
  };

class walk_frame_annote_list_data
  {
public:
    annote_list_e *previous;
    annote_list_e *next;
  };

class walk_frame_an_annote_data
  {
public:
    walk_an_annote_tag tag;
    annote *the_annote;
    immed_list *saved_immeds;
  };

class walk_frame_immed_list_data
  {
public:
    immed_list_e *previous;
    immed_list_e *next;
  };

class walk_frame_an_immed_data
  {
public:
    walk_an_immed_tag tag;
    immed_dataonly the_immed;
  };

class walk_frame_operand_data
  {
public:
    walk_operand_tag tag;
    operand_dataonly the_op;
    boolean is_destination;
  };

class walk_frame_node_list_data
  {
public:
    tree_node_list_e *previous;
    tree_node_list_e *next;
  };

class walk_frame_file_set_entry_data
  {
public:
    walk_file_set_entry_tag tag;
    file_set_entry *the_fse;
  };

class walk_frame_tree_node_data
  {
public:
    tree_node *the_node;
    unsigned child_list_num;
  };

class walk_frame_tree_instr_data
  {
public:
    walk_tree_instr_tag tag;
    tree_instr *the_tree_instr;
  };

class walk_frame_tree_loop_data
  {
public:
    walk_tree_loop_tag tag;
    tree_loop *the_loop;
  };

class walk_frame_tree_for_data
  {
public:
    walk_tree_for_tag tag;
    tree_for *the_for;
  };

class walk_frame_tree_if_data
  {
public:
    walk_tree_if_tag tag;
    tree_if *the_if;
  };

class walk_frame_tree_block_data
  {
public:
    walk_tree_block_tag tag;
    tree_block *the_block;
  };

class walk_frame_instruction_data
  {
public:
    walk_instruction_tag tag;
    instruction *the_instr;
    unsigned src_num;
  };

class walk_frame_ldc_instr_data
  {
public:
    walk_ldc_instr_tag tag;
    in_ldc *the_ldc;
  };

class walk_frame_bj_instr_data
  {
public:
    walk_bj_instr_tag tag;
    in_bj *the_bj;
  };

class walk_frame_mbr_instr_data
  {
public:
    walk_mbr_instr_tag tag;
    in_mbr *the_mbr;
    unsigned lab_num;
  };

class walk_frame_lab_instr_data
  {
public:
    walk_lab_instr_tag tag;
    in_lab *the_lab;
  };

class walk_frame_a_symtab_data
  {
public:
    walk_a_symtab_tag tag;
    base_symtab *the_symtab;
    union
      {
        sym_node_list_e *sym_e;
        var_def_list_e *def_e;
        type_node_list_e *type_e;
      } previous;
    union
      {
        sym_node_list_e *sym_e;
        var_def_list_e *def_e;
        type_node_list_e *type_e;
      } next;
  };

class walk_frame_a_symbol_data
  {
public:
    walk_a_symbol_tag tag;
    sym_node *the_sym;
  };

class walk_frame_a_variable_data
  {
public:
    walk_a_variable_tag tag;
    var_sym *the_var;
    unsigned child_num;
  };

class walk_frame_proc_sym_data
  {
public:
    walk_proc_sym_tag tag;
    proc_sym *the_proc;
  };

class walk_frame_var_def_data
  {
public:
    walk_var_def_tag tag;
    var_def *the_def;
  };

class walk_frame_a_type_data
  {
public:
    walk_a_type_tag tag;
    type_node *the_type;
    unsigned ref_type_num;
  };

class walk_frame_array_type_data
  {
public:
    walk_array_type_tag tag;
    array_type *the_array_type;
  };


class walk_frame_data
  {
public:
    walk_frame_tag tag;
    union
      {
        walk_frame_so_data so_data;
        walk_frame_annote_list_data annote_list_data;
        walk_frame_an_annote_data an_annote_data;
        walk_frame_immed_list_data immed_list_data;
        walk_frame_an_immed_data an_immed_data;
        walk_frame_operand_data operand_data;
        walk_frame_node_list_data node_list_data;
        walk_frame_file_set_entry_data file_set_entry_data;
        walk_frame_tree_node_data tree_node_data;
        walk_frame_tree_instr_data tree_instr_data;
        walk_frame_tree_loop_data tree_loop_data;
        walk_frame_tree_for_data tree_for_data;
        walk_frame_tree_if_data tree_if_data;
        walk_frame_tree_block_data tree_block_data;
        walk_frame_instruction_data instruction_data;
        walk_frame_ldc_instr_data ldc_instr_data;
        walk_frame_bj_instr_data bj_instr_data;
        walk_frame_mbr_instr_data mbr_instr_data;
        walk_frame_lab_instr_data lab_instr_data;
        walk_frame_a_symtab_data a_symtab_data;
        walk_frame_a_symbol_data a_symbol_data;
        walk_frame_a_variable_data a_variable_data;
        walk_frame_proc_sym_data proc_sym_data;
        walk_frame_var_def_data var_def_data;
        walk_frame_a_type_data a_type_data;
        walk_frame_array_type_data array_type_data;
      } u;

    walk_frame_data(suif_object *the_object)
      {
        tag = WALK_SUIF_OBJ;
        u.so_data.tag = WALK_SO_PRE;
        u.so_data.the_object = the_object;
        push_clue(the_object);
      }
    walk_frame_data(annote_list_e *the_elem)
      {
        tag = WALK_ANNOTE_LIST;
        u.annote_list_data.previous = NULL;
        u.annote_list_data.next = the_elem;
      }
    walk_frame_data(annote *the_annote)
      {
        tag = WALK_AN_ANNOTE;
        u.an_annote_data.tag = WALK_ANNOTE_PRE;
        u.an_annote_data.the_annote = the_annote;
        u.an_annote_data.saved_immeds = NULL;
        push_clue(the_annote);
      }
    walk_frame_data(immed_list_e *the_elem)
      {
        tag = WALK_IMMED_LIST;
        u.immed_list_data.previous = NULL;
        u.immed_list_data.next = the_elem;
      }
    walk_frame_data(immed the_immed)
      {
        tag = WALK_AN_IMMED;
        u.an_immed_data.tag = WALK_IMMED_CHILDREN;
        u.an_immed_data.the_immed = the_immed;
      }
    walk_frame_data(operand the_op)
      {
        tag = WALK_OPERAND;
        u.operand_data.tag = WALK_OP_PRE;
        u.operand_data.the_op = the_op;
        u.operand_data.is_destination = FALSE;
      }
    walk_frame_data(operand the_op, boolean is_destination)
      {
        tag = WALK_OPERAND;
        u.operand_data.tag = WALK_OP_PRE;
        u.operand_data.the_op = the_op;
        u.operand_data.is_destination = is_destination;
      }
    walk_frame_data(tree_node_list_e *the_elem)
      {
        tag = WALK_NODE_LIST;
        u.node_list_data.previous = NULL;
        u.node_list_data.next = the_elem;
      }
    walk_frame_data(walk_frame_tag init_tag, file_set_entry *the_fse)
      {
        assert(init_tag == WALK_FILE_SET_ENTRY);
        tag = WALK_FILE_SET_ENTRY;
        u.file_set_entry_data.tag = WALK_FSE_SYMTAB;
        u.file_set_entry_data.the_fse = the_fse;
      }
    walk_frame_data(walk_frame_tag init_tag, tree_node *the_node)
      {
        assert(init_tag == WALK_TREE_NODE);
        tag = WALK_TREE_NODE;
        u.tree_node_data.the_node = the_node;
        u.tree_node_data.child_list_num = 0;
      }
    walk_frame_data(tree_instr *the_tree_instr, walk_frame_tag init_tag)
      {
        assert(init_tag == WALK_TREE_INSTR);
        tag = WALK_TREE_INSTR;
        u.tree_instr_data.tag = WALK_TI_INSTR;
        u.tree_instr_data.the_tree_instr = the_tree_instr;
      }
    walk_frame_data(tree_loop *the_loop, walk_frame_tag init_tag)
      {
        assert(init_tag == WALK_TREE_LOOP);
        tag = WALK_TREE_LOOP;
        u.tree_loop_data.tag = WALK_LOOP_CONTINUE;
        u.tree_loop_data.the_loop = the_loop;
      }
    walk_frame_data(tree_for *the_for, walk_frame_tag init_tag)
      {
        assert(init_tag == WALK_TREE_FOR);
        tag = WALK_TREE_FOR;
        u.tree_for_data.tag = WALK_FOR_INDEX;
        u.tree_for_data.the_for = the_for;
      }
    walk_frame_data(tree_if *the_if, walk_frame_tag init_tag)
      {
        assert(init_tag == WALK_TREE_IF);
        tag = WALK_TREE_IF;
        u.tree_if_data.tag = WALK_IF_JUMPTO;
        u.tree_if_data.the_if = the_if;
      }
    walk_frame_data(tree_block *the_block, walk_frame_tag init_tag)
      {
        assert(init_tag == WALK_TREE_BLOCK);
        tag = WALK_TREE_BLOCK;
        u.tree_block_data.tag = WALK_TB_SYMTAB;
        u.tree_block_data.the_block = the_block;
      }
    walk_frame_data(walk_frame_tag init_tag, instruction *the_instr)
      {
        assert(init_tag == WALK_INSTRUCTION);
        tag = WALK_INSTRUCTION;
        u.instruction_data.tag = WALK_INST_SRCS;
        u.instruction_data.the_instr = the_instr;
        u.instruction_data.src_num = 0;
      }
    walk_frame_data(in_ldc *the_ldc, walk_frame_tag init_tag)
      {
        assert(init_tag == WALK_LDC_INSTR);
        tag = WALK_LDC_INSTR;
        u.ldc_instr_data.tag = WALK_LDC_IMMED;
        u.ldc_instr_data.the_ldc = the_ldc;
      }
    walk_frame_data(in_bj *the_bj, walk_frame_tag init_tag)
      {
        assert(init_tag == WALK_BJ_INSTR);
        tag = WALK_BJ_INSTR;
        u.bj_instr_data.tag = WALK_BJ_TARGET;
        u.bj_instr_data.the_bj = the_bj;
      }
    walk_frame_data(in_mbr *the_mbr, walk_frame_tag init_tag)
      {
        assert(init_tag == WALK_MBR_INSTR);
        tag = WALK_MBR_INSTR;
        u.mbr_instr_data.tag = WALK_MBR_LABELS;
        u.mbr_instr_data.the_mbr = the_mbr;
        u.mbr_instr_data.lab_num = 0;
      }
    walk_frame_data(in_lab *the_lab, walk_frame_tag init_tag)
      {
        assert(init_tag == WALK_LAB_INSTR);
        tag = WALK_LAB_INSTR;
        u.lab_instr_data.tag = WALK_LAB_SYMBOL;
        u.lab_instr_data.the_lab = the_lab;
      }
    walk_frame_data(walk_frame_tag init_tag, base_symtab *the_symtab)
      {
        assert(init_tag == WALK_A_SYMTAB);
        tag = WALK_A_SYMTAB;
        u.a_symtab_data.tag = WALK_SYMTAB_SYMBOLS;
        u.a_symtab_data.the_symtab = the_symtab;
        u.a_symtab_data.previous.sym_e = NULL;
        u.a_symtab_data.next.sym_e = the_symtab->symbols()->head();
      }
    walk_frame_data(walk_frame_tag init_tag, sym_node *the_sym)
      {
        assert(init_tag == WALK_A_SYMBOL);
        tag = WALK_A_SYMBOL;
        u.a_symbol_data.tag = WALK_SYMBOL_CHILDREN;
        u.a_symbol_data.the_sym = the_sym;
      }
    walk_frame_data(var_sym *the_var, walk_frame_tag init_tag)
      {
        assert(init_tag == WALK_A_VARIABLE);
        tag = WALK_A_VARIABLE;
        u.a_variable_data.tag = WALK_VAR_TYPE;
        u.a_variable_data.the_var = the_var;
        u.a_variable_data.child_num = 0;
      }
    walk_frame_data(proc_sym *the_proc, walk_frame_tag init_tag)
      {
        assert(init_tag == WALK_PROC_SYM);
        tag = WALK_PROC_SYM;
        u.proc_sym_data.tag = WALK_PROC_TYPE;
        u.proc_sym_data.the_proc = the_proc;
      }
    walk_frame_data(walk_frame_tag init_tag, var_def *the_def)
      {
        assert(init_tag == WALK_VAR_DEF);
        tag = WALK_VAR_DEF;
        u.var_def_data.tag = WALK_DEF_VAR;
        u.var_def_data.the_def = the_def;
      }
    walk_frame_data(walk_frame_tag init_tag, type_node *the_type)
      {
        assert(init_tag == WALK_A_TYPE);
        tag = WALK_A_TYPE;
        u.a_type_data.tag = WALK_TYPE_REF_TYPES;
        u.a_type_data.the_type = the_type;
        u.a_type_data.ref_type_num = 0;
      }
    walk_frame_data(array_type *the_array_type, walk_frame_tag init_tag)
      {
        assert(init_tag == WALK_ARRAY_TYPE);
        tag = WALK_ARRAY_TYPE;
        u.array_type_data.tag = WALK_ARRAY_LOWER;
        u.array_type_data.the_array_type = the_array_type;
      }

    ~walk_frame_data(void)
      {
        if (tag == WALK_SUIF_OBJ)
          {
            pop_clue(u.so_data.the_object);
          }
        else if (tag == WALK_AN_ANNOTE)
          {
            pop_clue(u.an_annote_data.the_annote);
            immed_list *saved_immeds = u.an_annote_data.saved_immeds;
            if (saved_immeds != NULL)
              {
                /* @@@ NULL for suif_object in next line isn't good */
                u.an_annote_data.the_annote->set_immeds(saved_immeds, NULL);
                delete saved_immeds;
              }
          }
      }
  };


DECLARE_DLIST_CLASS(walk_frame_data_list, walk_frame_data *);

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

/*----------------------------------------------------------------------*
    End Private Function Declarations
 *----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*
    Begin Public Function Implementations
 *----------------------------------------------------------------------*/

extern void walk(suif_object *the_object, simple_so_walkee *the_walkee)
  {
    so_walker the_walker;
    the_walker.walk(the_object, the_walkee);
  }

extern void walk(suif_object *the_object, basic_so_walkee *the_walkee)
  {
    so_walker the_walker;
    the_walker.walk(the_object, the_walkee);
  }

extern void walk(suif_object *the_object, simple_annote_walkee *the_walkee)
  {
    so_walker the_walker;
    the_walker.walk(the_object, the_walkee);
  }

extern void walk(suif_object *the_object, basic_annote_walkee *the_walkee)
  {
    so_walker the_walker;
    the_walker.walk(the_object, the_walkee);
  }

extern void walk(suif_object *the_object, simple_sym_walkee *the_walkee)
  {
    so_walker the_walker;
    the_walker.walk(the_object, the_walkee);
  }

extern void walk(suif_object *the_object, basic_sym_walkee *the_walkee)
  {
    so_walker the_walker;
    the_walker.walk(the_object, the_walkee);
  }

extern void walk(suif_object *the_object, simple_type_walkee *the_walkee)
  {
    so_walker the_walker;
    the_walker.walk(the_object, the_walkee);
  }

extern void walk(suif_object *the_object, basic_type_walkee *the_walkee)
  {
    so_walker the_walker;
    the_walker.walk(the_object, the_walkee);
  }

extern void walk(suif_object *the_object, simple_op_walkee *the_walkee)
  {
    so_walker the_walker;
    the_walker.walk(the_object, the_walkee);
  }

extern void walk(suif_object *the_object, basic_op_walkee *the_walkee)
  {
    so_walker the_walker;
    the_walker.walk(the_object, the_walkee);
  }


extern any_type walk(suif_object *the_object, simple_so_walkee *the_walkee,
                     any_type default_result)
  {
    so_walker the_walker;
    return the_walker.walk(the_object, the_walkee, default_result);
  }

extern any_type walk(suif_object *the_object, basic_so_walkee *the_walkee,
                     any_type default_result)
  {
    so_walker the_walker;
    return the_walker.walk(the_object, the_walkee, default_result);
  }

extern any_type walk(suif_object *the_object, simple_annote_walkee *the_walkee,
                     any_type default_result)
  {
    so_walker the_walker;
    return the_walker.walk(the_object, the_walkee, default_result);
  }

extern any_type walk(suif_object *the_object, basic_annote_walkee *the_walkee,
                     any_type default_result)
  {
    so_walker the_walker;
    return the_walker.walk(the_object, the_walkee, default_result);
  }

extern any_type walk(suif_object *the_object, simple_sym_walkee *the_walkee,
                     any_type default_result)
  {
    so_walker the_walker;
    return the_walker.walk(the_object, the_walkee, default_result);
  }

extern any_type walk(suif_object *the_object, basic_sym_walkee *the_walkee,
                     any_type default_result)
  {
    so_walker the_walker;
    return the_walker.walk(the_object, the_walkee, default_result);
  }

extern any_type walk(suif_object *the_object, simple_type_walkee *the_walkee,
                     any_type default_result)
  {
    so_walker the_walker;
    return the_walker.walk(the_object, the_walkee, default_result);
  }

extern any_type walk(suif_object *the_object, basic_type_walkee *the_walkee,
                     any_type default_result)
  {
    so_walker the_walker;
    return the_walker.walk(the_object, the_walkee, default_result);
  }

extern any_type walk(suif_object *the_object, simple_op_walkee *the_walkee,
                     any_type default_result)
  {
    so_walker the_walker;
    return the_walker.walk(the_object, the_walkee, default_result);
  }

extern any_type walk(suif_object *the_object, basic_op_walkee *the_walkee,
                     any_type default_result)
  {
    so_walker the_walker;
    return the_walker.walk(the_object, the_walkee, default_result);
  }


/*----------------------------------------------------------------------*
    End Public Function Implementations
 *----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*
    Begin Public Method Implementations
 *----------------------------------------------------------------------*/

so_walker::so_walker(void)
  {
    walk_stack = NULL;
    result_defined = FALSE;
    first_data_defined = 0;
    remainder_length = 0;

    leaf_simple_sym_walkee = NULL;
    leaf_basic_sym_walkee = NULL;
    leaf_simple_type_walkee = NULL;
    leaf_basic_type_walkee = NULL;

    pre_simple_so_walkee = NULL;
    pre_basic_so_walkee = NULL;
    pre_simple_annote_walkee = NULL;
    pre_basic_annote_walkee = NULL;
    pre_simple_op_walkee = NULL;
    pre_basic_op_walkee = NULL;

    post_simple_so_walkee = NULL;
    post_basic_so_walkee = NULL;
    post_simple_annote_walkee = NULL;
    post_basic_annote_walkee = NULL;
    post_simple_op_walkee = NULL;
    post_basic_op_walkee = NULL;
  }

so_walker::~so_walker(void)
  {
    assert(walk_stack == NULL);

    if (remainder_length != 0)
      {
        delete[] remainder_data_defined;
        delete[] remainder_values;
      }
  }


void so_walker::set_result(any_type new_value)
  {
    result_defined = TRUE;
    result_value = new_value;
  }

any_type so_walker::get_result(void)
  {
    assert(result_defined);
    return result_value;
  }

void so_walker::set_data(unsigned data_num, any_type new_value)
  {
    if (data_num < 4)
      {
        first_data_defined |= (1 << data_num);
        first_values[data_num] = new_value;
      }
    else
      {
        unsigned remainder_offset = data_num - 4;
        if (remainder_offset >= remainder_length)
          {
            any_type *new_values = new any_type[remainder_offset + 1];
            unsigned char *new_defined =
                    new unsigned char[(remainder_offset / CHAR_BIT) + 1];
            if (remainder_length != 0)
              {
                memcpy(new_values, remainder_values,
                       remainder_length * sizeof(any_type));
                memcpy(new_defined, remainder_data_defined,
                       (remainder_length + CHAR_BIT - 1) / CHAR_BIT);
              }
            remainder_data_defined = new_defined;
            remainder_values = new_values;
            remainder_length = remainder_offset + 1;
          }
        remainder_data_defined[remainder_offset / CHAR_BIT] |=
                (1 << (remainder_offset % CHAR_BIT));
        remainder_values[remainder_offset] = new_value;
      }
  }

any_type so_walker::get_data(unsigned data_num)
  {
    if (data_num < 4)
      {
        assert((first_data_defined & (1 << data_num)) != 0);
        return first_values[data_num];
      }
    else
      {
        unsigned remainder_offset = data_num - 4;
        assert(remainder_offset < remainder_length);
        assert((remainder_data_defined[remainder_offset / CHAR_BIT] &
                (1 << (remainder_offset % CHAR_BIT))) != 0);
        return remainder_values[remainder_offset];
      }
  }


void so_walker::walk(suif_object *the_object)
  {
    assert(walk_stack == NULL);

    walk_stack = new walk_frame_data_list;
    walk_stack->push(new walk_frame_data(the_object));

    while (!walk_stack->is_empty())
      {
        walk_frame_data *stack_top = walk_stack->head()->contents;
        switch (stack_top->tag)
          {
            case WALK_SUIF_OBJ:
              {
                walk_frame_so_data *this_so_data = &(stack_top->u.so_data);
                suif_object *this_object = this_so_data->the_object;
                switch (this_so_data->tag)
                  {
                    case WALK_SO_PRE:
                      {
                        this_so_data->tag = WALK_SO_ANNOTES;
                        if (pre_simple_so_walkee != NULL)
                            pre_simple_so_walkee(this_object);
                        else if (pre_basic_so_walkee != NULL)
                            pre_basic_so_walkee(this_object, this);
                        break;
                      }
                    case WALK_SO_ANNOTES:
                      {
                        this_so_data->tag = WALK_SO_CHILDREN;
                        annote_list *annotes = this_object->annotes();
                        walk_stack->push(new walk_frame_data(annotes->head()));
                        break;
                      }
                    case WALK_SO_CHILDREN:
                      {
                        this_so_data->tag = WALK_SO_POST;
                        walk_frame_data *new_data;
                        switch (this_object->object_kind())
                          {
                            case FILE_OBJ:
                              {
                                file_set_entry *this_fse =
                                        (file_set_entry *)this_object;
                                new_data =
                                        new walk_frame_data(
                                                WALK_FILE_SET_ENTRY, this_fse);
                                break;
                              }
                            case TREE_OBJ:
                              {
                                tree_node *this_node =
                                        (tree_node *)this_object;
                                new_data =
                                        new walk_frame_data(WALK_TREE_NODE,
                                                            this_node);
                                break;
                              }
                            case INSTR_OBJ:
                              {
                                instruction *this_instr =
                                        (instruction *)this_object;
                                new_data =
                                        new walk_frame_data(WALK_INSTRUCTION,
                                                            this_instr);
                                break;
                              }
                            case SYMTAB_OBJ:
                              {
                                base_symtab *this_symtab =
                                        (base_symtab *)this_object;
                                new_data =
                                        new walk_frame_data(WALK_A_SYMTAB,
                                                            this_symtab);
                                break;
                              }
                            case SYM_OBJ:
                              {
                                sym_node *this_sym = (sym_node *)this_object;
                                new_data =
                                        new walk_frame_data(WALK_A_SYMBOL,
                                                            this_sym);
                                break;
                              }
                            case DEF_OBJ:
                              {
                                var_def *this_def = (var_def *)this_object;
                                new_data =
                                        new walk_frame_data(WALK_VAR_DEF,
                                                            this_def);
                                break;
                              }
                            case TYPE_OBJ:
                              {
                                type_node *this_type =
                                        (type_node *)this_object;
                                new_data =
                                        new walk_frame_data(WALK_A_TYPE,
                                                            this_type);
                                break;
                              }
                            default:
				new_data = NULL;
                                assert(FALSE);
                          }
                        walk_stack->push(new_data);
                        break;
                      }
                    case WALK_SO_POST:
                      {
                        this_so_data->tag = WALK_SO_DONE;
                        if (post_simple_so_walkee != NULL)
                            post_simple_so_walkee(this_object);
                        else if (post_basic_so_walkee != NULL)
                            post_basic_so_walkee(this_object, this);
                        break;
                      }
                    case WALK_SO_DONE:
                      {
                        stack_top = walk_stack->pop();
                        delete stack_top;
                        break;
                      }
                    default:
                        assert(FALSE);
                  }
                break;
              }
            case WALK_ANNOTE_LIST:
              {
                walk_frame_annote_list_data *this_annote_list_data =
                        &(stack_top->u.annote_list_data);
                annote_list_e *the_elem = this_annote_list_data->next;
                if (the_elem != NULL)
                  {
                    this_annote_list_data->previous = the_elem;
                    this_annote_list_data->next = the_elem->next();
                    annote *this_annote = the_elem->contents;
                    assert(this_annote != NULL);
                    walk_stack->push(new walk_frame_data(this_annote));
                  }
                else
                  {
                    stack_top = walk_stack->pop();
                    delete stack_top;
                  }
                break;
              }
            case WALK_AN_ANNOTE:
              {
                walk_frame_an_annote_data *this_annote_data =
                        &(stack_top->u.an_annote_data);
                annote *this_annote = this_annote_data->the_annote;
                switch (this_annote_data->tag)
                  {
                    case WALK_ANNOTE_PRE:
                      {
                        this_annote_data->tag = WALK_ANNOTE_IMMEDS;
                        if (pre_simple_annote_walkee != NULL)
                            pre_simple_annote_walkee(this_annote);
                        else if (pre_basic_annote_walkee != NULL)
                            pre_basic_annote_walkee(this_annote, this);
                        break;
                      }
                    case WALK_ANNOTE_IMMEDS:
                      {
                        this_annote_data->tag = WALK_ANNOTE_POST;
                        immed_list *immeds = this_annote->immeds();
                        if (immeds != NULL)
                          {
                            walk_stack->push(new walk_frame_data(
                                                         immeds->head()));
                            if (this_annote->is_structured())
                                this_annote_data->saved_immeds = immeds;
                          }
                        break;
                      }
                    case WALK_ANNOTE_POST:
                      {
                        this_annote_data->tag = WALK_ANNOTE_DONE;
                        if (post_simple_annote_walkee != NULL)
                            post_simple_annote_walkee(this_annote);
                        else if (post_basic_annote_walkee != NULL)
                            post_basic_annote_walkee(this_annote, this);
                        break;
                      }
                    case WALK_ANNOTE_DONE:
                      {
                        stack_top = walk_stack->pop();
                        delete stack_top;
                        break;
                      }
                    default:
                        assert(FALSE);
                  }
                break;
              }
            case WALK_IMMED_LIST:
              {
                walk_frame_immed_list_data *this_immed_list_data =
                        &(stack_top->u.immed_list_data);
                immed_list_e *the_elem = this_immed_list_data->next;
                if (the_elem != NULL)
                  {
                    this_immed_list_data->previous = the_elem;
                    this_immed_list_data->next = the_elem->next();
                    immed this_immed = the_elem->contents;
                    walk_stack->push(new walk_frame_data(this_immed));
                  }
                else
                  {
                    stack_top = walk_stack->pop();
                    delete stack_top;
                  }
                break;
              }
            case WALK_AN_IMMED:
              {
                walk_frame_an_immed_data *this_immed_data =
                        &(stack_top->u.an_immed_data);
                immed this_immed = this_immed_data->the_immed;
                switch (this_immed_data->tag)
                  {
                    case WALK_IMMED_CHILDREN:
                      {
                        this_immed_data->tag = WALK_IMMED_DONE;
                        walk_frame_data *new_data = NULL;
                        switch (this_immed.kind())
                          {
                            case im_symbol:
                              {
                                sym_node *this_sym = this_immed.symbol();
                                if (leaf_simple_sym_walkee != NULL)
                                    leaf_simple_sym_walkee(this_sym);
                                else if (leaf_basic_sym_walkee != NULL)
                                    leaf_basic_sym_walkee(this_sym, this);
                                break;
                              }
                            case im_type:
                              {
                                type_node *this_type = this_immed.type();
                                if (leaf_simple_type_walkee != NULL)
                                    leaf_simple_type_walkee(this_type);
                                else if (leaf_basic_type_walkee != NULL)
                                    leaf_basic_type_walkee(this_type, this);
                                break;
                              }
                            case im_op:
                              {
                                operand this_op = this_immed.op();
                                new_data = new walk_frame_data(this_op);
                                break;
                              }
                            case im_instr:
                              {
                                instruction *this_instr = this_immed.instr();
                                new_data = new walk_frame_data(this_instr);
                                break;
                              }
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
                        if (new_data != NULL)
                            walk_stack->push(new_data);
                        break;
                      }
                    case WALK_IMMED_DONE:
                      {
                        stack_top = walk_stack->pop();
                        delete stack_top;
                        break;
                      }
                    default:
                        assert(FALSE);
                  }
                break;
              }
            case WALK_OPERAND:
              {
                walk_frame_operand_data *this_operand_data =
                        &(stack_top->u.operand_data);
                operand this_op = this_operand_data->the_op;
                switch (this_operand_data->tag)
                  {
                    case WALK_OP_PRE:
                      {
                        this_operand_data->tag = WALK_OP_CHILDREN;
                        if (pre_simple_op_walkee != NULL)
                            pre_simple_op_walkee(this_op);
                        else if (pre_basic_op_walkee != NULL)
                            pre_basic_op_walkee(this_op, this);
                        break;
                      }
                    case WALK_OP_CHILDREN:
                      {
                        this_operand_data->tag = WALK_OP_POST;
                        walk_frame_data *new_data = NULL;
                        switch (this_op.kind())
                          {
                            case OPER_NULL:
                                break;
                            case OPER_SYM:
                              {
                                sym_node *this_sym = this_op.symbol();
                                if (leaf_simple_sym_walkee != NULL)
                                    leaf_simple_sym_walkee(this_sym);
                                else if (leaf_basic_sym_walkee != NULL)
                                    leaf_basic_sym_walkee(this_sym, this);
                                break;
                              }
                            case OPER_INSTR:
                              {
                                if (!this_operand_data->is_destination)
                                  {
                                    instruction *this_instr = this_op.instr();
                                    new_data = new walk_frame_data(this_instr);
                                  }
                                break;
                              }
                            case OPER_REG:
                              {
                                type_node *this_type = this_op.type();
                                if (leaf_simple_type_walkee != NULL)
                                    leaf_simple_type_walkee(this_type);
                                else if (leaf_basic_type_walkee != NULL)
                                    leaf_basic_type_walkee(this_type, this);
                                break;
                              }
                            default:
                                assert(FALSE);
                          }
                        if (new_data != NULL)
                            walk_stack->push(new_data);
                        break;
                      }
                    case WALK_OP_POST:
                      {
                        this_operand_data->tag = WALK_OP_DONE;
                        if (post_simple_op_walkee != NULL)
                            post_simple_op_walkee(this_op);
                        else if (post_basic_op_walkee != NULL)
                            post_basic_op_walkee(this_op, this);
                        break;
                      }
                    case WALK_OP_DONE:
                      {
                        stack_top = walk_stack->pop();
                        delete stack_top;
                        break;
                      }
                    default:
                        assert(FALSE);
                  }
                break;
              }
            case WALK_NODE_LIST:
              {
                walk_frame_node_list_data *this_node_list_data =
                        &(stack_top->u.node_list_data);
                tree_node_list_e *the_elem = this_node_list_data->next;
                if (the_elem != NULL)
                  {
                    this_node_list_data->previous = the_elem;
                    this_node_list_data->next = the_elem->next();
                    tree_node *this_node = the_elem->contents;
                    walk_stack->push(new walk_frame_data(this_node));
                  }
                else
                  {
                    stack_top = walk_stack->pop();
                    delete stack_top;
                  }
                break;
              }
            case WALK_FILE_SET_ENTRY:
              {
                walk_frame_file_set_entry_data *this_file_set_entry_data =
                        &(stack_top->u.file_set_entry_data);
                file_set_entry *this_fse = this_file_set_entry_data->the_fse;
                switch (this_file_set_entry_data->tag)
                  {
                    case WALK_FSE_SYMTAB:
                      {
                        this_file_set_entry_data->tag = WALK_FSE_DONE;
                        walk_stack->push(
                                new walk_frame_data(this_fse->symtab()));
                        break;
                      }
                    case WALK_FSE_DONE:
                      {
                        stack_top = walk_stack->pop();
                        delete stack_top;
                        break;
                      }
                    default:
                        assert(FALSE);
                  }
                break;
              }
            case WALK_TREE_NODE:
              {
                walk_frame_tree_node_data *this_tree_node_data =
                        &(stack_top->u.tree_node_data);
                tree_node *this_node = this_tree_node_data->the_node;
                unsigned this_list_num = this_tree_node_data->child_list_num;
                if (this_list_num == 0)
                  {
                    this_tree_node_data->child_list_num = 1;
                    switch (this_node->kind())
                      {
                        case TREE_INSTR:
                          {
                            tree_instr *this_tree_instr =
                                    (tree_instr *)this_node;
                            walk_stack->push(new walk_frame_data(
                                                     this_tree_instr,
                                                     WALK_TREE_INSTR));
                            break;
                          }
                        case TREE_LOOP:
                          {
                            tree_loop *this_loop = (tree_loop *)this_node;
                            walk_stack->push(new walk_frame_data(this_loop,
                                                     WALK_TREE_LOOP));
                            break;
                          }
                        case TREE_FOR:
                          {
                            tree_for *this_for = (tree_for *)this_node;
                            walk_stack->push(new walk_frame_data(this_for,
                                                     WALK_TREE_FOR));
                            break;
                          }
                        case TREE_IF:
                          {
                            tree_if *this_if = (tree_if *)this_node;
                            walk_stack->push(new walk_frame_data(this_if,
                                                     WALK_TREE_IF));
                            break;
                          }
                        case TREE_BLOCK:
                          {
                            tree_block *this_block = (tree_block *)this_node;
                            walk_stack->push(new walk_frame_data(this_block,
                                                     WALK_TREE_BLOCK));
                            break;
                          }
                        default:
                            assert(FALSE);
                      }
                  }
                else if (this_list_num <= this_node->num_child_lists())
                  {
                    tree_node_list *this_list =
                            this_node->child_list_num(this_list_num - 1);
                    this_tree_node_data->child_list_num = this_list_num + 1;
                    walk_stack->push(new walk_frame_data(this_list->head()));
                  }
                else
                  {
                    stack_top = walk_stack->pop();
                    delete stack_top;
                  }
                break;
              }
            case WALK_TREE_INSTR:
              {
                walk_frame_tree_instr_data *this_tree_instr_data =
                        &(stack_top->u.tree_instr_data);
                tree_instr *this_tree_instr =
                        this_tree_instr_data->the_tree_instr;
                switch (this_tree_instr_data->tag)
                  {
                    case WALK_TI_INSTR:
                      {
                        this_tree_instr_data->tag = WALK_TI_DONE;
                        walk_stack->push(new walk_frame_data(
                                                 this_tree_instr->instr()));
                        break;
                      }
                    case WALK_TI_DONE:
                      {
                        stack_top = walk_stack->pop();
                        delete stack_top;
                        break;
                      }
                    default:
                        assert(FALSE);
                  }
                break;
              }
            case WALK_TREE_LOOP:
              {
                walk_frame_tree_loop_data *this_tree_loop_data =
                        &(stack_top->u.tree_loop_data);
                tree_loop *this_loop = this_tree_loop_data->the_loop;
                switch (this_tree_loop_data->tag)
                  {
                    case WALK_LOOP_CONTINUE:
                      {
                        this_tree_loop_data->tag = WALK_LOOP_BREAK;
                        label_sym *this_label = this_loop->contlab();
                        if (leaf_simple_sym_walkee != NULL)
                            leaf_simple_sym_walkee(this_label);
                        else if (leaf_basic_sym_walkee != NULL)
                            leaf_basic_sym_walkee(this_label, this);
                        break;
                      }
                    case WALK_LOOP_BREAK:
                      {
                        this_tree_loop_data->tag = WALK_LOOP_TOP;
                        label_sym *this_label = this_loop->brklab();
                        if (leaf_simple_sym_walkee != NULL)
                            leaf_simple_sym_walkee(this_label);
                        else if (leaf_basic_sym_walkee != NULL)
                            leaf_basic_sym_walkee(this_label, this);
                        break;
                      }
                    case WALK_LOOP_TOP:
                      {
                        this_tree_loop_data->tag = WALK_LOOP_DONE;
                        label_sym *this_label = this_loop->toplab();
                        if (leaf_simple_sym_walkee != NULL)
                            leaf_simple_sym_walkee(this_label);
                        else if (leaf_basic_sym_walkee != NULL)
                            leaf_basic_sym_walkee(this_label, this);
                        break;
                      }
                    case WALK_LOOP_DONE:
                      {
                        stack_top = walk_stack->pop();
                        delete stack_top;
                        break;
                      }
                    default:
                        assert(FALSE);
                  }
                break;
              }
            case WALK_TREE_FOR:
              {
                walk_frame_tree_for_data *this_tree_for_data =
                        &(stack_top->u.tree_for_data);
                tree_for *this_for = this_tree_for_data->the_for;
                switch (this_tree_for_data->tag)
                  {
                    case WALK_FOR_INDEX:
                      {
                        this_tree_for_data->tag = WALK_FOR_CONTINUE;
                        var_sym *this_var = this_for->index();
                        if (leaf_simple_sym_walkee != NULL)
                            leaf_simple_sym_walkee(this_var);
                        else if (leaf_basic_sym_walkee != NULL)
                            leaf_basic_sym_walkee(this_var, this);
                        break;
                      }
                    case WALK_FOR_CONTINUE:
                      {
                        this_tree_for_data->tag = WALK_FOR_BREAK;
                        label_sym *this_label = this_for->contlab();
                        if (leaf_simple_sym_walkee != NULL)
                            leaf_simple_sym_walkee(this_label);
                        else if (leaf_basic_sym_walkee != NULL)
                            leaf_basic_sym_walkee(this_label, this);
                        break;
                      }
                    case WALK_FOR_BREAK:
                      {
                        this_tree_for_data->tag = WALK_FOR_DONE;
                        label_sym *this_label = this_for->brklab();
                        if (leaf_simple_sym_walkee != NULL)
                            leaf_simple_sym_walkee(this_label);
                        else if (leaf_basic_sym_walkee != NULL)
                            leaf_basic_sym_walkee(this_label, this);
                        break;
                      }
                    case WALK_FOR_DONE:
                      {
                        stack_top = walk_stack->pop();
                        delete stack_top;
                        break;
                      }
                    default:
                        assert(FALSE);
                  }
                break;
              }
            case WALK_TREE_IF:
              {
                walk_frame_tree_if_data *this_tree_if_data =
                        &(stack_top->u.tree_if_data);
                tree_if *this_if = this_tree_if_data->the_if;
                switch (this_tree_if_data->tag)
                  {
                    case WALK_IF_JUMPTO:
                      {
                        this_tree_if_data->tag = WALK_IF_DONE;
                        label_sym *this_label = this_if->jumpto();
                        if (leaf_simple_sym_walkee != NULL)
                            leaf_simple_sym_walkee(this_label);
                        else if (leaf_basic_sym_walkee != NULL)
                            leaf_basic_sym_walkee(this_label, this);
                        break;
                      }
                    case WALK_IF_DONE:
                      {
                        stack_top = walk_stack->pop();
                        delete stack_top;
                        break;
                      }
                    default:
                        assert(FALSE);
                  }
                break;
              }
            case WALK_TREE_BLOCK:
              {
                walk_frame_tree_block_data *this_tree_block_data =
                        &(stack_top->u.tree_block_data);
                tree_block *this_block = this_tree_block_data->the_block;
                switch (this_tree_block_data->tag)
                  {
                    case WALK_TB_SYMTAB:
                      {
                        this_tree_block_data->tag = WALK_TB_PROC_SYM;
                        walk_stack->push(
                                new walk_frame_data(this_block->symtab()));
                        break;
                      }
                    case WALK_TB_PROC_SYM:
                      {
                        this_tree_block_data->tag = WALK_TB_DONE;
                        if (this_block->is_proc())
                          {
                            tree_proc *this_tree_proc =
                                    (tree_proc *)this_block;
                            proc_sym *this_proc_sym = this_tree_proc->proc();
                            if (leaf_simple_sym_walkee != NULL)
                                leaf_simple_sym_walkee(this_proc_sym);
                            else if (leaf_basic_sym_walkee != NULL)
                                leaf_basic_sym_walkee(this_proc_sym, this);
                          }
                        break;
                      }
                    case WALK_TB_DONE:
                      {
                        stack_top = walk_stack->pop();
                        delete stack_top;
                        break;
                      }
                    default:
                        assert(FALSE);
                  }
                break;
              }
            case WALK_INSTRUCTION:
              {
                walk_frame_instruction_data *this_instruction_data =
                        &(stack_top->u.instruction_data);
                instruction *this_instr = this_instruction_data->the_instr;
                switch (this_instruction_data->tag)
                  {
                    case WALK_INST_SRCS:
                      {
                        unsigned src_num = this_instruction_data->src_num;
                        if (src_num < this_instr->num_srcs())
                          {
                            this_instruction_data->src_num = src_num + 1;
                            operand this_op = this_instr->src_op(src_num);
                            walk_stack->push(new walk_frame_data(this_op));
                          }
                        else
                          {
                            this_instruction_data->tag = WALK_INST_DST;
                          }
                        break;
                      }
                    case WALK_INST_DST:
                      {
                        this_instruction_data->tag = WALK_INST_RESULT_TYPE;
                        walk_stack->push(
                                new walk_frame_data(this_instr->dst_op(),
                                                    TRUE));
                        break;
                      }
                    case WALK_INST_RESULT_TYPE:
                      {
                        this_instruction_data->tag = WALK_INST_CHILDREN;
                        type_node *result_type = this_instr->result_type();
                        if (leaf_simple_type_walkee != NULL)
                            leaf_simple_type_walkee(result_type);
                        else if (leaf_basic_type_walkee != NULL)
                            leaf_basic_type_walkee(result_type, this);
                        break;
                      }
                    case WALK_INST_CHILDREN:
                      {
                        this_instruction_data->tag = WALK_INST_DONE;
                        walk_frame_data *new_data = NULL;
                        switch (this_instr->opcode())
                          {
                            case io_ldc:
                              {
                                in_ldc *this_ldc = (in_ldc *)this_instr;
                                new_data = new walk_frame_data(this_ldc,
                                                               WALK_LDC_INSTR);
                                break;
                              }
                            case io_btrue:
                            case io_bfalse:
                            case io_jmp:
                              {
                                in_bj *this_bj = (in_bj *)this_instr;
                                new_data = new walk_frame_data(this_bj,
                                                               WALK_BJ_INSTR);
                                break;
                              }
                            case io_mbr:
                              {
                                in_mbr *this_mbr = (in_mbr *)this_instr;
                                new_data = new walk_frame_data(this_mbr,
                                                               WALK_MBR_INSTR);
                                break;
                              }
                            case io_lab:
                              {
                                in_lab *this_lab = (in_lab *)this_instr;
                                new_data = new walk_frame_data(this_lab,
                                                               WALK_LAB_INSTR);
                                break;
                              }
                            default:
                                break;
                          }
                        if (new_data != NULL)
                            walk_stack->push(new_data);
                        break;
                      }
                    case WALK_INST_DONE:
                      {
                        stack_top = walk_stack->pop();
                        delete stack_top;
                        break;
                      }
                    default:
                        assert(FALSE);
                  }
                break;
              }
            case WALK_LDC_INSTR:
              {
                walk_frame_ldc_instr_data *this_ldc_instr_data =
                        &(stack_top->u.ldc_instr_data);
                in_ldc *this_ldc = this_ldc_instr_data->the_ldc;
                switch (this_ldc_instr_data->tag)
                  {
                    case WALK_LDC_IMMED:
                      {
                        this_ldc_instr_data->tag = WALK_LDC_DONE;
                        walk_stack->push(
                                new walk_frame_data(this_ldc->value()));
                        break;
                      }
                    case WALK_LDC_DONE:
                      {
                        stack_top = walk_stack->pop();
                        delete stack_top;
                        break;
                      }
                    default:
                        assert(FALSE);
                  }
                break;
              }
            case WALK_BJ_INSTR:
              {
                walk_frame_bj_instr_data *this_bj_instr_data =
                        &(stack_top->u.bj_instr_data);
                in_bj *this_bj = this_bj_instr_data->the_bj;
                switch (this_bj_instr_data->tag)
                  {
                    case WALK_BJ_TARGET:
                      {
                        this_bj_instr_data->tag = WALK_BJ_DONE;
                        if (leaf_simple_sym_walkee != NULL)
                            leaf_simple_sym_walkee(this_bj->target());
                        else if (leaf_basic_sym_walkee != NULL)
                            leaf_basic_sym_walkee(this_bj->target(), this);
                        break;
                      }
                    case WALK_BJ_DONE:
                      {
                        stack_top = walk_stack->pop();
                        delete stack_top;
                        break;
                      }
                    default:
                        assert(FALSE);
                  }
                break;
              }
            case WALK_MBR_INSTR:
              {
                walk_frame_mbr_instr_data *this_mbr_instr_data =
                        &(stack_top->u.mbr_instr_data);
                in_mbr *this_mbr = this_mbr_instr_data->the_mbr;
                switch (this_mbr_instr_data->tag)
                  {
                    case WALK_MBR_LABELS:
                      {
                        unsigned lab_num = this_mbr_instr_data->lab_num;
                        if (lab_num < this_mbr->num_labs())
                          {
                            this_mbr_instr_data->lab_num = lab_num + 1;
                            label_sym *this_lab_sym = this_mbr->label(lab_num);
                            if (leaf_simple_sym_walkee != NULL)
                                leaf_simple_sym_walkee(this_lab_sym);
                            else if (leaf_basic_sym_walkee != NULL)
                                leaf_basic_sym_walkee(this_lab_sym, this);
                          }
                        else
                          {
                            this_mbr_instr_data->tag = WALK_MBR_DEFAULT;
                          }
                        break;
                      }
                    case WALK_MBR_DEFAULT:
                      {
                        this_mbr_instr_data->tag = WALK_MBR_DONE;
                        label_sym *this_lab_sym = this_mbr->default_lab();
                        if (leaf_simple_sym_walkee != NULL)
                            leaf_simple_sym_walkee(this_lab_sym);
                        else if (leaf_basic_sym_walkee != NULL)
                            leaf_basic_sym_walkee(this_lab_sym, this);
                        break;
                      }
                    case WALK_MBR_DONE:
                      {
                        stack_top = walk_stack->pop();
                        delete stack_top;
                        break;
                      }
                    default:
                        assert(FALSE);
                  }
                break;
              }
            case WALK_LAB_INSTR:
              {
                walk_frame_lab_instr_data *this_lab_instr_data =
                        &(stack_top->u.lab_instr_data);
                in_lab *this_lab = this_lab_instr_data->the_lab;
                switch (this_lab_instr_data->tag)
                  {
                    case WALK_LAB_SYMBOL:
                      {
                        this_lab_instr_data->tag = WALK_LAB_DONE;
                        if (leaf_simple_sym_walkee != NULL)
                            leaf_simple_sym_walkee(this_lab->label());
                        else if (leaf_basic_sym_walkee != NULL)
                            leaf_basic_sym_walkee(this_lab->label(), this);
                        break;
                      }
                    case WALK_LAB_DONE:
                      {
                        stack_top = walk_stack->pop();
                        delete stack_top;
                        break;
                      }
                    default:
                        assert(FALSE);
                  }
                break;
              }
            case WALK_A_SYMTAB:
              {
                walk_frame_a_symtab_data *this_a_symtab_data =
                        &(stack_top->u.a_symtab_data);
                base_symtab *this_symtab = this_a_symtab_data->the_symtab;
                switch (this_a_symtab_data->tag)
                  {
                    case WALK_SYMTAB_SYMBOLS:
                      {
                        sym_node_list_e *the_elem =
                                this_a_symtab_data->next.sym_e;
                        if (the_elem != NULL)
                          {
                            this_a_symtab_data->previous.sym_e = the_elem;
                            this_a_symtab_data->next.sym_e = the_elem->next();
                            sym_node *this_sym = the_elem->contents;
                            walk_stack->push(new walk_frame_data(this_sym));
                          }
                        else
                          {
                            this_a_symtab_data->tag = WALK_SYMTAB_TYPES;
                            this_a_symtab_data->next.type_e =
                                    this_symtab->types()->head();
                          }
                        break;
                      }
                    case WALK_SYMTAB_TYPES:
                      {
                        type_node_list_e *the_elem =
                                this_a_symtab_data->next.type_e;
                        if (the_elem != NULL)
                          {
                            this_a_symtab_data->previous.type_e = the_elem;
                            this_a_symtab_data->next.type_e = the_elem->next();
                            type_node *this_type = the_elem->contents;
                            walk_stack->push(new walk_frame_data(this_type));
                          }
                        else
                          {
                            this_a_symtab_data->tag = WALK_SYMTAB_DEFS;
                            this_a_symtab_data->next.def_e =
                                    this_symtab->var_defs()->head();
                          }
                        break;
                      }
                    case WALK_SYMTAB_DEFS:
                      {
                        var_def_list_e *the_elem =
                                this_a_symtab_data->next.def_e;
                        if (the_elem != NULL)
                          {
                            this_a_symtab_data->previous.def_e = the_elem;
                            this_a_symtab_data->next.def_e = the_elem->next();
                            var_def *this_def = the_elem->contents;
                            walk_stack->push(new walk_frame_data(this_def));
                          }
                        else
                          {
                            this_a_symtab_data->tag = WALK_SYMTAB_DONE;
                          }
                        break;
                      }
                    case WALK_SYMTAB_DONE:
                      {
                        stack_top = walk_stack->pop();
                        delete stack_top;
                        break;
                      }
                    default:
                        assert(FALSE);
                  }
                break;
              }
            case WALK_A_SYMBOL:
              {
                walk_frame_a_symbol_data *this_a_symbol_data =
                        &(stack_top->u.a_symbol_data);
                sym_node *this_sym = this_a_symbol_data->the_sym;
                switch (this_a_symbol_data->tag)
                  {
                    case WALK_SYMBOL_CHILDREN:
                      {
                        this_a_symbol_data->tag = WALK_SYMBOL_DONE;
                        switch (this_sym->kind())
                          {
                            case SYM_PROC:
                              {
                                proc_sym *this_proc = (proc_sym *)this_sym;
                                walk_stack->push(
                                        new walk_frame_data(this_proc,
                                                            WALK_PROC_SYM));
                                break;
                              }
                            case SYM_LABEL:
                                break;
                            case SYM_VAR:
                              {
                                var_sym *this_var = (var_sym *)this_sym;
                                walk_stack->push(
                                        new walk_frame_data(this_var,
                                                            WALK_A_VARIABLE));
                                break;
                              }
                            default:
                                assert(FALSE);
                          }
                        break;
                      }
                    case WALK_SYMBOL_DONE:
                      {
                        stack_top = walk_stack->pop();
                        delete stack_top;
                        break;
                      }
                    default:
                        assert(FALSE);
                  }
                break;
              }
            case WALK_A_VARIABLE:
              {
                walk_frame_a_variable_data *this_a_variable_data =
                        &(stack_top->u.a_variable_data);
                var_sym *this_var = this_a_variable_data->the_var;
                switch (this_a_variable_data->tag)
                  {
                    case WALK_VAR_TYPE:
                      {
                        this_a_variable_data->tag = WALK_VAR_PARENT;
                        if (leaf_simple_type_walkee != NULL)
                            leaf_simple_type_walkee(this_var->type());
                        else if (leaf_basic_type_walkee != NULL)
                            leaf_basic_type_walkee(this_var->type(), this);
                        break;
                      }
                    case WALK_VAR_PARENT:
                      {
                        this_a_variable_data->tag = WALK_VAR_CHILDREN;
                        var_sym *parent_var = this_var->parent_var();
                        if (parent_var != NULL)
                          {
                            if (leaf_simple_sym_walkee != NULL)
                                leaf_simple_sym_walkee(parent_var);
                            else if (leaf_basic_sym_walkee != NULL)
                                leaf_basic_sym_walkee(parent_var, this);
                          }
                        break;
                      }
                    case WALK_VAR_CHILDREN:
                      {
                        unsigned child_num = this_a_variable_data->child_num;
                        if (child_num < this_var->num_children())
                          {
                            this_a_variable_data->child_num = child_num + 1;
                            var_sym *this_child =
                                    this_var->child_var(child_num);
                            if (leaf_simple_sym_walkee != NULL)
                                leaf_simple_sym_walkee(this_child);
                            else if (leaf_basic_sym_walkee != NULL)
                                leaf_basic_sym_walkee(this_child, this);
                          }
                        else
                          {
                            this_a_variable_data->tag = WALK_VAR_DONE;
                          }
                        break;
                      }
                    case WALK_VAR_DONE:
                      {
                        stack_top = walk_stack->pop();
                        delete stack_top;
                        break;
                      }
                    default:
                        assert(FALSE);
                  }
                break;
              }
            case WALK_PROC_SYM:
              {
                walk_frame_proc_sym_data *this_proc_sym_data =
                        &(stack_top->u.proc_sym_data);
                proc_sym *this_proc = this_proc_sym_data->the_proc;
                switch (this_proc_sym_data->tag)
                  {
                    case WALK_PROC_TYPE:
                      {
                        this_proc_sym_data->tag = WALK_PROC_DONE;
                        if (leaf_simple_type_walkee != NULL)
                            leaf_simple_type_walkee(this_proc->type());
                        else if (leaf_basic_type_walkee != NULL)
                            leaf_basic_type_walkee(this_proc->type(), this);
                        break;
                      }
                    case WALK_PROC_DONE:
                      {
                        stack_top = walk_stack->pop();
                        delete stack_top;
                        break;
                      }
                    default:
                        assert(FALSE);
                  }
                break;
              }
            case WALK_VAR_DEF:
              {
                walk_frame_var_def_data *this_var_def_data =
                        &(stack_top->u.var_def_data);
                var_def *this_def = this_var_def_data->the_def;
                switch (this_var_def_data->tag)
                  {
                    case WALK_DEF_VAR:
                      {
                        this_var_def_data->tag = WALK_DEF_DONE;
                        if (leaf_simple_sym_walkee != NULL)
                            leaf_simple_sym_walkee(this_def->variable());
                        else if (leaf_basic_sym_walkee != NULL)
                            leaf_basic_sym_walkee(this_def->variable(), this);
                        break;
                      }
                    case WALK_DEF_DONE:
                      {
                        stack_top = walk_stack->pop();
                        delete stack_top;
                        break;
                      }
                    default:
                        assert(FALSE);
                  }
                break;
              }
            case WALK_A_TYPE:
              {
                walk_frame_a_type_data *this_a_type_data =
                        &(stack_top->u.a_type_data);
                type_node *this_type = this_a_type_data->the_type;
                switch (this_a_type_data->tag)
                  {
                    case WALK_TYPE_REF_TYPES:
                      {
                        unsigned ref_type_num = this_a_type_data->ref_type_num;
                        if (ref_type_num < this_type->num_ref_types())
                          {
                            this_a_type_data->ref_type_num = ref_type_num + 1;
                            type_node *this_ref =
                                    this_type->ref_type(ref_type_num);
                            if (leaf_simple_type_walkee != NULL)
                                leaf_simple_type_walkee(this_ref);
                            else if (leaf_basic_type_walkee != NULL)
                                leaf_basic_type_walkee(this_ref, this);
                          }
                        else
                          {
                            this_a_type_data->tag = WALK_TYPE_CHILDREN;
                          }
                        break;
                      }
                    case WALK_TYPE_CHILDREN:
                      {
                        this_a_type_data->tag = WALK_TYPE_DONE;
                     if (this_type->op() == TYPE_ARRAY)
                          {
                            array_type *this_array = (array_type *)this_type;
                            walk_stack->push(
                                    new walk_frame_data(this_array,
                                                        WALK_ARRAY_TYPE));
                          }
                        break;
                      }
                    case WALK_TYPE_DONE:
                      {
                        stack_top = walk_stack->pop();
                        delete stack_top;
                        break;
                      }
                    default:
                        assert(FALSE);
                  }
                break;
              }
            case WALK_ARRAY_TYPE:
              {
                walk_frame_array_type_data *this_array_type_data =
                        &(stack_top->u.array_type_data);
                array_type *this_array_type =
                        this_array_type_data->the_array_type;
                switch (this_array_type_data->tag)
                  {
                    case WALK_ARRAY_LOWER:
                      {
                        this_array_type_data->tag = WALK_ARRAY_UPPER;
                        array_bound lower_bound =
                                this_array_type->lower_bound();
                        if (lower_bound.is_variable())
                          {
                            var_sym *this_var = lower_bound.variable();
                            if (leaf_simple_sym_walkee != NULL)
                                leaf_simple_sym_walkee(this_var);
                            else if (leaf_basic_sym_walkee != NULL)
                                leaf_basic_sym_walkee(this_var, this);
                          }
                        break;
                      }
                    case WALK_ARRAY_UPPER:
                      {
                        this_array_type_data->tag = WALK_ARRAY_DONE;
                        array_bound upper_bound =
                                this_array_type->upper_bound();
                        if (upper_bound.is_variable())
                          {
                            var_sym *this_var = upper_bound.variable();
                            if (leaf_simple_sym_walkee != NULL)
                                leaf_simple_sym_walkee(this_var);
                            else if (leaf_basic_sym_walkee != NULL)
                                leaf_basic_sym_walkee(this_var, this);
                          }
                        break;
                      }
                    case WALK_ARRAY_DONE:
                      {
                        stack_top = walk_stack->pop();
                        delete stack_top;
                        break;
                      }
                    default:
                        assert(FALSE);
                  }
                break;
              }
            default:
                assert(FALSE);
          }
      }

    delete walk_stack;
    walk_stack = NULL;
  }

void so_walker::walk(suif_object *the_object, simple_so_walkee *walkee)
  {
    set_pre_function(walkee);
    walk(the_object);
  }

void so_walker::walk(suif_object *the_object, basic_so_walkee *walkee)
  {
    set_pre_function(walkee);
    walk(the_object);
  }

void so_walker::walk(suif_object *the_object, simple_annote_walkee *walkee)
  {
    set_pre_function(walkee);
    walk(the_object);
  }

void so_walker::walk(suif_object *the_object, basic_annote_walkee *walkee)
  {
    set_pre_function(walkee);
    walk(the_object);
  }

void so_walker::walk(suif_object *the_object, simple_sym_walkee *walkee)
  {
    set_leaf_function(walkee);
    walk(the_object);
  }

void so_walker::walk(suif_object *the_object, basic_sym_walkee *walkee)
  {
    set_leaf_function(walkee);
    walk(the_object);
  }

void so_walker::walk(suif_object *the_object, simple_type_walkee *walkee)
  {
    set_leaf_function(walkee);
    walk(the_object);
  }

void so_walker::walk(suif_object *the_object, basic_type_walkee *walkee)
  {
    set_leaf_function(walkee);
    walk(the_object);
  }

void so_walker::walk(suif_object *the_object, simple_op_walkee *walkee)
  {
    set_pre_function(walkee);
    walk(the_object);
  }

void so_walker::walk(suif_object *the_object, basic_op_walkee *walkee)
  {
    set_pre_function(walkee);
    walk(the_object);
  }


any_type so_walker::walk(suif_object *the_object, any_type default_result)
  {
    set_result(default_result);
    walk(the_object);
    return get_result();
  }

any_type so_walker::walk(suif_object *the_object, simple_so_walkee *walkee,
                         any_type default_result)
  {
    set_result(default_result);
    walk(the_object, walkee);
    return get_result();
  }

any_type so_walker::walk(suif_object *the_object, basic_so_walkee *walkee,
                         any_type default_result)
  {
    set_result(default_result);
    walk(the_object, walkee);
    return get_result();
  }

any_type so_walker::walk(suif_object *the_object, simple_annote_walkee *walkee,
                         any_type default_result)
  {
    set_result(default_result);
    walk(the_object, walkee);
    return get_result();
  }

any_type so_walker::walk(suif_object *the_object, basic_annote_walkee *walkee,
                         any_type default_result)
  {
    set_result(default_result);
    walk(the_object, walkee);
    return get_result();
  }

any_type so_walker::walk(suif_object *the_object, simple_sym_walkee *walkee,
                         any_type default_result)
  {
    set_result(default_result);
    walk(the_object, walkee);
    return get_result();
  }

any_type so_walker::walk(suif_object *the_object, basic_sym_walkee *walkee,
                         any_type default_result)
  {
    set_result(default_result);
    walk(the_object, walkee);
    return get_result();
  }

any_type so_walker::walk(suif_object *the_object, simple_type_walkee *walkee,
                         any_type default_result)
  {
    set_result(default_result);
    walk(the_object, walkee);
    return get_result();
  }

any_type so_walker::walk(suif_object *the_object, basic_type_walkee *walkee,
                         any_type default_result)
  {
    set_result(default_result);
    walk(the_object, walkee);
    return get_result();
  }

any_type so_walker::walk(suif_object *the_object, simple_op_walkee *walkee,
                         any_type default_result)
  {
    set_result(default_result);
    walk(the_object, walkee);
    return get_result();
  }

any_type so_walker::walk(suif_object *the_object, basic_op_walkee *walkee,
                         any_type default_result)
  {
    set_result(default_result);
    walk(the_object, walkee);
    return get_result();
  }


void so_walker::set_leaf_function(simple_sym_walkee *new_walkee)
  {
    assert(new_walkee != NULL);
    assert((leaf_simple_sym_walkee == NULL) &&
           (leaf_basic_sym_walkee == NULL));
    leaf_simple_sym_walkee = new_walkee;
  }

void so_walker::set_leaf_function(basic_sym_walkee *new_walkee)
  {
    assert(new_walkee != NULL);
    assert((leaf_simple_sym_walkee == NULL) &&
           (leaf_basic_sym_walkee == NULL));
    leaf_basic_sym_walkee = new_walkee;
  }

void so_walker::set_leaf_function(simple_type_walkee *new_walkee)
  {
    assert(new_walkee != NULL);
    assert((leaf_simple_type_walkee == NULL) &&
           (leaf_basic_type_walkee == NULL));
    leaf_simple_type_walkee = new_walkee;
  }

void so_walker::set_leaf_function(basic_type_walkee *new_walkee)
  {
    assert(new_walkee != NULL);
    assert((leaf_simple_type_walkee == NULL) &&
           (leaf_basic_type_walkee == NULL));
    leaf_basic_type_walkee = new_walkee;
  }


void so_walker::set_pre_function(simple_so_walkee *new_walkee)
  {
    assert(new_walkee != NULL);
    assert((pre_simple_so_walkee == NULL) && (pre_basic_so_walkee == NULL));
    pre_simple_so_walkee = new_walkee;
  }

void so_walker::set_pre_function(basic_so_walkee *new_walkee)
  {
    assert(new_walkee != NULL);
    assert((pre_simple_so_walkee == NULL) && (pre_basic_so_walkee == NULL));
    pre_basic_so_walkee = new_walkee;
  }

void so_walker::set_pre_function(simple_annote_walkee *new_walkee)
  {
    assert(new_walkee != NULL);
    assert((pre_simple_annote_walkee == NULL) &&
           (pre_basic_annote_walkee == NULL));
    pre_simple_annote_walkee = new_walkee;
  }

void so_walker::set_pre_function(basic_annote_walkee *new_walkee)
  {
    assert(new_walkee != NULL);
    assert((pre_simple_annote_walkee == NULL) &&
           (pre_basic_annote_walkee == NULL));
    pre_basic_annote_walkee = new_walkee;
  }

void so_walker::set_pre_function(simple_op_walkee *new_walkee)
  {
    assert(new_walkee != NULL);
    assert((pre_simple_op_walkee == NULL) && (pre_basic_op_walkee == NULL));
    pre_simple_op_walkee = new_walkee;
  }

void so_walker::set_pre_function(basic_op_walkee *new_walkee)
  {
    assert(new_walkee != NULL);
    assert((pre_simple_op_walkee == NULL) && (pre_basic_op_walkee == NULL));
    pre_basic_op_walkee = new_walkee;
  }


void so_walker::set_post_function(simple_so_walkee *new_walkee)
  {
    assert(new_walkee != NULL);
    assert((post_simple_so_walkee == NULL) && (post_basic_so_walkee == NULL));
    post_simple_so_walkee = new_walkee;
  }

void so_walker::set_post_function(basic_so_walkee *new_walkee)
  {
    assert(new_walkee != NULL);
    assert((post_simple_so_walkee == NULL) && (post_basic_so_walkee == NULL));
    post_basic_so_walkee = new_walkee;
  }

void so_walker::set_post_function(simple_annote_walkee *new_walkee)
  {
    assert(new_walkee != NULL);
    assert((post_simple_annote_walkee == NULL) &&
           (post_basic_annote_walkee == NULL));
    post_simple_annote_walkee = new_walkee;
  }

void so_walker::set_post_function(basic_annote_walkee *new_walkee)
  {
    assert(new_walkee != NULL);
    assert((post_simple_annote_walkee == NULL) &&
           (post_basic_annote_walkee == NULL));
    post_basic_annote_walkee = new_walkee;
  }

void so_walker::set_post_function(simple_op_walkee *new_walkee)
  {
    assert(new_walkee != NULL);
    assert((post_simple_op_walkee == NULL) && (post_basic_op_walkee == NULL));
    post_simple_op_walkee = new_walkee;
  }

void so_walker::set_post_function(basic_op_walkee *new_walkee)
  {
    assert(new_walkee != NULL);
    assert((post_simple_op_walkee == NULL) && (post_basic_op_walkee == NULL));
    post_basic_op_walkee = new_walkee;
  }


void so_walker::set_break(void)
  {
    assert(walk_stack != NULL);
    while (!walk_stack->is_empty())
      {
        walk_frame_data *this_data = walk_stack->pop();
        delete this_data;
      }
  }

void so_walker::set_skip(void)
  {
    assert(walk_stack != NULL);
    while (!walk_stack->is_empty())
      {
        walk_frame_data *this_data = walk_stack->pop();
        boolean done = ((this_data->tag == WALK_SUIF_OBJ) ||
                        (this_data->tag == WALK_AN_ANNOTE));
        delete this_data;
        if (done)
            break;
      }
  }


void so_walker::replace_object(suif_object *replacement)
  {
    assert(!walk_stack->is_empty());
    walk_frame_data *stack_top = walk_stack->pop();
    assert(stack_top->tag == WALK_SUIF_OBJ);
    delete stack_top;

    if (walk_stack->is_empty())
      {
        error_line(1, replacement,
                   "attempt to replace SUIF object that was root of walk");
      }
    stack_top = walk_stack->head()->contents;
    switch (stack_top->tag)
      {
        case WALK_AN_IMMED:
          {
            walk_frame_an_immed_data *this_immed_data =
                    &(stack_top->u.an_immed_data);
            immed old_immed = this_immed_data->the_immed;
            assert(old_immed.is_instr());
            if (replacement == NULL)
              {
                immed_replacement_internal(immed(), walk_stack->head());
              }
            else if (!replacement->is_instr_obj())
              {
                error_line(1, replacement,
                           "attempt to replace instruction in immed with "
                           "non-instruction SUIF object");
              }
            else
              {
             instruction *new_instr = (instruction *)replacement;
                immed_replacement_internal(immed(new_instr),
                                           walk_stack->head());
              }
            break;
          }
        case WALK_OPERAND:
          {
            walk_frame_operand_data *this_operand_data =
                    &(stack_top->u.operand_data);
            operand old_op = this_operand_data->the_op;
            assert(old_op.is_expr());
            if (replacement == NULL)
              {
                op_replacement_internal(operand(), walk_stack->head());
              }
            else if (!replacement->is_instr_obj())
              {
                error_line(1, replacement,
                           "attempt to replace instruction in operand with "
                           "non-instruction SUIF object");
              }
            else
              {
                instruction *new_instr = (instruction *)replacement;
                op_replacement_internal(operand(new_instr),
                                        walk_stack->head());
              }
            break;
          }
        case WALK_NODE_LIST:
          {
            walk_frame_node_list_data *this_node_list_data =
                    &(stack_top->u.node_list_data);
            tree_node_list_e *the_elem = this_node_list_data->previous;
            assert(the_elem != NULL);
            tree_node *old_node = the_elem->contents;
            assert(old_node != NULL);
            tree_node_list *parent_list = old_node->parent();
            if (replacement == NULL)
              {
                /* empty */
              }
            else if (!replacement->is_tree_obj())
              {
                error_line(1, replacement,
                           "attempt to replace tree_node in tree_node_list"
                           " with non-tree_node SUIF object");
              }
            else
              {
                tree_node *new_node = (tree_node *)replacement;
                parent_list->insert_before(new_node, the_elem);
              }
            parent_list->remove(old_node->list_e());
            delete old_node->list_e();
            break;
          }
        case WALK_FILE_SET_ENTRY:
          {
            walk_frame_file_set_entry_data *this_file_set_entry_data =
                    &(stack_top->u.file_set_entry_data);
            file_set_entry *this_fse = this_file_set_entry_data->the_fse;
            if (replacement == NULL)
              {
                error_line(1, this_fse,
                           "attempt to replace file symtab with nothing");
              }
            else if (!replacement->is_symtab_obj())
              {
                error_line(1, replacement,
                           "attempt to replace file symtab with non-symtab"
                           " SUIF object");
              }
            else
              {
                base_symtab *new_base_symtab = (base_symtab *)replacement;
                if (!new_base_symtab->is_file())
                  {
                    error_line(1, replacement,
                               "attempt to replace file symtab with non-file"
                               " symtab");
		  }
                else
                  {
                    file_symtab *new_file_symtab =
                            (file_symtab *)new_base_symtab;
                    file_symtab *old_symtab = this_fse->symtab();
                    old_symtab->parent()->add_child(new_file_symtab);
                    while (!old_symtab->children()->is_empty())
                      {
                        base_symtab *this_child =
                                old_symtab->children()->head()->contents;
                        old_symtab->remove_child(this_child);
                        new_file_symtab->add_child(this_child);
                      }
                    this_fse->set_symtab(new_file_symtab);
                    old_symtab->parent()->remove_child(old_symtab);
		  }
              }
            break;
          }
        case WALK_TREE_INSTR:
          {
            walk_frame_tree_instr_data *this_tree_instr_data =
                    &(stack_top->u.tree_instr_data);
            tree_instr *this_tree_instr = this_tree_instr_data->the_tree_instr;
            if (replacement == NULL)
              {
                stack_top = walk_stack->pop();
                delete stack_top;
                this_tree_instr->remove_instr(this_tree_instr->instr());
                kill_node(this_tree_instr);
              }
            else if (!replacement->is_instr_obj())
              {
                error_line(1, replacement,
                           "attempt to replace instruction in tree_instr with"
                           " non-instruction SUIF object");
              }
            else
              {
                instruction *new_instr = (instruction *)replacement;
                this_tree_instr->remove_instr(this_tree_instr->instr());
                this_tree_instr->set_instr(new_instr);
              }
            break;
          }
        case WALK_TREE_BLOCK:
          {
            walk_frame_tree_block_data *this_tree_block_data =
                    &(stack_top->u.tree_block_data);
            tree_block *this_block = this_tree_block_data->the_block;
            if (replacement == NULL)
              {
                error_line(1, this_block,
                           "attempt to replace block symtab with nothing");
              }
            else if (!replacement->is_symtab_obj())
              {
                error_line(1, replacement,
                           "attempt to replace block symtab with non-symtab"
                           " SUIF object");
              }
            else
              {
                base_symtab *new_base_symtab = (base_symtab *)replacement;
                if (!new_base_symtab->is_block())
                  {
                    error_line(1, replacement,
                               "attempt to replace block symtab with "
                               "non-block symtab");
		  }
                else
                  {
                    block_symtab *new_block_symtab =
                            (block_symtab *)new_base_symtab;
                    block_symtab *old_symtab = this_block->symtab();
                    old_symtab->parent()->add_child(new_block_symtab);
                    while (!old_symtab->children()->is_empty())
                      {
                        base_symtab *this_child =
                                old_symtab->children()->head()->contents;
                        old_symtab->remove_child(this_child);
                        new_block_symtab->add_child(this_child);
                      }
                    this_block->set_symtab(new_block_symtab);
                    old_symtab->parent()->remove_child(old_symtab);
		  }
              }
            break;
          }
        case WALK_A_SYMTAB:
          {
            walk_frame_a_symtab_data *this_a_symtab_data =
                    &(stack_top->u.a_symtab_data);
            base_symtab *this_symtab = this_a_symtab_data->the_symtab;
            switch (this_a_symtab_data->tag)
              {
                case WALK_SYMTAB_SYMBOLS:
                  {
                    sym_node_list_e *the_elem =
                            this_a_symtab_data->previous.sym_e;
                    assert(the_elem != NULL);
                    sym_node *old_sym = the_elem->contents;
                    if (replacement == NULL)
                      {
                        /* empty */
                      }
                    else if (!replacement->is_sym_obj())
                      {
                        error_line(1, replacement,
                                   "attempt to replace sym_node in symbol"
                                   " table with non-sym_node SUIF object");
                      }
                    else
                      {
                        sym_node *new_sym = (sym_node *)replacement;
                        this_symtab->add_sym(new_sym);
                      }
                    this_symtab->remove_sym(old_sym);
                    break;
               }
                case WALK_SYMTAB_TYPES:
                  {
                    type_node_list_e *the_elem =
                            this_a_symtab_data->previous.type_e;
                    assert(the_elem != NULL);
                    type_node *old_type = the_elem->contents;
                    if (replacement == NULL)
                      {
                        /* empty */
                      }
                    else if (!replacement->is_type_obj())
                      {
                        error_line(1, replacement,
                                   "attempt to replace type_node in symbol"
                                   " table with non-type_node SUIF object");
                      }
                    else
                      {
                        type_node *new_type = (type_node *)replacement;
                        this_symtab->add_type(new_type);
                      }
                    this_symtab->remove_type(old_type);
                    break;
                  }
                case WALK_SYMTAB_DEFS:
                  {
                    var_def_list_e *the_elem =
                            this_a_symtab_data->previous.def_e;
                    assert(the_elem != NULL);
                    var_def *old_def = the_elem->contents;
                    if (replacement == NULL)
                      {
                        /* empty */
                      }
                    else if (!replacement->is_def_obj())
                      {
                        error_line(1, replacement,
                                   "attempt to replace var_def in symbol"
                                   " table with non-var_def SUIF object");
                      }
                    else
                      {
                        var_def *new_def = (var_def *)replacement;
                        this_symtab->add_def(new_def);
                      }
                    this_symtab->remove_def(old_def);
                    break;
                  }
                default:
                    assert(FALSE);
              }
            break;
          }
        case WALK_SUIF_OBJ:
        case WALK_ANNOTE_LIST:
        case WALK_AN_ANNOTE:
        case WALK_IMMED_LIST:
        case WALK_TREE_NODE:
        case WALK_TREE_LOOP:
        case WALK_TREE_FOR:
        case WALK_TREE_IF:
        case WALK_INSTRUCTION:
        case WALK_LDC_INSTR:
        case WALK_BJ_INSTR:
        case WALK_MBR_INSTR:
        case WALK_LAB_INSTR:
        case WALK_A_SYMBOL:
        case WALK_A_VARIABLE:
        case WALK_PROC_SYM:
        case WALK_VAR_DEF:
        case WALK_A_TYPE:
        case WALK_ARRAY_TYPE:
            assert(FALSE);
        default:
            assert(FALSE);
      }
  }

void so_walker::replace_sym(sym_node *replacement)
  {
    assert(!walk_stack->is_empty());
    walk_frame_data *stack_top = walk_stack->head()->contents;
    switch (stack_top->tag)
      {
        case WALK_AN_IMMED:
          {
            walk_frame_an_immed_data *this_immed_data =
                    &(stack_top->u.an_immed_data);
            immed old_immed = this_immed_data->the_immed;
            assert(old_immed.is_symbol());
            if (replacement == NULL)
              {
                immed_replacement_internal(immed(0), walk_stack->head());
              }
            else
              {
                immed_replacement_internal(immed(replacement,
                                                 old_immed.offset()),
                                           walk_stack->head());
              }
            break;
          }
        case WALK_OPERAND:
          {
            walk_frame_operand_data *this_operand_data =
                    &(stack_top->u.operand_data);
            operand old_op = this_operand_data->the_op;
            assert(old_op.is_symbol());
            if (replacement == NULL)
              {
                op_replacement_internal(operand(), walk_stack->head());
              }
            else if (!replacement->is_var())
              {
                error_line(1, replacement,
                           "attempt to replace var_sym in operand with "
                           "non-var sym_node");
              }
            else
              {
                var_sym *new_var = (var_sym *)replacement;
                op_replacement_internal(operand(new_var), walk_stack->head());
              }
            break;
          }
        case WALK_TREE_LOOP:
          {
            walk_frame_tree_loop_data *this_tree_loop_data =
                    &(stack_top->u.tree_loop_data);
            tree_loop *this_loop = this_tree_loop_data->the_loop;
            if (replacement == NULL)
              {
                error_line(1, replacement,
                           "attempt to replace label of tree_loop with "
                           "nothing");
              }
            else if (!replacement->is_label())
              {
                error_line(1, replacement,
                           "attempt to replace label of tree_loop with "
                           "non-label sym_node");
              }
            else
              {
                label_sym *new_label = (label_sym *)replacement;
                switch (this_tree_loop_data->tag)
                  {
                    case WALK_LOOP_BREAK:
                        this_loop->set_contlab(new_label);
                        break;
                    case WALK_LOOP_TOP:
                        this_loop->set_brklab(new_label);
                        break;
                    case WALK_LOOP_DONE:
                        this_loop->set_toplab(new_label);
                        break;
                    default:
                        assert(FALSE);
                  }
              }
            break;
          }
        case WALK_TREE_FOR:
          {
            walk_frame_tree_for_data *this_tree_for_data =
                    &(stack_top->u.tree_for_data);
            tree_for *this_for = this_tree_for_data->the_for;
            switch (this_tree_for_data->tag)
              {
                case WALK_FOR_CONTINUE:
                  {
                    if (replacement == NULL)
                      {
                        error_line(1, replacement,
                                   "attempt to replace index of tree_for with"
                                   " nothing");
                      }
                    else if (!replacement->is_var())
                      {
                        error_line(1, replacement,
                                   "attempt to replace index of tree_for with"
                                   " non-var sym_node");
                      }
                    else
                      {
                        var_sym *new_var = (var_sym *)replacement;
                        this_for->set_index(new_var);
                      }
                    break;
                  }
                case WALK_FOR_BREAK:
                  {
                    if (replacement == NULL)
                      {
                        error_line(1, replacement,
                                   "attempt to replace continue label of "
                                   "tree_for with nothing");
                      }
                    else if (!replacement->is_label())
                      {
                        error_line(1, replacement,
                                   "attempt to replace continue label of "
                                   "tree_for with non-label sym_node");
                      }
                    else
                      {
                        label_sym *new_label = (label_sym *)replacement;
                        this_for->set_contlab(new_label);
                      }
                    break;
                  }
                case WALK_FOR_DONE:
                  {
                    if (replacement == NULL)
                      {
                        error_line(1, replacement,
                                   "attempt to replace break label of "
                                   "tree_for with nothing");
                      }
                    else if (!replacement->is_label())
                      {
                        error_line(1, replacement,
                                   "attempt to replace break label of "
                                   "tree_for with non-label sym_node");
                      }
                    else
                      {
                        label_sym *new_label = (label_sym *)replacement;
                        this_for->set_brklab(new_label);
                      }
                    break;
                  }
                default:
                    assert(FALSE);
              }
            break;
          }
        case WALK_TREE_IF:
          {
            walk_frame_tree_if_data *this_tree_if_data =
                    &(stack_top->u.tree_if_data);
            tree_if *this_if = this_tree_if_data->the_if;
            assert(this_tree_if_data->tag == WALK_IF_DONE);
            if (replacement == NULL)
              {
                error_line(1, replacement,
                           "attempt to replace jumpto label of tree_if with "
                           "nothing");
              }
            else if (!replacement->is_label())
              {
                error_line(1, replacement,
                           "attempt to replace jumpto label of tree_if with "
                           "non-label sym_node");
              }
            else
              {
                label_sym *new_label = (label_sym *)replacement;
                this_if->set_jumpto(new_label);
              }
            break;
          }
        case WALK_TREE_BLOCK:
          {
            walk_frame_tree_block_data *this_tree_block_data =
                    &(stack_top->u.tree_block_data);
            tree_block *this_block = this_tree_block_data->the_block;
            assert(this_tree_block_data->tag == WALK_TB_DONE);
            assert(this_block->is_proc());
            tree_proc *this_tree_proc = (tree_proc *)this_block;
            if (replacement == NULL)
              {
                error_line(1, replacement,
                           "attempt to replace proc_sym of tree_tree_proc "
                           "with nothing");
              }
            else if (!replacement->is_proc())
              {
                error_line(1, replacement,
                           "attempt to replace proc_sym of tree_proc with "
                           "non-proc sym_node");
              }
            else
              {
                proc_sym *new_proc_sym = (proc_sym *)replacement;
                proc_sym *old_proc_sym = this_tree_proc->proc();
                old_proc_sym->set_block(NULL);
                new_proc_sym->set_block(this_tree_proc);
              }
            break;
          }
        case WALK_BJ_INSTR:
          {
            walk_frame_bj_instr_data *this_bj_instr_data =
                    &(stack_top->u.bj_instr_data);
            in_bj *this_bj = this_bj_instr_data->the_bj;
            if (replacement == NULL)
              {
                error_line(1, replacement,
                           "attempt to replace target of in_bj with nothing");
              }
            else if (!replacement->is_label())
              {
                error_line(1, replacement,
                           "attempt to replace target of in_bj with non-label"
                           " sym_node");
              }
            else
              {
                label_sym *new_label = (label_sym *)replacement;
                this_bj->set_target(new_label);
              }
            break;
          }
        case WALK_MBR_INSTR:
          {
            walk_frame_mbr_instr_data *this_mbr_instr_data =
                    &(stack_top->u.mbr_instr_data);
            in_mbr *this_mbr = this_mbr_instr_data->the_mbr;
            if (replacement == NULL)
              {
                error_line(1, replacement,
                           "attempt to replace target of in_mbr with nothing");
              }
            else if (!replacement->is_label())
              {
                error_line(1, replacement,
                           "attempt to replace target of in_mbr with non-label"
                           " sym_node");
              }
            else
              {
                label_sym *new_label = (label_sym *)replacement;
                switch (this_mbr_instr_data->tag)
                  {
                    case WALK_MBR_LABELS:
                      {
                        unsigned lab_num = this_mbr_instr_data->lab_num - 1;
                        this_mbr->set_label(lab_num, new_label);
                        break;
                      }
                    case WALK_MBR_DONE:
                      {
                        this_mbr->set_default_lab(new_label);
                        break;
                      }
                    default:
                        assert(FALSE);
                  }
              }
            break;
          }
        case WALK_LAB_INSTR:
          {
            walk_frame_lab_instr_data *this_lab_instr_data =
                    &(stack_top->u.lab_instr_data);
            in_lab *this_lab = this_lab_instr_data->the_lab;
            if (replacement == NULL)
              {
                error_line(1, replacement,
                           "attempt to replace symbol of in_lab with nothing");
              }
            else if (!replacement->is_label())
              {
                error_line(1, replacement,
                           "attempt to replace symbol of in_lab with non-label"
                           " sym_node");
              }
            else
              {
                label_sym *new_label = (label_sym *)replacement;
                this_lab->set_label(new_label);
              }
            break;
          }
        case WALK_A_VARIABLE:
          {
            walk_frame_a_variable_data *this_a_variable_data =
                    &(stack_top->u.a_variable_data);
            var_sym *this_var = this_a_variable_data->the_var;
            assert (this_a_variable_data->tag == WALK_VAR_CHILDREN);
            unsigned child_num = this_a_variable_data->child_num;
            if (child_num == 0)
              {
                int old_offset = this_var->offset();
                this_var->parent_var()->remove_child(this_var);
                if (replacement == NULL)
                  {
                    /* empty */
                  }
                else if (!replacement->is_var())
                  {
                    error_line(1, replacement,
                               "attempt to replace parent variable with "
                               "non-var sym_node");
                  }
                else
                  {
                    var_sym *new_var = (var_sym *)replacement;
                    new_var->add_child(this_var, old_offset);
                  }
              }
            else
              {
                --child_num;
                assert(child_num < this_var->num_children());
                if (replacement == NULL)
                  {
                    this_var->remove_child(this_var->child_var(child_num));
                  }
                else if (!replacement->is_var())
                  {
                    error_line(1, replacement,
                               "attempt to replace child variable with "
                               "non-var sym_node");
                  }
                else
                  {
                    var_sym *new_var = (var_sym *)replacement;
                    this_var->replace_child(child_num, new_var);
                  }
              }
            break;
          }
        case WALK_VAR_DEF:
          {
            walk_frame_var_def_data *this_var_def_data =
                    &(stack_top->u.var_def_data);
            var_def *this_def = this_var_def_data->the_def;
            assert(this_var_def_data->tag == WALK_DEF_DONE);
            if (replacement == NULL)
              {
                error_line(1, replacement,
                           "attempt to replace variable of var_def with "
                           "nothing");
              }
            else if (!replacement->is_var())
              {
                error_line(1, replacement,
                           "attempt to replace variable of var_def with "
                           "non-var sym_node");
              }
            else
              {
                var_sym *new_var = (var_sym *)replacement;
                this_def->set_variable(new_var);
              }
            break;
          }
        case WALK_ARRAY_TYPE:
          {
            walk_frame_array_type_data *this_array_type_data =
                    &(stack_top->u.array_type_data);
            array_type *this_array_type = this_array_type_data->the_array_type;
            array_bound new_bound;
            if (replacement == NULL)
              {
                /* empty */
              }
            else if (!replacement->is_var())
              {
                error_line(1, replacement,
                           "attempt to replace variable of array bound with "
                           "non-var sym_node");
              }
            else
              {
                var_sym *new_var = (var_sym *)replacement;
                new_bound = array_bound(new_var);
              }
            switch (this_array_type_data->tag)
              {
                case WALK_ARRAY_UPPER:
                    this_array_type->set_lower_bound(new_bound);
                    break;
                case WALK_ARRAY_DONE:
                    this_array_type->set_upper_bound(new_bound);
                    break;
                default:
                    assert(FALSE);
              }
            break;
          }
        default:
            assert(FALSE);
      }
  }

void so_walker::replace_type(type_node *replacement)
  {
    assert(!walk_stack->is_empty());
    walk_frame_data *stack_top = walk_stack->head()->contents;
    switch (stack_top->tag)
      {
        case WALK_AN_IMMED:
          {
            walk_frame_an_immed_data *this_immed_data =
                    &(stack_top->u.an_immed_data);
            immed old_immed = this_immed_data->the_immed;
            assert(old_immed.is_type());
            if (replacement == NULL)
              {
                immed_replacement_internal(immed(0), walk_stack->head());
              }
            else
              {
                immed_replacement_internal(immed(replacement),
                                           walk_stack->head());
              }
            break;
          }
        case WALK_INSTRUCTION:
          {
            walk_frame_instruction_data *this_instruction_data =
                    &(stack_top->u.instruction_data);
            instruction *this_instr = this_instruction_data->the_instr;
            assert(this_instruction_data->tag == WALK_INST_CHILDREN);
            if (replacement == NULL)
              {
                error_line(1, replacement,
                           "attempt to replace instruction result type with"
                           " nothing");
              }
            else
              {
                this_instr->set_result_type(replacement);
              }
            break;
          }
        case WALK_A_VARIABLE:
          {
            walk_frame_a_variable_data *this_a_variable_data =
                    &(stack_top->u.a_variable_data);
            var_sym *this_var = this_a_variable_data->the_var;
            assert(this_a_variable_data->tag == WALK_VAR_PARENT);
            if (replacement == NULL)
              {
                error_line(1, replacement,
                           "attempt to replace variable type with nothing");
              }
            else
              {
                this_var->set_type(replacement);
              }
            break;
          }
        case WALK_PROC_SYM:
          {
            walk_frame_proc_sym_data *this_proc_sym_data =
                    &(stack_top->u.proc_sym_data);
            proc_sym *this_proc = this_proc_sym_data->the_proc;
            assert(this_proc_sym_data->tag == WALK_PROC_DONE);
            if (replacement == NULL)
              {
                error_line(1, replacement,
                           "attempt to replace procedure type with nothing");
              }
            else if (!replacement->is_func())
              {
                error_line(1, replacement,
                           "attempt to replace procedure type with "
                           "non-function type");
              }
            else
              {
                func_type *new_func_type = (func_type *)replacement;
                this_proc->set_type(new_func_type);
              }
            break;
          }
        case WALK_A_TYPE:
          {
            walk_frame_a_type_data *this_a_type_data =
                    &(stack_top->u.a_type_data);
            type_node *this_type = this_a_type_data->the_type;
            assert(this_a_type_data->tag == WALK_TYPE_REF_TYPES);
            unsigned ref_type_num = this_a_type_data->ref_type_num;
            assert(ref_type_num > 0);
            --ref_type_num;
            assert(ref_type_num < this_type->num_ref_types());
            if (replacement == NULL)
              {
                error_line(1, replacement,
                           "attempt to replace reference type with nothing");
              }
            else
              {
                this_type->set_ref_type(ref_type_num, replacement);
              }
            break;
          }
        default:
            assert(FALSE);
      }
  }

void so_walker::replace_op(operand replacement)
  {
    assert(!walk_stack->is_empty());
    op_replacement_internal(replacement, walk_stack->head());
  }


boolean so_walker::in_dest_op(void)
  {
    assert(!walk_stack->is_empty());
    walk_frame_data_list_e *follow_stack = walk_stack->head();
    while (follow_stack != NULL)
      {
        walk_frame_data *this_frame = follow_stack->contents;
        if (this_frame->tag == WALK_OPERAND)
          {
            walk_frame_operand_data *this_operand_data =
                    &(this_frame->u.operand_data);
            return this_operand_data->is_destination;
          }
        follow_stack = follow_stack->next();
      }
    return FALSE;
  }

boolean so_walker::in_annotation(void)
  {
    assert(!walk_stack->is_empty());
    walk_frame_data_list_e *follow_stack = walk_stack->head();
    while (follow_stack != NULL)
      {
        walk_frame_data *this_frame = follow_stack->contents;
        if (this_frame->tag == WALK_AN_ANNOTE)
            return TRUE;
        follow_stack = follow_stack->next();
      }
    return FALSE;
  }


/*----------------------------------------------------------------------*
    End Public Method Implementations
 *----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*
    Begin Private Method Implementations
 *----------------------------------------------------------------------*/


void so_walker::immed_replacement_internal(immed new_immed,
                        walk_frame_data_list_e *starting_point)
  {
    assert(starting_point != NULL);
    walk_frame_data *start_data = starting_point->contents;
    assert(start_data != NULL);
    assert(start_data->tag == WALK_AN_IMMED);
    start_data->u.an_immed_data.the_immed = new_immed;

    walk_frame_data_list_e *next_e = starting_point->next();
    assert(next_e != NULL);
    walk_frame_data *next_data = next_e->contents;
    switch(next_data->tag)
      {
        case WALK_IMMED_LIST:
          {
            walk_frame_immed_list_data *this_immed_list_data =
                    &(next_data->u.immed_list_data);
            immed_list_e *the_elem = this_immed_list_data->previous;
            assert(the_elem != NULL);
            the_elem->contents = new_immed;
            break;
          }
        case WALK_LDC_INSTR:
          {
            walk_frame_ldc_instr_data *this_ldc_instr_data =
                    &(next_data->u.ldc_instr_data);
            in_ldc *this_ldc = this_ldc_instr_data->the_ldc;
            this_ldc->set_value(new_immed);
            break;
          }
        default:
            assert(FALSE);
      }
  }

void so_walker::op_replacement_internal(operand new_op,
                                        walk_frame_data_list_e *starting_point)
  {
    assert(starting_point != NULL);
    walk_frame_data *start_data = starting_point->contents;
    assert(start_data != NULL);
    assert(start_data->tag == WALK_OPERAND);
    if (start_data->u.operand_data.is_destination && new_op.is_instr())
      {
        error_line(1, new_op.instr(),
                   "attempt to replace destination operand with instruction"
                   " operand");
      }
    start_data->u.operand_data.the_op = new_op;

    walk_frame_data_list_e *next_e = starting_point->next();
    assert(next_e != NULL);
    walk_frame_data *next_data = next_e->contents;
    switch(next_data->tag)
      {
        case WALK_AN_IMMED:
          {
            immed_replacement_internal(immed(new_op), next_e);
            break;
          }
        case WALK_INSTRUCTION:
          {
            walk_frame_instruction_data *this_instruction_data =
                    &(next_data->u.instruction_data);
            instruction *this_instr = this_instruction_data->the_instr;
            switch (this_instruction_data->tag)
              {
                case WALK_INST_SRCS:
                  {
                    assert(!start_data->u.operand_data.is_destination);
                    unsigned src_num = this_instruction_data->src_num - 1;
                    assert(src_num < this_instr->num_srcs());
                    if (this_instr->src_op(src_num).is_instr())
                        this_instr->src_op(src_num).remove();
                    this_instr->set_src_op(src_num, new_op);
                    break;
                  }
                case WALK_INST_RESULT_TYPE:
                  {
                    assert(start_data->u.operand_data.is_destination);
                    this_instr->set_dst(new_op);
                    break;
                  }
                default:
                    assert(FALSE);
              }
            break;
          }
        default:
            assert(FALSE);
      }
  }


/*----------------------------------------------------------------------*
    End Private Method Implementations
 *----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*
    Begin Internal Function Implementations
 *----------------------------------------------------------------------*/

/*----------------------------------------------------------------------*
    End Internal Function Implementations
 *----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*
    Begin Private Function Implementations
 *----------------------------------------------------------------------*/

/*----------------------------------------------------------------------*
    End Private Function Implementations
 *----------------------------------------------------------------------*/
