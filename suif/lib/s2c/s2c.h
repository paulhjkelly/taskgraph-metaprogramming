/* file "s2c.h" */

/*  Copyright (c) 1994 Stanford University

    All rights reserved.

    This software is provided under the terms described in
    the "suif_copyright.h" include file. */

#include <suif_copyright.h>

#define _MODULE_ "s2c"

#include <suif1.h>
#include <useful.h>

#include "io_class.h"
#include "ctree.h"

/*
 *  The user may specify via annotations a format string for a given
 *  io_gen instruction name.  The format string tells how the generic
 *  operand is to be printed.  The ``%'' character is used as an
 *  escape in the format string -- other characters are generally
 *  printed directly.  The following escape sequences are recognized:
 *
 *    * %%         -- print one ``%'' character
 *    * %a         -- print one of the arguments
 *    * %n<text>%m -- print all remaining unmatched arguments,
 *                    separated by <text> if there is more than one,
 *                    or nothing if there are no unmatched arguments
 *
 *  Note that there can be any number of ``%a'' directives, possibly
 *  before and after the ``%n'' directive, but there may only be one
 *  ``%n'' directive.  Arguments to all ``%a'' directives are matched
 *  first, either from the beginning of the argument list for those
 *  preceeding a ``%n'' directive or from the end for those coming
 *  after one.  It is an error for there to be too few arguments to
 *  match all ``%a'' directives, but it is not an error to have too
 *  many arguments.
 *
 *  Within a ``%n'' directive, the <text> string may include ``%%'',
 *  which translates to one ``%'' but no other occurances of ``%''.
 *
 *  The default format if nothing else is given is equivalent to
 *  "<name>(%n, %)" if there are any arguments or "<name>" if there
 *  are no arguments, where <name> is the name of the generic
 *  instruction.
 *
 *  EXAMPLES:
 *
 *      Format                        output
 *    "fun(%n, %)"   "fun()" "fun(op1)" "fun(op1, op2)" "fun(op1, op2, op3)"
 *    "fun()"        "fun()" "fun()"    "fun()"         "fun()"
 *    "{%a: %n, %}"  <error> "{op1: }"  "{op1: op2}"    "{op1: op2, op3}"
 *    "%a ? %a : %a" <error> <error>    <error>         "op1 ? op2 : op3"
 */


enum annote_place
  {
#define place2(x, y) ap_ ## y,
#define place(x) place2(x, x)
#include "places.h"
#undef place
#undef place2
    ap_last
  };

typedef struct
  {
    if_ops opcode;
    const char *op_name;
    const char *replacement;
    int num_operands;
    boolean used;
  } macro_op_data;

typedef struct
  {
    boolean space_around_binops;
    boolean space_around_assignments;
    boolean space_after_commas;
    boolean new_line_for_open_brace;
    boolean new_line_for_else;
    boolean indent_single_statement_bodies;
    int statement_indent;
    int single_statement_body_indent;
    int brace_indent;
    int param_indent;
    int label_indent;
    boolean extern_on_func_decl;
    int max_comma_items_per_line; /* 0 implies no limit */
  } c_style_type;

extern boolean log_simp;
extern boolean write_pseudo;
extern boolean keep_casts;
extern boolean omit_header;
extern boolean no_warn;
extern boolean always_intnum;
extern boolean array_exprs;
extern boolean all_annotes_to_comments;
extern boolean gcc_bug_flag;
extern boolean drop_bounds;
extern boolean ll_suffix;
extern boolean explicit_zero_init;
extern boolean limit_escape_sequences;
extern boolean fill_field_space_with_char_arrays;
extern char **show_annote_names;
extern boolean show_annote_opcodes[io_last];
extern boolean show_annote_places[ap_last];

extern int numindent;
extern boolean in_comment;
extern boolean in_pound_line;

extern FILE *outfile;

extern const char *k_s2c_label_name_used;
extern const char *k_s2c_needs_forward_declaration;
extern const char *k_s2c_comments;
extern const char *k_s2c_genop_format;
extern const char *k_s2c_one_use;
extern const char *k_s2c_multi_use;
extern const char *k_s2c_init_field;
extern const char *k_s2c_original_field_names;
extern const char *k_s2c_pragma;
extern const char *k_s2c_preamble_pragma;
extern const char *k_s2c_pound_line;

extern const char *k_builtin_args;

extern const char *reserved_words[];
extern macro_op_data macro_table[];
extern c_style_type c_style;

void process_globals(ctree*, global_symtab*);
ctree *process_proc(proc_sym*);
extern ctree *process_node_list(tree_node_list *nl, label_sym *target,
                                ctree **conditional_expr);
extern ctree *process_solo_instr(instruction *the_instr);
extern ctree *process_base_inst(instruction *inst, ctree **pending_expr,
                                label_sym **pending_target, ctree **pre_instr);
extern ctree *process_expr_inst(instruction *inst);
ctree *operand_to_tree(const operand &op);
ctree *process_if(tree_if*);
ctree *process_block(tree_block*);
ctree *process_loop(tree_loop*);
ctree *process_for(tree_for*);

const char *make_c_type(type_node *);
extern boolean expressable_as_char_const(i_integer c);
extern void get_c_char(int c, char **p, boolean is_string);
void print_c_char(io_class *, i_integer);
extern void remove_trailing_zero(char *string);
extern void comment_operand(operand the_op);
extern void comment_instr(instruction *the_instr);
const char *make_c_sym_type(var_sym*);
const char *make_c_agg_type(type_node*, const char*);
const char *make_c_proc_type(proc_sym*, const char **posttype);
extern const char *make_c_proc_prototype(proc_sym *psym);
extern void print_float(io_class *out, immed float_value);
extern boolean array_flattening_needed(in_array *the_array,
                                       array_type *base_array_type);
extern ctree *force_type(ctree *the_ctree, type_node *the_type);
extern void mistake(boolean *warned_var, suif_object *location, const char *message,
                    ...);
extern immed comment_for_annote(annote *the_annote);
extern const char *lookup_gen_op(const char *op_name, int num_args);
extern void register_gen_op(const char *name, const char *format, boolean is_default,
                            int num_args);
extern type_node *composite(type_node *type_1, type_node *type_2);

ctree *process_params(proc_sym *psym);
extern void transform_and_print_ctree(io_class *out, ctree *the_ctree);


    /* Defined in "preprocess.cc": */

extern void preprocess_symtab(base_symtab *the_symtab);
extern void preprocess_proc(tree_proc *the_proc);
extern void comment_object(suif_object *the_object);
extern void compact_a_struct(struct_type *the_struct);


    /* Defined in "namespace.cc": */
extern void reset_namespace ( );

extern void fix_names(base_symtab *the_symtab);


    /* Defined in "ldc.cc": */

extern enum_type *watch_enum_type;
extern unsigned watch_enum_member;
extern boolean watch_enum_used;

extern ctree *ldc_tree(type_node *the_type, immed value,
                       immed_list *field_immeds = NULL);
extern ctree *get_address_with_offset(ctree *object_tree,
                                      type_node *object_type,
                                      type_node *result_type, int offset,
                                      immed_list *field_immeds);


    /* Defined in "init.cc": */

extern void init_initialization(void);
extern ctree *build_initializers(type_node *the_type,
                                 base_init_struct_list *initializers);
extern boolean sym_ref_in_initializers(annote_list *initializers,
                                       sym_node *the_symbol);
extern boolean enum_const_ref_in_initializers(var_def *the_def,
                                              enum_type *the_enum,
                                              unsigned member_num);
extern void init_summaries_for_symtab(base_symtab *the_symtab, boolean global);
extern void add_and_mark_init_fields(struct_type *the_struct);
extern boolean is_init_field(type_node *field_type);


    /* Defined in "re_eval.cc": */

extern void fix_re_eval_instr(instruction *the_instr);
extern void fix_re_eval_for(tree_for *the_for);

    /* Defined in "array_copy.cc": */

extern void pass1_array_copy_on_instr(instruction *the_instr);
extern void pass1_array_copy_on_symtab(base_symtab *the_symtab);
extern void fix_array_copy_on_instr(instruction *the_instr);
extern void fix_function_array_types(base_symtab *the_symtab);

    /* Defined in "pass1.cc": */

extern void pass1_on_proc(tree_proc *the_proc);
extern void pass1_on_symtab(base_symtab *the_symtab);

    /* Defined in "group.cc": */

extern void init_group_layout(void);
extern void layout_groups_for_symtab(base_symtab *the_symtab);
extern void print_struct_with_fields(struct_type *the_struct, io_class *out,
                                     int nindent);
extern boolean struct_is_union(struct_type *the_struct);
extern void print_struct_short(struct_type *the_struct, io_class *out);
extern struct_type *init_view(struct_type *the_struct);
extern boolean init_is_union(struct_type *the_struct);
extern const char *output_field_name(struct_type *the_struct, unsigned field_num);
