/* file "walk.h" */

/*  Copyright (c) 1995 Stanford University

    All rights reserved.

    This software is provided under the terms described in
    the "suif_copyright.h" include file. */

#include <suif_copyright.h>

/*
 *  This is the header file for walkers over SUIF code.
 */

#ifndef WALK_H
#define WALK_H

#include <suif1.h>

/*
 *  The code here has essentially the same pupose as the
 *  tree_node::map(), tree_node_list::map(), tree_instr::instr_map(),
 *  and instruction::src_map() methods provided by the SUIF library.
 *  This new set of code reflects the experience of writing a great
 *  deal of code using SUIF for a variety of purposes.  It is intended
 *  to solve some of the problems and inconveniences of using the
 *  existing methods in practice and generalize them to new kinds of
 *  code walking that is often needed.
 *
 *  The code provided here will walk over everything in a given piece
 *  of SUIF code: through all the levels of tree_nodes, through
 *  symbols and types owned by symbol tables, through expression trees
 *  within tree_instrs, and recursively through annotations and
 *  everything within them, including nested expression trees.
 *
 *  At each target it finds, it calls a user-provided function and
 *  passes that target.  There are functions that use all owned SUIF
 *  objects as the targets, that use annotations as the targets, and
 *  some that take symbol or type references or operands as targets.
 *
 *  The user's function is optionally provided a pointer to an
 *  so_walker object that controls the walk.  This allows the user
 *  function to call methods to communicate with the walker when this
 *  is necessary, for example to replace a type reference or specify
 *  that the walker should omit the walking of children of a given
 *  object or change the next object to walk over in case the code is
 *  being modified by the user function.  It can also allow the walk
 *  to be aborted early for efficiency reasons once the user's
 *  function has determined that can be done.  That way each
 *  application can call whatever methods are needed and ignore the
 *  rest (and new so_walker methods can be added without affecting
 *  existing code).  This is in contrast to the instruction::src_map()
 *  method, for example, that allows the mapped function to change the
 *  operand by requiring a boolean be returned, causing more trouble
 *  for the majority of operations which do not require this
 *  capability of changing an operand.
 *
 *  The so_walker object can be created automatically by calling a
 *  function, or the user can explicitly create it before the walk and
 *  call methods to set options for the walk if desired.  This avoids
 *  all the additional parameters in the tree_node::map() method, for
 *  example, to a data pointer and set the ``preorder'' and
 *  ``descend'' attributes.
 *
 *  Instead of a single ``void *'' for the user to pass data to the
 *  node function, the so_walker provides an unlimited number of
 *  pieces of data of void * or any basic arithmetic type.  This
 *  avoids unnecessary arguments for functions that need no data or
 *  the cumbersome definition of new structure types and building and
 *  initialization of such structures just to pass more than one field
 *  of data to the node function.
 *
 *  Different pre-order and post-order node functions can be used at
 *  the same time, or the same function may be used for both.  The
 *  pre-order function is called before walking the children of a SUIF
 *  object, and the post-order function is called afterward.  Either
 *  function may be omitted.  The default is to have a single
 *  pre-order function.
 */

class so_walker;
class walk_frame_data_list;
class walk_frame_data_list_e;

union any_type
  {
    void *ptr;
    long sl;
    unsigned long ul;
    int si;
    unsigned ui;
    short ss;
    unsigned short us;
    char c;
    signed char sc;
    unsigned char uc;
    long double ld;
    double d;
    float f;

    any_type(void) { }
    any_type(void *init_ptr) : ptr(init_ptr)  { }
    any_type(long init_sl) : sl(init_sl)  { }
    any_type(unsigned long init_ul) : ul(init_ul)  { }
    any_type(int init_si) : si(init_si)  { }
    any_type(unsigned init_ui) : ui(init_ui)  { }
    any_type(short init_ss) : ss(init_ss)  { }
    any_type(unsigned short init_us) : us(init_us)  { }
    any_type(char init_c) : c(init_c)  { }
    any_type(signed char init_sc) : sc(init_sc)  { }
    any_type(unsigned char init_uc) : uc(init_uc)  { }
    any_type(long double init_ld) : ld(init_ld)  { }
    any_type(double init_d) : d(init_d)  { }
    any_type(float init_f) : f(init_f)  { }
  };

typedef void simple_so_walkee(suif_object *);
typedef void basic_so_walkee(suif_object *, so_walker *);
typedef void simple_annote_walkee(annote *);
typedef void basic_annote_walkee(annote *, so_walker *);
typedef void simple_sym_walkee(sym_node *);
typedef void basic_sym_walkee(sym_node *, so_walker *);
typedef void simple_type_walkee(type_node *);
typedef void basic_type_walkee(type_node *, so_walker *);
typedef void simple_op_walkee(operand);
typedef void basic_op_walkee(operand, so_walker *);

/*
 *  SUIF Object Walker
 */
class so_walker
  {
private:
    walk_frame_data_list *walk_stack;

    boolean result_defined;
    any_type result_value;

    unsigned char first_data_defined;
    any_type first_values[4];
    unsigned long remainder_length;
    unsigned char *remainder_data_defined;
    any_type *remainder_values;

    simple_sym_walkee *leaf_simple_sym_walkee;
    basic_sym_walkee *leaf_basic_sym_walkee;
    simple_type_walkee *leaf_simple_type_walkee;
    basic_type_walkee *leaf_basic_type_walkee;

    simple_so_walkee *pre_simple_so_walkee;
    basic_so_walkee *pre_basic_so_walkee;
    simple_annote_walkee *pre_simple_annote_walkee;
    basic_annote_walkee *pre_basic_annote_walkee;
    simple_op_walkee *pre_simple_op_walkee;
    basic_op_walkee *pre_basic_op_walkee;

    simple_so_walkee *post_simple_so_walkee;
    basic_so_walkee *post_basic_so_walkee;
    simple_annote_walkee *post_simple_annote_walkee;
    basic_annote_walkee *post_basic_annote_walkee;
    simple_op_walkee *post_simple_op_walkee;
    basic_op_walkee *post_basic_op_walkee;


    void immed_replacement_internal(immed new_immed,
                                    walk_frame_data_list_e *starting_point);
    void op_replacement_internal(operand new_op,
                                 walk_frame_data_list_e *starting_point);

public:
    so_walker(void);
    ~so_walker(void);

        /* Methods to be called either before, during, or after the walk */

    void set_result(any_type);
    any_type get_result(void);
    void set_data(unsigned data_num, any_type);
    any_type get_data(unsigned data_num);

        /* Methods to be called only before the walk */

    /*
     *  Note that the walkee function should be omitted in the call to
     *  walk() only if at least one function has already been set by a
     *  call to set_leaf_function(), set_pre_function() or
     *  set_post_function(), or else the walk will have no effect.
     */
    void walk(suif_object *);
    void walk(suif_object *, simple_so_walkee *);
    void walk(suif_object *, basic_so_walkee *);
    void walk(suif_object *, simple_annote_walkee *);
    void walk(suif_object *, basic_annote_walkee *);
    void walk(suif_object *, simple_sym_walkee *);
    void walk(suif_object *, basic_sym_walkee *);
    void walk(suif_object *, simple_type_walkee *);
    void walk(suif_object *, basic_type_walkee *);
    void walk(suif_object *, simple_op_walkee *);
    void walk(suif_object *, basic_op_walkee *);

    any_type walk(suif_object *, any_type default_result);
    any_type walk(suif_object *, simple_so_walkee *, any_type default_result);
    any_type walk(suif_object *, basic_so_walkee *, any_type default_result);
    any_type walk(suif_object *, simple_annote_walkee *,
                  any_type default_result);
    any_type walk(suif_object *, basic_annote_walkee *,
                  any_type default_result);
    any_type walk(suif_object *, simple_sym_walkee *, any_type default_result);
    any_type walk(suif_object *, basic_sym_walkee *, any_type default_result);
    any_type walk(suif_object *, simple_type_walkee *,
                  any_type default_result);
    any_type walk(suif_object *, basic_type_walkee *, any_type default_result);
    any_type walk(suif_object *, simple_op_walkee *, any_type default_result);
    any_type walk(suif_object *, basic_op_walkee *, any_type default_result);

    void set_leaf_function(simple_sym_walkee *);
    void set_leaf_function(basic_sym_walkee *);
    void set_leaf_function(simple_type_walkee *);
    void set_leaf_function(basic_type_walkee *);

    void set_pre_function(simple_so_walkee *);
    void set_pre_function(basic_so_walkee *);
    void set_pre_function(simple_annote_walkee *);
    void set_pre_function(basic_annote_walkee *);
    void set_pre_function(simple_op_walkee *);
    void set_pre_function(basic_op_walkee *);

    void set_post_function(simple_so_walkee *);
    void set_post_function(basic_so_walkee *);
    void set_post_function(simple_annote_walkee *);
    void set_post_function(basic_annote_walkee *);
    void set_post_function(simple_op_walkee *);
    void set_post_function(basic_op_walkee *);

        /* Methods to be called only during the walk */

    /* End the walk immediately after this call to the walkee
     * finishes. */
    void set_break(void);

    /* Do not do any more walking within the current node, instead go
     * on to the next node. */
    void set_skip(void);

    /* Replace the node given to the walkee with replacement.  Note
     * that each of these may only be called by a walkee function of
     * the appropriate type (a basic_so_walkee could call
     * replace_object(), a basic_sym_walkee could call replace_sym(),
     * etc.).  Any other call to one of these methods is an error. */
    void replace_object(suif_object *replacement);
    void replace_sym(sym_node *replacement);
    void replace_type(type_node *replacement);
    void replace_op(operand replacement);

    boolean in_dest_op(void);
    boolean in_annotation(void);
  };

extern void walk(suif_object *, simple_so_walkee *);
extern void walk(suif_object *, basic_so_walkee *);
extern void walk(suif_object *, simple_annote_walkee *);
extern void walk(suif_object *, basic_annote_walkee *);
extern void walk(suif_object *, simple_sym_walkee *);
extern void walk(suif_object *, basic_sym_walkee *);
extern void walk(suif_object *, simple_type_walkee *);
extern void walk(suif_object *, basic_type_walkee *);
extern void walk(suif_object *, simple_op_walkee *);
extern void walk(suif_object *, basic_op_walkee *);

extern any_type walk(suif_object *, simple_so_walkee *,
                     any_type default_result);
extern any_type walk(suif_object *, basic_so_walkee *,
                     any_type default_result);
extern any_type walk(suif_object *, simple_annote_walkee *,
                     any_type default_result);
extern any_type walk(suif_object *, basic_annote_walkee *,
                     any_type default_result);
extern any_type walk(suif_object *, simple_sym_walkee *,
                     any_type default_result);
extern any_type walk(suif_object *, basic_sym_walkee *,
                     any_type default_result);
extern any_type walk(suif_object *, simple_type_walkee *,
                     any_type default_result);
extern any_type walk(suif_object *, basic_type_walkee *,
                     any_type default_result);
extern any_type walk(suif_object *, simple_op_walkee *,
                     any_type default_result);
extern any_type walk(suif_object *, basic_op_walkee *,
                     any_type default_result);

#endif /* WALK_H */
