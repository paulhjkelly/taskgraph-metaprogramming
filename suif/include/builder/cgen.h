/* file "cgen.h" */

/*  Copyright (c) 1994 Stanford University

    All rights reserved.

    This software is provided under the terms described in
    the "suif_copyright.h" include file. */

#include <suif_copyright.h>

#pragma interface

/***************************************************************************
 ***************************************************************************
 *
 *                     S U I F   C o m p i l e r  S y s t e m
 *
 *                                B U I L D E R
 *
 ***************************************************************************
 ***************************************************************************/


extern int bdebug;

/*************************************************************************
 * what suif is generated                                                *
 *************************************************************************/
enum gen_tree_type { gtt_none, gtt_sym, gtt_trn, gtt_tnl, gtt_ins};

/*************************************************************************
 ***                                                                   ***
 *** A tree corresponding to the suif code that is generated.          ***
 ***                                                                   ***
 *************************************************************************/
struct gen_tree : public builder_base {
    gen_tree * par;
    type_node * tp;
    gen_tree_type gtt;
    union {
        tree_node      * trn;
        tree_node_list * tnl;
        instruction    * ins;
        sym_node       * sym;
    } gen;

    gen_tree() { par = NULL; gtt = gtt_none; }
    gen_tree(gen_tree * p) { par = p; gtt = gtt_none; }

    tree_node_list * get_list();
    tree_node      * get_node();
    instruction    * get_inst();
    operand          get_oper();


    void print_suif(FILE * fp=stdout) const;
};


/*************************************************************************
 ***                                                                   ***
 *** Suif code generator.                                              ***
 ***                                                                   ***
 *************************************************************************/
class generator : private builder_base {
public:
    tree_node      * return_tree_node(const block &);
    tree_node_list * return_tree_node_list(const block &);
    instruction    * return_instruction(const block &);

    generator() { }
private:
    gen_tree * gen_any(const block &, gen_tree *);
    gen_tree * gen_ldc(immed, gen_tree *);
    gen_tree * gen_lod(const block &, gen_tree *);
    gen_tree * gen_cpy(var_sym *, gen_tree *);
//    gen_tree * gen_lod(immed, gen_tree *);
    gen_tree * gen_op(const block &, gen_tree *);
    type_node * match_type(in_rrr * inst, type_node * c1, type_node * c2);
    gen_tree * gen_for_node(const block &, gen_tree *, block_symtab *);
    gen_tree * gen_if_node(const block &, gen_tree *, block_symtab *);
    gen_tree * gen_loop_node(const block &, gen_tree *, block_symtab *);
    gen_tree * gen_return_inst(const block &, gen_tree *);
    gen_tree * gen_array_access(const block &, gen_tree *);
    gen_tree * gen_stmt_list(const block & b, gen_tree * par = NULL);
    gen_tree * gen_call(const block & b, gen_tree * par);

    tree_node      * gen_tree_node(const block &, gen_tree *);
    tree_node_list * gen_tree_node_list(const block &, gen_tree *);
    instruction    * gen_instruction(const block &, gen_tree *);

    type_node * get_type(immed ic);
    type_node * get_type(operand ic);
    type_node * get_type();

    type_node * cast_to(type_node * c1, type_node * c2);
    void create_mk_cvt(in_rrr * inst, type_node * c, boolean first);
    type_node * create_mk_cvt(in_rrr * inst, type_node * c1, type_node * c2);

    static in_rrr * mk_cvt(operand, type_node * out);
};

