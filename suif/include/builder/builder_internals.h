/* file "builder_internals.h" */

/*  Copyright (c) 1994 Stanford University

    All rights reserved.

    This software is provided under the terms described in
    the "suif_copyright.h" include file. */

#include <suif_copyright.h>

#include <cstdarg>

#pragma interface

/***************************************************************************
 ***************************************************************************
 *
 *                     S U I F   C o m p i l e r  S y s t e m
 *
 *                               B U I L D E R
 *
 ***************************************************************************
 ***************************************************************************/

/*************************************************************************
 * what type of error, for error handler                                 *
 *************************************************************************/
enum error_type { et_none, et_type,
                  et_grammer, et_oper,
                  et_suif, et_gen };


/*************************************************************************
 * for internal parsing etc.                                             *
 *************************************************************************/
enum block_type { bt_none,  bt_expr,
                  bt_prim, bt_lval,
                  bt_label, bt_sym,
                  bt_stmt, bt_param,
                  bt_parmlist, bt_proc };

/*************************************************************************
 *                                                                       *
 *************************************************************************/
enum node_type {  nt_none, nt_oper,
                  nt_code, nt_const,
                  nt_array, nt_call,
                  nt_stmt_list, nt_for,
                  nt_if, nt_while,
                  nt_do, nt_goto,
                  nt_return };

/*************************************************************************
 * an unary_or_binary, uniary, binary or assignment operation            *
 *************************************************************************/
enum op_type { ot_none, ot_ub,
               ot_uni, ot_bin,
               ot_ass };


/*************************************************************************
 ***                                                                   ***
 *** Base type used by most of the classes.  This do the error         ***
 *** handling and records the current procedure.                       ***
 ***                                                                   ***
 *************************************************************************/
class builder_base {
    static error_type  error_level;
    static const char *      error_file;
    static int         error_line;



protected:
    static tree_proc * proc;
    static block_symtab * BlockSymTab;

    static void error_internal(const char *fmt, va_list args);
public:
    static void error(error_type, const char *fmt,...);
    static void _error(const char *fmt,...);
    static void _error_set(error_type t, const char * f, int l) { error_level = t;
                                                            error_file = f;
                                                            error_line = l; }
    static tree_proc * get_proc()                         { assert(proc);
                                                            return proc; }
};




/*************************************************************************
 ***                                                                   ***
 *** unary, binary or assignment operator.                             ***
 ***                                                                   ***
 *************************************************************************/
struct any_oper:public builder_base {
    op_type ot;
    unary_op  uop;
    binary_op bop;
    assign_op aop;

    any_oper() { clear(); }
    any_oper(const any_oper & a)        { init(a); }
    any_oper(const any_oper * a)        { init(*a); }
    any_oper(op_type op, int x)         { clear(); set(op, x); }

    void init(const any_oper & a);
    void clear()                        { ot = ot_none; }
    any_oper & operator=(const any_oper & b)  { init(b); return *this; }

    void set(op_type, int);
    const char * get() const;
    if_ops get_ifops() const;
};

/*************************************************************************
 * what type of SUIF to generate/store                                   *
 *************************************************************************/
enum code_type { ct_none, ct_sym_node,
                 ct_struct_access,
                 ct_tree_node, ct_tree_node_list,
                 ct_instruction, ct_type_node};

/*************************************************************************
 * value constant types                                                  *
 *************************************************************************/
enum const_type { cst_none, cst_int, cst_ext_int,
                  cst_double, cst_float, cst_ext_float,
                  cst_char, cst_str};

class block;

/*************************************************************************
 * for suif structures that need to be duplicated if more than one copy  *
 * is generated, this strucure keeps tab of the number of suif copies    *
 * generated for a given suif structure.                                 *
 *************************************************************************/
struct num_copies {
    int num;
    num_copies(int i=0) { num = i; }
};


/*************************************************************************
 ***                                                                   ***
 ***                                                                   ***
 *************************************************************************/
struct struct_access:public builder_base {
    var_sym * var;
    int offset;
    type_node * type;
    immed_list field_list;
    boolean dref_ptr;

    struct_access();
    struct_access(struct_access *);
    struct_access(var_sym * base, int field);
    struct_access(var_sym * base, char * fld);
    struct_access(struct_access * base, int field);
    struct_access(struct_access * base, char * fld);
    ~struct_access();
    void print() const;
private:
    void mk_st_ac(var_sym * v, type_node * tp, int off, int field);
    void cp_field_list(immed_list *);
    static int get_field(type_node * tp, char *);
};


/*************************************************************************
 ***                                                                   ***
 *** keep the information for each block.                              ***
 *** keep the code being read-in/generated and the constant value.     ***
 ***                                                                   ***
 *************************************************************************/
struct block_info:public builder_base {
    any_oper op;
    union {
        sym_node       * sym;
        struct_access  * sta;
        tree_node      * trn;
        tree_node_list * tnl;
        instruction    * ins;
        type_node      * typ;
    } code;
    code_type ct;
    num_copies * ncpy;          // for tree_nodes and tree_node_lists

    union {
        int     icon;
        double  dcon;
        float   fcon;
        char    ccon;
        const char*  scon;
    } con;
    const_type cst;

    block_info()                        { op.ot = ot_none;
                                          ct = ct_none;
                                          cst = cst_none;
                                          ncpy = NULL; }
    block_info(const block_info & b)    { init(b); }
    block_info(const block_info * b)    { init(*b); }
    void init(const block_info & b);
    block_info & operator=(const block_info & b) { init(b); return *this; }
    void set(any_oper a)                { op = a; }
    void set(op_type t, int o)          { op.set(t, o); }
    void set(sym_node * s)              { code.sym = s;
                                          ct = ct_sym_node; }
    void set(struct_access * sa)        { code.sta = sa;
                                          ct = ct_struct_access; }
    void set(tree_node * t,
             boolean duplicate=FALSE)   { code.trn = t;
                                          ct = ct_tree_node;
                                          ncpy = new num_copies(duplicate?1:0);
                                        }
    void set(tree_node_list * l,
             boolean duplicate=FALSE)   { code.tnl = l;
                                          ct = ct_tree_node_list;
                                          ncpy =new num_copies(duplicate?1:0);
                                        }
    void set(instruction *i)            { code.ins = i;
                                          ct = ct_instruction; }
    void set(int i)                     { con.icon = i;
                                          cst = cst_int; }
    void set_ext_int(const char *ext_int) { con.scon = ext_int;
                                          cst = cst_ext_int; }
    void set(double d)                  { con.dcon = d;
                                          cst = cst_double; }
    void set(float f)                   { con.fcon = f;
                                          cst = cst_float; }
    void set_ext_float(const char *ext_float) { con.scon = ext_float;
                                          cst = cst_ext_float; }
    void setch(char c)                  { con.ccon = c;
                                          cst = cst_char; }
    void setch(const char* s)           { con.scon = s;
                                          cst = cst_str; }
    void print(FILE * fp=stdout) const;
};


/*************************************************************************
 *** builder                                                           ***
 ***                                                                   ***
 *** Storage for each block and all the manipulation routines          ***
 *** Since all the operators(including =) are overloaded in block      ***
 *** class most of the manipulation is done here.                      ***
 *************************************************************************/
void delete_static_builder_data( void );
class builder:public builder_base {
friend class block;
friend class generator;
friend class any_oper;

    block_info info;                    // information for each block
    block * child;                      // children
    block * sib;                        // peers (maintained by the parent)
    node_type nt;
    block_type bt;

public:
    static block * B_NOOP;              // noop block

protected:
    static block_symtab * get_scope();
    static block_symtab * get_scope(tree_node *);
    static block_symtab * get_scope(tree_node_list *);
    static block_symtab * get_scope(instruction *);
    static tree_node      * get_tree_node(const block &, block_symtab *);
    static tree_node_list * get_tree_node_list(const block &, block_symtab *);
    static instruction    * get_instruction(const block &, block_symtab *);

    builder(tree_proc * p) { set_proc(p); }

    builder() { 
      atexit( delete_static_builder_data );
    }

    void init(const block * b);
    void init(const block & b);
    void init(const block &, const block &, const block &, const block &, const block &, const block &,
              const block &, const block &, const block &, const block &, const block &, const block &);
    void init();
    void init(tree_node *, boolean duplicate);
    void init(tree_node_list *, boolean duplicate);
    void init(instruction *);
    void init(const operand &);
    void init(immed);


//  <expr>
    static block * bld_op(char *, const block &);
    static block * bld_op(unary_op, const block &);
    static block * bld_op(const block &, binary_op, const block &);
    static block * bld_op(const block &, char *, const block &);
    static block * bld_assign(const block &, assign_op, const block &);
    static block * bld_expr(instruction *) { error(et_none, "No blk_expr yet"); return NULL; }
    static block * bld_call(const block &,
                            const block &, const block &, const block &, const block &,
                            const block &, const block &, const block &, const block &,
                            const block &, const block &, const block &, const block &);
    static block * add_call(block *, const block &);
    static block * bld_stmt(const block &, const block &, const block &, const block &,
                            const block &, const block &, const block &, const block &,
                            const block &, const block &, const block &, const block &);
    static block * add_stmt(block *, const block &, boolean ins=FALSE);
    static block * bld_stmt(tree_node *, boolean duplicate);
    static block * bld_stmt(tree_node_list *, boolean duplicate);
    static block * bld_stmt(instruction *);
    static block * bld_array(const block &,
                             const block & ex1, const block & ex2, const block & ex3,
                             const block & ex4, const block & ex5, const block & ex6);

    static block * bld_for(const block &, const block &, binary_op, const block &, const block &, const block &);
    static block * bld_if(const block &, const block &, const block &);
    static block * bld_while(const block &, const block &);
    static block * bld_do(const block &, const block &);
    static block * bld_return(const block &);
    static block * bld_goto(const block &);

//  <sym>
    static type_node * get_typ(const_type c);
    static block & mk_label(char *);
    static block & mk_label(label_sym *);

    static block * mk_struct_access(const block &, int f);
    static block * mk_struct_access(const block &, char * f);

    static any_oper * which_op(char *);
    static const char * get_op(any_oper & ao) { return ao.get(); }
    static const char * get_op(op_type, int op);
    static const char * get_bt(block_type);
    static int check_grammer(block_type, const block &);

    static void set_proc(tree_proc * p) { proc = p; }


    static block * build(char *,
                         const block &, const block &, const block &, const block &, const block &, const block &,
                         const block &, const block &, const block &, const block &, const block &, const block &);

    static void print(const block & b, FILE * fp = stdout, int debug=0);
    static void print(const block *, FILE * fp = stdout, int debug=0);

private:
    static tree_node      * generate_tree_node(const block & b);
    static tree_node_list * generate_tree_node_list(const block & b);
    static instruction    * generate_instruction(const block & b);
protected:
    static tree_for_test get_tft(const any_oper & op, type_node *comp_type);
};


#define ERROR(el, params) \
      _error_set(el, __FILE__, __LINE__), \
      _error params


#define LABBASE "__BLDR_"
