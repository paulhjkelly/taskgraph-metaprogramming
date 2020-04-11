/* file "builder_def.h" */

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

/**************************************************************************
 *** block                                                              ***
 ***                                                                    ***
 *** The Building Block of the Builder                                  ***
 ***                                                                    ***
 *** WARNNING:                                                          ***
 ***    block a, b;                                                     ***
 ***    a = b;                                                          ***
 *** This code does not assign b to a, it cretes a new block that       ***
 *** represents an assignment operation in SUIF.                        ***
 *** To do an assignment of blocks do a.set(b).                         ***
 ***                                                                    ***
 *** Do not assign to, make arrays of, create pointers to blocks.       ***
 ***                                                                    ***
 *** Also, need to perform a set_proc(..) before using the builder.     ***
 **************************************************************************/
class block:protected builder {
friend class builder;
friend class generator;

public:
    static void set_proc(tree_proc * p) { proc = p; }
    static tree_proc * get_proc()       { return proc; }

    block()                             { init(); }
    block(const block * b)              { init(b); }
    block(const block & b)              { init(b); }
    // Assignment
    void set(const block * b)           { init(block(b)); }
    void set(const block & b)           { init(block(b)); }
    // Composition
    block(const block & ex1, const block & ex2,
          const block & ex3 = *B_NOOP, const block & ex4 = *B_NOOP,
          const block & ex5 = *B_NOOP, const block & ex6 = *B_NOOP,
          const block & ex7 = *B_NOOP, const block & ex8 = *B_NOOP,
          const block & ex9 = *B_NOOP, const block & ex10= *B_NOOP,
          const block & ex11= *B_NOOP, const block & ex12= *B_NOOP);

    ~block();


 //  <expr>
     block(instruction * in)             { init(in); }
     static block & op(char * o,
                       const block & A)  { block * bk = bld_op(o, A);
                                           return *bk; }
     static block & op(unary_op o,
                       const block & A)  { block * bk = bld_op(o, A);
                                           return *bk; }
     static block & op(const block & A,
                       binary_op o,
                       const block & B)  { block * bk = bld_op(A, o, B);
                                           return *bk; }
     static block & op(const block & A,
                       char * o,
                       const block & B)  { block * bk = bld_op(A, o, B);
                                           return *bk; }
     static block & assign(const block & A,
                           assign_op o,
                           const block & B) { block * bk = bld_assign(A, o, B);
                                           return *bk; }
     static block & expr(instruction * asn) { block * bk = bld_expr(asn);
                                              return *bk; }
     static block & CALL(const block & s,
                         const block & ex1 = *B_NOOP, const block & ex2 = *B_NOOP,
                         const block & ex3 = *B_NOOP, const block & ex4 = *B_NOOP,
                         const block & ex5 = *B_NOOP, const block & ex6 = *B_NOOP,
                         const block & ex7 = *B_NOOP, const block & ex8 = *B_NOOP,
                         const block & ex9 = *B_NOOP, const block & ex10= *B_NOOP,
                         const block & ex11= *B_NOOP, const block & ex12= *B_NOOP);


     block & douniop(unary_op uop) const      { return op(uop, *this); }

     block & dobinop(binary_op bop,
                     const block & A) const    { return op(*this, bop, A); }
     block & dobinop(binary_op bop,
                     int i) const             { block * B = new block(i);
                                           return  op(*this, bop, *B); }
     block & dobinop(binary_op bop,
                     double d) const          { block * B = new block(d);
                                           return  op(*this, bop, *B); }
     block & dobinop(binary_op bop,
                     float d) const            { block * B = new block(d);
                                           return  op(*this, bop, *B); }
     block & dobinop(binary_op bop,
                     char * s) const           { block * B = new block(s);
                                           return  op(*this, bop, *B); }

     block & doasignop(assign_op aop,
                       const block & A) const  { return assign(*this, aop, A); }
     block & doasignop(assign_op aop,
                       int i) const           { block * B = new block(i);
                                           return assign(*this, aop, *B); }
     block & doasignop(assign_op aop,
                       double d) const         { block * B = new block(d);
                                           return assign(*this, aop, *B); }
     block & doasignop(assign_op aop,
                       float d) const         { block * B = new block(d);
                                           return assign(*this, aop, *B); }
     block & doasignop(assign_op aop,
                       char * s) const        { block * B = new block(s);
                                           return assign(*this, aop, *B); }


     block & operator-() const                { return douniop(uop_minus); }
     block & operator!() const                { return douniop(uop_lnot); }
     block & operator~() const                { return douniop(uop_not); }

     block & operator*(const block & b) const  { return dobinop(bop_mul, b); }
     block & operator/(const block & b) const  { return dobinop(bop_div, b); }
     block & operator%(const block & b) const { return dobinop(bop_mod, b); }
     block & operator+(const block & b) const { return dobinop(bop_add, b); }
     block & operator-(const block & b) const { return dobinop(bop_sub, b); }
     block & operator<<(const block & b) const { return dobinop(bop_lshift, b); }
     block & operator>>(const block & b) const { return dobinop(bop_rshift, b); }
     block & operator<(const block & b) const  { return dobinop(bop_less, b); }
     block & operator>(const block & b) const  { return dobinop(bop_gt, b); }
     block & operator<=(const block & b) const { return dobinop(bop_leq, b); }
     block & operator>=(const block & b) const { return dobinop(bop_geq, b); }
     block & operator==(const block & b) const { return dobinop(bop_eq, b); }
     block & operator!=(const block & b) const { return dobinop(bop_neq, b); }
     block & operator&(const block & b) const  { return dobinop(bop_and, b); }
     block & operator^(const block & b) const  { return dobinop(bop_xor, b); }
     block & operator|(const block & b) const  { return dobinop(bop_or, b); }
     block & operator&&(const block & b) const { return dobinop(bop_land, b); }
     block & operator||(const block & b) const { return dobinop(bop_lor, b); }

     block & operator=(const block & b) const    { return doasignop(aop_eq, b); }
     block & operator+=(const block & b) const   { return doasignop(aop_add, b); }
     block & operator-=(const block & b) const   { return doasignop(aop_sub, b); }
     block & operator*=(const block & b) const   { return doasignop(aop_mul, b); }
     block & operator/=(const block & b) const   { return doasignop(aop_div, b); }
     block & operator%=(const block & b) const   { return doasignop(aop_mod, b); }
     block & operator>>=(const block & b) const  { return doasignop(aop_rshift, b); }
     block & operator<<=(const block & b) const  { return doasignop(aop_lshift, b); }
     block & operator&=(const block & b) const   { return doasignop(aop_and, b); }
     block & operator^=(const block & b) const   { return doasignop(aop_xor, b); }
     block & operator|=(const block & b) const   { return doasignop(aop_or, b); }


     block & operator*(int b) const            { return dobinop(bop_mul, b); }
     block & operator/(int b) const            { return dobinop(bop_div, b); }
     block & operator%(int b) const            { return dobinop(bop_mod, b); }
     block & operator+(int b) const            { return dobinop(bop_add, b); }
     block & operator-(int b) const            { return dobinop(bop_sub, b); }
     block & operator<<(int b) const           { return dobinop(bop_lshift, b); }
     block & operator>>(int b) const           { return dobinop(bop_rshift, b); }
     block & operator<(int b) const            { return dobinop(bop_less, b); }
     block & operator>(int b) const            { return dobinop(bop_gt, b); }
     block & operator<=(int b) const           { return dobinop(bop_leq, b); }
     block & operator>=(int b) const           { return dobinop(bop_geq, b); }
     block & operator==(int b) const           { return dobinop(bop_eq, b); }
     block & operator!=(int b) const           { return dobinop(bop_neq, b); }
     block & operator&(int b) const            { return dobinop(bop_and, b); }
     block & operator^(int b) const            { return dobinop(bop_xor, b); }
     block & operator|(int b) const            { return dobinop(bop_or, b); }
     block & operator&&(int b) const           { return dobinop(bop_land, b); }
     block & operator||(int b) const           { return dobinop(bop_lor, b); }

     block & operator=(int b) const            { return doasignop(aop_eq, b); }
     block & operator+=(int b) const           { return doasignop(aop_add, b); }
     block & operator-=(int b) const           { return doasignop(aop_sub, b); }
     block & operator*=(int b) const           { return doasignop(aop_mul, b); }
     block & operator/=(int b) const           { return doasignop(aop_div, b); }
     block & operator%=(int b) const           { return doasignop(aop_mod, b); }
     block & operator>>=(int b) const          { return doasignop(aop_rshift, b); }
     block & operator<<=(int b) const          { return doasignop(aop_lshift, b); }
     block & operator&=(int b) const           { return doasignop(aop_and, b); }
     block & operator^=(int b) const           { return doasignop(aop_xor, b); }
     block & operator|=(int b) const           { return doasignop(aop_or, b); }

     block & operator*(double b) const         { return dobinop(bop_mul, b); }
     block & operator/(double b) const         { return dobinop(bop_div, b); }
     block & operator%(double b) const         { return dobinop(bop_mod, b); }
     block & operator+(double b) const         { return dobinop(bop_add, b); }
     block & operator-(double b) const         { return dobinop(bop_sub, b); }
     block & operator<<(double b) const        { return dobinop(bop_lshift, b); }
     block & operator>>(double b) const        { return dobinop(bop_rshift, b); }
     block & operator<(double b) const         { return dobinop(bop_less, b); }
     block & operator>(double b) const         { return dobinop(bop_gt, b); }
     block & operator<=(double b) const        { return dobinop(bop_leq, b); }
     block & operator>=(double b) const        { return dobinop(bop_geq, b); }
     block & operator==(double b) const        { return dobinop(bop_eq, b); }
     block & operator!=(double b) const        { return dobinop(bop_neq, b); }
     block & operator&(double b) const         { return dobinop(bop_and, b); }
     block & operator^(double b) const         { return dobinop(bop_xor, b); }
     block & operator|(double b) const         { return dobinop(bop_or, b); }
     block & operator&&(double b) const        { return dobinop(bop_land, b); }
     block & operator||(double b) const        { return dobinop(bop_lor, b); }

     block & operator=(double b) const         { return doasignop(aop_eq, b); }
     block & operator+=(double b) const        { return doasignop(aop_add, b); }
     block & operator-=(double b) const        { return doasignop(aop_sub, b); }
     block & operator*=(double b) const        { return doasignop(aop_mul, b); }
     block & operator/=(double b) const        { return doasignop(aop_div, b); }
     block & operator%=(double b) const        { return doasignop(aop_mod, b); }
     block & operator>>=(double b) const       { return doasignop(aop_rshift, b); }
     block & operator<<=(double b) const       { return doasignop(aop_lshift, b); }
     block & operator&=(double b) const        { return doasignop(aop_and, b); }
     block & operator^=(double b) const        { return doasignop(aop_xor, b); }
     block & operator|=(double b) const        { return doasignop(aop_or, b); }

     block & operator*(float b) const          { return dobinop(bop_mul, b); }
     block & operator/(float b) const          { return dobinop(bop_div, b); }
     block & operator%(float b) const          { return dobinop(bop_mod, b); }
     block & operator+(float b) const          { return dobinop(bop_add, b); }
     block & operator-(float b) const          { return dobinop(bop_sub, b); }
     block & operator<<(float b) const         { return dobinop(bop_lshift, b); }
     block & operator>>(float b) const         { return dobinop(bop_rshift, b); }
     block & operator<(float b) const          { return dobinop(bop_less, b); }
     block & operator>(float b) const          { return dobinop(bop_gt, b); }
     block & operator<=(float b) const         { return dobinop(bop_leq, b); }
     block & operator>=(float b) const         { return dobinop(bop_geq, b); }
     block & operator==(float b) const         { return dobinop(bop_eq, b); }
     block & operator!=(float b) const         { return dobinop(bop_neq, b); }
     block & operator&(float b) const          { return dobinop(bop_and, b); }
     block & operator^(float b) const          { return dobinop(bop_xor, b); }
     block & operator|(float b) const          { return dobinop(bop_or, b); }
     block & operator&&(float b) const         { return dobinop(bop_land, b); }
     block & operator||(float b) const         { return dobinop(bop_lor, b); }

     block & operator=(float b) const          { return doasignop(aop_eq, b); }
     block & operator+=(float b) const         { return doasignop(aop_add, b); }
     block & operator-=(float b) const         { return doasignop(aop_sub, b); }
     block & operator*=(float b) const         { return doasignop(aop_mul, b); }
     block & operator/=(float b) const         { return doasignop(aop_div, b); }
     block & operator%=(float b) const         { return doasignop(aop_mod, b); }
     block & operator>>=(float b) const        { return doasignop(aop_rshift, b); }
     block & operator<<=(float b) const        { return doasignop(aop_lshift, b); }
     block & operator&=(float b) const         { return doasignop(aop_and, b); }
     block & operator^=(float b) const         { return doasignop(aop_xor, b); }
     block & operator|=(float b) const         { return doasignop(aop_or, b); }

     block & operator*(char * b) const         { return dobinop(bop_mul, b); }
     block & operator/(char * b) const         { return dobinop(bop_div, b); }
     block & operator%(char * b) const         { return dobinop(bop_mod, b); }
     block & operator+(char * b) const         { return dobinop(bop_add, b); }
     block & operator-(char * b) const         { return dobinop(bop_sub, b); }
     block & operator<<(char * b) const        { return dobinop(bop_lshift, b); }
     block & operator>>(char * b) const        { return dobinop(bop_rshift, b); }
     block & operator<(char * b) const         { return dobinop(bop_less, b); }
     block & operator>(char * b) const         { return dobinop(bop_gt, b); }
     block & operator<=(char * b) const        { return dobinop(bop_leq, b); }
     block & operator>=(char * b) const        { return dobinop(bop_geq, b); }
     block & operator==(char * b) const        { return dobinop(bop_eq, b); }
     block & operator!=(char * b) const        { return dobinop(bop_neq, b); }
     block & operator&(char * b) const         { return dobinop(bop_and, b); }
     block & operator^(char * b) const         { return dobinop(bop_xor, b); }
     block & operator|(char * b) const         { return dobinop(bop_or, b); }
     block & operator&&(char * b) const        { return dobinop(bop_land, b); }
     block & operator||(char * b) const        { return dobinop(bop_lor, b); }

     block & operator=(char * b) const         { return doasignop(aop_eq, b); }
     block & operator+=(char * b) const        { return doasignop(aop_add, b); }
     block & operator-=(char * b) const        { return doasignop(aop_sub, b); }
     block & operator*=(char * b) const        { return doasignop(aop_mul, b); }
     block & operator/=(char * b) const        { return doasignop(aop_div, b); }
     block & operator%=(char * b) const        { return doasignop(aop_mod, b); }
     block & operator>>=(char * b) const       { return doasignop(aop_rshift, b); }
     block & operator<<=(char * b) const       { return doasignop(aop_lshift, b); }
     block & operator&=(char * b) const        { return doasignop(aop_and, b); }
     block & operator^=(char * b) const        { return doasignop(aop_xor, b); }
     block & operator|=(char * b) const        { return doasignop(aop_or, b); }


     static block & max(const block &a,
                        const block &b)       { return a.dobinop(bop_max, b); }
     static block & min(const block &a,
                        const block &b)       { return a.dobinop(bop_min, b); }
     static block & mod(const block &a,
                        const block &b)       { return a.dobinop(bop_mod, b); }
     static block & abs(const block &a)       { return a.douniop(uop_abs); }


 //  <prim>
     block(char *);
     static block val(char * c)          { return new block(c); }
     block(int);
     static block val(int i)             { return new block(i); }
     block(double);
     static block val(double d)          { return new block(d); }
     block(float);
     static block val(float d)          { return new block(d); }

 // int, double, float, or symbolic address
     block(immed im) { init(im); }

 //  <lval>
     static block & ARRAY(const block &,
                          const block & ex1 = *B_NOOP, const block & ex2 = *B_NOOP,
                          const block & ex3 = *B_NOOP, const block & ex4 = *B_NOOP,
                          const block & ex5 = *B_NOOP, const block & ex6 = *B_NOOP);

    block & operator[](const block & acc)       { return ARRAY(*this, acc); }
    block & operator[](block *acc)        { return ARRAY(*this, *acc); }
    block & operator[](int i)             { block acc(i);
                                            return ARRAY(*this, acc);  }

 //  <sym>
     static type_node * parse_type(char *,
                                   type_node * tn0=NULL, type_node * tn1=NULL,
                                   type_node * tn2=NULL, type_node * tn3=NULL,
                                   type_node * tn4=NULL, type_node * tn5=NULL,
                                   type_node * tn6=NULL, type_node * tn7=NULL,
                                   type_node * tn8=NULL, type_node * tn9=NULL);

     static type_node * parse_type(base_symtab * st,
                                   char *,
                                   type_node * tn0=NULL, type_node * tn1=NULL,
                                   type_node * tn2=NULL, type_node * tn3=NULL,
                                   type_node * tn4=NULL, type_node * tn5=NULL,
                                   type_node * tn6=NULL, type_node * tn7=NULL,
                                   type_node * tn8=NULL, type_node * tn9=NULL);

     block(sym_node *);
     block(struct_access *);
     static block sym(sym_node * s)      { return new block(s); }
     static block sym(type_node * tp);

     static block & mk_nr(type_node * t,
                          char * nm=0,
                          base_symtab * st = NULL);

    static block & new_sym(type_node * tp)    { return mk_nr(tp); }
    static block & new_sym(type_node * tp,
                           base_symtab * bs)  { return mk_nr(tp, NULL, bs); }
     static block & new_sym(const_type tp)    { return mk_nr(get_typ(tp)); }
     static block & new_sym(const_type tp,
                            char * nm)        { return mk_nr(get_typ(tp), nm);  }
     static block & new_sym(const_type tp,
                            char * nm,
                            base_symtab * bs) { return mk_nr(get_typ(tp), nm, bs);  }
     static block & new_sym(type_node * tp,
                            char * nm)   { return mk_nr(tp, nm); }
     static block & new_sym(type_node * tp,
                            char * nm,
                            base_symtab * bs)   { return mk_nr(tp, nm, bs); }
     static block & new_sym(char * parse_tp,
                            char * nm)   { return mk_nr(parse_type(parse_tp), nm); }
    static block & new_sym(char * parse_tp,
                            char * nm,
                            base_symtab * bs)   { return mk_nr(parse_type(parse_tp), nm, bs); }
     static block & new_label(char *nm)  { return mk_label(nm); }
     static block & new_label(label_sym *ls) { return mk_label(ls); }

    sym_node * get_sym();

     block & field(char *);
     block & field(int i);
     block & addr();
     block & dref();

     block(const operand * op)                 { init(*op); }
     block(const operand & op)                 { init(op); }

 //  <statement>
     static block & statement(const block & ex1 = *B_NOOP, const block & ex2 = *B_NOOP,
                              const block & ex3 = *B_NOOP, const block & ex4 = *B_NOOP,
                              const block & ex5 = *B_NOOP, const block & ex6 = *B_NOOP,
                              const block & ex7 = *B_NOOP, const block & ex8 = *B_NOOP,
                              const block & ex9 = *B_NOOP, const block & ex10= *B_NOOP,
                              const block & ex11= *B_NOOP, const block & ex12= *B_NOOP);
     block & statement_append(const block &);
     block & statement_insert(const block &);
     block(tree_node * tn,
           boolean duplicate=TRUE)       { init(tn, duplicate); }
     block(tree_node_list *tnl,
           boolean duplicate=TRUE)       { init(tnl, duplicate); }
     static block & statement(tree_node *,
                              boolean duplicate=TRUE);
     static block & statement(tree_node_list *,
                             boolean duplicate=TRUE);
    static block & statement(instruction *);
    static block & FOR(const block &index,
                       const block &lb,
                       const block &ub,
                       const block &body);
    static block & FOR(const block &index,
                       const block &lb,
                       binary_op test,
                       const block &ub,
                       const block &body);
    static block & FOR(const block &index,
                       const block &lb,
                       const block &ub,
                       const block &step,
                       const block &body);
    static block & FOR(const block &index,
                       const block &lb,
                       binary_op test,
                       const block &ub,
                       const block &step,
                       const block &body);
    static block & IF(const block &cond,
                      const block &then_part);
    static block & IF(const block &cond,
                      const block &then_part,
                      const block &else_part);
    static block & WHILE(const block &cond,
                         const block &body);
    static block & DO(const block &cond,
                      const block &body);
    static block & RETURN();
    static block & RETURN(const block &val);
    static block & GOTO(const block &label);

//  <param>
    block & param(int type);

//  <paramlist>
    static block & param(const block & ex1 = *B_NOOP, const block & ex2 = *B_NOOP,
                         const block & ex3 = *B_NOOP, const block & ex4 = *B_NOOP,
                         const block & ex5 = *B_NOOP, const block & ex6 = *B_NOOP,
                         const block & ex7 = *B_NOOP, const block & ex8 = *B_NOOP,
                         const block & ex9 = *B_NOOP, const block & ex10= *B_NOOP,
                         const block & ex11= *B_NOOP, const block & ex12= *B_NOOP);

//  <proc>
    static block & procedure(const block &, const block &, const block &);

// Create Code
    tree_node      * make_tree_node();
    tree_node      * make_tree_node(block_symtab *);
    tree_node      * make_tree_node(tree_node *);
    tree_node      * make_tree_node(tree_node_list *);
    tree_node      * make_tree_node(instruction *);
    tree_node_list * make_tree_node_list();
    tree_node_list * make_tree_node_list(block_symtab *);
    tree_node_list * make_tree_node_list(tree_node *);
    tree_node_list * make_tree_node_list(tree_node_list *);
    tree_node_list * make_tree_node_list(instruction *);
    instruction    * make_instruction();
    instruction    * make_instruction(block_symtab *);
    instruction    * make_instruction(tree_node *);
    instruction    * make_instruction(tree_node_list *);
    instruction    * make_instruction(instruction *);


// Misc.
    void print(FILE * fp = stdout)         { builder::print(this, fp, 0); }
    void debug_print(FILE * fp = stdout)   { builder::print(this, fp, 1); }
    void check();
};



typedef block * pblock;
