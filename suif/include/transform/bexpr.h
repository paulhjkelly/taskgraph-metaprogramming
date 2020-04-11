/* file "bexpr.h" */

/*  Copyright (c) 1994 Stanford University

    All rights reserved.

    This software is provided under the terms described in
    the "suif_copyright.h" include file. */

#include <suif_copyright.h>

/*  Bounds expressions */

#ifndef BEXPR_H
#define BEXPR_H

#pragma interface

#include <suif1.h>
#include <suifmath.h>
#include <dependence.h>

RCS_HEADER(bexpr_h, "$Id$")

extern const char *k_lbexpr;
extern const char *k_ubexpr;

class boundexpr;

#define MAXDEPTH 20


struct equation		/* coeff*pr + coeff*pr + ... >= rhs */
{
	int coeff[MAXDEPTH];
	access_vector *rhs;
        void print(FILE *,int cnt);
	equation();
	~equation();
};

/*
 * Bounds information necessary for loop transformation
 */

#define MAXEQUATIONS (4*MAXDEPTH)
struct bizarre_bounds_info
{
  private:
    void insbnd(int,int,equation*,access_vector*,int);
  public:
    int useless,loops,equations;
    tree_for **loop;
    access_vector lmin[MAXDEPTH];
    access_vector umax[MAXDEPTH];
    equation eq[MAXEQUATIONS];
    bizarre_bounds_info(tree_for **fors,int count);
    bizarre_bounds_info(){}
    void operator = (bizarre_bounds_info &b);
    void print(FILE *);
};

enum mn_type {mn_independent, mn_inc, mn_dec, mn_linear, mn_mess};

class bexpr {
 protected:
    void constructor_helper(instruction *,tree_node *,int);
    void constructor_helper(operand r, tree_node *tn, int d);
    bexpr(){}
 public:
    boundexpr *bx;            /* never zero */
    int divisor;              /* divide the entire bound expr by this amount */
    bexpr(instruction *,int div);
    bexpr(operand r, tree_node *parent, int div);
    bexpr(access_vector &av,int div);

    void insert_min(access_vector *,int divisor);
    void insert_max(access_vector *,int divisor);
    ~bexpr();

    bexpr *duplicate();

     /* if_ops is io_divfloor or io_divceil, to divide the divisor */
    operand generate_code(if_ops, block_symtab *scope);

    /* clean min/max lists and divide through by divisor (if possible) */
    void simplify(bizarre_bounds_info *);

    /* replace the induction variable of a with the access vector av */
    void replace(tree_for *a,access_vector *av);

    /* multiply bounds by an integer constant */
    void operator *=(int s);

    /* add access vector to bounds; done by adding to each component */
    void operator +=(access_vector &a);

    mn_type mono(tree_for *a,int crash_on_bad);

    boolean is_fctn(tree_for *a)    { return (mono(a,0) != mn_independent); }

    void print(FILE *f);
};

enum boundexprkind {bk_mn, bk_mx, bk_av};

struct boundlist {
    boundlist *link;
    struct boundexpr *item;
    boundlist(boundexpr *i,boundlist *l):link(l),item(i){}
};

struct boundexpr {
    virtual boundexprkind kind();
    virtual ~boundexpr();
        // generate_code destroys the data structure, might as well delete
        // it afterwards
    operand generate_code(block_symtab *scope, int divisor);
    virtual void replace(tree_for *a,access_vector *av);
    virtual mn_type mono(tree_for *a,int crash_on_bad);
    virtual void operator *= (int);
    virtual void operator += (access_vector &);
    virtual boundexpr *duplicate();
    virtual void print(FILE *f);
    virtual boundexpr *simplify(bizarre_bounds_info *b, int divisor);
    virtual boolean divisible_by(int div,int do_division);
    virtual operand gen(block_symtab *scope, int divisor);
    // mnok = 0: mn is meaningless, mnok = 1: exact, mnok = 2: mn only a bound
    virtual void range(int *mn,int *mnok,int *mx,int *mxok);
    virtual boolean av_only();
    virtual boolean av_min_only();
    virtual boolean av_max_only();
    boundexpr();
};

struct mn_boundexpr: public boundexpr {
    boundlist *mns;
    boundexprkind kind();
    void print(FILE *f);
    void range(int *mn,int *mnok,int *mx,int *mxok);
    mn_boundexpr(boundexpr *l,boundexpr *r);
    mn_boundexpr():mns(0){}
    ~mn_boundexpr();
    operand gen(block_symtab *scope, int divisor);
    boundexpr *simplify(bizarre_bounds_info *b, int divisor);
    void replace(tree_for *a,access_vector *av);
    mn_type mono(tree_for *a,int crash_on_bad);
    void operator *= (int);
    void operator += (access_vector&);
    boundexpr *duplicate();
    boolean divisible_by(int div,int do_division);
    boolean av_only();
    boolean av_min_only();
    boolean av_max_only();
};

struct mx_boundexpr: public boundexpr {
    boundlist *mxs;
    boundexprkind kind();
    //int mono(tree_for *a);	// index of a is monoinc, monodec
    void range(int *mn,int *mnok,int *mx,int *mxok);
    void print(FILE *f);
    mx_boundexpr(boundexpr *l,boundexpr *r);
    mx_boundexpr():mxs(0){}
    ~mx_boundexpr();
    operand gen(block_symtab *scope, int divisor);
    boundexpr *simplify(bizarre_bounds_info *b, int divisor);
    void replace(tree_for *a,access_vector *av);
    mn_type mono(tree_for *a,int crash_on_bad);
    void operator *= (int);
    void operator += (access_vector &);
    boundexpr *duplicate();
    boolean divisible_by(int div,int do_division);
    boolean av_only();
    boolean av_min_only();
    boolean av_max_only();
};

struct av_boundexpr: public boundexpr {
    access_vector *av;

    boundexprkind kind();
        //int mono(tree_for *a);	// index of a is monoinc, monodec
    void print(FILE *f);
    av_boundexpr(access_vector *a);
    void operator =(av_boundexpr &);
    void range(int *mn,int *mnok,int *mx,int *mxok);
    operand gen(block_symtab *scope, int divisor);
    ~av_boundexpr();
    boundexpr *simplify(bizarre_bounds_info *b, int divisor);
    void replace(tree_for *a,access_vector *av);
    mn_type mono(tree_for *a,int crash_on_bad);
    void operator *= (int);
    void operator += (access_vector&);
    boundexpr *duplicate();
    boolean divisible_by(int div,int do_division);
    boolean av_only();
    boolean av_min_only();
    boolean av_max_only();

};

#endif

