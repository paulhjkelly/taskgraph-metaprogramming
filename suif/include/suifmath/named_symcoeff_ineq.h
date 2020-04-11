/* file "named_symcoeff_ineq.h" */

/*  Copyright (c) 1994 Stanford University

    All rights reserved.

    This software is provided under the terms described in
    the "suif_copyright.h" include file. */

#include <suif_copyright.h>

#pragma interface

#ifndef NAMED_SYMCOEFF_INEQ_H
#define NAMED_SYMCOEFF_INEQ_H

//#define DO_BOUND_CHK

/**************************************************************************
 ***                                                                    ***
 *** System of linear inequalities with symbolic constants are          ***
 *** coefficients.  All variables and coefficients have names           ***
 *** associated with them.                                              ***
 ***                                                                    ***
 *** Data structure is represented by a dense 3D object.                ***
 *** (p)  First dimension is for symbolic coefficients, the first       ***
 ***      element of this dimension is for integer coefficients.(as in  ***
 ***      normal linear inequalities) There are names associated with   ***
 ***      the symbolic constants this dimension.                        ***
 *** (m)  The second dimension is for different inequalities. Multiple  ***
 ***      inequalities makes-up a system of inequalities.               ***
 *** (n)  The third dimension is for variables.  The first element of   ***
 ***      this dimension is for the constant value. (integer or         ***
 ***      symbolic) There are names associated with the variables of    ***
 ***      this dimsnsion.                                               ***
 ***                                                                    ***
 **************************************************************************/
class named_symcoeff_ineq {
friend void align(named_symcoeff_ineq & A, named_symcoeff_ineq & B);

    int unum;
    static int unum_cnt;
    name_table nt_c;
    name_table nt_p;
    lin_ineq   * L;
public:
    named_symcoeff_ineq();
    named_symcoeff_ineq(const named_symcoeff_ineq & c);
    named_symcoeff_ineq(const named_symcoeff_ineq * c);
    named_symcoeff_ineq(name_table &p, name_table &c,
                       lin_ineq ** ll);
    named_symcoeff_ineq(name_table &p, name_table &c,
                       lin_ineq * l0,
                       lin_ineq * l1=NULL, lin_ineq * l2=NULL,
                       lin_ineq * l3=NULL, lin_ineq * l4=NULL,
                       lin_ineq * l5=NULL, lin_ineq * l6=NULL,
                       lin_ineq * l7=NULL, lin_ineq * l8=NULL);
    named_symcoeff_ineq(name_table &n,
                       lin_ineq * l0,
                       immed * v1 =NULL, lin_ineq * l1 =NULL,
                       immed * v2 =NULL, lin_ineq * l2 =NULL,
                       immed * v3 =NULL, lin_ineq * l3 =NULL,
                       immed * v4 =NULL, lin_ineq * l4 =NULL,
                       immed * v5 =NULL, lin_ineq * l5 =NULL,
                       immed * v6 =NULL, lin_ineq * l6 =NULL,
                       immed * v7 =NULL, lin_ineq * l7 =NULL,
                       immed * v8 =NULL, lin_ineq * l8 =NULL);

    named_symcoeff_ineq(name_table & p,
                       named_lin_ineq ** ll);
    named_symcoeff_ineq(name_table & p,
                       named_lin_ineq * l0,
                       named_lin_ineq * l1=NULL, named_lin_ineq * l2=NULL,
                       named_lin_ineq * l3=NULL, named_lin_ineq * l4=NULL,
                       named_lin_ineq * l5=NULL, named_lin_ineq * l6=NULL,
                       named_lin_ineq * l7=NULL, named_lin_ineq * l8=NULL);
    named_symcoeff_ineq(named_lin_ineq * l0,
                       immed * v1 =NULL, named_lin_ineq * l1 =NULL,
                       immed * v2 =NULL, named_lin_ineq * l2 =NULL,
                       immed * v3 =NULL, named_lin_ineq * l3 =NULL,
                       immed * v4 =NULL, named_lin_ineq * l4 =NULL,
                       immed * v5 =NULL, named_lin_ineq * l5 =NULL,
                       immed * v6 =NULL, named_lin_ineq * l6 =NULL,
                       immed * v7 =NULL, named_lin_ineq * l7 =NULL,
                       immed * v8 =NULL, named_lin_ineq * l8 =NULL);

    named_symcoeff_ineq(const named_lin_ineq & cc);
    named_symcoeff_ineq(immed_list & il);
    ~named_symcoeff_ineq();
private:
    void initL(int r);
    void initL(lin_ineq **);
    void initL(named_lin_ineq **);
    void initL(lin_ineq * l0, lin_ineq * l1, lin_ineq * l2,
               lin_ineq * l3, lin_ineq * l4, lin_ineq * l5,
               lin_ineq * l6, lin_ineq * l7, lin_ineq * l8);
    void initL(const named_lin_ineq* l0,
	       named_lin_ineq* l1, named_lin_ineq* l2,
               named_lin_ineq* l3, named_lin_ineq* l4, named_lin_ineq* l5,
               named_lin_ineq* l6, named_lin_ineq* l7, named_lin_ineq* l8);
    void check();
public:
    void init();
    void init(int p, int r, int c);
    void init(const named_symcoeff_ineq & c);
    void init(const named_symcoeff_ineq * c)             { init(*c); }
    void init(name_table &p, name_table &c,
              lin_ineq ** ll);
    void init(name_table &p, name_table &c,
              lin_ineq * l0,
              lin_ineq * l1=NULL, lin_ineq * l2=NULL,
              lin_ineq * l3=NULL, lin_ineq * l4=NULL,
              lin_ineq * l5=NULL, lin_ineq * l6=NULL,
              lin_ineq * l7=NULL, lin_ineq * l8=NULL);
    void init(name_table &c,
              lin_ineq * l0,
              immed * v1 =NULL, lin_ineq * l1 =NULL,
              immed * v2 =NULL, lin_ineq * l2 =NULL,
              immed * v3 =NULL, lin_ineq * l3 =NULL,
              immed * v4 =NULL, lin_ineq * l4 =NULL,
              immed * v5 =NULL, lin_ineq * l5 =NULL,
              immed * v6 =NULL, lin_ineq * l6 =NULL,
              immed * v7 =NULL, lin_ineq * l7 =NULL,
              immed * v8 =NULL, lin_ineq * l8 =NULL);
    void init(name_table & p,
              named_lin_ineq ** ll);
    void init(name_table & p,
              named_lin_ineq * l0,
              named_lin_ineq * l1=NULL, named_lin_ineq * l2=NULL,
              named_lin_ineq * l3=NULL, named_lin_ineq * l4=NULL,
              named_lin_ineq * l5=NULL, named_lin_ineq * l6=NULL,
              named_lin_ineq * l7=NULL, named_lin_ineq * l8=NULL);
    void init(const named_lin_ineq * l0,
              immed * v1 =NULL, named_lin_ineq * l1 =NULL,
              immed * v2 =NULL, named_lin_ineq * l2 =NULL,
              immed * v3 =NULL, named_lin_ineq * l3 =NULL,
              immed * v4 =NULL, named_lin_ineq * l4 =NULL,
              immed * v5 =NULL, named_lin_ineq * l5 =NULL,
              immed * v6 =NULL, named_lin_ineq * l6 =NULL,
              immed * v7 =NULL, named_lin_ineq * l7 =NULL,
              immed * v8 =NULL, named_lin_ineq * l8 =NULL);
    void init(const named_lin_ineq & cc);
    int init(immed_list & il, int c=0);
    immed_list * cvt_immed_list();

    int uid() const { return unum; }

    name_table & planes()                       { return nt_p; }
    name_table & cols()                         { return nt_c; }

    const name_table & const_planes() const     { return nt_p; }
    const name_table & const_cols() const       { return nt_c; }

    int p() const                               { return nt_p.n(); }
    int m() const                               { return L[0].m(); }
    int n() const                               { return nt_c.n(); }

    lin_ineq * get_p(int) const;
    lin_ineq * get_m(int) const;
    lin_ineq * get_n(int) const;
    void set_p(int, lin_ineq *);
    void set_m(int, lin_ineq *);
    void set_n(int, lin_ineq *);

    constraint * get_pm(int, int) const;
    constraint * get_pn(int, int) const;
    constraint * get_mn(int, int) const;
    void set_pm(int, int, constraint *);
    void set_pn(int, int, constraint *);
    void set_mn(int, int, constraint *);

    named_symcoeff_ineq ineq(int i);

    // to get/set an element  (*this)[p][m][n]
    lin_ineq & operator[](int i)                {
#ifdef DO_BOUND_CHK
                                                  assert((i>=0)&&(i<p()));
#endif
                                                  return L[i]; }
    const lin_ineq & r(int i) const             {
#ifdef DO_BOUND_CHK
                                                  assert((i>=0)&&(i<p()));
#endif
                                                  return L[i]; }

    // operations on columns
    void swap_col(int i, int j);
    void add_col(const name_table_entry & nte, int i);
    void add_col(immed sym, int i);
    void del_col(int i, int j);
    void del_col(int i)                         { del_col(i, i); }
    void del_col(integer_row &);
    int find_col(immed v) const                { return const_cols().find(v); }

    // operations on planes
    void add_pln(name_table_entry & nte, int i);
    void add_pln(immed sym, int i);
    void del_pln(int i, int j);
    void del_pln(int i)                         { del_pln(i, i); }
    void del_pln(integer_row &);
    int find_pln(immed v)                   { return planes().find(v); }

    // operations on inequalities
    void add_ineq(int i);
    void del_ineq(int i, int j);
    void del_ineq(int i)                        { del_ineq(i, i); }
    void del_ineq(integer_row &);
    void set_n_ineq(int i)                      { initL(i); }

    named_lin_ineq * nli() const;

    // project away a variable or a list of variables (columns)
    void project_away(immed);
    void project_away(name_table &);

    // substitution expression given in expr is of the form
    //var >= sub_expr and var <= sub_expr or
    // sub_expr >= 0
    named_symcoeff_ineq * substitute(immed var, named_symcoeff_ineq & expr);


    named_symcoeff_ineq & operator=(const named_symcoeff_ineq & c);

    // operations to concatenate 2 systems of inequalities
    named_symcoeff_ineq operator&(const named_symcoeff_ineq & c);
    named_symcoeff_ineq & operator&=(const named_symcoeff_ineq & c);
    static named_symcoeff_ineq * opand(named_symcoeff_ineq * c1,
                                    named_symcoeff_ineq * c2,
                                    boolean del1=FALSE,
                                    boolean del2=FALSE);
    void operator||(named_symcoeff_ineq & c);    // align two systems
    void nt_align(name_table * col=NULL, name_table * pln=NULL);

    boolean is_same(named_symcoeff_ineq & c);
    // check is_contrain and equality using fm-elimination
    boolean operator==(named_symcoeff_ineq &);
    boolean operator!=(named_symcoeff_ineq & c)    { return !(*this == c); }
    boolean operator>>(named_symcoeff_ineq &);  // Is contained in opeartor
    boolean operator<<(named_symcoeff_ineq & c)    { return (c >> (*this)); }
    boolean operator~();

    named_symcoeff_ineq inverse_all();

    void find_bounds();

    // filter operations, can use both planes and columns as filters
    named_symcoeff_ineq filter_thru(constraint * column_kernel,
                                   constraint * plane_kernel,
                                   int sign) const;
    named_symcoeff_ineq filter_away(constraint * column_kernel,
                                   constraint * plane_kernel,
                                   int sign) const;
private:
    named_symcoeff_ineq * filter(int * filt) const;
    named_symcoeff_ineq filter(constraint * column_kernel,
                              constraint * plane_kernel,
                              int sign,
                              int tora) const;

public:
    void cleanup();
    void del_zeros();

    void print(FILE *fp=stdout);
    void print_exp(int i, FILE *fp=stdout);
    void print_exp(FILE *fp)  { print_exp(pet_single, fp); }
    void print_exp(print_exp_type tp=pet_single, FILE *fp = stdout);
    void print_exp(print_exp_type tp, int lhs, FILE *fp = stdout);
    void print_code(FILE *fp = stdout);
    void print_code(boolean c_format, FILE *fp = stdout);
    void print_code(boolean c_format, int st, FILE *fp = stdout);
    void print_code(boolean c_format, int st, int en, FILE *fp = stdout);


    static named_symcoeff_ineq * convert_exp(instruction *);
    static named_symcoeff_ineq * convert_exp(operand);

    static named_symcoeff_ineq * mk_sc(operand,
	                               immed ind,
                                       boolean lb);

    instruction * mk_bounds();
    instruction * create_expression(base_symtab * base = NULL) const;
    instruction * create_expression(immed v,
                         boolean is_ub, base_symtab * base = NULL) const;

    static void align_named_symcoeff_ineqs(named_symcoeff_ineq & A,
                                           named_symcoeff_ineq & B);
    static void change_name_types(named_symcoeff_ineq & A,
                                  named_symcoeff_ineq & B); // use A and change B
    static boolean is_aligned(named_symcoeff_ineq & A,
                               named_symcoeff_ineq & B);
    static named_symcoeff_ineq * merge_named_symcoeff_ineqs(named_symcoeff_ineq& A,
                                                            named_symcoeff_ineq& B);
    void move_col2plane(name_table_entry & nte, boolean * too_messy = NULL);
};


typedef named_symcoeff_ineq * p_named_symcoeff_ineq;


DECLARE_LIST_CLASS(named_symcoeff_ineq_list, named_symcoeff_ineq *);

class dvlist;
class tn_list;
dvlist * find_extra(in_array * ia1,
                    in_array * ia2,
                    int minnest,
                    tn_list * tnl,
                    boolean lexpos);

// NEED TO LINK LIBDEPENDENCE TO USE THIS FUNCTION
named_symcoeff_ineq * include_sc_for(tree_for * tf);
named_symcoeff_ineq * include_sc_if_true(tree_if * ti);
named_symcoeff_ineq * include_sc_if_false(tree_if * ti);


// assume a named_symcoeff ineq is a list of expressions.
// these functions will add/substract each matching expressions of two lists
// number of expressions in both lists (m()) has to be the same
named_symcoeff_ineq add(const named_symcoeff_ineq &,
			const named_symcoeff_ineq &);
named_symcoeff_ineq sub(const named_symcoeff_ineq &,
			const named_symcoeff_ineq &);
named_symcoeff_ineq * mul(const named_symcoeff_ineq &,
			  const named_symcoeff_ineq &);
named_symcoeff_ineq mul(const named_symcoeff_ineq &, int);
named_symcoeff_ineq mul(int i, const named_symcoeff_ineq & a);
named_symcoeff_ineq minus(const named_symcoeff_ineq &);

#endif



















