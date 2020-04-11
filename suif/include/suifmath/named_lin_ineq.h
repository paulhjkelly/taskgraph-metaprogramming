/* file "named_lin_ineq.h" */

/*  Copyright (c) 1994 Stanford University

    All rights reserved.

    This software is provided under the terms described in
    the "suif_copyright.h" include file. */

#include <suif_copyright.h>

#pragma interface

#ifndef NAMED_LIN_INEQ_H
#define NAMED_LIN_INEQ_H

enum name_table_entry_kind { nte_none,
                             nte_symconst,
                             nte_cond,
                             nte_loop,
                             nte_dim,
                             nte_summary,
                             nte_aux };

class array_info;
class access_vector;

/**************************************************************************
 ***                                                                    ***
 *** The name of each column.                                           ***
 ***                                                                    ***
 **************************************************************************/
class name_table_entry {
friend class name_table;
    name_table_entry_kind knd;
    union {
        var_sym * v;
        const char * s;
    } vsname;
    boolean is_var;

public:
    name_table_entry()                          { init(); }
    name_table_entry(const immed & im)          { init(im); }
    name_table_entry(name_table_entry_kind k,
                     const immed & nm)          { init(k, nm); }
    ~name_table_entry();

    void init()                                 { knd = nte_none;
                                                  is_var = FALSE;
                                                  vsname.s = NULL;
                                              }
    void init(const immed &);
    void init(name_table_entry_kind k, const immed & nm);
    void init(const name_table_entry &);
    void operator=(const name_table_entry &nte) { init(nte); };

    immed name() const;
    const char * string() const;
    var_sym * var() const                       { assert(is_var);
                                                  return vsname.v;
                                              }

    void set_name(const immed &);

    name_table_entry_kind kind() const         { return knd; }

    void mark_sym();
    void mark_cond();
    void mark_loop();
    void mark_dim();
    void mark_summary();
    void mark_aux();

    boolean operator==(const name_table_entry & nte) const;
    boolean operator!=(const name_table_entry & nte) const
      { return !(*this == nte); }

    base_symtab * get_symtab(base_symtab * in = NULL) const;

    void print(FILE * fp=stdout) const;
};


/**************************************************************************
 ***                                                                    ***
 *** A list of names identifying all the column.                        ***
 ***                                                                    ***
 *** The 0th column is for the constant, thus does not have a name.     ***
 *** All the other columns will be named by entries in the table        ***
 ***                                                                    ***
 **************************************************************************/
class name_table {
    name_table_entry * L;
    int sz;
    int rsz;
    void resize(int newsz);
public:
    name_table()                                { L = NULL; sz = 1; rsz = 1; }
    name_table(const name_table * nt)           { L = NULL; sz = 1; rsz = 1; init(*nt); }
    name_table(const name_table & nt)           { L = NULL; sz = 1; rsz = 1; init(nt); }
    name_table(int s)                           { L = NULL; sz = 1; rsz = 1; init(s); }
    name_table(const immed * v1,        const immed * v2 =NULL,
               const immed * v3 =NULL,  const immed * v4 =NULL,
               const immed * v5 =NULL,  const immed * v6 =NULL,
               const immed * v7 =NULL,  const immed * v8 =NULL,
               const immed * v9 =NULL,  const immed * v10=NULL,
               const immed * v11=NULL,  const immed * v12=NULL,
               const immed * v13=NULL,  const immed * v14=NULL,
               const immed * v15=NULL,  const immed * v16=NULL);
    ~name_table();
    void init(const name_table &);
    void init(int s);

    name_table &operator=(const name_table &other);

    int n() const                               { return sz; }
    name_table_entry & operator[](int i)        { assert((i>=1)&&(i<sz));
                                                  return L[i-1]; }
    const name_table_entry & e(int i) const     { assert((i>=1)&&(i<sz));
                                                  return L[i-1]; }

    int find(const name_table_entry &) const;
    int find(const immed & vs) const;

    void remove(int i, int j);
    void remove(int i)                          { remove(i, i); }
    void remove(const integer_row &rem);
    void insert(const name_table_entry & nte, int i);
    void insert(const immed & v, int i);

    name_table operator&(const name_table &) const;    // merge 2 name tables;
    name_table & operator&=(const name_table &);  // merge with other;

    // reorders the table so element kind ordering matches historical ordering;
    // sets ca so that new[ca[i]] = old[i];
    // returns TRUE if table order is changed;
    boolean do_reorder_table(integer_row &ca);

    // changes ntes occurring in both tables to sync their kinds;
    static void change_name_types(name_table & A,
                                  name_table & B);

    // are tables aligned and ordered?;
    static boolean is_aligned(const name_table & A,
                              const name_table & B);

    // makes a table ret aligning elts of na and nb, reordered;
    static name_table * mk_align(const name_table & na, const name_table & nb);

    // makes a table ret aligning elts of na and nb, reordered;
    // and sets ca and cb;
    // so that ret[ca[i]]==na[i] and ret[cb[i]]==nb[i];
    static name_table * align_tables(const name_table &na,
				     const name_table &nb,
                                     integer_row & ca, integer_row & cb);
    // aligns this with nb, reorders, and sets ca and cb so that;
    // so that this[ca[i]]==oldthis[i] and this[cb[i]]==nb[i];
    void align_with(const name_table &nb,
		    integer_row & ca, integer_row & cb);

    constraint lio_code_type() const;

    base_symtab * get_symtab(base_symtab * in = NULL) const;

    void print(FILE * fp=stdout) const;
    void print_symtabs(FILE * fp=stdout) const;
};


enum print_exp_type { pet_single, pet_system, pet_system_nl, pet_max, pet_min, pet_expr, pet_expr_nl};

/**************************************************************************
 ***                                                                    ***
 *** System of linear inequalities with names associated with           ***
 *** each column.                                                       ***
 ***                                                                    ***
 **************************************************************************/
class named_lin_ineq {
friend class named_lin_ineq_list;
    int unum;
    static int unum_cnt;
    name_table nt;
    lin_ineq   lq;
public:
    named_lin_ineq() : lq(0, 1)                 { init(); }
    named_lin_ineq(const named_lin_ineq & c)    { init(); init(c);  }
    named_lin_ineq(const named_lin_ineq * c)    { init(); init(*c); }
    named_lin_ineq(const immed_list & il)       { init(); init(il); }
    named_lin_ineq(int m, int n) : nt(n),lq(m, n) { init(); }
    named_lin_ineq(const name_table &n,
                   const lin_ineq &l)           { init();
                                                  nt.init(n);
                                                  lq = l;
                                                }
    ~named_lin_ineq();
private:
    void init();
public:
    void init(const named_lin_ineq & c);
    void init(int m, int n);
    int init(const immed_list & il, int c=0);
    immed_list * cvt_immed_list() const;

    int uid() const { return unum; }

    name_table & names()                        { return nt; }
    lin_ineq   & ineqs()                        { return lq; }

    const name_table & const_names() const      { return nt; }
    const lin_ineq   & const_ineqs() const      { return lq; }


    int n() const                               { assert(nt.n() == lq.n());
                                                  return nt.n(); }
    int m() const                               { return lq.m(); }

    void swap_col(int i, int j);
    void add_col(const name_table_entry & nte, int i);
    void del_col(int i, int j);
    void del_col(int i)                         { del_col(i, i); }
    void del_col(const integer_row &);

    int find(const immed & v) const          { return const_names().find(v); }

    void project();
    void project_away(const immed &, int *changed=0);
    void project_away(const name_table &, int *changed=0);

    named_lin_ineq & operator=(const named_lin_ineq & c);
    named_lin_ineq & operator=(const lin_ineq & l);

    named_lin_ineq operator&(const named_lin_ineq & c) const;
    named_lin_ineq & operator&=(const named_lin_ineq & c);
    named_lin_ineq & operator&=(const lin_ineq & l);
    // treats 0 as TRUE; returns 0 if c1==c2==0;
    // deletes or returns c{1,2} if del{1,2};
    static named_lin_ineq * opand(named_lin_ineq * c1,
                                named_lin_ineq * c2,
                                boolean del1=FALSE,
                                boolean del2=FALSE);
    boolean operator==(const named_lin_ineq & c2) const;
    boolean operator>=(const named_lin_ineq & c2) const;
    boolean operator<=(const named_lin_ineq & c2) const
	{ return (c2 >= (*this)); };
    boolean operator~() const;

    /* saman compatibility */
    boolean operator>>(const named_lin_ineq & c2) const
	{ return (*this >= c2); };
    boolean operator<<(const named_lin_ineq & c2) const
      { return (c2 >> (*this)); }
    // aligns tables and reorders them;
    void operator||(named_lin_ineq & c) { align_with(c); };
    /* end saman compatibility */

    // a simple substitution method
    // This is a cheap alternative to expensive Fourier-Motzkin
    // substitution expression given in expr is of the form
    //var >= sub_expr and var <= sub_expr or
    // sub_expr >= 0
    named_lin_ineq * substitute(const immed & var, const named_lin_ineq & expr)
            const;
    void do_substitute(const immed & var, const named_lin_ineq & expr);

    // do all sorts of fm-pairwise projection;
    void find_bounds();

    void cleanup(); // does simple cleanup;
    // returns true for no-soln sys after cleanup();
    boolean is_clean_false() const;
    void del_zero_cols();

    void simplify(); // does somewhat more work to cleanup;

    void print(FILE *fp = stdout) const;
    void print_exp(print_exp_type tp=pet_single, FILE *fp = stdout) const;

    instruction * mk_bounds() const;
    instruction * create_expression(const immed & v,
                                    boolean is_ub,
                                    block_symtab * sym=NULL) const;


    // NEED TO LINK LIBDEPENDENCE TO USE THIS FUNCTION
    static named_lin_ineq * mk_named_lin_ineq(array_info & ai,
                                              immed ind,
                                              boolean lb);
    // NEED TO LINK LIBDEPENDENCE TO USE THIS FUNCTION
    static named_lin_ineq * mk_named_lin_ineq(access_vector & av,
                                              immed ind,
                                              boolean lb);

    // reorders the table so element kind ordering matches historical ordering;
    // if ca, then sets ca so that new[*ca[i]] = old[i];
    void do_reorder_table(integer_row *ca=0);

    // aligns tables and reorders them;
    static void align_named_lin_ineqs(named_lin_ineq & A,
                                      named_lin_ineq & B);
    // aligns tables and reorders them;
    void align_with(named_lin_ineq &B);

    // aligns this table with nt; does not reorder;
    void align(const name_table & nt);

    // aligns tables, maximizing congruence of aux variables;
    // may alter systems;  returns number of auxes merged;
    int merge_auxes(named_lin_ineq &c2);

    static boolean is_aligned(const named_lin_ineq & A,
                              const named_lin_ineq & B);

    static named_lin_ineq * merge_named_lin_ineqs(const named_lin_ineq & A,
                                                  const named_lin_ineq & B);
};

typedef named_lin_ineq * p_named_lin_ineq;

// NEED TO LINK LIBDEPENDENCE TO USE THIS FUNCTION
named_lin_ineq * include_for(tree_for * tf,
                             named_lin_ineq * c = NULL);

#endif
