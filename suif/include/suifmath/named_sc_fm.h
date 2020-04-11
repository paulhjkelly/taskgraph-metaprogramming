/* file "named_sc_fm.h" */

/*  Copyright (c) 1994 Stanford University

    All rights reserved.

    This software is provided under the terms described in
    the "suif_copyright.h" include file. */

#include <suif_copyright.h>

#pragma interface

#ifndef NAMED_SC_FM_H
#define NAMED_SC_FM_H


/**************************************************************************
 **************************************************************************
 *****                                                                *****
 *****                                                                *****
 *****     F O U R I E R - M O T Z K I N   E L I M I N A T I O N      *****
 *****                                                                *****
 *****                                                                *****
 **************************************************************************
 **************************************************************************/


/**************************************************************************
 ***                                                                    ***
 *** A special representation of a inequality for fm-elimination        ***
 ***                                                                    ***
 *** Different for named_symcoeff_ineq data representation, inequality  ***
 *** selection is the outermost dimension. This representation is for   ***
 *** the inner 2 dimension;                                             ***
 ***   planes or symbolic constants are the outer dimension.            ***
 ***   columns or variables are the inner dimension.                    ***
 ***                                                                    *** 
 *** This representation is optimized for Fourier-Motzkin.  Since many  ***
 *** comparisons and calculations are made using inequalities and only  ***
 *** time an inequality is changed is at the creation, many commonly    ***
 *** used functions are pre-calculated.                                 ***
 ***    crnk - is the maximum rank in the column dimension              ***
 ***    prnk - is the maximum rank in the plane dimension               ***
 ***    sgn[i] - each sgn[i] is the sign of a given column              ***
 ***    magic_num - if two magic_nums are different the ineqalities are ***
 ***                garunteed to be different.                          ***
 ***                                                                    ***
 **************************************************************************/
#define BAD_SIGN        9985
class sc_fm_constraint {
friend class named_sc_fm;
    int * L;
    int nn;
    int pp;
    int * sgn;
    int crnk;
    int prnk;
    unsigned magic_num; // if two constraints are equal => magic #'s are equal
                        // testing this will reduce the cost of is_already_covered
public:
    sc_fm_constraint(int n, int p);
    ~sc_fm_constraint();

    int n()                                     { return nn; }
    int p()                                     { return pp; }
    
    // an element at plane p column n is (*this)[p][n];
    int * operator[](int ip)                    { 
#ifdef DO_BOUND_CHK
                                                  assert((0<=ip)&&(ip<p()));
#endif
                                                  return L+ip*n(); 
                                              }
    int sign(int in);
    int sign_no_chk(int in)                     { return sgn[in]; }
      
    int rank()                                  { return crnk; }
    int plane_rank()                            { return prnk; }
    boolean is_zero()                           { return (rank()==-1); }

    boolean is_zero(int in);
    void get(constraint &, int in);
    
    void recalc();
    void inverse();
    void div_by_gcd();
    boolean is_only_pos_const();
    boolean is_coef_eq_1(int j);

    void print(FILE *fd=stdout);
private:
    void recalc(int i);
    static void error();
};

typedef sc_fm_constraint * p_sc_fm_constraint;


/**************************************************************************
 ***                                                                    ***
 **************************************************************************/
class sc_fm_results {
    int mm;
    int sz;
    int * L;
public:
    sc_fm_results();
    ~sc_fm_results();
    
    void init(int im, int s);
    int m()                             { return mm; }
    int * operator[](int im)            { assert((im>=0)&&(im<m()));
                                          return &L[im*sz]; }

    void set(int im, sc_fm_constraint & c);
};
    
    

/**************************************************************************
 ***                                                                    ***
 *** Fourier motzkin elimination and creation of efficient loop bounds  ***
 ***                                                                    ***
 *** Note:  Static data representation to reduce malloc calls; thus     ***
 ***        only a single copy of this can exist in a program at any    ***
 ***        given time.                                                 ***
 ***                                                                    ***
 ***                                                                    ***
 **************************************************************************/
class named_sc_fm {
    // size of the static data-set 
    static int cm;
    static int cn;
    static int cp;
    static sc_fm_constraint ** L;                      // static data-set
    static unsigned * magic_list;
    int xm;                                             // # of ineq's in use
    name_table nm_p;                                    // plane names
    name_table nm_c;                                    // column names
    boolean keep;                                       // keep all ineqs or
                                                        // throw them out
    // don't reduce an inequality pair if both are below 
    // this point use for efficiency reasons when doing 
    // FM-elimination to get rid of redundent loop bounds.
    int cutoffpt; 

    sc_fm_results    * results;
                                                        

    // to accumilate statistics
    int stat_stm, stat_stn, stat_stp, stat_nexp, stat_maxm, stat_ninit, stat_ninitex; 
    long stat_sttime;

public:
    named_sc_fm(named_symcoeff_ineq* x, boolean keep=TRUE) { init(*x,keep); }
    named_sc_fm(named_symcoeff_ineq& x, boolean keep=TRUE) { init(x, keep); }
    ~named_sc_fm();
    // delete the staticly allocated data
    static void delete_all();           

    named_symcoeff_ineq * get();
    void get(named_symcoeff_ineq * t);

    named_symcoeff_ineq * internal_get();

private:
    void init(named_symcoeff_ineq & x, boolean k);
    int n()                                     { return nm_c.n(); }
    int p()                                     { return nm_p.n(); }
    int m()                                     { return xm; }
    sc_fm_constraint & operator[](int i)       { 
#ifdef DO_BOUND_CHK
                                                  assert((i>=0)&&(i<m()));
                                                  assert(i<cm);
                                                  assert(L[i]);
#endif
                                                  return *L[i]; }
    void init_block(int m);
    void extend_block(int m);

public:
    // Fourier-Motzkin Elimination
    // Return TRUE if answer exists  (opposite from lin_ineq~)
    boolean fm_step();
    boolean fm_step(int, int);
    boolean fm_step(int);
private:
    void fm_step(int j, int la, int lb, int ua, int ub);
    void fm_step(int, sc_fm_constraint &, sc_fm_constraint &, sc_fm_constraint &);
    boolean fm_lin_step();

public:
    // Calculating bounds of a loop nest
    void fm_bounds();
    void fm_bounds(int, int);

    // Only fourier Motzkin elimination; No tightning of the bounds
    void fm_project();
    void fm_project(int, int);
private:
    void fm_project(int);
    void fm_bounds(int);
    void fm_bounds(int, int, int, int *);
    void fm_bounds_do(int, boolean no_del);
    void fm_bounds_rec(int);

    boolean check_valid(int r);
    void remove(int *);
    void remove(int);
    boolean is_already_covered(int);
    boolean is_already_covered(sc_fm_constraint &c, sc_fm_constraint &tmp);
    boolean is_already_covered_chk_const(sc_fm_constraint &c, sc_fm_constraint &tmp);

    void sort();
    void swap(int i, int j);

    void print(FILE *fd=stdout);
    void error();
};

#endif





