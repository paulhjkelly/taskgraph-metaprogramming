/* file "bounds.h" */

/*  Copyright (c) 1994 Stanford University

    All rights reserved.

    This software is provided under the terms described in
    the "suif_copyright.h" include file. */

#include <suif_copyright.h>

#pragma interface

#ifndef BOUNDS_H
#define BOUNDS_H


/***************************************************************************
 *** poly_interator                                                      ***
 ***                                                                     ***
 *** Adopted From Scanning Polyhedra with DO Loops                       ***
 *** by C. Ancourt and F. Irigoin                                        ***
 *** (c) 1991 ACM 0-89791-390-6/91/0004/0039                             ***
 ***************************************************************************/
class poly_iterator {
friend class fmred;
    lin_ineq  S;		// initial lin_ineq;
    lin_ineq  SPDEL; 
    lin_ineq  SP;
    int dim; 			// columns in S;
    constraint sort_order;    // How to sort the array
    constraint reduce_order;  // Which to reduce in which oder
    constraint delete_order;  // Which to delete
    lin_ineq strides;         // Stride of each loop variable
public:
    poly_iterator(const lin_ineq * in) { S.init(in); 
                                         dim = in->n(); }

    poly_iterator(const lin_ineq & in) { S.init(in); 
                                         dim = in.n(); }

    ~poly_iterator() { }

    lin_ineq iniS() { return S; }
    lin_ineq *iniS_ptr() { return &S; }
private:
    // Needed for fm_pair
    int next(int i, int col, int sgn);

    void fm_pair(lin_ineq &outM, int i, lin_ineq & inM);
    void merge(lin_ineq & outM, int i, lin_ineq & inM);
    void delete_merge(lin_ineq & outM, int i, lin_ineq & inM);
    int num_dimensions() { return dim; }
    int last(lin_ineq & M, int row);
    void get_strides(lin_ineq & M, int * dv);
    void del_variables(lin_ineq & M, int * dv);
    // try to get rid of redundant constraints and tighten up others;
    void reduce_extra_constraints(lin_ineq & M);
    void reduce_extra_constraints2(lin_ineq & M);
    


public:
    void init(const lin_ineq * in) { S.init(in); 
                                     dim = in->n(); }
    /* fourier-motzkin project S, then sort according to sort_order */
    /* return S */
    lin_ineq * get_iterator(int no_del_on_merge=0);
    /* Create Source ordering s.t. */
    /*  s_o[c] = (dv[c]== - 1)*(1 + c) + (dv[c] > 0)*16*2^c  */
    /* so dv[c]>0 matters most, then dv[c]==0 */

    void set_sort_order(int * dv);
    void set_sort_order(column_stat dv[]);
    const lin_ineq *delete_irrelevant_variables();
    const lin_ineq *delete_irrelevant_variables(int * dv) { 
        get_strides(S, dv);
        del_variables(S, dv);
        return &S; }
    lin_ineq without_irrelevant_variables(int * dv) { 
        lin_ineq tmp(S);
        del_variables(tmp, dv);
        return tmp; }
    lin_ineq *reduce_extra_constraints_ptr() { 
        reduce_extra_constraints(S); 
        return &S; }
    lin_ineq *reduce_extra_constraints2_ptr() { 
        reduce_extra_constraints2(S); 
        return & S; }
    lin_ineq *get_stride_ptr() { return & strides; };
    void print(FILE *fp=stdout);

    void dror_test();

    /* the following are left in for backwards compatibility,
       but their use is strongly discouraged due to potential 
       for unsafe pointer use */
    lin_ineq &reduce_extra_constraints() { 
	return *reduce_extra_constraints_ptr(); }
    lin_ineq &reduce_extra_constraints2() { 
	return *reduce_extra_constraints2_ptr(); }
    lin_ineq &get_stride() { return *get_stride_ptr(); };

};





/**********************************************************************************
 *** access_e                                                                   ***
 ***                                                                            ***
 *** list of accesses with the corrosponding levels                             ***
 **********************************************************************************/
class access_ineq_e {
    access_ineq_e * next;
    friend class access_ineq_list;
public:
    lin_ineq LEQ;
    int level;
    name_store Name;

    access_ineq_e() { level=-1; next = 0; }
    access_ineq_e(lin_ineq & inq, int lev) { LEQ.init(&inq); level=lev; next = 0; }
    access_ineq_e * step() { return next; }
};

class access_ineq_list {
    friend class access_ineq_list_iter;
    access_ineq_e * head;
public:
    access_ineq_list() { head = 0; }
    ~access_ineq_list();

    int max_level();

    access_ineq_list & operator+=(access_ineq_e *);
    access_ineq_list & operator-=(access_ineq_e *);

    void strip(access_ineq_list & in,  int num1, int num2);
    void strip(access_ineq_list & in,  int num1) { this->strip(in, num1, num1); }
    void append(access_ineq_list & in) { this->strip(in, 0, 100); }

    int operator<<(access_ineq_e &);
    access_ineq_e * first() { return head; }
};

class access_ineq_list_iter {
    access_ineq_list * ail;
    access_ineq_e * curr;
    int u, l;
    
    int range(access_ineq_e * ae) { return ((ae->level >= l)&&(ae->level <= u))?1:0; }
    void update();
    void init(access_ineq_list & al, int i, int j); 
public:
    access_ineq_list_iter(access_ineq_list & al) { init(al, 0, 100); }
    access_ineq_list_iter(access_ineq_list & al, int i) { init(al, i, i); } 
    access_ineq_list_iter(access_ineq_list & al, int i, int j) { init(al, i, j); }

    int is_empty() { return curr==0; }
    void rm(access_ineq_e * elem);
    access_ineq_e * step() { access_ineq_e * ret = curr; if(curr) update(); return ret; }
};


/**********************************************************************************
 *** priv_access                                                                ***
 ***                                                                            ***
 *** Will keep accesses for each level and ??
 **********************************************************************************/
class priv_access {
    access_ineq_list ineqs;
public:

    priv_access() { }

    access_ineq_e * add(lin_ineq & inq, int lev) { access_ineq_e * elem = new access_ineq_e(inq, lev); 
                                                   ineqs += elem; 
                                                   return elem; }


private:
    void remove_overlapped_in(int i);
    void remove_overlapped_outer(int i);
public:
    void remove_overlapped();


    void print(FILE *fp=stdout);
    void print_code(FILE *fp=stdout);
};


lin_ineq * ProjectForCodeGen(lin_ineq * in);

#endif /* BOUNDS_H */
