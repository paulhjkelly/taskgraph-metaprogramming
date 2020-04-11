/* file "dodep.h" */

/*  Copyright (c) 1994 Stanford University

    All rights reserved.

    This software is provided under the terms described in
    the "suif_copyright.h" include file. */

#include <suif_copyright.h>

#pragma interface
enum deptest_result { dt_none, 
                      dt_ok, 
                      dt_indep, 
                      dt_no_common_nest, 
                      dt_too_messy };

class dependence_test {
    dvlist * dv;
    deptest_result res;
public:
    dependence_test() { dv = NULL;  res = dt_none; }
    void do_test(in_array *w, in_array *r, int flow=FALSE, int lexpos=FALSE, int arraysaresame=FALSE);
    dvlist * get_dv() { return dv; }
    deptest_result result() { return res; }
private:
    int num_loops(tree_node *, tree_node *top=NULL);
    int num_mod_nest(tree_node *, array_info &);
    int find_minnest(tree_node *, tree_node *);
    tn_list *find_astlist(tree_node *);
    tn_list *find_astlist(tree_node *,tree_node *, int);
    int dim_ok(access_vector *, tree_instr *,int);
    dvlist * make_vec(dvlist *, int prev, int minnest, int lexpos);
    void mk_star_vec(int minnest, int lexpos);
};


dvlist * DependenceTest(in_array *w, 
                        in_array *r, 
                        int lexpos,
                        deptest_result * res,
                        int arraysaresame=FALSE);

inline dvlist * DependenceTest(in_array *w, 
                               in_array *r, 
                               deptest_result * res,
                               int arraysaresame=FALSE) {
    return DependenceTest(w, r, TRUE, res, arraysaresame);
}


inline dvlist * DependenceTest(in_array *w, 
                               in_array *r, 
                               int lexpos) {
      return DependenceTest(w, r, lexpos, NULL);
}

inline dvlist * DependenceTest(in_array *w, 
                               in_array *r) {
    return DependenceTest(w, r, TRUE, NULL);
}

void print_array_access(in_array * in, FILE * fs = stdout);
