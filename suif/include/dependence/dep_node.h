/* file "dep_node.h" */

/*  Copyright (c) 1994 Stanford University

    All rights reserved.

    This software is provided under the terms described in
    the "suif_copyright.h" include file. */

#include <suif_copyright.h>

#pragma interface
#ifndef DEP_NODE
#define DEP_NODE

struct dep_proc_annote {
    tree_proc * my_proc;
    int fancy_access_vectors;

    dep_proc_annote(tree_proc * tp) { my_proc = tp; 
                                      fancy_access_vectors=TRUE; }
    ~dep_proc_annote();
    void fill_in_access(int normalize=1);
};

struct dep_instr_annote {
    instruction * my_instr;
    array_info *ai;
    dep_instr_annote(instruction * i) { my_instr = i;
                                        ai = 0; }
    ~dep_instr_annote();
};

struct dep_for_annote {
    tree_for * my_for;
    access_vector *stp;
    array_info *lb,*ub;
    
    dep_for_annote(tree_for * tf) { my_for = tf;
                                    stp = NULL;
                                    lb = ub = NULL; }
    ~dep_for_annote();
    void fill_in_access(tree_for * t, int rec,int normalize = 1);
    int must_execute();
    int must_not_execute();
};


// class to tell which dependency tests to do
class dependency_test {
    static int ex;
public:
    dependency_test();
    ~dependency_test();
    int exact() { return ex; }
    void do_exact() { ex = 1; }
    void no_exact() { ex = 0; }
// statistics
    static int one_var_succ;
    static int one_var_fail;
    static int one_var_ind;
    static int one_var_dep;
    static int fourier_ok;
    static int fourier_not_ok;
    static int fourier_ind;
    static int fourier_dep;
    static int cycle_ok;
    static int cycle_not_ok;
    static int cycle_ind;
    static int cycle_dep;
    static int lr_ok;
    static int lr_not_ok;
    static int lr_ind;
    static int lr_dep;
    static int const_test;
    static int num_indep;
    static int num_dep;

    static int num_check_bounds;
    static int num_first_call_fourier;
    static int num_call_fourier;
    static int max_bb_lev;
    static int num_in_ineqs;
    static int max_in_ineqs;
    static int reply_true;
};


extern const char * k_dep_for_annote;
extern const char * k_dep_proc_annote;
extern const char * k_dep_instr_annote;
extern const char * k_depset_is_run;
extern const char * k_depset_symconst_ok;


enum normalize_kinds {
    NORMALIZE_NONE,          // don't normalize
    NORMALIZE_VECTORS,       // normalize access vectors but don't modify code
    NORMALIZE_CODE           // normalize access vectors *and* modify the code
};

void fill_in_access(tree_proc * tp, 
                    boolean normalize = TRUE,
		    boolean fancy=TRUE,
                    boolean forget_about_mod=FALSE);

void fill_in_access(tree_for  * tp, 
                    normalize_kinds norm = NORMALIZE_NONE,
                    boolean * normalize_code_ok=NULL);

DECLARE_DLIST_CLASSES(tn_list, tn_list_e,
		      tn_list_iter, tree_node*);

#endif
