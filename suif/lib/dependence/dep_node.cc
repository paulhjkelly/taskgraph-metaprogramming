/* file "dep_node.cc" */

/*  Copyright (c) 1994 Stanford University

    All rights reserved.

    This software is provided under the terms described in
    the "suif_copyright.h" include file. */

#include <suif_copyright.h>

#define _MODULE_ "libdependence.a"
#pragma implementation "dep_node.h"

#include <cstdio>
#include <suif1.h>
#include <builder.h>
#include <suifmath.h>
#include "dependence.h"
#include "include.h"

const char * k_dep_for_annote;
const char * k_dep_proc_annote;
const char * k_dep_instr_annote;
boolean dep_stats_flag = FALSE;


static void append_to_list(array_info *a, const operand & op, tree_node *tn, if_ops multi, int style)
{
    if((!op.is_instr()) || op.instr()->opcode() != multi ) {
        //    if(si->instr->opcode != multi && si->instr->opcode != io_cpy) {
        // if it's a copy, r2==nil_register and you recurse
        a->append(new access_vector(op, tn, style));
    } else {
        assert(op.instr()->opcode() == multi);
        in_rrr * ir = (in_rrr *)op.instr();
        append_to_list(a, ir->src1_op(), tn, multi, style);
        append_to_list(a, ir->src2_op(), tn, multi, style);
    }
}


// find access vectors for bounds or step.
// If recursive, fix array references inside and also inside fors

static void fill_walk(tree_node_list *a, int rec, int normalize=1);

struct fw_data {
    tree_node * t;
    int rec;
    int normalize;
    fw_data(tree_node * tn, int r, int n) { t=tn; rec=r; normalize=n; }
};
static void fill_walk(const operand & op, tree_node * t, int rec, int normalize = 1);
static boolean fill_walk_src_map(instruction *, operand *, void * v);
static void fill_walk_instr_map(instruction * inst, void * v);


void _fill_walk(tree_node_list *l,int rec,int normalize = 1)
{
    fill_walk(l,rec,normalize);
}

dep_proc_annote::~dep_proc_annote()
{

}

dep_instr_annote::~dep_instr_annote()
{
    if(ai) delete ai;
}

dep_for_annote::~dep_for_annote()
{
    if(stp) delete stp;
    if(lb) delete lb;
    if(ub) delete ub;
}

void dep_for_annote::fill_in_access(tree_for * t, int rec,int normalize)
{
    dep_proc_annote *p = (dep_proc_annote *)t->proc()->block()->peek_annote(k_dep_proc_annote);
    assert(p);

    if(stp) delete stp;
    stp = new access_vector(t->step_op(), t, p->fancy_access_vectors);
    if(lb) delete lb;
    lb = new array_info;
    append_to_list(lb, t->lb_op(), t, io_max, p->fancy_access_vectors);
    if(ub) delete ub;
    ub = new array_info;
    append_to_list(ub, t->ub_op(), t, io_min, p->fancy_access_vectors);

    if(lb->is_empty()) {
        printf("*** WARNING: EMPTY LOWER BOUND *** loop %s \n",t->index()->name());
//BUGBUG        t->lb_op().print(stdout);
        fflush(stdout);
    }
    if(ub->is_empty()) {
        printf("*** WARNING: EMPTY UPPER BOUND *** loop %s \n",t->index()->name());
//BUGBUG        t->ub_op().print(stdout);
        fflush(stdout);
    }

//    printf(" lb: "); lb->print();
//    printf(" ub: "); ub->print();

    if(rec && normalize) {
    	lb->normalize_step_ref();
    	ub->normalize_step_ref();
    	stp->normalize_step_ref();
    }

   if(rec) {
       fill_walk(t->lb_op(), t, 1, normalize);
       fill_walk(t->ub_op(), t, 1, normalize);
       fill_walk(t->step_op(), t, 1, normalize);
       fill_walk(t->landing_pad(), 1, normalize);
       fill_walk(t->body(), 1, normalize);

	if(normalize) {
            normalize_step_loops(lb, ub, stp, t->test());
            normalize_test(lb, ub, stp, t->test()); 	
        }

   }

}

static void fill_walk(tree_node_list *a, int rec, int normalize)
{
    tree_node_list_iter tnli(a);
    while(!tnli.is_empty()) {
        tree_node *n = tnli.step();
        switch(n->kind()) {
        case TREE_LOOP:
            if(rec) {
                fill_walk(((tree_loop *)n)->body(), 1, normalize);
                fill_walk(((tree_loop *)n)->test(), 1, normalize);
            }
            break;
        case TREE_IF:
            if(rec) {
                fill_walk(((tree_if *)n)->header(), 1, normalize);
                fill_walk(((tree_if *)n)->then_part(), 1, normalize);
                fill_walk(((tree_if *)n)->else_part(), 1, normalize);
            }
            break;
        case TREE_FOR:
	    {
            tree_for *f = (tree_for *)n;
            dep_for_annote * olda = (dep_for_annote *)f->get_annote(k_dep_for_annote);
            if(olda) delete olda;
            dep_for_annote * afor = new dep_for_annote(f);
            f->append_annote(k_dep_for_annote, afor);
            afor->fill_in_access(f,  rec, normalize);
            break;
	    }
        case TREE_INSTR:
	    {
            tree_instr *s = (tree_instr *)n;
            fw_data D(n, rec, normalize);
            s->instr_map(fill_walk_instr_map, (void *)&D);
            break;
	    }
        case TREE_BLOCK:
            fill_walk(((tree_block *)n)->body(), 1, normalize);
            break;
        default:
            error_line(1, NULL, "unknown ast kind: %d", n->kind());
        }
    }
}


void fill_walk(const operand & op, tree_node * t, int rec, int normalize)
{
    if(!op.is_instr()) return;
    fw_data D(t, rec, normalize);
    op.instr()->src_map(fill_walk_src_map, (void *)&D);
}

void fill_walk_array_instr(in_array * ins, fw_data * D)
{
    dep_proc_annote *p = (dep_proc_annote *)D->t->proc()->block()->peek_annote(k_dep_proc_annote);
    assert(p);

    dep_instr_annote * olda = (dep_instr_annote *)ins->get_annote(k_dep_instr_annote);
    if(olda) delete olda;
    dep_instr_annote * asi = new dep_instr_annote(ins);
    ins->append_annote(k_dep_instr_annote, asi);
    asi->ai = new array_info(ins, p->fancy_access_vectors);
    if (D->normalize) asi->ai->normalize_step_ref();
    
}

boolean fill_walk_src_map(instruction *, operand * op, void * v)
{
    fw_data * D = (fw_data *)v;
    
    if(op->is_instr()) {
        instruction * ins = op->instr();
        if(ins->opcode() == io_array) {
            fill_walk_array_instr((in_array *)ins, D);
        }
        if(D->rec)
            ins->src_map(fill_walk_src_map, (void *)D);
    }

    return FALSE;
}

void fill_walk_instr_map(instruction * ins, void * v)
{
    fw_data * D = (fw_data *)v;
    
    if(ins->opcode() == io_array) {
        fill_walk_array_instr((in_array *)ins, D);
    }
}


void fill_in_access(tree_proc * tp, 
                    boolean normalize, 
                    boolean fancy,
                    boolean forget_about_mod)
{
    forget_about_mod_this = forget_about_mod;
    dep_proc_annote * olda = (dep_proc_annote *)tp->get_annote(k_dep_proc_annote);
    if(olda) delete olda;
    dep_proc_annote * a = new dep_proc_annote(tp);
    a->fancy_access_vectors = fancy;
    tp->append_annote(k_dep_proc_annote, a);
    fill_walk(tp->body(), 1, normalize);
}


extern boolean normalize_code(tree_for *tf);
void fill_in_access(tree_for * tf, normalize_kinds norm, boolean * normalize_code_ok)
{
    if(normalize_code_ok) 
        *normalize_code_ok = TRUE;
    dep_for_annote * olda = (dep_for_annote *)tf->get_annote(k_dep_for_annote);
    if(olda) delete olda;
    dep_for_annote * afor = new dep_for_annote(tf);
    tf->append_annote(k_dep_for_annote, afor);

    boolean normalize = norm >= NORMALIZE_VECTORS;
    afor->fill_in_access(tf, TRUE, normalize);

    if (norm == NORMALIZE_CODE) {
        if(normalize_code(tf) == FALSE) {
            if(normalize_code_ok) 
                *normalize_code_ok = FALSE;
            else
                assert_msg(0, ("Normalization failed for loop %s", tf->index()->name()));
        }
        afor->fill_in_access(tf, TRUE, TRUE);
    }
}

/*
static int le(int l, int u, char *test)
{
    int ok;
    if(test == k_ulte) ok = ((unsigned) l) <= u;
    else if(test == k_ult) ok = ((unsigned) l) < u;
    else if(test == k_ugte) ok = ((unsigned) l) >= u;
    else if(test == k_ugt) ok = ((unsigned) l) > u;
    else if(test == k_slte) ok = l <= u;
    else if(test == k_slt) ok = l < u;
    else if(test == k_sgte) ok = l >= u;
    else if(test == k_sgt) ok = l > u;
    else error_line(1, NULL, "unknown test \"%s\"", test);
    return ok;
}
*/

/*
static int all_identical_excluding_const(array_info *lb, array_info *ub, 
                                         int *lbmax, int *ubmin)
{
    assert(!lb->is_empty() && !ub->is_empty() && lbmax && ubmin);
    array_info_iter aii(lb);
    access_vector *av = aii.step();
    if(av->too_messy)
        return 0;
    *lbmax = av->con;
    while(!aii.is_empty()) {
        access_vector *av2 = aii.step();
        if(av2->too_messy)
            return 0;
        av_compare_info c(av, av2);
        if(!c.identical_excluding_const())
            return 0;
        if(av2->con > *lbmax) *lbmax = av2->con;
    }
    aii = ub;
    *ubmin =  ((unsigned) (-1))>>1;
    while(!aii.is_empty()) {
        access_vector *av2 = aii.step();
        if(av2->too_messy)
            return 0;
        av_compare_info c(av, av2);
        if(!c.identical_excluding_const())
            return 0;
        if(av2->con < *ubmin) *ubmin = av2->con;
    }
    return 1;
}
*/

/*
int dep_for_annote::must_not_execute()
{
    int l, u;

    if(step_is_constant(0) &&
       lb && lb->count() > 0 &&
       ub && ub->count() > 0 &&
       all_identical_excluding_const(lb, ub, &l, &u) &&
       !le(l, u, test))
        return 1;
    else
        return ast_node_for::must_not_execute();
}
*/

/*
int dep_for_annote::must_execute()
{
    int l, u;
    if(step_is_constant(0) &&
       lb && lb->count() > 0 &&
       ub && ub->count() > 0 &&
       all_identical_excluding_const(lb, ub, &l, &u) &&
       le(l, u, test))
        return 1;
    else
        return ast_node_for::must_execute();
}
*/

// Static variable declarations for class dependency_test in dep_node.h
int dependency_test::ex = 1;              
int dependency_test::one_var_succ = 0;    
int dependency_test::one_var_fail = 0;    
int dependency_test::cycle_ok = 0;        
int dependency_test::cycle_not_ok = 0;    
int dependency_test::lr_ok = 0;           
int dependency_test::lr_not_ok = 0;       
int dependency_test::fourier_ok = 0;      
int dependency_test::fourier_not_ok = 0;  
int dependency_test::const_test = 0;      
int dependency_test::num_indep = 0;       
int dependency_test::num_dep = 0;
int dependency_test::num_check_bounds = 0;
int dependency_test::num_first_call_fourier = 0;
int dependency_test::num_call_fourier = 0;
int dependency_test::max_bb_lev = 0;
int dependency_test::num_in_ineqs = 0;
int dependency_test::max_in_ineqs = 0;
int dependency_test::reply_true = 0;



dependency_test::dependency_test()
{

}

dependency_test::~dependency_test()
{
    if (!dep_stats_flag)
	return;
  fprintf(stderr, "Dependency test statistics\n");
  fprintf(stderr, "%30s = %10d\n", "one_var_succ", one_var_succ);
  fprintf(stderr, "%30s = %10d\n", "one_var_fail", one_var_fail);
  fprintf(stderr, "%30s = %10d\n", "cycle_ok", cycle_ok);
  fprintf(stderr, "%30s = %10d\n", "cycle_not_ok", cycle_not_ok);
  fprintf(stderr, "%30s = %10d\n", "lr_ok", lr_ok);
  fprintf(stderr, "%30s = %10d\n", "lr_not_ok", lr_not_ok);
  fprintf(stderr, "%30s = %10d\n", "fourier_ok", fourier_ok);
  fprintf(stderr, "%30s = %10d\n", "fourier_not_ok", fourier_not_ok);
  fprintf(stderr, "%30s = %10d\n", "const_test", const_test);
  fprintf(stderr, "%30s = %10d\n", "num_indep", num_indep);
  fprintf(stderr, "%30s = %10d\n\n", "num_dep", num_dep);
  fprintf(stderr, "%30s = %10d\n", "num_check_bounds", num_check_bounds);
  fprintf(stderr, "%30s = %10d\n", "num_first_call_fourier", num_first_call_fourier);
  fprintf(stderr, "%30s = %10d\n", "num_call_fourier", num_call_fourier);
  fprintf(stderr, "%30s = %10d\n", "max_bb_lev", max_bb_lev);
  fprintf(stderr, "%30s = %10d\n", "num_in_ineqs", num_in_ineqs);
  fprintf(stderr, "%30s = %10d\n", "max_in_ineqs", max_in_ineqs);
  fprintf(stderr, "%30s = %10d\n", "reply_true", reply_true);
  fprintf(stderr, "\n");
}

static void free_dep_for_annote(void *data)
{
    delete (dep_for_annote *) data;
}

static void free_dep_proc_annote(void *data)
{
    delete (dep_proc_annote *) data;
}

static void free_dep_instr_annote(void *data)
{
    delete (dep_instr_annote *) data;
}

boolean dependence_library_no_integer_result(const integer_matrix &,
                                             boolean *);
integer_matrix dependence_library_integer_solver(const integer_matrix &,
                                                 integer_row *);

void init_dependence(int &argc, char *argv[])
{
    static cmd_line_option dependence_options[] =
      {
        {CLO_NOARG, "-dep-stats", NULL, &dep_stats_flag}
      };

    STRUCT_ANNOTE(k_dep_for_annote, "dep_for_annote", FALSE, NULL, NULL,
                  &free_dep_for_annote, NULL);
    STRUCT_ANNOTE(k_dep_proc_annote, "dep_proc_annote", FALSE, NULL, NULL,
                  &free_dep_proc_annote, NULL);
    STRUCT_ANNOTE(k_dep_instr_annote, "dep_instr_annote", FALSE, NULL, NULL,
                  &free_dep_instr_annote, NULL);
    ANNOTE(k_depset_is_run, "depset_is_run", TRUE);
    ANNOTE(k_depset_symconst_ok, "depset_symconst_ok", TRUE);
    

    NIR_hook = dependence_library_no_integer_result;
    IS_hook  = dependence_library_integer_solver;

    parse_cmd_line(argc, argv, dependence_options,
                   sizeof(dependence_options) / sizeof(cmd_line_option));

    dependency_test dt;
    dt.do_exact();

//    REGISTER_PASSTHRU(k_dep_for_annote, "dep_for_annote");
//    REGISTER_PASSTHRU(k_dep_proc_annote, "dep_proc_annote");
//    REGISTER_PASSTHRU(k_dep_instr_annote, "dep_instr_annote");
}

void exit_dependence(void)
{
    return;
}

