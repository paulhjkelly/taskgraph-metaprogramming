/* file "exact.h" */

/*  Copyright (c) 1994 Stanford University

    All rights reserved.

    This software is provided under the terms described in
    the "suif_copyright.h" include file. */

#include <suif_copyright.h>

#pragma interface
//
// exact.h
//   data structures to help with exact test
//


struct dir_array;
class dir_list;

#undef MAX
#undef MIN

// class to handle values that are either integer or indef
class int_indef {  
public:
    int val;
    boolean is_indef;
    int_indef() {is_indef = TRUE; val=0xdeadbead;}
    int_indef(int i) {is_indef = FALSE ; val=i;}
friend int_indef operator+(int_indef a,int_indef b);
friend int_indef operator-(int_indef a,int_indef b);
friend int_indef operator*(int_indef a,int_indef b);
friend int_indef operator/(int_indef a,int_indef b);
friend boolean operator>(int_indef a,int_indef b);
friend int_indef MAX(int_indef a, int_indef b);
friend int_indef MIN(int_indef a, int_indef b);
    void print(FILE *fp) {
        if(is_indef) fprintf(fp,"*** ");else fprintf(fp,"%3d ",val);
    }
    void set_indef() {is_indef = TRUE;}
};


enum answer{independent, dependent, unsure};


#define LOWUPSIZE       150

// class for exact test
class exact {
	int indep;
	void set_indep() { indep=TRUE; }
	// arrays used in Wolfe's algorithm 
	int DD;  // all arrays are square with each dimension = DD+1
		 // except for T_constr which has 3*DD+1 row
	int *write_used;
	int flow;
	integer_matrix S;
	integer_matrix *L;
	integer_matrix *U;
	integer_matrix T_constr2; // simplified constraints after cyclic
			      // test (ie even if cyclic doesn't 
			      // completely apply, it may simplify);
	int_indef low[LOWUPSIZE];// lower and upper bounds on t variables
	int_indef up[LOWUPSIZE];
	void set_low_up(integer_matrix &  constr,int *ind);
	void set_low_up(integer_matrix & constr, int i, int *ind);
	int_indef *T;
	// Wolfe's algorithms
	void enforce_low_limits(int *lb_valid);  // Program 2.7
	void enforce_high_limits(int *ub_valid);  // Program 2.7
	int enforce_direction(dir_array *d, boolean *const_dist,
				int num_symb); // almost Progr 2.8
						      // return 1 implies indep
	void unenforce_direction(dir_array *d, boolean *const_dist,int num_symb); 
	int addbound(int *V, int k, int x); // fill up an entry in 
					    // the T_constr matrix;
	void delbound(int *V, int k, int x); 

	// loop through possible dir vectors
	void check_directions(dir_array *da,boolean *const_dist,
				int num_symb,int *lb_valid, int *ub_valid);
	// does single-variable test apply
	boolean single_var_ok(integer_matrix & constr);
	boolean single_var_check();
	// cycle test
	answer cycle_test(integer_matrix & constr, int *done_constraint);
	// lr test
	boolean lr_test_ok(integer_matrix & constr, int *done_constraint);
	boolean lr_test(integer_matrix & constr, int *done_constraint);
	boolean lr_test(int_indef **dag, int n);
	boolean lr_test(int_indef **dag, int n, int first);
	boolean lr_test(int_indef **dag, int n, int first, int node, int cost,
			int *onpath);
	answer fourier(integer_matrix & input, int *done_constraint);	
        answer fourier(integer_matrix & input, int *done_constraint, int bb_level);
        answer fourier(integer_matrix & input, int *done_constraint, int number, int * var_used, int num_vars, int bb_level);

	void elim_redundant(integer_matrix & constr,int v,int &num_constr, int vars,
				int first = 0);
	boolean fourier(integer_matrix & constr, int m, int v);
	// the output direction vectors
	dir_list *dl;
	// utilities
	int int_sample(integer_matrix & constr,int n,int *first,int *num2,int,int*, int*, double *, double *);
	boolean int_sample_v(integer_matrix  & constr,int mi,int mf, int v,int *val, double* low, double* high);
	void set_var(integer_matrix & constr, int m, int v, int val);
	int fnormalize(integer_matrix & constr, int m, int n, int var);
	void lr_norm_setg(integer_matrix & constr, int *done_constraint, 
							int_indef **dag);
	int pos_gcd(int a, int b);
	int pos_lcm(int a, int b);
	void set_var(integer_matrix & constr,int var, int_indef val, 
			int *done_constraint,int *ind);
	boolean single_var_constr(integer_matrix & constr, int i);
	boolean done_constraint(int i);
	boolean constraints_left(int *done_constraint, int n);
	int var_leaf(integer_matrix & constr, int var, int *done_constraint);
	int enforce_max(int *ub_valid,int var);  
	int enforce_minmax(int *lb_valid, int *ub_valid,int var);  
public:
	int *sample;	// a sample solution
	// check if constraints violated
	boolean check_bounds(int *lb_valid, int *ub_valid, dir_array *da,
		int *fail=0);
	boolean check_bounds(int *fail=0);
	int num_constr;
	integer_matrix T_constr;  // constraints in terms of t variables
	void set_dep() { indep=0; }
	exact(integer_matrix *S,int m, dir_array *da,boolean
              *const_dist,integer_matrix *Linput, integer_matrix *Uinput,
		int *lb_valid, int *ub_valid,int num_symb,
		int fl=0,int *wu = 0); 
	exact() { indep = 0; flow=0; write_used=0;}
	~exact() { ; }	
	int is_indep() { return indep; };
	dir_list *directions() { return dl; };
};


enum flow_constr{fc_none, fc_max,fc_minmax};

struct dir_array 
{
	int size;
	distance *data;  
	dir_array(int s) { size =s; data = new distance[s]; };
	dir_array(dir_array *d);
	dir_array(distance_vector *dv);
	int base_case();
	int lexpos();
	int level();
	int flow_ok(boolean *is_used);
	int flow_ok(boolean *is_used,flow_constr *fc);
	dir_list *expand(int first=0);
	void print(FILE *fp) {
		fprintf(fp,"(");
		for (int i=0; i<size; i++) {
			data[i].print(fp);
			if (i == size -1) fprintf(fp,")\n");
			else fprintf(fp,",");
		}
	};
	int first_star(); // find the first star, return -1 if none
	~dir_array() { delete data; };
};


// list of direction vectors

struct dir_list
{
	friend class dir_array;
	dir_array *data;
	dir_list *next;
public:
	dir_list(dir_array *d) { this->data = d; next = 0; };
	dir_list() { data = 0; next = 0; }
	dir_list(dvlist *dv);
	dir_list(dir_list *dl) { 
		if (!dl) { 
			data = 0; next = 0; 
		} else {
		    data = new dir_array(dl->data);
		    if (dl->next) {
			next = new dir_list(dl->next);
		    } else {
			next = 0;
		    }
		}
	}
        dir_list *step() { return next; }
	dir_array *get_data() { return data; }
	void flow_reduce(int *is_used);
	void add_unused(boolean *used, int n);
	void expand();
	void push(dir_array *d, dir_list **list); 
	void append(dir_list *dl) {
		dir_list *temp = this;
		while(temp->next) temp=temp->next;
		temp->next=dl;
	}
	dir_array *pop();
	void print(FILE *fp) {
		for (dir_list *temp = this; temp; temp = temp->next) {
			temp->data->print(fp);
		}
	}
/*	~dir_list() { 
		dir_list *temp = this;
		while (temp) {
			dir_list *temp2 = temp->next;
			delete temp;
			temp = temp2;
		}
	}
*/

};

