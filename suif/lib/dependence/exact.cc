/* file "exact.cc" */

/*  Copyright (c) 1994 Stanford University

    All rights reserved.

    This software is provided under the terms described in
    the "suif_copyright.h" include file. */

#include <suif_copyright.h>

#define _MODULE_ "libdependence.a"
#pragma implementation "exact.h"
//
// exact.c
//
// An exact data dependence test 
//
// Take as input a coeff describing all integer solutions to the
//  data dependency equation and an astlist
//  used to get to the access_vectors for the loop bound
//  and a dir_array giving the initial direction vector (ie distances
//       found by GCD)
//  and a boolean array const_dist which says if this element has a
//  	constant distance (we do not need to add direction constrainst to
//  	constant distances)
// Convert these into matrices as used in Wolfe's book
//  then perform Wolfe's programs 2.7, ,2.8 and specialized exact
// tests to see if independent and set direction vectors
//
// 2.8 is slightly different than wolf in the sense that if GCD has
// found a constant distance for a component there is no need to set
// direction constraints on this component
//
// this version uses a hash table to store results of previous tests
//
// numsymbols is number of symbolic variables, no need to do
// direction_vectors for them
//

#include <cstdio>
#include <cmath>
#include <suif1.h>
#include <builder.h>
#include <suifmath.h>
#include "dependence.h"
#include "ehash.h"


// hash table
full_hash_table *table = 0;
struct deleter_t {
~deleter_t() {
	delete table; table = 0;
}
} theoneandonlydeleter;

exact::exact(integer_matrix *S_input,int m,dir_array *da, boolean *const_dist,
		integer_matrix *L_input, integer_matrix *U_input, boolean *lb_valid, 
		boolean *ub_valid,int num_symb, int fl, int *wu)
{
	indep = FALSE;
	DD = m;
	dl = 0;
	num_constr=0;
	write_used = wu;
	flow = fl;

	S.init(S_input);

	//T_constr.init(3*DD+1,DD+1);
	T_constr.init(4*DD+1,DD+1);
	L = L_input;
	U = U_input;

	if (!table)
	    table = new full_hash_table(997);
	if (!table->hit_or_miss
		(&S, L, U,lb_valid,ub_valid,&dl,&indep,num_symb)) {
	    integer_matrix *S2=new integer_matrix; 
	    integer_matrix *L2= new integer_matrix;
	    integer_matrix *U2= new integer_matrix;
	    S2->init(&S); L2->init(L); U2->init(U);

	    // hash miss, call dependence algos
	    enforce_low_limits(lb_valid); 
	    if (is_indep()) {
		dependency_test::one_var_succ++;
		table->add_entry(S2,L2,U2,lb_valid,ub_valid,dl,1,num_symb);
		return;
	    }
	    enforce_high_limits(ub_valid); 
	    if (is_indep()) {
		dependency_test::one_var_succ++;
		table->add_entry(S2,L2,U2,lb_valid,ub_valid,dl,1,num_symb);
		return;
	    }
	    T = new int_indef[DD+1];
  	    if (check_bounds()) {
		set_indep();
  	    } else {
		delete[] T;
		check_directions(da,const_dist,num_symb,lb_valid,ub_valid);
		if (!dl) { // independent but fourier without dv failed
			set_indep();
  			dependency_test::fourier_ok++;
  			dependency_test::fourier_not_ok--;
			if (dependency_test::fourier_not_ok) {
				fprintf(stderr,"Warning, dependence test had to make a conservative assumption\n");
			}
		}
	    }

	    indep = is_indep();
	    dir_list *dl2 = new dir_list(dl);
	    table->add_entry(S2,L2,U2, lb_valid,ub_valid,dl2,indep,num_symb);
	} else {
		delete[] lb_valid;
		delete[] ub_valid;
	}
}


//
// loop through the stars in the direction vector
//
void exact::check_directions(dir_array *da, boolean *const_dist,int num_symb,
				int *lb_valid, int *ub_valid)
{
	int n = da->first_star();
	if (n == -1) {  // no stars left, add this vector to list and leave
		dir_array *da2 = new dir_array(da);
		this->dl->push(da2,&(this->dl));
	} else {
		unenforce_direction(da,const_dist,num_symb);
		da->data[n].set_direction(d_lt);
//fprintf(stderr,"Checking "); da->print(stderr);
		if (!enforce_direction(da,const_dist,num_symb)) {
		    T = new int_indef[DD+1];
		    if (check_bounds(lb_valid,ub_valid,da)) delete T;
		    else {
//fprintf(stderr,"It's dependent \n");
			delete [] T;
			check_directions(da,const_dist,num_symb,
					lb_valid,ub_valid);
		    }
		}
		unenforce_direction(da,const_dist,num_symb);
		da->data[n].set_direction(d_eq);
//fprintf(stderr,"Checking "); da->print(stderr);
		if (!enforce_direction(da,const_dist,num_symb)) {
		    T = new int_indef[DD+1];
  		    if (check_bounds(lb_valid,ub_valid,da)) delete T;
  		    else {
//fprintf(stderr,"It's dependent \n");
			delete [] T;
			check_directions(da,const_dist,num_symb,lb_valid,
						ub_valid);
		    }
		}

		unenforce_direction(da,const_dist,num_symb);
		da->data[n].set_direction(d_gt);
//fprintf(stderr,"Checking "); da->print(stderr);
		if (!enforce_direction(da,const_dist,num_symb)) {
		    T = new int_indef[DD+1];
  		    if (check_bounds(lb_valid,ub_valid,da)) delete T;
  		    else {
//fprintf(stderr,"It's dependent \n");
			delete [] T;
			check_directions(da,const_dist,num_symb,lb_valid,
						ub_valid);
		    }
		}
		unenforce_direction(da,const_dist,num_symb);
		da->data[n].set_direction(d_star);
		enforce_direction(da,const_dist,num_symb);
	}
}




//
// Program 2.7 from Wolfe
// separated into lower and upper so that one can deal with an unknow
// lower bound and known upper bound or vice versa
//

void exact::enforce_low_limits(int *lb_valid) 
{
	int *V = new int[DD+1];for (int i=0; i<DD+1; i++) V[i] = 0; 

	int row  = 1;
	for (int k=1; k<=DD; k++) {
	    	for (int l=1; l <= lb_valid[k]; l++,row++) {
		    V[0] = S[k][0] - (*L)[row][0];
		    for (int m=1; m<= k-1; m++) {
		    	    V[0] = V[0] - (*L)[row][m]*S[m][0];;
		    }
		    for (int n=1; n<= k-1; n++) {
			    V[n] = S[k][n];
			    for (int m=n; m<= k-1; m++) {
				    V[n] = V[n] - (*L)[row][m]*S[m][n];
			    }
		    }
		    V[k] = S[k][k];
		    if (addbound(V,k,1)) {
			set_indep();
		     }
		}
	}
	delete[] V;
}

void exact::enforce_high_limits (int *ub_valid) 
{
	int *W = new int[DD+1]; 
	int i;
        for (i=0; i<DD+1; i++) W[i] = 0;

	i=1;
	for (int k=1; k<=DD; k++) {
		for (int l=1; l<= ub_valid[k]; l++,i++) {
		    W[0] = S[k][0] - (*U)[i][0];
		    for (int m=1; m<= k-1; m++) {
			    W[0] = W[0] - (*U)[i][m]*S[m][0];;
		    }
		    for (int n=1; n<= k-1; n++) {
			    W[n] = S[k][n];
			    for (int m=n; m<= k-1; m++) {
				    W[n] = W[n] - (*U)[i][m]*S[m][n];
			    }
		    }
		    W[k] = S[k][k];
		    if (addbound(W,k,-1)) {
			set_indep();
		    }
		}
	}
	delete[] W;
}


//
// 2.8 from Wolfe 
// adds in the restrictions caused by direction vector
// returns 1 if addbound causes an independency
//
int exact::enforce_direction(dir_array *da, boolean *const_dist,int num_symb) 
{
	int retvalue = 0;
	int *V = new int[DD+1]; for (int i=0; i<DD+1; i++) V[i] = 0;
	assert((DD-num_symb)%2 == 0);
	int D = (DD-num_symb)/2;
	for (int m=1; m<=D; m++) {
	    if (!const_dist[m-1]) {
		V[0] = S[2*m-1+num_symb][0] - S[2*m+num_symb][0];
		for (int n=1; n<= 2*m-1+num_symb; n++) {
			V[n] = S[2*m-1+num_symb][n] - S[2*m+num_symb][n];
		}
		V[2*m+num_symb] = -S[2*m+num_symb][2*m+num_symb];
		switch(da->data[m-1].dir()) {
		case d_lt:
			V[0]--;
			if (addbound(V,2*m+num_symb,1)) retvalue = 1;
			break;
		case d_eq:
			if (addbound(V,2*m+num_symb,1)) retvalue = 1;
			if (addbound(V,2*m+num_symb,-1)) retvalue = 1;
			break;
		case d_gt:
			V[0]++;
			if (addbound(V,2*m+num_symb,-1)) retvalue = 1;
			break;
		case d_star: break;
		default: assert(0);
			 break;
		}
	    }
	}
	if (retvalue) {
	    dependency_test::one_var_succ++;
	}
	delete[] V;
	return retvalue;
}

//
// undo the effects of enforce directions
//
void exact::unenforce_direction(dir_array *da, boolean *const_dist,int num_symb) 
{
	int *V = new int[DD+1]; for (int i=0; i<DD+1; i++) V[i] = 0;
	assert((DD-num_symb)%2 == 0);
	int D = (DD-num_symb)/2;
	for (int m=D; m>=1; m--) {
	    if (!const_dist[m-1]) {
		V[0] = S[2*m+num_symb][0] - S[2*m-1+num_symb][0];
		for (int n=1; n<= 2*m-1; n++) {
			V[n] = S[2*m+num_symb][n] - S[2*m-1+num_symb][n];
		}
		V[2*m+num_symb] = S[2*m+num_symb][2*m+num_symb];
		switch(da->data[m-1].dir()) {
		case d_lt:
			V[0]--;
			delbound(V,2*m+num_symb,1);
			break;
		case d_eq:
			delbound(V,2*m+num_symb,1);
			delbound(V,2*m+num_symb,-1);
			break;
		case d_gt:
			V[0]++;
			delbound(V,2*m+num_symb,-1);
			break;
		case d_star: break;
		default: assert(0);
			 break;
		}
	    }
	}
	delete[] V;
}




//
// adds a constraint to the T_constr matrix
// constraints of the form sum of j = 0 to n T_const[i][j] * tj >= 0
// returns 1 if independent (in addition to setting variables)
//
int exact::addbound(int *V, int k, int x) 
{
	int l = k;
	while ((V[l] == 0) && (l > 0)) {
		l--;
	}
	if (!l) {
		if (x*V[0] < 0) {
			return 1;	
		}
	} else {
		assert(num_constr < T_constr.m());
		for (int m=0; m<= l; m++) {
			T_constr[num_constr][m] = x*V[m];
		}
		num_constr++;
	}
	return 0;
}

//
// Undo the effects of a previous addbound
//
void exact::delbound(int *V, int k, int x) 
{
	int l = k;
	while ((V[l] == 0) && (l > 0)) {
		l--;
	}
	if (!l) {
		if (x*V[0] < 0) {
			return;
		}
	} else {
		num_constr--;
		for (int m=0; m<= l; m++) {
			T_constr[num_constr][m] = 0;
		}
	}
}


//  if flow is true, throw out directions which can't be a flow vector
boolean exact::check_bounds(int *lb_valid, int *ub_valid,dir_array *da,
					int *fail)
{
	if (!flow) return check_bounds(fail);
	if (!da->base_case()) return check_bounds(fail);
	if (!da->lexpos()) return check_bounds(fail);

	flow_constr *fc = new flow_constr[da->size];
	for (int i=0; i<da->size; i++) fc[i] = fc_none;
	int nc = num_constr;
	if (da->flow_ok(write_used,fc)) {
	        int i;
		for (i=0; i<da->size; i++) {
			if (fc[i] == fc_max) {
				if (enforce_max(ub_valid,i)) return 1;
			} else if (fc[i] == fc_minmax) {
				if (enforce_minmax(lb_valid,ub_valid,i)) 
					return 1;
			}
		}
		int ans = check_bounds(fail);
		for (i=nc; i<num_constr; i++) {
			for (int a=0; a<T_constr.n(); a++) {
				T_constr[i][a] = 0;
			}
		}
		num_constr = nc;
		return ans;
	}
	
	if (fail) *fail = 0;
	return 1;
}


//
// Program 2.10
// returns 1 if independent
// if fail exists, sets it to one on a failure case
//
boolean exact::check_bounds(int *fail)
{
//fprintf(stderr,"Check bounds: Start\n");
  dependency_test::num_check_bounds++;

    if (fail) *fail = 0;
    answer a;
    if (num_constr ==0) {
        dependency_test::one_var_succ++;
        return 0;
    }
    int *done_constraint = new int[num_constr];  // constr no longer needed
    
//fprintf(stderr,"Check bounds: 1\n");
    
    int ind = 0;
    set_low_up(T_constr,&ind);
    if (ind) {
        delete[] done_constraint;
        return 1;
    }

//fprintf(stderr,"Check bounds: 2\n");    
    // set all constraints with <= 1 variable to done
    // all their info is represented by low and up
    for (int i=0; i<num_constr; i++) {
        done_constraint[i] = single_var_constr(T_constr,i);
    }
    
    if (single_var_ok(T_constr)) {
        boolean result = single_var_check();
//fprintf(stderr,"Check bounds: 3\n");    
        delete[] done_constraint;
        return result;
    } else {
        T_constr2.init(&T_constr);  // make a copy of constraints;
        a = cycle_test(T_constr2,done_constraint);
        if (a == independent) {
            dependency_test::cycle_ok++;
//fprintf(stderr,"Check bounds: 4\n");    
            delete[] done_constraint;
            return 1;
        } else if (a == dependent) {
            dependency_test::cycle_ok++;
//fprintf(stderr,"Check bounds: 5\n");    
            delete[] done_constraint;
            return 0;
        } else {  // cycle test doesn't apply
            dependency_test::cycle_not_ok++;
            if (lr_test_ok(T_constr2,done_constraint)) {
                dependency_test::lr_ok++;
                boolean result = 
                    lr_test(T_constr2,done_constraint);
//fprintf(stderr,"Check bounds: 6\n");    
                delete[] done_constraint;
                return result;
            } else {
                dependency_test::lr_not_ok++;
                a=fourier(T_constr2,done_constraint);
                if (a == independent) {
                    dependency_test::fourier_ok++;
//fprintf(stderr,"Check bounds: 7\n");    
                    delete[] done_constraint;
                    return 1;
                } else if (a == dependent) {
                    dependency_test::fourier_ok++;
//fprintf(stderr,"Check bounds: 8\n");    
                    delete[] done_constraint;
                    return 0;
                } else {
                    dependency_test::fourier_not_ok++;
                    if (fail) *fail = 1;
//fprintf(stderr,"Check bounds: 9\n");    
                    delete[] done_constraint;
                    return 0;
                }
            }
        }
    }	
}



// are there at most one variable per equation
boolean exact::single_var_ok(integer_matrix & constr)
{
	for (int i=0; i<num_constr; i++) {
		if (!single_var_constr(constr,i)) {
			dependency_test::one_var_fail++;
			return 0;
		}
	}
	dependency_test::one_var_succ++;
	return 1;
}


// apply the single variable test
// returns 1 if independent
boolean exact::single_var_check()
{
	for (int j=1; j<T_constr.n(); j++) {
		if(low[j] > up[j]) {
			return 1;
		}
	}
	return 0;
}


// apply the cyclical test
// ie set each variable which is not both less than a ti and greater than
// tj to the appropriate endpoint
// done_constraint is one for each constraint which no longer needs to
//   be considered
answer exact::cycle_test(integer_matrix & constr, int *done_constraint)
{
	int ind=0;
	// cycle through the variables searching for a leaf, set the
	// leaf and start again
	int sign=0;
	while (1) {
	    int j;
	    for (j=1; j<constr.n() && 
		          !(sign=var_leaf(constr,j,done_constraint)); j++) {
	    }

	    if (sign > 0) {
		set_var(constr,j,up[j],done_constraint,&ind);
		if (ind) return independent;
	    } else if (sign < 0) {
		set_var(constr,j,low[j],done_constraint,&ind);
		if (ind) return independent;
	    } else if (constraints_left(done_constraint,num_constr)) {
			if (single_var_check()) return independent;
			else return unsure;  // cycle test does not apply 
	    } else if (single_var_check()) {
		return independent;
	    } else {
		return dependent;
	    }
	}
}


// does the loop residue test apply
boolean exact::lr_test_ok(integer_matrix & constr, int *done_constraint)
{
	for (int i=0; i<num_constr; i++) {
	    if (!done_constraint[i]) {
		int num_vars = 0;
		int multiplier = 0;
		for (int j=1 ; j<constr.n(); j++) {
		    if (constr[i][j]) {
			if (num_vars == 0) {
				num_vars++;
				multiplier = constr[i][j];
			} else if (num_vars == 1) {
				if (constr[i][j] != -multiplier) {
					return 0;
				}
				num_vars++;
			} else {
				return 0;  // too many variables
			}
		     }
		}
		assert(num_vars == 2);
	    }
	}
	return 1;
}

// do the loop residue test
// dag is a two dimensional matrix st if i-j >= a then dag[i][j] = a;
// if there is no edge then dag[i][j] = indef
// return 1 if independent
boolean exact::lr_test(integer_matrix & constr, int *done_constraint)
{
	typedef int_indef *ip;
	int_indef **dag = new ip[constr.n()];
	int i;
	for (i=0; i<constr.n(); i++) dag[i] = new int_indef[constr.n()];

	lr_norm_setg(constr,done_constraint,dag);
	boolean ret = lr_test(dag,constr.n());

        for (i=0; i<constr.n(); i++) delete[] dag[i];
        delete[] dag;
        return ret;
}


// return 1 if the loop residue test applied to the dag immplies
// independent
boolean exact::lr_test(int_indef **dag, int n)
{
	for (int first=0; first<n; first++) { 
		// check for cycles starting with first
		if (lr_test(dag,n,first)) {
			return 1;
		}
	}
	return 0;
}


// return 1 if there is a cycle starting at first which violates the
// constraints
boolean exact::lr_test(int_indef **dag, int n, int first)
{
    int *onpath = new int[n];
    for (int i=0; i<n; i++) onpath[i] = 0;
    boolean ret = lr_test(dag,n,first,first,0,onpath);
    delete[] onpath;
    return ret;
}

// visit node "node"
boolean exact::lr_test(int_indef **dag, int n, int first, int node, int cost,
			int *onpath)
{
	onpath[node] = 1;

	for (int i=0; i<n; i++) {
	    if (!dag[node][i].is_indef) {
		if ((i==first) || !onpath[i]) {
			if (i == first) { // a cycle
				if ((cost+dag[node][first]) > 0) {
					return 1;
				}
			} else {
			    if (lr_test(dag,n,first,i,cost+dag[node][i].val, 
							onpath)) {
					return 1;
			    }
			}
		}
	    }
	}
	onpath[node] = 0;
	return 0;
}

int set_fourier_var = 0;
static int not_first_time_fourier = 0;
static integer_matrix *four_constr = 0, *four_constr2 = 0, *four_bb = 0;

const int maxnumber = 4000;
const int maxnumber2 = 8000;
const int maxnumberbb = 1024;

#define DO3LIMIT        10
#define LIMITSZ         3

#define DEBUG
#undef DEBUG


// do the fourier motzkin test 
// return 1 if independent


answer exact::fourier(integer_matrix & input, int *done_constraint)
{
//    fprintf(stderr,"==================================================\n");
    return fourier(input, done_constraint, 0);
}

answer exact::fourier(integer_matrix & input, int *done_constraint, int bb_level)
{
    set_fourier_var = 0;
    if(bb_level == 0) {
      dependency_test::num_first_call_fourier++;
      dependency_test::num_in_ineqs += input.m();
      dependency_test::max_in_ineqs = MAX(dependency_test::max_in_ineqs, input.m());
    }
    dependency_test::num_call_fourier++;
    dependency_test::max_bb_lev = MAX(dependency_test::max_bb_lev, bb_level);


    
    // create an array of fractions
    assert(maxnumber/50 > input.n());
    if (!not_first_time_fourier) {
	four_constr = new integer_matrix(maxnumber,maxnumber/50);
    }
    
    // have second array to store old constraints, this is used to
    // come up with a sample solution, if sample solution is integer
    // we know that there is an integer solution
    assert(maxnumber2/50 > input.n());
    if(!not_first_time_fourier) {
        four_constr2 = new integer_matrix(maxnumber2, maxnumber2/50);
        four_bb = new integer_matrix(maxnumberbb, maxnumber2/50);
    }
    not_first_time_fourier = 1;

    // move the constraint matrix to the fourier matrix
    // compacting it using done_constraints
    int * var_used = new int[input.n()];
    int i;
    for (i=0; i<input.n(); i++) var_used[i] = 0;
    int number = 0;
    for (i = 0; i<num_constr; i++) {
        if (!done_constraint[i]) {
            assert(number < maxnumber);
            for (int j=0; j<input.n(); j++) {
                (*four_constr)[number][j] = input[i][j];
                if (input[i][j]) var_used[j] = 1;
            }
            number++;
        }
    }

    // how many variables actually used
    int num_vars=0;
    for (i=1; i<input.n(); i++) {
        if (var_used[i]) num_vars++;
    }
    assert(num_vars > 1);  // otherwise other tests would have solved

    // add the upper and lower bounds to constr 
    for (i=0; i<input.n(); i++) {
        if (var_used[i]) {
            if (!low[i].is_indef) {
                assert(number < maxnumber);
                (*four_constr)[number][0] = -low[i].val;
                for (int j=1; j<input.n(); j++) {
                    if (j == i) (*four_constr)[number][j] = 1;
                    else (*four_constr)[number][j] = 0;
                }
                number++;
            }
            if (!up[i].is_indef) {
                assert(number < maxnumber);
                (*four_constr)[number][0] = up[i].val;
                for (int j=1; j<input.n(); j++) {
                    if (j == i) (*four_constr)[number][j] = -1;
                    else (*four_constr)[number][j] = 0;
                }
                number++;
            }
        }
    }
    
    for(int bb=0; bb<bb_level; bb++) {
        assert(number < maxnumber);
        for (i=0; i<input.n() ; i++) 
            (*four_constr)[number][i] = (*four_bb)[bb][i];
        number++;
    }
            
    answer reply =  fourier(input, done_constraint, number, var_used, num_vars, bb_level);

    delete var_used;
    if(reply) dependency_test::reply_true++;
    return reply;
}


answer exact::fourier(integer_matrix & input, int *done_constraint, int number, int * var_used, int num_vars, int bb_level)
{
    int nn = input.n();
//fprintf(stderr,"%d-------fourier()-------------------- input=\n", bb_level);
//input.print(stderr);
    
    int number2=0;	// next entry of constr2 to be used
    int * first = new int[nn];
    for (int ii=0; ii<nn; ii++) first[ii] = 0;
    int * num2 = new int[nn];
    int i;
    for (i=0; i<nn ; i++) num2[i] = 0;
    
//    fprintf(stderr,"four_constr=\n");
//    for(i=0; i < number; i++) {
//        for(ii=0; ii<nn; ii++)
//            fprintf(stderr,"%3d", (*four_constr)[i][ii]);
//        fprintf(stderr,"\n");
//    }
    
    answer reply;
    // do the test
    void eq_sub(integer_matrix & input,int &number,int n); 
    // BUGBUG while loop put to see if this will not fail - saman - 8/26/92
    while(1) {
        int oldnum = number;
        eq_sub(*four_constr,number, nn);
        if(oldnum == number) break;
    }
    
    int last_v=0;
    int oldnumber=0;
    for (int v=1; num_vars>1; v++)  {  // eliminate var v
        int min_number = number;
        if (var_used[v]) {
            num_vars--;
            elim_redundant(*four_constr,v,number,nn);

            if (fnormalize(*four_constr,number,nn,v)) {
                //                delete var_used;
                delete first;
                delete num2;
                return independent;
            }

            
            if (number < min_number) min_number = number;
            for (int plus=0; plus<min_number; plus++) { 
                if ((*four_constr)[plus][v] > 0 ) {
                    if (plus % 10 == 0 || number > maxnumber/4) {
                        if (number>3*maxnumber/4 || oldnumber-2 <= plus) {
                            elim_redundant(*four_constr,v,number,nn,
                                           plus+1);
                        } else if (oldnumber -2< number) {
                            elim_redundant(*four_constr,v,number,nn,
                                           oldnumber-2);
                        }
                        oldnumber = number;
                    }  
                    // copy pos into constr2
                    if (num2[v]==0) {
                        first[v]=number2;
                    }
                    num2[v]++;
                    assert(number2 < maxnumber2);
                    for (i=0; i<nn; i++) {
                        (*four_constr2)[number2][i] = 
                            (*four_constr)[plus][i];
                    }
                    number2++;
                    
                    // compare this positive against all negatives
                    if (number < min_number) min_number = number;
                    for (int minus=0; minus<min_number; minus++) {
                        if ((*four_constr)[minus][v] < 0) {	

                            int gcd = pos_gcd((*four_constr)[plus][v],
					      (*four_constr)[minus][v]);
                            int plus_mult = (*four_constr)[plus][v]/gcd;
                            int minus_mult = -(*four_constr)[minus][v]/gcd;
                            assert(number < maxnumber);
                            (*four_constr)[number][0] = 
                                minus_mult*(*four_constr)[plus][0] +
                                    plus_mult*(*four_constr)[minus][0];
                            int null_constr = 1; // does this have vars
			    int i;
                            for (i=1; i<v; i++) (*four_constr)[number][i] = 0;
                            for (i=v; i<nn; i++) {
                                (*four_constr)[number][i] = 
                                    minus_mult*(*four_constr)[plus][i] +
                                        plus_mult*(*four_constr)[minus][i]; 
                                if ((*four_constr)[number][i] != 0) 
                                    null_constr=0;
                            }
                            if (null_constr) {
                                if ((*four_constr)[number][0] < 0) {
                                    //                                    delete var_used;
                                    delete first;
                                    delete num2;
                                    return independent;
                                }
                            } else {
                                number++;
                            }
                        }
                        if (number < min_number) min_number = number;
                    }
                    number--;
                    // copy over the plus constraint
                    if (plus != number) {
			int i;
                        for (i=0; i<nn; i++) {
                            (*four_constr)[plus][i] = (*four_constr)[number][i];
                        }
                    }
                    if ((*four_constr)[plus][v] > 0) plus--;
                }
                if (number < min_number) min_number = number;
            }

 
            // move all negative constraints from constr to constr2
            for (int minus=0; minus<number; minus++) {
                if ((*four_constr)[minus][v] < 0) {
                    // copy neg into constr2
                    assert(number2 < maxnumber2);
                    if (num2[v]==0) {
                        first[v]=number2;
                    }
                    num2[v]++;
                    for (i=0; i<nn; i++) {
                        (*four_constr2)[number2][i] = 
                            (*four_constr)[minus][i];
                    }
                    number2++;
                    
                    // compact over value in constr
                    number--;
                    if (minus != number) {
			int i;
                        for (i=0; i<nn; i++) {
                            (*four_constr)[minus][i]=(*four_constr)[number][i];
                        }
                    }
                    if ((*four_constr)[minus][v] < 0) minus--;
                }
            }
	    last_v = v;
        }
    }

    last_v++;
    while(!var_used[last_v]) last_v++;
    // last_v is last variable left to solve for
    
    
    // place last variable into constr2;
    for (int last=0; last<number; last++) { 
        if ((*four_constr)[last][last_v] != 0) {
            assert(number2 < maxnumber2);
            if (num2[last_v]==0) {
                first[last_v]=number2;
            }
            num2[last_v]++;
            for (i=0; i<nn; i++) {
                (*four_constr2)[number2][i] = (*four_constr)[last][i];
            }
            number2++;
        }
    }
    
    
    
    // check if last variable consistent
    int no_soln = fourier(*four_constr,number,last_v);
    
    if (no_soln) {
        reply = independent;
    } else {
        int varNum;
        double DlowVal, DhighVal;
        if (int_sample(*four_constr2,nn,first,num2,number2, var_used, &varNum, &DlowVal, &DhighVal)) { 
            reply = dependent;
        } else if (!set_fourier_var) {
            reply = independent;
        } else {
            int lowV1=0, highV1=0;
            int highV2, lowV3;

            if(DhighVal - DlowVal > DO3LIMIT) {
/*
                double Dv1 = (LIMITSZ*DlowVal + DhighVal)/(LIMITSZ+1.0);
                double Dv2 = (DlowVal + LIMITSZ*DhighVal)/(LIMITSZ+1.0);
                highV2 = (int)floor(Dv1);
                lowV1  = (int)ceil(Dv1);
                highV1 = (int)floor(Dv2);
                lowV3 =  (int)ceil(Dv2);
*/
                double Dv = (DlowVal + DhighVal)/2.0;
                highV2 = (int)floor(DlowVal+1);
                lowV1  = (int)ceil(Dv);
                highV1 = (int)floor(Dv);
                lowV3 =  (int)ceil(DhighVal-1);
            } else {
                double Dv = (DlowVal + DhighVal)/2.0;
                highV2 = (int)floor(Dv);
                lowV3 = (int)ceil(Dv);
            }
#ifdef DEBUG
            fprintf(stderr,"%8.2f %8.2f ", DlowVal, DhighVal);
            for(int bb=0; bb<bb_level; bb++) fprintf(stderr,"   ");
            fprintf(stderr,"%d[%d %d]", varNum, (int)ceil(DlowVal), (int)floor(DhighVal));
            if(DhighVal - DlowVal > DO3LIMIT) 
                fprintf(stderr,"   (- %d) (%d %d) (%d -)\n", highV2, lowV1, highV1, lowV3);
            else
                fprintf(stderr,"   (- %d) (%d -)\n", highV2, lowV3);
            fprintf(stderr,"                  ");
            for(bb=0; bb<bb_level; bb++) fprintf(stderr,"   ");
            fprintf(stderr,"{\n");
#endif
            if(bb_level+1 >=  maxnumberbb) {
                delete first;
                delete num2;
                return unsure;
            }
            /* Do the 1st check */
            for (i=0; i<nn ; i++) 
                (*four_bb)[bb_level][i] = 0;
            (*four_bb)[bb_level][0] = highV2;
            (*four_bb)[bb_level][varNum] = -1;
            reply = fourier(input, done_constraint, bb_level+1);
            
#ifdef DEBUG
            fprintf(stderr,"                  ");
            for(bb=0; bb<bb_level; bb++) fprintf(stderr,"   ");
            fprintf(stderr,"} %d\n", reply);
#endif
            
            if(reply == independent) {
#ifdef DEBUG
                fprintf(stderr,"                  ");
                for(bb=0; bb<bb_level; bb++) fprintf(stderr,"   ");
                fprintf(stderr,"{\n");
#endif
                /* Do the 2nd check */
                (*four_bb)[bb_level][0] = -lowV3;
                (*four_bb)[bb_level][varNum] = 1;
                
                reply = fourier(input, done_constraint, bb_level+1);
                
#ifdef DEBUG
                fprintf(stderr,"                  ");
                for(bb=0; bb<bb_level; bb++) fprintf(stderr,"   ");
                fprintf(stderr,"} %d\n", reply);
#endif
                if(DhighVal - DlowVal > DO3LIMIT) {
                    if(reply == independent) {
                        if(bb_level+2 >= maxnumberbb) {
                            delete first;
                            delete num2;
                            return unsure;
                        }
             
                        /* Do the 3rd check (only done when range is large) */
                        for (i=0; i<nn ; i++) {
                            (*four_bb)[bb_level][i] = 0;
                            (*four_bb)[bb_level+1][i] = 0;
                        }
                        (*four_bb)[bb_level][0] = highV1;
                        (*four_bb)[bb_level][varNum] = -1;
                        (*four_bb)[bb_level+1][0] = -lowV1;
                        (*four_bb)[bb_level+1][varNum] = 1;
                        reply = fourier(input, done_constraint, bb_level+2);
#ifdef DEBUG
                        fprintf(stderr,"                  ");
                        for(bb=0; bb<bb_level; bb++) fprintf(stderr,"   ");
                        fprintf(stderr,"} %d\n", reply);
#endif
                    }                
                } 
            }
        }
    }
    
    
    // free up storage
    delete first;
    delete num2;
    
    return reply;
}


// eliminate any redundant constrnts from the constraint matrix
// look only at constraints >= first
void exact::elim_redundant(integer_matrix & constr, int v,int &number,int vars,
			   int first)
{
    int *redun =  new int[number];
    int i;
    for (i=0; i<number; i++) redun[i] = 0;
    
    
	// find the redundant constraints
	for (i=first; i<number; i++) {
	    if (!redun[i]) {
		for (int j=i+1; j<number; j++) {
		    if (!redun[j]) { 
			redun[j] = 1;
			int has_multiple = 0;
			fract mult=1;
			for (int k=v; k<vars && redun[j]; k++) {
			    if (constr[i][k] == 0) {
				if (constr[j][k] != 0) redun[j] = 0;
			    } else if (constr[j][k] == 0) {
				if (constr[i][k] != 0) redun[j] = 0;
			    } else {
				if (has_multiple) {
				    if (fract(constr[i][k])/
				        fract(constr[j][k]) != mult) {
					    redun[j] = 0;
				    }
				} else {
			            has_multiple = 1;
			    	    mult = fract(constr[i][k])/
					       fract(constr[j][k]);
				    if (mult < 0) redun[j] = 0;
			   	}
			    }
			}
			if (redun[j]) {
			    if(fract(constr[j][0])*mult< fract(constr[i][0])) {
				// tighter constraint
				fract f = fract(constr[j][0]) * mult;
				int rn(fract f);
				constr[i][0] = rn(f);
			    }
		      }
		    }
		}
	    }
	}

	// eliminate the redundant constraints
	// basically use an insertion sort
	int i2=0;
	i=0;
	while (i<number) {
		while(i<number && redun[i]) i++;
		if (i < number) {
		    // swap
		    constr.swap(i2,i);
		    i++;
		    i2++;
		}
	}

	number = i2;
	delete redun; 
}


// is the system of equations which now only uses variable v consistent
// return 1 if independent
boolean exact::fourier(integer_matrix & constr, int m, int v) 
{
	int l = 0;
	int u = 0;
	int have_low =0;
	int have_up = 0;
	for (int i=0; i<m; i++) {
		if (constr[i][v] == 0) {
                        if (constr[i][0] < 0) return 1;
		} else if (constr[i][v] > 0) {
			int temp = 0;
			if (constr[i][0] % constr[i][v] == 0) {
				temp = -constr[i][0] / constr[i][v];
			} else if (constr[i][0] > 0) {
				temp = - (constr[i][0] / constr[i][v]);
			} else {
				temp = (-constr[i][0]) / constr[i][v] + 1;
			}
			if (!have_low || (temp >l) ) {
				have_low =1;
				l = temp;
			} 
		} else if (constr[i][v] < 0) {
			int temp = 0;
			if (constr[i][0] % constr[i][v] == 0) {
				temp = constr[i][0] / (-constr[i][v]);
			} else if (constr[i][0] > 0) {
				temp =  constr[i][0] / (-constr[i][v]);
			} else {
				temp =  -((-constr[i][0]) / (-constr[i][v]))-1;
			}
			if (!have_up || (temp <u) ) {
				have_up =1;
				u = temp;
			} 
		}
		if (have_low && have_up && (l > u)) return 1;
	}
	return 0;
}



//
// UTILITIES USED BY EXACT TESTS
//


// find a sample solution to the constraints and return
// 1 if it is an integer
//  used by fourier-motzkin
int exact::int_sample(integer_matrix & constr, int n, int *first, int *num2,
			int number2, int *var_used, int * vnum, double * lv, double * hv)
{
	int val;

	for (int var=n-1; var >= 1; var--) { // find a val for var
	    if (num2[var] > 0) {
		int last=first[var]+num2[var]-1;
		if (!int_sample_v(constr,first[var],last, var,&val, lv, hv)) {
                    *vnum = var;
                    return 0;
		} else {
			set_var(constr,first[var],var,val);
		}
	    } else if (var_used[var]) {
		set_var(constr,number2,var,0);
	    }
	}
	return 1;
}

// set var to val
void exact::set_var(integer_matrix & constr, int m, int v, int val)
{
	set_fourier_var = 1;
	for (int i=0; i<m; i++) {
		constr[i][0] = constr[i][0] + val*constr[i][v];
		constr[i][v]=0;
	}
}


//
// give a sample value for variable v that meets constraints mi to mf
// return 0 if none exists
//
boolean exact::int_sample_v(integer_matrix & constr, int mi, int mf, int v, int *val, double * low, double * high) 
{
	int l=0;
	int u=0;
	int have_low =0;
	int have_up = 0;
        double Dl, Du; Dl = Du = -1234.0;
	for (int i=mi; i<=mf; i++) {
		if (constr[i][v] == 0) {
			if (constr[i][0] < 0) {
                          assert(0); 
                        }
		} else if (constr[i][v] > 0) {
			int temp = 0;
                        
			if (constr[i][0] % constr[i][v] == 0) {
				temp = -constr[i][0] / constr[i][v];
			} else if (constr[i][0] > 0) {
				temp = - (constr[i][0] / constr[i][v]);
			} else {
				temp = (-constr[i][0]) / constr[i][v] + 1;
			}

                        double Dtemp;
                        Dtemp = -(double)constr[i][0] / (double)constr[i][v];
			if (!have_low || (Dtemp >Dl) ) 
                            Dl = Dtemp;


			if (!have_low || (temp >l) ) {
				have_low =1;
				l = temp;
			} 
		} else if (constr[i][v] < 0) {
			int temp = 0;
			if (constr[i][0] % constr[i][v] == 0) {
				temp = constr[i][0] / (-constr[i][v]);
			} else if (constr[i][0] > 0) {
				temp =  constr[i][0] / (-constr[i][v]);
			} else {
				temp =  -((-constr[i][0]) / (-constr[i][v]))-1;
			}

                        double Dtemp;
                        Dtemp = (double)constr[i][0] / -(double)constr[i][v];
			if (!have_up || (Dtemp <Du) ) 
                            Du = Dtemp;

			if (!have_up || (temp <u) ) {
				have_up =1;
				u = temp;
			} 
		}
	}
	if (!have_low && !have_up) {
		*val = 0;
		return 1;
	} else if (!have_low) {
		*val = u-1000;
	} else if (!have_up) {
		*val = l+1000;
	} else {
		*val = (u+l)/2;
		if ((*val < l) || (*val > u)) {
                    *low = Dl;
                    *high = Du;
                    return 0;
                }
	}
	return 1;
}


// round a fraction down towards negative infinite
int rn(fract f)
{
	int n = f.num();
	int d = f.denom();
	if (n<0) n= -n;
	if (d<0) d= -d;

	if (f >=  0) {
		return (n / d);
	} else {
            if ((n % d) == 0) return -(n/d);
            return (-(n/d) - 1);
        }
}
		
// round a fraction up towards infinite
int rp(fract f)
{
	int n = f.num();
	int d = f.denom();
	if (n<0) n= -n;
	if (d<0) d= -d;

	if (f > 0) {
		if ((n % d) == 0) return n/d;
		return (n / d)+1;
	} else return -(n/d);
}



//
// UTILITIES USED BY EXACT TESTS

inline int int_abs(int x) { return x >= 0 ? x : -x; }

// to help prevent overflow first divide each row by its gcd
// used by fourier
// return 1 if independent
int exact::fnormalize(integer_matrix & constr,int m, int n, int /* var */)
{
	for (int i=0; i<m; i++) {
		int gcd=int_abs(constr[i][1]);
		int j;
		for (j=2; j<n && (gcd!=1); j++) {
			if (constr[i][j]) {
				gcd = pos_gcd(gcd,constr[i][j]);
			}
		}

		if (!gcd) {
			if (constr[i][0] < 0) return 1;
			else return 0;
		} else if (gcd!=1) {
		    for (j=1; j<n; j++) {
			if (constr[i][j]) constr[i][j] /= gcd;
		    }

		    if (constr[i][0]) {
		        if (constr[i][0] % gcd == 0) {
			    constr[i][0] /= gcd;
		        } else if (constr[i][0] > 0) {
			    constr[i][0] = constr[i][0] / gcd ;
		        } else if (constr[i][0] < 0) {
			    constr[i][0] = - (-constr[i][0] / gcd + 1);
		        }
		    }
		}
	}
	return 0;
}


int exact::pos_lcm(int a, int b)
{
	return int_abs(a * b/pos_gcd(a,b));
}

int exact::pos_gcd(int a,int b)
{
	a = int_abs(a);
	b = int_abs(b);
	if (!b) return a;
	if (!a) return b;

	if (b>a) {
		int tmp = a;
		a = b;
		b = tmp;
	}

	while (a % b != 0) {
		int tmp = b;
		b = a % b;
		a = tmp;
	}
	return b;
}

// normalize each constraint so that multipliers equal 1
// and fill in the digraph
// this assumes each constrain is of the form a*ti - a*tj = b;
void exact::lr_norm_setg(integer_matrix & constr, int *done_constraint, 
							int_indef **dag)
{
	for (int i=0; i<num_constr; i++) {
	    if (!done_constraint[i]) {
		int first_var = 1;
		int first_pos = 0;
		int multiplier = 0;
		for (int j=1 ; j<constr.n(); j++) {
		    if (constr[i][j]) {
			if (first_var) {
				multiplier = constr[i][j];
				first_pos = j;
				first_var = 0;
			} else {
			    if ((multiplier != 1)  && (multiplier != -1)) {
				if (multiplier > 0) {
					constr[i][first_pos] = 1;
					constr[i][j] = -1;
				} else {
					constr[i][j] = 1;
					constr[i][first_pos] = -1;
					multiplier = -multiplier;
				}
				if (constr[i][0] > 0) {
				    constr[i][0] = constr[i][0] / multiplier;
				} else {
				    int c = -constr[i][0];
				    constr[i][0]= -((c-1)/multiplier + 1);
				}
			    }
			    if (constr[i][first_pos] == 1) {
			    	dag[first_pos][j] = MAX(-constr[i][0],
							dag[first_pos][j]);
			    } else {
				dag[j][first_pos] = MAX(-constr[i][0],
							dag[j][first_pos]);
			    }
			}
		    }
		}
	    }
	}

	// set 0 row and collumn of dag
	{
	    int i;
	    for (i=0; i<constr.n(); i++) {
		dag[0][i] = 0-up[i];
		dag[i][0] = low[i];
	    }
	}
}

// set  variable v in constr to val
// update low, up and done_constraint as needed
void exact::set_var(integer_matrix & constr, int var, int_indef val, 
						int *done_constraint,int *ind)
{
	for (int i=0; i< num_constr; i++) {
		if (constr[i][var]) {
		    if (val.is_indef) { // constraint meaningless
			for (int j=0; j<constr.n(); j++) {
				constr[i][j] = 0;
			}
			done_constraint[i] = 1;
		    } else {
			constr[i][0] = constr[i][0] + constr[i][var]*val.val;
			constr[i][var] = 0;
			set_low_up(constr,i,ind);
			if (*ind) return;
			if (!done_constraint[i] &&
			    single_var_constr(constr,i)) done_constraint[i] = 1;
		    }
		}
	}
}

// are there any constraints with >1 variable left
boolean exact::constraints_left(int *done_constraint, int n)
{
	for (int i=0; i<n; i++) {
		if (!done_constraint[i]) return 1;
	}
	return 0;
}

// does this constraint have at most one variable
boolean exact::single_var_constr(integer_matrix & constr, int i)
{
	int num_var = 0;
	for (int j=1; j<constr.n(); j++) {
		if (constr[i][j]) num_var++;
		if (num_var >1) {
				return 0;
		}
	}
	return 1;
}

// does this variable appear in any multi-variable constraint,
// if so is it a leaf of the dag (ie is the sign of it's multiplier
// the same)
// only look at constraints with >1 variable (ie use done_constraint)
// if a leaf, return the sign of the constraint
int exact::var_leaf(integer_matrix & constr, int var, int *done_constraint)
{
	int first_constr = 1;
	int sign = 0;
	for (int i=0; i<num_constr; i++) {
	  if (!done_constraint[i]) {
		if (constr[i][var] > 0) {
			if (first_constr) {
				first_constr = 0;
				sign = 1;
			} else if (sign == -1) return 0;
		} else if (constr[i][var] < 0) {
			if (first_constr) {
				first_constr = 0;
				sign = -1;
			} else if (sign == 1) return 0;
		}
	    }
	}
	if (first_constr) return 0;  // var doesn't appear
	else return sign;
}

// 
// use all constraints with only one variable to set lower
// and upper bounds on variables
//
void exact::set_low_up(integer_matrix & constr, int *ind)
{
	assert (constr.n() < LOWUPSIZE);
	int i;
	for (i=1; i<=constr.n();i++) {
		low[i].set_indef();
		up[i].set_indef();
	}

	for (i=0; i<num_constr; i++) {
		set_low_up(constr,i,ind);
		if (*ind) return;
	}
}

//
// use one constraint to set lower and upper
//
void exact::set_low_up(integer_matrix & constr, int i, int *ind)
{	
	if (single_var_constr(constr,i)) {
	    int j;
		for (j=1; (j<constr.n()) && !constr[i][j]; j++); 
						// j is the variable
		if (j == constr.n()) { // null constraint
			if (constr[i][0] < 0) {
				*ind = 1;
				return;
			}
		} else if (constr[i][j] > 0) { // lower bound
			int lb;
			if (constr[i][0] >0) {
				lb = -(constr[i][0]/constr[i][j]);
			} else {
				lb = (-constr[i][0])/constr[i][j] ;
				if ((-constr[i][0]) % constr[i][j] !=0) {
					lb++;
				}
			}
			low[j] = MAX(lb,low[j]);
		} else {
			assert(constr[i][j] < 0);
			int ub;
			if (constr[i][0] >0) {
				ub = constr[i][0]/(-constr[i][j]);
			} else {
				ub = -(-constr[i][0])/(-constr[i][j]) ;
				if ((-constr[i][0])%(-constr[i][j]) !=0) {
					ub--;
				}
			}
			up[j] = MIN(ub,up[j]);
		}
		if (low[j] > up[j]) {
			*ind = 1;
			return;
		}
	}
}



// routines to deal with int_indef

// if a or b is indef return indef otherwise return a+b
int_indef operator+(int_indef a, int_indef b) {
	if(a.is_indef) return (a);
	else if(b.is_indef) return (b);
	else {int_indef temp(a.val+b.val); return temp;}
}

// if a or b is indef return indef otherwise return a-b
int_indef operator-(int_indef a, int_indef b) {
	if(a.is_indef) return (a);
	else if(b.is_indef) return (b);
	else {int_indef temp(a.val-b.val); return temp;}
}


// if a or b is 0 return 0 else
// if a or b is indef return indef otherwise return a*b
int_indef operator*(int_indef a, int_indef b) {
	if(a.is_indef) {
		if  (!b.is_indef && (b.val==0)) return b;
		else return (a);
	} else if (a.val==0) return a;
	else if (b.is_indef) return b;
	else {int_indef temp(a.val*b.val); return temp;}
}

// if a is 0 return 0 else
// if a or b is indef return indef otherwise return a+b
int_indef operator/(int_indef a, int_indef b) {
	if(a.is_indef || (a.val==0)) return a;
	else if (b.is_indef) return b;
	else {int_indef temp(a.val/b.val); return temp;}
}

// false if one argument is_indef
boolean operator>(int_indef a, int_indef b) {
	if (a.is_indef || b.is_indef) return FALSE;
	else return (a.val > b.val);
}

// if a is indef return b and vice-versa
// otherwise return MAX
int_indef MAX(int_indef a, int_indef b) {
	if (a.is_indef) return b;
	else if (b.is_indef || (a.val > b.val)) return a;
	else return b;
}

// if a is indef return b and vice-versa
// otherwise return MIN
int_indef MIN(int_indef a, int_indef b) {
	if (a.is_indef) return b;
	else if (b.is_indef || (a.val < b.val)) return a;
	else return b;
}



//
// find the first star in a dir_array, return -1 if none
//
int dir_array::first_star()
{
	for (int i=0; i<size; i++) {
		if (data[i].dir() == d_star) return i;
	}
	return -1;
}

// return the level of a dir_array, assume it's expanded and
// lexpos
int dir_array::level()
{
        for (int i=0; i<size; i++) {
                if (data[i].dir() == d_eq) {
                        ;
                } else if (data[i].dir() == d_gt) {
                	return i;
                } else {
                        error_line(1, NULL,"dir_array::level has bad arg\n");
                }
	}
	return size;
}




//
// copy a dir_array
//
dir_array::dir_array(dir_array *d)
{
	size = d->size;
	data = new distance[size];
	for (int i=0; i < size; i++) data[i]= d->data[i];
}	

// create a dir_array from a distance_vector
dir_array::dir_array(distance_vector *dv)
{
	size = dv->count();
	data = new distance[size];
	distance_vector_iter dvi(dv);
	int i=0;
	while (!dvi.is_empty()) {
		data[i] = dvi.step()->d;
		i++;
	}
}

// create a dir_list from a dvlist
dir_list::dir_list(dvlist *dv)
{
	data = 0; next = 0;
	dvlist_iter dvi(dv);
	while (!dvi.is_empty()) {
            dir_list *temp = new dir_list(new dir_array(dvi.step()->dv));
            temp->next = next;
            next = temp;
	}
}

// eliminate dir_arrays that are inconsistent with flow
void dir_list::flow_reduce(boolean *is_used)
{
	expand();
	
	dir_list *new_list = new dir_list();
	dir_array *dir;

	while ((dir=pop()) != 0) {
	    if (dir->lexpos()) {
		if (dir->flow_ok(is_used)) {
			push(dir,&new_list);
		}
	    } else {
		push(dir,&new_list);
	    }
	}
	*this = *new_list;
	delete new_list;
}

// is this dir_array (which is lexpos) legitimate as a flow vector
// assumes that the array has been expanded
// is_used is twice as big as minnest
int dir_array::flow_ok(int *is_used) 
{
	int all_eq = 1;
	for (int i=0; i<size; i++) {
		if (is_used[2*i]) {
			if (data[i].dir() != d_eq) {
				all_eq = 0;
			}
		} else {
			if (!all_eq) {
				while(i<size && !is_used[2*i]) {
					if (data[i].dir() == d_gt) {
						return 0;
					}
					i++;
				}
			} else {
				int io = i;
				while(i<size && !is_used[2*i]) {
					if (data[i].dir() != d_eq) {
						all_eq = 0;
					}
					i++;
				}
				int last_ge = 1;
				if (i<size && data[i].dir() == d_lt) 
					last_ge = 0;
				if (last_ge) {
					if (!all_eq) return 0;
				} else {	
					while(data[io].dir()==d_eq) {
						io++;
						assert(io <size);
					}
					while(io<size && !is_used[2*io]) {
					    if (data[io].dir() == d_gt) 
						return 0;
					}
				} 
			}
		}
	}
	return 1;
}

// is this dir_array (which is lexpos) legitimate as a flow vector
// assumes that the array has been expanded
// is_used is the same size as the nest
int dir_array::flow_ok(int *is_used,flow_constr *fc) 
{
	int all_eq = 1;
	for (int i=0; i<size; i++) {
		if (is_used[i]) {
			if (data[i].dir() != d_eq) {
				all_eq = 0;
			}
		} else {
			if (!all_eq) {
				while(i<size && !is_used[i]) {
					if (data[i].dir() == d_gt) {
						return 0;
					} else if (data[i].dir()==d_eq) {
						fc[i] = fc_max;
					}
					i++;
				}
			} else {
				int io = i;
				while(i<size && !is_used[i]) {
					if (data[i].dir() != d_eq) {
						all_eq = 0;
					}
					i++;
				}
				int last_ge = 1;
				if (i<size && data[i].dir() == d_lt) 
					last_ge = 0;
				if (last_ge) {
					if (!all_eq) return 0;
				} else {	
					while(data[io].dir()==d_eq) {
						io++;
						assert(io <size);
					}
					io++;
					while(io<size && !is_used[io]) {
						if (data[io].dir()==d_gt) 
							return 0;
						fc[io++] = fc_minmax;
					}
				} 
			}
		}
	}
	return 1;
}

// is the dir_array a base case
int dir_array::base_case()
{
	for (int i=0; i<size; i++) {
		if (data[i].dir() != d_lt && 
				data[i].dir() != d_eq && 
				data[i].dir() != d_gt) {
			return 0;
		}
	}
	return 1;
}

// must the dir_array be lexigraphically positive
int dir_array::lexpos()
{
	for (int i=0; i<size; i++) {
		if (data[i].dir() == d_lt || 
				data[i].dir() == d_le || 
				data[i].dir() == d_lg || 
				data[i].dir() == d_star) {
			return 0;
		} else if (data[i].dir() == d_gt) { 
			return 1;
		} else {
			assert(data[i].dir() ==d_eq || data[i].dir()==d_ge);
		}
	}
	return 1;
}

// expand all the dir_arrays on the dir_list to their base components
// ie (* =) => (= =) (< =) (> =)
void dir_list::expand()
{
	dir_array *dpop=pop();
	if (!dpop) return;

	dir_list *dl2=dpop->expand();
	while ((dpop=pop()) != 0) {
		dl2->append(dpop->expand());
	}
	*this = *dl2;
}

dir_list *dir_array::expand(int first)
{
	dir_list *result;
	for (int i=first; i<size; i++) {
		if (data[i].dir() == d_star) {
			dir_array *lt = new dir_array(this);
			dir_array *gt = new dir_array(this);
			dir_array *eq = new dir_array(this);
			lt->data[i].set_direction(d_lt);
			gt->data[i].set_direction(d_gt);
			eq->data[i].set_direction(d_eq);
			result=lt->expand(i+1);
			result->append(gt->expand(i+1));
			result->append(eq->expand(i+1));
			return result;
		} else  if (data[i].dir() == d_le) {
			dir_array *lt = new dir_array(this);
			lt->data[i].set_direction(d_lt);
			dir_array *eq = new dir_array(this);
			eq->data[i].set_direction(d_eq);
			result = lt->expand(i+1);
			result->append(eq->expand(i+1));
			return result;
		} else if (data[i].dir() == d_ge) {
			dir_array *gt = new dir_array(this);
			gt->data[i].set_direction(d_gt);
			dir_array *eq = new dir_array(this);
			eq->data[i].set_direction(d_eq);
			result = gt->expand(i+1);
			result->append(eq->expand(i+1));
			return result;
		} else if (data[i].dir() == d_lg) {
			dir_array *gt = new dir_array(this);
			gt->data[i].set_direction(d_gt);
			dir_array *lt = new dir_array(this);
			lt->data[i].set_direction(d_lt);
			result = gt->expand(i+1);
			result->append(lt->expand(i+1));
			return result;
		}
	}
	return new dir_list(this);
}
		
		




// check if any two constraints in input are really an equal constr
// if so, and one of the vars has a value of +-1, substitute it's value in
// hack to increase applicability and efficiency of dependence analysis
void eq_sub(integer_matrix & input,int &number,int n) 
{
	void eq_sub2(integer_matrix & input, int row1, int row2,int &number,int n);
	for (int i=1; i<number; i++) {
		for (int j=0; j<i; j++) {
			eq_sub2(input,i,j,number,n);
			if (number <= i) break;
		}
	}
}



// check if equality constraint between row1 and row2
void eq_sub2(integer_matrix & input, int row1, int row2,int &number,int n)
{
	int row;

	// check to see that they're an equality constraint
	int num_col = n;
	int i;
	for (i=0; i<num_col; i++) {
		if (input[row1][i] != -input[row2][i]) {
			return ;  // no equality constraint
		}
	}
	// it's an equality, find an element with value -1
	// (don't look at constant field, element 0)
	int col;
	for (col=1; (col < num_col) && (input[row1][col ] != -1) &&
			(input[row2][col ] != -1); col++) {
	}
	if (col == num_col) {
		return ; // no such element
	}	
	assert(col<num_col);
	
	// set row to the appropriate constraint
	if (input[row2][col] == -1) {
		row = row2;
	} else {
		row = row1;
	}

	// do the substitution
	for (i=0; i<number; i++) {  // sub equation i
		if ((i != row1) && (i !=row2)) {
			int multiplier = input[i][col];
			for (int j=0; j<num_col; j++) {
				input[i][j] += multiplier*input[row][j];
			}
			input[i][col] = 0;
		}
	}
// erase the eqaulity constraint
	input.swap(row1,number-1);
	number--;
	input.swap(row2,number-1);
	number--;
	return ;
}



dir_array *dir_list::pop() 
{
    	dir_array *ret = data;
	if (next) {
		data = next->data;
		next = next->next;
	} else data=0;
	return ret;
}


void dir_list::push(dir_array *d, dir_list **list) { 
	if ((*list) && !(*list)->data) (*list)->data = d;
	else {
		dir_list *temp = new dir_list(d);
		temp->next = (*list);
		(*list) = temp;
	}
}


// var vwrite must be >= it's upper bound
// assume write comes before read
int  exact::enforce_max(int *ub_valid,int var) 
{
	int *V = new int[DD+1];for (int i=0; i<DD+1; i++) V[i] = 0; 

	int row  = 1;
	int k;
	for (k=1; k<=2*var; k++) {
		row += ub_valid[k];
	}
	k = 2*var+1;
	V[0] = S[k][0] - (*U)[row][0];
	for (int m=1; m<= k-1; m++) {
		V[0] = V[0] - (*U)[row][m]*S[m][0];;
	}
	for (int n=1; n<= k-1; n++) {
		V[n] = S[k][n];
		for (int m=n; m<= k-1; m++) {
			V[n] = V[n] - (*U)[row][m]*S[m][n];
		}
	}
	V[k] = S[k][k];
	if (addbound(V,k,1)) {
		return 1;
	}
	delete V;
	return 0;
}



// write var v must be >= its upper bound
// read var v must be <= its lower bound
int  exact::enforce_minmax(int *lb_valid,int *ub_valid, int var) 
{
	if (enforce_max(ub_valid,var)) return 1;

	int *V = new int[DD+1];
	int i; for (i=0; i<DD+1; i++) V[i] = 0; 

	int row  = 1;
	int k;
	for (k=1; k<=2*var+1; k++) {
		row += lb_valid[k];
	}
	k = 2*var+2;
	V[0] = S[k][0] - (*L)[row][0];
	for (int m=1; m<= k-1; m++) {
		V[0] = V[0] - (*L)[row][m]*S[m][0];;
	}
	for (int n=1; n<= k-1; n++) {
		V[n] = S[k][n];
		for (int m=n; m<= k-1; m++) {
			V[n] = V[n] - (*L)[row][m]*S[m][n];
		}
	}
	V[k] = S[k][k];
	for (i=0; i<DD+1; i++) V[i] = -V[i];
	if (addbound(V,k,1)) {
		return 1;
	}
	delete V;
	return 0;
}





