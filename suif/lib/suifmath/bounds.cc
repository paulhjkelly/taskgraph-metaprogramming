/* file "bounds.cc" */

/*  Copyright (c) 1994 Stanford University

    All rights reserved.

    This software is provided under the terms described in
    the "suif_copyright.h" include file. */

#include <suif_copyright.h>

#define _MODULE_ "libsuifmath.a"
#pragma implementation "bounds.h"
 
#include <cstdlib>
#include <suif1.h>
#include "suifmath.h"




extern int saman_flag_f;

/* ##################################################
   #####   poly_iterator                        #####
   ################################################## */

#undef DBGPRINT

lin_ineq * fast_fourier_motzkin(lin_ineq & in, integer_row *elim = 0,
				boolean iterate=TRUE);


/********************************************************************************
 * get_iterator                                                                 *
 ********************************************************************************/
lin_ineq * poly_iterator::get_iterator(int /* no_del_on_merge */)
{
    S.del_repetition();  /* foobar - delete ? */
    
    S.check();
    
    /* foobar - delete test ? */
    if(~S) {
        fprintf(stderr,"Ooops.. Contradiction at start\n");
        if(saman_flag_f) {
            fprintf(stderr,"Ooops.. Contradiction at start\n");
        }
        S.init(0, num_dimensions());
        return &S;
    }

    /* fourier-motzkin expand S */
    lin_ineq * leq = fast_fourier_motzkin(S);
    if (!leq) {
	fprintf(stderr,"FFM gives different result from ~ in poly_iterator::get_iterator\n");
	fprintf(stderr,"S = \n");
	S.print(stderr);
	/* assert(leq); */
    }
    S.init(leq);
    
    /* foobar can delete */
    S.del_repetition();
    
    if(saman_flag_f) {
        fprintf(stderr,"Before Sort S=\n"); S.print(stderr, 1); 
    }
    /* sort rows of S */
    S.sort(&sort_order);
    if(saman_flag_f) {
        fprintf(stderr,"\nAfter sort S=\n"); 
        sort_order.print(stderr);
        S.print(stderr, 1);
    }
    
    if(saman_flag_f) {
        fprintf(stderr,"Done Result=\n");
        S.print(stderr, 1); 
        fprintf(stderr,"\n");
    }
    return &S;
}

/********************************************************************************
 * fm_pair                                                                      *
 *                                                                              *
 * Fourier Motskin pair-wise projection.                                        *
 * Multyply instead of divide                                                   *
 * Uses values from S, returns the pairs(ONLY pairs) in SP                      *
 ********************************************************************************/
void poly_iterator::fm_pair(lin_ineq & outM, int col, lin_ineq & inM)
{
    assert((col>=0)&&(col < inM.n()));

    inM.sort();

    int n_m = 0;
    int n_p = 0;
    int i;

    for(i=0; i<inM.m(); i++) {
        if(inM[i][col] > 0) 
            n_p++;
        else if(inM[i][col] < 0) 
            n_m++;
    }

    if((n_m == 0)||(n_p == 0)) {
        outM.init(0, num_dimensions());
        return;
    }

    outM.init(n_m*n_p, num_dimensions());
    
    int l = inM.col_lcm(col);  // Find the least common multiple
    

    // Make all the indicies of valid column col the same
    for(i=0; i<inM.m(); i++) {
        if(inM[i][col] != 0)
            inM[i] *= l/ABS(inM[i][col]);
    }

    if(saman_flag_f) {
	fprintf(stderr,"pair:In after lcm\n");
	inM.print(stderr, 1);
    }
    int curr = 0;
    constraint c(inM.n());
    for(int curr_m=0; curr_m < inM.m(); curr_m++)
        if(inM[curr_m][col] < 0)
            for(int curr_p=0; curr_p < inM.m(); curr_p++)
                if(inM[curr_p][col] > 0) {
                    c =  inM[curr_p] + inM[curr_m];
                    /* Check if constraint already in */
		    if(saman_flag_f)
			fprintf(stderr,"(%d, %d)", curr_m, curr_p);
                    int found = 0;
		    int i;
                    for(i=curr-1; (i>=0)&&(!found); i--)
                        if(outM[i] == c) found = 1;
                    if(!found) {
                        outM[curr] = c;
                        curr++;
                    }
		    if(saman_flag_f) {
			if(!found)
			    fprintf(stderr,"*=%d ",curr-1);
			else 
			    fprintf(stderr,"<%d> ", i);
		    }
                }

    if(saman_flag_f) {
	fprintf(stderr,"pair: before del_zeros and gcd\n");
	outM.print(stderr, 1);
    }
    
    assert(curr <= n_m*n_p);
    outM.del_zeros();

    for(i=0; i<outM.m(); i++) {
        int g = outM[i].row_gcd();
        if(g > 0) {
	    if(saman_flag_f) fprintf(stderr,"[%d]=%d ",i, g);
            outM[i] /= g;
	}
    }
    if(saman_flag_f) fprintf(stderr,"\n");
}
       

    


/********************************************************************************
 * merge                                                                        *
 *                                                                              *
 ********************************************************************************/
void poly_iterator::merge(lin_ineq & outM, int col, lin_ineq & inM)
{
    assert((col>=0)&&(col < outM.n()));

    int i;

    if(inM.m() == 0) return;

    lin_ineq tmpM(&outM);

    outM.init(tmpM.m()+inM.m(), num_dimensions());
    
    int curr = 0;
    for(i=0; i<tmpM.m(); i++) 
        outM[curr++] = tmpM[i];

    for(i=0; i<inM.m(); i++) 
        outM[curr++] = inM[i];



    assert(curr == outM.m());
}

/********************************************************************************
 * merge                                                                        *
 *                                                                              *
 ********************************************************************************/
void poly_iterator::delete_merge(lin_ineq & outM, int col, lin_ineq & inM)
{
    assert((col>=0)&&(col <  outM.n()));

    int i;

    if(inM.m() == 0) return;

    lin_ineq tmpM(&outM);

    int with_col = 0;

    for(i=0; i<tmpM.m(); i++) 
        if(tmpM[i][col]  == 0) with_col++;
    
    outM.init(with_col+inM.m(), num_dimensions());
    
    int curr = 0;
    for(i=0; i<tmpM.m(); i++)
        if(tmpM[i][col]  == 0) 
            outM[curr++] = tmpM[i];


    for(i=0; i<inM.m(); i++) 
        outM[curr++] = inM[i];
    
    assert(curr == outM.m());
}

    

                    
/********************************************************************************
 * last                                                                         *
 *                                                                              *
 * if this is a unique constraint.  If so is this the last of that kind         *
 ********************************************************************************/
int poly_iterator::last(lin_ineq & M, int row)
{
    int i;
    int rnk = M[row].rank();

    // see if this is unique.  
    if(M[row].unique()) {
        // Check if other unique exist for the given rank with same sign
        for(i=0; i<M.m(); i++)
            if(row != i)                                // eliminate self
                if(M[i].rank() == rnk)                  // is same rank
                    if(M[row][rnk]*M[i][rnk] > 0)       // same sign
                        if(M[i].unique())               // Is another unique
                            return 0;
        // No other unique with this rank found.  
        return 1;
    } else {
        // Not a unique, so see if any other has the var with the same sign covered
        for(i=0; i<M.m(); i++)
            if(row != i)                                // eliminate self
                if(M[row][rnk]*M[i][rnk] > 0)           // same sign exist
                    return 0;                           // others exist
        // Nobody exist with the same sign
        return 1;
    }
}



/********************************************************************************
 * reduce                                                                       *
 ********************************************************************************/
void poly_iterator::reduce_extra_constraints(lin_ineq & M)
{
    if(saman_flag_f) {
        fprintf(stderr,"Reducing\n");
        M.print(stderr, 1); 
        fprintf(stderr,"\n");
    }

    // attempt to delete redundant constraints */
    M.normalize(1);
    M.min_constant();
    M.del_repetition();

    if(saman_flag_f) {
        fprintf(stderr,"normalize, min constant and del rep\n");
        M.print(stderr, 1); 
        fprintf(stderr,"\n");
    }

    integer_row M_rank(M.m());

    int j;
    for(j = 0; j<M.m(); j++) 
        M_rank[j] = M[j].rank();

    // try to remove some more redundancy;
    for(int i = M.m()-1; i>=0; i--) {
        int rnk = M_rank[i];                    // my rank
        int sgn = M[i][rnk];                    // my sign
        int cnt = 0;
        for(j=0; j<M.m(); j++)
            if(M_rank[j] == rnk)              // same rank
                if(M[j][rnk]*sgn > 0) cnt++;    // same sign

        assert(cnt>0);

        // if this row is redundant with row of same rank and lower;
	// then remove it;
        if(cnt > 1) {                           // other than me
            lin_ineq MM(M);
            MM[i] = -MM[i];                     // Compliment

            //UPDATED: Changed on 2/18/92
            // Since I want bounds at current rank each contribution from
            // Higher ranks may create a circular dependency thus make the
            // bounds larger than neccessary
	    integer_row del_rows(MM.m());
	    for(j=0; j<MM.m(); j++)             
                if(M_rank[j] > M_rank[i]) del_rows[j] = 1;
	    MM.do_del_rows(del_rows);

            MM.min_constant();
            MM.del_repetition();

            if(~MM) {                            // redundant
		if(saman_flag_f) {
		    fprintf(stderr,"%dth inequality will be removed\n", i);
		    M.print(stderr, 1); 
		    fprintf(stderr,"\n");
		}
		
		M.do_del_row(i);
		M_rank.do_del_col(i);
		i = MIN(i+1, M.m()); // gotta do new row i;
            } 
        } 
    }
}


int is_redundant(int num, lin_ineq * fromL, lin_ineq * other1=0, lin_ineq * other2=0, lin_ineq * other3=0)
{
    lin_ineq check;

    check = *fromL;
    check[num] = -check[num];
    if(other1) {
        check = check || *other1;
        if(other2) {
            check = check || *other2;
            if(other3)
                check = check || *other3;
        }
    }
    check.min_constant();
    check.del_zeros();

    if(~check) return 1;
    return 0;
}



/********************************************************************************
 * reduce                                                                       *
 *                                                                              *
 * 1) Normalize by dividing each row by the gcd and upping constant value to    *
 *    the next lowest integer divisible by gcd                                  *
 * 2) If all coif.'s are the same only the constraint with lowest const.        *
 * 3) Delete all repeated constraints                                           *
 * 4) Delete all zero constraints (null constraints)                            *
 * 5) For linear ineqalities of a given rank  (lower -> higher)                 *
 *      a) if rank_var = val, substitute that all over above                    *
 * 6) do step 1, 2, 3 and 4 again                                               *
 * 7) For linear ineqalities of a given rank  (higher -> lower)                 *
 *      a) Find upper, lower and lesser ranks                                   *
 *      b) Sort upper and lower such that outer variables are removed later     *
 *      c) If more than one constraint(in upper or lower) (no equal eliminated) *
 *           remove all unneccessary upper and lower constraints                *
 ********************************************************************************/
void poly_iterator::reduce_extra_constraints2(lin_ineq & M)
{
    if(saman_flag_f) {
        fprintf(stderr,"Reducing\n");
	M.print(stderr, 1); 
        fprintf(stderr,"\n");
    }
    
    constraint eq_const(M.n());
    
    M.normalize(1);
    M.min_constant();
    M.del_repetition();

    if(saman_flag_f) {
        fprintf(stderr,"normalize, min constant and del rep\n");
        M.print(stderr, 1); 
        fprintf(stderr,"\n");
    }
    
    constraint kind(M.n());
    kind = NM_LOCATIONS;
    kind[0] = NM_CONSTANT;
    lin_ineq_op OP(kind);
    
    int rnk;
    lin_ineq Res;
    lin_ineq Curr;
    
    /* 5 */
    Curr = M;
    Res.init(0, 0);
    
    constraint sort_order(M.n());
    int i,c;
    for(i=0, c=16; i<M.n(); i++, c *= 2) sort_order[M.n()-i-1] = c + 10;
    
    for(rnk = 0; rnk < M.n()-1; rnk++) {
        lin_ineq less = OP.get_lesser(Curr, rnk+1);
        lin_ineq lo   = OP.get_lower(less, rnk);
        lin_ineq hi   = OP.get_upper(less, rnk);
        lin_ineq great = OP.get_greater(Curr, rnk+1);
        
        lo.sort(&sort_order);
        hi.sort(&sort_order);


	if(saman_flag_f) {
	    fprintf(stderr,"rank=%d\nless\n", rnk);
	    less.print(stderr, 1);
	    fprintf(stderr,"great\n");
	    great.print(stderr, 1);
	    fprintf(stderr,"lo\n");
	    lo.print(stderr, 1);
	    fprintf(stderr,"hi\n");
	    hi.print(stderr, 1); 
	    fprintf(stderr,"\n");
	}
        
        int single_assign = 0;
	int do_replace = 0;
        int found_lo, found_hi=0;
        // Find if var can be expressed as i = .....
        for(found_lo=0; found_lo < lo.m(); found_lo++) {
            for(found_hi=0; found_hi < hi.m(); found_hi++) {
                constraint tmp = lo[found_lo] + hi[found_hi];
                if(tmp.rank() == 0) {                           // Range = 1 
		    // find the gcd of the coefficients
		    constraint cons(lo[found_lo]);
		    cons[0] = 0;
		    int g = cons.row_gcd();
                    // There is an integer in the  constant range
                    if(ABS(tmp[0]) == g-1) {
                        single_assign = 1;
			if(ABS(tmp[0]) == 0)
			    do_replace = 1;
                    }
                }
                if(single_assign) break;
            }
            if(single_assign) break;
        }

	if(saman_flag_f) 
	    fprintf(stderr,"single_assign=%d (%d, %d)\n", single_assign, found_lo, found_hi);
	
        if(single_assign) {
            eq_const[rnk+1] = 0;

	    if(saman_flag_f) {
		lo[found_lo].print(stderr);
		fprintf(stderr,"pre update (%d)\n", rnk+1);
		great.print(stderr, 1); 
	    }
            // Bound value, other variable should use the bound
	    if(do_replace)
		OP.single_update(great, lo[found_lo], rnk+1);
	    if(saman_flag_f) {
		fprintf(stderr,"post update\n");
		great.print(stderr, 1); 
		fprintf(stderr,"\n");
	    }
            
//            int div = lo[found_lo][rnk+1];
            lin_ineq lhc(2, M.n());
            lhc[0] = hi[found_hi];
            lhc[1] = lo[found_lo];
            
            Res = lhc || Res;
        } else
            Res = lo || hi || Res;

	if(saman_flag_f) {
	    fprintf(stderr,"res\n");
	    Res.print(stderr, 1); 
	    fprintf(stderr,"\n");
	}
        
        Curr = great;
        if(Curr.m() == 0) break;
    }

    if(saman_flag_f) {
        fprintf(stderr,"Post 5\n");
        Res.print(stderr, 1); 
        fprintf(stderr,"\n");
    }
    
    Res.normalize(1);
    Res.min_constant();
    Res.del_repetition();
    
    /* 7 */
    Curr = Res;
    Res.init(0, 0);
    
    for(rnk = M.n()-2; rnk >= 0; rnk--) {
        lin_ineq lo   = OP.get_lower(Curr, rnk);
        lin_ineq hi   = OP.get_upper(Curr, rnk);
        lin_ineq less = OP.get_lesser(Curr, rnk);
        
        lo.sort(&sort_order, -1);
        hi.sort(&sort_order, -1);
        
        if(lo.m() > 1) {
            for(i=0; i<lo.m(); i++) 
                if(is_redundant(i, &lo, &hi, &less)) lo[i] = 0;
            lo.del_repetition();
            lo.del_zeros();
        }
        
        if(hi.m() > 1) {
            for(i=0; i<hi.m(); i++) 
                if(is_redundant(i, &hi, &lo, &less)) hi[i] = 0;
            hi.del_repetition();
            hi.del_zeros();
        }
        
        Res = lo || hi || Res;
        
        Curr = less;
        if(Curr.m() == 0) break;
    } 

    M = Res;

    if(saman_flag_f) {
        fprintf(stderr,"Post 7\n");
        Res.print(stderr, 1); 
        fprintf(stderr,"\n");
    }

}


/********************************************************************************
 *                                                                              *
 ********************************************************************************/
int poly_iterator::next(int curr, int col, int sgn)
{
    assert((col>=0)&&(col < S.n()));

    for(int i=curr+1; i<S.m(); i++)
        if(S[i][col] * sgn > 0) return i;

    return -1;
}


void poly_iterator::print(FILE *fd)
{
    S.print(fd, 1);
}


const lin_ineq *poly_iterator::delete_irrelevant_variables() 
{ 
    assert(delete_order.n() == num_dimensions());
    
    get_strides(S, delete_order.data_array());
    del_variables(S, delete_order.data_array());
    
    return &S; 
}

/****************************************************************************
* get_strides                                                              *
*                                                                          *
* 1) Get all the ineqalities that are destined to be removed               *
 ****************************************************************************/
void poly_iterator::get_strides(lin_ineq & M, int * dv)
{
    if(M.m() == 0) return;
    constraint filt_del(M.n());
    int i;
    for(i=1; i<M.n(); i++)
        if(dv[i] == 1) 
            filt_del[i] = 1;

    for(i=1; i<M.n(); i++)
        if(dv[i] == -1) {
            constraint filt_curr(M.n());
            filt_curr[i] = 1;
            lin_ineq Rest = M.filter_thru(filt_curr, 0).filter_thru(filt_del, 0);
            if(Rest.m() > 0) {  
                // Got some ineq's with aux and curr
                // But we know there can be only a*aux = curr    

                if(strides.m() == 0)
                    strides = Rest;
                else
                    strides = strides || Rest;

                // only 2 lineq's
                assert(Rest.m() == 2);
                // 2 ineq's represent  = 
                constraint x = Rest[0] + Rest[1];
                assert(x.rank() == 0);
                assert(x[0] == 0);
                // coif of curr is 1
                assert(ABS(Rest[0][i]) == 1);
                Rest[0][i] = 0;

                int aux = Rest[0].rank();
                // aux exist
                assert(aux > 0);
                // really an aux not any other variable
                assert(dv[aux] == 1);

                Rest[0][aux] = 0;
                // only 2 variables (curr and aux)
                assert(Rest[0].rank() == 0);
                
            }

        }
//    fprintf(stderr,"===========the_strides\n");
//    strides.print(stderr);
}


/****************************************************************************
 * del_variables                                                            *
 *                                                                          *
 * Delete all rows i s.t. there exist a  c  s.t.                            *
 *   (M[i][c] != 0 and dv[c] = 1) or                                        *
 *                        there dos not exist a  c  s.t.                    *
 *    (M[i][c] != 0 and dv[c] = -1)                                         *
 *                                                                          *
       inv = 0                                                              *
 * dv   0 -1  1                                                             *
 *                                                                          *
 *      0  0  0  del                                                        *
 *      0  0  8  del                                                        *
 *      0  8  0                                                             *
 *      0  8  8  del                                                        *
 *      8  0  0  del                                                        *
 *      8  0  8  del                                                        *
 *      8  8  0                                                             *
 *      8  8  8  del                                                        *
 *                                                                          *
 ****************************************************************************/
void poly_iterator::del_variables(lin_ineq & M, int * dv)
{
    assert(dv);
    
    for(int i=0; i<M.m(); i++) {
        int got_del_var = 0;
        int find_loc = 0;
        for(int j=0; (j<M.n())&&(!got_del_var); j++)  
            if(dv[j]==1){
                if(M[i][j] != 0) got_del_var=1;
            } else if(dv[j]==-1){
                if(M[i][j] != 0) find_loc=1;       
            }
        if((got_del_var)||(!find_loc)) {
            M[i] = 0;
        } 
    }
    M.del_zeros();
}


/******************************************************************************
 * set_sort_order                                                             *
 *                                                                            *
 * Create Source ordering s.t.                                                *
 *  s_o[c] = (   dv[c]== - 1 )*(1 + c) + (  dv[c] > 0   )*2^(c+4)             *
 *            c is a location                c is a ind. var                  *
 *  I.E. (vaguely)
 *       rows with rightmost c (dv[c]>0) nonzero elts go last
 *       then rows with rightmost c (dv[c]==-1) nonzero elts go last
 *       
 *  BUT  multiple leftmost c (dv[c]==-1) nonzero elts can override righmost
 *       or c (dv[c]==0) elt can sometimes override (dv[c]==-1) elts.
 *       or num_dims()>16 can screw it up.
 *
 *  SO, let's change it so that
 *       I.E. above is exact; - brm
 ******************************************************************************/
void poly_iterator::set_sort_order(int * dv)
{
    assert(dv);

    sort_order.init(num_dimensions());
    
    /* 
       // only really works for num_dimensions < 16;
    assert (num_dimensions() < 16);
    for(int c=0, pow=16; c<num_dimensions(); c++, pow *= 2)  
        sort_order[c] = (dv[c] == -1)*(1+c) + (dv[c] > 0)*pow;
	*/

    /* the following screws up turb3d; why? */
    int min_order = 0;
    int i;
    for(i=0; i<num_dimensions(); i++)
	if ((dv[i] < -1) || (dv[i] == 0)) {
	    sort_order[i] = 0;
	};
    min_order = num_dimensions()+1;
    for(i=0; i<num_dimensions(); i++)
	if (dv[i] == -1) {
	    sort_order[i] = min_order;
	    int min_order2 = 2 * min_order;
	    assert (min_order < min_order2);
	    min_order = min_order2;
	}
    for(i=0; i<num_dimensions(); i++)
	if (dv[i] > 0) {
	    sort_order[i] = min_order;
	    int min_order2 = 2 * min_order;
	    assert (min_order < min_order2);
	    min_order = min_order2;
	}
}

/******************************************************************************
 * set_sort_order                                                             *
 *                                                                            *
 ******************************************************************************/
void poly_iterator::set_sort_order(column_stat dv[])
{
    assert(dv);

    sort_order.init(num_dimensions());
    reduce_order.init(num_dimensions());
    delete_order.init(num_dimensions());

    int ndr, nras, nra;
    ndr = nras = nra = 0;
    int c;
    for(c=0; c<num_dimensions(); c++) 
        if(dv[c].cs_op == RO_DONT_REDUCE) ndr++;
        else if(dv[c].cs_op == RO_REDUCE_SHOW) nras++;
        else if(dv[c].cs_op == RO_REDUCE_AWAY) nra++;

    assert(ndr + nras + nra == num_dimensions());

    for(c=0; c<num_dimensions(); c++) 
        delete_order[c] = (dv[c].cs_op == RO_REDUCE_AWAY)*1 + 
                          (dv[c].cs_op == RO_REDUCE_SHOW)*-1;


    
    for(c=0; c<num_dimensions(); c++) 
        if(c != dv[c].cs_rank) {
            fprintf(stderr, "In current implimentation rank should be column #\n");
            exit(-1);
        }

    for(c=0; c<num_dimensions(); c++) 
        sort_order[c] = dv[c].cs_rank +
                       (dv[c].cs_op == RO_REDUCE_SHOW)*num_dimensions() +
                       (dv[c].cs_op == RO_REDUCE_AWAY)*2*num_dimensions();

    int ord = 1;
    for(c=0; c<num_dimensions(); c++) 
        if(dv[c].cs_op == RO_DONT_REDUCE)
            reduce_order[c] = 0;
        else if(dv[c].cs_op == RO_REDUCE_SHOW)
            reduce_order[c] = ord++;

    for(c=0; c<num_dimensions(); c++) 
        if(dv[c].cs_op == RO_REDUCE_AWAY)
            reduce_order[c] = ord++;
}

void poly_iterator::dror_test()
{

    for(int i = 1; i < S.n(); i++) {
        fm_pair(SP, i, S);
        delete_merge(S, i, SP);
//        fprintf(stderr,"(%d)=%d\n", i, S.m());
    }
}


/* ##################################################
   #####   access classes                       #####
   #####        * access_ineq_e                 #####
   #####        * access_ineq_list              #####
   #####        *  access_ineq_list_iter        #####
   ################################################## */

access_ineq_list::~access_ineq_list()
{
    access_ineq_list_iter iter(*this);
    
    while(!iter.is_empty()) {
        access_ineq_e * nxt = iter.step();
        delete nxt;
    }
}


access_ineq_list & access_ineq_list::operator+=(access_ineq_e * elem) 
{
    assert(elem);

    elem->next = head;
    head  = elem;

    return *this;
}


access_ineq_list & access_ineq_list::operator-=(access_ineq_e * elem) 
{
    assert(elem);

    if(head == elem) {
        head = head->next;
        elem->next = 0;
        return *this;
    } else {
        access_ineq_e * bif = head;
        while(bif) {
            if(bif->next == elem) {
                bif->next = bif->next->next;
                elem->next = 0;
                return *this;
            }
            bif = bif->next;
        }
    }

    assert(0);
    return *this;
}


int  access_ineq_list::operator<<(access_ineq_e & elem)
{
    access_ineq_list_iter iter(*this);

    while(!iter.is_empty()) {
        access_ineq_e * ae = iter.step();
        if(ae == &elem) return 1;
    }

    return 0;
}


int  access_ineq_list::max_level()
{
    int max = 0;
    access_ineq_list_iter iter(*this);

    while(!iter.is_empty()) {
        access_ineq_e * ae = iter.step();
        if(ae->level > max) max = ae->level;
    }

    return max;
}


void  access_ineq_list::strip(access_ineq_list & in, int num1, int num2)
{
    access_ineq_list_iter rmiter(in, num1, num2);

    while(!rmiter.is_empty()) {
        access_ineq_e * rm_ac = rmiter.step();
        in -= rm_ac;
        *this += rm_ac;
    }
}


void access_ineq_list_iter::init(access_ineq_list & al, int i, int j)
{
    ail = &al;

    assert(i <= j);
    l = i;
    u = j;

    curr = al.head;

    while((curr)&&(!range(curr))) curr = curr->step();
}

void access_ineq_list_iter::update()
{
    assert(curr);
    curr = curr->step();
    while((curr)&&(!range(curr))) curr = curr->step();
}


void access_ineq_list_iter::rm(access_ineq_e * elem)
{
    if(elem == curr) update();
    (*ail) -= elem;
}
    

extern name_store *nmstore;  // print names of the vars and params
    
/* ##################################################
   #####   priv_access                          #####
   ################################################## */
void priv_access::print(FILE * fp)
{
    access_ineq_list_iter iter(ineqs);

    int first = 0;
    while(!iter.is_empty()) {
        access_ineq_e * eqe = iter.step();

        if(first==0) {
            first = 1;
            fprintf(stderr,"\n       ");
            for(int a=1; a < eqe->LEQ.n(); a++)
                fprintf(stderr," %6s", nmstore->params[a-1]);
            fprintf(stderr,"\n");
        } 
        
//        fprintf(fp, "[%d]\n", eqe->level);
        eqe->LEQ.print(fp);
        fprintf(stderr,"\n");
    }
}


void priv_access::print_code(FILE * fp)
{
    access_ineq_list_iter iter(ineqs);

//    int first = 0;
    while(!iter.is_empty()) {
        access_ineq_e * eqe = iter.step();
        lin_ineq_op liqop(eqe->LEQ.n(), eqe->Name.paramkind);
        liqop.print_code(eqe->LEQ, NULL, &eqe->Name, fp);
    }
}


/********************************************************************************
 * remove overlapped                                                            *
 *                                                                              *
 * For each levels if any one on that level contains the other then that should *
 * not exist. Also if any access on outer level is contained in a access in a   *
 * lower level then it shoud not exist;                                         *
 ********************************************************************************/
void priv_access::remove_overlapped()
{
    if(saman_flag_f) {
        fprintf(stderr,"\n\nRemove Overlapped \n");
    }

    int lev =  ineqs.max_level();
    for(int i=0; i< lev; i++) {
        remove_overlapped_in(i);
//        remove_overlapped_outer(i);
    }
    remove_overlapped_in(lev);

}


#define DBGPRINT
#undef  DBGPRINT

/********************************************************************************
 * remove_overlapped_in                                                         *
 *                                                                              *
 * USEFUL = null                                                                *
 * For each access Curr with level=i in ineqs                                   *
 *     remove Curr from ineqs                                                   *
 *     There exist access ac in USEFUL s.t. ac >> Curr then                     *
 *         For all accesses ac in USEFUL                                        *
 *             if(Curr >> ac) USEFUL -= ac                                      *
 *         USEFUL += Curr                                                       *
 * ineqs += USEFUL;                                                             *
 ********************************************************************************/
void priv_access::remove_overlapped_in(int i)
{
    lin_ineq_op liqop(ineqs.first()->LEQ.n(), ineqs.first()->Name.paramkind);

//    access_ineq_list level_i;
//    access_ineq_list useful;
    if(saman_flag_f) {
        fprintf(stderr,"\n\nOverlapped in on %d\n", i);
    }
    access_ineq_list_iter eq_iter(ineqs, i);
    
    while(!eq_iter.is_empty()) {
        access_ineq_e * Curr = eq_iter.step();
        if(saman_flag_f) {
            fprintf(stderr,"*****Checking\n"); Curr->LEQ.print(stderr);
        }  
        ineqs -= Curr;
        int curr_in = 0;
        access_ineq_list_iter eq_iter2(ineqs, i);
        while((!eq_iter2.is_empty())&&(!curr_in)) {
            access_ineq_e * chk = eq_iter2.step();
            if(chk->LEQ >> Curr->LEQ) 
                curr_in = 1;
            if(saman_flag_f) {
                fprintf(stderr,"against\n"); chk->LEQ.print(stderr);
                if(curr_in) fprintf(stderr,"Contained=======\n");
            }  
        }

        
        if(!curr_in) {
            int got_union = 0;
            lin_ineq union_val;

            access_ineq_list_iter eq_iter3(ineqs, i);
            while((!eq_iter3.is_empty())&&(!got_union)) {
                access_ineq_e * chk = eq_iter3.step();
                union_val = liqop.overlap_or_contig(chk->LEQ, Curr->LEQ, 0) ;
                if(!union_val.is_empty()) {
                    if(saman_flag_f) {
                        fprintf(stderr,"One\n"); chk->LEQ.print(stderr);
                        fprintf(stderr,"and Two\n"); Curr->LEQ.print(stderr);
                        fprintf(stderr," Results = \n"); union_val.print(stderr);
                    }  
                    got_union = 1;
                    chk->LEQ = union_val;
                }
            }
            if(!got_union)
                ineqs += Curr;

        } else
            delete Curr;
    }
    
}



/********************************************************************************
 * remove_overlapped_in                                                         *
 *                                                                              *
 ********************************************************************************/
void priv_access::remove_overlapped_outer(int i)
{

    if(saman_flag_f) {
        fprintf(stderr,"\n\nOverlapped outer on %d\n", i);
    }
    access_ineq_list_iter gt_iter(ineqs, i+1, 100);

    while(!gt_iter.is_empty()) {
        access_ineq_e * Curr = gt_iter.step();
        if(saman_flag_f) {
            fprintf(stderr,"*****Checking\n"); Curr->LEQ.print(stderr);
        }  
        access_ineq_list_iter eq_iter(ineqs, i);
        int in_inner = FALSE;

        while((!eq_iter.is_empty())&&(!in_inner)) {
            access_ineq_e * ac = eq_iter.step();
            
            if(ac->LEQ >> Curr->LEQ) in_inner = TRUE;

            if(saman_flag_f) {
                fprintf(stderr,"against\n"); ac->LEQ.print(stderr);
                if(in_inner) fprintf(stderr,"Contained=======\n");
            }  
        }
        
        if(in_inner) {
            ineqs -= Curr;
            delete Curr;
        }

    }

}


lin_ineq * ProjectForCodeGen(lin_ineq * in)
{
    if(~(*in)) return NULL;
    poly_iterator Poly(*in);
    constraint del_list(in->n());
    del_list = 0;
    Poly.set_sort_order(del_list.data_array());

    lin_ineq M;
    M = Poly.get_iterator(0);
    M = Poly.reduce_extra_constraints2();
    M.sort();
    return new lin_ineq(M);
}


