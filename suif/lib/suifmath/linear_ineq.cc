/* file "linear_ineq.cc" */

/*  Copyright (c) 1994 Stanford University

    All rights reserved.

    This software is provided under the terms described in
    the "suif_copyright.h" include file. */

#include <suif_copyright.h>

#define _MODULE_ "libsuifmath.a"
#pragma implementation "linear_ineq.h"

#include <cstdlib>
#include <cstring>
#include <cmath>
#include <suif1.h>
#include "suifmath.h"


no_integer_result_hook_f NIR_hook = NULL;
integer_solver_hook_f    IS_hook  = NULL;



/* ##################################################
   #####   constraint                           #####
   ################################################## */



/********************************************************************************
 * complement                                                                   *
 *                                                                              *
 * when the raw represent R>=0  -> R>-1 compliment is R<=-1 -> -1-R>=0          *
 ********************************************************************************/
void constraint::complement()
{
    (*this) *= -1;
    (*this)[0]--;
}



/********************************************************************************
 * normalize                                                                    *
 ********************************************************************************/
/* if (!norm_bounds), this /= row_gcd();
   if (norm_bounds), 
      g = row_gcd(all but 0)
      this /= g; (take floor of this[0]/g);
*/

boolean constraint::normalize(boolean norm_bounds)
{
    int g = -1;
    for(int i=(norm_bounds)?1:0; i<n(); i++)
        if((*this)[i] != 0) {
            if(g == -1)
                g = ABS((*this)[i]);
            else
                g = gcd(g, ABS((*this)[i]));
        }

    if(g > 1) {
        if(norm_bounds) {
	    for(int i=1; i<n(); i++)
		R[i] /= g;
	    int new0 = (int) floor(((double)R[0])/(double)g);
	    boolean chg = ((g*new0) != R[0]);
	    R[0] = new0;
	    return chg;
	} else
	    (*this) /= g;
	return FALSE;
    } else
	return FALSE;
}



/********************************************************************************
 * highest non-zero value                                                       *
 ********************************************************************************/
int constraint::rank() const
{

    const int * curr = &(R[n()-1]);
    int i = n();
    while(i-->0)
        if(*curr-- != 0) return i;
    return 0;

/* speed hack above
    int rnk = 0;
    for(int cc = 0; cc<n(); cc++)
        if(c(cc) != 0) rnk=cc;
    return rnk; */
}

/********************************************************************************
 * highest order                                                                *
 * return val s.t. For all c R[c]!=0 val +=sort_order[c]                        *
 ********************************************************************************/
int constraint::highest_order(const constraint & sort_order) const
{
    assert(sort_order.n() == this->n());

    int hi = 0;
    for(int cc = 0; cc<n(); cc++)
        if(c(cc) != 0) hi = hi + sort_order.c(cc);

    return hi;
}

/********************************************************************************
 * Only one non-zero variable (1st location is a const, thus can have any val.  *
 ********************************************************************************/
boolean constraint::unique() const
{
    boolean found = FALSE;
    for(int cc = 1; cc<n(); cc++)
        if(c(cc) != 0) {
            if (found)
                return FALSE; // more than one
            else
                found = TRUE; // found first
        }
    if(found)
        return TRUE;  // last one was the only var
    else
        return FALSE; // Only constant
}


/********************************************************************************
 * gcd and lcm                                                                  *
 ********************************************************************************/
int constraint::row_gcd() const
{
    int g = 0;
    for(int i = 0; i<n(); i++)
        if(c(i) != 0) {
            if(g==0)
                g = ABS(c(i));
            else
                g = gcd(g, ABS(c(i)));
        }
    return g;
}

int constraint::row_lcm() const
{
    int l = 0;
    for(int i = 0; i<n(); i++)
        if(c(i) != 0) {
            if(l==0)
                l = ABS(c(i));
            else
                l = lcm(l, ABS(c(i)));
        }
    return l;
}


/* 
   if (type==0)
   then result[i]=1 if lower<=this[i]<=upper
   else result[i]=1 if lower<=leftofthis[i]<=upper
                    where leftofthis[i] = sum(0,i,this[i]==type)-1
*/

constraint constraint::make_filter(int type, int lower, int upper) const
{
    constraint reply(n());

    if(type == 0) { // array vars;
        for(int i = 0; i<n(); i++)
            reply[i] = ((c(i) >= lower)&&(c(i) <= upper))?1:0;
    } else {
        int count = -1;
        for(int i = 0; i<n(); i++) {
            if(type==c(i)) count++;
            reply[i] = ((count >= lower)&&(count <= upper))?1:0;
        }
    }

    return reply;
}

/*
   returns AND(0<=i<n(),kernal[i] && same_sign(this[i],sgn))
*/

int constraint::filter(const constraint & kernal, int sgn) const
{
    for(int i = 0; i<n(); i++)
        if(kernal.c(i) == 1)                      // mark in kernal
            if(c(i) != 0)                 // entry in constraint
                if(c(i)*sgn >= 0)        // sign +/- check sign of entry
                    return 1;                   // if sign=0 any sign is ok
    return 0;
}



/* ##################################################
   #####   lin_ineq                             #####
   ################################################## */


integer_row *lin_ineq::alloc_rows(int num_rows)
{
    realmm = num_rows;
    return new constraint[num_rows];
}


/********************************************************************************
 * Column Operations                                                            *
 ********************************************************************************/
int lin_ineq::col_lcm(int col) const
{
    assert((col>=0)&&(col<nn));
    int l = 0;
    for(int i = 0; i<mm; i++)
        if(r(i).c(col) != 0) {
            if(l==0)
                l = ABS(r(i).c(col));
            else
                l = lcm(l, ABS(r(i).c(col)));
        }
    return l;
}


int lin_ineq::col_gcd(int col) const
{
    assert((col>=0)&&(col<nn));
    int g = 0;
    for(int i = 0; i<mm; i++)
        if(r(i).c(col) != 0) {
            if(g==0)
                g = ABS(r(i).c(col));
            else
                g = gcd(g, ABS(r(i).c(col)));
        }
    return g;
}



/********************************************************************************
 * Operators                                                                       *
 ********************************************************************************/
lin_ineq lin_ineq::operator&&(const lin_ineq & mat) const
{
    if(m() == 0) return mat;
    if(mat.m() == 0) return *this;
    
    assert(n() == mat.n());

    lin_ineq result(this, m()+mat.m());
    for(int i=0; i<mat.m(); i++)
        result[i+m()] = mat.r(i);

    return result;
}

lin_ineq &lin_ineq::operator&=(const lin_ineq & mat)
{
    if(mat.m() == 0) return *this;
    if(m() == 0) { init(mat); return *this; };

    assert(n() == mat.n());

    int oldm = m();
    do_add_rows(mat.m());

    for(int i=0; i<mat.m(); i++)
        (*this)[i+oldm] = mat.r(i);
    
    return *this;
}

// for debugging operator~();
// static int soln_count = 0;

int lin_ineq::operator~() const
{
    assert_msg(NIR_hook, ("No integer solver given, "
                          "(link in the dependence library)"));
//    fprintf(stderr,"[%d]lin_ineq::~ applied to\n",++soln_count);
//    print(stderr);
    boolean ans = (*NIR_hook)(*this, NULL);
//    fprintf(stderr,"result == %d\n",(int)ans);
    return ans;
}

boolean lin_ineq::operator>=(const lin_ineq & mat) const
{
    assert(n() == mat.n());

    if (~mat) return 1;

    lin_ineq testln(&mat, mat.m()+1);

    for(int i=0; i<m(); i++) {
        testln[mat.m()] = -r(i);
        if((~testln)==0) return 0;
    }

    lin_ineq tmp = mat||(*this);
    if (~tmp) {
	fprintf(stderr,"Error in dependence test!!!\n THIS = \n");
	print(stderr);
	fprintf(stderr,"MAT = \n");
	mat.print(stderr);
	fprintf(stderr,"tmp = \n");
	tmp.print(stderr);
	assert(0);
    }

    return 1;
}

lin_ineq lin_ineq::operator%(const constraint & row) const
{
    return lin_ineq(integer_matrix::operator%(row));
}

/********************************************************************************
 * output                                                                       *
 ********************************************************************************/
void lin_ineq::print(FILE *fp, int flag) const
{

    for (int i=0; i<m(); i++)  {
        if(flag == 1) fprintf(fp,"%2d:  ", i);
        for (int j=0; j<n(); j++)  {
            fprintf(fp, " %6d", r(i).c(j));
        }
        fprintf(fp,"\n");
    }
}


int lin_ineq::max_rank() const
{
    int rank = 0;
    for(int i=0; i<m(); i++) rank = MAX(rank, r(i).rank());

    return rank;
}


/********************************************************************************
 * zero                                                                         *
 *                                                                              *
 ********************************************************************************/
void lin_ineq::del_zeros()
{
    int *lst = new int[m()];

    int i;
    int cnt = 0;
    for(i=0; i<m(); i++)
        if((r(i).rank() != 0) || (r(i).c(0) < 0)) {
            cnt++;
            lst[i] = 1;
        } else
            lst[i] = 0;

    if(cnt != m()) {
	int src, dst;
	for (src=0, dst=0; src < m(); src++) {
	    if (lst[src]) {
		if (src != dst)
		    (*this)[dst] ^= (*this)[src];
		dst++;
	    }
	}
	mm = cnt;
    }
    delete[] lst;
}

/********************************************************************************
 * zero                                                                         *
 *                                                                              *
 ********************************************************************************/
void lin_ineq::del_unused_var()
{
    lin_ineq tmpM(this);

    integer_row lst(tmpM.n());

    int i, j;

    lst[0] = 1;
    for(i=0; i<tmpM.m(); i++)
        for(j=0; j<tmpM.n(); j++) if(tmpM[i][j]) lst[j] = 1;

    do_del_columns(lst);
}


/********************************************************************************
 * normalization                                                                *
 *                                                                              *
 ********************************************************************************/

void lin_ineq::normalize(boolean norm_bounds)
{
    for(int i=0; i < this->m(); i++)
        (*this)[i].normalize(norm_bounds);

}

/********************************************************************************
 * repatitions                                                                  *
 *                                                                              *
 ********************************************************************************/
// rem identical rows;
// as compared to itself and (0,a-1);
void lin_ineq::del_repetition(int a, int b, integer_row *deleted)
{

    if ((m() > 0) && (a <= b)) {
	assert(0<=a);
	assert(b<m());
	int *rank = new int[m()];
	int i;
	for(i=0; i < m(); i++) {
	    rank[i] = (*this)[i].rank();
	}
	
	integer_row *del_rows = deleted ? deleted : new integer_row(m());
	if (deleted) {
	    assert(deleted->n() == m());
        }
	for(i=a; i <= b; i++)
	    for(int j=0; j < i; j++) {
		if(rank[i] == rank[j])
		    if((*this)[i] == (*this)[j]) {
			(*del_rows)[j] = 1; // two equals
			rank[i] = 0;
			break;
		    }
	    }
	delete[] rank;
	do_del_rows(*del_rows);
	if (!deleted) delete del_rows;
    }
}

// rem identical rows in (incl)range;
void lin_ineq::del_repetition()
{
    if(m() <= 1) return;

    del_repetition(0, m()-1);
}

// rem otherwise identical row with bigger constant;
// in (incl) range given;
void lin_ineq::del_loose(int r1, int r2, integer_row *deleted)
{
    if (r1 > r2) return;

    integer_row *del_rows = deleted ? deleted : new integer_row(m());
    if (deleted) {
	assert (deleted->n() == m());
    }
    int *this_rank = new int[m()];

    assert ((0<=r1) && (r1<=r2) && (r2<m()));
    
    for(int j = 0; j<m(); j++)
        this_rank[j] = (*this)[j].rank();
    
    for (int i=r1; i <= r2; i++)
	if (((*del_rows)[i]==0) && ((*this)[i].rank() != 0)) {
            constraint A = (*this)[i];
            int c1 = A[0];
	    int A_rank = A.rank();
            A[0] = 0;
            for (int j=0; j < i; j++) {
		if (A_rank == this_rank[j]) {
		    constraint B = (*this)[j];
		    int c2 = (*this)[j][0];
		    B[0] = 0;
		    if (A == B) {
			if (c2 > c1)
			    (*del_rows)[j] = 1;
			else {
			    (*del_rows)[i] = 1;
			    break;
			};
		    };
		};
	    };
	};
    delete[] this_rank;
    do_del_rows(*del_rows);
    if (!deleted) delete del_rows;
}

// rem otherwise identical row with bigger constant;
void lin_ineq::del_loose()
{
    if (m() <= 1) return;
    del_loose(0,m()-1);
}

void lin_ineq::row_swap(int i, int j)
{
    assert((i>=0)&&(i<m()));
    assert((j>=0)&&(j<m()));

    A[i] ^= A[j];
}




/********************************************************************************
 * sort                                                                         *
 *                                                                              *
 * If sort order is given sort in the increasing order of f(M[i])               *
 *   f(M[i]) = val s.t. For all c M[i][c]!=0 val=MAX(s_order[c])                *
 * else Sort S in the order of increasing constraint rank                       *
 ********************************************************************************/
struct rh {
    int rnk;
    int high;

    void rh_swap(rh & o) {
        int tmp;
        tmp = rnk;  rnk  = o.rnk;  o.rnk  = tmp;
        tmp = high; high = o.high; o.high = tmp;
    }
};

// if s_order, sort according to 
//   row.highest_order(s_order)+row.rank();
//   (recall row.highest_order(s_order) = Sum{i s.t. row[i]!=0} s_order[i]);
// dir > 0 => sort into row[i] <= row[i+1];
void lin_ineq::sort(const constraint * s_order, int dir)
{
    if(this->m() <= 1) return;

    if(s_order) {
      assert(s_order->n() == this->n());
    }

    rh * RH = new rh[this->m()];

    int i;
    for(i=0; i<this->m(); i++) {
        RH[i].rnk  = (*this)[i].rank();
        RH[i].high = (s_order)?(*this)[i].highest_order(*s_order):0;
    }

//    s_order->print(stderr);
//    for(int f=0; f<this->m(); f++)
//        fprintf(stderr,"(%d %d+%d) ", f, (*this)[f].highest_order(*s_order),
//                                      (*this)[f].rank());
//    fprintf(stderr,"\n");

    for(i=0; i<this->m()-1; i++) {
        int swap = i;
        for(int j=i+1; j<this->m(); j++)
            if(s_order) {
                if(dir > 0) {
                    if(RH[swap].high+RH[swap].rnk >  RH[j].high+RH[j].rnk)
                        swap = j;
                } else {
                    if(RH[swap].high+RH[swap].rnk <  RH[j].high+RH[j].rnk)
                        swap = j;
                }
            } else {
                if(dir > 0) {
                    if(RH[swap].rnk > RH[j].rnk) swap = j;
                } else {
                    if(RH[swap].rnk < RH[j].rnk) swap = j;
                }
            }
        if(swap != i) {
            RH[i].rh_swap(RH[swap]);
            this->row_swap(i, swap);
        }
    }

    delete[] RH;
}

/********************************************************************************
 * filter                                                                       *
 ********************************************************************************/

/*
   deletes rows for which filter(kernal,sgn) is not true;
   */
void lin_ineq::do_filter_thru(const constraint & kernal, int sign)
{
    if (m() > 0) {
	integer_row del_rows(m());
	
	for(int i=0; i<m(); i++)
	    if(!(*this)[i].filter(kernal, sign))
		del_rows[i] = 1;
	do_del_rows(del_rows);
    }
}

lin_ineq lin_ineq::filter_thru(const constraint & kernal, int sign) const
{
    lin_ineq result(this);
    result.do_filter_thru(kernal,sign);
    return result;
}

/*
   deletes rows for which filter(kernal,sgn) is true;
   */
void lin_ineq::do_filter_away(const constraint & kernal, int sign)
{
    if (m() > 0) {
	integer_row del_rows(m());
	
	for(int i=0; i<m(); i++)
	    if((*this)[i].filter(kernal, sign))
		del_rows[i] = 1;
	do_del_rows(del_rows);
    }
}

lin_ineq lin_ineq::filter_away(const constraint & kernal, int sign) const
{
    lin_ineq result(this);
    result.do_filter_away(kernal,sign);
    return result;
}

/*
   Takes a row with a non-0 for every variable whose bounds matter.
   Modifies the row to take the transitive closure over this leq,
   adding every variable related to those vars.
   */
void lin_ineq::do_bounds(integer_row &bounds0) const
{
    assert(bounds0.n() == n());

    boolean changed = TRUE;
    while (changed) {
	changed = FALSE;

	int i;
	for (i=1; i<n(); i++) {
	    if (bounds0[i]) {
		for (int j=0; j<m(); j++) {
		    const integer_row &thisrow = r(j);
		    if (thisrow.c(i)) {
			for (int i2=1; i2<n(); i2++) {
			    if ((i2 != i) && thisrow.c(i2)) {
				if (!bounds0[i2]) {
				    bounds0[i2] = 1;
				    changed = TRUE;
				};
			    };
			}
		    }
		}
	    }
	}
    }
}

/* ##################################################
   #####   lin_ineq_negate_iter                 #####
   ################################################## */
void lin_ineq_negate_iter::init(const lin_ineq & l)
{
    assert(l.m() < 32);
    L = l;

    curr = 0x01;
    prev = 0x00;

    done_mark = 0x01 << L.m();
}


lin_ineq * lin_ineq_negate_iter::step()
{
    unsigned long chang;

    chang = curr ^ prev;

    for(int i=0; i<L.m(); i++) {
        if(chang & 0x01) L[i] = -L[i];
        chang >>= 1;
    }
    prev = curr;
    curr = curr + 0x01;

    return &L;
}


/* ##################################################
   #####   lin_ineq_difference_iter             #####
   ################################################## */

void lin_ineq_difference_iter::init(const lin_ineq & A, const lin_ineq & B)
{
    D = A & B;
    if(D.m()==0) {
        indep = TRUE;
        D = A;
    } else {
        indep = FALSE;

	D.del_zeros();
	D.del_repetition();
	D.del_loose();
	D.sort();

        // remove constraints of D that are also bounds of A
	integer_matrix m1A(1,A.n());
        lin_ineq A1 = Compose(2, 1,
                              & A,
                              & m1A);
        for(int i=0; i<D.m(); i++) {
            A1[A1.m()-1] = -D[i];
            if(~A1) D[i] = 0;
        }
        D.del_zeros();
    }
}


lin_ineq * lin_ineq_difference_iter::step()
{
    curr++;
    if(indep) {
        return new lin_ineq(D);
    } else {
        lin_ineq * ret = new lin_ineq(curr, D.n());

        ret = new lin_ineq(D.resize(0, curr, 0, D.n()));
        (*ret)[curr] = -(*ret)[curr];

        return ret;
    }
}

/* ##################################################
   #####   lin_ineq_op                          #####
   ################################################## */


/********************************************************************************
 *  constructor                                                                 *
 ********************************************************************************/
lin_ineq_op::lin_ineq_op(int sz, const int * k)
{
    kind.init(sz);

    kind[0] = NM_CONSTANT;

    for(int i=1; i<kind.n(); i++)
        kind[i] = k[i-1];
}

lin_ineq_op::lin_ineq_op(int a, int b)
{
    kind.init(a+b);

    kind[0] = NM_CONSTANT;
    int i;
    for(i=1; i<a; i++) kind[i] = NM_SYMBOLS;
    for(i=a; i<a+b; i++)   kind[i] = NM_LOCATIONS;
}


/********************************************************************************
 *  filtering                                                                   *
 ********************************************************************************/

lin_ineq lin_ineq_op::get(const lin_ineq & LEQ, int nloc, int sgn) const
{
    /* flt[i] = 1 if there are nloc+1 NM_LOCATIONS left of column i in kind */
    constraint flt = kind.make_filter(NM_LOCATIONS, nloc);

    /* removes rows not containing right signed value in column of flt */
    lin_ineq result = LEQ.filter_thru(flt, sgn);

    return result;
}

lin_ineq lin_ineq_op::rem(const lin_ineq & LEQ, int nloc, int sgn) const
{
    constraint flt = kind.make_filter(NM_LOCATIONS, nloc);

    lin_ineq result = LEQ.filter_away(flt, sgn);

    return result;
}


// keeps only rows with a + value in a column i;
// with at least nloc+1 NM_LOCATIONS to the left of it in kind;
lin_ineq lin_ineq_op::get_lesser(const lin_ineq & LEQ, int nloc) const
{
    constraint flt = kind.make_filter(NM_LOCATIONS, nloc, LEQ.n());

    lin_ineq result = LEQ.filter_away(flt, 0);

    return result;
}

// opposite of get_lesser();
lin_ineq lin_ineq_op::get_greater(const lin_ineq & LEQ, int nloc) const
{
    constraint flt = kind.make_filter(NM_LOCATIONS, nloc, LEQ.n());

    lin_ineq result = LEQ.filter_thru(flt, 0);

    return result;
}


lin_ineq lin_ineq_op::get_exclusive(const lin_ineq & LEQ, int nloc, int sgn)
        const
{
    // get rows with a value of right sign in nloc+1 NM_LOCATIONS;
    lin_ineq result = get(LEQ, nloc, sgn);

    constraint flt = kind.make_filter(NM_LOCATIONS) - kind.make_filter(NM_LOCATIONS, nloc);

    // get rid of such rows with any non-0 value in another NM_LOCATIONS col;
    result = result.filter_away(flt, 0);

    return result;
}



/********************************************************************************
 *  overlap or contigous ranges in given dim                                    *
 *                                                                              *
 *  - get both ineq's with given array dim present and no other array dims      *
 *  - If no subset of ineq`s exist like that, cannot get an indipendent         *
 *    variable which might overlap or fuse toghter -> good bye                  *
 *  - If there is an overlap of the ind. dimention all the time can merge       *
 *  - Now check if the two dimentions can be fused toghter. i.e. no gap in the  *
 *    middle of the two ranges.                                                 *
 ********************************************************************************/
lin_ineq lin_ineq_op::overlap_or_contig(const lin_ineq & a, const lin_ineq & b,
                                        int nloc) const
{
    lin_ineq result;

    lin_ineq curr_a;
    lin_ineq curr_b;

    // get rows from a/b with non-0 only in column of nloc+1 NM_LOCATIONS;
    // but in no other NM_LOCATIONS columns;
    curr_a = get_exclusive(a, nloc, 0);
    curr_b = get_exclusive(b, nloc, 0);

    if((curr_a.is_empty())||(curr_b.is_empty())) return result;

    // Now check if a is after b
    
    // get rows from a with + values in nloc+1 columns;
    curr_a = get_exclusive_lower(a, nloc);
    // get rows from b with - values in nloc+1 columns;
    curr_b = get_exclusive_upper(b, nloc);

    int gap1 = check_gap(curr_a, curr_b);
    if(gap1 < 0) return result;

    // vice versa;
    curr_a = get_exclusive_upper(a, nloc);
    curr_b = get_exclusive_lower(b, nloc);

    int gap2 = check_gap(curr_a, curr_b);
    if(gap2 < 0) return result;

    if(gap1 < gap2)
        result = rem_lower(a, nloc) || rem_upper(b, nloc);  // remove lower_a and upper_b
    else
        result = rem_upper(a, nloc) || rem_lower(b, nloc);  // remove upper_a and lower_b

    return result;
}


/********************************************************************************
 *  check_gap                                                                   *
 *                                                                              *
 * input:       for a single array dim. a set with upper bounds and the         *
 *              other set with lower bounds                                     *
 *              Order does not matter (if a is LB then b is UB vice vesa)       *
 *                                                                              *
 * Theory:                                                                      *
 *         ------------|---|-------                                             *
 *                     a   b                                                    *
 *      From LB  i <= a     a - i >= 0                                          *
 *      From UB  i >= b    -b + i >= 0                                          *
 *              add:        a - b                                               *
 *      The gap between rangers cannot be grater than 1   a >= b - 1            *
 *                          a - b >= -1                                         *
 *                                                                              *
 * Method:                                                                      *
 *    For each constraint L in Lower bound set of ineqs                         *
 *        For each constraint U in Upper bound set of ineqs                     *
 *            k = U + L                                                         *
 *            if k only is a constant (rank(k) == 0)                            *
 *                if const(k) >= -1                                             *
 *                    dist = min(dist, cosnt(k))                                *
 *                    delete U from set of upper bounds                         *
 *    if gap is no grater than 1 U shoud be empty.                              *
 *    Now do the same to L                                                      *
 ********************************************************************************/
int lin_ineq_op::check_gap(const lin_ineq & a, const lin_ineq & b) const
{
    lin_ineq liq_a(a);
    lin_ineq liq_b(b);
    constraint k;
    int dist = 10000;

    for(int i = 0; i < a.num_constraint(); i++)
        for(int j = 0; j < b.num_constraint(); j++) {
            k = a.r(i) + b.r(j);
            if(k.rank() == 0)
                if(k.c(0) >= -1) {
                    dist = MIN(dist+2, k.c(0));
                    liq_a[i] = 0;
                    liq_b[j] = 0;
                }
        }

    liq_a.del_zeros();
    liq_b.del_zeros();

    if((!liq_a.is_empty())||(!liq_a.is_empty())) return -1;

    return dist;
}
