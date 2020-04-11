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
#include <suif.h>
#include <suifmath.h>
#include "dependence.h"



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
void constraint::normalize(int norm_bounds)
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
        if(norm_bounds) 
            (*this)[0] = g*(int)floor((double)((*this)[0])/(double)g);
        (*this) /= g;
    }
}
    


/********************************************************************************
 * highest non-zero value                                                       *
 ********************************************************************************/
int constraint::rank()
{

    int * curr = &(*this)[n()-1];
    int i = n();
    while(i-->0) 
        if(*curr-- != 0) return i;
    return 0;

/* speed hack above
    int rnk = 0;
    for(int c = 0; c<n(); c++)
        if((*this)[c] != 0) rnk=c;
    return rnk; */
}

/********************************************************************************
 * highest order                                                                *
 * return val s.t. For all c R[c]!=0 val +=sort_order[c]                        *
 ********************************************************************************/
int constraint::highest_order(constraint & sort_order)
{
    assert(sort_order.n() == this->n());

    int hi = 0;
    for(int c = 0; c<n(); c++)
        if((*this)[c] != 0) hi = hi + sort_order[c];

    return hi;
}

/********************************************************************************
 * Only one non-zero variable (1st location is a const, thus can have any val.  *
 ********************************************************************************/
int constraint::unique() 
{
    int found = 0;
    for(int c = 1; c<n(); c++)
        if((*this)[c] != 0) {
            if(found) 
                return 0; // more than one
            else
                found = 1; // found first
        }
    if(found) 
        return 1;  // last one was the only var
    else
        return 0; // Only constant
}


/********************************************************************************
 * gcd and lcm                                                                  *
 ********************************************************************************/
int constraint::row_gcd()
{
    int g = 0;
    for(int i = 0; i<n(); i++)
        if((*this)[i] != 0) {
            if(g==0)
                g = ABS((*this)[i]);
            else
                g = gcd(g, ABS((*this)[i]));
        }
    return g;
}

int constraint::row_lcm()
{
    int l = 0;
    for(int i = 0; i<n(); i++)
        if((*this)[i] != 0) {
            if(l==0)
                l = ABS((*this)[i]);
            else
                l = lcm(l, ABS((*this)[i]));
        }
    return l;
}


constraint constraint::make_filter(int type, int lower, int upper)
{
    constraint reply(n());

    if(type == 0) { // array var's
        for(int i = 0; i<n(); i++) 
            reply[i] = (((*this)[i] >= lower)&&((*this)[i] <= upper))?1:0;
    } else {
        int count = -1;
        for(int i = 0; i<n(); i++) {
            if(type==(*this)[i]) count++;
            reply[i] = ((count >= lower)&&(count <= upper))?1:0;
        }
    }

    return reply;
}
        

int constraint::filter(constraint & kernal, int sgn)
{
    for(int i = 0; i<n(); i++) 
        if(kernal[i] == 1)                      // mark in kernal
            if((*this)[i] != 0)                 // entry in constraint
                if((*this)[i]*sgn >= 0)        // sign +/- check sign of entry
                    return 1;                   // if sign=0 any sign is ok
    return 0;
}



/* ##################################################
   #####   lin_ineq                             #####
   ################################################## */

/********************************************************************************
 * Column Operations                                                            *
 ********************************************************************************/
int lin_ineq::col_lcm(int col) 
{
    assert((col>=0)&&(col<nn));
    int l = 0;
    for(int i = 0; i<mm; i++)
        if((*this)[i][col] != 0) {
            if(l==0)
                l = ABS((*this)[i][col]);
            else
                l = lcm(l, ABS((*this)[i][col]));
        }
    return l;
}


int lin_ineq::col_gcd(int col) 
{
    assert((col>=0)&&(col<nn));
    int g = 0;
    for(int i = 0; i<mm; i++)
        if((*this)[i][col] != 0) {
            if(g==0)
                g = ABS((*this)[i][col]);
            else
                g = gcd(g, ABS((*this)[i][col]));
        }
    return g;
}



/********************************************************************************
 * Operators                                                                       *
 ********************************************************************************/
lin_ineq lin_ineq::operator||(lin_ineq & mat)
{
    if(m() == 0) return mat;
    if(mat.m() == 0) return *this;

    assert(n() == mat.n());

    lin_ineq result(this, m()+mat.m());
    for(int i=0; i<mat.m(); i++)
        result[i+m()] = mat[i];

    return result;
}

/*
lin_ineq lin_ineq::operator||(constraint & c)
{
    if(m() == 0) {
        lin_ineq res(1, c.n());
        res[0] = c;
        return res;
    }
    if(c.n() == 0) return *this;

    assert(n() == c.n());

    lin_ineq result(this, m()+1);
    result[m()] = c;

    return result;
}
*/

int lin_ineq::operator~()
{
    exact ex;
    int fail;

//    printf("(%d x %d)\n", m(), n());

    ex.T_constr.init(m(),n());
    
    for (int a=0; a<m(); a++) 
        for (int c=0; c<n(); c++) 
            ex.T_constr[a][c] = (*this)[a][c];
    
    ex.num_constr = m();
    
    int ans = ex.check_bounds(&fail);
    if (fail) {
        fprintf(stderr, "-------------------\n");
        print(stderr);
        error_line(1, NULL, "Dependence failure checking the read \n");
    } 

    return ans;
}

int lin_ineq::operator>>(lin_ineq & mat)
{
    assert(n() == mat.n());

    lin_ineq testln(&mat, mat.m()+1);

    for(int i=0; i<m(); i++) {
        testln[mat.m()] = -(*this)[i];
        if((~testln)==0) return 0;
    }

    assert((~(mat||(*this))) == 0);

    return 1;
}

lin_ineq lin_ineq::operator%(constraint & row) 
{ 
    lin_ineq * ret = (lin_ineq *)new integer_matrix((*(integer_matrix *)this) % (integer_row &) row); 

    return *ret;
}

/********************************************************************************
 * output                                                                       *
 ********************************************************************************/
void lin_ineq::print(FILE *fp, int flag=0) 
{

    for (int i=0; i<m(); i++)  {
        if(flag == 1) printf("%2d:  ", i);
        for (int j=0; j<n(); j++)  {
            fprintf(fp," %6d",(*this)[i][j]);
        }
        fprintf(fp,"\n");
    }
}


int lin_ineq::max_rank()
{
    int rank = 0;
    for(int i=0; i<m(); i++) rank = MAX(rank, (*this)[i].rank());

    return rank;
}


/********************************************************************************
 * zero                                                                         *
 *                                                                              *
 ********************************************************************************/
void lin_ineq::del_zeros()
{
    
    int * lst = new int[m()];
    
    int i;
    int cnt = 0;
    for(i=0; i<m(); i++) 
        if((*this)[i].rank() != 0) {
            cnt++;  
            lst[i] = 1;
        } else
            lst[i] = 0;
    
    if(cnt != m()) {
        lin_ineq tmpM(this);
        this->init(cnt, tmpM.n());
        
        int curr = 0;
        for(i=0; i<tmpM.m(); i++)
            if(lst[i] == 1)
                (*this)[curr++] = tmpM[i];
        assert(curr == this->m());
    }

    delete lst;
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

    init(del_columns(lst));
}


/********************************************************************************
 * normalization                                                                *
 *                                                                              *
 ********************************************************************************/
void lin_ineq::normalize(int norm_bounds)
{
    for(int i=0; i < this->m(); i++) 
        (*this)[i].normalize(norm_bounds);

}

/********************************************************************************
 * repatitions                                                                  *
 *                                                                              *
 ********************************************************************************/
void lin_ineq::del_repatition(int a, int b)
{

    assert(a>=0);
    assert(b<=m());
    assert(b>a);
    int * rank = new int[b-a+1];
    for(int i=a; i <= b; i++) {
        rank[i-a] = (*this)[i].rank();
    }

    for(i=a; i <= b-1; i++) 
        if(rank[i-a] != 0) {
            for(int j=i+1; j <= b; j++) {
                if(rank[i-a] == rank[j-a])
                    if((*this)[i] == (*this)[j]) {
                        (*this)[j] = 0; // two equals
                        rank[j-a] = 0;
                    }
            }
        }
    delete rank;
}

#define DEL_REC_LT 100

/***
  *  Exploit locality in very large set of ineqalities. When lot of repatitions     occur this will yield to O(nlogn) instead of O(n^2)
 ***/
void lin_ineq::del_repatition()
{

    if(m() <= 1) return;

/*    int lt = DEL_REC_LT;
    while(m() > 2*lt) {
        int cnt = 0;
        for(int i=0; i< m(); i += lt) {
            del_repatition(i, MIN(i+lt, m())-1);
            cnt++;
        }
        int bef = m();
        del_zeros();
        int aft = m();
        fprintf(stderr, "(%d %d)%d", bef, aft, cnt);
        if(aft < bef/2) 
            lt *= 4;
        else
            break;
    }
*/


    del_repatition(0, m()-1);
//    int x = m();
    del_zeros();
}


void lin_ineq::del_loose()
{
    for(int i=0; i < this->m()-1; i++) 
        if((*this)[i].rank() != 0) 
            for(int j=i+1; j < this->m(); j++) {
                constraint c = (*this)[i] - (*this)[j];
                if(c.rank() == 0) {
                    if(c[0] > 0) 
                        (*this)[i] = 0;
                    else
                        (*this)[j] = 0;
                }
            }
    
    del_zeros();
}


/********************************************************************************
 * minimum constant                                                             *
 *                                                                              *
 *  c1 + Ax >= 0  - - - - - - - - (1)                                           *
 *  c2 + Ax >= 0  - - - - - - - - (2)                                           *
 * if c2 > c1  then (1) -> (2)                                                  *
 ********************************************************************************/
void lin_ineq::min_constant()
{

    int * this_rank = new int[m()];

    for(int j = 0; j<m(); j++) 
        this_rank[j] = (*this)[j].rank();


    for(int i=0; i < m(); i++) 
        if(this_rank[i] != 0) {
            constraint A = (*this)[i];
            int c1 = (*this)[i][0];
            A[0] = 0;
            int A_rank = A.rank();
            for(j=0; j < m(); j++) {
                if(i != j)
                    if(A_rank == this_rank[j]) {
                        constraint B = (*this)[j];
                        int c2 = (*this)[j][0];
                        B[0] = 0;
                        if(c2 > c1) 
                            if(A == B)
                                (*this)[j] = 0;
                    }
            }
        }

    delete this_rank;
}



void lin_ineq::row_swap(int i, int j)
{
    assert((i>=0)&&(i<m()));
    assert((j>=0)&&(j<m()));
    
    int * tmp = A[i].R;
    A[i].R = A[j].R;
    A[j].R = tmp;
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

void lin_ineq::sort(constraint * s_order, int dir) 
{
    if(this->m() == 0) return;

    if(s_order) assert(s_order->n() == this->n());

    rh * RH = new rh[this->m()];

    for(int i=0; i<this->m(); i++) {
        RH[i].rnk  = (*this)[i].rank();
        RH[i].high = (s_order)?(*this)[i].highest_order(*s_order):0;
    }

//    s_order->print(stdout);
//    for(int f=0; f<this->m(); f++) 
//        printf("(%d %d+%d) ", f, (*this)[f].highest_order(*s_order),(*this)[f].rank());
//    printf("\n");

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

    delete RH;
}



lin_ineq  lin_ineq::conjunct(lin_ineq & a)
{
    return compose(2, 1,
                   NIM(*this),
                   NIM(a)); 

/*    if(a.m() == 0) return *this;
    if(m() == 0) return a;

    assert(a.n() == n());
    lin_ineq T(m() + a.m(), n());

    int i;
    for(i=0; i<m(); i++) T[i] = (*this)[i];
    for(i=0; i<a.m(); i++) T[i+m()] = a[i];

   return T; */
}

/********************************************************************************
 * filter                                                                       *
 ********************************************************************************/
lin_ineq lin_ineq::filter(constraint & kernal, int sgn, int op)
{
    lin_ineq result(this);

    if(op == 1) {
        for(int i=0; i<m(); i++) 
            if(!result[i].filter(kernal, sgn)) 
                result[i] = 0;
    } else if(op == -1) {
        for(int i=0; i<m(); i++) 
            if(result[i].filter(kernal, sgn)) 
                result[i] = 0;
    } else
        assert(0);

    result.del_zeros();

    return result;
}


/* ##################################################
   #####   lin_ineq_negate_iter                 #####
   ################################################## */
void lin_ineq_negate_iter::init(lin_ineq & l)
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

void lin_ineq_difference_iter::init(lin_ineq & A, lin_ineq & B)
{
    D = A & B;
    if(D.m()==0) {
        indep = TRUE;
        D = A;
    } else {
        indep = FALSE;
        poly_iterator Poly(D);
        Poly.get_iterator();
        D = Poly.reduce_extra_constraints();
        
        // remove constraints of D that are also bounds of A
        lin_ineq A1 = Compose(2, 1,
                              NIM(A),
                              NIM(1, A.n()));
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
lin_ineq_op::lin_ineq_op(int sz, int * k)
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
    for(int i=1; i<a; i++) kind[i] = NM_SYMBOLS;
    for(i=a; i<a+b; i++)   kind[i] = NM_LOCATIONS;
}


/********************************************************************************
 *  filtering                                                                   *
 ********************************************************************************/

lin_ineq lin_ineq_op::get(lin_ineq & LEQ, int nloc, int sgn)
{
    constraint flt = kind.make_filter(NM_LOCATIONS, nloc);

    lin_ineq result = LEQ.filter_thru(flt, sgn);

    return result;
}

lin_ineq lin_ineq_op::rem(lin_ineq & LEQ, int nloc, int sgn)
{
    constraint flt = kind.make_filter(NM_LOCATIONS, nloc);

    lin_ineq result = LEQ.filter_away(flt, sgn);

    return result;
}


lin_ineq lin_ineq_op::get_lesser(lin_ineq & LEQ, int nloc)
{
    constraint flt = kind.make_filter(NM_LOCATIONS, nloc, LEQ.n());

    lin_ineq result = LEQ.filter_away(flt, 0);

    return result;
}

lin_ineq lin_ineq_op::get_greater(lin_ineq & LEQ, int nloc)
{
    constraint flt = kind.make_filter(NM_LOCATIONS, nloc, LEQ.n());

    lin_ineq result = LEQ.filter_thru(flt, 0);

    return result;
}


lin_ineq lin_ineq_op::get_exclusive(lin_ineq & LEQ, int nloc, int sgn)
{
    lin_ineq result = get(LEQ, nloc, sgn);

    constraint flt = kind.make_filter(NM_LOCATIONS) - kind.make_filter(NM_LOCATIONS, nloc);

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
lin_ineq lin_ineq_op::overlap_or_contig(lin_ineq & a, lin_ineq & b, int nloc)
{
    lin_ineq result;

    lin_ineq curr_a;
    lin_ineq curr_b;

    curr_a = get_exclusive(a, nloc, 0);
    curr_b = get_exclusive(b, nloc, 0);

    if((curr_a.is_empty())||(curr_b.is_empty())) return result;

    // Now check if a is after b
    curr_a = get_exclusive_lower(a, nloc);
    curr_b = get_exclusive_upper(b, nloc);

    int gap1 = check_gap(curr_a, curr_b);
    if(gap1 < 0) return result;


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
int lin_ineq_op::check_gap(lin_ineq & a, lin_ineq & b)
{
    lin_ineq liq_a(a);
    lin_ineq liq_b(b);
    constraint k;
    int dist = 10000;

    for(int i = 0; i < a.num_constraint(); i++)
        for(int j = 0; j < b.num_constraint(); j++) {
            k = a[i] + b[j];
            if(k.rank() == 0) 
                if(k[0] >= -1) {
                    dist = MIN(dist+2, k[0]);
                    liq_a[i] = 0;
                    liq_b[j] = 0;
                }
        }

    liq_a.del_zeros();
    liq_b.del_zeros();

    if((!liq_a.is_empty())||(!liq_a.is_empty())) return -1;

    return dist;
}
            

    
