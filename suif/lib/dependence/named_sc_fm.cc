/* file "named_sc_fm.cc" */

/*  Copyright (c) 1994 Stanford University

    All rights reserved.

    This software is provided under the terms described in
    the "suif_copyright.h" include file. */

#include <suif_copyright.h>

#define _MODULE_ "libdependence.a"
#pragma implementation "named_sc_fm.h"

#include <cstdio>
#include <cstdarg>
#include <csetjmp>
#include <suif.h>
#include <sys/times.h>
#include <suifmath.h>
#include "dependence.h"
#include <builder.h>


/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Print debug information                                             *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
//#define DEBUG_PRINT
//#define DEBUG_PRINT2
//#define DEBUG_PRINT3

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * turn statistics gathering and printing on                           *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
//#define STAT_ON

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * When watch is turned-on, for large problems that run forever,       *
 * incremental status messages will be displayed.                      *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
//#define WATCH_ON

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Use hand-tuned code in computationally intensive areas.             *
 * These were developed after profiling and obsurving the              *
 * bottlenecks.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
#define USE_SPEED_HACK


#define DIM_M_ST        64
#define DIM_M_OFF       16




int named_sc_fm::cm = 0;
int named_sc_fm::cn = 0;
int named_sc_fm::cp = 0;
sc_fm_constraint ** named_sc_fm::L = NULL;
unsigned * named_sc_fm::magic_list = NULL;

jmp_buf * _named_sc_fm_longjmp_env = NULL;


char * get_name(name_table_entry & nte);

#ifdef STAT_ON
unsigned stat_fc_cons                = 0;
unsigned stat_fc_recalc              = 0;
unsigned stat_fc_div_by_gcd          = 0;
unsigned stat_fc_inverse             = 0;
unsigned stat_fc_is_only_pos_const   = 0;
unsigned stat_fc_is_coef_eq_1        = 0;
unsigned stat_fr_cons                = 0;
unsigned stat_fr_init                = 0;
unsigned stat_fr_set                 = 0;
unsigned stat_fm_cons                = 0;
unsigned stat_fm_extend_block        = 0;
unsigned stat_fm_step0               = 0;
unsigned stat_fm_step1               = 0;
unsigned stat_fm_step2               = 0;
unsigned stat_fm_step3               = 0;
unsigned stat_fm_step4               = 0;
unsigned stat_fm_lin_step            = 0;
unsigned stat_fm_bounds0             = 0;
unsigned stat_fm_bounds1             = 0;
unsigned stat_fm_bounds2             = 0;
unsigned stat_fm_bounds3             = 0;
unsigned stat_fm_is_already0         = 0;
unsigned long stat_fm_is_already0x   = 0;
unsigned stat_fm_is_already1         = 0;
unsigned stat_fm_is_already2         = 0;
unsigned stat_fm_sort                = 0;
unsigned stat_fm_swap                = 0;
unsigned stat_fm_remove              = 0;
unsigned stat_fm_get                 = 0;
unsigned stat_fm_check_valid         = 0;

static void print_stat();
#endif

#ifdef STAT_ON
#define CALL_STAT(NM)   NM++;
#else
#define CALL_STAT(NM)
#endif





#if 0
static long get_time()
{
    char buff[512];
    long t = times((tms *)buff);
    return t;
}
#endif




/***************************************************************************
 * Find a good value to set the num ineq dimension.                        *
 *                                                                         *
 *   x - wanted, pr - old value,  st - minimum size, off - incriment size  *
 ***************************************************************************/
static int next_up(int x, int pr, int st, int off)
{
    // Dont make the block very small compared to the last block. 
    if(pr - off > x) return pr - off;
    int v = st;
    while(v < x) v += off;
    return v;
}


/***************************************************************************
 *   given the number of rows columns and planes, guesstimate a value for  *
 *   maximum number of inequalities that will result in doing fm-solve.    *
 ***************************************************************************/
static int nrows_in_blk(int p, int r, int c)
{
    return MAX(p*r*c, r*4);
}


/***************************************************************************
 *  return the sign of all the non-zero elements of a constraint.  If all  *
 *  elements were zero or elements have different signs return 0.          *
 ***************************************************************************/
static int sign_constraint(constraint & c) { 
    int sgn = 0;
    for(int i=0; i<c.n(); i++)
        if(c[i]) { 
            if(sgn == 0) 
                sgn = (c[i]>0)?1:-1; 
            else if(sgn*c[i]<0) 
                return 0; 
        }
    return sgn;
}


/***************************************************************************
 *  Is a + b == 0 ?                                                        *
 ***************************************************************************/
static boolean is_add_eq_zero(sc_fm_constraint &a, sc_fm_constraint & b)
{
    assert(a.n() == b.n());
    assert(a.p() == b.p());
    for(int ip = 0; ip<a.p(); ip++)
        for(int in = 0; in<a.n(); in++)
            if(a[ip][in] + b[ip][in]) return FALSE;
    return TRUE;
}


#ifdef DEBUG_PRINT
static boolean chk_is_in(int ix, 
                         named_symcoeff_ineq & all, 
                         named_symcoeff_ineq & curr)
{
    assert(all.n() == curr.n());
    assert(all.p() == curr.p());
    assert((ix>=0)&&(ix<all.m()));
    for(int im=0; im<curr.m(); im++) {
        int found = TRUE;
        for(int ip=0; (ip<curr.p())&&(found); ip++) 
            for(int in=0; in<curr.n(); in++) 
                if(all[ip][ix][in] != curr[ip][im][in]) found = FALSE;
        if(found) return TRUE;
    }
    return FALSE;
}
#endif


/* ##################################################
   #####   sc_fm_constraint                    #####
   ################################################## */

/***************************************************************************
 *  create a fm_constraint. Number of planes and columns are given         *
 ***************************************************************************/
sc_fm_constraint::sc_fm_constraint(int in, int ip)
{
    CALL_STAT(stat_fc_cons);
    nn = in;
    pp = ip;
    assert((in>=0)&&(ip>=0));
    L = (in*ip)?(new int[in*ip]):NULL;
    sgn = (in)?(new int[in]):NULL;
    prnk = -1;
    crnk = -1;
    magic_num = 0;
}


/***************************************************************************
 *   Delete                                                                *
 ***************************************************************************/
sc_fm_constraint::~sc_fm_constraint()
{
    if(L)   delete[] L;
    if(sgn) delete[] sgn;
}


/***************************************************************************
 *  Need to call when constraint may have changed.                         *
 *  Recalculate the cached results.                                        *
 ***************************************************************************/
void sc_fm_constraint::recalc()
{
    CALL_STAT(stat_fc_recalc);
    crnk = -1;
    prnk = -1;
    magic_num = 0;
    
    for(int i=0; i<n(); i++)
        recalc(i);
}


/***************************************************************************
 *  called for each column, Will find the                                  *
 *    1) sign of the column.                                               *
 *    2) contribute to calculating the maximum rank of the columns         *
 *    3) contribute to calculating the maximum rank of the planes          *
 *    4) contribute to calculating the unique magic number                 *
 ***************************************************************************/
void sc_fm_constraint::recalc(int in)
{
    int * s = &sgn[in];
    *s = 0;
    boolean bad = FALSE;
    for(int ip=0; ip<p(); ip++) {
        int c = (*this)[ip][in];

        // this simple function will 'somewhat' garuntee that two
        // different systems will have two different magic-numbers
        // most of the time.
        // note that the constant term is disregarded when calculating
        // the magic_num.
        // magic_num ^= (in>0)*c*(41*ip + 31*in + 11*(ip+7)*(in+41) +
        //                        (0x1101 << (MAX(0,18-ip))) +
        //                        (0x1001 << (MAX(0,20-in))) +
        //                        c*(ip+5)*(in+31) +
        //                        7*c*c);

        if(c > 0) {
            magic_num ^= (in>0)*c*(41*ip + 31*in + 11*(ip+7)*(in+41) +
                                   (0x1101 << (MAX(0,18-ip))) +
                                   (0x1001 << (MAX(0,20-in))) +
                                   c*(ip+5)*(in+31) +
                                   7*c*c);
            if(*s == -1) bad = TRUE;
            *s = 1;
            prnk = MAX(prnk, ip);
            crnk = in;
        } else if(c < 0) {
            magic_num ^= (in>0)*c*(41*ip + 31*in + 11*(ip+7)*(in+41) +
                                   (0x1101 << (MAX(0,18-ip))) +
                                   (0x1001 << (MAX(0,20-in))) +
                                   c*(ip+5)*(in+31) +
                                   7*c*c);
            if(*s == 1) bad = TRUE;
            *s = -1;
            prnk = MAX(prnk, ip);
            crnk = in;
        }
    }
    if(bad) *s = BAD_SIGN;
}


/***************************************************************************
 * Divide the constraint by its common-sub-expressions.                    *
 *                                                                         *
 *  1) first find the integer gcd value. if it is > 1 divide the entire    *
 *     constraint by the gcd.                                              *
 *  2) Check if a common-sub-expression (>0) exist for all the non-zero    *
 *     terms. If so divide by that common-sub-expression.                  *
 ***************************************************************************/
void sc_fm_constraint::div_by_gcd()
{
    CALL_STAT(stat_fc_div_by_gcd);

    boolean redo = FALSE;
    // check for a simple integer multiple
    int g = -1;
    for(int ip=0; (ip<p())&&(g!=1); ip++) {
        for(int in=0; (in<n())&&(g!=1); in++)
            if((*this)[ip][in]) {
                if(g == -1) 
                    g = ABS((*this)[ip][in]);
                else
                    g = gcd(g, ABS((*this)[ip][in]));
            }
    }

    if(g==-1) return;
    if(g==0) return;

    if(g != 1) {
        for(ip=0; ip<p(); ip++) {
            for(int in=0; in<n(); in++)
                (*this)[ip][in] /= g;
        }
        redo = TRUE;
    }

    
    // check for a multiple of a linear expression
    constraint nz(p());
    constraint mnz(p());
    constraint tmp(p());
    boolean foundnz = FALSE;
    boolean matched = TRUE;
    for(int in=0; (in<n())&&(matched); in++)
        if(!is_zero(in)) {
            if(!foundnz) {
                get(nz, in);
                if(nz.rank() == 0) return;      // not a lin expr.
                int rg = nz.row_gcd();
                nz /= rg;

                // the sign of all the non-zero elements has to be the same 
                int sgn = sign_constraint(nz);
                if(sgn == 0) return;
                nz *= sgn;                      // nz is always positive

                mnz = nz;
                mnz *= -1;                      // nmz is -nz (always neg.)
                foundnz = TRUE;
            } else {
                get(tmp, in);
                int rg = tmp.row_gcd();
                tmp /= rg;
                if((tmp != nz)&&(tmp != mnz))
                    matched = FALSE;
            }
        }

    if(foundnz && matched) {
        for(int in=0; in<n(); in++) {
            if(!is_zero(in)) {
                get(tmp, in);
                int rg = tmp.row_gcd();
                tmp /= rg;
                if(tmp == nz) {
                    for(int ip=1; ip<p(); ip++)
                        (*this)[ip][in] = 0;
                    (*this)[0][in] = rg;
                } else if(tmp == mnz) {
                    for(int ip=1; ip<p(); ip++)
                        (*this)[ip][in] = 0;
                    (*this)[0][in] = -rg;
                }  else
                    assert(0);
            }
        }
        redo = TRUE;
    }

    if(redo) recalc();
}


/***************************************************************************
 *  Find the inverse of the inequality; negate everything and substract 1  *
 ***************************************************************************/
void sc_fm_constraint::inverse()
{
    CALL_STAT(stat_fc_inverse);

    for(int ip=0; ip<p(); ip++)
        for(int in=0; in<n(); in++)
            (*this)[ip][in] *= -1;
    (*this)[0][0] += -1;
    recalc();
}


/***************************************************************************
 * Check if only non-zero values are the constants and those are also      *
 * positive constants.  If so, this inequality does not carry any          * 
 * information, thus can be eliminated.                                    *
 ***************************************************************************/
boolean sc_fm_constraint::is_only_pos_const()
{
    CALL_STAT(stat_fc_is_only_pos_const);

    if(rank() > 0) return FALSE;
    
    for(int ip=0; ip<p(); ip++) 
        for(int in=1; in<n(); in++)
            if((*this)[ip][in]) return FALSE;

    int cnt=0;
    for(ip=1; ip<p(); ip++)
        if((*this)[ip][0]<0) 
            return FALSE;
        else if((*this)[ip][0] > 0) 
            cnt += (*this)[ip][0];

    // since all plane vars V >= 1 we can assume aV1+bV2 >= a+b
    // thus c + aV1+bV2 >=0 -> c+a+b >= 0
    if((*this)[0][0] < -cnt) return FALSE;

    return TRUE;
}

/***************************************************************************
 *
 ***************************************************************************/
boolean sc_fm_constraint::is_coef_eq_1(int j)
{
    CALL_STAT(stat_fc_is_coef_eq_1);

    if(ABS((*this)[0][j]) > 1) return FALSE;
    for(int ip=1; ip<p(); ip++)
        if((*this)[ip][j]) return FALSE;
    return TRUE;
}

/***************************************************************************
 *  Check if a given column of the ineqality is zero.  i.e. a variable is  *
 *  not present in the inequality.                                         *
 ***************************************************************************/
boolean sc_fm_constraint::is_zero(int in)
{
    for(int ip=0; ip<p(); ip++)
        if((*this)[ip][in]) return FALSE;
    return TRUE;
}


/***************************************************************************
 *  Get the coefficient of a given variable (or the constant value)        *
 ***************************************************************************/
void sc_fm_constraint::get(constraint &c, int in)
{
    assert(c.n() == p());
    for(int ip=0; ip<p(); ip++)
        c[ip] = (*this)[ip][in];
}




/***************************************************************************
 *  Error handling                                                         *
 ***************************************************************************/
void sc_fm_constraint::error()
{
    if(_named_sc_fm_longjmp_env)
        longjmp(*_named_sc_fm_longjmp_env, 1);
    assert(0);
}


/***************************************************************************
 *  print the information in a fm_constraint
 ***************************************************************************/
void sc_fm_constraint::print()
{
    printf("rank=%d, plane_rank=%d,  magic=%d\n", 
           rank(), 
           plane_rank(), 
           magic_num);
    for(int in=0; in<n(); in++) {
        if(sgn[in] == BAD_SIGN) 
            printf("xx: ");
        else
            printf("%2d: ", sgn[in]);
        for(int ip=0; ip<p(); ip++)
            printf("%2d ", (*this)[ip][in]);
        printf("\n");
    }
}

/* ##################################################
   #####   sc_fm_results                       #####
   ################################################## */

sc_fm_results::sc_fm_results()
{
    CALL_STAT(stat_fr_cons);

    mm = 0;
    sz = 0;
    L = NULL;
}


sc_fm_results::~sc_fm_results()
{
    if(L) delete[] L;
}


void sc_fm_results::init(int im, int s)
{
    CALL_STAT(stat_fr_init);

    if((im==m())&&(s==sz)) return;

    if(L) delete[] L;

    mm = im;
    sz = s;
    
    L = (m()*sz)?(new int[m()*sz]):NULL;
}


void sc_fm_results::set(int im, sc_fm_constraint & c)
{
    CALL_STAT(stat_fr_set);

    assert(sz == c.p()*c.n());
    int * dat = (*this)[im];
    for(int ip=0; ip<c.p(); ip++)
        for(int in=0; in<c.n(); in++)
            *dat++ = c[ip][in];
}



/* ##################################################
   #####   named_sc_fm                         #####
   ################################################## */

/***************************************************************************
 *  Delete the FM-solver                                                   *
 ***************************************************************************/
named_sc_fm::~named_sc_fm()
{
#ifdef STAT_ON
    long endtime = get_time();
    endtime -= stat_sttime;
    double t = ((double)endtime)/(double)60.0;  
    fprintf(stderr, "@@@ %5.2f @@@(%d,%d,%d)-%d/%d->(%d(%d),%d,%d)@@@\n",
            t,
            stat_stm, stat_stn, stat_stp,
            stat_nexp, stat_ninit,
            stat_ninitex, stat_maxm, stat_stn, stat_stp);
#endif

    if(results) delete[] results;
}


/***************************************************************************
 *   Resize the static data block                                          *
 ***************************************************************************/
void named_sc_fm::init_block(int newm)
{
    CALL_STAT(stat_fm_cons);

#ifdef STAT_ON
    stat_ninit++;
    stat_ninitex = MAX(newm, stat_ninitex);
#endif

    results = (n())?(new sc_fm_results[n()]):NULL;

    if((newm <= cm) &&
       (n() == cn) &&
       (p() == cp)) {

        for(int im=0; im<cm; im++) {
            for(int ip=0; ip<cp; ip++)
                for(int in=0; in<cn; in++)
                    (*L[im])[ip][in] = 0;
            magic_list[im] = 0;
        }
        return;
    }

    if(cm > 0) {
        for(int i=0; i<cm; i++) delete L[i];
        if(L) delete L;
        if(magic_list) delete[] magic_list;
    }

    cm = next_up(newm, cm, DIM_M_ST, DIM_M_OFF);
    cn = n();
    cp = p();

#ifdef STAT_ON
    stat_maxm = MAX(stat_maxm, cm);
    stat_nexp++;
#endif

    L = (cm)?(new p_sc_fm_constraint[cm]):NULL;
    magic_list = (cm)?(new unsigned[cm]):NULL;
    for(int i=0; i<cm; i++) {
        L[i] = new sc_fm_constraint(cn, cp);
        magic_list[i] = 0;
        assert(L[i]);
    }
    xm = 0;
}

#ifdef WATCH_ON
#define WATCH_CNT       1000
#define WATCH_PAIR_CALL 5000
#define WATCH_SING_CALL  500

int watch_next = WATCH_CNT;
int watch_step1 = WATCH_PAIR_CALL;        
int watch_step2 = WATCH_SING_CALL;        
int watch_ns1 = 0;
int watch_ns2 = 0;
#endif

/***************************************************************************
 *  Increase the size of the static data block. The block need to          *
 *  represent newm-inequalilites.                                          *
 ***************************************************************************/
void named_sc_fm::extend_block(int newm)
{
    CALL_STAT(stat_fm_extend_block);

#ifdef STAT_ON
    stat_ninit++;
    stat_ninitex = MAX(newm, stat_ninitex);
#endif

    if(newm<=cm)return;

    int oldcm  = cm;
    cm = next_up(newm, cm, DIM_M_ST, DIM_M_OFF);

#ifdef STAT_ON
    stat_maxm = MAX(stat_maxm, cm);
    stat_nexp++;
#endif
#ifdef WATCH_ON
    if(cm > watch_next) {
        fprintf(stderr, "-watch--cm=%d--\n", cm);
        watch_next += WATCH_CNT;
    }
#endif
    

    sc_fm_constraint ** newL = (cm)?(new p_sc_fm_constraint[cm]):NULL;
    unsigned * newml = (cm)?(new unsigned[cm]):NULL;

    for(int i=0; i<oldcm; i++) {
        newL[i] = L[i];
        newml[i] = magic_list[i];
    }
    for(; i<cm; i++) {
        newL[i] = new sc_fm_constraint(cn, cp);
        assert(newL[i]);
        newml[i] = 0;
    }

    if(L) delete L;
    L = newL;

    if(magic_list) delete[] magic_list;
    magic_list = newml;

    for(int x=0; x<cm; x++) assert(L[x]);
}



/***************************************************************************
 *   Initialize the FM-problem.                                            *
 *                                                                         *
 * Initialize the size of the problem and the static data block using the  *
 * input data-set.  Calculate the cached results.                          *
 ***************************************************************************/
void named_sc_fm::init(named_symcoeff_ineq & x, boolean k)
{

    keep = k;
    cutoffpt = 0;
    nm_p.init(x.planes());
    nm_c.init(x.cols());

#ifdef STAT_ON
    stat_maxm = cm; 
    stat_stm = x.m(); 
    stat_stn = n(); 
    stat_stp = p(); 
    stat_nexp = 0; 
    stat_ninit = stat_ninitex = 0; 

    stat_sttime = get_time();
#endif

    init_block(nrows_in_blk(p(), x.m(), n()));

    xm = x.m();
    for(int im=0; im<m(); im++) {
        for(int ip=0; ip<p(); ip++)
            for(int in=0; in<n(); in++)
                (*this)[im][ip][in] = x[ip][im][in];
        (*this)[im].recalc();
        magic_list[im] = (*this)[im].magic_num;
    }
}

/***************************************************************************
 *   write back to a named_symcoeff_ineq with internal data                 *
 ***************************************************************************/
named_symcoeff_ineq * named_sc_fm::internal_get()
{
    CALL_STAT(stat_fm_get);

    named_symcoeff_ineq * ret = new named_symcoeff_ineq();
    ret->init(p(), m(), n());
    ret->planes().init(nm_p);
    ret->cols().init(nm_c);
    
    for(int im=0; im<m(); im++)
        for(int ip=0; ip<p(); ip++)
            for(int in=0; in<n(); in++)
                (*ret)[ip][im][in] = (*this)[im][ip][in];

    return ret;
}


/***************************************************************************
 *   update the named_symcoeff_ineq using the current result                *
 ***************************************************************************/
named_symcoeff_ineq * named_sc_fm::get()
{
    assert(p() == nm_p.n());
    assert(n() == nm_c.n());
    named_symcoeff_ineq * ret = new named_symcoeff_ineq();
    get(ret);
    return ret;
}


/***************************************************************************
 *   write back to a named_symcoeff_ineq of the current result              *
 ***************************************************************************/
void named_sc_fm::get(named_symcoeff_ineq * ret)
{
    CALL_STAT(stat_fm_get);

    assert(ret);
    assert(nm_p.n() == p());
    assert(nm_c.n() == n());

    int totm = 0;
    for(int i=0; i<n(); i++)
        totm += results[i].m();

    ret->init(p(), totm, n());
    ret->planes().init(nm_p);
    ret->cols().init(nm_c);

    int currm = 0;
    for(int inx=0; inx<n(); inx++) {
        sc_fm_results & res = results[inx];
        for(int im=0; im<res.m(); im++) {
            int * dat = res[im];
            for(int ip=0; ip<p(); ip++)
                for(int in=0; in<n(); in++)
                    (*ret)[ip][currm][in] = *dat++;
            currm++;
        }
    }
    assert(currm == totm);
}
    




/***************************************************************************
 * Perform Fourier-Motzkin elimination for the entire problem.             *
 * Return TRUE  if a valid solution exists,                                *
 *        FALSE if the problem is inconsistant.                            *
 ***************************************************************************/
boolean named_sc_fm::fm_step()
{
    CALL_STAT(stat_fm_step0);

#ifdef WATCH_ON
    watch_ns2 = 0;
    watch_step2 = WATCH_SING_CALL;
#endif
    return fm_step(0, n());
}


/***************************************************************************
 *  Perform FM-elimination from columns j backto i (both inclusive)        *
 *  Special step for the constant term.                                    *
 ***************************************************************************/
boolean named_sc_fm::fm_step(int i, int j)
{
    CALL_STAT(stat_fm_step1);

    if(MAX(1, i)<j)
        for(int i=0; i<m(); i++)
            if(check_valid(i) == FALSE) return FALSE;

    for(int x=j-1; x>=MAX(1, i); x--)
        if(fm_step(x) == FALSE) return FALSE;
    if(i==0) return fm_lin_step();
    return TRUE;
}


/***************************************************************************
 *  Perform FM-elimination of the j-th variable.                           *
 ***************************************************************************/
boolean named_sc_fm::fm_step(int j)
{
    CALL_STAT(stat_fm_step2);

    assert((j>0)&&(j<n()));
#ifdef WATCH_ON
    if(++watch_ns2 >= watch_step2) {
        fprintf(stderr, "-watch--#call to single step=%d--\n", watch_ns2);
        watch_step2 += WATCH_SING_CALL;
    }
    watch_ns1 = 0;
    watch_step1 = WATCH_PAIR_CALL;
#endif
    
    int low, up, none;
    low = up = none = 0;
    
    
    // find number of inequalities that have a positive coefficient for j,
    // negagive coefficient for j and j is not a participant
    for(int i=0; i<m(); i++) {
        int sg = (*this)[i].sign(j);
        if(sg > 0) low++;
        else if(sg < 0) up++;
        else none++;
    }
    
    // Nothing to eliminate by FM
    if((low == 0)||(up == 0)) {
        if(!keep) {
            if(m()) {
                int * del = new int[m()];
                for(int im=0; im<m(); im++)
                    del[im] = ((*this)[im].sign(j) != 0);
                remove(del);
                delete[] del;
            }
        }
        return TRUE;
    }
    
    // Resize the block to handle all the new inequalities.
    extend_block(m() + low*up);
    int oldm = m();
    
    
#ifdef DEBUG_PRINT3
    printf("....................\n [ ");
    for(int ix=0; ix<m(); ix++)
        printf("%d ", (*this)[ix].sign(j));
    printf(" ]\n");
#endif
    
    // Only new inequalities are the ones after the cutoffpt
    // Thus, we do not have to eliminate two inequalities that are both
    // old since that should have been already done.
    fm_step(j,         0, cutoffpt,  cutoffpt,     oldm);
    fm_step(j,  cutoffpt,     oldm,         0, cutoffpt);
    fm_step(j,  cutoffpt,     oldm,  cutoffpt,     oldm); 
    
    
#ifdef DEBUG_PRINT
    printf("fm_step---%d(%s)--- [oldM=%d, #chk=%d -> newM=%d]\n", 
           j, get_name(nm_c[j]),
           oldm, oldm-cutoffpt, m());
#ifdef DEBUG_PRINT2
    if(oldm != m())
        print();
    else 
        printf("No change\n");
#endif
#endif
    
    
    // Check if the resulting system of inequalities is consistant
    for(i=oldm; i<m(); i++)
        if(check_valid(i) == FALSE) return FALSE;
    
    if(!keep) {
        // remove the inequalities that participated in the FM-step
        if(m()) {
            int * del = new int[m()];
            for(int im=0; im<m(); im++)
                del[im] = ((*this)[im].sign(j) != 0);
            remove(del);
            delete[] del;
        }
    }
    
    
    return TRUE;
}


#ifdef DEBUG_PRINT3
static int tmp_var;
#endif

/***************************************************************************
 *  For the given range of inequalities for coef of j>0 and coef of j<0,   *
 *  find the pairs, get the resulting inequality after performing the FM   *
 *  elimination step.  If that inquality is not already in and has         *
 *  information, add that into the system.                                 *
 ***************************************************************************/
void named_sc_fm::fm_step(int j, 
                           int la, int lb, 
                           int ua, int ub)
{
    CALL_STAT(stat_fm_step3);

    if((!(la<lb))||(!(ua<ub))) return;

    for(int l=la; l<lb; l++)
        if((*this)[l].sign(j) > 0) 
            for(int u=ua; u<ub; u++)
                if((*this)[u].sign(j) < 0) {
                    int tm = xm++;
#ifdef DEBUG_PRINT3
                    printf(" (%d %d)%d", l, u, tm); fflush(stdout);
#endif

                    fm_step(j, (*this)[tm], (*this)[l], (*this)[u]);
                    (*this)[tm].div_by_gcd();
                    magic_list[tm] = (*this)[tm].magic_num;

#ifdef DEBUG_PRINT3
                    printf("%s", (*this)[tm].is_only_pos_const()?"C":"");
                    if(is_already_covered(tm)) printf("I(%d)", tmp_var);
#endif
                    if((*this)[tm].is_only_pos_const())
                        xm--;
                    else if(is_already_covered(tm))
                        xm--;
                }
#ifdef DEBUG_PRINT3
    printf("\n");
#endif
}


/***************************************************************************
 * Perform FM-elimination of the two inequalities A and B w.r.t. the       *
 * variable j and deposit the result in res.                               *
 * Many different cases to consider; speed-hacks provided to speed-up the  *
 * calculations.                                                           *
 ***************************************************************************/
void named_sc_fm::fm_step(int j, 
                           sc_fm_constraint & res,
                           sc_fm_constraint & A,
                           sc_fm_constraint & B)
{
    CALL_STAT(stat_fm_step4);

    assert((A.p() == B.p())&&(A.n() == B.n()));
    int as = A.sign(j);
    int bs = B.sign(j);
    assert(as * bs == -1);


#ifdef WATCH_ON
    if(++watch_ns1 >= watch_step1) {
        fprintf(stderr, "-watch--#call to pair-wise step=%d--\n", watch_ns1);
        watch_step1 += WATCH_PAIR_CALL;
    }
#endif


    // check if A[j] and B[j] if of the form ca*A[j] == cb*B[j]
    constraint cxa(p());
    constraint cxb(p());
    A.get(cxa, j);
    B.get(cxb, j);
    int ca = cxa[cxa.rank()];
    int cb = cxb[cxb.rank()];
    ca = ABS(ca);
    cb = ABS(cb);

    cxa *= cb;
    cxb *= ca;
    constraint cc = cxa + cxb;
    if((cc.rank()==0)&&(cc[0] == 0)) {
        // res = cb*A + ca*B

#ifndef USE_SPEED_HACK
        for(int ip=0; ip<p(); ip++)
            for(int in=0; in<n(); in++)
                res[ip][in] = cb*A[ip][in] + ca*B[ip][in];
#else
        int * pres = &res[0][0];
        int * bound = &res[p()-1][n()-1];
        int * pa   = &A[0][0];
        int * pb   = &B[0][0];
        while(pres <= bound)
            *pres++ = cb * *pa++ + ca * *pb++;
#endif


        assert((res.p() == A.p())&&(res.n() == A.n()));
        res.recalc();
        return;
    }

    if(cxa.rank() == 0) {
#ifndef USE_SPEED_HACK
        for(int ip=0; ip<p(); ip++)
            for(int in=0; in<n(); in++)
                res[ip][in] = B[ip][in]*ABS(A[0][j]);
#else
        int * pres = &res[0][0];
        int * bound = &res[p()-1][n()-1];
        int * pb   = &B[0][0];
        int mul = ABS(A[0][j]);
        while(pres <= bound)
            *pres++ =  *pb++ * mul;
#endif
    } else {
        if(B.plane_rank() != 0) {
            if(_named_sc_fm_longjmp_env)
                longjmp(*_named_sc_fm_longjmp_env, 1);
            printf("Cannot reduce, too complicated (1)\n");
            printf("j=%d\n", j);
            printf("A\n");   A.print();
            printf("B\n");   B.print();
            printf("as=%d bs=%d\nca=%d cb=%d\n", as, bs, ca, cb);
            printf("cxa = "); cxa.print(stdout);
            printf("cxb = "); cxb.print(stdout);
            printf("cc  = "); cc.print(stdout); 
            error();
        }

#ifndef USE_SPEED_HACK
        for(int ip=0; ip<p(); ip++)
            for(int in=0; in<n(); in++)
                res[ip][in] = B[0][in]*ABS(A[ip][j]);
#else
        for(int ip=0; ip<p(); ip++) {
            int * pres = &res[ip][0];
            int * bound = &res[ip][n()-1];
            int * pb   = &B[0][0];
            int mul = ABS(A[ip][j]);
            while(pres <= bound)
                *pres++ = *pb++ * mul;
        }
#endif
    }

    if(cxb.rank() == 0) {
#ifndef USE_SPEED_HACK
        for(int ip=0; ip<p(); ip++)
            for(int in=0; in<n(); in++)
                res[ip][in] += A[ip][in]*ABS(B[0][j]);
#else
        int * pres = &res[0][0];
        int * bound = &res[p()-1][n()-1];
        int * pa   = &A[0][0];
        int mul = ABS(B[0][j]);
        while(pres <= bound)
            *pres++ +=  *pa++ * mul;
#endif
    } else {
        if(A.plane_rank() != 0) {
            if(_named_sc_fm_longjmp_env) 
                longjmp(*_named_sc_fm_longjmp_env, 1);
            printf("Cannot reduce, too complicated (2)\n");
            printf("j=%d\n", j);
            printf("A\n");   A.print();
            printf("B\n");   B.print();
            printf("as=%d bs=%d\nca=%d cb=%d\n", as, bs, ca, cb);
            printf("cxa = "); cxa.print(stdout); 
            printf("cxb = "); cxb.print(stdout); 
            printf("cc  = "); cc.print(stdout); 
            error();
        }

#ifndef USE_SPEED_HACK
        for(int ip=0; ip<p(); ip++)
            for(int in=0; in<n(); in++)
                res[ip][in] += A[0][in]*ABS(B[ip][j]);
#else
        for(int ip=0; ip<p(); ip++) {
            int * pres = &res[ip][0];
            int * bound = &res[ip][n()-1];
            int * pa   = &A[0][0];
            int mul = ABS(B[ip][j]);
            while(pres <= bound)
                *pres++ += *pa++ * mul;
        }
#endif

    }
    assert((res.p() == A.p())&&(res.n() == A.n()));
    res.recalc();
}



/***************************************************************************
 * Is the constant term consistant.                                        *
 *                                                                         *
 * This can be treated as a linear inequality of the symbolic constants    *
 * (the planes).  Since each symbolic constant is > 0, need to check all   *
 * the inequalities with only a constant term to see if the system is      *
 * consistant.                                                             *
 ***************************************************************************/
boolean named_sc_fm::fm_lin_step()
{
    CALL_STAT(stat_fm_lin_step);

    if(m()==0) return TRUE;

    int cnt = 0;
    int * clist = new int[m()];
    for(int i=0; i<m(); i++) {
        if((*this)[i].rank() == 0) {
            cnt++;
            clist[i] = 1;
        } else
            clist[i] = 0;
    }
    
    if(cnt==0) {
        delete[] clist;
        return TRUE;
    }
    
    lin_ineq ll(cnt, p());

    int c = 0;
    for(i=0; i<m(); i++) 
        if(clist[i]) {
            for(int pp=0; pp<p(); pp++)
                ll[c][pp] = (*this)[i][pp][0];
            c++;
        }
    assert(c == cnt);
    delete[] clist;


    nim_op O;
    // add ineqs saying all plane vars >= 1
    lin_ineq x(p()-1, 1);
    for(i=0; i<p()-1; i++) x[i][0] = -1;
    lin_ineq lla = Compose(2, 1,
                           O.NIM(ll),
                           O.NIM(Compose(1, 2,
                                         O.NIM(x),
                                         O.NIM(Ident(p()-1)))));
         
#ifdef DEBUG_PRINT                  
    named_lin_ineq leq(nm_p, lla);
    printf("fm_step---lin-- ineqs=%d -\n", lla.m());
    leq.print_exp(pet_system_nl);
#endif

    if(~lla == TRUE) return FALSE;
    return TRUE;
}






void named_sc_fm::fm_project()
{
    CALL_STAT(stat_fm_bounds0);

    fm_project(1, n());
}


void named_sc_fm::fm_project(int i, int j)
{
    CALL_STAT(stat_fm_bounds1);

#ifdef DEBUG_PRINT
    printf("@@@@@@@@@@@@@F@M@@P@R@O@J@E@C@T@@@@@@@@@@@@@@@@\n");
    sort();
    print();
#endif
    assert((0<i)&&(i<=n()));
    assert((0<j)&&(j<=n())&&(i<=j));
#ifdef WATCH_ON
    watch_ns2 = 0;
    watch_step2 = WATCH_SING_CALL;
#endif

    keep = TRUE;

    fm_step(i, j);

#ifdef DEBUG_PRINT
    printf("@@@@@@@@@@@@@@@@@@after fm_project(%d, %d)@@@@@@@@@@@@@@@@@@@@@\n", i, j);
    sort();
    print();
#endif

    for(int x=j-1; x>=i; x--) {
#ifdef DEBUG_PRINT
        printf("post step================\n");
#endif  
        fm_project(x);

    }
}


void named_sc_fm::fm_project(int j)
{
    fm_bounds_do(j, TRUE);
    fm_bounds_rec(j);
}



/***************************************************************************
 * Find efficient loop bounds for the variables given by the system of     *
 * inequalities.                                                           *
 * Assumption: the system of inequalities is valid                         *
 ***************************************************************************/
void named_sc_fm::fm_bounds()
{
    CALL_STAT(stat_fm_bounds0);

    fm_bounds(1, n());
}


/***************************************************************************
 * Find efficient loop bounds for the i-th to j-th variables in the system *
 * of inequalities.                                                        *
 * Assumption: the system of inequalities is valid                         *
 ***************************************************************************/
void named_sc_fm::fm_bounds(int i, int j)
{
    CALL_STAT(stat_fm_bounds1);

#ifdef DEBUG_PRINT
    printf("@@@@@@@@@@@@@F@M@@B@O@U@N@D@S@@@@@@@@@@@@@@@@@@\n");
    sort();
    print();
#endif
    assert((0<i)&&(i<=n()));
    assert((0<j)&&(j<=n())&&(i<=j));
#ifdef WATCH_ON
    watch_ns2 = 0;
    watch_step2 = WATCH_SING_CALL;
#endif

    keep = TRUE;


    fm_step(i, j);

#ifdef DEBUG_PRINT
    printf("@@@@@@@@@@@@@@@@@@after fm_step(%d, %d)@@@@@@@@@@@@@@@@@@@@@\n", i, j);
    sort();
    print();
#endif

    for(int x=j-1; x>=i; x--) {
#ifdef DEBUG_PRINT
        printf("post step================\n");
#endif  
        fm_bounds(x);

    }
}


void named_sc_fm::fm_bounds(int j)
{
    fm_bounds_do(j, FALSE);
    fm_bounds_rec(j);
}



void named_sc_fm::fm_bounds_rec(int x)
{
    int cnt = 0;
    for(int i=0; i< m(); i++)
        if((*this)[i].rank() == x) 
            cnt++;
    
    if(cnt>0) {
        results[x].init(cnt, n()*p());
        
        int curr = 0;
        for(int i=m()-1; i>= 0; i--)
            if((*this)[i].rank() == x) {
                results[x].set(curr++, (*this)[i]);
                remove(i);
            }
        assert(curr == cnt);
    }
}


/***************************************************************************
 *  Find efficient loop bounds for the j-th variable.                      *
 ***************************************************************************/
void named_sc_fm::fm_bounds_do(int j, boolean no_del)
{
    CALL_STAT(stat_fm_bounds2);
    
    assert((j>0)&&(j<n()));
    
    if(m() == 0) return;
    
    sort();
    
#ifdef DEBUG_PRINT
    printf("fm_bounds %d(%s) ===============================\n", j, get_name(nm_c[j]));
#endif
    
#ifdef DEBUG_PRINT
    named_symcoeff_ineq * tmp = internal_get();
#endif
    
    int low, up, none;
    low = up = none = 0;
    
    int * s_l = new int[m()];
    int * s_u = new int[m()];
    int * s_eq= new int[m()];
    int * dumb = new int[m()];
    
    for(int i=0; i<m(); i++) {
        s_eq[i]= 0;
        s_l[i] = 0;
        s_u[i] = 0;
        int s = 0;
        int r = (*this)[i].rank();
        dumb[i] = r;
        if(r==j) {
            s = (*this)[i].sign(j);
            s_l[i] = (s > 0);
            s_u[i] = (s < 0);
        }
        if(s > 0) low++;
        else if(s < 0) up++;
        else none++;
    }
    
    int numeq = 0;
    int simpeq = -1;
    for(int l=0; l<m(); l++)
        if(s_l[l])
            for(int u=0; (u<m())&&(s_eq[l]==0); u++)
                if(s_u[u] && (s_eq[u]==0)) {
                    if(is_add_eq_zero((*this)[l], (*this)[u])) {
                        numeq++;
                        s_eq[l] = numeq;
                        s_eq[u] = numeq;
                        if(simpeq == -1)
                            if((*this)[l].is_coef_eq_1(j))
                                simpeq = numeq;
                    }
                }
    
    if(simpeq == -1) simpeq = 1;
    int eq[2];
    eq[0] = eq[1] = -1;
    int ex=0;
    if(numeq > 0) {                     
        // Found equality relationship
        // Give it presidence over other inequalities
        for(i=m()-1; i>=0; i--)
            if(s_eq[i] == simpeq) {
                assert(ex<=1);
                eq[ex++] = i;
            }
        assert(ex == 2);
    } 

    if(!no_del)
        fm_bounds(j, low, up, eq);
    
    
    
#ifdef DEBUG_PRINT
    named_symcoeff_ineq * tmpnew = internal_get();
    printf("   %d--rnk--e-l-u-del--- numeq=%d simpeq=\n", 
           j, numeq, simpeq);
    for(i=0; i<tmp->m(); i++) {
        printf("%2d    %d    %d %s %s  %s   ", 
               i, 
               dumb[i], 
               s_eq[i],
               (s_l[i])?"*":" ", 
               (s_u[i])?"*":" ", 
               chk_is_in(i, *tmp, *tmpnew)?" ":"X");
        tmp->print_exp(i); printf("  >= 0\n");
    }
    delete tmp;
    delete tmpnew;
#endif
    
    
    delete[] s_l;
    delete[] s_u;
    delete[] s_eq;
    delete[] dumb;
}


/***************************************************************************
 *  For each ineqality marked in ilist, mark it deleteable if it is not    *
 *  necessary.                                                             *
 *                                                                         *
 *  Invert the ineqality, and check if the system is consistant.           *
 *  BUGBUG: if ineq. is marked delete it should be deleted before checking *
 *          the next. Otherwise it might interfere.                        *
 ***************************************************************************/
void named_sc_fm::fm_bounds(int j, int low, int up, int * eq)
{
    CALL_STAT(stat_fm_bounds3);
    
    for(int i=m()-1; i>=0; i--) 
        if((i != eq[0])&&(i != eq[1]))  {
            int r = (*this)[i].rank();
            int s = (*this)[i].sign(j);
            if((r==j)&&(s)) {
                (*this)[i].inverse();
                magic_list[i] = (*this)[i].magic_num;
                
                swap(i, m()-1);     // so this can be compared with everyone by
                // upping the cutoffpt by one
                int currm = m();
                cutoffpt = m()-1;
#ifdef DEBUG_PRINT
                printf("{*** Inv %d\n", i);
#endif
                boolean ok = fm_step(0, n());
#ifdef DEBUG_PRINT
                printf(" *** Inv %d res=%d }\n", i, ok);
#endif
                xm = currm;
                cutoffpt = 0;
                
                swap(i, m()-1);
                
                (*this)[i].inverse();
                magic_list[i] = (*this)[i].magic_num;
                
                if(!ok) {
                    if(s > 0) { 
                        if(--low > 0) 
                            remove(i);
                    } else {
                        if(--up > 0) 
                            remove(i);
                    }
                }
            }
        }
}




/***************************************************************************
 * Swap two ineqalities in the list.                                       *
 ***************************************************************************/
void named_sc_fm::swap(int i, int j)
{
    CALL_STAT(stat_fm_swap);

    assert((i>=0)&&(i<m()));
    assert((j>=0)&&(j<m()));

    if(i == j) return;

    sc_fm_constraint * tmpl = L[i];
    L[i] = L[j];    
    L[j] = tmpl;

    unsigned tmpm = magic_list[i];
    magic_list[i] = magic_list[j];
    magic_list[j] = tmpm;
}


/***************************************************************************
 *   Sort the inequalities according to the column rank.                   *
 ***************************************************************************/
void named_sc_fm::sort()
{
    CALL_STAT(stat_fm_sort);

    for(int i=0; i<m(); i++)
        for(int j=i+1; j<m(); j++)
            if((*this)[i].rank() > (*this)[j].rank()) {
                swap(i, j);
            }
}


/***************************************************************************
 *  Check if the inequality is valid.                                      *
 *                                                                         *
 *  inequality is not valid if it has no variables and all the constant    *
 *  are less than zero.                                                    *
 ***************************************************************************/
boolean named_sc_fm::check_valid(int r)
{
    CALL_STAT(stat_fm_check_valid);

    boolean found = FALSE;
    for(int d=0; d<p(); d++) 
        if((*this)[r][d][0] < 0) found = TRUE;
        else if((*this)[r][d][0] > 0) return TRUE;

    if(found) {
        for(int d=0; d<p(); d++) 
            for(int c=1; c<n(); c++) 
                if((*this)[r][d][c]) return TRUE;
        return FALSE;
    }
    return TRUE;
}



/***************************************************************************
 *  Check if the chk_this inequality is already covered by another         *
 *  ineqality in the system.                                               *
 *                                                                         *
 *                                                                         *
 *  Since this function is the most expensive in the profile, many things  *
 *  were done to speed this up.  A magic number is calculated for each     *
 *  ineqality s.t. if two ineqalities are the same => they have the same   *
 *  magic number.  That is used as a first cut to eliminate expensive      *
 *  tests.  Also all the magic numbers are kept in a seperate list so that *
 *  the N^2 test can be done cheaper.  The expensive part is called only   *
 *  when the matic numbers match.                                          *
 *  Still this N^2 is the most expensive.                                  *
 ***************************************************************************/
boolean named_sc_fm::is_already_covered(int chk_this)
{
    CALL_STAT(stat_fm_is_already0);


    sc_fm_constraint & newc = (*this)[chk_this];
    
    assert(newc.n() == n());
    assert(newc.p() == p());

#ifndef USE_SPEED_HACK
    unsigned newc_magic = newc.magic_num;
    unsigned * pt_magic = &magic_list[0];

    for(int im=0; im<m(); im++, pt_magic++) 
        if(im != chk_this) {
            CALL_STAT(stat_fm_is_already0x);
            sc_fm_constraint & currc = (*this)[im];
            assert(currc.magic_num == *pt_magic);
            if(currc.magic_num == newc_magic) {
                pt_magic++;
                int ix = (pt_magic - 1 - &magic_list[0]);
                pt_magic--;
                assert(ix == im);
#ifdef DEBUG_PRINT3
            tmp_var = im;
#endif
                if(is_already_covered(newc, currc)) return TRUE;
            }
        }

#else   // the SPEED_HACK

    unsigned newc_magic = newc.magic_num;
    unsigned * pt_magic = &magic_list[0];
    unsigned * pt_end  =  &magic_list[chk_this];
    while(pt_magic < pt_end) {
        CALL_STAT(stat_fm_is_already0x);
        if(*pt_magic++ == newc_magic) {
            int im = (pt_magic - 1 - &magic_list[0]);
            sc_fm_constraint & currc = (*this)[im];
#ifdef DEBUG_PRINT3
            tmp_var = im;
#endif
            if(is_already_covered(newc, currc)) return TRUE;
        }
    }
    pt_magic++;
    pt_end  =  &magic_list[m()-1];
    while(pt_magic <= pt_end) {
        CALL_STAT(stat_fm_is_already0x);
        if(*pt_magic++ == newc_magic) {
            int im = (pt_magic - 1 - &magic_list[0]);
            sc_fm_constraint & currc = (*this)[im];
#ifdef DEBUG_PRINT3
            tmp_var = im;
#endif
            if(is_already_covered(newc, currc)) return TRUE;
        }
    }
#endif  // USE_SPEED_HACK

    return FALSE;
}


/***************************************************************************
 *  Check if the inequalities newc is coverd by oldc                       *
 *                                                                         *
 *  If the variable terms are different, not covered.                      *
 *  First compare if all the signs match. This is a cheaper test that will *
 *  find some non-matching pairs.If the signs match then start the final   *
 *  (expensive) check; that is to check all the elements.  If all the      *
 *  elements match, variable terms are identical.                          *
 *  If the coefficients of the variables are identical, check the constant *
 *  terms.                                                                 *
 ***************************************************************************/
boolean named_sc_fm::is_already_covered(sc_fm_constraint &newc, 
                                         sc_fm_constraint &oldc)
{
    CALL_STAT(stat_fm_is_already1);

#ifndef USE_SPEED_HACK
    boolean found = TRUE;
    for(int in=1; (in<n())&&(found); in++)     
        if(oldc.sign_no_chk(in) != newc.sign_no_chk(in)) 
            found = FALSE;
#else
    int * tsg = &oldc.sgn[1];
    int * csg = &newc.sgn[1];
    int * bound1 = &oldc.sgn[n()-1]; 
    while(tsg<=bound1)
        if(*tsg++ != *csg++)  goto false1;
    goto label1;
 false1:
    return FALSE;
 label1:
#endif
    

#ifndef USE_SPEED_HACK
    for(int ip=0; (ip<p())&&(found); ip++)  {
        for(in=1; in<n(); in++)     
            if(oldc[ip][in] != newc[ip][in])
                found = FALSE;
    }
    if(found) return is_already_covered_chk_const(newc, oldc);
#else
    int * pt = &oldc[0][1];
    int * pc = &newc[0][1];
    int * step =  &oldc[1][0];
    int * bound2 = &oldc[p()-1][n()-1] + 1;
    while(1) 
        if(pt == step) {
            if(pt == bound2)
                return is_already_covered_chk_const(newc, oldc);
            pc++;
            pt++;
            step += n();
        }
        else if(*pc++ != *pt++) return FALSE;
#endif

    return FALSE;
}


/***************************************************************************
 *                                                                         *
 *  A + oc >=0 and A + nc >= 0                                             *
 *  for all plains            oc <= nc    -> old covers new                *
 *  there exist a plane s.t.  oc > nc     -> old does not cover new        *
 ***************************************************************************/
boolean named_sc_fm::is_already_covered_chk_const(sc_fm_constraint &newc, 
                                                   sc_fm_constraint &oldc)
{
    CALL_STAT(stat_fm_is_already2);

    for(int ip=0; ip<p(); ip++)  {
        int nv = newc[ip][0];
        int ov = oldc[ip][0];
        if(ov > nv) return FALSE;
    }
    return TRUE;
}



/***************************************************************************
 * remove the i-th inequality iff del[i]!=0                                *
 ***************************************************************************/
void named_sc_fm::remove(int * del)
{
    for(int i=m()-1; i>=0; i--)
        if(del[i]) remove(i);
}

/***************************************************************************
 * remove the i-th inequality                                              *
 ***************************************************************************/
void named_sc_fm::remove(int i)
{
    CALL_STAT(stat_fm_remove);

    assert(i<m());
    if(i != m()-1)
        swap(i, m()-1);
    xm--;
}


/***************************************************************************
 *  error handling                                                         *
 ***************************************************************************/
void named_sc_fm::error()
{
    if(_named_sc_fm_longjmp_env)
        longjmp(*_named_sc_fm_longjmp_env, 1);
    print();
    assert(0);
}


/***************************************************************************
 *  Print the current set of inequalities.                                 *
 ***************************************************************************/
void named_sc_fm::print()
{
    printf("Internal:\n");
    named_symcoeff_ineq * tmpi = internal_get();
    tmpi->print_exp(pet_system_nl);
    printf("Results:\n");
    named_symcoeff_ineq * tmpr = get();
    tmpr->print_exp(pet_system_nl);
    delete tmpi;
    delete tmpr;
}


void named_sc_fm::delete_all()
{
#ifdef STAT_ON
    print_stat();
#endif
  
    if(cm > 0) {
        for(int i=0; i<cm; i++) delete L[i];
        if(L) delete L;
        if(magic_list) delete[] magic_list;
    }
    L = NULL;
    magic_list = NULL;
    cm = 0; cn = 0; cp = 0;
}


#ifdef STAT_ON
static void print_stat()
{
    fflush(stdout);
    fprintf(stderr, "\nStatistics of Fourier Motzkin\n");
    fprintf(stderr, "=============================\n");
    fprintf(stderr, "\n**sc_fm_constraint**\n");
    fprintf(stderr, "       constructors =%5d,",  stat_fc_cons);
    fprintf(stderr, "             recalc =%6d,",  stat_fc_recalc);
    fprintf(stderr, "         div_by_gcd =%6d\n", stat_fc_div_by_gcd);
    fprintf(stderr, "            inverse =%5d,",  stat_fc_inverse);
    fprintf(stderr, "  is_only_pos_const =%6d,",  stat_fc_is_only_pos_const);
    fprintf(stderr, "       is_coef_eq_1 =%6d\n", stat_fc_is_coef_eq_1);
    fprintf(stderr, "\n**sc_fm_results**\n");
    fprintf(stderr, "       constructors =%5d,",  stat_fr_cons);
    fprintf(stderr, "               init =%5d,",  stat_fr_init);
    fprintf(stderr, "                set =%5d\n", stat_fr_set);
    fprintf(stderr, "\n**named_sc_fm**\n");
    fprintf(stderr, "       constructors =%5d,",  stat_fm_cons);
    fprintf(stderr, "       extend block =%5d,",  stat_fm_extend_block);
    fprintf(stderr, "                get =%5d\n", stat_fm_get);
    fprintf(stderr, "               sort =%5d,",  stat_fm_sort);
    fprintf(stderr, "               swap =%5d,",  stat_fm_swap);
    fprintf(stderr, "             remove =%5d\n", stat_fm_remove);

    fprintf(stderr, "\n"
            "      STEP           all =%6d,    range =%6d,   single =%6d\n"
            "                 section =%6d,     pair =%6d,      lin =%6d\n",   
           stat_fm_step0,
           stat_fm_step1,
           stat_fm_step2,
           stat_fm_step3,
           stat_fm_step4,
           stat_fm_lin_step);
    fprintf(stderr, "\n"
            "    BOUNDS           all =%6d,    range =%6d,   single =%6d       chk =%6d\n",   
           stat_fm_bounds0,
           stat_fm_bounds1,
           stat_fm_bounds2,
           stat_fm_bounds3);
    fprintf(stderr, "\n"
            "ALREADY IN         magic =%6d,  (total)=%10ld,  vars =%7d,  const =%7d\n",
           stat_fm_is_already0,
           stat_fm_is_already0x,
           stat_fm_is_already1,
           stat_fm_is_already2);
    fprintf(stderr, 
            "                     passed magic_num test      = %6.2f%%\n"
            "                     magic_num test was correct = %6.2f%%\n",
           (double)stat_fm_is_already1/(double)stat_fm_is_already0x*100.0,
           (double)stat_fm_is_already2/(double)stat_fm_is_already1*100.0);

    fprintf(stderr, "\n");

    stat_fc_cons                = 0;
    stat_fc_recalc              = 0;
    stat_fc_div_by_gcd          = 0;
    stat_fc_inverse             = 0;
    stat_fc_is_only_pos_const   = 0;
    stat_fc_is_coef_eq_1        = 0;
    stat_fr_cons                = 0;
    stat_fr_init                = 0;
    stat_fr_set                 = 0;
    stat_fm_cons                = 0;
    stat_fm_extend_block        = 0;
    stat_fm_step0               = 0;
    stat_fm_step1               = 0;
    stat_fm_step2               = 0;
    stat_fm_step3               = 0;
    stat_fm_step4               = 0;
    stat_fm_lin_step            = 0;
    stat_fm_bounds0             = 0;
    stat_fm_bounds1             = 0;
    stat_fm_bounds2             = 0;
    stat_fm_bounds3             = 0;
    stat_fm_is_already0         = 0;
    stat_fm_is_already0x        = 0;
    stat_fm_is_already1         = 0;
    stat_fm_is_already2         = 0;
    stat_fm_sort                = 0;
    stat_fm_swap                = 0;
    stat_fm_remove              = 0;
    stat_fm_get                 = 0;
    stat_fm_check_valid         = 0;

}

#endif
