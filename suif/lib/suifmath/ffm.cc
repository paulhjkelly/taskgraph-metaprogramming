/* file "ffm.cc" */

/*  Copyright (c) 1994 Stanford University

    All rights reserved.

    This software is provided under the terms described in
    the "suif_copyright.h" include file. */

#include <suif_copyright.h>

#define _MODULE_ "libsuifmath.a"
 
#include <suif1.h>
#include "suifmath.h"
#include <cstdlib>
#include <cstring>

 
// #define DEBUG
#undef DEBUG
// #undef FFM_DEBUG
// int ffm_debug = 0;
#undef FFM_DEBUG

#define DIVFLOOR(x,y) (((x)>0) ? (x)/(y) : ((x)-(y)+1)/(y))


#define ALLOCMIN          4086
#define ALLOCMULTI        8
#define ALLOCLARGE        (524288*8)
#define MAXADDROWS        1024
typedef char * charp;
#define MAXROUNDS2CONVERGE    32


// IF this variable is true equality constraints will get substituted
// into other inequalities.
boolean FFMeqsub = TRUE;

void lcm_and_add(constraint &pi, constraint &pj, constraint &pres, 
		 int xn)
{
    int l = lcm(ABS(pi[xn]),  ABS(pj[xn]));

    if(l == 1) {
	pres = pi;
	pres += pj;
    } else {
	int mi = l/ABS(pi[xn]);
	int mj = l/ABS(pj[xn]);
	pres = pi;
	pres *= mi;
	pres.do_addmul(pj, mj);
    }
}

lin_ineq * fast_fourier_motzkin(lin_ineq & in, integer_row *elim = 0,
				boolean iterate=TRUE)
{
    lin_ineq M(in);
    integer_row RR;
    integer_row CR;
    int nn = in.n();
    integer_row zero(nn);

#ifdef FFM_DEBUG
    fprintf(stderr,"FFM in = \n");
    in.print(stderr);
#endif /* FFM_DEBUG */

    M.normalize(TRUE);
#ifdef FFM_DEBUG
    fprintf(stderr,"Normalized M = \n");
    M.print(stderr);
#endif /* FFM_DEBUG */
    M.del_loose();
    M.del_repetition();

    if (M.m() == 0) {
	lin_ineq * leq = new lin_ineq;
	leq->init(M);
	return leq;
    }

    RR.init(M.m()*2); RR.do_del_col(M.m(),M.m()*2-1);
    CR.init(M.m()*2); CR.do_del_col(M.m(),M.m()*2-1);
    for(int i=0; i<M.m(); i++) {
	RR[i] = M[i].rank();
	CR[i] = 0;
    }

#ifdef FFM_DEBUG
    fprintf(stderr,"Tight M = \n");
    M.print(stderr);
#endif /* FFM_DEBUG */

    integer_matrix last_soln;
    // for each variable, from outer to inner
    boolean gcd_changed = TRUE;
    int round;
    for(round = 0; ((round<MAXROUNDS2CONVERGE)&&(gcd_changed)
		    &&(iterate || (round == 0))) ; round++) {
	gcd_changed = FALSE;
	
	for(int xn = nn-1; xn>0; xn--) {
	    int cm = M.m();

	    if (elim && !(*elim)[xn])
		continue;
#ifdef FFM_DEBUG
	    fprintf(stderr,"round %d, xn=%d, cm=%d, M =\n", round, xn, cm);
	    M.print(stderr);
#endif /* FFM_DEBUG */
	    
	    // ************** Find if equality constraint exists *************
	    boolean found_eq = FALSE;
	    int eqi = 0;
	    int eqj = 0;
	    if(FFMeqsub) {
		for(int i=0; (i < cm)&&(!found_eq); i++)
		    if((RR[i]==xn)&&(M[i][xn] == 1))
			for(int j=0; (j < cm)&&(!found_eq); j++)
			    if((RR[j]==xn)&&(M[j][xn] == -1)) {
				found_eq = TRUE;
				eqi = i;
				eqj = j;
				for(int x=0; x<nn; x++)
				    if(M[i][x] + M[j][x] != 0)
					found_eq = FALSE;
			    }
	    }
	    
	    
	    {
		// ************** Pair-wise **************
		// do the pair-wise elimination and add the constraints
		for(int i=0; i < cm; i++)
		    if(M[i][xn] > 0)
			for(int j=0; j < cm; j++)
			    if(M[j][xn] < 0) {
				constraint tmpc(nn);
				
				lcm_and_add(M[i], M[j], tmpc, xn);
				if (tmpc.rank()==0) {
				    if (tmpc[0]<0) {
					// system inconsistent;
					return 0;
				    }
				} else {
				    boolean chg = tmpc.normalize(TRUE);
				    /* ??? why this condition ??? */
				    if (((RR[i]==xn)&&(RR[j]==xn)) || chg) {
					int currm = M.m();

#ifdef FFM_DEBUG
					fprintf(stderr, "adding row:\n");
					tmpc.print(stderr);
#endif /* FFM_DEBUG */

					M.do_add_rows(1);
					RR.do_insert_col(currm);
					CR.do_insert_col(currm);
					M[currm] ^= tmpc;
					RR[currm] = M[currm].rank();
					if (chg) CR[currm] = 1;
					else CR[currm] = 0;
				    }
				}
			    }	    
	    }

	    if(found_eq) {
		// ************** Substitution by an equality **************
		// If an equality is present, substitute that equality
		
		constraint tmpc(nn);
		for(int j=0; j < cm; j++)
		    if (RR[j]!=xn) { /* ??? why the condition ??? */
			if (M[j][xn] != 0) {
			    if(M[j][xn] < 0) {
				lcm_and_add(M[eqi], M[j], tmpc, xn);
			    } else /* if (M[i][xn] > 0) */ {
				lcm_and_add(M[j], M[eqj], tmpc, xn);
			    };

			    if (tmpc.rank() == 0) {
				if (tmpc[0]<0) {
				    // system inconsistent;
				    return 0;
				}
			    } else {
				boolean chg = tmpc.normalize(TRUE);
				if (chg) {
				    int currm = M.m();
#ifdef FFM_DEBUG
				    fprintf(stderr, "adding eq subst row:\n");
				    tmpc.print(stderr);
#endif /* FFM_DEBUG */
				    M.do_add_rows(1);
				    RR.do_insert_col(currm);
				    CR.do_insert_col(currm);
				    M[currm] ^= tmpc;
				    RR[currm] = M[currm].rank();
				    if (chg) CR[currm] = 1;
				    else CR[currm] = 0;
				}
			    }
			};
		    };
	    }
	    
	    // ************** Remove redundant **************
	    int search_end = found_eq ? 1 : cm;
	    integer_row deleted(M.m());
#ifdef FFM_DEBUG
	    fprintf(stderr,"Before:\n");
	    fprintf(stderr,"deleted: "); deleted.print(stderr);
	    fprintf(stderr,"M: "); M.print(stderr);
	    fprintf(stderr,"RR: "); RR.print(stderr);
	    fprintf(stderr,"CR: "); CR.print(stderr);
#endif
	    M.del_loose(search_end, M.m()-1, &deleted);
	    RR.do_del_columns(deleted);
	    CR.do_del_columns(deleted); 
#ifdef FFM_DEBUG
	    fprintf(stderr,"Middle:\n");
	    fprintf(stderr,"deleted: "); deleted.print(stderr);
	    fprintf(stderr,"M: "); M.print(stderr);
	    fprintf(stderr,"RR: "); RR.print(stderr);
	    fprintf(stderr,"CR: "); CR.print(stderr);
#endif
	    deleted.init(M.m());
	    M.del_repetition(search_end, M.m()-1, &deleted);
	    RR.do_del_columns(deleted);
	    CR.do_del_columns(deleted);
#ifdef FFM_DEBUG
	    fprintf(stderr,"After:\n");
	    fprintf(stderr,"deleted: "); deleted.print(stderr);
	    fprintf(stderr,"M: "); M.print(stderr);
	    fprintf(stderr,"RR: "); RR.print(stderr);
	    fprintf(stderr,"CR: "); CR.print(stderr);
#endif

	    for (int i=0; i<M.m(); i++) {
		if (CR[i]) {
		    gcd_changed = TRUE;
		    CR[i] = 0;
		};
	    }
	    
	}
	if(gcd_changed && iterate) {
	    // compare with the last round's results to see if there is
	    // an actual change.
	    if(round==0) {
		last_soln.init(M);
	    } else {
		if(last_soln == M)
		    gcd_changed = FALSE;
		else
		    last_soln.init(M);
	    }
	}
    }
    
    if(gcd_changed && iterate)
	fprintf(stderr, "WARNING: Fourier-Motzkin error. No convergence after %d iterations, result may be conservative\n", MAXROUNDS2CONVERGE);
    
    lin_ineq * leq = new lin_ineq;
    leq->init(M);
    return leq;
    
}








