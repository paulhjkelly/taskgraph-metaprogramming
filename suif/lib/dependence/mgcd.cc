/* file "mgcd.cc" */

/*  Copyright (c) 1994 Stanford University

    All rights reserved.

    This software is provided under the terms described in
    the "suif_copyright.h" include file. */

#include <suif_copyright.h>

/*
 * apply the generalized gcd test to a set of equations
 */
#define _MODULE_ "libdependence.a"

#include <suif1.h>
#include <builder.h>
#include <suifmath.h>
#include "dependence.h"
#include "include.h"
#include "ehash.h"
#include <cstdio>
#include <cstring>

/*
 * fill in I with the identity matrix
 */
void MakeIdentity(int *I, int n) 
{
	int x;

	memset(I, 0, n*n*sizeof(int));
	for (x=0; x<n; x++) {
		I[x*n+x] = 1;
	}
}


/*
 * print out the expression given by the coefficient structure 
 */
void PrintCoeff(coeff x, char variable)
{
	int n=x.n,m=x.m;
	int i,j;

	for (i=0; i<m; i++) {
		printf("%d ",x.constant[i]);
		for (j=0; j<n; j++) {
			if (x.vars[j*m+i] != 0) {
				printf("+ %d * %c%d ",x.vars[j*m+i],
						variable,j+1);
			}
		}
		printf("\n");
	}
}


/*
 * implement Algorithm 5.5.1 from Banerjee's book
 */
void FindGcd(coeff A, int *U, int *D)
{
    int n=A.n;
    int m=A.m;
    int i;
    int q,j,k,temp;
    
    i = n-1;
    if (n<1) {
        printf("Error, illegal number of coefficients: %d \n",n);
        exit(1);
    }
    if (m<1) {
        printf("Error, illegal number of equations: %d \n",m);
        exit(1);
    }
    
    MakeIdentity(U,n);
    memcpy(D, A.vars, n*m*sizeof(int));
    
    for (j=0; j<m; j++) {
        for (i=n-1; i>j; i--) {
            while (D[i*m+j] != 0) {
                q = D[(i-1)*m+j]/D[i*m+j];
                if (q != 0) {
                    for (k= 0; k<n; k++) {  /* row op */
                        U[(i-1)*n+k] -= q*U[i*n+k];
                    }
                    for (k= 0; k<m; k++) {  /* row op */
                        D[(i-1)*m+k] -= q*D[i*m+k];
                    }
                }
                
/* interchange rows i and i-1 */
                
                for (k= 0; k<n; k++) {
                    temp = U[(i-1)*n+k];
                    U[(i-1)*n+k] = U[i*n+k];
                    U[i*n+k] = temp;
                }
                
                for (k= 0; k<m; k++) {
                    temp = D[(i-1)*m+k];
                    D[(i-1)*m+k] = D[i*m+k];
                    D[i*m+k] = temp;
                }
            }
        }
    }
    if (D[0] < 0)  {	/* multiply (U,D)[0] by -1 */
        for (k=0; k<n; k++) {
            U[0*n+k] = -U[0*n+k];
        }
        for (k=0; k<m; k++) {
            D[0*m+k] = -D[0*m+k];
        }
    }
    
}

/* 
 * is row t of the m collumn matrix zero 
 */
int IsZeroRow(int *D,int t, int m)
{
	int j=0,iszero=0;

	while((j<m) && (iszero = (D[t*m+j] == 0))) {
		j++;
	}
	return(iszero);
}



/*
 * find the rank of an nXm row upper echelon matrix
 * ie the number of non-zero rows
 */
int RankD(int *D,int n, int m) 
{
	int t=0;
	
	while((t<n) && !IsZeroRow(D,t,m)) {
		t++;
	}
	return(t);
}


/*
 * is there an integer soln?
 * this is determined by whether there exists an integer matrix t
 * st TD+C=0;
 * return T if it exists
 */
 int IsIntSoln(int *T, int *D, int *C, int n, int m, int rank)
 {
	int i=0, j, sum;

	while (i<m) {	
		sum = -C[i];
		for (j=0; j<i; j++) {
			sum -= T[j]*D[j*m+i];
		}
		if (D[i*m+i] == 0) {
			if (sum==0) { 		/* free parameter */
			} else {	
				return(FALSE);	/* inconsistent equations */
			}
		} else {
			if ((sum % D[i*m+i]) != 0) {  /* not integer solution */
				return(FALSE);
			}
			T[i] = sum/D[i*m+i];
		}
		i++;
	}

	/* suppress warning messages from compiler */
	n=n;
	rank=rank;

	return(TRUE);
}


// the hash tables (put it here since can't have static local table)
struct hashes {
hashes() : UD(0), X(0) {
}

~hashes() {
	delete UD; UD = 0;
	delete X; X = 0;
}
UD_hash_table *UD;
X_hash_table *X;
} hashtables;

// was the last attempt a hit
int u_hit = 0;
int x_hit = 0;

extern int num_t[10];
extern int num_indep;


/* 
 * using the Unimodal matrix found in Barerjee's 5.5.1 find 
 * the solution to the equation Ax.number + x.constant =0 if it exists.
 * return whether or not it exists
 * First check cache for X
 *   if miss then check for U&D
 * 	  if a miss calculate U&D and place in cache
 *   then place X in cache
 */
int Gcd(coeff A, coeff **X)
{
	int *U=0, *D=0,*T;
	int n=A.n,m=A.m;
	int i,j,rank;
	boolean ind;

	if (!hashtables.X || !hashtables.X->hit_or_miss(&A,X,&ind)) {  // not in cache
		x_hit = 0;
		T = new int[n];
		if (!hashtables.X) {
		    hashtables.X = new X_hash_table(997);
		    hashtables.UD = new UD_hash_table(997);
		}

		if (!hashtables.UD->hit_or_miss(&A,&U,&D)) {  // not in cache
			u_hit = 0;
			U = new int[n*n];
			D = new int[n*m];
			FindGcd(A,U,D);
			coeff *A2 =  A.copy();
			hashtables.UD->add_entry(A2,U,D);
		} else {
			u_hit = 1;
		}
		
		rank = RankD(D,n,m);

		if (IsIntSoln(T,D,A.constant,n,m,rank)) {  /* gen soln x=TU */
			(**X).m = n;   
			(**X).n = n-rank;	/* number of free variables */
			for (j=0; j<n; j++) {
				(**X).constant[j] = 0;
				for (i=0; i<rank; i++) {
					(**X).constant[j] += T[i]*U[i*n+j];
				}
				for (i=rank; i<n; i++) {
				      (**X).vars[(i-rank)*(**X).m+j]= U[i*n+j];
				}
			}
			coeff *A3 =  A.copy();
			coeff *X2 = (*X)->copy();
			ind = FALSE;
			hashtables.X->add_entry(A3,X2,ind);
		} else {
			coeff *A3 =  A.copy();
			ind = TRUE;
			hashtables.X->add_entry(A3,(coeff *)0,ind);
		}
		delete[] T;
	} else {
		x_hit = 1; 
	}
//        printf("num UD hits, misses is %d %d \n",
//		UD_table->num_hits,UD_table->num_misses);
 //       printf("num X hits, misses is %d %d \n",
//		X_table->num_hits,X_table->num_misses);
	return(!ind);
}

