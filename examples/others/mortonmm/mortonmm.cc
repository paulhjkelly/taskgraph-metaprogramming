/*
 * Matrix multiply - with Morton layout, using TaskGraph to generate
 * a loop nested to traverse the Morton layout in order.
 * This code does not use lookup tables, and walks the B array
 * in layout order.
 *
 * Paul H J Kelly  Imperial College London  July 2004
 *
 * Run this by typing "./mortonmm 0 1 256 1 0"
 * Check the #defines below to get more informative output.
 *
 * Based on flexible-layout ~jeyan/works/research/Morton/src/MMijk
 * by Jeyarajan Thiyagalingam and Olav Beckmann.
 * Scroll down to "Start of TaskGraph code" for the new stuff.
 *
 * The  "makeMMTiledUnrolled" version below unrolls the inner
 * levels of the quadtree to a selected depth,  and assumes
 * the problem size is a multiple of the unroll factor.
 * This eliminates a conditional from the inner loop
 * and allows lots of instruction scheduling etc. optimisation.
 * The result is high performance, but different rounding leads
 * to remarkably different results (more that a million times DBL_EPSILON).
 *
 */

// If you fix the tiled loop body to use "array" then you should be able to select
// non-Morton layouts:
#define MORTON
//#define ROWMAJOR
//#define COLMAJOR

#include <TaskGraph>
#include <limits>
#include <iostream>
#include <cstdio>
#include <cmath>
#include <cstdlib>
#include <cassert>
#include "layouts.h" /* Arrays layouts */
#include "timing.h"  /* Timing routines */
const FLOATTYPE OUR_EPSILON = std::numeric_limits<FLOATTYPE>::epsilon() * 1000 * 1000000;
//const FLOATTYPE OUR_EPSILON = std::numeric_limits<FLOATTYPE>::epsilon();

using namespace tg;

// These are really useful for getting more informative output

//#define DUMP
//#define PRINT_CONTROL_VARIABLES
//#define PRINT_GENERATED_CODE

int power(const int two, const int n) { int r=1; for(int i=0; i<n; i++) r *= two; return r; }

#ifdef USE_PAPI     /* if we have to use the performance counters */
#include<papi.h>
#define MAX_PAPI_EVENTS 2
#endif


/* Some Globals - but now implementation specific*/

FLOATTYPE getElement(FLOATTYPE* const M, unsigned i, unsigned j, Layout_Type l, unsigned sz)
{
  switch(l){
  case  LAYOUT_ROW_MAJOR:
    return  (M[(i)*(sz)+(j)]);
  case LAYOUT_COL_MAJOR:
    return (M[(j)*(sz)+(i)]);
#if defined MORTON
  case LAYOUT_MORTON:
    return (M[Tab1[(i)]+Tab0[(j)]]);
#elif defined FOURDMORTON
  case LAYOUT_FOURDMORTON:
    return (M[Tab1[(i)]+Tab0[(j)]]);
#endif
  default:
    fprintf(stderr, "No layout defined\n");
    exit(EXIT_FAILURE);
  }
}

FLOATTYPE* getAddress(FLOATTYPE* const M, unsigned i, unsigned j, Layout_Type l, unsigned sz)
{
  switch(l){
  case  LAYOUT_ROW_MAJOR:
    return  &(M[(i)*(sz)+(j)]);
  case LAYOUT_COL_MAJOR:
    return &(M[(j)*(sz)+(i)]);
#if defined MORTON
  case LAYOUT_MORTON:
    return &(M[Tab1[(i)]+Tab0[(j)]]);
#elif defined FOURDMORTON
  case LAYOUT_FOURDMORTON:
    return &(M[Tab1[(i)]+Tab0[(j)]]);
#endif
  default:
    fprintf(stderr, "No layout defined\n");
    exit(EXIT_FAILURE);
  }
}


Layout_Type layout_map()
{
  Layout_Type  layout_val;
#if defined MORTON
  layout_val=LAYOUT_MORTON;
#elif defined ROWMAJOR
  layout_val=LAYOUT_ROW_MAJOR;
#elif defined COLMAJOR
  layout_val=LAYOUT_COL_MAJOR;
#elif defined FOURDMORTON
  layout_val=LAYOUT_FOURDMORTON;
#endif
  return layout_val;
}

/* Matrix filling routine for the mmijk  - base version
   where the layout is assumed to be in row-major
*/
void fill_matrix_base_version(const unsigned int sz, FLOATTYPE* const M)
{
  for(unsigned int i = 0; i < sz; ++i) {
    for(unsigned int j = 0; j < sz; ++j) {
      //      M[ROWMAJOR_MAP(i,j,sz)] = drand48();
      M[ROWMAJOR_MAP(i,j,sz)] = i*1.0 + j/1000.0;
    }
  }
  return;
}

void fill_matrix(const unsigned int sz, FLOATTYPE* const M)
{
  static int c = 0;

  for(unsigned int i = 0; i < sz; ++i) {
    for(unsigned int j = 0; j < sz; ++j) {
      // array(M,i,j,sz) = drand48();
      // array(M,i,j,sz) = i>j ? i*1.0 + j/1000.0  : i*2.0 + j/1000.0  ;
      array(M,i,j,sz) = 0.001*(c+i+j); ++c;
    }
  }
}

void zero_matrix_base_version(const unsigned int sz, FLOATTYPE* const M)
{
  for(unsigned int i = 0; i < sz; ++i) {
    for(unsigned int j = 0; j < sz; ++j) {
      M[ROWMAJOR_MAP(i,j,sz)] = 0.0;
    }
  }
}

void zero_matrix(const unsigned int sz, FLOATTYPE* const M)
{
  zero_matrix_base_version(sz,M);
}


void print_matrix( const unsigned int sz, FLOATTYPE* const M ) 
{
  for( unsigned int i = 0; i < sz; ++i ) {
    for( unsigned int j = 0; j < sz; ++j ) {
      printf("%1.3f ",array(M,i,j,sz));
    }
    printf("\n");
  }
}


/* Compare two matrices which are in different layouts */
/* Since the user knows that the comparison is going to be slow enough */
/* we can call functions to map the matrices */
unsigned  compare_matrices( const unsigned int sz, FLOATTYPE* const M , Layout_Type l1, FLOATTYPE* const N , Layout_Type l2)
{
  FLOATTYPE diff;
  for(unsigned int i = 0; i < sz; ++i) {
    for(unsigned int j = 0; j < sz; ++j) {
      diff = fabs( getElement(M,i,j,l1,sz) - getElement(N,i,j,l2,sz));
      if( diff > OUR_EPSILON ) {
	printf("\nError at %d,%d\n",i,j);
	printf("\nElements are %.15f\t%.15f\n",getElement(M,i,j,l1,sz),getElement(N,i,j,l2,sz));
	printf("\nDelta is  at %.15f\n",diff);
	return 1;
      }
    }
  }

  return 0;
}




unsigned copy_matrices(const unsigned int sz, FLOATTYPE* const SRC, Layout_Type l1, FLOATTYPE* const DST, Layout_Type l2)
{
  for( unsigned int i = 0; i < sz; ++i ) {
    for( unsigned int j = 0; j < sz; ++j ) {
      *(getAddress(DST,i,j,l2,sz))=getElement(SRC,i,j,l1,sz);
    }
  }

  return 0;
}


/* This is the mmijk  Base version - where the two arrays are in row-major form
   and the check is made against this version
*/
double mmijk_base_version(unsigned sz, FLOATTYPE* const A, FLOATTYPE* const B, FLOATTYPE* const C)
{
  tstart();

  for (unsigned int i=0;i<sz;i++){
    for (unsigned int j=0;j<sz;j++){
      for (unsigned int k=0;k<sz;k++){
	C[ROWMAJOR_MAP(i,j,sz)]+=A[ROWMAJOR_MAP(i,k,sz)]*B[ROWMAJOR_MAP(k,j,sz)];
      }
    }
  }

  tend();

  return tval();
}


double mmijk_basic(unsigned sz,FLOATTYPE* const A,FLOATTYPE* const B,FLOATTYPE* const C)
{
  tstart();

  for (unsigned int i=0;i<sz;i++){
    for (unsigned int j=0;j<sz;j++){
      for (unsigned int k=0;k<sz;k++){
	array(C,i,j,sz)+=array(A,i,k,sz)*array(B,k,j,sz);
      }
    }
  }

  tend();
  return tval();
}


/*****************************************************************************/
/************ Start of Taskgraph code ****************************************/
/*****************************************************************************/

/* Simple MM, no tiling.  Uses the "array" macro so works on layout determined
 * by #define MORTON, #define COLMAJOR etc.
 */

typedef tuTaskGraph code_MatrixToMatrixToMatrixToVoid;

code_MatrixToMatrixToMatrixToVoid makeMMSimple(const unsigned sz)
{
  code_MatrixToMatrixToMatrixToVoid TG;

  unsigned int vectorSize[1] = {sz};
  unsigned int matrixSize[1] = {sz * sz};

  tu_taskgraph(TG) 
  {
    tParameter(tArrayFromList(unsigned, Tab0, 1, vectorSize));
    tParameter(tArrayFromList(unsigned, Tab1, 1, vectorSize));
    tParameter(tArrayFromList(double, A, 1, matrixSize));
    tParameter(tArrayFromList(double, B, 1, matrixSize));
    tParameter(tArrayFromList(double, C, 1, matrixSize));

    tVar(int, i);
    tVar(int, j);
    tVar(int, k);

    tFor(i, 0, sz-1) {
      tFor(j, 0, sz-1) {
	tFor(k, 0, sz-1) {
	  array(C,i,j,sz)+=array(A,i,k,sz)*array(B,k,j,sz);
	}
      }
    }
  }
  return TG;
}

/* Tiled MM
 *
 * The function the client should call is below.
 *
 * This function handles the recursive case:
 *
 *   - We assume the caller has opened the "taskgraph(5)(TG, Tab0, Tab1, A, B, C) {" scope.
 *     This function works by adding nodes to its caller's taskgraph.
 *     Personally I think this is a hacky way to work and we should explicitly
 *     return an "open" TaskGraph (open in that it refers to outer variables).
 *
 *   - The trickiest bit is to keep track of the i, j and k values.
 *     These are calculated from the caller's i, j and k values, i_n, j_n and k_n.
 *     Except at the outermost level.
 *
 *   - We use the same trick to accumulate the dilated representation
 *     of i,j and k (we accumulate the D0(i), D0(j), D0(k) values; if you
 *     need D1() you need to shift).  The optimal thing to do depends on
 *     how the loop control variables are used in the loop body.
 *
 *   If your loop body doesn't use i,j,k (only dilated versions) hopefully
 *   the compiler's induction variable substitution will sort everything out (?).
 *   (actually no since the outer tIf uses them...).
 *
 *   Note that we have to make Tab0 and Tab1 parameters are they are used in
 *   the "array" macro (this doesn't affect the generated code, so it's no loss
 *   if we don't use the array macro after all).
 */

template<typename T1,typename T2>
void makeMMTiledRecursive(unsigned sz, int level,
			  TaskArray<1,T1> Tab0, TaskArray<1,T1> Tab1,
			  TaskArray<1,T2> A, TaskArray<1,T2> B, TaskArray<1,T2> C,
			  TaskScalarVariable i_n, TaskScalarVariable j_n,TaskScalarVariable k_n,
			  TaskScalarVariable Di_n, TaskScalarVariable Dj_n,TaskScalarVariable Dk_n)
{
  tVar(int, i0);
  tVar(int, j0);
  tVar(int, k0);
  tVar(int, Di0);
  tVar(int, Dj0);
  tVar(int, Dk0);

  tIf (i_n < sz && j_n < sz && k_n < sz) {  // This deals with SIZE != 2^k but hurts performance
    if (level == 0) {
#ifdef PRINT_CONTROL_VARIABLES
      tPrintf("Iteration i=%4d, j=%4d, k=%d ", i_n, j_n, k_n);
      tPrintf("Di=%4d Dj=%4d Dk=%4d\t", Di_n, Dj_n, Dk_n);
      tPrintf("D0[i] + D1[k]=%4d\t", (Di_n << 1) + Dk_n);
      tPrintf("D0[k] + D1[j]=%4d\n", (Dk_n << 1) + Dj_n);
#endif
      // We can either use the array macro and work on selected layout:
      // array(C,i_n,j_n,sz)+=array(A,i_n,k_n,sz)*array(B,k_n,j_n,sz);
      // Or we can calculate the Morton addresses directly, without tables:
      C[(Di_n<<1)+Dj_n] += A[(Di_n<<1)+Dk_n] * B[(Dk_n<<1)+Dj_n];
    }
    else {
      Di0 = 0;
      tForStep(i0, 0, power(2,level)-1, power(2,(level-1))) {
        Dj0 = 0;
        tForStep(j0, 0, power(2,level)-1, power(2,(level-1))) {
	  Dk0 = 0;
	  tForStep(k0, 0, power(2,level)-1, power(2,(level-1))) {
	    tVar(int, i_nplus1);
	    tVar(int, j_nplus1);
	    tVar(int, k_nplus1);
	    tVar(int, Di_nplus1);
	    tVar(int, Dj_nplus1);
	    tVar(int, Dk_nplus1);
	    i_nplus1 = i_n+i0; // accumulate correct values for i and j and k
	    j_nplus1 = j_n+j0;
	    k_nplus1 = k_n+k0;
	    Di_nplus1 = Di_n+Di0; // accumulate correct values for Di and Dj and Dk
	    Dj_nplus1 = Dj_n+Dj0;
	    Dk_nplus1 = Dk_n+Dk0;

	    makeMMTiledRecursive(sz, level-1,
				 Tab0, Tab1,
				 A, B, C,
				 i_nplus1, j_nplus1, k_nplus1,
				 Di_nplus1, Dj_nplus1, Dk_nplus1);

	    Dk0 += power(2,2*(level-1));
	  }
	  Dj0 += power(2,2*(level-1));
	}
	Di0 += power(2,2*(level-1));
      }
    }
  }
}

/* Tiled MM - this is the function the client calls
 *
 * Generates code to multiply 2^k x 2^k matrices of sizes up to 2^level.
 */

code_MatrixToMatrixToMatrixToVoid makeMMTiled(unsigned sz, int level)
{
  code_MatrixToMatrixToMatrixToVoid TG;

  unsigned int vectorSize[1] = {sz};
  unsigned int matrixSize[1] = {sz * sz};

  tu_taskgraph(TG) 
  {    
    tParameter(tArrayFromList(unsigned, Tab0, 1, vectorSize));
    tParameter(tArrayFromList(unsigned, Tab1, 1, vectorSize));
    tParameter(tArrayFromList(double, A, 1, matrixSize));
    tParameter(tArrayFromList(double, B, 1, matrixSize));
    tParameter(tArrayFromList(double, C, 1, matrixSize));

    tVar(int, i0);
    tVar(int, j0);
    tVar(int, k0);
    i0 = 0;
    j0 = 0;
    k0 = 0;
    makeMMTiledRecursive(sz, level,
			 Tab0, Tab1,
			 A, B, C,
			 i0, j0, k0, // i,j.k initially zero
			 i0, j0, k0  // Dilated i,j,k also initially zero
			 );
  }

  return TG;
}




/***********************************************************************************/
/* Tiled MM - with unrolled innermost loops
 *
 * This version is the same as the one above except that at a selected level,
 * it switches to a variant that unrolls all the for loops.
 *
 * The function the client should call is below.
 *
 * This function handles the recursive case:
 *
 *   - We assume the caller has opened the "taskgraph(5)(TG, Tab0, Tab1, A, B, C) {" scope.
 *     This function works by adding nodes to its caller's taskgraph.
 *     Personally I think this is a hacky way to work and we should explicitly
 *     return an "open" TaskGraph (open in that it refers to outer variables).
 *
 *   - The trickiest bit is to keep track of the i, j and k values.
 *     These are calculated from the caller's i, j and k values, i_n, j_n and k_n.
 *     Except at the outermost level.
 *
 *   - We use the same trick to accumulate the dilated representation
 *     of i,j and k (we accumulate the D0(i), D0(j), D0(k) values; if you
 *     need D1() you need to shift).  The optimal thing to do depends on
 *     how the loop control variables are used in the loop body.
 *
 *   If your loop body doesn't use i,j,k (only dilated versions) hopefully
 *   the compiler's induction variable substitution will sort everything out (?).
 *   (actually no since the outer tIf uses them...).
 *
 *   Note that we have to make Tab0 and Tab1 parameters are they are used in
 *   the "array" macro (this doesn't affect the generated code, so it's no loss
 *   if we don't use the array macro after all).
 */

// This generates a fully-unrolled matrix multiply recursive loop nest.
// It's called as the base case of the similar recursive function below it.

template<typename T1,typename T2>
void makeMMTiledRecursiveUnrolled(unsigned sz, int level,
				  TaskArray<1,T1> Tab0, TaskArray<1,T1> Tab1,
				  TaskArray<1,T2> A, TaskArray<1,T2> B, TaskArray<1,T2> C,
				  TaskScalarVariable i_n, TaskScalarVariable j_n,TaskScalarVariable k_n,
				  TaskScalarVariable Di_n, TaskScalarVariable Dj_n,TaskScalarVariable Dk_n)
{
  tVar(int, Di0);
  tVar(int, Dj0);
  tVar(int, Dk0);

  // This should be redundant for sz a multiple of 2^unrollLevel but it isn't
  /*  tIf (i_n >= sz || j_n >= sz || k_n >= sz) {
    tPrintf("!!Anomalous iteration i=%4d, j=%4d, k=%d ", i_n, j_n, k_n);
    }*/
  //  tIf (i_n < sz && j_n < sz && k_n < sz)   // This deals with SIZE != 2^k but hurts performance
    if (level == 0) {
#ifdef PRINT_CONTROL_VARIABLES
      tPrintf("Iteration i=%4d, j=%4d, k=%d ", i_n, j_n, k_n);
      tPrintf("Di=%4d Dj=%4d Dk=%4d\t", Di_n, Dj_n, Dk_n);
      tPrintf("D0[i] + D1[k]=%4d\t", (Di_n << 1) + Dk_n);
      tPrintf("D0[k] + D1[j]=%4d\n", (Dk_n << 1) + Dj_n);
#endif
      // We can either use the array macro and work on selected layout:
      // array(C,i_n,j_n,sz)+=array(A,i_n,k_n,sz)*array(B,k_n,j_n,sz);
      // Or we can calculate the Morton addresses directly, without tables:
      C[(Di_n<<1)+Dj_n] += A[(Di_n<<1)+Dk_n] * B[(Dk_n<<1)+Dj_n];
    }
    else {
      Di0 = 0;
      for(int i0 = 0; i0 <= power(2,level)-1; i0 += power(2,(level-1))) {
        Dj0 = 0;
        for (int j0 = 0; j0 <= power(2,level)-1; j0 += power(2,(level-1))) {
	  Dk0 = 0;
	  for (int k0 = 0; k0 <= power(2,level)-1; k0 += power(2,(level-1))) {
	    tVar(int, i_nplus1);
	    tVar(int, j_nplus1);
	    tVar(int, k_nplus1);
	    tVar(int, Di_nplus1);
	    tVar(int, Dj_nplus1);
	    tVar(int, Dk_nplus1);
	    i_nplus1 = i_n+i0; // accumulate correct values for i and j and k
	    j_nplus1 = j_n+j0;
	    k_nplus1 = k_n+k0;
	    Di_nplus1 = Di_n+Di0; // accumulate correct values for Di and Dj and Dk
	    Dj_nplus1 = Dj_n+Dj0;
	    Dk_nplus1 = Dk_n+Dk0;

	    makeMMTiledRecursiveUnrolled(sz, level-1,
				 Tab0, Tab1,
				 A, B, C,
				 i_nplus1, j_nplus1, k_nplus1,
				 Di_nplus1, Dj_nplus1, Dk_nplus1);

	    Dk0 += power(2,2*(level-1));
	  }
	  Dj0 += power(2,2*(level-1));
	}
	Di0 += power(2,2*(level-1));
      }

  }
}




template<typename T1,typename T2>
void makeMMTiledRecursivePartiallyUnrolled(unsigned sz, int level, int unrollLevel,
					   TaskArray<1,T1> Tab0, TaskArray<1,T1> Tab1,
					   TaskArray<1,T2> A, TaskArray<1,T2> B, TaskArray<1,T2> C,
					   TaskScalarVariable i_n, TaskScalarVariable j_n,TaskScalarVariable k_n,
					   TaskScalarVariable Di_n, TaskScalarVariable Dj_n,TaskScalarVariable Dk_n)
{
  tVar(int, i0);
  tVar(int, j0);
  tVar(int, k0);
  tVar(int, Di0);
  tVar(int, Dj0);
  tVar(int, Dk0);

  tIf (i_n < sz && j_n < sz && k_n < sz)   // This deals with SIZE != 2^k but hurts performance
    {
      if (level <= unrollLevel) {
	  {
#ifdef PRINT_CONTROL_VARIABLES
	    tPrintf("makeMMTiledRecursiveUnrolled(%d, %d, ... ", sz, level);
	    tPrintf(                                          "i_n=%d, j_n=%d, k_n=%d)\n", i_n, j_n, k_n);
#endif
	    makeMMTiledRecursiveUnrolled(sz, level,
					 Tab0, Tab1,
					 A, B, C,
					 i_n, j_n, k_n,
					 Di_n, Dj_n, Dk_n);
	  }
      }
      else {
	Di0 = 0;
	tForStep(i0, 0, power(2,level)-1, power(2,(level-1))) {
	  Dj0 = 0;
	  tForStep(j0, 0, power(2,level)-1, power(2,(level-1))) {
	    Dk0 = 0;
	    tForStep(k0, 0, power(2,level)-1, power(2,(level-1))) {
	      tVar(int, i_nplus1);
	      tVar(int, j_nplus1);
	      tVar(int, k_nplus1);
	      tVar(int, Di_nplus1);
	      tVar(int, Dj_nplus1);
	      tVar(int, Dk_nplus1);
	      i_nplus1 = i_n+i0; // accumulate correct values for i and j and k
	      j_nplus1 = j_n+j0;
	      k_nplus1 = k_n+k0;
	      Di_nplus1 = Di_n+Di0; // accumulate correct values for Di and Dj and Dk
	      Dj_nplus1 = Dj_n+Dj0;
	      Dk_nplus1 = Dk_n+Dk0;

	      makeMMTiledRecursivePartiallyUnrolled(sz, level-1, unrollLevel,
						    Tab0, Tab1,
						    A, B, C,
						    i_nplus1, j_nplus1, k_nplus1,
						    Di_nplus1, Dj_nplus1, Dk_nplus1);

	      Dk0 += power(2,2*(level-1));
	    }
	    Dj0 += power(2,2*(level-1));
	  }
	  Di0 += power(2,2*(level-1));
	}
      }
    }
}

  /* Tiled MM - this is the function the client calls
 *
 * Generates code to multiply 2^k x 2^k matrices of sizes up to 2^level.
 */

code_MatrixToMatrixToMatrixToVoid makeMMTiledUnrolled (unsigned sz, int level, int unrollLevel)
{
  code_MatrixToMatrixToMatrixToVoid TG;

  unsigned int vectorSize[1] = {sz};
  unsigned int matrixSize[1] = {sz * sz};

  tu_taskgraph(TG) 
  {    
    tParameter(tArrayFromList(unsigned, Tab0, 1, vectorSize));
    tParameter(tArrayFromList(unsigned, Tab1, 1, vectorSize));
    tParameter(tArrayFromList(double, A, 1, matrixSize));
    tParameter(tArrayFromList(double, B, 1, matrixSize));
    tParameter(tArrayFromList(double, C, 1, matrixSize));

    tVar(int, i0);
    tVar(int, j0);
    tVar(int, k0);
    i0 = 0;
    j0 = 0;
    k0 = 0;
    makeMMTiledRecursivePartiallyUnrolled(sz, level, unrollLevel,
			 Tab0, Tab1,
			 A, B, C,
			 i0, j0, k0, // i,j.k initially zero
			 i0, j0, k0  // Dilated i,j,k also initially zero
			 );
  }

  return TG;
}


/*****************************************************************************/
/************ End of Taskgraph code (apart from invocation in main) **********/
/*****************************************************************************/


int main(int argc, char* argv[])
{
  unsigned int samples,sz,debug,check,align;
  FLOATTYPE *A, *B,*C, *REFERENCE_1,*REFERENCE_2,*REFERENCE_3;
  unsigned int layout_val;
  double t_taken;

#if defined USE_PAPI
  /* we are interested only in two events:
     L1 and L2 misses (or their missrates)
     Unfortunately, PAPI doesn't permit
     to count L1 and L2 misses at the same time (especially
     under the x86 clones)
     so, I am not so sure, how do we determine the L2
     miss rate? (which is PAPI_L2_DCM/PAPI_L1_DCM)

     Until we resolve this, it is a better idea to count only
     the L1 miss rate (PAPI_L1_DCM/PAPI_LST_INS)
  */
  long long eventValues[2]={0,0}; /* to hold the event outcomes */
  int nHWCounter;                 /* Number of HW counters */
  int res;
#if defined PAPI_L1_COUNT
  int Events[MAX_PAPI_EVENTS]={PAPI_L1_DCM,PAPI_L2_DCM};
#elif defined PAPI_L2_COUNT
  int Events[MAX_PAPI_EVENTS]={PAPI_L2_DCM,PAPI_LST_INS};
#endif
#endif

  layout_val=layout_map();

  if( argc < 6 ) {
    printf( " Usage: %s <debug> <check-result> <data-size> <align> <samples>\n",*argv );
    exit(EXIT_FAILURE);
  }

  debug   = atoi( *(++argv) );
  check   = atoi( *(++argv) );
  sz      = atoi( *(++argv) );
  align   = atoi( *(++argv) );
  samples = atoi( *(++argv) );
  /* ignore the last one -- number of sweeps*/

  A  = allocate_matrix( sz, align, "A" ,layout_val);
  B  = allocate_matrix( sz, align, "B" ,layout_val);
  C  = allocate_matrix( sz, align, "C" ,layout_val);
#ifdef FOURDMORTON
  Tab1 = allocate_vector(getNextPowerOfTwo(sz),"Tab1");
  Tab0  = allocate_vector(getNextPowerOfTwo(sz),"Tab0");
  Fill4DMortonTables(Tab1,Tab0,sz);
#endif

#ifdef MORTON
  Tab1 = allocate_vector(getNextPowerOfTwo(sz),"Tab1");
  Tab0  = allocate_vector(getNextPowerOfTwo(sz),"Tab0");
  FillMortonTables(Tab1,Tab0,sz);
#endif

  if( debug){
    printf("Layout - Preferred  = %d\n",layout_val);
    printf("Size   - Required   = %d\n",sz);
    printf("Size   - Declared   = %d OR %d\n",getNextPowerOfTwo(sz), (sz+(MIN_TILE_SIZE-1))&~(MIN_TILE_SIZE-1));
  }

  //  srand48(time(NULL));
  srand(12);
  fill_matrix(sz,A);
  fill_matrix(sz,B);
  zero_matrix(sz,C);

  if( check ) {
    /* perform basic computation and keep the result */
    /* We will allocate the matrix through calloc -- to avoid more complicated calls */

    REFERENCE_1 = allocate_matrix( sz, align, "REF1", LAYOUT_ROW_MAJOR);
    REFERENCE_2 = allocate_matrix( sz, align, "REF2", LAYOUT_ROW_MAJOR);
    REFERENCE_3 = allocate_matrix( sz, align, "REF3", LAYOUT_ROW_MAJOR);


    copy_matrices(sz, A, layout_val, REFERENCE_1, LAYOUT_ROW_MAJOR);
    copy_matrices(sz, B, layout_val, REFERENCE_2, LAYOUT_ROW_MAJOR);
    zero_matrix_base_version(sz, REFERENCE_3);

    /* TODO: Improve this part */

    mmijk_base_version(sz,REFERENCE_1,REFERENCE_2,REFERENCE_3); /* Ignore the timing result -- may be we don't bother much */
  }
  else
  {
    REFERENCE_1=NULL;
    REFERENCE_2=NULL;
    REFERENCE_3=NULL;
  }

  tstart();
  // The number of levels of the quadtree to generate loops for.
  const int levels = static_cast<int>(std::ceil(std::log(sz)/std::log(2.0))); 
  code_MatrixToMatrixToMatrixToVoid TG = makeMMTiledUnrolled(sz, levels, 2);
  tend();
  t_taken = tval();
  printf("TG = makeMMTiledUnrolled(%d,%d,2) time: %.5f\n", sz, levels, t_taken*1000000.0);

#ifdef PRINT_GENERATED_CODE
  TG.print();
#endif
  tstart();

  TG.compile(tg::GCC, true);
  tend();
  t_taken = tval();
  printf("TG.compile() time: %.5f\n",t_taken*1000000.0);


#if defined USE_PAPI
  nHWCounter=PAPI_num_counters();
  if (nHWCounter>=MAX_PAPI_EVENTS)
    nHWCounter=MAX_PAPI_EVENTS;
  else{ /*i.e. we have very few counters working on ...*/
    printf("Possibly, unsupported platform for PAPI -- very few events \n");
    exit(EXIT_FAILURE);
  }
  if ((res=PAPI_start_counters(Events, nHWCounter) )!= PAPI_OK) {
    printf("Error when starting up the counters %d \n",res);
    PAPI_perror(res,NULL,NULL);
    exit(EXIT_FAILURE);
  }
#endif


 //     typedef double (AP)[16];
   //   TG.execute(static_cast<AP>(A),static_cast<AP>(B),static_cast<AP>(C));

   //   typedef double (&AT)[SIZE];
   //   double (*myA)[SIZE];
   //   TG.execute(*myA,*myA,*myA);

   //   double (*AP)[SIZE] = (double(*)[SIZE])A;
   //   TG.execute(*AP, *AP, *AP);
/*
  unsigned int myTab0[SIZE];
  unsigned int myTab1[SIZE];
   double myA[SIZE*SIZE];
   double myB[SIZE*SIZE];
   double myC[SIZE*SIZE];
   for (int i=0; i<SIZE; ++i) {myTab0[i]=Tab0[i];}
   for (int i=0; i<SIZE; ++i) {myTab1[i]=Tab1[i];}
   for (int i=0; i<SIZE*SIZE; ++i) {myA[i]=A[i];}
   for (int i=0; i<SIZE*SIZE; ++i) {myB[i]=B[i];}
   for (int i=0; i<SIZE*SIZE; ++i) {myC[i]=C[i];}
*/
   tstart();
  // TG.execute(myTab0, myTab1, myA, myB, myC);
   //mmijk_basic(sz,myA,myB,myC);

   TG.setParameter("Tab0", Tab0);
   TG.setParameter("Tab1", Tab1);
   TG.setParameter("A", A);
   TG.setParameter("B", B);
   TG.setParameter("C", C);
   TG.execute();

   tend();
   t_taken = tval();
   printf("TG.execute() time %.5f (%.5f MFlops)\n",t_taken*1000000.0, (sz*sz*sz*2)/t_taken/1000000);

//   for (int i=0; i<SIZE*SIZE; ++i) {C[i]=myC[i];}


#if defined USE_PAPI
  if ((res=PAPI_read_counters(eventValues,nHWCounter) ) != PAPI_OK){
    printf("Error when reading up the counters %d\n",res);
    PAPI_perror(res,NULL,NULL);
    exit(EXIT_FAILURE);
  }
#endif


  if(check)
  {
    if(debug) printf("Check is Enabled - Checking the answers\n");
    if (!compare_matrices(sz, C,layout_val,REFERENCE_3, LAYOUT_ROW_MAJOR))
      printf("No Errors\n");
  }
  else
  {
#if defined USE_PAPI
    printf("%d\t%d\n",eventValues[0],eventValues[1]);
#else
    printf("%.5f\n",t_taken*1000000.0);
#endif
  }



#ifdef DUMP
  printf("A:\n");
  print_matrix(sz,A);
  printf("B:\n");
  print_matrix(sz,B);
  printf("C:\n");
  print_matrix(sz,C);
#endif

  free_matrix(A);
  free_matrix(B);
  free_matrix(C);

  if (REFERENCE_1 != NULL) free_matrix( REFERENCE_1 );
  if (REFERENCE_2 != NULL) free_matrix( REFERENCE_2 );
  if (REFERENCE_3 != NULL) free_matrix( REFERENCE_3 );

  return EXIT_SUCCESS;
}

