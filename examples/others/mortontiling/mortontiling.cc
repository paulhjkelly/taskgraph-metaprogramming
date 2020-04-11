/*
 * TaskGraph matrix transpose
 *
 * Paul H J Kelly  Imperial College London  July 2004
 *
 */

#include "TaskGraph"
#include <cstdio>
#include <iostream>

using namespace tg;

#define SIZE 11
#define NREPEATS 58
#define PRINT
typedef TaskGraph<void, double[SIZE][SIZE], double[SIZE][SIZE]> code_MatrixToMatrixToVoid;

#define min(aa,bb) ((aa) < (bb)? (aa) : (bb))

int power(int two, int n) { int r=1; for(int i=0; i<n; i++) r *= two; return r; }

/* Simple transpose, no tiling
 */

code_MatrixToMatrixToVoid makeTransposeSimple ()
{
  code_MatrixToMatrixToVoid TG;

  taskgraph(code_MatrixToMatrixToVoid, TG, tuple2(A, B)) {
    tVar(int, i);
    tVar(int, j);

    tFor(i, 0, SIZE-1) {
      tFor(j, 0, SIZE-1) {
	B[i][j] = A[j][i];
      }
    }
  }
  return TG;
}

/* Tiled transpose: manually tiled to depth 2^3=8.
 * Works when SIZE=8.
 */

code_MatrixToMatrixToVoid makeTranspose8x8Tiled ()
{
  code_MatrixToMatrixToVoid TG;

  taskgraph(code_MatrixToMatrixToVoid, TG, tuple2(A, B)) {
    tVar(int, i);
    tVar(int, j);
    tVar(int, i0);
    tVar(int, j0);

    tForStep(i0, 0, SIZE-1, SIZE/2) {
      tForStep(j0, 0, SIZE-1, SIZE/2) {
        i = i0;
	j = j0;

	tVar(int, i1);
	tVar(int, j1);
	tForStep(i1, 0, SIZE/2-1, SIZE/2/2) {
	  tForStep(j1, 0, SIZE/2-1, SIZE/2/2) {

	    i = i0+i1;
	    j = j0+j1;

	    tVar(int, i2);
	    tVar(int, j2);
	    tForStep(i2, 0, SIZE/2/2-1, SIZE/2/2/2) {
	      tForStep(j2, 0, SIZE/2/2-1, SIZE/2/2/2) {

		i = i0+i1+i2;
		j = j0+j1+j2;

		B[i][j] = A[j][i];
	      }
	    }
	  }
	}
      }
    }
  }
  return TG;
}

/* Tiled transpose: in 8x8 blocks, manually tiled to depth 2^3=8.
 * Works for power-of-two matrix sizes bigger than 8x8.  Same as above but outermost
 * loop goes over 8x8 blocks.
 */

template<typename T>
code_MatrixToMatrixToVoid makeTranspose8x8TiledBlockwise (code_MatrixToMatrixToVoid& TG, TaskArray<2,T> A, TaskArray<2,T> B)
{
  taskgraph(code_MatrixToMatrixToVoid, TG, tuple2(A, B)) {
    tVar(int, i);
    tVar(int, j);
    tVar(int, i0);
    tVar(int, j0);

    tForStep(i0, 0, SIZE-1, 4) {
      tForStep(j0, 0, SIZE-1, 4) {
        i = i0;
	j = j0;

	tVar(int, i1);
	tVar(int, j1);
	tForStep(i1, 0, 4-1, 2) {
	  tForStep(j1, 0, 4-1, 2) {

	    i = i0+i1;
	    j = j0+j1;

	    tVar(int, i2);
	    tVar(int, j2);
	    tForStep(i2, 0, 2-1, 1) {
	      tForStep(j2, 0, 2-1, 1) {

		i = i0+i1+i2;
		j = j0+j1+j2;

		B[i][j] = A[j][i];
	      }
	    }
	  }
	}
      }
    }
  }
  return TG;
}



/* Tiled transpose: recursively tiled to arbitrary depth.
 * Make sure SIZE <= 2^level.
 *
 * The function the client should call is below.
 *
 * This function handles the recursive case:
 *
 *   - We assume the caller has opened the "taskgraph(TG, tuple2(A, B)) {" scope.
 *     This function works by adding nodes to its caller's taskgraph.
 *
 *   - The trickiest bit is to keep track of the i and j values.
 *     These are calculated from the caller's i and j values, i_n and j_n.
 *     Except at the outermost level.
 *
 *   - We use the same trick to accumulate the dilated representation
 *     of i and j (we accumulate the D0(i) and D0(j) values; if you
 *     need D1() you need to shift).
 *
 *   If your loop body doesn't use i an j (only dilated versions) hopefully
 *   the compiler's induction variable substitution will sort everything out (?).
 */

template<typename T>
void makeTransposeNxNTiledRecursive (int level,
				     TaskArray<2,T> A, TaskArray<2,T> B,
				     TaskScalarVariable i_n, TaskScalarVariable j_n,
				     TaskScalarVariable Di_n, TaskScalarVariable Dj_n)
{
  tVar(int, i0);
  tVar(int, j0);
  tVar(int, Di0);
  tVar(int, Dj0);

  tIf (i_n < SIZE && j_n < SIZE) {  // This deals with SIZE != 2^k but hurts performance
    if (level == 0) {
      tPrintf("Iteration i=%4d, j=%4d ", i_n, j_n);
      tPrintf("Di=%4d Dj=%4d\t", Di_n, Dj_n);
      tPrintf("D0[i] + D1[j]=%4d\t", Di_n + (Dj_n << 1));
      tPrintf("D0[j] + D1[i]=%4d\n", Dj_n + (Di_n << 1));
      B[i_n][j_n] = A[j_n][i_n];
    }
    else {
      Di0 = 0;
      tForStep(i0, 0, power(2,level)-1, power(2,(level-1))) {
	Dj0 = 0;
	tForStep(j0, 0, power(2,level)-1, power(2,(level-1))) {
	  tVar(int, i_nplus1);
	  tVar(int, j_nplus1);
	  tVar(int, Di_nplus1);
	  tVar(int, Dj_nplus1);
	  i_nplus1 = i_n+i0; // accumulate correct values for i and j
	  j_nplus1 = j_n+j0;
	  Di_nplus1 = Di_n+Di0; // accumulate correct values for Di and Dj
	  Dj_nplus1 = Dj_n+Dj0;

	  makeTransposeNxNTiledRecursive(level-1, A, B, i_nplus1, j_nplus1, Di_nplus1, Dj_nplus1);
	  Dj0 += power(2,2*(level-1));
	}
	Di0 += power(2,2*(level-1));
      }
    }
  }
}

/* This is the function the client calls.
 *
 * Generates code to transpose 2^k x 2^k matrices of sizes up to 2^level.
 */
code_MatrixToMatrixToVoid makeTransposeNxNTiled (int level)
{
  code_MatrixToMatrixToVoid TG;

  taskgraph(code_MatrixToMatrixToVoid, TG, tuple2(A, B)) {
    tVar(int, i0);
    tVar(int, j0);
    i0 = 0;
    j0 = 0;
    makeTransposeNxNTiledRecursive(level, A, B, i0, j0, i0, j0);
  }
  return TG;
}

void initMatrix(double A[SIZE][SIZE])
{
  for (int i=0; i<SIZE; ++i)
    for (int j=0; j<SIZE; j++) {
      A[i][j] = i*1.0 + j/1000.0;
    }
}

void printMatrix(double A[SIZE][SIZE])
{
  for (int i=0; i<SIZE; ++i) {
    for (int j=0; j<SIZE; j++) {
      printf("%1.3f ", A[i][j]);
    }
    printf("\n");
  }
}


int main ( int argc, char **argv ) {
  double a[SIZE][SIZE];
  double b[SIZE][SIZE];

  initMatrix(a);
#ifdef PRINT
  printf("Initialised a:\n"); printMatrix(a);
#endif

  code_MatrixToMatrixToVoid TG = makeTransposeNxNTiled(4);

  printf("Code built\n");
//  TG.print();

  TG.compile ( tg::GCC );
  TG.execute ( a, b );
  printf("Again:\n");
  TG.execute ( a, b );

#ifdef PRINT
  printf("After transpose b:\n"); printMatrix(b);
#endif

  //  Timer timer;
  for ( unsigned i = 0; i < NREPEATS; ++i ) {
    printf("test %d\n", i);
    TG.execute ( a, b );
  }
  //  timer.stop();

  //  std::cout << timer.getTime() << " seconds, " << timer.getTime()/NREPEATS << " each\n";
  //  theTimingInfo.print ( std::cout );
}
