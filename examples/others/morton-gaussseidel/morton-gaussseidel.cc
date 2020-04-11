/****************************************************************************
 * morton-gaussseidel.cc                                                    *
 *                                                                          *
 * This is a Gauss-Seidel stencil loop (ie averages 4 neighbouring elements *
 * of 2d array).  It operates on an array stored in Morton layout, and      *
 * computes the offsets using a table-lookup based bit-interleaving trick   *
 * To reduce the overheads, the innermost loop is unrolled by a             *
 * power-of-two factor.  This means we can avoid (strength-reduce) the      *
 * offset calculation, doing the table lookups per iteration of the         *
 * unrolled loop instead of at every iteration.  The tricky bit is that the *
 * strength reduction trick doesn't work for A[i,j-1] at the first          *
 * iteration of the unrolled block, and for A[i,j+1] at the end - so we     *
 * have to resort to table lookups there.                                   *
 *                                                                          *
 * Author: Paul H J Kelly, Imperial College, 2003                           *
 *                                                                          *
 * To compile: run `make all`                                               *
 * To change MAXiDIM MAXjDIM edit TG_EXAMPLE_CCFLAGS in Makefile            *
 * (sizes must be powers of two for Morton to work)                         *
 *                                                                          *
 * To run it:                                                               *
 *                                                                          *
 * ./morton-gaussseidel 1000 1000 10                                        *
 *                                                                          *
 * This says do 10 iterations of a 1000x1000 Gauss-Seidel sweep.            *
 * The sweep must lie within DMAXiDIM x DMAXjDIM                            *
 *                                                                          *
 * VALIDATION:                                                              *
 * With the command line above you should get the following                 *
 * output:                                                                  *
 *                                                                          *
 * Command line options:                                                    *
 *  sweepi = 1000                                                           *
 *  sweepj = 1000                                                           *
 *  Niters      = 10                                                        *
 * After 10 iterations, sumcheck: 1.2210756074290104e+04                    *
 *                                                                          *
 * The sumcheck must match exactly or you're getting the wrong              *
 * answer.                                                                  *
 ****************************************************************************/

#include <TaskGraph>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <sys/time.h>
#include <math.h>

using namespace tg;

//char *prog_who_string = "dummy";
//char *prog_ver_string = "dummy";

/* These defines allow you to control the diagnostic output
 */

/* #define PRINT_GRID
 */
/* #define COMPUTE_NORM
 */
#define COMPUTE_SUMCHECK_AT_END
/* #define PRINT_COMMAND_LINE_OPTIONS
 */
#define TIME_USING_UTIME

/* The size of the arrays.  These have to be compile-time constant for C to
 * handle indexing.  They cannot be too big or you'll run out of swap space
 */

#ifndef MAXiDIM
//#define MAXiDIM 1000  /* For row-major shouldn't be a power of two to avoid cache conflicts */
#define MAXiDIM 1024  /* For Morton it must be a power of two */
#endif
#ifndef MAXjDIM
#define MAXjDIM 1024
#endif

#define ABS(x) ((x) > 0 ? (x) : -(x))
#define MAX(x,y) ((x>y) ? (x) : (y))
#define MIN(x,y) ((x<y) ? (x) : (y))


typedef double gridtype[MAXiDIM*MAXjDIM];


/****************************************************************
*	int ParseCommandLine (int argc,char **argv)		*
*	returns N, the first command line argument		*
****************************************************************/
typedef struct {
  int sweepi;
  int sweepj;
  int Niters;
} CommandLine;
CommandLine TheCommandLine;

void ParseCommandLine (int argc, char **argv)
{
  /**************************************************/
  /* Parse input values - make sure the value is OK */
  /**************************************************/

  int argcount = argc;
  char **argvp = argv+1;

  if (argcount != 4) {
    fprintf(stderr, "Usage: %s <sweepi> <sweepj> <Niters>\n", argv[0]);
    exit (-1);
  }

  TheCommandLine.sweepi = atoi(*argvp++);
  TheCommandLine.sweepj = atoi(*argvp++);
  TheCommandLine.Niters = atoi(*argvp++);

#ifdef PRINT_COMMAND_LINE_OPTIONS
  printf("Command line options:\n");
  printf(" sweepi = %d\n", TheCommandLine.sweepi);
  printf(" sweepj = %d\n", TheCommandLine.sweepj);
  printf(" Niters      = %d\n", TheCommandLine.Niters);
#endif
}

double utime () {
  struct timeval tv;

  gettimeofday (&tv, NULL);

  return (tv.tv_sec + double (tv.tv_usec) * 1e-6);
}

/****************************************************************
*****************************************************************
* Standard layout versions			                *
*****************************************************************
*****************************************************************/

#define array(A,xx,yy) (A[(xx)*MAXjDIM+(yy)])

/****************************************************************
* void InitGrid(double *grid, int iDim, int jDim)			                *
*								*
* initialize dirichlet boundary conditions			*
*****************************************************************/
void InitGrid(gridtype grid, int iDim, int jDim)
{
  int i,j;
  for (i=0; i<iDim; ++i) {
    for (j = 0; j<jDim; ++j) {
      array(grid,i,j) = 1.0;
    }
  }
  for (i=1; i<iDim-1; ++i) {
    for (j = 1; j<jDim-1; ++j) {
      array(grid,i,j) = 0.0;
    }
  }
}
/****************************************************************
* void InitGridAnnulus(double *grid, int iDim, int jDim)	*
*								*
* initialize dirichlet boundary conditions assuming previously  *
* zero (eg because freshly callocd)   		 	        *
*****************************************************************/
void InitGridAnnulus(gridtype grid, int iDim, int jDim)
{
  int i,j;
  for (i=0; i<=iDim; i+=iDim-1) {  /* for i=0 and iDim-1 only */
    for (j = 0; j<jDim; ++j) {
      array(grid,i,j) = 1.0;
    }
  }
  for (i=0; i<iDim; ++i) {
    for (j = 0; j<=jDim; j+=jDim-1) { /* for j=0 and jDim-1 only */
      array(grid,i,j) = 1.0;
    }
  }
}
/****************************************************************
* void PrintGrid(Array2dDouble grid, int iDim, int jDim)	*
*								*
* Print grid to stdout                     			*
*****************************************************************/
void PrintGrid(gridtype grid, int iDim, int jDim)
{
  int i,j;

  for (i = 0; i<iDim; ++i) {
    for (j = 0; j<jDim; ++j) {
      printf("%1.2f ", array(grid,i,j));
    }
    printf("\n");
  }
  printf("\n");
  fflush(stdout);
}

/****************************************************************
* double computeSum(Array2dDouble U,                            *
*                    int iDim, int jDim)                        *
*								*
* compute the sum of all the elements                  		*
*****************************************************************/
double computeSum(gridtype grid, int iDim, int jDim)
{
  int i,j;
  double result = 0.0;

  for (i=0; i<iDim; ++i)
    {
      for (j=0; j<jDim; ++j)
	{
	  result += array(grid,i,j);
	}
    }
  return(result);
}

/***************************************************************/
/* Simple Gauss-Seidel implementation                          */
/***************************************************************/

void gausseidelV1(gridtype grid, int iDim, int jDim, int Niters)
{
  int it;
  int i,j;
  double onequarter = 1.0/4.0;

  for (it=0; it<Niters; it+=1) {
#ifdef PRINT_GRID
    printf("grid prior to it=%d\n", it);
    PrintGrid(grid, iDim, jDim);
#endif
    for (i=1; i<iDim-1; ++i)
      for (j=1; j<jDim-1; ++j)
	array(grid,i,j) = onequarter *
	  (array(grid,i-1,j) + array(grid,i+1,j) + array(grid,i,j-1) + array(grid,i,j+1));
  }
}

/***************************************************************
****************************************************************
* Morton layout stuff
****************************************************************
****************************************************************/

#define Ones_1 0x55555555
#define Ones_0 0xaaaaaaaa

#define INT_WORDLENGTH 32

/* A routine to check whether a given value is power of two or not */

static int isPowerOfTwo(long n)
{
  while (n>1L) {
    if (n%2L) break;
    n >>= 1L;
  }
  return(n==1L);
}

/* Returns the log2 of something... - yeah that something should
   be a number
*/
double  getLog2Of(unsigned sz)
{
  const double log2     = log( 2.0 );
  const double log2sz   = log( static_cast<double>(sz) ) / log2;
  return log2sz;
}


/* Returns the previous power of two value to the given
   value - e.g 16 for 18, 32 for 35 but 16 for 16
*/


unsigned getPreviousPowerOfTwo(unsigned sz)
{
  const double log2     = log( 2.0 );
  double log2sz   = log( static_cast<double>(sz) ) / log2;
  int padded_sz    = sz;


  if (!isPowerOfTwo(sz)){
    log2sz   = log( static_cast<double>(sz) ) / log2;
    padded_sz   = static_cast<int>(pow( 2.0, floor( log2sz ) ));
  }
  return padded_sz;
}

/* Returns the next  power of two value to the given
   value - e.g 16 for 12, 32 for 25 but 16 for 16
*/

unsigned getNextPowerOfTwo(unsigned sz)
{
  const double log2     = log( 2.0 );
  double log2sz;
  int padded_sz=sz ;


  if (!isPowerOfTwo(sz)){
    log2sz   = log( static_cast<double>(sz) ) / log2;
    padded_sz   = static_cast<int>(pow( 2.0, ceil( log2sz ) ));
  }
  return padded_sz;
}

unsigned int SpreadBits(int oddeven, unsigned int bits)
{
  int i;
  unsigned int r = 0;
  for (i=0; i<=INT_WORDLENGTH/2; ++i) {
    unsigned int mask = 1 << i;
    unsigned int thisbit = mask & bits;
    unsigned int shiftedbit = thisbit << i;
    r |= shiftedbit;
  }
  return r << oddeven;
}

void PrintBits(FILE *fd, unsigned int bits)
{
  int i;
  for (i=INT_WORDLENGTH-1; i>=0; --i) {
    unsigned int mask = 1 << i;
    unsigned int thisbit = mask & bits;
    fprintf(fd, "%d", (thisbit==0) ? 0 : 1 );
  }
  fprintf(fd, "(=%d=0x%x) ", bits, bits);
}
void FillMortonTables(unsigned  tab1[], unsigned tab2[], unsigned int sz)
{
  /*
    Tab1 is Even and
    Tab2 is Odd
  */

  unsigned int i;
  /* Well find the padded size of this matrix of size sz*/
  unsigned szm=getNextPowerOfTwo(sz);
  unsigned szn=getNextPowerOfTwo(sz);

  for (i=0; i<szm; ++i) {
    tab1[i] = SpreadBits(0,i);
  }
  for (i=0; i<szn; ++i) {
    tab2[i] = SpreadBits(1,i);
  }

#ifdef _PRINT_TABLES_
  printf("\ni\t\tTab1[i]\n");
  printf("-\t\t------\n");
  for(i=0;i<sz;i++) {
    printf("%d\t\t%d = ",i,tab1[i]);
    PrintBits(stdout, tab1[i]);
    printf("\n");
  }

  printf("\n\n");
  printf("\nj\t\tTab2[j]\n");
  printf("-\t\t------\n");
  for(i=0;i<sz;i++) {
    printf("%d\t\t%d = ",i,tab2[i]);
    PrintBits(stdout, tab1[i]);
    printf("\n");
  }
#endif
}

/****************************************************************
*****************************************************************
* Morton versions                                               *
*****************************************************************
*****************************************************************/
#undef array
#define array(A,xx,yy) (A[D0[(xx)]+D1[(yy)]])

/****************************************************************
* void MortonInitGrid(double *grid, int iDim, int jDim, int D0[], int D1[]) *
*								*
* initialize dirichlet boundary conditions			*
*****************************************************************/
void MortonInitGrid(gridtype grid, int iDim, int jDim, unsigned int D0[], unsigned int D1[])
{
  int i,j;
  for (i=0; i<iDim; ++i) {
    for (j = 0; j<jDim; ++j) {
      array(grid,i,j) = 1.0;
    }
  }
  for (i=1; i<iDim-1; ++i) {
    for (j = 1; j<jDim-1; ++j) {
      array(grid,i,j) = 0.0;
    }
  }
}
/****************************************************************
* void MortonInitGridAnnulus(double *grid, int iDim, int jDim, int D0[], int D1[])	*
*								*
* initialize dirichlet boundary conditions assuming previously  *
* zero (eg because freshly callocd)   		 	        *
*****************************************************************/
void MortonInitGridAnnulus(gridtype grid, int iDim, int jDim, unsigned int D0[], unsigned int D1[])
{
  int i,j;
  for (i=0; i<=iDim; i+=iDim-1) {  /* for i=0 and iDim-1 only */
    for (j = 0; j<jDim; ++j) {
      array(grid,i,j) = 1.0;
    }
  }
  for (i=0; i<iDim; ++i) {
    for (j = 0; j<=jDim; j+=jDim-1) { /* for j=0 and jDim-1 only */
      array(grid,i,j) = 1.0;
    }
  }
}
/****************************************************************
* void MortonPrintGrid(Array2dDouble grid, int iDim, int jDim, int D0[], int D1[])	*
*								*
* Print grid to stdout                     			*
*****************************************************************/
void MortonPrintGrid(gridtype grid, int iDim, int jDim, unsigned int D0[], unsigned int D1[])
{
  int i,j;

  for (i = 0; i<iDim; ++i) {
    for (j = 0; j<jDim; ++j) {
      printf("%1.2f ", array(grid,i,j));
    }
    printf("\n");
  }
  printf("\n");
  fflush(stdout);
}

/****************************************************************
* double MortoncomputeSum(Array2dDouble U,                            *
*                    int iDim, int jDim, int D0[], int D1[])    *
*								*
* compute the sum of all the elements                  		*
*****************************************************************/
double MortoncomputeSum(gridtype grid, int iDim, int jDim, unsigned int D0[], unsigned int D1[])
{
  int i,j;
  double result = 0.0;

  for (i=0; i<iDim; ++i)
    {
      for (j=0; j<jDim; ++j)
	{
	  result += array(grid,i,j);
	}
    }
  return(result);
}

/***************************************************************/
/* Simple Gauss-Seidel implementation                          */
/***************************************************************/

void MortongausseidelV1(gridtype grid, int iDim, int jDim, int Niters, unsigned int D0[], unsigned int D1[])
{
  int it;
  int i,j;
  double onequarter = 1.0/4.0;

  for (it=0; it<Niters; it+=1) {
#ifdef PRINT_GRID
    printf("grid prior to it=%d\n", it);
    PrintGrid(grid, iDim, jDim);
#endif
    for (i=1; i<iDim-1; ++i)
      for (j=1; j<jDim-1; ++j)
	array(grid,i,j) = onequarter *
	  (array(grid,i-1,j) + array(grid,i+1,j) + array(grid,i,j-1) + array(grid,i,j+1));
  }
}


/****************************************************************
* TaskGraph Version						*
*								*
*****************************************************************/

typedef TaskGraph<void, double[MAXiDIM*MAXjDIM], int, unsigned int[2*(MAXiDIM+MAXjDIM)], unsigned int[2*(MAXiDIM+MAXjDIM)]> gaussseidel_TaskGraph;

unsigned size[] = { MAXiDIM*MAXjDIM };
unsigned tabsize[] = { 2*(MAXiDIM+MAXjDIM) };

void taskMortonGaussSeidel (gaussseidel_TaskGraph &t, int iDim, int jDim)
{
  taskgraph ( gaussseidel_TaskGraph, t, tuple4(grid, Niters, D0, D1) ) {

    tVar (int, it );
    tVar (int, i );
    tVar (int, j );
    double onequarter = 1.0/4.0;

    tFor (it, 0, Niters-1) {
      tFor (i, 1, iDim-2 )
	tFor (j, 1, jDim-2 )
  	  grid[D0[i]+D1[j]] = onequarter *
	    (grid[D0[i-1]+D1[j]] + grid[D0[i+1]+D1[j]] + grid[D0[i]+D1[j-1]] + grid[D0[i]+D1[j+1]]);
    }
  }
}

/****************************************************************
* Unrolled TaskGraph Version               			*
*								*
*****************************************************************/

// Number of iterations to unroll, must be power of two
#define UNROLLFACTOR 8

// D0P(jd,js) = D0P(jd+js) provided jd is divisible by UNROLLFACTOR and js >=0 && js < UNROLLFACTOR
// jd is dynamic, js static.
#define D0P(jd,js) ((js >= 0 && js < UNROLLFACTOR) ? (D0[jd] + SpreadBits(0,js)) : D0[jd+js])
#define D1P(jd,js) ((js >= 0 && js < UNROLLFACTOR) ? (D1[jd] + SpreadBits(1,js)) : D1[jd+js])

void taskMortonGaussSeidelUnrolled (gaussseidel_TaskGraph &t, int iDim, int jDim)
{
  taskgraph ( gaussseidel_TaskGraph, t, tuple4(grid, Niters, D0, D1) ) {

    tVar (int, it );
    tVar (int, i );
    tVar (int, j );
    tVar (int, jj );
    int u;
    double onequarter = 1.0/4.0;

    int PreLoop_lwb = 1;
    int PreLoop_upb = MIN(jDim-2, UNROLLFACTOR-1);
    int PreLoop_RemainingTrips = jDim-2-UNROLLFACTOR;
    int MainLoop_lwb = UNROLLFACTOR;
    int MainLoop_upb = MainLoop_lwb + (PreLoop_RemainingTrips/UNROLLFACTOR - 1)*UNROLLFACTOR;
    int PostLoop_lwb = MainLoop_upb + UNROLLFACTOR;
    int PostLoop_upb = jDim-2;

    tFor (it, 0, Niters-1) {
      tFor (i, 1, iDim-2 ) {

	// Peel off odd iterations

	tFor (j, PreLoop_lwb, PreLoop_upb)
  	  grid[D0[i]+D1[j]] = onequarter *
	    (grid[D0[i-1]+D1[j]] + grid[D0[i+1]+D1[j]] + grid[D0[i]+D1[j-1]] + grid[D0[i]+D1[j+1]]);

	// Now, next j is a multiple of UNROLLFACTOR

        tForStep (jj, MainLoop_lwb, MainLoop_upb, UNROLLFACTOR) {

	  for (u = 0; u <= UNROLLFACTOR-1; u++) {
	    //	        j = jj+u;
	    //          grid[D0[i]+D1[j]] = onequarter *
	    //            (grid[D0[i-1]+D1[j]] + grid[D0[i+1]+D1[j]] + grid[D0[i]+D1[j-1]] + grid[D0[i]+D1[j+1]]);
	    grid[D0[i]+D1P(jj,u)] = onequarter *
	      (grid[D0[i-1]+D1P(jj,u)] + grid[D0[i+1]+D1P(jj,u)] + grid[D0[i]+D1P(jj,u-1)] + grid[D0[i]+D1P(jj,u+1)]);
	  }
	}

	tFor (j, PostLoop_lwb, PostLoop_upb)
  	  grid[D0[i]+D1[j]] = onequarter *
	    (grid[D0[i-1]+D1[j]] + grid[D0[i+1]+D1[j]] + grid[D0[i]+D1[j-1]] + grid[D0[i]+D1[j+1]]);
      }
    }
  }
}



/****************************************************************
* main()							*
*								*
*****************************************************************/
int main(int argc, char **argv)
{
  int iDim, jDim;
  double stop;
  static gridtype grid;

#ifdef TIME_USING_UTIME
  double lasttime;
#endif

  ParseCommandLine(argc,argv);

  iDim = TheCommandLine.sweepi;
  jDim = TheCommandLine.sweepj;

  /******* Row major layout *********/

  {
    InitGrid(grid, iDim, jDim);

#ifdef TIME_USING_UTIME
    lasttime = utime();
#endif

    gausseidelV1(grid, iDim, jDim, TheCommandLine.Niters);

#ifdef TIME_USING_UTIME
    {
      double time = utime()-lasttime;
      double mflops = iDim*jDim*TheCommandLine.Niters*4/time/1000000.0;

      printf("MAXiDIM %d MAXjDIM %d sweepi %d sweepj %d Niters %d time %f mflops %f\n",
	     MAXiDIM, MAXjDIM, iDim, jDim, TheCommandLine.Niters, time, mflops);
    }
#endif

#ifdef COMPUTE_SUMCHECK_AT_END
    stop = computeSum(grid, iDim, jDim);
    printf("After %d iterations, sumcheck: %1.16e\n", TheCommandLine.Niters, stop);
#endif
  }

  /******* Morton layout *********/

  {
    /* Allocate Morton tables, big enough for next power of two */
    unsigned int MortonTabEven[2*(MAXiDIM+MAXjDIM)];
    unsigned int MortonTabOdd[2*(MAXiDIM+MAXjDIM)];
    FillMortonTables(MortonTabEven, MortonTabOdd, MAXiDIM+MAXjDIM);

    printf("Morton: Tables initialised\n");

    MortonInitGrid(grid, iDim, jDim, MortonTabEven, MortonTabOdd);

    printf("Morton: Grid initialised\n");

#ifdef TIME_USING_UTIME
    lasttime = utime();
#endif

    MortongausseidelV1(grid, iDim, jDim, TheCommandLine.Niters, MortonTabEven, MortonTabOdd);

#ifdef TIME_USING_UTIME
    {
      double time = utime()-lasttime;
      double mflops = iDim*jDim*TheCommandLine.Niters*4/time/1000000.0;

      printf("MAXiDIM %d MAXjDIM %d sweepi %d sweepj %d Niters %d time %f mflops %f\n",
	     MAXiDIM, MAXjDIM, iDim, jDim, TheCommandLine.Niters, time, mflops);
    }
#endif

#ifdef COMPUTE_SUMCHECK_AT_END
    stop = MortoncomputeSum(grid, iDim, jDim, MortonTabEven, MortonTabOdd);
    printf("After %d iterations, sumcheck: %1.16e\n", TheCommandLine.Niters, stop);
#endif


    /******* Morton layout using TaskGraph *********/

    {
      /* Allocate Morton tables, big enough for next power of two */
      unsigned int MortonTabEven[2*(MAXiDIM+MAXjDIM)];
      unsigned int MortonTabOdd[2*(MAXiDIM+MAXjDIM)];
      FillMortonTables(MortonTabEven, MortonTabOdd, MAXiDIM+MAXjDIM);

      printf("Morton: Tables initialised\n");

      MortonInitGrid(grid, iDim, jDim, MortonTabEven, MortonTabOdd);

      printf("Morton: Grid initialised\n");

#ifdef TIME_USING_UTIME
      lasttime = utime();
#endif

      gaussseidel_TaskGraph t;
      taskMortonGaussSeidel(t, iDim, jDim);
      t.print();
      t.compile ( tg::GCC );
#ifdef TIME_USING_UTIME
      {
	double time = utime()-lasttime;
	printf("Construction and compilation: %f seconds\n", time );
      }
      lasttime = utime();
#endif

      t.execute( grid, TheCommandLine.Niters, MortonTabEven, MortonTabOdd );

#ifdef TIME_USING_UTIME
      {
	double time = utime()-lasttime;
	double mflops = iDim*jDim*TheCommandLine.Niters*4/time/1000000.0;

	printf("MAXiDIM %d MAXjDIM %d sweepi %d sweepj %d Niters %d time %f mflops %f\n",
	       MAXiDIM, MAXjDIM, iDim, jDim, TheCommandLine.Niters, time, mflops);
      }
#endif

#ifdef COMPUTE_SUMCHECK_AT_END
      stop = MortoncomputeSum(grid, iDim, jDim, MortonTabEven, MortonTabOdd);
      printf("After %d iterations, sumcheck: %1.16e\n", TheCommandLine.Niters, stop);
#endif
    }




    /******* Morton layout using unrolled TaskGraph *********/

    {
      /* Allocate Morton tables, big enough for next power of two */
      unsigned int MortonTabEven[2*(MAXiDIM+MAXjDIM)];
      unsigned int MortonTabOdd[2*(MAXiDIM+MAXjDIM)];
      FillMortonTables(MortonTabEven, MortonTabOdd, MAXiDIM+MAXjDIM);

      printf("Morton: Tables initialised\n");

      MortonInitGrid(grid, iDim, jDim, MortonTabEven, MortonTabOdd);

      printf("Morton: Grid initialised\n");

#ifdef TIME_USING_UTIME
      lasttime = utime();
#endif

      gaussseidel_TaskGraph t;
      taskMortonGaussSeidelUnrolled(t, iDim, jDim);
      t.print();
      t.compile ( tg::GCC );
#ifdef TIME_USING_UTIME
      {
	double time = utime()-lasttime;
	printf("Construction and compilation: %f seconds\n", time );
      }
      lasttime = utime();
#endif

      t.execute( grid, TheCommandLine.Niters, MortonTabEven, MortonTabOdd );

#ifdef TIME_USING_UTIME
      {
	double time = utime()-lasttime;
	double mflops = iDim*jDim*TheCommandLine.Niters*4/time/1000000.0;

	printf("MAXiDIM %d MAXjDIM %d sweepi %d sweepj %d Niters %d time %f mflops %f\n",
	       MAXiDIM, MAXjDIM, iDim, jDim, TheCommandLine.Niters, time, mflops);
      }
#endif

#ifdef COMPUTE_SUMCHECK_AT_END
      stop = MortoncomputeSum(grid, iDim, jDim, MortonTabEven, MortonTabOdd);
      printf("After %d iterations, sumcheck: %1.16e\n", TheCommandLine.Niters, stop);
#endif
    }


    return(0);
  }
}
