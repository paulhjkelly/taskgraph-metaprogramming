/* 
 * Iterated Gauss-Seidel smoother example for TaskGraph library
 *
 * Paul H J Kelly and Michael Mellor, Imperial College London, March 2006
 *
 * This example illustrates the library's support for loop skewing
 * which enables this loop to be tiled.  The final skewed, tiled
 * version of both the 1d and the 2d loop nest is slightly faster on a
 * Pentium 4.
 *
 * Usage: 
 * To compile:
 *  make
 * or
 *  make "EXTRA_CXXFLAGS=-DDONTPRINT -DDONTCHECK" (you can set lots of flags here)
 * To run:
 *  ./skewedgaussseidel  (this runs all the tests one after the other, doesn't work with DONTCHECK)
 * or
 *  ./skewedgaussseidel a (or b, or c etc; this runs one specified test at a time)
 *
 * Results (on this 3.2GHz Pentium 4 with 2MB cache, "ray05.doc.ic.ac.uk", gcc3.4.3):
 * 
 * 2D Gauss-Seidel:
 *  make "EXTRA_CXXFLAGS=-DDONTPRINT -DDONTCHECK -DMATRIXSIZE=5012 -DTILESIZE=8"
 *
 * Runtime (vanilla C version, ./skewedgaussseidel e): 50.5 seconds (199 MFLOPs)
 * Runtime (vanilla TG version, ./skewedgaussseidel f): 32.3 seconds (311 MFLOPs)
 * Runtime (tiled, skewed TG version, ./skewedgaussseidel h): 24.7 seconds (407 MFLOPs)
 *
 * For the 2d case (on this machine), the optimum TILESIZE is 8.
 * 
 * 1D Gauss-Seidel:
 *  make "EXTRA_CXXFLAGS=-DDONTPRINT -DDONTCHECK -DMATRIXSIZE=10000 -DTILESIZE=3 -DNITERS=10000"
 *
 * Runtime (vanilla C version, ./skewedgaussseidel a): 1.09 seconds (183 MFLOPs)
 * Runtime (vanilla TG version, ./skewedgaussseidel b): 1.09 seconds (183 MFLOPs)
 * Runtime (tiled, skewed TG version, ./skewedgaussseidel d): 0.65 seconds (308 MFLOPs)
 *
 * For the 1d case (on this machine), the optimum TILESIZE is 3.
 * 
 * Different machines will benefit to differing extents, sometimes not
 * at all.  We suspect performance is crippled on some machines by the
 * min and max operators.
 */

#include <TaskGraph>
#include <iostream>

#ifndef MATRIXSIZE
#define MATRIXSIZE 1088
#endif
#ifndef NITERS
#define NITERS 100
#endif
#ifndef TILESIZE
#define TILESIZE 16
#endif
#ifndef EPSILON
#define EPSILON 0.0000001
#endif
#ifndef SCALARTYPE
#define SCALARTYPE double
#endif

using namespace tg;

// A naive Gauss-Seidel smoother function for comparison purposes

void gaussseidel1d ( unsigned niters, SCALARTYPE a1d[MATRIXSIZE] ) {
	for ( unsigned x = 0; x < niters; ++x ) {
		for ( unsigned y = 1; y < MATRIXSIZE - 1; ++y ) {
			a1d[y] = 0.5*(a1d[y-1] + a1d[y+1]);
		}
	}
}
void gaussseidel2d ( unsigned niters, SCALARTYPE a2d[MATRIXSIZE][MATRIXSIZE] ) {
	for ( unsigned x = 0; x < niters; ++x ) {
		for ( unsigned y = 1; y < MATRIXSIZE - 1; ++y ) {
			for ( unsigned z = 1; z < MATRIXSIZE - 1; ++z ) {
				a2d[y][z] = 0.25*(a2d[y-1][z] + a2d[y+1][z] +
				a2d[y][z-1] + a2d[y][z+1]);
			}
		}
	}
}

typedef SCALARTYPE MatrixType2d[MATRIXSIZE][MATRIXSIZE];
typedef SCALARTYPE MatrixType1d[MATRIXSIZE];

// this defines the type of the taskgraphs we're going to build
typedef TaskGraph<void, MatrixType1d> gs_TaskGraph1d;
typedef TaskGraph<void, MatrixType2d> gs_TaskGraph2d;

// This function builds a taskgraph to do exactly the same thing as the
// function above

void build_gaussseidel1d ( int niters, gs_TaskGraph1d &t ) {
	taskgraph ( gs_TaskGraph1d, t, tuple1(a1d) ) {
		tVar ( int, x );
		tVar ( int, y );

		tFor ( x, 0, niters-1 ) {
		//      tPrintf("x = %d\n", x);
			tFor ( y, 1, MATRIXSIZE - 2 ) {
				a1d[y] = 0.5*(a1d[y-1] + a1d[y+1]);
			}
		}
	}
}
void build_gaussseidel2d ( int niters, gs_TaskGraph2d &t ) {
	taskgraph ( gs_TaskGraph2d, t, tuple1(a2d) ) {
		tVar ( int, x );
		tVar ( int, y );
		tVar ( int, z );
	
		tFor ( x, 0, niters-1 ) {
			// tPrintf("x = %d\n", x);
			tFor ( y, 1, MATRIXSIZE - 2 ) {
				tFor ( z, 1, MATRIXSIZE - 2 ) {
					a2d[y][z] = 0.25*(a2d[y-1][z] + a2d[y+1][z] +
					a2d[y][z-1] + a2d[y][z+1]);
				}
			}
		}
	}
}

void randomInitMatrix1d ( SCALARTYPE m[MATRIXSIZE] ) {
	for ( unsigned y = 0; y < MATRIXSIZE; ++y ) {
		m[y] = (y%7) ? 1.0f : 0.1f;
		if (y==0) m[y] = 2.0f; // break symmetry to expose bugs
	}
}

void randomInitMatrix2d ( SCALARTYPE m[][MATRIXSIZE] ) {
	for ( unsigned y = 0; y < MATRIXSIZE; ++y ) {
		for ( unsigned x = 0; x < MATRIXSIZE; ++x ) {
			m[x][y] = ((x+y)%7) ? 1.0f : 0.1f;
			if (x==0) m[x][y] = 2.0f; // break symmetry to expose bugs
		}
	}
}

bool compare1d(SCALARTYPE m1[MATRIXSIZE], SCALARTYPE m2[MATRIXSIZE]) {
	bool r = true;
	for ( unsigned y = 0; y < MATRIXSIZE; ++y ) {
		SCALARTYPE d = m1[y] - m2[y];
		d = (d<0) ? -d : d;
		if (d > EPSILON) {
			std::cout << "Differs: m1["<<y<<"]="<<m1[y]<<
			" m2["<<y<<"]="<<m2[y]<<" (d="<<d<<")\n";
			r = false;
		}
	}
	return r;
}

bool compare2d(SCALARTYPE m1[MATRIXSIZE][MATRIXSIZE], 
	       SCALARTYPE m2[MATRIXSIZE][MATRIXSIZE])
{
	bool r = true;
	for ( unsigned y = 0; y < MATRIXSIZE; ++y ) {
		for ( unsigned x = 0; x < MATRIXSIZE; ++x ) {
		SCALARTYPE d = m1[y][x] - m2[y][x];
		d = (d<0) ? -d : d;
		if (d > EPSILON) {
			std::cout << "Differs: m1["<<y<<"]["<<x<<"]="<<m1[y][x]<<
	  		" m2["<<y<<"]["<<x<<"]="<<m2[y][x]<<" (d="<<d<<")\n";
			r = false;
			}
		}
	}
	return r;
}


SCALARTYPE a1d_ref0[MATRIXSIZE];             // reference to test against
SCALARTYPE a1d_ref1[MATRIXSIZE];             // reference to test against
SCALARTYPE a1d[MATRIXSIZE];
SCALARTYPE a2d_ref0[MATRIXSIZE][MATRIXSIZE]; // reference to test against
SCALARTYPE a2d_ref1[MATRIXSIZE][MATRIXSIZE]; // reference to test against
SCALARTYPE a2d[MATRIXSIZE][MATRIXSIZE];

int main ( int argc, char **argv ) {

  // Optional command line argument selects just one experiment to do

  char whichTest = 0; // which test to do, if we're to do all tests set it to zero
  if (argc == 1) {
    whichTest = 0;
  } else if (argc == 2) {
    whichTest = argv[1][0]; // first character of first command line argument
#ifndef DONTCHECK
    std::cout << "Warning: compile with DONTCHECK since you have nothing to check against\n";
#endif
  } else {
    std::cout << "Usage: "<<argv[0]<<" (do run all tests)\n";
    std::cout << "   or: "<<argv[0]<<" a|b|c|d|e (run run test a, test b etc)\n";
  }

  // baseline 1D non-taskgraph version
  if (whichTest == 0 || whichTest == 'a') {
    Timer timera;
    randomInitMatrix1d ( a1d_ref0 );
    gaussseidel1d ( NITERS, a1d_ref0 );
    timera.stop ( );
    std::cout << "gs1d: C version took " << timera.getTime ( ) << "\n";
  }

  // equivalent 1D code using taskgraph
  if (whichTest == 0 || whichTest == 'b') {
    gs_TaskGraph1d gs;
    build_gaussseidel1d ( NITERS, gs );
#ifndef DONTPRINT
    gs.print();
#endif

    gs.compile ( tg::GCC );

    randomInitMatrix1d ( a1d_ref1 );
    Timer timer0;
    std::cout << "Executing\n";
    gs.execute ( a1d_ref1 );
    timer0.stop ( );
    std::cout << "Finished\n";
    std::cout << "gs1d: Taskgraph version took " << timer0.getTime ( ) << "\n";

#ifndef DONTCHECK
    if (!compare1d(a1d_ref1, a1d_ref0)) 
      std::cout << "Check failed\n";
    else
      std::cout << "Check passed\n";
#endif
  }

  // skewed 1D taskgraph version
  if (whichTest == 0 || whichTest == 'c') {
    gs_TaskGraph1d gs;
    build_gaussseidel1d ( NITERS, gs );

    SkewSettings skew ( LoopIdentifier ( 1 ), LoopIdentifier ( 1, 1 ), 1 );
    gs.applyOptimisation ( "skew", &skew );

#ifndef DONTPRINT
    gs.print();
#endif

    gs.compile ( tg::GCC );

    randomInitMatrix1d ( a1d );
    Timer timer0;
    std::cout << "Executing\n";
    gs.execute ( a1d );
    timer0.stop ( );
    std::cout << "Finished\n";
    std::cout << "gs1d: skewed taskgraph version took " << timer0.getTime ( ) << "\n";

#ifndef DONTCHECK
    if (!compare1d(a1d, a1d_ref1)) 
      std::cout << "Check failed\n";
    else
      std::cout << "Check passed\n";
#endif
  }

  // skewed, tiled 1D taskgraph version
  if (whichTest == 0 || whichTest == 'd') {
    gs_TaskGraph1d gs;
    build_gaussseidel1d ( NITERS, gs );

    SkewSettings skew ( LoopIdentifier ( 1 ), LoopIdentifier ( 1, 1 ), 1 );
    gs.applyOptimisation ( "skew", &skew );

    // Unfortunately, using SUIF to do the loop interchange fails,
    // because the bounds aren't computed correctly.
    /*
      InterchangeSettings inter;
      inter.firstLoop = LoopIdentifier ( 1 );
      inter.secondLoop = LoopIdentifier ( 1, 1 );
      gs.applyOptimisation ( "interchange", &inter );
    */

    // But tiling works:

    TileSettings tile ( LoopIdentifier ( 1 ), 2, TILESIZE );
    gs.applyOptimisation ( "tile", &tile );

#ifndef DONTPRINT
    gs.print();
#endif

    gs.compile ( tg::GCC );

    randomInitMatrix1d ( a1d );
    Timer timer0;
    std::cout << "Executing\n";
    gs.execute ( a1d );
    timer0.stop ( );
    std::cout << "Finished\n";
    std::cout << "gs1d: Tiled skewed taskgraph version took " << timer0.getTime ( ) << "\n";

#ifndef DONTCHECK
    if (!compare1d(a1d, a1d_ref1)) 
      std::cout << "Check failed\n";
    else
      std::cout << "Check passed\n";
#endif
  }
	
  // baseline 2D non-taskgraph version
  if (whichTest == 0 || whichTest == 'e') {
    randomInitMatrix2d ( a2d_ref0 );
    Timer timera;
    gaussseidel2d ( NITERS, a2d_ref0 );
    timera.stop ( );
    std::cout << "gs2d: C version took " << timera.getTime ( ) << "\n";
  }
  // equivalent 2D code using taskgraph
  if (whichTest == 0 || whichTest == 'f') {
    gs_TaskGraph2d gs;
    build_gaussseidel2d ( NITERS, gs );
#ifndef DONTPRINT
    gs.print();
#endif

    gs.compile ( tg::GCC );

    randomInitMatrix2d ( a2d_ref1 );
    Timer timer0;
    std::cout << "Executing\n";
    gs.execute ( a2d_ref1 );
    timer0.stop ( );
    std::cout << "Finished\n";
    std::cout << "gs2d: Taskgraph version took " << timer0.getTime ( ) << "\n";

#ifndef DONTCHECK
    if (!compare2d(a2d_ref1, a2d_ref0)) 
      std::cout << "Check failed\n";
    else
      std::cout << "Check passed\n";
#endif
  }

  // skewed 2D taskgraph version
  if (whichTest == 0 || whichTest == 'g') {
    gs_TaskGraph2d gs;
    build_gaussseidel2d ( NITERS, gs );

    // skew y loop by x loop with factor 1
    {
      SkewSettings skew ( LoopIdentifier ( 1 ), LoopIdentifier ( 1, 1 ), 1 );
      gs.applyOptimisation ( "skew", &skew );
    }
    // skew z loop by x loop with factor 1
    {
      SkewSettings skew ( LoopIdentifier ( 1 ), LoopIdentifier ( 1, 1, 1 ), 1 );
      gs.applyOptimisation ( "skew", &skew );
    }
#ifndef DONTPRINT
    gs.print();
#endif

    gs.compile ( tg::GCC );

    randomInitMatrix2d ( a2d );
    Timer timer0;
    std::cout << "Executing\n";
    gs.execute ( a2d );
    timer0.stop ( );
    std::cout << "Finished\n";
    std::cout << "gs2d: Skewed taskgraph version took " << timer0.getTime ( ) << "\n";

#ifndef DONTCHECK
    if (!compare2d(a2d, a2d_ref1)) 
      std::cout << "Check failed\n";
    else
      std::cout << "Check passed\n";
#endif
  }
	
  // Tiled skewed 2D taskgraph version
  if (whichTest == 0 || whichTest == 'h') {
    gs_TaskGraph2d gs;
    build_gaussseidel2d ( NITERS, gs );

    // skew y loop by x loop with factor 1
    {
      SkewSettings skew ( LoopIdentifier ( 1 ), LoopIdentifier ( 1, 1 ), 1 );
      gs.applyOptimisation ( "skew", &skew );
    }
    // skew z loop by x loop with factor 1
    {
      SkewSettings skew ( LoopIdentifier ( 1 ), LoopIdentifier ( 1, 1, 1 ), 1 );
      gs.applyOptimisation ( "skew", &skew );
    }
    // Now tile it
    {
      TileSettings tile ( LoopIdentifier ( 1 ), 3, TILESIZE );
      gs.applyOptimisation ( "tile", &tile );
    }

#ifndef DONTPRINT
    gs.print();
#endif

    gs.compile ( tg::GCC );

    randomInitMatrix2d ( a2d );
    Timer timer0;
    std::cout << "Executing\n";
    gs.execute ( a2d );
    timer0.stop ( );
    std::cout << "Finished\n";
    std::cout << "gs2d: Skewed taskgraph version took " << timer0.getTime ( ) << "\n";

#ifndef DONTCHECK
    if (!compare2d(a2d, a2d_ref1)) 
      std::cout << "Check failed\n";
    else
      std::cout << "Check passed\n";
#endif
  }
	
}
