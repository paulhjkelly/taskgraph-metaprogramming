/*
 * Matrix multiply example for TaskGraph library
 *
 * Paul H J Kelly and Michael Mellor, Imperial College London, March 2006
 *
 * This example illustrates the library's support for loop interchange
 * and interchange.
 *
 * You will need to adjust the MATRIXSIZE and TILESIZE for your hardware.
 * Interchange is good value on all machines, but for some platforms 
 * it is hard to get tiling to pay off, because of the redundant "max"
 * operators and avoidable "min" operators that are introduced.
 */

#include <TaskGraph>
#include <iostream>

#define MATRIXSIZE 1088
#define TILESIZE 144
#define SCALARTYPE double

using namespace tg;

// A naive matrix multiply function for comparison purposes

void matrixMult ( SCALARTYPE a[][MATRIXSIZE], SCALARTYPE b[][MATRIXSIZE], SCALARTYPE c[][MATRIXSIZE] ) {
	for ( unsigned x = 0; x < MATRIXSIZE; ++x ) {
		for ( unsigned y = 0; y < MATRIXSIZE; ++y ) {
			for ( unsigned z = 0; z < MATRIXSIZE; ++z ) {
				c[x][y] += a[x][z] * b[z][y];
			}
		}
	}
}

typedef SCALARTYPE MatrixType[MATRIXSIZE][MATRIXSIZE];

// this defines the type of the taskgraphs we're going to build
typedef TaskGraph<void, MatrixType, MatrixType, MatrixType> mm_TaskGraph;

// This function builds a taskgraph to do exactly the same thing as the
// matrixmult function above

void taskMatrixMult ( mm_TaskGraph &t ) {
	taskgraph ( mm_TaskGraph, t, tuple3(a, b, c) ) {
		tVar ( int, x );
		tVar ( int, y );
		tVar ( int, z );

		tFor ( x, 0, MATRIXSIZE - 1 ) {
			tFor ( y, 0, MATRIXSIZE - 1 ) {
				tFor ( z, 0, MATRIXSIZE - 1 ) {
				c[x][y] += a[x][z] * b[z][y];
				}
			}
		}
	}
}

void randomInitMatrix ( SCALARTYPE m[][MATRIXSIZE] ) {
	for ( unsigned y = 0; y < MATRIXSIZE; ++y ) {
		for ( unsigned x = 0; x < MATRIXSIZE; ++x ) {
			m[x][y] = 1.0f;
		}
	}
}

void zeroMatrix ( SCALARTYPE m[][MATRIXSIZE] ) {
	for ( unsigned y = 0; y < MATRIXSIZE; ++y ) {
		for ( unsigned x = 0; x < MATRIXSIZE; ++x ) {
			m[x][y] = 0.0f;
		}
	}
}

SCALARTYPE a[MATRIXSIZE][MATRIXSIZE], b[MATRIXSIZE][MATRIXSIZE], c[MATRIXSIZE][MATRIXSIZE];

int main ( int argc, char **argv ) {
	mm_TaskGraph mm0, mm1, mm2;

	// initialise operand matrices

	randomInitMatrix ( a );
	randomInitMatrix ( b );

	// baseline non-taskgraph version

	zeroMatrix ( c );
	Timer timera;
	matrixMult ( a, b, c );
	timera.stop ( );
	std::cout << "C (ijk version) took " << timera.getTime ( ) << "\n";

	// equivalent code using taskgraph

	taskMatrixMult ( mm0 );
//	mm0.print();

	mm0.compile ( tg::GCC );

	zeroMatrix ( c );
	Timer timer0;
	mm0.execute ( a, b, c );
	timer0.stop ( );
	std::cout << "Taskgraph (ijk version) took " << timer0.getTime ( ) << "\n";

	// Taskgraph version with loop interchange

	taskMatrixMult ( mm1 );

	InterchangeSettings inter;
	inter.firstLoop = LoopIdentifier ( 1, 1 );
	inter.secondLoop = LoopIdentifier ( 1, 1, 1 );
	mm1.applyOptimisation ( "interchange", &inter );

	mm1.print();

	mm1.compile ( tg::GCC );

	zeroMatrix ( c );
	Timer timer1;
	mm1.execute ( a, b, c );
	timer1.stop ( );
	std::cout << "Taskgraph (ikj version) took " << timer1.getTime ( ) << "\n";

	// Taskgraph version with tiling

	taskMatrixMult ( mm2 );

	// first, apply interchange as above to get ikj version
	{
		InterchangeSettings inter;
		inter.firstLoop = LoopIdentifier ( 1, 1 );
		inter.secondLoop = LoopIdentifier ( 1, 1, 1 );
		mm2.applyOptimisation ( "interchange", &inter );
	}
	// now tile the inner two loops, k and j
	{
		TileSettings tile ( LoopIdentifier ( 1, 1 ), 2, TILESIZE );
		mm2.applyOptimisation ( "tile", &tile );
	}
	// now interchange the outermost loop (i) so it's inside the kk and jj loops
	{
		InterchangeSettings inter;
		inter.firstLoop = LoopIdentifier ( 1 );
		inter.secondLoop = LoopIdentifier ( 1, 1, 1 );
		mm2.applyOptimisation ( "interchange", &inter );
	}

	mm2.print();

	mm2.compile ( tg::GCC );

	zeroMatrix ( c );
	Timer timer2;
	mm2.execute ( a, b, c );
	timer2.stop ( );
	std::cout << "Taskgraph (tiled version) took " << timer2.getTime ( ) << "\n";
}
