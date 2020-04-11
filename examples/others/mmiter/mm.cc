#include "TaskGraph"
#include "TaskTimer.h"
#include "TaskIterative.h"
#include <iostream>

//const unsigned MATRIXSIZE = 1024;
const unsigned MATRIXSIZE = 512;
const unsigned CLUSTERING = 4;

typedef float matrix[MATRIXSIZE][MATRIXSIZE];

using namespace tg;

/* Set the matrix to zero
 */
void zeroMatrix ( matrix m, float value = 0.0f ) {
	for ( unsigned y = 0; y < MATRIXSIZE; ++y ) {
		for ( unsigned x = 0; x < MATRIXSIZE; ++x ) {
			m[x][y] = value;
		}
	}
}

/* Set the matrix to a random value.
 * Actually just 1.0f but it is safe to assume the processor doesn't do value prediction and skew
 * the results!
 */
void randomInitMatrix ( matrix m ) {
	zeroMatrix ( m, 1.0f );
}

void checkMatrices ( matrix a, matrix b ) {
	unsigned x, y;
	int error = 0;

	for ( x = 0; x <  MATRIXSIZE; ++x ) {
		for ( y = 0; y <  MATRIXSIZE; ++y ) {
			if ( a[x][y] != b[x][y] ) {
				++error;
			}
		}
	}
	if ( error )
		std::cerr << "Errors = " << error << "\n";
}

/* The plain C version of matrix mult
 */
void mm_c ( matrix a, matrix b, matrix c ) {
	for ( unsigned i = 0; i < MATRIXSIZE; ++i ) {
		for ( unsigned k = 0; k < MATRIXSIZE; ++k ) {
			for ( unsigned j = 0; j < MATRIXSIZE; ++j ) {
				c[i][j] += a[i][k] * b[k][j];
			}
		}
	}
}

typedef TaskGraph<void, matrix, matrix, matrix> mm_TaskGraph;

/* Taskgraph matrix multiply, with interchange/tiling etc done by hand
 */
void taskMatrixMult ( coreTaskGraph *&t, unsigned tilesize ) {

  mm_TaskGraph *tg = new mm_TaskGraph();
	taskgraph ( mm_TaskGraph, *tg, tuple3(a, b, c) ) {
	tVar ( unsigned, x );
	tVar ( unsigned, y );
	tVar ( unsigned, z );
	tVar ( unsigned, y_tile );
	tVar ( unsigned, z_tile );

	unsigned leftOver = MATRIXSIZE % tilesize;
	int top = MATRIXSIZE - leftOver - 1;

	tForStep ( z_tile, 0, top, tilesize ) {
		tForStep ( y_tile, 0, top, tilesize ) {
			tFor ( x, 0, MATRIXSIZE - 1 ) {
				tFor ( z, z_tile, z_tile + (tilesize - 1) ) {
					tFor ( y, y_tile, y_tile + (tilesize - 1) ) {
						c[x][y] += a[x][z] * b[z][y];
					}
				}
			}
		}
		if ( leftOver != 0 ) {
			tFor ( x, 0, MATRIXSIZE - 1 ) {
				tFor ( z, z_tile, z_tile + (tilesize - 1) ) {
					tFor ( y, top+1, MATRIXSIZE - 1 ) {
						c[x][y] += a[x][z] * b[z][y];
					}
				}
			}
		}
	}
	if ( leftOver != 0 ) {
		tForStep ( y_tile, 0, top, tilesize ) {
			tFor ( x, 0, MATRIXSIZE - 1 ) {
				tFor ( z, top+1, MATRIXSIZE - 1 ) {
					tFor ( y, y_tile, y_tile + (tilesize - 1) ) {
						c[x][y] += a[x][z] * b[z][y];
					}
				}
			}
		}
		tFor ( x, 0, MATRIXSIZE - 1 ) {
			tFor ( z, top+1, MATRIXSIZE - 1 ) {
				tFor ( y, top+1, MATRIXSIZE - 1 ) {
					c[x][y] += a[x][z] * b[z][y];
				}
			}
		}
	}

	}

	t = tg;
}

/* TaskGraph matrix multiply done by SUIF
 */
void taskMatrixMultOpt ( coreTaskGraph *&t, unsigned tilesize ) {
  mm_TaskGraph *tg = new mm_TaskGraph();
	taskgraph ( mm_TaskGraph, *tg, tuple3(a, b, c) ) {
	tVar ( int, x );
	tVar ( int, y );
	tVar ( int, z );

	tFor ( x, 0, MATRIXSIZE - 1 ) {
		tFor ( z, 0, MATRIXSIZE - 1 ) {
			tFor ( y, 0, MATRIXSIZE - 1 ) {
				c[x][y] += a[x][z] * b[z][y];
			}
		}
	}
	}

	t = tg;
#ifdef USE_SUIF1_IR
	// Tile the loops z and y. LoopIdentifier 1 , 1 specifies first loop in the first loop (i.e. z)
	// 2 = depth
	TileSettings tile ( LoopIdentifier ( 1, 1 ), 2, tilesize );

	t->applyOptimisation ( "tile", &tile );

	// Swap the newly created loops and loop x
	InterchangeSettings inter;

	inter.firstLoop = LoopIdentifier ( 1 );
	inter.secondLoop = LoopIdentifier ( 1, 1 );
	t->applyOptimisation ( "interchange", &inter );

	inter.firstLoop = LoopIdentifier ( 1, 1 );
	inter.secondLoop = LoopIdentifier ( 1, 1, 1 );
	t->applyOptimisation ( "interchange", &inter );
#endif

}

// One variable iterative compilation
// Min and Max of 4 - MATRIXSIZE, iterates by splitting gaps into 3 and clusters at most CLUSTERING taskgraphs together

OneVariableIterative iter_hand ( taskMatrixMult, 8, 128*2, 4, CLUSTERING );
OneVariableIterative iter_suif ( taskMatrixMultOpt, 8, 128*2, 4, CLUSTERING );

// Hand tiled matrix multiply
mm_TaskGraph *mm_hand ( matrix a, matrix b, matrix c ) {
	unsigned tileSize;
	mm_TaskGraph *m = static_cast<mm_TaskGraph *>(iter_hand.getNext ( &tileSize ));
	tg::Timer timer;
	m->execute ( a, b, c );
	timer.stop ( );
	iter_hand.setResult ( tileSize, timer );
//	std::cout << "TaskGraph Version of Matrix Multiply with tile size " << tileSize << " took " << timer << " seconds\n";
	return m;
}

// SUIF optimised(with help) matrix multiply
mm_TaskGraph *mm_suif ( matrix a, matrix b, matrix c ) {
	unsigned tileSize;
	mm_TaskGraph *m = static_cast<mm_TaskGraph *>(iter_suif.getNext ( &tileSize ));
	tg::Timer timer;
	m->execute ( a, b, c );
	timer.stop ( );
	iter_suif.setResult ( tileSize, timer.getTime ( ) );
//	std::cout << "TaskGraph Version of Matrix Multiply with tile size " << tileSize << " took " << timer.getTime ( ) << " seconds\n";
	return m;
}

matrix a, b, c, d, e;

unsigned iterations = 150;

int main ( int argc, char **argv ) {
	if ( argc == 2 )
		iterations = atoi ( argv[1] );

//	iter_hand.setCompiler ( new IntelCCompiler ( ) );
	randomInitMatrix ( a );
	randomInitMatrix ( b );
	zeroMatrix ( c );
	zeroMatrix ( d );
	zeroMatrix ( e );

//	tg::Timer alltime;
	for ( unsigned j = 0; j < iterations; ++j ) {
		std::cout << j << "\t";
		tg::Timer time_c;
		mm_c ( a, b, c );
		time_c.stop();
		std::cout << time_c << "\t";

		tg::Timer time_hand;
		mm_hand ( a, b, c );
		time_hand.stop();
		std::cout << time_hand << "\t";

		tg::Timer time_suif;
		mm_suif ( a, b, c );
		time_suif.stop();
		std::cout << time_suif << "\t";
		std::cout << "\n";

	}
//	alltime.stop();
//	std::cout << "Total" << "\t" << alltime.getTime() << "\n";
#if 0
// Check that the matrix multiplies are the same
	for ( unsigned j = 0; j < iterations; ++j ) {
		std::cout << "Iter\n";
		randomInitMatrix ( a );
		randomInitMatrix ( b );
		zeroMatrix ( c );
		zeroMatrix ( d );
		zeroMatrix ( e );

		mm_c ( a, b, c );
		mm_hand ( a, b, d );
		mm_suif ( a, b, e );
		std::cout <<  "Check a\n";
		checkMatrices ( c, d );
		std::cout << "Check b\n";
		checkMatrices ( c, e );
	}
#endif
}
