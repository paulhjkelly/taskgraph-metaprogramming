#include <cstdio>
#include <TaskGraph>

using namespace tg;

typedef TaskGraph<void,int[100]> fusearray_TaskGraph;

int main ( ) {
	fusearray_TaskGraph T;

	taskgraph (fusearray_TaskGraph, T, tuple1(x) ) {
		tVar ( int, i );
		// It isn't legal to fuse these two loops

		tFor (i, 1, 10)
			x[2 * i] = x[2 * i + 1] + 2;

		tFor (i, 1, 10)
			x[2 * i + 3] = x[2 * i] + i;

		// A statement to break up the loops
		x[0] = 0;

		// It *is* legal to fuse these two loops

		tFor (i, 1, 10)
			x[2 * i] = x[2 * i + 1] + 2;

		tFor (i, 1, 10)
			x[2 * i] = x[2 * i] + i;

		// A statement to break up the loops
		x[0] = 0;

		// It *is* legal to fuse these as well

		tFor (i, 1, 10)
			x[2 * i] = x[2 * i + 1] + 2;

		tFor (i, 1, 10)
			x[2 * i - 1] = x[2 * i] + i;
	}

	printf ("Before optimisation:\n");

	T.print ();

#ifdef USE_SUIF1_IR
	T.applyOptimisation ( "fusion" );
#endif

#ifdef USE_ROSE_IR
	T.singleFusion();
#endif

	printf ("\nAfter optimisation:\n");

	T.print ();
}
