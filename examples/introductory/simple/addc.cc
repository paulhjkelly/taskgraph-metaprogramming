/*
 * Simple taskgraph example which shows how to create
 * and execute a taskgraph.
*/
#include <stdio.h>
#include <stdlib.h>
#include <TaskGraph>

using namespace tg;

typedef TaskGraph<int, int> addc_TaskGraph;

int main( int argc, char *argv[] ) {
	if (argc != 2) {
		printf("Usage: addc <c>\n");
		return -1;
	}

	addc_TaskGraph T;
	int b = 1;
	int c = atoi( argv[1] );

	taskgraph( addc_TaskGraph, T, tuple1(a) ) {
		tReturn(a + c);
	}

	T.compile( tg::GCC, true );
	printf( "T(b) = %d where b = %d\n", T(b), b );
}
