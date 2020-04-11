#include <stdio.h>
#include <stdlib.h>
#include <TaskGraph>
using namespace tg;

typedef TaskGraph<int, int> simple_TaskGraph;

int main( int argc, char *argv[] ) {
	simple_TaskGraph T;
	taskgraph( simple_TaskGraph, T, tuple1(a) ) {
		tReturn(a + 1);
	}
	T.compile();
	int b;
	b = 10;
	printf( "T(b) = %d where b = %d\n", T(b), b );
	b = 100;
	printf( "T(b) = %d where b = %d\n", T(b), b );
}
