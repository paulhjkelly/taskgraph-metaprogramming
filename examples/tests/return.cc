#include <TaskGraph>
#include <stdio.h>
using namespace tg;

typedef TaskGraph<int> intTaskGraph;
typedef TaskGraph<double> doubleTaskGraph;
typedef TaskGraph<void> voidTaskGraph;

int main( int argc, char *argv[] ) {
	intTaskGraph T1;
	taskgraph( intTaskGraph, T1, nil ) {
		tVar(int,a);
		a = 2;
		tReturn (a);
	}

	doubleTaskGraph T2;
	taskgraph( doubleTaskGraph, T2, nil ) {
		tVar(double,a);
		a = 2.345;
		tReturn (a);
	}

	voidTaskGraph T3;
	taskgraph( voidTaskGraph, T3, nil ) {
	}

	T1.compile();
	printf("2 = %d\n", T1() );

	T2.compile();
	printf("2.345 = %g\n", T2() );

	T3.compile();
	T3();
}
