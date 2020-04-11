#include <stdio.h>
#include <stdlib.h>
#include <TaskGraph>
using namespace tg;

typedef TaskGraph<int, void> variable_TaskGraph;

template<typename TYPE> void adder ( TaskScalarVariable &total ) {
	tVarTemplateType(TYPE,a);
	a = 100;
	total += a;
}

int main( int argc, char *argv[] ) {
	variable_TaskGraph T;
	taskgraph( variable_TaskGraph, T, tuple0() ) {
		tVar(int,total);
		total = 0;

		tVar(int,a);
		a = 1;
		tScope {
			tVar(int,a);
			a = 10;
			adder<int>(total);
			total += a;
		}
		total += a;
		tReturn(total);
	}
	T.compile();
	printf( "total = %d\n", T() );
}
