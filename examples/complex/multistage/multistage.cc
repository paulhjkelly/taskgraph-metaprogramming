/*
 * This example demonstrates using the taskgraph for mulit-stage
 * programming (i.e. taskgraphs which build taskgraphs).
 *
 * NOTE: This example does not work under cygwin.
 *
 */

#include <cstdio>
#include <TaskGraph>
#include "TaskUserFunctions.h"

using namespace tg;

typedef TaskGraph<int, int> IntToIntTG;

int mul ( int a, int b ) {
	IntToIntTG T2;

	taskgraph ( IntToIntTG, T2, tuple1( a ) ) {
		tVar ( int, res );
		int c = b;
		res = 0;
		while ( b-- > 0 )
			res += a;
		tPrintf ( "TaskGraph in second stage: %d * %d = %d\n", a, c, res );
		tReturn ( res );
	}
	T2.compile ( tg::GCC );

	return T2.execute ( a );
}


extern "C" int myFunc ( int a, int b ) {
	return mul ( a, b );
}

TaskExpression tMul ( const TaskExpression &expr1, const TaskExpression &expr2 ) {
	static TaskFunction2<int, int, int> func ( "myFunc" );

	return func.call ( expr1, expr2 );
}

int main ( ) {
#ifdef ENV_CYGWIN
	fprintf(stderr, "This example does not work in cygwin due to linking issues in Windows\n");
	return -1;
#endif
	IntToIntTG T1;

	int a = 5;

	taskgraph ( IntToIntTG, T1, tuple1 ( a ) ) {
		tVar ( int, i );
		tVar ( int, tmpres );
		tVar ( int, sum );

		sum = 0;
		tFor ( i, 1, a ) {
			tmpres = tMul ( i, i );
			sum += tmpres;
			tPrintf ( "TaskGraph in first stage: i = %d, result from second stage = %d, sum = %d\n", i, tmpres, sum );
		}
		tReturn ( sum );
	}

	T1.compile ( tg::GCC );
	printf ( "Final sum = %d\n", T1.execute ( a ) );
}
