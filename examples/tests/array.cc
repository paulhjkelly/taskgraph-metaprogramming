#include <stdio.h>
#include <stdlib.h>
#include <TaskGraph>
using namespace tg;

typedef TaskGraph<int, int[10],int[10][10],int[10][10][10],int[10][10][10][10]> variable_TaskGraph;

int main( int argc, char *argv[] ) {
	variable_TaskGraph T;
	taskgraph( variable_TaskGraph, T, tuple4(a,b,c,d) ) {
		tVar(int,total);
		total = a[2] + b[2][5] + c[4][6][7] + d[3][7][2][8];
		tVar(int[5][5][5][5],x);
		x[1][2][3][4] = 25;
		total += x[1][2][3][4];
		tReturn(total);
	}
	int a[10];
	int b[10][10];
	int c[10][10][10];
	int d[10][10][10][10];
	a[2] = 1;
	b[2][5] = 4;
	c[4][6][7] = 9;
	d[3][7][2][8] = 16;
	T.compile();
	printf( "total = %d\n", T(a,b,c,d) );
}
