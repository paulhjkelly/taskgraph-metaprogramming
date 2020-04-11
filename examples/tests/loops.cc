#include <cstdio>
#include <TaskGraph>

using namespace tg;

typedef TaskGraph<int> loops_TaskGraph;

void loopA ( loops_TaskGraph &T ) {
	taskgraph( loops_TaskGraph, T, tuple0() ) {
		tVar(int, i);
		tVar(int, total);
		total = 0;
		tFor(i,1,5) {
			total += i;
		}
		tReturn(total);
	}
}

void loopB ( loops_TaskGraph &T ) {
	taskgraph( loops_TaskGraph, T, tuple0() ) {
		tVar(int, i);
		tVar(int, total);
		total = 0;
		tFor(i,1,5) {
			tIf ( i == 2 )
				tContinue;
			tIf ( i == 4 )
				tBreak;
			total += i;
		}
		tReturn(total);
	}
}

void loopC ( loops_TaskGraph &T ) {
	taskgraph( loops_TaskGraph, T, tuple0() ) {
		tVar(int, i);
		tVar(int, total);
		total = 0;
		tForDown(i,5,1) {
			total += i;
		}
		tReturn(total);
	}
}

void loopD ( loops_TaskGraph &T ) {
	taskgraph( loops_TaskGraph, T, tuple0() ) {
		tVar(int, i);
		tVar(int, total);
		total = 0;
		tForStep(i,0,10,2) {
			total += i;
		}
		tReturn(total);
	}
}

int main( int argc, char *argv[] ) {
	loops_TaskGraph A, B, C, D;

	loopA(A);
	A.compile();
	printf ( "total = %d\n", A() );

	loopB(B);
	B.compile();
	printf ( "total = %d\n", B() );

	loopC(C);
	C.compile();
	printf ( "total = %d\n", C() );

	loopD(D);
	D.compile();
	printf ( "total = %d\n", D() );
}
