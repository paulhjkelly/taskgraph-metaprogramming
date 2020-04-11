#include <TaskGraph>
#include <iostream>

#define COUNT_MAX	1000000

using namespace tg;

void c_fn (int &a, int &b, int &c, int &d) {
	int i, j, k, l;

  /* We even try to give the C++ compiler a helping hand by using
     references (not pointers) and declaring all loop variables
     individually */

	for (i = 1; i < 11; i++)
		b = b * i;

	for (j = 1; j < 11; j++)
		a = a + 1;

	for (k = 1; k < 11; k++)
		c *= b + a;

	for (l = 1; l < 11; l++)
		d = d + b - a;
}

typedef TaskGraph<void,int, int, int, int> fusion_TaskGraph;

int main (int argc, char **argv) {
	fusion_TaskGraph T;
	unsigned  count;
	int a, b, c, d;

	taskgraph ( fusion_TaskGraph, T, tuple4(a, b, c, d) ) {

		tVar ( int, i );
		// These first three are fusable
		tFor (i, 1, 10)
			b = b * i;

		tFor (i, 1, 10)
			a = a + 1;

		tFor (i, 1, 10)
			c = c * 2;

	/* This one can't be fused with any of the above because it
	 defines c and uses b and a... */
		tFor (i, 1, 10)
			c *= b + a;

      // But it can be fused with this one, because this one doesn't use c
		tFor (i, 1, 10)
			d = d + b - a;


		tFor (i, 1, 20)
			a += 10;

		c = 100;
		tFor (i, 1, 20)
			a += 10 + c;
	}

	std::cout << "Before optimisation:\n";

	T.print ( );

#ifdef USE_SUIF1_IR
	T.applyOptimisation ( "fusion" );
#endif

#ifdef USE_ROSE_IR
	T.allFission();
#endif

	std::cout << "\nAfter optimisation:\n";

	T.print ();

	std::cout << "\n\n";

	T.compile ();

	a = b = c = d = 1;
	tg::Timer ctime;
	for ( count = 0; count < COUNT_MAX; ++count )
 		c_fn (a, b, c, d);
	ctime.stop ( );

	a = b = c = d = 1;

	tg::Timer time;
	for ( count = 0; count < COUNT_MAX; ++count )
		T ( a, b, c, d );

	time.stop();

	std::cout << "C code executed in " << ctime.getTime() << " seconds.\n";
	std::cout << "Compiled TaskGraph executed in " << time.getTime() << " seconds.\n";
	theTimingInfo.print ( std::cout );
}
