#include <TaskGraph>

using namespace tg;

typedef TaskGraph<void> fusion_TaskGraph;

int main (int argc, char **argv) {
	fusion_TaskGraph T;

	taskgraph ( fusion_TaskGraph, T, nil ) {
		tVar(int [100], a);
		tVar(int [100], b);
		tVar(int [100][100], aa);
		tVar(int [100][100], bb);
		tVar(int, c);
		tVar(int, i);
		tVar(int, j);
		tVar(int, k);

	// No
		tFor (i, 5, 20)
			a[i] = 100 + c;
		c = 20;
		tFor (i, 5, 20)
			a[i+1] = 100;

		c = 1000;
	// Yes*2
		tFor (i, 5, 20)
			a[i] = 100 + c;
		c = 20;
		tFor (i, 5, 20)
			a[i] = 100;


		tFor (i, 2, 20) {
			tFor (j, 5, 20)
				a[i] = a[i];
		}
		tFor (i, 2, 20) {
			tFor (j, 5, 20)
				a[i-2] = a[i];
		}
		tFor (j, 5, 20)
			a[j] = 1;
		tFor (j, 5, 20)
			a[j-1] = 1;
		tFor (i, 5, 30) {
			tFor (j, 5, 20)
				a[j] = 10 + c;
			c = 20;
			tFor (j, 5, 20)
				a[j-2] += 100;
		}
	}

	T.applyOptimisation ( "fusion" );

	T.print ();
}
