#include <stdio.h>
#include <stdlib.h>
#include <TaskGraph>
using namespace tg;

typedef TaskGraph <void,int> addc_TaskGraph;

int main( int argc, char *argv[] ) {
  if (argc != 3) {
    printf("Usage: multipletg b c\n");
    return -1;
  }
   int b = 1;
  int c = atoi( argv[1] );
  int count = atoi(argv[2]);
  for (int i = 0; i < count; i++) {
  addc_TaskGraph *T = new addc_TaskGraph();
  taskgraph(addc_TaskGraph,*T, tuple1(a) ) {
    tVar (int, x);
    tVar (float, y);
    tVar (double[10], dd);
    tWhile ( a < count ) {
      a = a + c;
    }
    tIf ( a == count ) {
      a = 1;
    } tElse {
      a = 0;
    }
    x = 0;
    y = 4.2;
    dd[1] = dd[2];
    y = dd[3];
    dd[4] = y;
    tPrintf("Done iteration %d (inside taskgraph)\n", i);
  }
  T->compile( tg::GCC, true );
  T->execute( b );
  printf( "b = %d\n", b );
  delete T;
  }
}
