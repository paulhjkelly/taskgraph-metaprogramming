#include <cstdio>
#include <cstdlib>
#include <vector>
#include <TaskGraph>

using namespace tg;

typedef TaskGraph<int, int> test_TaskGraph;
typedef TaskGraph<float, int> test2_TaskGraph;
typedef TaskGraph<int, float> test3_TaskGraph;
typedef TaskGraph<int, int> test4_TaskGraph;

int main(int argc, char* argv[]) {
  test_TaskGraph T;
  test2_TaskGraph U;
  test3_TaskGraph V;
  test4_TaskGraph W;

  TaskPipe tPipe;

  taskgraph( test_TaskGraph, T, tuple1(a)) {
    tPrintf("T\n");
    tVar(int, r);
    r = a + 1;
    tReturn(r);
  }

  taskgraph( test2_TaskGraph, U, tuple1(a)) {
    tPrintf("U\n");
    tVar(float, r);
    r = 3.14159 + a;
    tReturn(r);
  }

  taskgraph( test3_TaskGraph, V, tuple1(a)) {
    tPrintf("V\n");
    tVar(int, r);
    r = 2 * a;
    tReturn(r);
  }

  taskgraph( test4_TaskGraph, W, tuple1(a)) {
    tPrintf("W\n");
    tReturn(a);
  }

  T.compile( tg::SPU_GCC, false );
  U.compile( tg::SPU_GCC, false );
  V.compile( tg::SPU_GCC, false );
  W.compile( tg::SPU_GCC, false );

  tPipe.add(&T);
  tPipe.add(&U);
  tPipe.add(&V);
  tPipe.add(&W);

  int a = 1, b = 2, c = 3, d = 4, e = 5, f = 6, g = 7;

  int res __attribute__((aligned(SPU_ALIGN))) = 0;
  int res2 __attribute__((aligned(SPU_ALIGN))) = 0;
  int res3 __attribute__((aligned(SPU_ALIGN))) = 0;
  int res4 __attribute__((aligned(SPU_ALIGN))) = 0;
  int res5 __attribute__((aligned(SPU_ALIGN))) = 0;
  int res6 __attribute__((aligned(SPU_ALIGN))) = 0;
  int res7 __attribute__((aligned(SPU_ALIGN))) = 0;
 
  std::vector<void *> params;
  params.push_back(&a);
  params.push_back(&b);
  params.push_back(&c);
  params.push_back(&d);
  params.push_back(&e);
  params.push_back(&f);
  params.push_back(&g);

  tPipe.addParameters(params);

  tPipe.addResultDest(&res);
  tPipe.addResultDest(&res2);
  tPipe.addResultDest(&res3);
  tPipe.addResultDest(&res4);
  tPipe.addResultDest(&res5);
  tPipe.addResultDest(&res6);
  tPipe.addResultDest(&res7);

  tPipe.execute();

  printf("res = %d, res2 = %d, res3 = %d, res4 = %d, res5 = %d, res6 = %d, res7 = %d\n", res, res2, res3, res4, res5, res6, res7);
  //printf("res = %d\n", res);
}
