#include <cstdio>
#include <TaskGraph>

using namespace tg;

struct foo {
	short a;
	int b;
	float c;
};

#define FOOFIELDS ((short,a))((int,b))((float,c))
TASK_STRUCT(tgFoo,foo,FOOFIELDS)
#undef FOOFIELDS

struct bar {
	int x,y;
	foo f;
	//int ar[10];
};

#define BARFIELDS ((int,x))((int,y))((foo,f))
//((int[10],ar))
TASK_STRUCT(tgBar,bar,BARFIELDS)
#undef BARFIELDS

typedef TaskGraph<void,foo,bar[10],bar[5][5]> andor_TaskGraph;
typedef TaskGraph<void> test_TaskGraph;

int main( int argc, char *argv[] ) {
  andor_TaskGraph T;
  test_TaskGraph T2,T3;

  bar info;
  info.x = 5;
  info.y = 10;
  info.f.a = 1;
  info.f.b = 2;
  info.f.c = 3;
  taskgraph( andor_TaskGraph, T, tuple3(a,b,c) ) {
	  b[1].x = c[1][1].y;
	  b[1].y = a.a;
	  a.c = 10.4;

	  tVar(int,f);
	  tVar(bar,x);
	  tVar(bar,y);
	  f = x.x;
	  x.y = x.x * 2;
	  x.y = 2;

	  tVar(foo[2],farr);
	  farr[2].c = 10.4;

      b[2] = b[1];
	  b[2] = x;
	  x = y;
	  y = b[4];
	  x.f.b = 11;
	  b[9].f.b = 10;
	  b[9].f.a = 10;
	  b[10] = info;
  }
  taskgraph( test_TaskGraph, T2, tuple0() )
  {
   tVar(bar,z);
  }
  taskgraph( test_TaskGraph, T3, tuple0() )
  {
   tVar(bar,z);
  }
  T.emit();
  T.print();
  T.compile();
  foo mf;
  bar arr[10], arr2[5][5];
  mf.a = 10;
  arr2[1][1].y = 2;
  T.execute(mf, arr, arr2);
  printf("arr[1].x=%d\n", arr[1].x);
  printf("arr[1].y=%d\n", arr[1].y);
}

