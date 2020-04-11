#include <TaskGraph>

using namespace tg;

typedef TaskGraph<void,int> andor_TaskGraph;

int main( int argc, char *argv[] ) {
  andor_TaskGraph T;
  taskgraph( andor_TaskGraph, T, tuple1(a) ) {
    tVar(int, b);
    b = ((a < 12) + (a > 42));
    b = ((a > 6) - (a < 9));
    b = ((a < 12) * (a > 42));
    b = ((a > 6) / (a < 9));
    b = ((a < 12) & (a > 42));
    b = ((a > 6) | (a < 9));
    b = ((a < 12) && (a > 42));
    b = ((a > 6) || (a < 9));
    b = ((a < 12) << (a > 42));
    b = ((a > 6) >> (a < 9));
    tIf( b > 1 ) { }
  }
  T.print();
}
