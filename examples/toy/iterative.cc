#include <TaskGraph>
#include "TaskIterative.h"

using namespace tg;

typedef TaskGraph<void,int> iterative_TaskGraph;

void taskTest (coreTaskGraph *&t, unsigned param) 
{
  iterative_TaskGraph* const tg = new iterative_TaskGraph();
  taskgraph(iterative_TaskGraph, *tg, tuple1(b)) 
  {
    tPrintf("param + b = %d\n", param + b);
  }
  t = tg;
}

int main(int argc, char* argv[])
{
	OneVariableIterative iter(taskTest, 8, 128*2, 4, 4);
	int a = (argc > 1) ? atoi(argv[1]) : 0;
	unsigned int param;
	for (int i = 0; i < 150; ++i) 
	{
	  iterative_TaskGraph* const iterative = static_cast<iterative_TaskGraph *>(iter.getNext(&param));
	  tg::Timer timer;
	  iterative->execute(a);
	  timer.stop();
	  iter.setResult(param, timer.getTime());
	}
}
