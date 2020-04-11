#ifndef RETURN_TASK_GRAPH_H__
#define RETURN_TASK_GRAPH_H__

#include <cassert>
#include "TaskDefines.h"
#include "TaskGraph.h"
#include "TaskCell.h"

namespace tg
{

template<typename ReturnType>
class ReturnTaskGraph : public coreTaskGraph {
public:
  typedef ReturnType (*FunctionType) ( void ** );
  
  ReturnTaskGraph() {
    returnTypeString = tg_typeInfo<ReturnType>::name();
  }
  
  ReturnTaskGraph(const ReturnTaskGraph &taskgraph) : coreTaskGraph(taskgraph) {
    returnType = taskgraph.returnType->ref();
    returnTypeString = taskgraph.returnTypeString;
  }
  
  ReturnTaskGraph& operator=(const ReturnTaskGraph &taskgraph) {
    coreTaskGraph::operator=(taskgraph);
    returnType = taskgraph.returnType->ref();
    returnTypeString = taskgraph.returnTypeString;
    return *this;
  }
  
  ReturnType execute ( ) 
  {
    ReturnType result;
    if (isSpuCode) {
      #ifdef CELL_SUPPORT

      #ifdef TASK_GRAPH_EXECUTION_TIMER
      theTimingInfo.executionTimer.start();
      #endif

      UnitTask tUnit(this);
      result = tUnit.execute<ReturnType>();
      
      #ifdef TASK_GRAPH_EXECUTION_TIMER
      theTimingInfo.executionTimer.stop();	
      #endif

      #else
      assert(false && "SPU code flag was set on a TaskGraph version compiled without Cell support. Please file a bug report.");
      #endif
    } 
    else 
    {
      FunctionType function = reinterpret_cast<FunctionType>(functionAddress);
      #ifdef TASK_GRAPH_EXECUTION_TIMER
      theTimingInfo.executionTimer.start ( );
      #endif

      result = function(&(parameterBinding[0]));

      #ifdef TASK_GRAPH_EXECUTION_TIMER
      theTimingInfo.executionTimer.stop ( );
      #endif
    }
    return result;
  }
	
  Type *getReturnType ( ) {
    return createType<ReturnType>()(this)->ref();
  }
	
  int getReturnSize() {
    return tg_typeInfo<ReturnType>::size;
  }
};

template<>
class ReturnTaskGraph<void> : public coreTaskGraph {
public:
	typedef void (*FunctionType) ( void ** );
  
  ReturnTaskGraph() {
    returnTypeString = tg_typeInfo<void>::name();
  }

  ReturnTaskGraph(const ReturnTaskGraph &taskgraph) : coreTaskGraph(taskgraph) {
    returnType = taskgraph.returnType->ref();
    returnTypeString = taskgraph.returnTypeString;
  }
  
  ReturnTaskGraph& operator=(const ReturnTaskGraph &taskgraph) {
    coreTaskGraph::operator=(taskgraph);
    returnType = taskgraph.returnType->ref();
    returnTypeString = taskgraph.returnTypeString;
    return *this;
  }

  void execute ( ) 
  {
    if (isSpuCode) {
      #ifdef CELL_SUPPORT

      #ifdef TASK_GRAPH_EXECUTION_TIMER
      theTimingInfo.executionTimer.start ( );
      #endif

      UnitTask tUnit(this);
      tUnit.execute();

      #ifdef TASK_GRAPH_EXECUTION_TIMER
      theTimingInfo.executionTimer.stop ( );
      #endif

      #else
      assert(false && "SPU code flag was set on a TaskGraph version compiled without Cell support. Please file a bug report.");
      #endif
    }
    else 
    {
      FunctionType function = reinterpret_cast<FunctionType>(functionAddress);
      
      #ifdef TASK_GRAPH_EXECUTION_TIMER
      theTimingInfo.executionTimer.start ( );
      #endif

      function(&(parameterBinding[0]));

      #ifdef TASK_GRAPH_EXECUTION_TIMER
      theTimingInfo.executionTimer.stop ( );
      #endif
    }
  }

  Type* getReturnType() {
    return createType<void>()(this)->ref();
  }

  int getReturnSize() {
    return 0;
  }
};

}

#endif
