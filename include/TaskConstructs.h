#ifndef TASKCONSTRUCTS_H__
#define TASKCONSTRUCTS_H__

#include "TaskDefines.h"
#include "TaskGraph.h"
#include <boost/preprocessor/repetition/repeat.hpp>
#include <boost/preprocessor/array/elem.hpp>
#include <boost/preprocessor/array/size.hpp>

namespace tg 
{

// Used by the construct
inline coreTaskGraph *switchToElsePart__ ( coreTaskGraph *taskgraph ) {
  BlockStructure *block = taskgraph->reviveLastBlockStructure ( );
  static_cast<IfBlock *>(block)->switchToElsePart ( );
  return taskgraph;
}

// Used by taskgraph macro below
inline coreTaskGraph *taskGraphBegin__ ( coreTaskGraph *taskgraph, const char *name ) {
  coreTaskGraph::setCurrentTaskGraph ( taskgraph );
  taskgraph->begin ( name );
  return taskgraph;
}

inline coreTaskGraph *taskGraphContinue__ ( coreTaskGraph *taskgraph ) {
  taskgraph->continueEditing ( );
  return taskgraph;
}

struct FinishBlockEnder {
  inline static void begin ( coreTaskGraph *taskgraph ) {
  }
  inline static void end ( coreTaskGraph *taskgraph ) {
  	taskgraph->finishBlock ( );
  }
};

struct FinishTaskGraphEnder {
  inline static void begin ( coreTaskGraph *taskgraph ) {
  }
  inline static void end ( coreTaskGraph *taskgraph ) {
  	taskgraph->finishBlock ( );
  	taskgraph->finish ( );
  }
};

template<typename Ender>
struct BlockEnder {
private:
  coreTaskGraph *taskgraph;
  bool cnt;

public:
  inline BlockEnder ( coreTaskGraph *_taskgraph ) {
    taskgraph = _taskgraph;
    Ender::begin ( taskgraph );
    cnt = true;
  }

  inline ~BlockEnder ( ) {
    Ender::end ( taskgraph );
  }

  inline bool hasDone ( ) const {
    return cnt;
  }

  inline void done ( ) {
    cnt = false;
  }
};

}

// Macros used for taskgraph declaration
#define nil tuple0()
#define tuple0() (0, ())
#define tuple1(x1) (1, (x1))
#define tuple2(x1, x2) (2, (x1, x2))
#define tuple3(x1, x2, x3) (3, (x1, x2, x3))
#define tuple4(x1, x2, x3, x4) (4, (x1, x2, x3, x4))
#define tuple5(x1, x2, x3, x4, x5) (5, (x1, x2, x3, x4, x5))
#define tuple6(x1, x2, x3, x4, x5, x6) (6, (x1, x2, x3, x4, x5, x6))
#define tuple7(x1, x2, x3, x4, x5, x6, x7) (7, (x1, x2, x3, x4, x5, x6, x7))
#define tuple8(x1, x2, x3, x4, x5, x6, x7, x8) (8, (x1, x2, x3, x4, x5, x6, x7, x8))
#define tuple9(x1, x2, x3, x4, x5, x6, x7, x8, x9) (9, (x1, x2, x3, x4, x5, x6, x7, x8, x9))
#define tuple10(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10) (10, (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10))
#define tuple11(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11) (11, (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11))
#define tuple12(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12) (12, (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12))

#define _beginTaskgraph(x)  for ( tg::BlockEnder<tg::FinishTaskGraphEnder> __ender( tg::taskGraphBegin__ ( &(x), #x ) ); __ender.hasDone(); __ender.done() )
#define _parameter(type, name) for ( tTypeSafeParameter(type, name); __ender.hasDone(); __ender.done()	)
#define _parameterStart  for ( CURRENT_TASKGRAPH->setAddingParameters ( true ); __ender.hasDone(); __ender.done())
#define _parameterEnd  for ( CURRENT_TASKGRAPH->setAddingParameters ( false ); __ender.hasDone(); __ender.done())
#define TASKGRAPH_PARAMETER(z, n, data) _parameter(BOOST_PP_ARRAY_ELEM(0, data)::ParTypes::type##n, BOOST_PP_ARRAY_ELEM(n, BOOST_PP_ARRAY_ELEM(1, data)))

// definition of taskgraph macro
#define taskgraph(type, t, params) \
        _beginTaskgraph(t) \
        _parameterStart \
        BOOST_PP_REPEAT(BOOST_PP_ARRAY_SIZE(params), TASKGRAPH_PARAMETER, (2, (type, params))) \
        _parameterEnd

#define taskgraphUnique(x) for ( tg::BlockEnder<tg::FinishTaskGraphEnder> __ender( tg::taskGraphBegin__ ( &x, NULL ) ); __ender.hasDone(); __ender.done() )
#define continueTaskgraph(x) for ( tg::BlockEnder<tg::FinishTaskGraphEnder> __ender( tg::taskGraphContinue__ ( &x ) ); __ender.hasDone(); __ender.done() )

#define tIf(expr) for ( tg::BlockEnder<tg::FinishBlockEnder> __ender(CURRENT_TASKGRAPH->startIfBlock ( expr ) ); __ender.hasDone(); __ender.done() )
#define tElse for ( tg::BlockEnder<tg::FinishBlockEnder> __ender(switchToElsePart__ ( CURRENT_TASKGRAPH ) ); __ender.hasDone(); __ender.done() )

// TODO: Figure out how to make them work
//#define tElseIf(expr) for ( BlockEnder<FinishBlockEnder> __ender(switchToElsePart__ ( CURRENT_TASKGRAPH )->startIfBlock ( expr ) ); __ender.hasDone(); __ender.done() )

#define tWhile(expr) for ( tg::BlockEnder<tg::FinishBlockEnder> __ender(CURRENT_TASKGRAPH->startWhileBlock ( expr ) ); __ender.hasDone(); __ender.done() )

#define tFor(var,from,to) for ( tg::BlockEnder<tg::FinishBlockEnder> __ender(CURRENT_TASKGRAPH->startForBlock ( var, from, to, 1, opLESSTHANOREQUALTO ) ); __ender.hasDone(); __ender.done() )
#define tForStep(var,from,to,step) for ( tg::BlockEnder<tg::FinishBlockEnder> __ender(CURRENT_TASKGRAPH->startForBlock ( var, from, to, step, opLESSTHANOREQUALTO ) ); __ender.hasDone(); __ender.done() )
#define tForDown(var,from,to) for ( tg::BlockEnder<tg::FinishBlockEnder> __ender(CURRENT_TASKGRAPH->startForBlock ( var, from, to, -1, opGREATERTHANOREQUALTO ) ); __ender.hasDone(); __ender.done() )
#define tForDownStep(var,from,to,step) for ( tg::BlockEnder<tg::FinishBlockEnder> __ender(CURRENT_TASKGRAPH->startForBlock ( var, from, to, step, opGREATERTHANOREQUALTO ) ); __ender.hasDone(); __ender.done() )

#define tScope for ( tg::BlockEnder<tg::FinishBlockEnder> __ender(CURRENT_TASKGRAPH->startScopeBlock ( ) ); __ender.hasDone(); __ender.done() )

#define tBreak CURRENT_TASKGRAPH->addBreakStatement ( );
#define tContinue CURRENT_TASKGRAPH->addContinueStatement ( );

#define tGetId(x) CURRENT_TASKGRAPH->setIdentifier ( x );

#define tReturn(x) CURRENT_TASKGRAPH->addReturnStatement ( x );


#define tu_taskgraph(x) for ( tg::BlockEnder<tg::FinishTaskGraphEnder> __ender( tg::taskGraphBegin__ ( &(x), #x ) ); __ender.hasDone(); __ender.done() )

namespace tg 
{
  typedef ReturnTaskGraph<void> tuTaskGraph;
}

#endif

