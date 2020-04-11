#ifndef TASK_USER_FUNCTIONS_H__
#define TASK_USER_FUNCTIONS_H__

#include "TaskGraph.h"
#include "TaskType.h"
#include "TaskVariable.h"
#include "TaskDefines.h"

namespace tg {

template<typename R>
class TaskFunction {
public:
	TaskFunction ( const char *funcName ) : name ( funcName ), init ( false ), func ( 0 ) {
	}
	~TaskFunction() {
		if ( init ) {
			returnType->unref();
		}
	}
	void initialise ( unsigned numberArguments, Type **argumentTypes ) {
		if ( !init ) {
			returnType = &createType<R> () ( CURRENT_TASKGRAPH )->ref();
			func = CURRENT_TASKGRAPH->createFunction ( name, returnType, numberArguments, argumentTypes, false );
			init = true;
		}
	}
	TaskExpression call ( unsigned numberArguments, Type **argumentTypes, TaskIR::tgExpression *args ) {
		initialise ( numberArguments, argumentTypes );
		return CURRENT_TASKGRAPH->callFunction ( func, 1, args, returnType );
	}

protected:
	const char *name;
	bool init;
	TaskIR::tgFunction func;
	Type *returnType;
};

template<int SIZE> class TaskFunctionBase {
public:
	TaskFunctionBase ( const char *_name ) : name ( _name ), init ( false ) {
	}
	virtual ~TaskFunctionBase() {  }
protected:
	const char *name;
	bool init;
	TaskIR::tgFunction func;
	Type *returnType;
	Type *argumentType[SIZE];
};

template<typename R, typename ARG1>
class TaskFunction1 : public TaskFunctionBase<1> {
public:
	TaskFunction1 ( const char *funcName ) : TaskFunctionBase<1> ( funcName ) {
	}
	~TaskFunction1() {
		if ( init ) {
			argumentType[0]->unref();
			returnType->unref();
		}
	}
	void initialise ( ) {
		if ( !init ) {
			coreTaskGraph *tg = CURRENT_TASKGRAPH;
			argumentType[0] = createType<ARG1>() ( tg )->ref();
			returnType = createType<R>() ( tg )->ref();
			func = CURRENT_TASKGRAPH->createFunction ( name, returnType, 1, argumentType, false );
			init = true;
		}
	}
	TaskExpression call ( const TaskExpression &expr ) {
		initialise ( );
		TaskIR::tgExpression args[1];
		args[0] = expr.getExpression ( );
		return CURRENT_TASKGRAPH->callFunction ( func, 1, args, returnType );
	}
};

template<typename R, typename ARG1, typename ARG2>
class TaskFunction2 : public TaskFunctionBase<2> {
public:
	TaskFunction2 ( const char *funcName ) : TaskFunctionBase<2> ( funcName ) {
	}
	~TaskFunction2() {
		if ( init ) {
			argumentType[0]->unref();
			argumentType[1]->unref();
			returnType->unref();
		}
	}
	void initialise ( ) {
		if ( !init ) {
			coreTaskGraph *tg = CURRENT_TASKGRAPH;
			argumentType[0] = createType<ARG1>() ( tg )->ref();
			argumentType[1] = createType<ARG2>() ( tg )->ref();
			returnType = createType<R>() ( tg )->ref();
			func = CURRENT_TASKGRAPH->createFunction ( name, returnType, 2, argumentType, false );
			init = true;
		}
	}
	TaskExpression call ( const TaskExpression &expr1, const TaskExpression &expr2 ) {
		initialise ( );
		TaskIR::tgExpression args[2];
		args[0] = expr1.getExpression ( );
		args[1] = expr2.getExpression ( );
		return CURRENT_TASKGRAPH->callFunction ( func, 2, args, returnType );
	}
protected:
	TaskIR::tgExpression args[2];
};

template<typename R, typename ARG1, typename ARG2, typename ARG3>
class TaskFunction3 : public TaskFunctionBase<3> {
public:
		TaskFunction3 ( const char *funcName ) : TaskFunctionBase<3> ( funcName ) {
		}
		~TaskFunction3() {
			if ( init ) {
				argumentType[0]->unref();
				argumentType[1]->unref();
				argumentType[2]->unref();
				returnType->unref();
			}
		}
		void initialise ( ) {
			if ( !init ) {
				coreTaskGraph *tg = CURRENT_TASKGRAPH;
				argumentType[0] = createType<ARG1>() ( tg )->ref();
				argumentType[1] = createType<ARG2>() ( tg )->ref();
				argumentType[2] = createType<ARG3>() ( tg )->ref();
				returnType = createType<R>() ( tg )->ref();
				func = CURRENT_TASKGRAPH->createFunction ( name, returnType, 3, argumentType, false );
				init = true;
			}
		}
		TaskExpression call ( const TaskExpression &expr1, const TaskExpression &expr2 , const TaskExpression &expr3 ) {
			initialise ( );
			TaskIR::tgExpression args[3];
			args[0] = expr1.getExpression ( );
			args[1] = expr2.getExpression ( );
			args[2] = expr3.getExpression ( );
			return CURRENT_TASKGRAPH->callFunction ( func, 3, args, returnType );
		}
protected:
	TaskIR::tgExpression args[3];
};

}

#endif
