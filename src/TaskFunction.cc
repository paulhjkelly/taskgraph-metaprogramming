#include "TaskFunction.h"
#include "TaskUserFunctions.h"

namespace {

using namespace tg;

TaskIR::tgFunction initPrintf ( ) {
	static Type *arguments[] = { createType<char *>() ( CURRENT_TASKGRAPH ) };
	static Type *returnType = createType<int>() ( CURRENT_TASKGRAPH );
	static bool init = false;
	static TaskIR::tgFunction func = 0;

	if ( !init ) {
		func = CURRENT_TASKGRAPH->createFunction ( "printf", returnType, 1, arguments, true );
		init = true;
	}
	return func;
}

}

#include <boost/preprocessor/repetition/repeat.hpp>
#include <boost/preprocessor/cat.hpp>

namespace tg 
{

#define PRINTF_PARAM(z, n, text) , const TaskExpression & BOOST_PP_CAT(text,n)
#define PRINTF_DO_PARAM_LIST(SIZE) BOOST_PP_REPEAT(SIZE, PRINTF_PARAM, expr)
#define PRINTF_DO_GET_EXPRESSION(z, n, text) args[n+1] = BOOST_PP_CAT(expr,n).getExpression ( );

#define PRINTF_DO_FUNCTION(SIZE) \
    TaskIR::tgFunction func = initPrintf (); \
    std::vector<TaskIR::tgExpression> args(SIZE+1); \
    args[0] = TaskExpression ( test ).getExpression ( ); \
    BOOST_PP_REPEAT(SIZE, PRINTF_DO_GET_EXPRESSION, dummy) \
    CURRENT_TASKGRAPH->callFunction ( func, SIZE+1, &args[0] );

#define PRINTF_MAKE_FUNC(z,SIZE, text) \
    void tPrintf (const char *test  PRINTF_DO_PARAM_LIST(SIZE) ) { PRINTF_DO_FUNCTION(SIZE) }

BOOST_PP_REPEAT(MAX_TPRINTF_PARAMS, PRINTF_MAKE_FUNC, dummy)


// Double versions

TaskExpression tSqrt ( const TaskExpression &expr ) {
	static TaskFunction1<double, double> func ( "sqrt" );
	return func.call ( expr );
}

TaskExpression tAbs ( const TaskExpression &expr ) {
	static TaskFunction1<double, double> func ( "fabs" );
	return func.call ( expr );
}

TaskExpression tLog ( const TaskExpression &expr ) {
	static TaskFunction1<double, double> func ( "log" );
	return func.call ( expr );
}

TaskExpression tLog10 ( const TaskExpression &expr ) {
	static TaskFunction1<double, double> func ( "log10" );
	return func.call ( expr );
}

TaskExpression tPow ( const TaskExpression &base, const TaskExpression &exponent ) {
	static TaskFunction2<double, double, double> func ( "pow" );
	return func.call ( base, exponent );
}


// Float versions

TaskExpression tSqrtf ( const TaskExpression &expr ) {
	static TaskFunction1<float, float> func ( "sqrtf" );
	return func.call ( expr );
}

TaskExpression tAbsf ( const TaskExpression &expr ) {
	static TaskFunction1<float, float> func ( "fabsf" );
	return func.call ( expr );
}

TaskExpression tLogf ( const TaskExpression &expr ) {
	static TaskFunction1<float, float> func ( "logf" );
	return func.call ( expr );
}

TaskExpression tLog10f ( const TaskExpression &expr ) {
	static TaskFunction1<float, float> func ( "log10f" );
	return func.call ( expr );
}

TaskExpression tPowf ( const TaskExpression &base, const TaskExpression &exponent ) {
	static TaskFunction2<float, float, float> func ( "powf" );
	return func.call ( base, exponent );
}

}
