#ifndef TASKFUNCTIONS_H__
#define TASKFUNCTIONS_H__

#include "TaskDefines.h"
#include "TaskVariable.h"
#include <boost/preprocessor/repetition/repeat.hpp>
#include <boost/preprocessor/cat.hpp>

#define MAX_TPRINTF_PARAMS 10

namespace tg 
{

#define PRINTF_PARAM(z, n, text) , const TaskExpression & BOOST_PP_CAT(text,n)
#define PRINTF_MAKE_FUNC(z,SIZE, text) \
    void tPrintf (const char *test  BOOST_PP_REPEAT(SIZE, PRINTF_PARAM, expr) );

BOOST_PP_REPEAT(MAX_TPRINTF_PARAMS, PRINTF_MAKE_FUNC, dummy)

#undef PRINTF_PARAM
#undef PRINTF_MAKE_FUNC

TaskExpression tSqrt (const TaskExpression & expr);
TaskExpression tAbs (const TaskExpression & expr);
TaskExpression tLog (const TaskExpression & expr);
TaskExpression tLog10 (const TaskExpression & expr);
TaskExpression tPow (const TaskExpression & base, const TaskExpression & exponent);

TaskExpression tSqrtf (const TaskExpression & expr);
TaskExpression tAbsf (const TaskExpression & expr);
TaskExpression tLogf (const TaskExpression & expr);
TaskExpression tLog10f (const TaskExpression & expr);
TaskExpression tPowf (const TaskExpression & base, const TaskExpression & exponent);

}

#endif
