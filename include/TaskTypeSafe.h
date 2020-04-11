#ifndef TASKTYPESAFE_H__
#define TASKTYPESAFE_H__

#define TASKGRAPH_MAX_PARAMS 12

#include "TaskGraph.h"
#include "TaskDefines.h"
#include <boost/preprocessor/iteration/iterate.hpp>
#include <boost/preprocessor/iteration/local.hpp>
#include <boost/preprocessor/repetition/enum.hpp>
#include <boost/preprocessor/repetition/enum_params.hpp>
#include <boost/preprocessor/repetition/enum_params_with_a_default.hpp>
#include <boost/preprocessor/arithmetic/dec.hpp>
#include <boost/preprocessor/control/if.hpp>
#include <boost/preprocessor/comparison/equal.hpp>

#include <boost/mpl/list.hpp>
#include <boost/mpl/lambda.hpp>
#include <boost/mpl/count_if.hpp>
#include <boost/mpl/not.hpp>
#include <boost/type_traits/is_void.hpp>
#include <boost/type_traits/is_arithmetic.hpp>
#include <boost/type_traits/remove_all_extents.hpp>
#include <boost/static_assert.hpp>

namespace tg {

// This class handles arrays, by translating their type to a declarable type, like for type int[2], we can't write int[2] a; but instead need to write int a[2]. Also stores tVariableType, which is the type used inside the TaskGraph block for variables. This type is TaskScalarVariable for scalars and TaskArray<...> for arrays. see TypeConstructs.h for #define taskgraph
//template <typename T, unsigned depth = tg::arrayHelper<T>::depth > struct variableHelper {
// 
// tVariableType used by tVar macros to create object
// 
template <typename T, unsigned depth> struct variableHelper {
	// Throw a compile-time error if this traits class is being used for anything other than a arithmetic type
	BOOST_STATIC_ASSERT(boost::is_arithmetic<T>::value);
	typedef T  originalType;
	typedef T& declarationType;
	typedef tg::TaskScalarVariable tVariableType;
	typedef tg::TaskScalarVariable parameterType;
};

// We need the following version that handles the & referenced types, that scalars are turned into.
template <typename T> struct variableHelper<T&, 0> 
{
	inline static void *returnAddress (T& variable ) 
        {
		return &variable;
	}
};

template <typename T, unsigned N, unsigned D> struct variableHelper<T[N], D> {
	typedef T originalType[N];
	typedef T declarationType[N];
        typedef typename boost::remove_all_extents<T>::type baseType;
	typedef typename tg::TaskArray<D, baseType> tVariableType;
	typedef typename tg::TaskArray<D, baseType> parameterType;

	inline static void* returnAddress (declarationType variable) 
        {
		return variable;
	}
};

template <typename T> struct variableHelper<T*, 0> {
	typedef T* originalType;
	typedef T* declarationType;

	// TODO: This is wrong
	typedef tg::TaskPointer<T> tVariableType;
	typedef tg::TaskPointer<T> parameterType;

	inline static void* returnAddress(declarationType variable ) 
        {
		return variable;
	}
};

template <> struct variableHelper<void, 0> {
	typedef void declarationType;
	typedef void baseType;
};

// Par class contains parameter types, from 0 to 12. These types are stored as declarable (can be used in declaring variables) typedefs named type1 to type12 and also as their tg_typeInfo equivalents. The latter are available by an instance of Par and this way accessible from the taskgraph object. This way typeof was avoided.
template <BOOST_PP_ENUM_PARAMS_WITH_A_DEFAULT(TASKGRAPH_MAX_PARAMS, typename Par, void)>
struct Par {
	#define BOOST_PP_LOCAL_MACRO(N) typedef variableHelper<Par##N> type##N;
	#define BOOST_PP_LOCAL_LIMITS (0, BOOST_PP_DEC(TASKGRAPH_MAX_PARAMS))
	#include BOOST_PP_LOCAL_ITERATE()

	typedef boost::mpl::list<BOOST_PP_ENUM_PARAMS(TASKGRAPH_MAX_PARAMS, Par)> ParList;
	typedef boost::mpl::lambda< boost::mpl::not_< boost::is_void<boost::mpl::_1> > >::type NonVoidPred; 
	typedef typename boost::mpl::count_if<ParList, NonVoidPred>::type NonVoidCount;

	enum { parNum = NonVoidCount::value };
};

// TaskGraphClass with type checking. The third template argument is automatically instantiated and also used in specializing the function to 12 different implementations. Normally specialization to the number of parameters is only needed for the methods setParameters and execute, but partial specialization of members is only allowed if the whole class is specialized.
// TODO: check if these member functions can be separately templated, but using the parameterNumber. Then we could use on model for the class with generic template parameters P, R, PN and include specializations of the member functions only. The drawback is that user will have to include an additional empty <> when using these functions.
template <typename ParameterTypes, typename ReturnType, typename TaskGraphClass, int PN=ParameterTypes::parNum>
class SubTaskGraph : public TaskGraphClass {
public:
};

#define BOOST_PP_ITERATION_PARAMS_1 (3, (0, TASKGRAPH_MAX_PARAMS,"TaskTypeSafeHelper.h"))
#include BOOST_PP_ITERATE()
#undef BOOST_PP_ITERATION_PARAMS_1

template <typename ReturnType=void, BOOST_PP_ENUM_PARAMS_WITH_A_DEFAULT(TASKGRAPH_MAX_PARAMS, typename Par, void),
	  typename P=Par<BOOST_PP_ENUM_PARAMS(TASKGRAPH_MAX_PARAMS, Par)> >
class TaskGraph : public SubTaskGraph<P, ReturnType, ReturnTaskGraph<ReturnType> > {
public:
  typedef SubTaskGraph<P, ReturnType, ReturnTaskGraph<ReturnType> > tParentType; 

  TaskGraph()
  {
  }

  TaskGraph(const TaskGraph& t) : tParentType(t)
  {
  }

  TaskGraph& operator=(const TaskGraph& t)
  {
    tParentType::operator=(t);
    return *this;
  }

  TaskGraph duplicate() {
    return *this;
  }	
};

}

#endif

