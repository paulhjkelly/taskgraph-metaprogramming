// This file has no include guards because it is intended to be included multiple times.

#define SUBTASKGRAPH_NUM_ARGS                                 BOOST_PP_ITERATION()
#define SUBTASKGRAPH_SINGLE_PARAM_DECL(Z, N, DATA)            typename P::type##N::declarationType par##N
#define SUBTASKGRAPH_SINGLE_PARAM_DECL_EXPAND(Z, N, DATA)     SUBTASKGRAPH_SINGLE_PARAM_DECL(Z, N, DATA)
#define SUBTASKGRAPH_PARAM_DECL                               BOOST_PP_ENUM(SUBTASKGRAPH_NUM_ARGS, SUBTASKGRAPH_SINGLE_PARAM_DECL_EXPAND, unused)
#define SUBTASKGRAPH_PARAMS                                   BOOST_PP_ENUM_PARAMS(SUBTASKGRAPH_NUM_ARGS, par)
#define SUBTASKGRAPH_SET_PARAMETER(N)                         setParameter(variableHelper<typename P::type##N::declarationType>::returnAddress(par##N), N);

template <typename P, typename R, typename TaskGraphClass>
class SubTaskGraph<P, R, TaskGraphClass, SUBTASKGRAPH_NUM_ARGS> : public TaskGraphClass
{
public:
	typedef P ParTypes;
	typedef R RetType;

protected: 
	void setParameter ( void *par, unsigned num ) {
		TaskGraphClass::parameterBinding[num] = par;
	}

public:
	enum { parNum = SUBTASKGRAPH_NUM_ARGS };
 
        SubTaskGraph()
        {
        }

        SubTaskGraph(const SubTaskGraph& s) : TaskGraphClass(s)
        {
        }
   
        SubTaskGraph& operator=(const SubTaskGraph& s)
        {
          TaskGraphClass::operator=(s);
          return *this;
        }

	void setParameters (SUBTASKGRAPH_PARAM_DECL) {
		#define BOOST_PP_LOCAL_MACRO(SUBTASKGRAPH_PAR_NUM) BOOST_PP_IF(BOOST_PP_EQUAL(SUBTASKGRAPH_NUM_ARGS, 0),, SUBTASKGRAPH_SET_PARAMETER(SUBTASKGRAPH_PAR_NUM))
		#define BOOST_PP_LOCAL_LIMITS (0, BOOST_PP_DEC(SUBTASKGRAPH_NUM_ARGS))
		#include BOOST_PP_LOCAL_ITERATE()
	}

	R execute (SUBTASKGRAPH_PARAM_DECL) {
		setParameters(SUBTASKGRAPH_PARAMS);
		return static_cast<R>(TaskGraphClass::execute());
	}

	R operator() (SUBTASKGRAPH_PARAM_DECL) {
		return execute(SUBTASKGRAPH_PARAMS);
	}
};

template <typename P, typename TaskGraphClass>
class SubTaskGraph<P, void, TaskGraphClass, SUBTASKGRAPH_NUM_ARGS> : public TaskGraphClass
{
public: 
	typedef P ParTypes;
	typedef void RetType;

protected:
	void setParameter ( void *par, unsigned num ) { 
		TaskGraphClass::parameterBinding[num] = par; 
	}

public:
	enum { parNum = SUBTASKGRAPH_NUM_ARGS };

        SubTaskGraph()
        {
        }

        SubTaskGraph(const SubTaskGraph& s) : TaskGraphClass(s)
        {
        }

        SubTaskGraph& operator=(const SubTaskGraph& s)
        {
          TaskGraphClass::operator=(s);
          return *this;
        }

	void setParameters (SUBTASKGRAPH_PARAM_DECL) {
		#define BOOST_PP_LOCAL_MACRO(SUBTASKGRAPH_PAR_NUM) BOOST_PP_IF(BOOST_PP_EQUAL(SUBTASKGRAPH_NUM_ARGS, 0),, SUBTASKGRAPH_SET_PARAMETER(SUBTASKGRAPH_PAR_NUM))
		#define BOOST_PP_LOCAL_LIMITS (0, BOOST_PP_DEC(SUBTASKGRAPH_NUM_ARGS))
		#include BOOST_PP_LOCAL_ITERATE()
	}

	void execute (SUBTASKGRAPH_PARAM_DECL) {
		setParameters(SUBTASKGRAPH_PARAMS);
		TaskGraphClass::execute();
	}

	void operator() (SUBTASKGRAPH_PARAM_DECL) {
		execute(SUBTASKGRAPH_PARAMS);
	}
};
