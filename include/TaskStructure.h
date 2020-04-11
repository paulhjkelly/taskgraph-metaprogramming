#ifndef TASK_STRUCTURE_H__
#define TASK_STRUCTURE_H__

#include <boost/preprocessor/seq/for_each.hpp> 
#include <boost/preprocessor/tuple/elem.hpp>
#include <boost/preprocessor/seq/for_each_i.hpp>
#include <boost/preprocessor/stringize.hpp>
#include <boost/preprocessor/seq/size.hpp>
#include <boost/preprocessor/punctuation/comma_if.hpp> 

#define STRUCT_TYPE_INFO(TASKTYPE,REALTYPE,SIZE) \
namespace tg{ \
template<> \
struct tg_typeInfo<REALTYPE>  { \
	enum { _depth = 0 }; \
	enum { _isSigned = false }; \
	enum { _size = SIZE }; \
	static const unsigned size = SIZE; \
	static const bool isSigned = false; \
	static const unsigned depth = 0; \
	static const TaskType type = typeStructure; \
	typedef void arrayType; \
	typedef void pointer_type; \
	typedef TASKTYPE TaskStructure; \
	static std::string name ( ) { \
		return #REALTYPE; \
	} \
}; \
 \
template <> struct variableHelper<REALTYPE,0> { \
	typedef REALTYPE  originalType; \
	typedef REALTYPE& declarationType; \
	typedef TASKTYPE  tVariableType; \
	typedef TASKTYPE  parameterType; \
}; \
}

#define TASK_DO_INIT_LIST(r,REALTYPE,i,elem) BOOST_PP_COMMA_IF(i) BOOST_PP_TUPLE_ELEM(2,1,elem)(this,offsetof(REALTYPE,BOOST_PP_TUPLE_ELEM(2,1,elem)), tg::createType<BOOST_PP_TUPLE_ELEM(2,0,elem)>() ( CURRENT_TASKGRAPH ))
#define INITFILEDS(REALTYPE,FIELDS) BOOST_PP_SEQ_FOR_EACH_I(TASK_DO_INIT_LIST, REALTYPE, FIELDS)

#define BEGIN_TASK_STRUCT(NAME, REALTYPE, FIELDS) \
struct NAME : public TaskStructure { \
	NAME( const char *_name ) \
	: TaskStructure ( _name, 0, getStaticType(), CURRENT_TASKGRAPH->getAddingParameters ( ) ), \
      INITFILEDS(REALTYPE,FIELDS)\
    { \
		symbol = CURRENT_TASKGRAPH->addVariableDefinition ( getName().c_str(), isParameter(), getType() ); \
	} \
	NAME( const TaskExpressionSymbol& _base ) : TaskStructure( _base, getStaticType() ), \
        INITFILEDS(REALTYPE,FIELDS)\
    { \
	} \
    NAME ( TaskStructure *str, int _offset, Type *_type ) \
        : TaskStructure ( str, _offset, _type ), \
        INITFILEDS(REALTYPE,FIELDS)\
    { \
    } \
	NAME( const char *name, coreTaskGraph *owner, StructType *_type ) \
	: TaskStructure ( name, 0, _type, owner->getAddingParameters ( ) ), \
        INITFILEDS(REALTYPE,FIELDS)\
    { \
		symbol = owner->addVariableDefinition ( getName().c_str(), isParameter(), _type ); \
	} \
	void operator=( const NAME &rhs ) { \
		TaskStructure::operator=(rhs); \
	} \
	StructType *getType() const { \
		return getStaticType(); \
	} \
	static NAME createFromBase(const TaskExpressionSymbol& base ) { \
		return NAME(base); \
	} \
	typedef NAME return_type; \
private: \
        struct Cleaner\
        {\
          std::vector<Type*> fieldTypes;\
          \
          ~Cleaner() \
          {\
            for(std::vector<Type*>::iterator fieldTypeIter(fieldTypes.begin()); fieldTypeIter!=fieldTypes.end(); ++fieldTypeIter)\
              (*fieldTypeIter)->unref();\
            statictype->unref();\
          }\
        };\
\
	static StructType *statictype; \
        static Cleaner cleaner;\
\
public:


#define TASK_DO_FIELDS(r,data,elem) tg::variableHelper<BOOST_PP_TUPLE_ELEM(2,0,elem)>::tVariableType BOOST_PP_TUPLE_ELEM(2,1,elem);
#define TASK_DO_REGISTER(r,REALTYPE,i,elem) TaskStructure::regField<BOOST_PP_TUPLE_ELEM(2,0,elem)>(cleaner.fieldTypes, statictype, BOOST_PP_STRINGIZE(BOOST_PP_TUPLE_ELEM(2,1,elem)), i, offsetof(REALTYPE,BOOST_PP_TUPLE_ELEM(2,1,elem)) );
#define TASK_DO_ASSIGNMENT(r,REALTYPE,elem) BOOST_PP_TUPLE_ELEM(2,1,elem) = rhs.BOOST_PP_TUPLE_ELEM(2,1,elem);

#define TASK_FILL_FIELDS(FIELDS,TASKTYPE,REALTYPE,SIZE) \
BOOST_PP_SEQ_FOR_EACH(TASK_DO_FIELDS, _, FIELDS) \
static StructType *getStaticType() { \
	if (statictype == 0) { \
        TASKTYPE::cleaner.fieldTypes.reserve(BOOST_PP_SEQ_SIZE(FIELDS)); \
		statictype = TaskStructure::reg(#TASKTYPE,getSize(),getFields()); \
BOOST_PP_SEQ_FOR_EACH_I(TASK_DO_REGISTER, REALTYPE, FIELDS) \
		TaskStructure::installType(statictype); \
                statictype->ref();\
	} \
	return statictype; \
} \
	void operator=( const REALTYPE &rhs ) { \
BOOST_PP_SEQ_FOR_EACH(TASK_DO_ASSIGNMENT, _, FIELDS) \
	} \
    static unsigned getSize() { \
       return SIZE; \
    } \
    static unsigned getFields() { \
        return BOOST_PP_SEQ_SIZE(FIELDS); \
    } \
};

#define DO_STATICS(TASKTYPE) \
StructType *TASKTYPE::statictype = 0; \
TASKTYPE::Cleaner TASKTYPE::cleaner;


#define TASK_STRUCT(TASKTYPE,REALTYPE,FIELDS) \
BEGIN_TASK_STRUCT(TASKTYPE,REALTYPE,FIELDS) \
TASK_FILL_FIELDS(FIELDS,TASKTYPE,REALTYPE,sizeof(REALTYPE)) \
STRUCT_TYPE_INFO(TASKTYPE,REALTYPE,sizeof(REALTYPE)) \
DO_STATICS(TASKTYPE)

#endif

