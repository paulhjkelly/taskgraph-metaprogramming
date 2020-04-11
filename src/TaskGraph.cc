#include <TaskGraph.h>
#include <TaskCompilers.h>
#include <TaskType.h>
#include <TaskVariable.h>
#include <TaskIR.h>
#include <TaskException.h>
#include <TaskCell.h>
#include <cstdarg>
#include <cstdlib>
#include <cstdio>
#include <climits>
#include <cassert>
#include <iostream>
#include <ostream>
#include <string>
#include <algorithm>

#include <unistd.h>
#include <dlfcn.h>
#include <sys/types.h> 

#include <functional>
#include <list>
#include <typeinfo>
#include <sstream>
#include <boost/bind.hpp>
#include <boost/integer.hpp>
#include <boost/static_assert.hpp>
#include <boost/shared_ptr.hpp>
#include <boost/type_traits/is_function.hpp>
#include <boost/type_traits/is_pointer.hpp>
#include <boost/type_traits/remove_pointer.hpp>

namespace 
{

using namespace tg;
// The prefix of the functions generated in C
const std::string functionNamePrefix = "taskGraph_";

void compile( TaskCompiler *compiler, bool keepSourceCode,
              TaskCompiledCode *code, TaskIROutputter *outputter) 
{

  // Make filenames
  static int num=0;
  const uid_t uid = getuid();

  std::stringstream uniqueIdStream;
  uniqueIdStream << uid << "-" << num++;
  const std::string uniqueId = uniqueIdStream.str();

  std::string objName = "/tmp/TaskGraph-Object-" + uniqueId;
  const std::string sourceName = "/tmp/TaskGraph-Source-" + uniqueId + ".c";
  const std::string asmName = "/tmp/TaskGraph-Asmblr-" + uniqueId + ".s";

  #ifdef ENV_CYGWIN
  objName+=".dll";
  #endif

  outputter->outputToC (sourceName.c_str());

  #ifdef TASKGRAPH_TIMING
  theTimingInfo.compileTimer.start();
  #endif

  // Set off the compiler
  compiler->compile (sourceName.c_str(), objName.c_str(), asmName.c_str());

  #ifdef TASKGRAPH_TIMING
  theTimingInfo.compileTimer.stop();
  #endif

  code->load (objName.c_str()) ;

  if (!keepSourceCode) {
    unlink(sourceName.c_str());
    unlink(asmName.c_str());
  }
}

void errorFunction ( void ** ) {
  std::cerr <<  "You need to compile a TaskGraph object before executing it." << std::endl;
  exit(EXIT_FAILURE);
}

}

namespace tg 
{

TimingInfo theTimingInfo;

#ifdef TASKGRAPH_TIMING
void TimingInfo::print ( std::ostream &stream ) {
  stream << "Creation time: " << creationTimer.getTime() << " seconds" << std::endl;
  stream << "Compile time : " << compileTimer.getTime() << " seconds" << std::endl;

  #ifdef TASKGRAPH_EXECUTION_TIMER
  stream << "Total Execution time:" << executionTimer.getTime() << " seconds" << std::endl;
  double time = executionTimer.getTime();
  unsigned restarts = executionTimer.getRestarts();
  if ( restarts > 0 )
    time /= restarts;
  stream << "Avg. Execution time :" << time << " seconds for " << restarts << " executions" << std::endl;
  #endif
}

void TimingInfo::reset ( ) {
  compileTimer.reset();
  creationTimer.reset();
  #ifdef TASKGRAPH_EXECUTION_TIMER
  executionTimer.reset();
  #endif
}
#endif

void NativeCompiledCode::load (const char *fileName) {
  // Load the shared lib
  objName = fileName;
  sharedLib = dlopen ( fileName, RTLD_LAZY | RTLD_GLOBAL );
  if ( sharedLib == NULL ) {
    throw TaskCompileException ( std::string ( "dlopen error:" ) + dlerror ( ) );
  }
}

NativeCompiledCode::~NativeCompiledCode() {
  if ( sharedLib != NULL ) {
    dlclose  ( sharedLib );
    unlink ( objName.c_str ( ) );
  }
}

// C++ doesn't allow void pointers to be casted to function pointers but the POSIX dynamic linking
// interface depends on it. This casts a void pointer to a function pointer via an intermediate 
// integral type of the same size.
template<typename result_type>
result_type void_pointer_to_function_pointer_cast(void* const pointer)
{
  typedef boost::uint_t<sizeof(void*) * CHAR_BIT>::least integral_type;

  // Check result type really is a function pointer
  BOOST_STATIC_ASSERT(boost::is_pointer<result_type>::value);
  BOOST_STATIC_ASSERT(boost::is_function<typename boost::remove_pointer<result_type>::type>::value);

  // Check the source, destination and intermediate types are all the same size
  BOOST_STATIC_ASSERT(sizeof(integral_type) == sizeof(void*));
  BOOST_STATIC_ASSERT(sizeof(integral_type) == sizeof(result_type));

  const integral_type intermediate = reinterpret_cast<integral_type>(pointer);
  const result_type function_pointer = reinterpret_cast<result_type>(intermediate);
  return function_pointer;
}

taskGraphFunction NativeCompiledCode::getFunctionAddress (const char *name) {
  taskGraphFunction func = void_pointer_to_function_pointer_cast<taskGraphFunction>(dlsym ( sharedLib, name ));
  const char* error = dlerror ( );
  if ( error != NULL ) {
    throw TaskCompileException ( error );
  }

  return func;
}


TaskGraphGroup::TaskGraphGroup ( ) : code (), group ( new TaskIRGroup ) {
}

TaskGraphGroup::~TaskGraphGroup ( ) {
  delete group;
}

void TaskGraphGroup::compile ( int compiler, bool keepSourceCode ) {
  TaskCompiler* taskCompiler;
  if ( compiler == tg::GCC ) {
    taskCompiler = new GnuCCompiler();
  } else if ( compiler == tg::GCC_DEBUG ) {
    GnuCCompiler* const gccCompiler = new GnuCCompiler();
    gccCompiler->setDebug ( true );
    taskCompiler = gccCompiler;
  } else if ( compiler == tg::ICC ) {
    taskCompiler = new IntelCCompiler ( );
  } else if ( compiler == tg::ICC_DEBUG ) {
    IntelCCompiler* const iccCompiler = new IntelCCompiler();
    iccCompiler->setDebug ( true );
    taskCompiler = iccCompiler;
  #ifdef CELL_SUPPORT
  } else if (compiler == tg::SPU_GCC) {
    taskCompiler = new SpuGnuCCompiler();
  } else if (compiler == tg::SPU_XLC) {
    taskCompiler = new SpuXlcCCompiler();
  #endif
  } else if (compiler == tg::PPU_XLC) {
    taskCompiler = new PpuXlcCCompiler();
  } else {
    throw TaskException ( "'compile' called with unknown compiler." );
  }

  compile ( taskCompiler, keepSourceCode );
  delete taskCompiler;
}

void TaskGraphGroup::compile(TaskCompiler* compiler, bool keepSourceCode) {
  if (compiler->isSpuCompiler())
  {
    for (std::vector<coreTaskGraph*>::const_iterator tgIter = graphs.begin(); tgIter != graphs.end(); ++tgIter)
    {
      static_cast<TaskSpuCompiler*>(compiler)->setTaskGraphFunctionInfo((*tgIter)->returnTypeString, (*tgIter)->getName());
      (*tgIter)->compile(compiler, keepSourceCode);
    }
  }
  else
  {
    code = boost::shared_ptr<TaskCompiledCode>(new NativeCompiledCode());
    std::for_each ( graphs.begin(), graphs.end(), boost::bind(&TaskIRGroup::addToGroup, group, _1));
    ::compile ( compiler, keepSourceCode, code.get(), group);
    std::for_each ( graphs.begin ( ), graphs.end ( ), boost::bind(&TaskIRGroup::removeFromGroup, group, _1) );
    std::for_each ( graphs.begin(), graphs.end(), boost::bind(&coreTaskGraph::setTaskCompiledCode, _1, code));
  }
}

void TaskGraphGroup::print() {
  std::for_each(graphs.begin(), graphs.end(), boost::bind(&TaskIRGroup::addToGroup, group, _1));
  group->print();
  std::for_each(graphs.begin(), graphs.end(), boost::bind(&TaskIRGroup::removeFromGroup, group, _1));
}

coreTaskGraph *coreTaskGraph::currentTaskGraph = NULL;

/** Construct a new taskgraph object
 */
coreTaskGraph::coreTaskGraph ( ) :
  isConstructing ( false ),
  isConstructed ( false ),
  addingParamters ( false ),
  currentParameterIndex(0),
  result (NULL), 
  delayedFinish ( false ),
  functionAddress(errorFunction),
  isCompiled ( false ),
  nextId ( 0 ),
  code (),
  isSpuCode ( false )
{
  // fprintf( stderr, "TaskGraph()\n" );
}

/*  Delete the taskgraph object
 */
coreTaskGraph::~coreTaskGraph ( ) {
  // fprintf( stderr, "~TaskGraph()\n" );
  returnType->unref();
}

/* Copy the object, although we only need to copy what is needed as the TaskGraph
 * should have already been constructed
 */
coreTaskGraph::coreTaskGraph(const coreTaskGraph &taskgraph) :
  isConstructing(false),
  isConstructed (true),
  functionName(taskgraph.functionName),
  objName(taskgraph.objName),
  parameterInfo(taskgraph.parameterInfo),
  parameterBinding(taskgraph.parameterBinding),
  result(NULL),
  statementBodies(taskgraph.statementBodies),
  delayedFinish(taskgraph.delayedFinish),
  functionAddress(taskgraph.functionAddress),
  isCompiled(taskgraph.isCompiled),
  code(taskgraph.code),
  main_body(taskgraph.main_body),
  isSpuCode(taskgraph.isSpuCode)
{
  if ( !taskgraph.isConstructed )
    throw TaskException ( "Copying a taskgraph object which isn't constructed is not allowed." );

  if ( taskgraph.isConstructing )
    throw TaskException ( "Copying a taskgraph object which hasn't finished being constructed is not allowed." );

  TaskIR::deepClone(taskgraph, functionName.c_str());
}

/** Copy from the one object to this one
 */
coreTaskGraph &coreTaskGraph::operator=(const coreTaskGraph &taskgraph)
{
  if ( !taskgraph.isConstructed )
    throw TaskException ( "Copying a taskgraph object which isn't constructed is not allowed." );
  if ( taskgraph.isConstructing )
    throw TaskException ( "Copying a taskgraph object which hasn't finished being constructed is not allowed." );

  isConstructing = false;
  isConstructed = true;
  isCompiled = taskgraph.isCompiled;
  functionName = taskgraph.functionName;
  objName = taskgraph.objName;
  parameterInfo = taskgraph.parameterInfo;
  parameterBinding = taskgraph.parameterBinding;
  result = NULL;
  statementBodies = taskgraph.statementBodies;
  delayedFinish = taskgraph.delayedFinish;
  functionAddress = taskgraph.functionAddress;
  main_body = taskgraph.main_body;
  isSpuCode = taskgraph.isSpuCode;
  
  code = taskgraph.code;
  
  TaskIR::deepClone(taskgraph, functionName.c_str());
  return *this;
}

/** Set the current TaskGraph name
 */
void coreTaskGraph::setName ( const char *pTaskGraphName ) {
  // TODO: Make it work
  if ( isConstructed || isConstructing )
    throw TaskException ( "Name cannot be set once TaskGraph has or is being built." );

  /*	if ( pTaskGraphName != NULL ) {
  // Remove the * if this TaskGraph is made from a pointer
  while (*pTaskGraphName == '*' )
  ++pTaskGraphName;
  functionName = functionNamePrefix + pTaskGraphName;
  } else {
  */		
  
  static int num = 0;
  std::stringstream functionNameStream;
  functionNameStream << functionNamePrefix << num++;
  functionName = functionNameStream.str();
  //	}
}

/** Start the construction of a taskgraph
 */
void coreTaskGraph::begin ( const char *pTaskGraphName ) {
  TaskIR::initialise();
  
  #ifdef TASKGRAPH_TIMING
  theTimingInfo.creationTimer.start ( );
  #endif
  setName ( pTaskGraphName );

  isConstructing = true;

  returnType = getReturnType();
  main_body = TaskIR::init ( functionName.c_str ( ), returnType->getIRType() );

  BaseBlock *baseBlock = new BaseBlock ( this, main_body );
  statementBodies.push_back ( baseBlock );
}

void coreTaskGraph::continueEditing ( ) {
  if ( main_body == NULL ) {
    begin ( 0 );
    return;
  }
  #ifdef TASKGRAPH_TIMING
  theTimingInfo.creationTimer.start ( );
  #endif

  isConstructing = true;

  BaseBlock *baseBlock = new BaseBlock ( this, main_body );
  statementBodies.push_back ( baseBlock );
}

//
// Create an integer type
//
NumericType *coreTaskGraph::createIntegerType ( unsigned _size, bool _isSigned ) {
  NumericType *type = new NumericType;

  type->type = typeInteger;
  type->size = _size;
  type->isSigned = _isSigned;
  type->setIRType ( TaskIR::createIntegerType ( _size, _isSigned ) );
  return type;
}

//
// Create a float type
//
NumericType *coreTaskGraph::createFloatType ( unsigned _size ) {
  NumericType *type = new NumericType;

  type->type = typeFloat;
  type->size = _size;
  type->isSigned = true;
  type->setIRType ( TaskIR::createFloatType ( _size ) );
  return type;
}

PointerType *coreTaskGraph::createPointerType ( Type *baseType ) {
  PointerType *type = new PointerType ( baseType );

  type->setIRType ( TaskIR::createPointerType ( baseType->getIRType() ) );

  return type;
}

StructType *coreTaskGraph::createStructType ( const char *name, int size, int fields ) {
    StructType *type = new StructType ( size );
    type->setIRType ( TaskIR::createStructType ( name, size, fields ) );
    return type;
}

void coreTaskGraph::setStructField ( StructType *type, unsigned pos, const char *name, Type *fieldType, int offset ) {
    TaskIR::setStructField(type->getIRType(),pos, name,fieldType->getIRType(),offset);
}


TaskIR::tgVariable coreTaskGraph::addVariableDefinition ( const char *name, bool parameter, Type *type ) {
  if (parameter)
    recordParameterInfo(name, type);
  return TaskIR::addVariableDefinition ( this, statementBodies.back()->getCurrentList(), name, parameter, type->getIRType() );
}

TaskIR::tgVariable coreTaskGraph::addPointerDefinition ( const char *name, bool parameter, PointerType *type ) {
  // FIXME: We can't do anything for the Cell here because we have no size information.
  // if (parameter)
  //   recordParameterInfo(name, type);
  return TaskIR::addPointerDefinition ( this, statementBodies.back()->getCurrentList(), name, parameter, type->getIRType() );
}

TaskIR::tgVariable coreTaskGraph::addArrayDefinition ( const char *name, bool parameter, ArrayType *type ) {
  if (parameter)
    recordParameterInfo(name, type);
  return TaskIR::addArrayDefinition ( this, statementBodies.back()->getCurrentList(), name, type->getNumDimensions(), type->getDimensions(), parameter, type->getBaseType()->getIRType() );
}

ExpressionPair coreTaskGraph::createSignedIntegerConstantExpression ( long number, unsigned size ) {
  Type *type = createIntegerType ( size, true );
  return ExpressionPair ( TaskIR::createSignedIntegerConstantExpression ( number, size, type->getIRType ( ) ), type );
}

ExpressionPair coreTaskGraph::createUnsignedIntegerConstantExpression ( unsigned long number, unsigned size ) {
  Type *type = createIntegerType ( size, false );
  return ExpressionPair ( TaskIR::createUnsignedIntegerConstantExpression ( number, size, type->getIRType ( ) ), type );
}

ExpressionPair coreTaskGraph::createFloatConstantExpression ( double number, unsigned size ) {
  Type *type = createFloatType ( size );
  return ExpressionPair ( TaskIR::createFloatConstantExpression ( number, size, type->getIRType ( ) ), type );
}

ExpressionPair coreTaskGraph::createStringConstantExpression ( const char *text ) {
  PointerType *type = createPointerType ( createIntegerType ( sizeof ( char ), true ) );
  return ExpressionPair ( TaskIR::createStringConstantExpression ( text, type->getIRType ( ) ), type );
}

ExpressionPair coreTaskGraph::createBinaryMathExpression ( BinaryOperatorType operation, const TaskExpression &lhs, const TaskExpression &rhs ) {
  tgExpression lhsExp = lhs.getExpression ( );
  tgExpression rhsExp = rhs.getExpression ( );
  NumericType *lhsDataInfo = static_cast<NumericType *>( lhs.getType ( ) );
  NumericType *rhsDataInfo = static_cast<NumericType *>( rhs.getType ( ) );
  NumericType *type;

  // Check for appropriate cast
  if ( lhsDataInfo->type != rhsDataInfo->type ) {
    if ( lhsDataInfo->type == typeFloat ) {
	type = lhsDataInfo;
	rhsExp = TaskIR::createCastExpression ( type->getIRType(), rhsExp );
    } else {
	type = rhsDataInfo;
	lhsExp = TaskIR::createCastExpression ( type->getIRType(), lhsExp );
    }
  } else if ( lhsDataInfo->size != rhsDataInfo->size ) {
    if ( lhsDataInfo->size < rhsDataInfo->size ) {
	type = rhsDataInfo;
	lhsExp = TaskIR::createCastExpression ( type->getIRType(), lhsExp );
    } else {
	type = lhsDataInfo;
	rhsExp = TaskIR::createCastExpression ( type->getIRType(), rhsExp );
    }
  } else {
    type = lhsDataInfo;
  }

  return ExpressionPair ( TaskIR::createBinaryMathExpression ( operation, lhsExp, rhsExp, type->getIRType ( ) ), type );
}

ExpressionPair coreTaskGraph::createBinaryComparisonExpression ( BinaryOperatorType 
			operation, const TaskExpression &lhs, const TaskExpression &rhs ) {
  tgExpression lhsExp = lhs.getExpression ( );
  tgExpression rhsExp = rhs.getExpression ( );
  NumericType *lhsDataInfo = static_cast<NumericType *>( lhs.getType ( ) );
  NumericType *rhsDataInfo = static_cast<NumericType *>( rhs.getType ( ) );

  NumericType *type = createIntegerType ( sizeof ( int ), true );

  // Check for appropriate cast
  if ( lhsDataInfo->type != rhsDataInfo->type ) {
    if ( lhsDataInfo->type == typeFloat ) {
	rhsExp = TaskIR::createCastExpression ( lhsDataInfo->getIRType(), rhsExp );
    } else {
	lhsExp = TaskIR::createCastExpression ( rhsDataInfo->getIRType(), lhsExp );
    }
  } else if ( lhsDataInfo->size != rhsDataInfo->size ) {
    if ( lhsDataInfo->size < rhsDataInfo->size ) {
	lhsExp = TaskIR::createCastExpression ( rhsDataInfo->getIRType(), lhsExp );
    } else {
	rhsExp = TaskIR::createCastExpression ( lhsDataInfo->getIRType(), rhsExp );
    }
  }

  return ExpressionPair ( TaskIR::createBinaryComparisonExpression ( operation, lhsExp, rhsExp, type->getIRType() ), type );
}

ExpressionPair coreTaskGraph::createBinaryLogicalBooleanExpression ( BinaryOperatorType operation, const TaskExpression &lhs, const TaskExpression &rhs ) {
  tgExpression lhsExp = lhs.getExpression ( );
  tgExpression rhsExp = rhs.getExpression ( );
  NumericType *lhsDataInfo = static_cast<NumericType *>( lhs.getType ( ) );
  NumericType *rhsDataInfo = static_cast<NumericType *>( rhs.getType ( ) );

  // Check for appropriate cast
  if ( lhsDataInfo->type != typeInteger || rhsDataInfo->type != typeInteger ) {
    throw TaskException ( "Incompatible types for boolean operation." );
  }

  Type *type = createIntegerType ( sizeof ( int ), true );

  return ExpressionPair ( TaskIR::createBinaryLogicalBooleanExpression ( operation, lhsExp, rhsExp, type->getIRType() ), type );
}

ExpressionPair coreTaskGraph::createUnaryExpression ( UnaryOperatorType operation, const TaskExpression &expr ) {
  return ExpressionPair ( TaskIR::createUnaryExpression ( operation, expr.getExpression ( ), expr.getType()->getIRType() ), expr.getType() );
}

ExpressionPair coreTaskGraph::createLoadVariableExpression ( const TaskVariable &variable ) {
  return ExpressionPair ( TaskIR::createLoadVariableExpression ( variable.getSymbol(), variable.getType()->getIRType() ), variable.getType() );
}

ExpressionPair coreTaskGraph::createLoadWithBaseExpression ( const TaskExpression &base, Type *type ) {
  return ExpressionPair ( TaskIR::createLoadWithBaseExpression ( base.getExpression(), type->getIRType() ), type );
}

ExpressionPair coreTaskGraph::createLoadAndDereferenceVariableExpression ( const TaskVariable &variable ) {
  return ExpressionPair ( TaskIR::createLoadAndDereferenceVariableExpression ( variable.getSymbol ( ), variable.getType()->getIRType() ), variable.getType() );
}

ExpressionPair coreTaskGraph::createLoadStructureExpression (  TaskStructure *str, unsigned offset, Type *fieldType ) {
	bool isParameter = str->isParameter();
	if ( str->hasBase() ) {
		return ExpressionPair ( TaskIR::createLoadStructureWithBaseExpression ( str->getBase().getExpression(), str->getType()->getIRType(), offset, fieldType->getIRType(), isParameter ), fieldType );//createPointerType(fieldType) );
	} else {
		return ExpressionPair ( TaskIR::createLoadStructureExpression ( str->getSymbol(), str->getType()->getIRType(), offset, fieldType->getIRType(), isParameter ), fieldType );//createPointerType(fieldType) );
	}
}

void coreTaskGraph::createStoreStructureStatement ( const TaskStructure &str, unsigned offset, Type *fieldType, const TaskExpression &expr, bool isParameter ) {
	if ( str.hasBase() ) {
		addStatement ( TaskIR::createStoreStructureWithBaseStatement ( str.getBase().getExpression(), str.getType()->getIRType(), offset, fieldType->getIRType(), expr.getExpression(), isParameter ) );
	} else {
		addStatement ( TaskIR::createStoreStructureStatement ( str.getSymbol(), str.getType()->getIRType(), offset, fieldType->getIRType(), expr.getExpression(), isParameter ) );
	}
}

void coreTaskGraph::addStoreVariableStatement ( const TaskVariable &variable, const TaskExpression &expression ) {
  TaskIR::tgExpression exp = expression.getExpression ( );
  
  recordVariableModification(variable);
  // If types differ do a cast
  // TODO: Clean
  if ( ! expression.getType()->equals(*variable.getType()) ) {
    exp = TaskIR::createCastExpression ( variable.getType()->getIRType(), exp );
  }

  addStatement ( TaskIR::addStoreVariableStatement ( variable.getSymbol(), exp ) );
}

void coreTaskGraph::addStoreWithBaseStatement ( const TaskExpressionSymbol &base, const TaskExpression &expression, Type *type ) {
  recordVariableModification(base.getVariable());

  addStatement ( TaskIR::addStoreWithBaseStatement  ( base.getExpression(), expression.getExpression ( ), type->getIRType() ) );
}

void coreTaskGraph::addDereferenceAndStoreVariableExpression( const TaskVariable &variable, const TaskExpression &expression ) {
  TaskIR::tgExpression exp = expression.getExpression ( );

  recordVariableModification(variable);
  // If sizes differ do a cast
  // TODO: Clean
  if ( ! expression.getType()->equals(*variable.getType()) ) {
    exp = TaskIR::createCastExpression ( variable.getType()->getIRType(), exp );
  }

  addStatement ( TaskIR::addDereferenceAndStoreVariableExpression ( variable.getSymbol(), exp, variable.getType()->getIRType() ) );
}

void coreTaskGraph::installStructType( StructType *type ) {
	TaskIR::installStructType(type->getIRType());
}

ExpressionPair coreTaskGraph::createArrayAccessExpression ( TaskIR::tgVariable variable, Type *type, unsigned dimensions, const unsigned *sizes, TaskIR::tgExpression *indices, bool parameter ) {
  return ExpressionPair ( TaskIR::createArrayAccessExpression ( statementBodies.back()->getCurrentList(), variable, dimensions, sizes, indices, type->getIRType(), parameter ), createPointerType(type) );
}

ExpressionPair coreTaskGraph::createArrayAccessWithBaseExpression ( const TaskExpression &base, Type *type, unsigned dimensions, const unsigned *sizes, TaskIR::tgExpression *indices, bool parameter ) {
	return ExpressionPair ( TaskIR::createArrayAccessWithBaseExpression ( statementBodies.back()->getCurrentList(), base.getExpression(), dimensions, sizes, indices, type->getIRType() ), createPointerType(type) );
}

ExpressionPair coreTaskGraph::createPointerAccessExpression ( tgVariable variable, const TaskExpression &index, bool parameter, PointerType *type ) {
  return ExpressionPair ( TaskIR::createPointerAccessExpression ( statementBodies.back()->getCurrentList(), variable, index.getExpression(), type->getIRType(), parameter ), type );
}

ExpressionPair coreTaskGraph::createPointerAccessWithBaseExpression ( const TaskExpression &base, const TaskExpression &index, PointerType *type ) {
  return ExpressionPair ( TaskIR::createPointerAccessWithBaseExpression ( statementBodies.back()->getCurrentList(), base.getExpression(), index.getExpression(), type->getIRType() ), type );
}

void coreTaskGraph::addArrayStoreStatement ( const TaskVariable &variable, const TaskExpression &expression, unsigned dimensions, const unsigned *sizes, TaskIR::tgExpression *indices, bool parameter ) {
  TaskIR::tgExpression exp = expression.getExpression ( );

  recordVariableModification(variable);
  // If types differ do a cast
  // TODO: Clean
  if (!expression.getType()->equals(*variable.getType()))
    exp = TaskIR::createCastExpression ( variable.getType()->getIRType(), exp );

  addStatement ( TaskIR::addArrayStoreStatement ( statementBodies.back()->getCurrentList(), variable.getSymbol(), exp, dimensions, sizes, indices, variable.getType()->getIRType(), parameter ) );
}

void coreTaskGraph::addArrayStoreStatement ( const TaskExpression &baseExpression, const TaskExpression &expression, unsigned dimensions, const unsigned *sizes, TaskIR::tgExpression *indices, ArrayType *type) {
  TaskIR::tgExpression exp = expression.getExpression ( );

  //TODO: Check cast
  // If types differ do a cast
  //if ( (Type *)expression.getType ( ) != (Type *)variable.getType ( ) ) {
  //  exp = TaskIR::createCastExpression ( variable.getType().getIRType(), exp );
  //}

  addStatement ( TaskIR::addArrayStoreStatement ( statementBodies.back()->getCurrentList(), baseExpression.getExpression(), exp, dimensions, sizes, indices, type->getBaseType()->getIRType() ) );
 }

void coreTaskGraph::callFunction ( TaskIR::tgFunction func, unsigned numberArgs, tgExpression *args ) {
  addStatement ( TaskIR::callFunctionStatement ( func, numberArgs, args ) );
}

TaskExpression coreTaskGraph::callFunction ( TaskIR::tgFunction func, unsigned numberArgs, tgExpression *args, Type *returnType ) {
  return TaskExpression ( TaskIR::callFunction ( func, numberArgs, args ), returnType );
}

ExpressionPair coreTaskGraph::createCastExpression ( const TaskExpression &expr, Type *type ) {
  return ExpressionPair ( TaskIR::createCastExpression ( type->getIRType(), expr.getExpression() ), type );
}

void coreTaskGraph::addBreakStatement ( ) {
  // Find first 'for' or 'while' loop in block stack
  std::vector<BlockStructure *>::reverse_iterator end = statementBodies.rend ( );
  std::vector<BlockStructure *>::reverse_iterator iter = statementBodies.rbegin ( );

  // Avoid the following: tFor (...) { } tBreak;
  if ( delayedFinish )
    ++iter;

  while ( iter != end ) {
    if ( (*iter)->getType() == BST_For ) {
	//TaskIR::tgCodeLabel label = static_cast<ForBlock *>(*iter)->getBreakLabel( );
	//addStatement ( TaskIR::createJumpStatement ( label ) );
	addStatement ( static_cast<ForBlock *>(*iter)->createBreakStatement() );
	break;
    }
    if ( (*iter)->getType() == BST_While ) {
	//TaskIR::tgCodeLabel label = static_cast<WhileBlock *>(*iter)->getBreakLabel( );
	//addStatement ( TaskIR::createJumpStatement ( label ) );
	addStatement ( static_cast<WhileBlock *>(*iter)->createBreakStatement() );
	break;
    }
    ++iter;
  }

  if ( iter == end )
    throw TaskException ( "tBreak used when not in a tWhile or tFor loop." );
}

void coreTaskGraph::addContinueStatement ( ) {
  // Find first 'for' or 'while' loop in block stack
  std::vector<BlockStructure *>::reverse_iterator end = statementBodies.rend ( );
  std::vector<BlockStructure *>::reverse_iterator iter = statementBodies.rbegin ( );

  // Avoid the following: tIf (...) { } tBreak;
  if ( delayedFinish )
    ++iter;

  while ( iter != end ) {
    if ( (*iter)->getType() == BST_For ) {
	//TaskIR::tgCodeLabel label = static_cast<ForBlock *>(*iter)->getContinueLabel( );
	//addStatement ( TaskIR::createJumpStatement ( label ) );
	addStatement ( static_cast<ForBlock *>(*iter)->createContinueStatement() );
	break;
    }
    if ( (*iter)->getType() == BST_While ) {
	//TaskIR::tgCodeLabel label = static_cast<WhileBlock *>(*iter)->getContinueLabel( );
	//addStatement ( TaskIR::createJumpStatement ( label ) );
	addStatement ( static_cast<WhileBlock *>(*iter)->createContinueStatement() );
	break;
    }
    ++iter;
  }
  if ( iter == end ) {
    throw TaskException ( "tContinue used when not in a tWhile or tFor loop." );
  }
}

void coreTaskGraph::addReturnStatement ( const TaskExpression &expr ) {
/*	std::vector<BlockStructure *>::reverse_iterator end = statementBodies.rend ( );
	std::vector<BlockStructure *>::reverse_iterator iter = statementBodies.rbegin ( );

	while ( iter != end ) {
		if ( (*iter)->getType() == BST_Base ) break;
		++iter;
	}
	static_cast<BaseBlock *>(*iter)->getCurrentList()->list->parent()->proc_syms()->symbols();
*/

  // TODO: Only cast if types differ
  addStatement ( TaskIR::createReturnStatement ( TaskIR::createCastExpression(returnType->getIRType(), expr.getExpression ( )), returnType->getIRType() ) );
}

unsigned coreTaskGraph::addParameter(const char *name) {
  parameterInfo[name].index = currentParameterIndex;
  return currentParameterIndex++;
}


void coreTaskGraph::recordParameterInfo(const std::string& name, const Type* const type)
{
  parameterInfo[name].size = type->getSize();
}

void coreTaskGraph::recordVariableModification(const TaskVariable& var)
{
  if (var.isParameter())
    parameterInfo[var.getName()].isRef = true;
}

void coreTaskGraph::addStatement ( tgStatement statement ) {
  if ( delayedFinish ) {
    finishTop ( );
  }
  statementBodies.back()->addStatement ( statement );
}

void coreTaskGraph::setIdentifier ( TaskLoopIdentifier &id ) {
  if ( nextId != 0 )
    throw TaskException ( "Setting identifier before previous one has been set." );
  nextId = &id;
}

void coreTaskGraph::finishTop ( ) {
  delayedFinish = false;
  //	do {
  BlockStructure *block = statementBodies.back();
  statementBodies.pop_back();
  //		printf ( "Finish:%d\n", block->getType() );
  block->finish();
  delete block;

  //		if ( statementBodies.size ( ) == 0 )
  //			throw TaskException ( "Somehow the base block was revived" );
  //	} while ( statementBodies.top ( )->getRevived ( ) );
}

void coreTaskGraph::finishBlock ( ) {
  if ( delayedFinish )
    finishTop ( );

  //while ( true ) {
  if ( !statementBodies.back()->delayFinish ( ) ) {
    BlockStructure *block = statementBodies.back();
    statementBodies.pop_back();
    //			printf ( "Finish:%d\n", block->getType() );
    block->finish();
    delete block;
  } else {
    //			printf ( "Delaying Finish:%d\n", statementBodies.top()->getType() );
    delayedFinish = true;
    //			break;
  }
  //if ( statementBodies.size ( ) == 0 )
  //break;
  //if ( !statementBodies.top ( )->getRevived ( ) )
  //break;
  //printf("Looping\n");
  //}
}

/** Finish the construction of the taskgraph
 */
void coreTaskGraph::finish ( ) {
  if ( statementBodies.size ( ) != 0 ) {
    throw TaskException ( "Finishing without all blocks finished." );
  }

  isConstructing = false;
  isConstructed = true;

  // Set the parameter binding size
  parameterBinding.resize ( parameterInfo.size ( ) );
  #ifdef TASKGRAPH_TIMING
  theTimingInfo.creationTimer.stop ( );
  #endif
}

BlockStructure *coreTaskGraph::reviveLastBlockStructure ( ) {
  if ( !delayedFinish ) {
    throw TaskException ( "Trying to revive a block that doesn't exist." );
  }
  //	printf ( "revived\n" );
  statementBodies.back()->setRevived();
  delayedFinish = false;
  return statementBodies.back();
}

void coreTaskGraph::addBlockStructure ( BlockStructure *block ) {
  if ( delayedFinish ) {
    finishTop ( );
  }
  statementBodies.push_back ( block );
}

coreTaskGraph *coreTaskGraph::startIfBlock ( const TaskExpression &expr ) {
  IfBlock *ifBlock = new IfBlock ( this, expr.getExpression ( ) );
  addBlockStructure ( ifBlock );
  return this;
}

coreTaskGraph *coreTaskGraph::startWhileBlock ( const TaskExpression &expr ) {
  WhileBlock *whileBlock = new WhileBlock ( this, expr.getExpression ( ) );
  addBlockStructure ( whileBlock );
  return this;
}

coreTaskGraph *coreTaskGraph::startForBlock ( const TaskVariable &variable, const TaskExpression &from, const TaskExpression &to, const TaskExpression &step, BinaryOperatorType comparison ) {
  // TODO: Add non-parameter check here for loop variable
  ForBlock *forBlock = new ForBlock ( this, variable.getSymbol ( ), static_cast<NumericType *>(variable.getType())->isSigned, from.getExpression ( ), to.getExpression ( ), step.getExpression ( ), comparison, nextId );
  addBlockStructure ( forBlock );
  nextId = 0;
  return this;
}

coreTaskGraph *coreTaskGraph::startScopeBlock ( ) {
  ScopeBlock *scopeBlock = new ScopeBlock ( this );
  addBlockStructure ( scopeBlock );
  return this;
}

const char *coreTaskGraph::getName ( ) {
  return functionName.c_str ( );
}

void coreTaskGraph::outputToC ( const char *fileName ) {
  TaskIR::outputToC ( fileName );
}

/** Bind the parameter name to binding
*/
void coreTaskGraph::setParameter ( const char *name, void *binding ) {
  const std::map<std::string, ParameterInfo>::const_iterator iter = parameterInfo.find(std::string(name));
  if (iter != parameterInfo.end())
    parameterBinding[iter->second.index] = binding;
  else
    throw TaskException ( std::string ( "Parameter '" )  + name + "' not found, perhaps NULL missing on the end of setParameters." );
}

void** coreTaskGraph::getParameterBinding ( const char *name ) {
  const std::map<std::string, ParameterInfo>::const_iterator iter = parameterInfo.find(std::string(name));
  if (iter != parameterInfo.end())
  {
    return &parameterBinding[iter->second.index];
  }
  else
  {
    throw TaskException ( std::string ( "Parameter '" )  + name + "' not found, perhaps NULL missing on the end of setParameters." );
  }
}

/** Set the parameters in the list
*/
void coreTaskGraph::setParameters ( const char *first, ... ) {
  va_list val;
  va_start ( val, first );
  setParameters ( first, val );
  va_end ( val );
}

void coreTaskGraph::setParameters ( const char *first, va_list val ) {
  const char *name = first;
  while ( name ) {
    void *var = va_arg ( val, void * );
    setParameter ( name, var );
    name = va_arg ( val, const char * );
  }
}

void coreTaskGraph::execute ( ) {
  #ifdef TASK_GRAPH_EXECUTION_TIMER
  theTimingInfo.executionTimer.start ( );
  functionAddress ( &(parameterBinding[0]) );
  theTimingInfo.executionTimer.stop ( );
  #else
  functionAddress ( &(parameterBinding[0]) );
  #endif
}

/** Set the parameters and then execute
*/
void coreTaskGraph::execute ( const char *first, ... ) {
  va_list val;
  va_start ( val, first );
  setParameters ( first, val );
  va_end ( val );

  execute ( );
}

//
// The old style of choosing the compiler
//
void coreTaskGraph::compile ( const int compiler, const bool keepSourceCode ) {
  TaskCompiler* taskCompiler;
  if ( compiler == tg::GCC ) {
    taskCompiler = new GnuCCompiler();
  } else if ( compiler == tg::GCC_DEBUG ) {
    GnuCCompiler* const gccCompiler = new GnuCCompiler();
    gccCompiler->setDebug ( true );
    taskCompiler = gccCompiler;
  } else if ( compiler == tg::ICC ) {
    taskCompiler = new IntelCCompiler ( );
  } else if ( compiler == tg::ICC_DEBUG ) {
    IntelCCompiler* const iccCompiler = new IntelCCompiler();
    iccCompiler->setDebug ( true );
    taskCompiler = iccCompiler;
  #ifdef CELL_SUPPORT
  } else if (compiler == tg::SPU_GCC) {
    taskCompiler = new SpuGnuCCompiler();
  } else if (compiler == tg::SPU_XLC) {
    taskCompiler = new SpuXlcCCompiler();
  #endif
  } else if (compiler == tg::PPU_XLC) {
    taskCompiler = new PpuXlcCCompiler();
  } else {
    throw TaskException ( "'compile' called with unknown compiler." );
  }

  compile (taskCompiler, keepSourceCode);
  delete taskCompiler;
}

void coreTaskGraph::compile(TaskCompiler *compiler, bool keepSourceCode) {
  boost::shared_ptr<TaskCompiledCode> _code;

  if (compiler->isSpuCompiler())
  {
    #ifdef CELL_SUPPORT
    isSpuCode = true;
    _code = boost::shared_ptr<TaskCompiledCode>(new SpuTaskCompiledCode());
    static_cast<TaskSpuCompiler*>(compiler)->setTaskGraphFunctionInfo(returnTypeString, functionName);
    #else
    assert(false && "SPU code flag was set on a TaskGraph version compiled without Cell support. Please file a bug report.");
    #endif
  }
  else
  {
    _code = boost::shared_ptr<TaskCompiledCode>(new NativeCompiledCode());
  }

  ::compile(compiler, keepSourceCode, _code.get(), this);
  setTaskCompiledCode(_code);

  // This TaskGraph is now compiled!
  isCompiled = true;
}

void coreTaskGraph::setTaskCompiledCode (boost::shared_ptr<TaskCompiledCode>& _code) {
  code = _code;
  functionAddress = code->getFunctionAddress ( getName ( ) );
}


void coreTaskGraph::setResultDest(void *dst) {
    result = dst;
}

BaseBlock::BaseBlock ( coreTaskGraph *_taskgraph, TaskIR::tgStatementList _list )
  : BlockStructure(_taskgraph), list ( _list ) {
}

void BaseBlock::addStatement ( TaskIR::tgStatement statement ) {
  taskgraph->appendStatement ( list, statement );
}

void BaseBlock::finish ( ) {
}

TaskIR::tgStatementList BaseBlock::getCurrentList ( ) {
  return list;
}


//
// Handles the construction of if
//

IfBlock::IfBlock ( coreTaskGraph *_taskgraph, TaskIR::tgExpression expression )
  : BlockStructure(_taskgraph), ifpart ( expression ), whichPart ( 0 ) {
  thenpart = taskgraph->createStatementList ( taskgraph->getCurrentBlockStructure()->getCurrentList() );
  elsepart = taskgraph->createStatementList ( taskgraph->getCurrentBlockStructure()->getCurrentList() );
}

void IfBlock::addStatement ( TaskIR::tgStatement statement ) {
  if ( whichPart == 0 )
    taskgraph->appendStatement ( thenpart, statement );
  else
    taskgraph->appendStatement ( elsepart, statement );
}

void IfBlock::switchToElsePart ( ) {
  whichPart = 1;
  //	elsepart = taskgraph->createStatementList ( taskgraph->getCurrentBlockStructure()->getCurrentList() );
}

void IfBlock::finish ( ) {
  taskgraph->addStatement ( taskgraph->createIfStatement ( taskgraph->getCurrentBlockStructure()->getCurrentList(), ifpart, thenpart, elsepart ) );
}

TaskIR::tgStatementList IfBlock::getCurrentList ( ) {
  if ( whichPart == 0 )
    return thenpart;
  else
    return elsepart;
}

bool IfBlock::delayFinish ( ) {
  if ( whichPart == 0 )
    return true;
  else
    return false;
}

//
// Handles the construction of while
//

WhileBlock::WhileBlock ( coreTaskGraph *_taskgraph, TaskIR::tgExpression expression )
  : BlockStructure(_taskgraph), test(expression), loop(0) {
  list = taskgraph->createStatementList ( taskgraph->getCurrentBlockStructure()->getCurrentList() );
  loop = taskgraph->createWhileStatement ( taskgraph->getCurrentBlockStructure()->getCurrentList(), test, list );
}

void WhileBlock::addStatement ( TaskIR::tgStatement statement ) {
  taskgraph->appendStatement ( list, statement );
}

void WhileBlock::finish ( ) {
  //taskgraph->addStatement ( taskgraph->createWhileStatement ( taskgraph->getCurrentBlockStructure()->getCurrentList(), test, list, breakLabel, continueLabel ) );
  taskgraph->addStatement ( taskgraph->finishWhileStatement ( loop ) );
}

TaskIR::tgStatementList WhileBlock::getCurrentList ( ) {
  return list;
}

//  TaskIR::tgCodeLabel WhileBlock::getBreakLabel( ) {
//    return breakLabel;
//  }

//  TaskIR::tgCodeLabel WhileBlock::getContinueLabel( ) {
//    return continueLabel;
//  }

TaskIR::tgStatement WhileBlock::createBreakStatement( ) {
  return taskgraph->createBreakStatement( loop );
}

TaskIR::tgStatement WhileBlock::createContinueStatement( ) {
  return taskgraph->createContinueStatement( loop );
}

//
// Handles the construction of for loops
//
ForBlock::ForBlock ( coreTaskGraph *_taskgraph, TaskIR::tgVariable _symbol, bool _varSigned, TaskIR::tgExpression _from, TaskIR::tgExpression _to, TaskIR::tgExpression _step, BinaryOperatorType _comparison, TaskLoopIdentifier *_nextId )
  : BlockStructure(_taskgraph), symbol(_symbol), from(_from), to(_to), step(_step), loop(0), comparison(_comparison), nextId ( _nextId ), varSigned (_varSigned)
{
  list = taskgraph->createStatementList ( taskgraph->getCurrentBlockStructure()->getCurrentList() );
  loop = taskgraph->createForStatement ( taskgraph->getCurrentBlockStructure()->getCurrentList(), symbol, varSigned, from, to, step, list, comparison);
  //breakLabel = taskgraph->createCodeLabel ( taskgraph->getCurrentBlockStructure()->getCurrentList() );
  //continueLabel = taskgraph->createCodeLabel ( taskgraph->getCurrentBlockStructure()->getCurrentList() );
}

void ForBlock::addStatement ( TaskIR::tgStatement statement ) {
  taskgraph->appendStatement ( list, statement );
}

void ForBlock::finish (  ) {
  //TaskIR::tgStatement statement = taskgraph->createForStatement ( taskgraph->getCurrentBlockStructure()->getCurrentList(), symbol, varSigned, from, to, step, list, breakLabel, continueLabel, comparison );
  TaskIR::tgStatement statement = taskgraph->finishForStatement ( loop );
  taskgraph->addStatement ( statement );
  if ( nextId != 0 ) {
    nextId->setStatement ( statement );
  }
}

TaskIR::tgStatementList ForBlock::getCurrentList ( ) {
  return list;
}

//  TaskIR::tgCodeLabel ForBlock::getBreakLabel( ) {
//    return breakLabel;
//  }

//  TaskIR::tgCodeLabel ForBlock::getContinueLabel( ) {
//    return continueLabel;
//  }

TaskIR::tgStatement ForBlock::createBreakStatement( ) {
  return taskgraph->createBreakStatement( loop );
}

TaskIR::tgStatement ForBlock::createContinueStatement( ) {
  return taskgraph->createContinueStatement( loop );
}

ScopeBlock::ScopeBlock ( coreTaskGraph *_taskgraph )
  : BlockStructure(_taskgraph) {
  list = taskgraph->createStatementList ( taskgraph->getCurrentBlockStructure()->getCurrentList() );
}

void ScopeBlock::addStatement ( TaskIR::tgStatement statement ) {
  taskgraph->appendStatement ( list, statement );
}

void ScopeBlock::finish ( ) {
  taskgraph->addStatement ( taskgraph->createScopeStatement ( taskgraph->getCurrentBlockStructure()->getCurrentList(), list ) );
}

TaskIR::tgStatementList ScopeBlock::getCurrentList ( ) {
  return list;
}
  
}
