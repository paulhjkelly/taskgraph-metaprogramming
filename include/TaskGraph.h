#ifndef TASKGRAPH_H__
#define TASKGRAPH_H__

#include "TaskDefines.h"
#include "TaskIR.h"
#include <string>
#include <iostream>
#include <vector>
#include <iosfwd>
#include <cstdarg>
#include "TaskTimer.h"
#include "TaskTypes.h"
#include <boost/shared_ptr.hpp>

#define CURRENT_TASKGRAPH tg::coreTaskGraph::getCurrentTaskGraph ( )

namespace tg {

class coreTaskGraph;
class TaskCompiler;

#ifdef TASKGRAPH_TIMING
struct TimingInfo {
	void print ( std::ostream &stream );
	void reset ( );
	tg::AccumTimer compileTimer, creationTimer;
	#ifdef TASKGRAPH_EXECUTION_TIMER
	tg::AccumTimer executionTimer;
	#endif
};
extern TimingInfo theTimingInfo;
#endif

class TaskExpression;
class TaskVariable;
class TaskExpression;
class TaskExpressionSymbol;
class Type;
class ArrayType;
class NumericType;
class PointerType;
class StructType;
class ExpressionPair;
class TaskStructure;

enum Compilers {
	GCC,
	GCC_DEBUG,
	ICC,
	ICC_DEBUG,
	SPU_GCC,
	SPU_GCC_DEBUG,
	SPU_XLC,
	SPU_XLC_DEBUG,
  	PPU_XLC,
  	PPU_XLC_DEBUG
};

enum BlockStructureType {
	BST_Base = 0,
	BST_If = 1,
	BST_While = 2,
	BST_For = 3,
	BST_Scope = 4
};

class TaskLoopIdentifier {
public:
	TaskLoopIdentifier ( ) : statement(0) {
	}
	void setStatement ( TaskIR::tgStatement _statement ) {
		statement = _statement;
	}
	TaskIR::tgStatement getStatement ( ) const {
		return statement;
	}
private:
	TaskIR::tgStatement statement;
};

struct BlockStructure {
	BlockStructure ( coreTaskGraph *_taskgraph ) : taskgraph(_taskgraph), revived(false) {
	}
	virtual ~BlockStructure () { };
	virtual void addStatement ( TaskIR::tgStatement statement ) = 0;
	virtual BlockStructureType getType ( ) = 0;
	virtual void finish ( ) = 0;
	virtual TaskIR::tgStatementList getCurrentList ( ) = 0;
	virtual bool delayFinish ( ) {
		return false;
	}
	void setRevived ( ) {
		revived = true;
	}
	bool getRevived ( ) const {
		return revived;
	}
protected:
	coreTaskGraph *taskgraph;
	bool revived;
};

struct BaseBlock : public BlockStructure {
	BaseBlock ( coreTaskGraph *taskgraph, TaskIR::tgStatementList _list );
	virtual BlockStructureType getType ( ) {
		return BST_Base;
	}
	void addStatement ( TaskIR::tgStatement statement );
	void finish ( );
	TaskIR::tgStatementList getCurrentList ( );
private:
	TaskIR::tgStatementList list;
};

struct IfBlock : public BlockStructure {
	IfBlock ( coreTaskGraph *taskgraph, TaskIR::tgExpression expression );
	virtual BlockStructureType getType ( ) {
		return BST_If;
	}
	void addStatement ( TaskIR::tgStatement statement );
	void switchToElsePart ( );
	void finish ( );
	TaskIR::tgStatementList getCurrentList ( );
	bool delayFinish ( );
private:
	TaskIR::tgExpression ifpart;
	TaskIR::tgStatementList thenpart, elsepart;
	int whichPart;
};

struct WhileBlock : public BlockStructure {
	WhileBlock ( coreTaskGraph *taskgraph, TaskIR::tgExpression expression );
	virtual BlockStructureType getType ( ) {
		return BST_While;
	}
	void addStatement ( TaskIR::tgStatement statement );
	void finish ( );
	TaskIR::tgStatementList getCurrentList ( );
	TaskIR::tgStatement createBreakStatement( );
	TaskIR::tgStatement createContinueStatement( );
private:
	TaskIR::tgExpression test;
	TaskIR::tgStatementList list;
  TaskIR::tgLoopStatement loop;
  // TaskIR::tgCodeLabel breakLabel, continueLabel;
};

struct ForBlock : public BlockStructure {
	ForBlock ( coreTaskGraph *taskgraph, TaskIR::tgVariable _symbol, bool _varSigned, TaskIR::tgExpression _from, TaskIR::tgExpression _to, TaskIR::tgExpression _step, BinaryOperatorType _comparison, TaskLoopIdentifier *_nextId );
	virtual BlockStructureType getType ( ) {
		return BST_For;
	}
	void addStatement ( TaskIR::tgStatement statement );
	void finish ( );
	TaskIR::tgStatementList getCurrentList ( );
	TaskIR::tgStatement createBreakStatement( );
	TaskIR::tgStatement createContinueStatement( );
private:
	TaskIR::tgVariable symbol;
	TaskIR::tgExpression from, to, step;
	TaskIR::tgStatementList list;
  TaskIR::tgLoopStatement loop;
  // TaskIR::tgCodeLabel breakLabel, continueLabel;
	BinaryOperatorType comparison;
	TaskLoopIdentifier *nextId;
	bool varSigned;
};

struct ScopeBlock : public BlockStructure {
	ScopeBlock ( coreTaskGraph *taskgraph );
	virtual BlockStructureType getType ( ) {
		return BST_Scope;
	}
	void addStatement ( TaskIR::tgStatement statement );
	void finish ( );
	TaskIR::tgStatementList getCurrentList ( );
private:
	TaskIR::tgStatementList list;
};


struct ParameterInfo {
        // We assume read-only until we detect otherwise
	ParameterInfo() : isRef(false)
	{
	}

	unsigned int index;
	unsigned int size;
	bool isRef;
};


//typedef void *(*taskGraphFunction) ( void ** );
typedef void (*taskGraphFunction)( void ** );

class TaskCompiledCode {
public:
	virtual void load ( const char *fileName ) = 0;
	virtual taskGraphFunction getFunctionAddress ( const char *name ) = 0;
	virtual ~TaskCompiledCode ( ) {}
};

class NativeCompiledCode : public TaskCompiledCode {
private:
  void* sharedLib;
  std::string objName;

public:
  NativeCompiledCode() : sharedLib(NULL)
  {
  }

  virtual void load( const char *fileName );
  virtual taskGraphFunction getFunctionAddress ( const char *name );
  ~NativeCompiledCode();
};

class coreTaskGraph
	: public TaskIR {
public:
	coreTaskGraph ( );
	~coreTaskGraph ( );

	// Copy the TaskGraph object
	// NOTE: this can be costly
	coreTaskGraph ( const coreTaskGraph &taskgraph );
	coreTaskGraph &operator=( const coreTaskGraph &taskgraph );

	void setResultDest(void *dst);

	void begin ( const char *pTaskGraphName );
	void continueEditing ( );

	void compile ( int compiler = tg::GCC, bool keepSourceCode = false );
	void compile ( TaskCompiler *compiler, bool keepSourceCode = false );

	NumericType *createFloatType ( unsigned _size );
	NumericType *createIntegerType ( unsigned _size, bool _isSigned );
	PointerType *createPointerType ( Type *baseType );
	void installStructType( StructType *type);

	TaskIR::tgVariable addVariableDefinition ( const char *name, bool parameter, Type *type );
	TaskIR::tgVariable addPointerDefinition ( const char *name, bool parameter, PointerType *type );
	TaskIR::tgVariable addArrayDefinition ( const char *name, bool parameter, ArrayType *type );

	ExpressionPair createArrayAccessExpression ( TaskIR::tgVariable variable, Type *type, unsigned dimensions, const unsigned *sizes, TaskIR::tgExpression *indices, bool parameter );
	ExpressionPair createArrayAccessWithBaseExpression ( const TaskExpression &base, Type *type, unsigned dimensions, const unsigned *sizes, TaskIR::tgExpression *indices, bool parameter );
	ExpressionPair createPointerAccessExpression ( tgVariable variable, const TaskExpression &index, bool parameter, PointerType *type );
	ExpressionPair createPointerAccessWithBaseExpression ( const TaskExpression &base, const TaskExpression &index, PointerType *type );

	void addArrayStoreStatement ( const TaskVariable &variable, const TaskExpression &rhs, unsigned dimensions, const unsigned *sizes, TaskIR::tgExpression *indices, bool parameter );
	void addArrayStoreStatement ( const TaskExpression &baseExpression, const TaskExpression &expression, unsigned dimensions, const unsigned *sizes, TaskIR::tgExpression *indices,  ArrayType *type );

	void addStoreVariableStatement ( const TaskVariable &variable, const TaskExpression &expression );
	void addStoreWithBaseStatement ( const TaskExpressionSymbol &base, const TaskExpression &expression, Type *type );
	void addDereferenceAndStoreVariableExpression ( const TaskVariable &variable, const TaskExpression &expression );
	void createStoreStructureStatement ( const TaskStructure &str, unsigned offset, Type *fieldType, const TaskExpression &expr, bool isParameter );

	void setIdentifier ( TaskLoopIdentifier &id );

	void callFunction ( TaskIR::tgFunction func, unsigned numberArgs, tgExpression *args );
	TaskExpression callFunction ( TaskIR::tgFunction func, unsigned numberArgs, tgExpression *args, Type *returnType );

	ExpressionPair createLoadStructureExpression ( TaskStructure *str, unsigned offset, Type *fieldType );

	ExpressionPair createBinaryMathExpression ( BinaryOperatorType operation, const TaskExpression &lhs, const TaskExpression &rhs );
	ExpressionPair createBinaryComparisonExpression ( BinaryOperatorType operation, const TaskExpression &lhs, const TaskExpression &rhs );
	ExpressionPair createBinaryLogicalBooleanExpression ( BinaryOperatorType operation, const TaskExpression &lhs, const TaskExpression &rhs );

	ExpressionPair createUnaryExpression ( UnaryOperatorType operation, const TaskExpression &expr );

	ExpressionPair createLoadVariableExpression ( const TaskVariable &variable );
	ExpressionPair createLoadWithBaseExpression ( const TaskExpression &base, Type *type );
	ExpressionPair createLoadAndDereferenceVariableExpression ( const TaskVariable &variable );

	ExpressionPair createSignedIntegerConstantExpression ( long number, unsigned size );
	ExpressionPair createUnsignedIntegerConstantExpression ( unsigned long number, unsigned size );
	ExpressionPair createFloatConstantExpression ( double number, unsigned size );
	ExpressionPair createStringConstantExpression ( const char *text );
	ExpressionPair createCastExpression ( const TaskExpression &expr, Type *type );

    StructType *createStructType ( const char *name, int size, int fields );
    void setStructField ( StructType *type, unsigned pos, const char *name, Type *fieldType, int offset );

	void addBreakStatement ( );
	void addContinueStatement ( );

	void addReturnStatement ( const TaskExpression &expr );

	void addBlockStructure ( BlockStructure *block );

	unsigned addParameter ( const char *name );

	BlockStructure *getCurrentBlockStructure ( ) const {
		return statementBodies.back ( );
	}

	BlockStructure *reviveLastBlockStructure ( );

	void finish ( );
	void finishBlock ( );
	void addStatement ( tgStatement statement );

	// TODO: Stop these returning TaskGraph as it is only for the dodgy macro stuff
	// and not part of the interface ( can be replaced by Function in TaskConstruct.h)
	coreTaskGraph *startIfBlock ( const TaskExpression &expr );
	coreTaskGraph *startWhileBlock ( const TaskExpression &expr );
	coreTaskGraph *startForBlock ( const TaskVariable &variable, const TaskExpression &from, const TaskExpression &to, const TaskExpression &step, BinaryOperatorType comparison );
	coreTaskGraph *startScopeBlock ( );

	void setAddingParameters ( bool adding ) {
		addingParamters = adding;
	}

	bool getAddingParameters ( ) const {
		return addingParamters;
	}

	static coreTaskGraph *getCurrentTaskGraph ( ) {
		return currentTaskGraph;
	}

	static void setCurrentTaskGraph ( coreTaskGraph *graph ) {
		currentTaskGraph = graph;
	}

	
	void execute ( );

	void execute ( const char *first, ... );

	void setParameters ( const char *first, va_list val );

	void setParameters ( const char *first, ... );

	void** getParameterBinding ( const char *name );

	void setParameter ( const char *name, void *binding );

	void setTaskCompiledCode ( boost::shared_ptr<TaskCompiledCode>& _code );

	void outputToC ( const char *fileName );

	void finishTop ( );

	bool compiled ( ) const {
		return isCompiled;
	}

	void printTimingInfo ( std::ostream &stream );
	virtual int getReturnSize() = 0;
	const char *getName ( );
protected:
	virtual Type *getReturnType ( ) = 0;
	void setName ( const char *pTaskGraphName );
	void checkCompile ( );

	static coreTaskGraph *currentTaskGraph;
	bool isConstructing, isConstructed;

	std::string functionName, objName;
	bool addingParamters;

	// The first array holds the name of the parameter
	// the second contains the binding
	unsigned int currentParameterIndex;
public:
 	void recordParameterInfo(const std::string& name, const Type* const type);
        void recordVariableModification(const TaskVariable& var);

	std::map<std::string, ParameterInfo> parameterInfo;
	std::vector<void *> parameterBinding;
	// Pointer to write result back to if required (i.e. in a farm)
	void *result;

	// Should be a stack but we need to examine it for break and continue
	std::vector<BlockStructure *> statementBodies;
	bool delayedFinish;

	taskGraphFunction functionAddress; // The function in that library
	bool isCompiled;	     // True if the TaskGraph has been compiled

	TaskLoopIdentifier *nextId;
	boost::shared_ptr<TaskCompiledCode> code;
	TaskIR::tgStatementList main_body;
	Type *returnType;
	std::string returnTypeString;
	// Do we contain SPU code, or standard code for this architecture?
	bool isSpuCode;
};



class TaskGraphGroup {
public:
	TaskGraphGroup ( );
	~TaskGraphGroup ( );
	void add ( coreTaskGraph *taskGraph ) {
		graphs.push_back ( taskGraph );
	}
	void compile ( int compiler = tg::GCC, bool keepSourceCode = false );
	void compile ( TaskCompiler *compiler, bool keepSourceCode = false );
	void print ( );
private:
	boost::shared_ptr<TaskCompiledCode> code;
	std::vector<coreTaskGraph *> graphs;
	bool isCompiled;
	TaskIRGroup *group;
};

}

#endif

