#ifndef TASKIR_H__
#define TASKIR_H__

#include "TaskDefines.h"
#include "TaskTypes.h"
#include <cstdio>
#include <vector>

class DataInfo;
class coreTaskGraph;

class Variable {
public:
	Variable ( bool _parameter, int _number )
	 : parameter ( _parameter ), number(_number) {
	}
	virtual void outputToC ( FILE *file ) = 0;
	virtual void outputDeclerationToC ( FILE *file ) = 0;
protected:
	bool parameter;
	int number;
};

class Statement {
public:
	virtual void outputToC ( FILE *file ) = 0;
};

class StatementList {
public:
	StatementList ( ) {
	}
	void addStatement ( Statement *statement ) {
		statements.push_back ( statement );
	}
	void addVariableDefinition ( Variable *var ) {
		variableDefinitions.push_back ( var );
	}
	void outputToC ( FILE *file );
private:
	// TODO: Maybe vector is not the best container
	std::vector<Statement *> statements;
	std::vector<Variable *> variableDefinitions;
};

class Expression {
public:
	virtual void outputToC ( FILE *file ) = 0;
};

class DataType {
public:
	DataType ( TaskType _type, unsigned _size, bool _isSigned )
		: type (_type ), size ( _size ), isSigned ( _isSigned ) {
	}
	void outputToC ( FILE *file );
private:
	TaskType type;
	unsigned size;
	bool isSigned;
};

class Function {
public:
	Function ( const char *_name, StatementList *_body ) 
	 : name(_name), body(_body){
	}
	void outputToC ( FILE *file );
	void addVariableDefinition ( Variable *var ) {
		body->addVariableDefinition ( var );
	}
private:
	const char *name;
	StatementList *body;
};

class TaskIR {
public:
	typedef ::StatementList *tgStatementList;
	typedef ::Statement *tgStatement ;
	typedef ::Variable *tgVariable;
	typedef ::Expression *tgExpression;
	typedef ::DataType *tgDataType;
public:
	void outputToC ( char *fileName );

	tgStatementList init ( const char *functionName );

	void appendStatement ( tgStatementList list, tgStatement statement );
	tgStatementList createStatementList ( );

	tgDataType createIntegerType ( DataInfo &dataInfo );
	tgDataType createFloatType ( DataInfo &dataInfo );

	tgVariable addVariableDefinition ( coreTaskGraph *taskgraph, const char *name, bool parameter, DataInfo &dataInfo );
	tgVariable addArrayDefinition ( coreTaskGraph *taskgraph, const char *name, unsigned dimensions, unsigned *sizes, bool parameter, DataInfo& dataInfo );

	tgExpression createSignedIntegerConstantExpression ( long number, unsigned size, DataInfo &dataInfo );
	tgExpression createUnsignedIntegerConstantExpression ( unsigned long number, unsigned size, DataInfo &dataInfo );
	tgExpression createFloatConstantExpression ( double number, unsigned size, DataInfo &dataInfo );

	tgExpression createBinaryMathExpression ( BinaryOperatorType operation, tgExpression lhs, tgExpression rhs, tgDataType dataType );
	tgExpression createBinaryComparisonExpression ( BinaryOperatorType operation, tgExpression lhs, tgExpression rhs, tgDataType dataType );
	tgExpression createBinaryLogicalBooleanExpression ( BinaryOperatorType operation, tgExpression lhs, tgExpression rhs, DataInfo &dataInfo );
	
	tgExpression createLoadVariableExpression ( tgVariable variable, tgDataType dataInfo );
	tgExpression createLoadAndDereferenceVariableExpression ( tgVariable variable, tgDataType dataInfo );
	tgExpression createArrayAccessExpression ( tgVariable variable, unsigned dimensions, unsigned *sizes, tgExpression *indices, tgDataType dataInfo );

	tgExpression createCastExpression ( tgDataType dataType, tgExpression expr );

	tgStatement addStoreVariableStatement ( tgVariable variable, tgExpression expression );
	tgStatement addDereferenceAndStoreVariableExpression( tgVariable variable, tgExpression expression, tgDataType dataType );
	tgStatement addArrayStoreStatement ( tgVariable variable, tgExpression rhs, unsigned dimensions, unsigned *sizes, tgExpression *indices, tgDataType dataType  );

	tgStatement createWhileStatement ( tgExpression test, tgStatementList list );
	tgStatement createIfStatement ( tgExpression ifpart, tgStatementList thenpart, tgStatementList elsepart );
	tgStatement createForStatement ( tgVariable symbol, tgExpression from, tgExpression to, tgExpression step, tgStatementList list);

	tgExpression deepClone ( tgExpression expression );

private:
	Function *main;
};

#endif
