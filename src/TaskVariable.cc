#include <TaskVariable.h>
#include <TaskType.h>

namespace tg {

//
// Create an integer type
//
NumericType *createIntegerType ( coreTaskGraph *owner, unsigned _size, bool _isSigned ) {
	return owner->createIntegerType ( _size, _isSigned );
}

//
// Create a float type
//
NumericType *createFloatType ( coreTaskGraph *owner, unsigned _size ) {
	return owner->createFloatType ( _size );
}

PointerType *createPointerType ( coreTaskGraph *owner, Type *baseType ) {
	return owner->createPointerType ( baseType );
}

ArrayType *createArrayType ( coreTaskGraph *owner, Type *baseType, unsigned numDimensions, unsigned *dimensions ) {
	TaskIR::tgDataType irType = baseType->getIRType();

	for(int i = numDimensions-1; i >= 0; --i )
		irType = owner->createArrayType ( irType, dimensions[i]-1 );
	
	ArrayType *type = new ArrayType ( baseType, numDimensions, dimensions );

        // FIXME: The created irType is almost never used, causing memory leaks.
	type->setIRType ( irType );
	return type;
}

StructType *createStructType ( coreTaskGraph *owner, const char *name, int size, int fields ) {
    return owner->createStructType ( name, size, fields );
}

void setStructField ( coreTaskGraph *owner, StructType *type, unsigned pos, const char *name, Type *fieldType, int offset ) {
    owner->setStructField ( type, pos, name, fieldType, offset );
}

TaskExpressionSymbol TaskVariable::getBase ( ) const {
	if (parentStruct != 0) {
		return TaskExpressionSymbol(CURRENT_TASKGRAPH->createLoadStructureExpression( parentStruct, getOffset(), getType() ), getRootVariable());
	}
	return base;
}

int TaskVariable::getOffset() const {
	if ( parentStruct != 0) {
//		return offset + parentStruct->getOffset();
	}
	return offset;
}

const TaskVariable *TaskVariable::getRootVariable() const {
    if ( isSymbol )
        return this;
    if ( parentStruct != 0 ) {
        return parentStruct->getRootVariable();
    }
    return base.getVariable().getRootVariable();
}

ExpressionPair TaskScalarVariable::load ( ) const {
	if ( hasSymbol() ) {
		if ( !parameter )
			return CURRENT_TASKGRAPH->createLoadVariableExpression ( *this );
		else
			return CURRENT_TASKGRAPH->createLoadAndDereferenceVariableExpression ( *this );
	} else {
		return CURRENT_TASKGRAPH->createLoadWithBaseExpression ( getBase(), getType() );
	}
}

TaskExpression TaskScalarVariable::operator= ( const TaskExpression &rhs ) {
	if ( hasSymbol() ) {
		if ( !parameter )
			CURRENT_TASKGRAPH->addStoreVariableStatement ( *this, rhs );
		else
			CURRENT_TASKGRAPH->addDereferenceAndStoreVariableExpression ( *this, rhs );
	} else {
		CURRENT_TASKGRAPH->addStoreWithBaseStatement ( getBase(), rhs, getType() );
	}
	// return result variable to allow: a = b = c, etc
	//TODO: This could do be with being delayed
	return TaskExpression ( *this );
}

ExpressionPair TaskStructure::load ( ) const {
	if ( hasSymbol() ) {
		if ( !parameter )
			return CURRENT_TASKGRAPH->createLoadVariableExpression ( *this );
		else
			return CURRENT_TASKGRAPH->createLoadAndDereferenceVariableExpression ( *this );
	} else {
		return CURRENT_TASKGRAPH->createLoadWithBaseExpression ( getBase(), getType() );
	}
}

// TODO: Return type
void TaskStructure::operator=( const TaskStructure &rhs ) {
	if ( hasSymbol() ) {
		if ( !parameter )
			CURRENT_TASKGRAPH->addStoreVariableStatement ( *this, rhs );
		else
			CURRENT_TASKGRAPH->addDereferenceAndStoreVariableExpression ( *this, rhs );
	} else {
		CURRENT_TASKGRAPH->addStoreWithBaseStatement ( getBase(), rhs, getType() );
	}
}

TaskExpression::TaskExpression ( MathType type, const TaskExpression &lhs, const TaskExpression &rhs, BinaryOperatorType action ) {
	if ( type == typeMATH ) {
		pair = CURRENT_TASKGRAPH->createBinaryMathExpression ( action, lhs, rhs );
	} else if ( type == typeCOMPARISON ) {
		pair = CURRENT_TASKGRAPH->createBinaryComparisonExpression ( action, lhs, rhs );
	} else if ( type == typeLOGICALBOOLEAN ) {
		pair = CURRENT_TASKGRAPH->createBinaryComparisonExpression ( action, lhs, rhs );
	}
}

TaskExpression::TaskExpression ( const TaskExpression &expr, UnaryOperatorType action )
	: pair ( CURRENT_TASKGRAPH->createUnaryExpression ( action, expr ) ) {
}

TaskIR::tgExpression TaskExpression::cloneExpression ( ) const {
	return CURRENT_TASKGRAPH->deepClone ( pair.getExpression() );
}

TaskExpression::TaskExpression ( const TaskVariable &var )
	: pair ( var.load ( ) ) {
}

TaskExpression TaskExpression::cast ( Type *type ) const {
	return TaskExpression ( CURRENT_TASKGRAPH->createCastExpression ( *this, type ) );
}

#define CREATE_NUMBER(type,func)  TaskExpression::TaskExpression ( type number ) \
	: pair ( coreTaskGraph::getCurrentTaskGraph()->func ( number, sizeof ( type ) ) ) { \
}

CREATE_NUMBER(long,createSignedIntegerConstantExpression)
CREATE_NUMBER(short,createSignedIntegerConstantExpression)
CREATE_NUMBER(int,createSignedIntegerConstantExpression)
CREATE_NUMBER(signed char,createSignedIntegerConstantExpression)

CREATE_NUMBER(unsigned long,createUnsignedIntegerConstantExpression)
CREATE_NUMBER(unsigned short,createUnsignedIntegerConstantExpression)
CREATE_NUMBER(unsigned int,createUnsignedIntegerConstantExpression)
CREATE_NUMBER(unsigned char,createUnsignedIntegerConstantExpression)

CREATE_NUMBER(float,createFloatConstantExpression)
CREATE_NUMBER(double,createFloatConstantExpression)


TaskExpression::TaskExpression ( const char *text )
	: pair ( coreTaskGraph::getCurrentTaskGraph()->createStringConstantExpression ( text ) ) {
}

}
