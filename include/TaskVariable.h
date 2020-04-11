#ifndef TASKVARIABLE_H__
#define TASKVARIABLE_H__

#include "TaskDefines.h"
#include "TaskUtilities.h"
#include "TaskType.h"
#include "TaskTypes.h"
#include "TaskIR.h"
#include "TaskGraph.h"
#include <algorithm>
#include <iostream>
#include <string>
#include <cassert>
#include <boost/scoped_array.hpp>

#define PRE_UNARY_OPERATOR(type,operator1,operator2) \
type &operator operator1 ( ) {\
	*this operator2 1;\
	return *this;\
}\

namespace tg 
{

enum MathType {
	typeMATH = 0,
	typeCOMPARISON = 1,
	typeLOGICALBOOLEAN = 2
};

template <typename T, unsigned depth = tg::tg_typeInfo<T>::_depth > struct variableHelper;

// FIXME: Ref count and clone as needed
struct ExpressionPair {
	// TODO: Why does this exist? Assumes expr is pointer
	ExpressionPair ( ) : expression(NULL), type(NULL) {
	}

        ExpressionPair(const ExpressionPair& e) : expression(e.expression), type(e.type) {
          if (type != NULL)
            type->ref();
        }

	ExpressionPair ( TaskIR::tgExpression _expression, Type *_type )
		: expression ( _expression ), type ( _type ) {
                assert(expression != NULL);
                assert(type != NULL);
		type->ref();
	}

	ExpressionPair& operator=(const ExpressionPair& other) {
		setType(other.type);
		setExpression(other.expression);
		return *this;
	}

	~ExpressionPair() {
		if (type != NULL ) 
                  type->unref();
	}

	Type *getType ( ) const {
		// TODO: Add assert NULL here
		return type;
	}

	TaskIR::tgExpression getExpression ( ) const {
		return expression;
	}

	void setExpression ( TaskIR::tgExpression _expression ) {
		expression = _expression;
	}

	void setType(Type* _type) {
                if (_type != NULL)
                  _type->ref();

		if (type != NULL) 
                  type->unref();

		type = _type;
	}

private:
	TaskIR::tgExpression expression;
	Type *type;
};

class TaskVariable;


class TaskExpression {
public:
	TaskExpression ( ) {
	}

	TaskExpression ( TaskIR::tgExpression expr, Type *type )
		: pair ( expr, type ) {
		}

	TaskExpression ( const ExpressionPair& _pair )
		: pair ( _pair ) {
		}

	TaskExpression ( const TaskExpression &expr, UnaryOperatorType action );
	TaskExpression ( MathType type, const TaskExpression &lhs, const TaskExpression &rhs, BinaryOperatorType action );

	TaskExpression ( const TaskVariable &var );

	TaskExpression ( const TaskExpression &expr )
		: pair ( expr.getExpression ( ), expr.getType ( ) ) {
        }

	TaskExpression &operator= ( const TaskExpression &expr ) {
              if (pair.getExpression() != NULL)
                coreTaskGraph::deleteExpression( pair.getExpression() );

              pair.setExpression ( expr.getExpression() );
              pair.setType ( expr.getType ( ) );
	      return *this;
	}

	TaskExpression ( long number );
	TaskExpression ( int number );
	TaskExpression ( short number );
	TaskExpression ( signed char number );
	TaskExpression ( unsigned long number );
	TaskExpression ( unsigned int number );
	TaskExpression ( unsigned short number );
	TaskExpression ( unsigned char number );
	TaskExpression ( float number );
	TaskExpression ( double number );
	TaskExpression ( const char * );

	TaskExpression cast ( Type *type ) const;

	~TaskExpression () {
			coreTaskGraph::deleteExpression( pair.getExpression() );
	}

	// Dangers here if original expression changes?
	TaskIR::tgExpression getExpression ( ) const {
			return cloneExpression ( );
	}

	Type *getType ( ) const {
		return pair.getType ( );
	}
private:
	TaskIR::tgExpression cloneExpression ( ) const;

private:
	ExpressionPair pair;
};

class TaskExpressionSymbol : public TaskExpression {
public:
    TaskExpressionSymbol() {
    }
    TaskExpressionSymbol ( const ExpressionPair& _pair, const TaskVariable *_var )
        : TaskExpression ( _pair ), var ( _var ) {
    }
    const TaskVariable &getVariable() const {
        return *var;
    }
private:
    const TaskVariable *var;
};

class TaskStructure;
class TaskVariable {
public:
	TaskVariable ( const char *_name, TaskIR::tgVariable _symbol, Type *_type, bool _parameter )
		: name(_name),
	symbol(_symbol),
	type(_type),
	parameter(_parameter),
	isSymbol(true),
	parentStruct(0),
	offset(0) {
		assert(name.size() > 0);
		type->ref();
	}

	TaskVariable ( const TaskExpressionSymbol &_pair, Type *_type )
		: name(""),
	symbol(0),
	type(_type),
	parameter(false),
	isSymbol(false),
	base(_pair),
	parentStruct(0),
	offset(0)  {
		type->ref();
	}

    TaskVariable ( TaskStructure *str, int _offset, Type *_type )
         : symbol(NULL), type(_type), parameter(false), isSymbol(false), parentStruct(str), offset(_offset) {
        _type->ref();
        assert(_type != NULL);
	}

	// FIXME: Clone base if used
	TaskVariable ( const TaskVariable &other )
	  : name(other.name),
	    symbol(other.symbol),
	    type(other.type),
	    parameter(other.parameter),
	    isSymbol(other.isSymbol),
		parentStruct(other.parentStruct),
		offset(other.offset) {
		type->ref();
	}

	virtual ~TaskVariable ( ) {
		type->unref();
	}

	TaskIR::tgVariable getSymbol ( ) const {
		return symbol;
	}

	virtual Type *getType ( ) const {
		return type;
	}

	bool hasBase ( ) const {
		return !hasSymbol();
	}

	bool hasSymbol ( ) const {
		if (parentStruct != 0) {
			return false;
		}
		return isSymbol;
	}

        const TaskVariable *getRootVariable() const;

	virtual TaskExpressionSymbol getBase ( ) const;

	std::string getName() const {
		return name;
	}

	bool isParameter() const {
		return parameter;
	}

	virtual int getOffset() const;
	virtual ExpressionPair load ( ) const = 0;
protected:
	std::string name;
	TaskIR::tgVariable symbol;
	Type *type;
	bool parameter;
	bool isSymbol;
	TaskExpressionSymbol base;
	TaskStructure *parentStruct;
	int offset;
};


class TaskScalarVariable : public TaskVariable {
public:
	TaskScalarVariable ( const char *_name, coreTaskGraph *owner, NumericType *_type )
	: TaskVariable ( _name, 0, _type, owner->getAddingParameters ( ) ) {
		symbol = owner->addVariableDefinition ( name.c_str(), parameter, _type );
	}

    TaskScalarVariable ( TaskStructure *str, int _offset, Type *_type )
        : TaskVariable ( str, _offset, _type ) {
    }

	TaskScalarVariable ( const TaskScalarVariable &other ) : TaskVariable(other) {
	}

	TaskScalarVariable ( const TaskExpressionSymbol &pair, Type *_type )
	: TaskVariable ( pair, _type ) {
	}

    //TaskVariable &operator++ ( int a );
	TaskExpression operator= ( const TaskExpression &rhs );
	TaskExpression operator+= ( const TaskExpression &rhs );
	TaskExpression operator-= ( const TaskExpression &rhs );
	TaskExpression operator/= ( const TaskExpression &rhs );
	TaskExpression operator*= ( const TaskExpression &rhs );

	// Prefix ++, --
	PRE_UNARY_OPERATOR(TaskScalarVariable,++,+=);
	PRE_UNARY_OPERATOR(TaskScalarVariable,--,-=);

	// C++ makes us put this in
	TaskExpression operator= ( const TaskScalarVariable &rhs ) {
		return this->operator= ( TaskExpression ( rhs ) );
	}

	static TaskScalarVariable createFromPointer ( const TaskExpressionSymbol &pair, Type *_type ) {
		return TaskScalarVariable ( pair, _type );
	}

	ExpressionPair load ( ) const;
};

template<typename T, TaskType type = tg_typeInfo<T>::type>
class ReturnHelper
{
public:
	typedef TaskScalarVariable return_type;

	static return_type createFromBase(const TaskExpressionSymbol& base ) {
		return TaskScalarVariable(base, createType<T>()(CURRENT_TASKGRAPH) );
	}
};

template<typename T>
class  ReturnHelper<T, typeStructure> 
{
private:
        typedef typename tg_typeInfo<T>::TaskStructure TaskStructure;

public:
	typedef typename TaskStructure::return_type return_type;

	static return_type createFromBase(const TaskExpressionSymbol& base) {
		return TaskStructure::createFromBase(base);
	}
};

class TaskStructure : public TaskVariable {
public:
	TaskStructure ( const char *_name, TaskIR::tgVariable _symbol, Type *_type, bool _parameter )
	: TaskVariable ( _name, _symbol, _type, _parameter ) {
	}

	TaskStructure ( const TaskExpressionSymbol &_pair, Type *_type )
	: TaskVariable ( _pair, _type ){
	}

    TaskStructure ( TaskStructure *str, int _offset, Type *_type )
        : TaskVariable ( str, _offset, _type ) {
    }

	static StructType *reg(const char *name, int size, int fields) {
		return createStructType(CURRENT_TASKGRAPH, name, size, fields );
	}

	template<typename T>
	static void regField(std::vector<Type*>& types, StructType *parent, const char *name, unsigned pos, unsigned offset) {
		Type* const type = tg::createType<T>() ( CURRENT_TASKGRAPH );
                types.push_back(type);
                type->ref();
		setStructField(CURRENT_TASKGRAPH, parent, pos, name, type, offset);
	}

	static void installType(StructType *type) {
		CURRENT_TASKGRAPH->installStructType(type);
	}

	ExpressionPair load ( ) const;
	// TODO: Return type
	void operator=( const TaskStructure &rhs );
};

template<int SIZE, typename BASETYPE>
class TaskArray : TaskVariable {
private:
	const TaskArray operator= ( const TaskArray &other );

	TaskArray ( const TaskExpressionSymbol &pair, ArrayType *_type )
		: TaskVariable(pair, _type), indices(0), indiceSize(0) {
	}
public:
    static TaskArray<SIZE,BASETYPE> createFromPointer ( const TaskExpressionSymbol &pair, const ArrayType &_type ) {
		return TaskArray<SIZE,BASETYPE> ( pair, _type );
	}

	friend class TaskArray<SIZE-1,BASETYPE>;
	friend class TaskArray<SIZE+1,BASETYPE>;

    TaskArray ( TaskStructure *str, int _offset, Type *_type )
        : TaskVariable ( str, _offset, _type ), indices(0), indiceSize(0) {
    }

	TaskArray ( const char *_name, coreTaskGraph *_owner, ArrayType *_type )
		: TaskVariable(_name, 0, _type, _owner->getAddingParameters ( ) ), indices(0), indiceSize(0) {
			symbol = _owner->addArrayDefinition ( name.c_str(), parameter, getType());
		}

	TaskArray ( const TaskArray &other )
		: TaskVariable(other)
    {
		indiceSize = other.indiceSize;
		indices =  new TaskIR::tgExpression[ indiceSize ];
		std::copy ( other.indices, &other.indices[indiceSize], indices );
	}

	~TaskArray ( ) {
		delete [] indices;
	}

	TaskArray<SIZE-1,BASETYPE> operator[]( const TaskExpression &index ) const {
		return TaskArray<SIZE-1,BASETYPE> ( *this, index );
	}

	ArrayType *getType ( ) const {
		return static_cast<ArrayType*> ( type );
	}

	unsigned getIndiceSize ( ) const {
		return indiceSize;
	}

	TaskIR::tgExpression *getIndices () {
		return indices;
	}
    virtual ExpressionPair load ( ) const {
        throw TaskException("Load not supported on arrays");
    }
protected:
	TaskArray ( const TaskArray<SIZE+1,BASETYPE> &other, const TaskExpression &_index )
		: TaskVariable(other)
    {
            indiceSize = other.indiceSize + 1;
            indices =  new TaskIR::tgExpression[ indiceSize ];
            indices [ 0 ] = _index.getExpression();
            std::copy ( other.indices, &other.indices[indiceSize-1], &indices[1] );
    }

protected:
	TaskIR::tgExpression *indices;
	unsigned indiceSize;
};

template<typename BASETYPE>
class TaskArray<1,BASETYPE> : TaskVariable {
private:
	const TaskArray operator= ( const TaskArray &other );

	TaskArray ( const TaskExpressionSymbol &pair, ArrayType *_type ) 
        : TaskVariable ( pair, _type ),
		  indices(0), indiceSize(0) {
		}
public:
    static TaskArray<1,BASETYPE> createFromPointer ( const TaskExpressionSymbol &pair, const ArrayType &_type ) {
		return TaskArray<1,BASETYPE> ( pair, &_type );
	}

	friend class TaskArray<2,BASETYPE>;

	TaskArray ( const char *_name, coreTaskGraph *_owner, ArrayType *_type )
        : TaskVariable ( _name, 0, _type, _owner->getAddingParameters ( ) ),
		  indices(0), indiceSize(0) {
			symbol = _owner->addArrayDefinition ( name.c_str(), parameter, getType());
		}

	TaskArray ( const TaskArray &other )
		: TaskVariable(other)
	{
		indiceSize = other.indiceSize;
		indices =  new TaskIR::tgExpression[ indiceSize ];
		std::copy ( other.indices, &other.indices[indiceSize], indices );
    }


	~TaskArray ( ) {
		if ( indices != 0 ) {
			delete [] indices;
		}
	}

    TaskArray ( TaskStructure *str, int _offset, Type *_type )
        : TaskVariable ( str, _offset, _type ), indices(0), indiceSize(0) {
    }

	typedef typename ReturnHelper<BASETYPE>::return_type return_type;

	return_type operator[]( const TaskExpression &index ) const {
		boost::scoped_array<TaskIR::tgExpression> nindices(new TaskIR::tgExpression[indiceSize + 1]);
		nindices [ 0 ] = index.getExpression();
		std::copy ( indices, &indices[indiceSize], &nindices[1] );

		if ( parentStruct != 0 ) {
			TaskExpression base = TaskExpression(CURRENT_TASKGRAPH->createLoadStructureExpression( parentStruct, offset, getType() ));
			ExpressionPair expressionPair = CURRENT_TASKGRAPH->createArrayAccessWithBaseExpression ( base, getType()->getBaseType(), indiceSize+1, getType()->getDimensions(), &nindices[0], parameter );
			return ReturnHelper<BASETYPE>::createFromBase ( TaskExpressionSymbol(expressionPair, getRootVariable()) );
		}
		ExpressionPair expressionPair = CURRENT_TASKGRAPH->createArrayAccessExpression ( getSymbol(), getType()->getBaseType(), indiceSize+1, getType()->getDimensions(), &nindices[0], parameter );
		return ReturnHelper<BASETYPE>::createFromBase ( TaskExpressionSymbol(expressionPair, getRootVariable()) );
	}

	ArrayType *getType ( ) const {
        return static_cast<ArrayType*> ( type );
	}

	unsigned getIndiceSize ( ) const {
		return indiceSize;
	}

	TaskIR::tgExpression *getIndices () {
		return indices;
	}

    virtual ExpressionPair load ( ) const {
        throw TaskException("Load not supported on arrays");
    }
	template<typename I>
	void operator=(I *assign) {
		throw TaskException("Array assignment not implemented yet!!");
	}
protected:
	TaskArray ( const TaskArray<2,BASETYPE> &other, const TaskExpression &_index )
		: TaskVariable(other) {
		indiceSize = other.indiceSize + 1;
		indices =  new TaskIR::tgExpression[ indiceSize ];
		indices [ 0 ] = _index.getExpression();
		std::copy ( other.indices, &other.indices[indiceSize-1], &indices[1] );
		}

protected:
	TaskIR::tgExpression *indices;
	unsigned indiceSize;
};


template<typename T>
class TaskPointer : public TaskVariable {
private:
	const TaskPointer operator= ( const TaskPointer &other );

public:

	TaskPointer (const char *_name, coreTaskGraph* _owner, PointerType* _type )
		: TaskVariable( _name, 0, _type, _owner->getAddingParameters ( ) )
        {
			symbol = _owner->addPointerDefinition ( name.c_str(), parameter, _type );
	}

        TaskPointer(const TaskPointer& t) : TaskVariable(t)
        {
        }

	TaskPointer ( const TaskExpressionSymbol &pair, PointerType* _type )
		: TaskVariable(pair, type) {
	}

	static TaskPointer<T> createFromPointer ( const TaskExpressionSymbol &pair, PointerType* _type ) {
		return TaskPointer<T> ( pair, _type );
	}
	
        PointerType *getType ( ) const {
          return static_cast<PointerType*> ( type );
	}

	typename tg::variableHelper<T>::tVariableType operator[]( const TaskExpression &index ) const {
                ExpressionPair expressionPair;

		if ( isSymbol ) {
			expressionPair = CURRENT_TASKGRAPH->createPointerAccessExpression ( symbol, index, parameter, getType());
		} else {
                        expressionPair = CURRENT_TASKGRAPH->createPointerAccessWithBaseExpression ( base, index, getType());
		}

                TaskExpressionSymbol expressionSymbol(expressionPair, getRootVariable());
		return tg::variableHelper<T>::tVariableType::createFromPointer(expressionSymbol, tg::createType<T>() ( CURRENT_TASKGRAPH ) );
	}

        typename tg::variableHelper<T>::tVariableType operator*() const {
          return (*this)[0];  
        }

	ExpressionPair load ( ) const {
		return ExpressionPair();
	}
};

#define DEFINE_BINARY_OPERATOR(symbol,type,action) \
inline TaskExpression operator symbol ( const TaskExpression &lhs, const TaskExpression &rhs ) { \
	return TaskExpression ( type, lhs, rhs, action ); \
}

#define DEFINE_EQUAL_OPERATOR(type,symbol) \
inline TaskExpression type::operator symbol##= ( const TaskExpression &rhs ) { \
	return (*this = *this symbol rhs); \
}

// TODO: Add bitwise and, or, xor - remainder - shifts - functions for max and min
DEFINE_BINARY_OPERATOR ( +, typeMATH, opADD )
DEFINE_BINARY_OPERATOR ( -, typeMATH, opSUB )
DEFINE_BINARY_OPERATOR ( *, typeMATH, opMULT )
DEFINE_BINARY_OPERATOR ( /, typeMATH, opDIV )
DEFINE_BINARY_OPERATOR ( ==, typeCOMPARISON, opEQUALTO )
DEFINE_BINARY_OPERATOR ( <, typeCOMPARISON, opLESSTHAN )
DEFINE_BINARY_OPERATOR ( >, typeCOMPARISON, opGREATERTHAN )
DEFINE_BINARY_OPERATOR ( <=, typeCOMPARISON, opLESSTHANOREQUALTO )
DEFINE_BINARY_OPERATOR ( >=, typeCOMPARISON, opGREATERTHANOREQUALTO )
DEFINE_BINARY_OPERATOR ( !=, typeCOMPARISON, opNOTEQUALTO )
DEFINE_BINARY_OPERATOR ( ||, typeLOGICALBOOLEAN, opLOGICALOR )
DEFINE_BINARY_OPERATOR ( &&, typeLOGICALBOOLEAN, opLOGICALAND )
DEFINE_BINARY_OPERATOR ( &, typeMATH, opBITWISEAND )
DEFINE_BINARY_OPERATOR ( |, typeMATH, opBITWISEOR )

//TODO: Change from typeMath
DEFINE_BINARY_OPERATOR ( <<, typeMATH, opLEFTSHIFT )
DEFINE_BINARY_OPERATOR ( >>, typeMATH, opRIGHTSHIFT )

//TODO: All of them
DEFINE_EQUAL_OPERATOR ( TaskScalarVariable, + )
DEFINE_EQUAL_OPERATOR ( TaskScalarVariable, - )
DEFINE_EQUAL_OPERATOR ( TaskScalarVariable, * )
DEFINE_EQUAL_OPERATOR ( TaskScalarVariable, / )

//DEFINE_EQUAL_OPERATOR ( TaskArray<0,T>, + )
//DEFINE_EQUAL_OPERATOR ( TaskArray<0>, - )
//DEFINE_EQUAL_OPERATOR ( TaskArray<0>, * )
//DEFINE_EQUAL_OPERATOR ( TaskArray<0>, / )

inline const TaskExpression &operator+( const TaskExpression &expr ) {
	return expr;
}

inline const TaskExpression operator-( const TaskExpression &expr ) {
	return TaskExpression ( expr, opNEGATE );
}

inline const TaskExpression operator!( const TaskExpression &expr ) {
	return TaskExpression ( expr, opNOT );
}

inline const TaskExpression operator~( const TaskExpression &expr ) {
	return TaskExpression ( expr, opINVERT );
}

template<typename T> TaskExpression cast ( const TaskExpression &expr ) {
	return expr.cast ( createType<T>() ( CURRENT_TASKGRAPH ) );
}

}


// This macro can now handle both scalar and array variables, so tArray can be safely replaced by tVar
// tVar is used explicitly to declare local dynamic variables (scalar or arrays).
#define tVar(type,x) tg::variableHelper<type>::tVariableType x ( #x, CURRENT_TASKGRAPH, tg::createType<type>() ( CURRENT_TASKGRAPH ) )
#define tVarNamed(type,x,name) tg::variableHelper<type>::tVariableType x ( name, CURRENT_TASKGRAPH, tg::createType<type>() ( CURRENT_TASKGRAPH ) )
#define tVarTemplateType(type,x) typename tg::variableHelper<type>::tVariableType x ( #x, CURRENT_TASKGRAPH, tg::createType<type>() ( CURRENT_TASKGRAPH ) )
#define tVarTemplateTypeNamed(type,x,name) typename tg::variableHelper<type>::tVariableType x ( name, CURRENT_TASKGRAPH, tg::createType<type>() ( CURRENT_TASKGRAPH ) )

// tParameter is used internally and should not be used explicitly.
#define tTypeSafeParameter(type,x) type::parameterType x ( #x, CURRENT_TASKGRAPH, tg::createType<type::originalType>()(CURRENT_TASKGRAPH ) )

// Remains for compatibility, works like tVar, but can only handle arrays.
#define tArray(type,x) tg::TaskArray< arrayHelper<type>::depth, type> x ( #x, CURRENT_TASKGRAPH, tg::createType<type>() ( CURRENT_TASKGRAPH ) );
#define tArrayFromList(type,x,ndims,sizes) tg::TaskArray<ndims, type> x ( #x, CURRENT_TASKGRAPH, tg::createArrayType ( CURRENT_TASKGRAPH, tg::createType<type>() ( CURRENT_TASKGRAPH ), ndims, sizes ) );
#define tArrayFromListNamed(type,x,ndims,sizes,name) tg::TaskArray<ndims, type> x ( name, CURRENT_TASKGRAPH, tg::createArrayType ( CURRENT_TASKGRAPH, tg::createType<type>() ( CURRENT_TASKGRAPH ), ndims, sizes ) );


#define tParameter(x)  CURRENT_TASKGRAPH->setAddingParameters ( true ); x; CURRENT_TASKGRAPH->setAddingParameters ( false );
#define tParameterStart  CURRENT_TASKGRAPH->setAddingParameters ( true );
#define tParameterEnd  CURRENT_TASKGRAPH->setAddingParameters ( false );

#endif
