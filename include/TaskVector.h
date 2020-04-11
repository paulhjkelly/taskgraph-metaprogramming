#ifndef TASK_VECTOR_H__
#define TASK_VECTOR_H__

#include "TaskDefines.h"
#include <cmath>
#include <boost/shared_ptr.hpp>

namespace tg {

// Hack using partial specialization
template<typename T, unsigned SIZE, bool TYPE = true>
class tVector {
private:
	typedef typename tg::variableHelper<T[SIZE]>::tVariableType array_type;
public:
	tVector ( ) :
	    array ( new array_type ( "vector", CURRENT_TASKGRAPH, createType<T[SIZE]>()( CURRENT_TASKGRAPH ) ) ) {
	}

	tVector ( const char *name ) :
	    array ( new array_type ( name, CURRENT_TASKGRAPH, createType<T[SIZE]>()( CURRENT_TASKGRAPH ) ) ) {
	}

	explicit tVector ( const TaskExpression &val ) :
	    array ( new array_type ( "vector", CURRENT_TASKGRAPH, createType<T[SIZE]>()( CURRENT_TASKGRAPH ) ) ) {
		for ( unsigned a = 0; a < SIZE; ++a )
			(*this)[a] = val;
	}

	tVector ( const tVector <T,SIZE,false> &vec ) :
	    array ( new array_type ( "vector", CURRENT_TASKGRAPH, createType<T[SIZE]>()( CURRENT_TASKGRAPH ) ) ) {
		for ( unsigned a = 0; a < SIZE; ++a )
			(*this)[a] = vec[a];
	}

	tVector ( const tVector <T,SIZE,true> &vec ) :
	    array ( new array_type ( "vector", CURRENT_TASKGRAPH, createType<T[SIZE]>()( CURRENT_TASKGRAPH ) ) ) {
		for ( unsigned a = 0; a < SIZE; ++a )
			(*this)[a] = vec[a];
	}

	tVector ( const array_type &_array ) :
	    array ( new array_type ( _array )  ) {
	}

	~tVector () {
	}

	typename array_type::return_type operator[](const TaskExpression &index) const {
		return (*array.get())[index];
	}

	typename array_type::return_type get(const TaskExpression &index) const {
		return (*array.get())[index];
	}

	void set(const TaskExpression &index, const TaskExpression &value) const {
		(*array.get())[index] = value;
	}

	template<bool TYPE2>
	void operator=(const tVector<T,SIZE,TYPE2> &rhs);

	void operator=(const TaskExpression &rhs) {
		for ( unsigned a = 0; a < SIZE; ++a )
			(*this)[a] = rhs;
	}

	TaskExpression squareMagnitude() const {
		TaskExpression val = (*this)[0] * (*this)[0];
		for ( unsigned a = 1; a < SIZE; ++a )
			val = val + ((*this)[a] * (*this)[a]);
		return val;
	}

	// Calculate the magnitude
	TaskExpression magnitude() const {
		return tSqrt ( squareMagnitude ( ) );
	}

	// Normalize the vector
	void normalise() {
		tVar ( int, normalise );
		normalise = magnitude();
		for ( unsigned a = 0; a < SIZE; ++a )
			(*this)[a] = (*this)[a] / normalise;
	}
private:
	boost::shared_ptr<array_type> array;
};

template<typename T, unsigned SIZE>
class tVector<T,SIZE,false> {
public:
	tVector ( ) {
	}

/*	tVector ( const tVector<T,SIZE,true> &vec ) {
		for ( unsigned a = 0; a < SIZE; ++a )
			(*this)[a] = vec[a];
	}*/

	tVector ( const tVector<T,SIZE> &vec );

	const TaskExpression& operator[](unsigned index) const {
		return array[index];
	}

	TaskExpression& operator[](unsigned index) {
		return array[index];
	}

	TaskExpression &get(const TaskExpression &index) const {
		return array[index];
	}

	void set(const TaskExpression &index, const TaskExpression &value) const {
		array[index] = value;
	}

private:
	TaskExpression array[SIZE];
};


template<typename T, unsigned SIZE, bool TYPE>
template<bool TYPE2>
void tVector<T,SIZE,TYPE>::operator=(const tVector<T,SIZE,TYPE2> &rhs) {
	for ( unsigned a = 0; a < SIZE; ++a )
		set(a, rhs.get(a));
}

#define BINARY_VECTOR_VECTOR_OPERATOR(op) \
template<typename T, unsigned SIZE, bool TYPE, bool TYPE2> \
inline tVector<T,SIZE,false> operator op (const tVector<T,SIZE,TYPE> &lhs, const tVector<T,SIZE,TYPE2> &rhs ) { \
	tVector<T,SIZE,false> temp; \
	for ( unsigned a = 0; a < SIZE; ++a ) \
		temp[a] = lhs[a] op rhs[a]; \
	return temp; \
}

BINARY_VECTOR_VECTOR_OPERATOR ( + )
BINARY_VECTOR_VECTOR_OPERATOR ( - )
BINARY_VECTOR_VECTOR_OPERATOR ( * )

#define BINARY_VECTOR_EXPRESSION_OPERATOR(op) \
template<typename T, unsigned SIZE, bool TYPE> \
inline tVector<T,SIZE,false> operator op (const tVector<T,SIZE,TYPE> &lhs, const TaskExpression &rhs ) { \
	tVector<T,SIZE,false> temp; \
	for ( unsigned a = 0; a < SIZE; ++a ) \
		temp[a] = lhs[a] op rhs; \
	return temp; \
}

BINARY_VECTOR_EXPRESSION_OPERATOR(+)
BINARY_VECTOR_EXPRESSION_OPERATOR(-)
BINARY_VECTOR_EXPRESSION_OPERATOR(*)
BINARY_VECTOR_EXPRESSION_OPERATOR(/)

template<typename T, unsigned SIZE, bool TYPE>
inline tVector<T,SIZE,false> operator- (const tVector<T,SIZE,TYPE> &vec ) {
	tVector<T,SIZE,false> temp;
	for ( unsigned a = 0; a < SIZE; ++a )
		temp[a] = -vec[a];
	return temp;
}

template<typename T, unsigned SIZE, bool TYPE, bool TYPE2>
TaskExpression dotProduct ( const class tVector<T,SIZE,TYPE> &lhs, const class tVector<T,SIZE,TYPE2> &rhs ) {
	TaskExpression val = lhs[0] * rhs[0];
	for ( unsigned a = 1; a < SIZE; ++a )
		val = val + lhs[a] * rhs[a];
	return val;
}

}

#endif
