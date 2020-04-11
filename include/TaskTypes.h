#ifndef TASKTYPES_H__
#define TASKTYPES_H__

#include "TaskDefines.h"
#include <string>

namespace tg {

enum TaskType {
	typeInteger,
	typeFloat,
	typePointer,
	typeArray,
	typeStructure
};

enum BinaryOperatorType  {
opADD = 0,
opSUB = 1,
opMULT = 2,
opDIV = 3,
opEQUALTO = 4,
opNOTEQUALTO = 5,
opLESSTHAN = 6,
opGREATERTHAN = 7,
opLESSTHANOREQUALTO = 8,
opGREATERTHANOREQUALTO = 9,
opLOGICALOR = 10,
opLOGICALAND = 11,
opBITWISEOR = 12,
opBITWISEAND = 13,
opLEFTSHIFT = 14,
opRIGHTSHIFT = 15
};

enum UnaryOperatorType {
opNEGATE = 0,
opNOT = 1,
opINVERT = 2
};

template<typename T> struct tg_typeInfo {
	static std::string name ( ) {
		return "unknown";
	}
};

//TODO: Remove me as I shouldn't exist
template<typename T> struct tg_typeInfo<T&> {
	enum { _depth = tg_typeInfo<T>::_depth };
	static std::string name ( ) {
		return "ref";
	}
};

// Specialise for all the types required
template<> struct tg_typeInfo<void>  {
	enum { _depth = 0 };
	enum { _isSigned = false };
	enum { _size = 0 };
	static const unsigned size = 0; //sizeof ( void );
	static const bool isSigned = false;
	static const unsigned depth = 0;
	static const TaskType type = typeInteger;
	typedef void arrayType;
	typedef void pointer_type;
	static std::string name ( ) {
		return "void";
	}
};

template<> struct tg_typeInfo<long>  {
	enum { _depth = 0 };
	enum { _isSigned = true };
	enum { _size = sizeof ( long ) };
	static const unsigned size = sizeof ( long );
	static const bool isSigned = true;
	static const unsigned depth = 0;
	static const TaskType type = typeInteger;
	typedef void arrayType;
	typedef void pointer_type;
	static std::string name ( ) {
		return "long";
	}
};

template<> struct tg_typeInfo<int> {
	enum { _depth = 0 };
	enum { _isSigned = true };
	enum { _size = sizeof ( int ) };
	static const unsigned size = sizeof ( int );
	static const bool isSigned = true;
	static const unsigned depth = 0;
	static const TaskType type = typeInteger;
	typedef void arrayType;
	typedef void pointer_type;
	static std::string name ( ) {
		return "int";
	}
};

template<> struct tg_typeInfo<short> {
	enum { _depth = 0 };
	enum { _isSigned = true };
	enum { _size = sizeof ( short ) };
	static const unsigned size = sizeof ( short );
	static const bool isSigned = true;
	static const unsigned depth = 0;
	static const TaskType type = typeInteger;
	typedef void arrayType;
	typedef void pointer_type;
	static std::string name ( ) {
		return "short";
	}
};

template<> struct tg_typeInfo<unsigned long> {
	enum { _depth = 0 };
	enum { _isSigned = false };
	enum { _size = sizeof ( unsigned long ) };
	static const unsigned size = sizeof ( unsigned long );
	static const bool isSigned = false;
	static const unsigned depth = 0;
	static const TaskType type = typeInteger;
	typedef void arrayType;
	typedef void pointer_type;
	static std::string name ( ) {
		return "ulong";
	}
};

template<> struct tg_typeInfo<unsigned int> {
	enum { _depth = 0 };
	enum { _isSigned = false };
	enum { _size = sizeof ( unsigned int ) };
	static const unsigned size = sizeof ( unsigned int );
	static const bool isSigned = false;
	static const unsigned depth = 0;
	static const TaskType type = typeInteger;
	typedef void arrayType;
	typedef void pointer_type;
	static std::string name ( ) {
		return "uint";
	}
};

template<> struct tg_typeInfo<unsigned short> {
	enum { _depth = 0 };
	enum { _isSigned = false };
	enum { _size = sizeof ( unsigned short ) };
	static const unsigned size = sizeof ( unsigned short );
	static const bool isSigned = false;
	static const unsigned depth = 0;
	static const TaskType type = typeInteger;
	typedef void arrayType;
	typedef void pointer_type;
	static std::string name ( ) {
		return "ushort";
	}
};

template<> struct tg_typeInfo<signed char> {
	enum { _depth = 0 };
	enum { _isSigned = true };
	enum { _size = sizeof ( signed char ) };
	static const unsigned size = sizeof ( signed char );
	static const bool isSigned = true;
	static const unsigned depth = 0;
	static const TaskType type = typeInteger;
	typedef void arrayType;
	typedef void pointer_type;
	static std::string name ( ) {
		return "schar";
	}
};

template<> struct tg_typeInfo<char> {
	enum { _depth = 0 };
	enum { _isSigned = true };
	enum { _size = sizeof ( char ) };
	static const unsigned size = sizeof ( char );
	static const bool isSigned = true;
	static const unsigned depth = 0;
	static const TaskType type = typeInteger;
	typedef void arrayType;
	typedef void pointer_type;
	static std::string name ( ) {
		return "char";
	}
};

template<> struct tg_typeInfo<unsigned char> {
	enum { _depth = 0 };
	enum { _isSigned = false };
	enum { _size = sizeof ( unsigned char ) };
	static const unsigned size = sizeof ( unsigned char );
	static const bool isSigned = false;
	static const unsigned depth = 0;
	static const TaskType type = typeInteger;
	typedef void arrayType;
	typedef void pointer_type;
	static std::string name ( ) {
		return "uchar";
	}
};

template<> struct tg_typeInfo<double> {
	enum { _depth = 0 };
	enum { _isSigned = false };
	enum { _size = sizeof ( double ) };
	static const unsigned size = sizeof ( double );
	static const bool isSigned = false;
	static const unsigned depth = 0;
	static const TaskType type = typeFloat;
	typedef void arrayType;
	typedef void pointer_type;
	static std::string name ( ) {
		return "double";
	}
};

template<> struct tg_typeInfo<float>  {
	enum { _depth = 0 };
	enum { _isSigned = false };
	enum { _size = sizeof ( float ) };
	static const unsigned size = sizeof ( float );
	static const bool isSigned = false;
	static const unsigned depth = 0;
	static const TaskType type = typeFloat;
	typedef void arrayType;
	typedef void pointer_type;
	static std::string name ( ) {
		return "float";
	}
};

template<typename T> struct tg_typeInfo<T *>  {
	enum { _depth = 0 };
	enum { _isSigned = false };
	enum { _size = 0 };
	static const unsigned size = 0;
	static const bool isSigned = false;
	static const unsigned depth = 0;
	typedef T pointer_type;
	typedef void arrayType;
	static const TaskType type = typePointer;
	static std::string name ( ) {
		return "pointer to " + tg_typeInfo<T>::name();
	}
};
/*
template<> struct tg_typeInfo<char *>  {
	enum { _depth = 0 };
	static const unsigned size = 0;
  static const bool isSigned = false;
	static const unsigned depth = 0;
	typedef char pointer_type;
	typedef void arrayType;
	static const TaskType type = typePointer;
};
*/

template<typename T>
struct arrayHelper {
	static void getList ( unsigned *list, unsigned i = 0 ) { }
	typedef T baseType;
};

template<typename T, unsigned N>
struct arrayHelper<T[N]> {
	static void getList ( unsigned *list, unsigned i = 0 ) {
		list[i] = N;
		arrayHelper<T>::getList ( list, i + 1 );
	}
	typedef typename arrayHelper<T>::baseType baseType;
};

template<typename T, unsigned N> struct tg_typeInfo<T[N]> {
	enum { _depth = tg_typeInfo<T>::_depth + 1 };
	enum { _isSigned = false };
	enum { _size = tg_typeInfo<T>::size*N };
	static const unsigned size = _size;
	static const bool isSigned = false;
	static const unsigned depth = _depth;
	static const TaskType type = typeArray;
	typedef T arrayType[N];
	typedef void pointer_type;
	static void getList ( unsigned *list, unsigned i = 0 ) {
		return arrayHelper<T[N]>::getList ( list, i );
	}
	static std::string name ( ) {
		return "array of " + tg_typeInfo<T>::name();
	}
	typedef typename arrayHelper<T>::baseType baseType;
};

}

#endif
