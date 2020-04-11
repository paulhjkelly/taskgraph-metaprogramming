#ifndef TASKTYPE_H__
#define TASKTYPE_H__

#include "TaskDefines.h"
#include "TaskException.h"
#include "TaskIR.h"
#include <iostream>
#include <boost/scoped_array.hpp>

namespace tg {

class coreTaskGraph;

class Type;
class NumericType;
class PointerType;
class ArrayType;
class StructType;

NumericType *createFloatType ( coreTaskGraph *owner, unsigned _size );
NumericType *createIntegerType ( coreTaskGraph *owner, unsigned _size, bool _isSigned );
PointerType *createPointerType ( coreTaskGraph *owner, Type *baseType );
ArrayType   *createArrayType ( coreTaskGraph *owner, Type *baseType, unsigned numDimensions, unsigned *dimensions );
StructType *createStructType ( coreTaskGraph *owner, const char *name, int size, int fields );
void setStructField ( coreTaskGraph *owner, StructType *type, unsigned pos, const char *name, Type *fieldType, int offset );


// TODO: make types reference counted and auto-cleaned (avoids lots of issues)
class Type {
	protected:
		Type ( ) : count(0) { }
		Type ( const Type &other ) : count(0), irType ( other.irType ) { }
		virtual ~Type() { };
	public:
		TaskIR::tgDataType getIRType ( ) const {
			return irType;
		}

		void setIRType ( TaskIR::tgDataType _irType ) {
			irType = _irType;
		}

		virtual void print ( ) const {
			std::cout << "ERROR:Type????" << std::endl;
		}

		Type *ref() {
			++count;
			return this;
		}

		void unref() {
			if ( --count == 0 )
				delete this;
		}

		virtual unsigned getSize() const = 0;

                virtual bool equals(const Type& t)
                {
                  return irType == t.irType;
                }

	protected:
		unsigned count;
		TaskIR::tgDataType irType;
};

class StructType : public Type {
private:
    unsigned size;

public:
    StructType ( unsigned _size )  : size(_size) {
    }

    unsigned getSize() const {
        return size;
    }
};

class NumericType : public Type {
public:
        unsigned size;
	bool isSigned;
	TaskType type;

        inline bool operator==(const NumericType &rhs) const {
          return type == rhs.type && size == rhs.size && isSigned == rhs.isSigned;
        }

        inline bool operator!=(const NumericType &rhs) const {
          return !(*this == rhs);
        }

	virtual void print ( ) const  {
          std::cout << "NumericType " << size << " " << isSigned << " " << getIRType() << " " << std::endl;
	}
			
        virtual unsigned getSize() const
        {
	  return size;
	}
};

class ArrayType
	: public Type {
		public:
			ArrayType ( Type *_baseType, int _numDimensions, unsigned *_dimensions )
				: baseType ( _baseType ),
			      numDimensions ( _numDimensions ), dimensions(new unsigned [numDimensions]) {
				baseType->ref();
				std::copy ( _dimensions, _dimensions + numDimensions, dimensions.get());
			}

			ArrayType ( const ArrayType &other )
			: baseType ( other.baseType ),
				numDimensions ( other.numDimensions ), dimensions(new unsigned [numDimensions]) {
				baseType->ref();
				std::copy (other.dimensions.get(), other.dimensions.get() + numDimensions, dimensions.get());
			}

			virtual ~ArrayType ( ) {
				baseType->unref();
			}

			unsigned *getDimensions ( ) const {
				return dimensions.get();
			}

			unsigned getNumDimensions ( ) const {
				return numDimensions;
			}

			Type *getBaseType ( ) const {
				return baseType;
			}
			virtual void print ( ) const  {
				std::cout << "ArrayType " << numDimensions << " of" << std::endl;
				std::cout << "  ";
				baseType->print();
			}
			
			virtual unsigned getSize() const
			{
				unsigned size = baseType->getSize();
				for(unsigned int dimension = 0; dimension < numDimensions; ++dimension)
				{
					size *= dimensions[dimension];
				}
				return size;
			}

		private:
			Type *baseType;
			unsigned numDimensions;
			boost::scoped_array<unsigned> dimensions;
	};

class PointerType
	: public Type {
		public:
		  // TODO: This needs to go
			PointerType ( ) {
			}

			PointerType ( Type *_baseType )
				: baseType ( _baseType ) {
				baseType->ref();
				}

			~PointerType() {
				baseType->unref();
			}

			Type *baseType;
			virtual void print ( ) const  {
				std::cout << "PointerType of" << std::endl;
				baseType->print();
			}
			
			virtual unsigned getSize() const
			{
				throw TaskException ( "Cannot get size of a PointerType" ); 
			}


	};

template<typename T, TaskType type = tg_typeInfo<T>::type> struct createType {
};

template<typename T> struct createType <T, typeInteger> {
	inline NumericType *operator ( ) ( coreTaskGraph *taskGraph ) {
		return createIntegerType ( taskGraph, tg_typeInfo<T>::size, tg_typeInfo<T>::isSigned );
	}
};

template<typename T> struct createType <T, typeFloat> {
	inline NumericType *operator ( ) ( coreTaskGraph *taskGraph ) {
		return createFloatType ( taskGraph, tg_typeInfo<T>::size );
	}
};

template<typename T> struct createType <T, typePointer> {
	inline PointerType *operator ( ) ( coreTaskGraph *taskGraph ) {
		return createPointerType ( taskGraph, createType < typename tg_typeInfo<T>::pointer_type >() ( taskGraph ) );
	}
};


template<typename T> struct createType <T, typeArray> {
	inline ArrayType *operator ( ) ( coreTaskGraph *taskGraph ) {
		unsigned dimensions [ tg_typeInfo<T>::_depth ];
		tg_typeInfo<T>::getList ( dimensions );
		return createArrayType ( taskGraph, createType< typename tg_typeInfo<T>::baseType > ( ) ( taskGraph ), tg_typeInfo<T>::_depth, dimensions );
	}
};

template<typename T> struct createType <T, typeStructure> {
	inline StructType *operator ( ) ( coreTaskGraph *taskGraph ) {
		return tg_typeInfo<T>::TaskStructure::getStaticType();
	}
};

}
#endif
