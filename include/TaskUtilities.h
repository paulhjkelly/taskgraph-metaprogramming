#ifndef TASKUTILITIES_H__
#define TASKUTILITIES_H__

#include "TaskDefines.h"
#include <utility>
#include <functional>

namespace tg {
	template<typename T>
	struct select1st : public std::unary_function<T, typename T::first_type> {
		const typename T::first_type &operator() ( const T &p ) const {
			return p.first;
		}
	};

	template<typename T>
	struct select2nd : public std::unary_function<T, typename T::first_type> {
		const typename T::second_type &operator() ( const T &p ) const {
			return p.second;
		}
	};

	template<typename T, typename T2, typename Obj>
	struct pairless : std::binary_function<T2, T2, bool> {
		bool operator() ( const T2& x, const T2& y ) const {
			return std::less<T>()( Obj() ( x ), Obj() ( y ) );
		}
	};

	template<typename T>
	struct pair_first_equal : std::binary_function<T, typename T::first_type, bool> {
		bool operator() ( const T& x, const typename T::first_type y ) const {
			return std::equal_to<typename T::first_type>()( x.first, y );
		}
	};
	
	template<typename T>
	T prev ( T iter ) {
		return --iter;
	}

}

#define COMPILE_TIME_ERROR(x) int array[-1], x;

#endif

