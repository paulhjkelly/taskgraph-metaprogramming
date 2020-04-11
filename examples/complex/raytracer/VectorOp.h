#ifndef VECTOROPS_H__
#define VECTOROPS_H__

#ifdef SPECIALISE
#include <TaskGraph>
#include <TaskVector.h>
#endif

#include "Vector3D.h"

template<typename T, bool TYPE>
inline tg::tVector<T,3,false> operator+ (const tg::tVector<T,3,TYPE> &lhs, const Vector3D &rhs ) {
	tg::tVector<T,3,false> temp;
	for ( unsigned a = 0; a < 3; ++a )
		temp[a] = lhs[a] + rhs[a];
	return temp;
}

template<typename T, bool TYPE>
inline tg::tVector<T,3,false> operator+ ( const Vector3D &lhs, const tg::tVector<T,3,TYPE> &rhs ) {
	return rhs + lhs;
}

template<typename T, bool TYPE>
inline tg::tVector<T,3,false> operator- (const tg::tVector<T,3,TYPE> &lhs, const Vector3D &rhs ) {
	tg::tVector<T,3,false> temp;
	for ( unsigned a = 0; a < 3; ++a )
		temp[a] = lhs[a] - rhs[a];
	return temp;
}

template<typename T, bool TYPE>
inline tg::tVector<T,3,false> operator- ( const Vector3D &lhs, const tg::tVector<T,3,TYPE> &rhs ) {
	return rhs - lhs;
}

inline tg::tVector<SCALAR,3,false> operator* ( const Vector3D &lhs, const tg::TaskVariable &rhs ) {
	tg::tVector<SCALAR,3,false> temp;
	for ( unsigned a = 0; a < 3; ++a )
		temp[a] = lhs[a] * rhs;
	return temp;
}

inline tg::tVector<SCALAR,3,false> toVector ( const Vector3D &vec ) {
	tg::tVector<SCALAR,3,false> temp;
	for ( unsigned a = 0; a < 3; ++a )
		temp[a] = vec[a];
	return temp;
}
/*
inline tVector<SCALAR,3,false> toVector ( const ColourRGB &col ) {
	tVector<SCALAR,3,false> temp;
	temp[0] = col.r;
	temp[1] = col.g;
	temp[2] = col.b;
	return temp;
}

inline tVector<SCALAR,3,false> toVector ( const ColourRGBFT &col ) {
	tVector<SCALAR,3,false> temp;
	temp[0] = col.r;
	temp[1] = col.g;
	temp[2] = col.b;
	return temp;
}
*/
#endif
