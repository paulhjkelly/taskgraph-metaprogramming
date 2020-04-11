#ifndef __VECTOR3D_H
#define __VECTOR3D_H

#include "Math3D.h"

class Vector3D {
public:
	SCALAR x, y, z;
public:
	// Constructors
	Vector3D();
	Vector3D(SCALAR cx, SCALAR cy, SCALAR cz);
	
	SCALAR get(int index) const;
	void set(SCALAR nx, SCALAR ny, SCALAR nz);
	void set(const Vector3D &vec);
	void set(int index, SCALAR value);
	
	void add(const Vector3D &a);
	void sub(const Vector3D &a);
	void scale(SCALAR factor);
	void negate();

	static Vector3D add(const Vector3D &a, const Vector3D &b);
	static Vector3D sub(const Vector3D &a, const Vector3D &b);
	static Vector3D scale(const Vector3D &a, SCALAR factor);
	
	const SCALAR& operator[](int index) const;
	SCALAR& operator[](int index);

	void operator+=(const Vector3D &v);
	void operator-=(const Vector3D &v);
	void operator*=(const SCALAR val);
	void operator/=(const SCALAR val);
	void operator=(const Vector3D &v);

	static SCALAR dotProduct(const Vector3D &v1, const Vector3D &v2);
	static Vector3D crossProduct(const Vector3D &v1, const Vector3D &v2);

	// Functions
	void normalise();
	SCALAR squareMagnitude() const;
	SCALAR magnitude() const;

	// For debugging
	char *toString(char *buffer);
};

Vector3D operator+(const Vector3D &v1, const Vector3D &v2);
Vector3D operator-(const Vector3D &v1, const Vector3D &v2);

Vector3D operator/(const Vector3D &v, SCALAR val);
Vector3D operator*(const Vector3D &v, SCALAR val);
Vector3D operator*(const SCALAR val, const Vector3D &v);
	
Vector3D operator-(const Vector3D &v);
Vector3D operator+(const Vector3D &v);

typedef Vector3D Vector;
typedef Vector3D Point3D;
typedef Point3D Point;

#endif
