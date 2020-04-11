#include <stdio.h>
#include <math.h>
#include "Vector3D.h"
#include "Math3D.h"

// Create a blank vecotr
Vector3D::Vector3D() {
}

// Create a vector with the following componentes
Vector3D::Vector3D(SCALAR cx, SCALAR cy, SCALAR cz) {
	x = cx;
	y = cy;
	z = cz;
}

// Get a component
SCALAR Vector3D::get(int index) const {
	return (&x)[index];
}

// Set a component
void Vector3D::set(int index, SCALAR value){
	(&x)[index] = value;
}

// Set all components
void Vector3D::set(SCALAR nx, SCALAR ny, SCALAR nz){
	x = nx;
	y = ny;
	z = nz;
}

void Vector3D::set(const Vector3D &vec) {
	x = vec.x;
	y = vec.y;
	z = vec.z;
}

// Add two vectors together
Vector3D Vector3D::add(const Vector3D &a, const Vector3D &b) {
	return Vector3D(a.x + b.x, a.y + b.y, a.z + b.z);
}

// Subtract two vectors
Vector3D Vector3D::sub(const Vector3D &a, const Vector3D &b) {
	return Vector3D(a.x - b.x, a.y - b.y, a.z - b.z);
}

// Subtract two vectors
Vector3D Vector3D::scale(const Vector3D &a, SCALAR factor) {
	return Vector3D(a.x * factor, a.y * factor, a.z * factor);
}

// Add a vector to this vector
void Vector3D::add(const Vector3D &a) {
	x += a.x;
	y += a.y;
	z += a.z;
}

// Subtract a vector to this vector
void Vector3D::sub(const Vector3D &a) {
	x -= a.x;
	y -= a.y;
	z -= a.z;
}

// Scale this vectors
void Vector3D::scale(SCALAR factor) {
	x *= factor;
	y *= factor;
	z *= factor;
}

// Negation
void Vector3D::negate() {
	x = -x;
	y = -y;
	z = -z;
}

// Calculate the square magnitude of a vector
SCALAR Vector3D::squareMagnitude() const {
	return (x * x + y * y + z * z);
}

// Calculate the magnitude
SCALAR Vector3D::magnitude() const {
	return static_cast<SCALAR>(sqrt(squareMagnitude()));
}

// Normalize the vector
void Vector3D::normalise() {
	SCALAR val;

	val = magnitude();
	x /= val;
	y /= val;
	z /= val;
}

// Return the dot product
SCALAR Vector3D::dotProduct(const Vector3D &v1, const Vector3D &v2) {
	return (v1.x * v2.x + v1.y * v2.y + v1.z * v2.z);
}

// Return the cross product
Vector3D Vector3D::crossProduct(const Vector3D &v1, const Vector3D &v2) {
    Vector3D result;

    result.x = v1.y * v2.z - v1.z * v2.y;
    result.y = v1.z * v2.x - v1.x * v2.z;
    result.z = v1.x * v2.y - v1.y * v2.x;

    return result;
}


char *Vector3D::toString(char *buffer) {
	sprintf(buffer, "<%f, %f, %f> length = %f\n", x, y, z, magnitude());
	return buffer;
}

// Operators
// Get vector value
const SCALAR& Vector3D::operator[](int index) const {
    return (&x)[index];
}

// Set vector value
SCALAR& Vector3D::operator[](int index) {
    return (&x)[index];
}

void Vector3D::operator=(const Vector3D &v) {
	set(v);
}

void Vector3D::operator+=(const Vector3D &v) {
	add(v);
}

void Vector3D::operator-=(const Vector3D &v) {
	sub(v);
}

void Vector3D::operator*=(const SCALAR val) {
	scale(val);
}

void Vector3D::operator/=(const SCALAR val) {
	scale(static_cast<SCALAR>(1.0)/val);
}

Vector3D operator+(const Vector3D &v1, const Vector3D &v2) {
	return Vector3D::add(v1, v2);
}

Vector3D operator-(const Vector3D &v1, const Vector3D &v2) {
	return Vector3D::sub(v1, v2);
}

Vector3D operator*(const SCALAR val, const Vector3D &v) {
	return Vector3D::scale(v, val);
}

Vector3D operator*(const Vector3D &v, SCALAR val) {
	return Vector3D::scale(v, val);
}

Vector3D operator/(const Vector3D &v, SCALAR val) {
	return Vector3D::scale(v, static_cast<SCALAR>(1.0)/val);
}

Vector3D operator+(const Vector3D &v) {
	return v;
}

Vector3D operator-(const Vector3D &v) {
	return Vector3D(-v.x, -v.y, -v.z);
}
