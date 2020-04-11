#ifndef __RAY_H
#define __RAY_H

#include "Vector3D.h"

class Ray {
public:
	Ray ( const Vector3D &pos, const Vector3D &dir ) : position ( pos ), direction ( dir ) {
	}

	const Vector3D &getPosition() {
		return position;
	}

	const Vector3D &getDirection() {
		return direction;
	}

protected:
	Vector3D position, direction;
};

#endif
