#ifndef __OBJECT_H
#define __OBJECT_H

#include "Math3D.h"
#include "Colour.h"
#include "Finish.h"

class Vector3D;
class Ray;

#ifdef SPECIALISE
#include <TaskGraph>
#include <TaskVector.h>
#endif

class Object {
public:
	Object() :
		finish ( new Finish() ),
		colour ( 1.0, 1.0, 1.0 ) {
	}

	virtual ~Object() {
		delete finish;
	}

	virtual Vector3D getNormalAtPosition(const Vector3D *pos) = 0;
	virtual bool getFirstRayIntersection(Ray *r, SCALAR *par) = 0;

	#ifdef SPECIALISE
//	virtual TaskGraph &getTaskGraph ( ) = 0;
	virtual void SpecialisegetFirstRayIntersection(tg::tVector<SCALAR,3> &rayPos, tg::tVector<SCALAR,3> &rayDir, tg::TaskScalarVariable &r, tg::TaskScalarVariable &par) = 0;
	#endif

	ColourRGBFT getColourAtPoint(const Vector3D *pos) {
		return colour;
	}
public:
	Finish *finish;
	ColourRGBFT colour;
};

#endif
