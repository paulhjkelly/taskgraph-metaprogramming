#ifndef __INTERSECTION_H
#define __INTERSECTION_H

#include "Math3D.h"

class Ray;
class Sphere;

#ifdef SPECIALISE
#include <TaskGraph>
#include <TaskVector.h>
#endif

class Intersection {
public:
	static bool getFirstPositiveRaySphere(Ray *r, Sphere *s, SCALAR *par);
};

#ifdef SPECIALISE

typedef tg::TaskGraph<void, int, SCALAR, SCALAR[3], SCALAR[3] > intersection_TaskGraph;

class SpecialisedIntersection {
public:
	static void getFirstPositiveRaySphere(Sphere *s, intersection_TaskGraph &t );
	static void SpecialisegetFirstPositiveRaySphere ( Sphere *s, tg::tVector<SCALAR,3> &rayPos, tg::tVector<SCALAR,3> &rayDir, tg::TaskScalarVariable &res, tg::TaskScalarVariable &par);
};

#endif

#endif
