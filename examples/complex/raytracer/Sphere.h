#ifndef __SPHERE_H
#define __SPHERE_H

#include "Vector3D.h"
#include "Object.h"
#include "Sphere.h"
#include "Intersection.h"
#include "Ray.h"

#ifdef SPECIALISE
#include <TaskGraph>
#endif

class Sphere
	: public Object {
public:
	Sphere ( Vector3D _center, SCALAR _radius ) :
		center ( _center ),
		radius ( _radius ) {
	}
	virtual ~Sphere() {
	}

	SCALAR getRadius() {
        return radius;
	}

	Vector3D getCenter() {
		return center;
	}

	virtual Vector3D getNormalAtPosition(const Vector3D *pos) {
		Vector3D normal = *pos - center;
		normal.normalise();
		return normal;
	}

	//#ifndef SPECIALISE
	virtual bool getFirstRayIntersection(Ray *r, SCALAR *par) {
        return Intersection::getFirstPositiveRaySphere(r, this, par);
	}
	//#else
/*	virtual bool getFirstRayIntersection(Ray *r, SCALAR *par) {
		*parBind = par;
		*rayDirBind = (void *)&r->getDirection();
		*rayPosBind = (void *)&r->getPosition();
		t.execute ( );
        return res;
	}

	virtual TaskGraph &getTaskGraph ( ) {
		SpecialisedIntersection::getFirstPositiveRaySphere ( this, t );
		t.setParameter ( "res", &res );
		parBind = t.getParameterBinding ( "par" );
		rayDirBind = t.getParameterBinding ( "rayDir" );
		rayPosBind = t.getParameterBinding ( "rayPos" );
		return t;
	}
	TaskGraph t;
	int res;
	void **rayDirBind, **rayPosBind, **parBind;
	#endif*/
	#ifdef SPECIALISE
	virtual void SpecialisegetFirstRayIntersection(tg::tVector<SCALAR,3> &rayPos, tg::tVector<SCALAR,3> &rayDir, tg::TaskScalarVariable &res, tg::TaskScalarVariable &par) {
		SpecialisedIntersection::SpecialisegetFirstPositiveRaySphere ( this, rayDir, rayPos, res, par );
	}
	#endif

protected:
	Vector3D center;
	SCALAR radius;
};

#endif
