#include <math.h>

#ifdef SPECIALISE
#include <TaskGraph>
#include <TaskVector.h>
#include "VectorOp.h"
#endif

#include "Intersection.h"
#include "Ray.h"
#include "Sphere.h"

// Returns the first interseection of a ray with a sphere
bool Intersection::getFirstPositiveRaySphere(Ray *r, Sphere *s, SCALAR *par) {
	Vector3D deltaPos = r->getPosition() - s->getCenter();
	SCALAR firstPart = Vector3D::dotProduct(r->getDirection(), deltaPos);
	SCALAR plusMinusSquared = firstPart*firstPart - deltaPos.squareMagnitude() + s->getRadius()*s->getRadius();
	if (plusMinusSquared < 0)
		return false;

	SCALAR secondPart = sqrt(plusMinusSquared);
	SCALAR s1, s2;

	s1 = -firstPart - secondPart;
	s2 = -firstPart + secondPart;
	if (s1 >= 0) {
		*par = s1;
	} else if (s2 >= 0) {
		*par = s2;
	} else {
	// both are negative
		return false;
	}
	return true;
}

#ifdef SPECIALISE

void SpecialisedIntersection::SpecialisegetFirstPositiveRaySphere ( Sphere *s, tg::tVector<SCALAR,3> &rayPos, tg::tVector<SCALAR,3> &rayDir, tg::TaskScalarVariable &res, tg::TaskScalarVariable &par) {
	tg::tVector<SCALAR,3> deltaPos = rayPos - s->getCenter();

	tVar ( SCALAR, firstPart );
	tVar ( SCALAR, plusMinusSquared );

	firstPart = dotProduct ( rayDir, deltaPos );
	plusMinusSquared = firstPart*firstPart - deltaPos.squareMagnitude() + s->getRadius()*s->getRadius();
	tIf (plusMinusSquared < 0) {
		res = false;
	} tElse {
		tVar ( SCALAR, secondPart );
		tVar ( SCALAR, s1 );
		tVar ( SCALAR, s2 );
		secondPart = tSqrt ( plusMinusSquared );

		s1 = -firstPart - secondPart;
		s2 = -firstPart + secondPart;
		res = true;
		tIf (s1 >= 0) {
			par = s1;
		} tElse { tIf (s2 >= 0) {
			par = s2;
		} tElse {
		// both are negative
			res = false;
		}
		}
	}
}

void SpecialisedIntersection::getFirstPositiveRaySphere(Sphere *s, intersection_TaskGraph &t ) {
	taskgraph ( intersection_TaskGraph, t, tuple4 ( res, par, aRayDir, aRayPos )  ) {

	tg::tVector<SCALAR,3> rayDir(aRayDir);
	tg::tVector<SCALAR,3> rayPos(aRayPos);

	tg::tVector<SCALAR,3> deltaPos = rayPos - s->getCenter();

	tVar ( SCALAR, firstPart );
	tVar ( SCALAR, plusMinusSquared );

	firstPart = dotProduct ( rayDir, deltaPos );
	plusMinusSquared = firstPart*firstPart - deltaPos.squareMagnitude() + s->getRadius()*s->getRadius();
	tIf (plusMinusSquared < 0) {
		res = false;
	} tElse {
		tVar ( SCALAR, secondPart );
		tVar ( SCALAR, s1 );
		tVar ( SCALAR, s2 );
		secondPart = tSqrt ( plusMinusSquared );

		s1 = -firstPart - secondPart;
		s2 = -firstPart + secondPart;
		res = true;
		tIf (s1 >= 0) {
			par = s1;
		} tElse { tIf (s2 >= 0) {
			par = s2;
		} tElse {
		// both are negative
			res = false;
		}
		}
	}
	}
}

#endif /* SPECIALISE */

