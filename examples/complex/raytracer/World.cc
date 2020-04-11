#include "World.h"
#include "Sphere.h"
#include "Light.h"
#include "Ray.h"
#include "Intersection.h"
#include "Finish.h"
#include <cmath>
#include <cstddef>

using namespace std;

namespace {
	template<class T>void clipToNormalRange(T &v) {
		if (v > static_cast<T>(1.0))
			v = static_cast<T>(1.0);
		else if (v < static_cast<T>(0.0))
			v = static_cast<T>(0.0);
	}

	void clipColourRGBToNormalRange(ColourRGB &c) {
		clipToNormalRange(c.r);
		clipToNormalRange(c.g);
		clipToNormalRange(c.b);
	}
}

World::~World() {
	unsigned int a;

	for (a = 0; a < objects.size(); a++) {
		delete objects[a];
	}
	for (a = 0; a < lights.size(); a++) {
		delete lights[a];
	}
}

#ifndef SPECIALISE
bool World::getRayIntersection(Ray *r, SCALAR *index, Object **closestObject) {
	*closestObject = NULL;
	SCALAR closestPar = 10000000.0;

	// Iterate over objects
	for (unsigned int a = 0; a < objects.size(); a++) {
		SCALAR par;
		if (objects[a]->getFirstRayIntersection(r, &par)) {
			if (par < closestPar) {
				closestPar = par;
				*closestObject = objects[a];
			}
		}
	}
	*index = closestPar;
	return *closestObject != NULL;
}

#else
typedef tg::TaskGraph<void, int, SCALAR[3], SCALAR[3], SCALAR> world_TaskGraph;
world_TaskGraph t;

bool World::getRayIntersection(Ray *r, SCALAR *index, Object **closestObject) {
	int closest = -1;
	SCALAR d[3], p[3];
	d[0] = r->getDirection()[0];
	d[1] = r->getDirection()[1];
	d[2] = r->getDirection()[2];
	p[0] = r->getPosition()[0];
	p[1] = r->getPosition()[1];
	p[2] = r->getPosition()[2];
	t.execute ( closest, d, p, *index );
	if ( closest == -1 ) {
		return false;
	} else {
		*closestObject = objects[closest];
		return true;
	}
}

void World::SpecialiseGetRayIntersection ( ) {
	taskgraph ( world_TaskGraph, t, tuple4(closestObject, aRayDir, aRayPos, closestPar) ) {

	tg::tVector<SCALAR,3> rayDir(aRayDir);
	tg::tVector<SCALAR,3> rayPos(aRayPos);

	tVar ( SCALAR, par );
	tVar ( SCALAR, res );
	closestPar = 10000000.0;

	// Iterate over objects
	for ( unsigned int a = 0; a < objects.size(); ++a ) {

		objects[a]->SpecialisegetFirstRayIntersection ( rayDir, rayPos, res, par );
		tIf ( res ) {
			tIf (par < closestPar) {
				closestPar = par;
				closestObject = a;
			}
		}

	}
	}
}
#endif

int maxDepth = 5;

#ifndef SPECIALISE
void World::begin ( ) {
}
#else
void World::begin ( ) {
//	TaskGraphGroup group;
//	for (unsigned a = 0; a < objects.size(); a++) {
//		group.add ( &objects[a]->getTaskGraph ( ) );
//	}
	SpecialiseGetRayIntersection ( );
	t.compile ( );
//	group.compile ( );
}
#endif

ColourRGB World::shootRay(Ray *r, int depth) {
	ColourRGB colour(0.0);
	SCALAR rayHit;
	Object *closestObject;

	if (getRayIntersection(r, &rayHit, &closestObject)) {
		colour = closestObject->finish->ambientColour;
		Vector3D pos = r->getPosition() + rayHit * r->getDirection();
		Vector3D normal = closestObject->getNormalAtPosition(&pos);

		// Iterate over lights
		for (unsigned int a = 0; a < lights.size(); a++) {
			ColourRGB lcolour;

			if (lights[a]->getColourAtPoint(this, closestObject, pos, normal, r->getDirection(), lcolour)) {
				colour += lcolour;
			}
		}
		if (closestObject->finish->useReflection) {
			if (depth < maxDepth) {
				Vector3D reflection = r->getDirection() - ((2.0 * Vector3D::dotProduct(r->getDirection(), normal)) * normal);
				Ray reflectedRay(pos + reflection * SCALAR_SMALL_DELTA, reflection);
				colour += shootRay(&reflectedRay, depth + 1) * closestObject->finish->reflectionColour;
			}
		}
		colour = closestObject->getColourAtPoint(&pos).makeRGB() * colour;
	}
	clipColourRGBToNormalRange(colour);
	return colour;
}

