#include <math.h>
#include "Light.h"
#include "Colour.h"
#include "Vector3D.h"
#include "World.h"
#include "Object.h"
#include "Ray.h"
#include "Finish.h"


bool Light::getColourAtPoint(World *world, const Object *obj, const Vector3D &pos, const Vector3D &normal, const Vector3D &rayDir, ColourRGB &lightColour) {
	Object *closestObject;
	SCALAR rayHit;

	Vector3D lightDir = getPosition() - pos;
	lightDir.normalise();

	Ray lightRay(getPosition(), -lightDir);
	world->getRayIntersection(&lightRay, &rayHit, &closestObject);
//	if (rayHit*rayHit < squareDistance - SCALAR_COMP_DELTA) {
//		return false;
//	}
	if ( obj != closestObject ) {
		return false;
	}
	SCALAR intensity = 0.0;

// Diffuse
	intensity = Vector3D::dotProduct(normal, lightDir);
	if (obj->finish->useBrilliance) {
		intensity = pow(intensity, obj->finish->brilliance);
	}
	intensity *= obj->finish->diffuse;

// Phong Highlights
	if (obj->finish->usePhong) {
		Vector3D reflection = lightDir - (2 * (Vector3D::dotProduct(lightDir, normal)) * normal);
		SCALAR phong = Vector3D::dotProduct(rayDir, reflection);
		if (phong > static_cast<SCALAR>(0))
			intensity += obj->finish->phong * pow(phong, obj->finish->phongSize);
	}

// Specular Highlights
	if (obj->finish->useSpecular) {
		Vector3D halfway = lightDir - rayDir;
		halfway.normalise();

		SCALAR specular = Vector3D::dotProduct(halfway, normal);
		if (specular > static_cast<SCALAR>(0))
			intensity += obj->finish->specular * pow(specular, obj->finish->inverseRoughness);
	}

	lightColour = getColour() * (intensity/*/sqrt(squareDistance)*/);

	return true;
}
