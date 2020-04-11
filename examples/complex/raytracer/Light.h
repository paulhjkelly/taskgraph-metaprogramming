#ifndef __LIGHT_H
#define __LIGHT_H

#include "Math3D.h"
#include "Vector3D.h"
#include "Colour.h"

class World;
class Object;

class Light {
public:
	Light ( const Vector3D &pos ) : position ( pos ) {
		setColour ( ColourRGB ( 1.0 ) );
	}

	~Light() {
	}

	Vector3D &getPosition() {
		return position;
	}

	void setColour(const ColourRGB &col) {
		colour = col;
	}

	const ColourRGB &getColour() {
		return colour;
	}

	bool getColourAtPoint(World *world, const Object *obj, const Vector3D &pos, const Vector3D &normal, const Vector3D &rayDir, ColourRGB &lightColour);
protected:
	Vector3D position;
	ColourRGB colour;
};

#endif
