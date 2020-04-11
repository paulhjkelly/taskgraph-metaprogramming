#ifndef __WORLD_H
#define __WORLD_H

#include "Math3D.h"

class Object;
class Light;
class Ray;
class ColourRGB;

#include <vector>

#include "Camera.h"

class World {
public:
	World() {
	}

	~World();
	
	void addObject ( Object *newObj ) {
		objects.push_back ( newObj );
	}

	void addLight(Light *newLight) {
		lights.push_back ( newLight );
	}
	
	void begin ( );

	ColourRGB shootRay ( Ray *r, int depth = 1 );

	bool getRayIntersection(Ray *r, SCALAR *index, Object **closestObject);
	
	#ifdef SPECIALISE
	void SpecialiseGetRayIntersection ( );
	#endif

	Camera *getCamera() {
		return &camera;
	}

	void setCamera ( Camera *cam ) {
		camera = *cam;
	}
protected:
	Camera camera;
	std::vector<Light *> lights;
	std::vector<Object *> objects;
};

#endif
