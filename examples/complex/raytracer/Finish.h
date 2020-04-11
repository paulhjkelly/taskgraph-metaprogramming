#ifndef __FINISH_H
#define __FINISH_H

#include "Math3D.h"
#include "Colour.h"

class Finish {
public:
	Finish();
// Create the default finish
	Finish(bool unused);
public:
	ColourRGB ambientColour;
	ColourRGB reflectionColour;
	bool useReflection;

	bool useSpecular, usePhong, useBrilliance;
	SCALAR diffuse;
	SCALAR specular, inverseRoughness;	// Inverse is more useful in calculations
	SCALAR brilliance;
	SCALAR phong, phongSize;
	
	static Finish *defaultFinish;
};

#endif

