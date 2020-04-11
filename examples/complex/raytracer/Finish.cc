#include "Finish.h"

// TODO: Add a way to clean this up
Finish *Finish::defaultFinish = new Finish(true);
namespace {
	struct thecleaner {
		~thecleaner() {
			delete Finish::defaultFinish;
			Finish::defaultFinish = 0;
		}
	} theoneandonly;
}

Finish::Finish() {
	*this = *defaultFinish;
}

// Create the default finish
// bool to identify the write constructor
Finish::Finish(bool unused) {
	useReflection = false;

	ambientColour = ColourRGB(0.1, 0.1, 0.1);

	diffuse = 0.6;
	brilliance = 1.0;
	useBrilliance = false; // 1.0 is assumed if not used

	useSpecular = false;
	specular = 0.0;
	inverseRoughness = 1.0/0.05;

	usePhong = false;
	phong = 0.0;
	phongSize = 40.0;
}
