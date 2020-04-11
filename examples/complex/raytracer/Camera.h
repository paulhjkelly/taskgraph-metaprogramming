#ifndef __CAMERA_H
#define __CAMERA_H

#include "Vector3D.h"

class Camera {
public:
	Camera()
	 :  right ( 4.0 / 3.0, 0.0, 0.0),
		up(0.0, 1.0, 0.0),
		forward(0.0, 0.0, 1.0),
		position(0.0, 0.0, 0.0) {
	}

	void setLocation ( const Vector3D &loc ) {
		position = loc;
	}
public:
	Vector3D right, up, forward, position;
};

#endif
