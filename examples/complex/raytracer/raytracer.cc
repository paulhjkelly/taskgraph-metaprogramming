#include <stdio.h>
#include <stdlib.h>
#include <sys/time.h>
#include <math.h>

#include "Vector3D.h"
#include "World.h"
#include "Ray.h"
#include "Colour.h"
#include "Sphere.h"
#include "Light.h"

void doScene ( World &world );
void outputPPM ( const char *filename );

#define HEIGHT 480*2
#define WIDTH 640*2

unsigned char screen[HEIGHT][WIDTH][3];


double utime () {
  struct timeval tv;

  gettimeofday (&tv, NULL);

  return (tv.tv_sec + double (tv.tv_usec) * 1e-6);
}


int main ( ) {
	World world;
	Light *lightA = new Light ( Vector3D ( -2.0, -3.0, 0.0 ) );
	lightA->setColour(ColourRGB(0.7));
	world.addLight ( lightA );
	Light *lightB = new Light ( Vector3D ( 2.0, -3.0, 0.0 ) );
	lightB->setColour(ColourRGB(0.7));
	world.addLight ( lightB );
	int type = 0;

	for ( float x = -2.0; x < 2.0; x += 0.4 ) {
		for ( float y = -2.0; y < 2.0; y += 0.4 ) {
			Sphere *sphere = new Sphere( Vector3D ( x, y, 5.0 - fabs(x)+fabs(y) ), 0.2 );
			sphere->finish->useReflection = true;
			switch ( type ) {
			case 0:
				sphere->colour = ColourRGBFT ( 1.0, 0.5, 0.5, 0.0, 0.0 );
				sphere->finish->reflectionColour = ColourRGB ( 0.5 );
				break;
			case 1:
				sphere->colour = ColourRGBFT ( 0.5, 1.0, 0.5, 0.0, 0.0 );
				sphere->finish->reflectionColour = ColourRGB ( 0.2 );
				sphere->finish->usePhong = true;
				sphere->finish->phong = 0.2;
				sphere->finish->phongSize = 40.0;
				break;
			case 2:
				sphere->colour = ColourRGBFT ( 0.5, 0.5, 1.0, 0.0, 0.0 );
				sphere->finish->reflectionColour = ColourRGB ( 1.0 );
				break;
			case 3:
				sphere->colour = ColourRGBFT ( 0.1, 0.1, 1.0, 0.0, 1.0 );
				sphere->finish->useReflection = false;
				sphere->finish->useSpecular = true;
				sphere->finish->specular = 0.2;
				break;
			};
			if ( ++type == 4 )
				type = 0;
			world.addObject ( sphere );
		}
	}

	double t = -utime ();
	world.begin ( );

	t += utime ();
	printf ( "Init took: %f seconds\n", t );

	doScene ( world );
	outputPPM ( "test.ppm" );
}

void outputPPM ( const char *filename ) {
	FILE *out = fopen ( filename, "wb" );
	if ( out == 0 ) {
		printf ( "Error opening file\n" );
		exit ( 1 );
	}
	fputs ( "P3\n", out );
	fprintf ( out, "%u %u\n", WIDTH, HEIGHT );
	fputs ( "255\n", out );
	for ( unsigned y = 0; y < HEIGHT; ++y ) {
		for ( unsigned x = 0; x < WIDTH; ++x ) {
			fprintf ( out, "%u %u %u\n", screen[y][x][0], screen[y][x][1], screen[y][x][2] );
		}
	}
	fclose ( out );
}

void doScene ( World &world ) {
	double height = HEIGHT;
	double width = WIDTH;
	double halfHeight = height / 2.0;
	double halfWidth = width / 2.0;

	ColourRGB colour;

	double t = -utime ();

	Camera cam = *world.getCamera();
	for ( unsigned y = 0; y < HEIGHT; ++y ) {
		double ry =  ( y - halfHeight ) / height;
		for ( unsigned x = 0; x < WIDTH; ++x ) {
			double rx = ( x - halfWidth ) / width;

			Vector3D dup = cam.up * ry;
			Vector3D dright = cam.right * rx;
			Vector3D rayDir = cam.forward + dright + dup;
			rayDir.normalise();
			Ray r ( cam.position, rayDir );

			colour = world.shootRay ( &r );

			screen[y][x][0] = static_cast<unsigned char>(255 * colour.r);
			screen[y][x][1] = static_cast<unsigned char>(255 * colour.g);
			screen[y][x][2] = static_cast<unsigned char>(255 * colour.b);
		}
	}
	t += utime ();
	printf ( "Rendering took: %f seconds\n", t );
}

