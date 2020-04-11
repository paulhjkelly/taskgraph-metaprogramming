#include <time.h>
#include <sys/time.h>
#include <stdlib.h>
#include <iostream>

#define IMG_SIZE	256*4
#define MSK_SIZE	8
#define NUM_MASKS	10
#define NUM_IMAGES	1

/*
 * Name    : filter
 * Function: Apply a mask to an image. We only filter the central portion
 *
 */

void filter (float *mask, unsigned n, unsigned m, const float *input, float *output, unsigned p, unsigned q)
{
	float    sum;
	int half_n = (n/2);
	int half_m = (m/2);

	for (unsigned i = half_n; i < p - half_n; i++) {
		for (unsigned j = half_m; j < q - half_m; j++) {
			sum = 0;

			for (unsigned k = 0; k < n; k++)
				for (unsigned l = 0; l < m; l++)
					sum += input[(i + k - half_n) * q + (j + l - half_m)] * mask[k * m + l];

			output[i * q + j] = sum;
		}
	}
}

double utime () {
  struct timeval tv;

  gettimeofday (&tv, NULL);

  return (tv.tv_sec + double (tv.tv_usec) * 1e-6);
}

/*
 * Name    : main
 * Function: Filter an image using a mask.
 *
 */

int main (int argc, char **argv) {
	float (*im1)[IMG_SIZE] = new float[IMG_SIZE][IMG_SIZE];
	float (*im2)[IMG_SIZE] = new float[IMG_SIZE][IMG_SIZE];
	float (*msk)[MSK_SIZE] = new float[MSK_SIZE][MSK_SIZE];
	double t;

	// Generate a random image
	srand48 (time (NULL));
	for (unsigned i = 0; i < IMG_SIZE; i++)
		for (unsigned j = 0; j < IMG_SIZE; j++)
			im1[i][j] = static_cast<float>(drand48 ());


	// Generate NUM_MASKS masks and filter im1 with them
	t = -utime ();

	for (unsigned n = 0; n < NUM_MASKS; n++) {
		// Generate a mask
		for (unsigned i = 0; i < MSK_SIZE; i++)
			for (unsigned j = 0; j < MSK_SIZE; j++)
				msk[i][j] = static_cast<float>(drand48 ());

		// Filter the image
		for (unsigned m = 0; m < NUM_IMAGES; m++)
			filter ((float *) msk, MSK_SIZE, MSK_SIZE, (float *) im1, (float *) im2, IMG_SIZE, IMG_SIZE);
    }

	t += utime ();

	std::cout << "Filtering took " << t << " seconds.\n";

	delete[] im1;
	delete[] im2;
	delete[] msk;
}
