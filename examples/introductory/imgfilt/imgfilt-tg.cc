/*
 * Image convolution matrix example for the TaskGraph.
 *
 * This example shows loop unrolling and specialisation
 * to the convolution matrices.
 */
#include <TaskGraph>
#include <iostream>
#include <stdlib.h>

using namespace tg;

#define IMG_SIZE	256*4
#define MSK_SIZE	8
#define NUM_MASKS	10
#define NUM_IMAGES	1

/*
 * Name    : filter
 * Function: Apply a mask to an image. We only filter the central portion
 *
 */

template<typename T>
void filter (float *mask, unsigned n, unsigned m, TaskArray<2,T> &input, TaskArray<2,T> &output, unsigned p, unsigned q)
{
	unsigned k, l;
	unsigned half_n = (n/2);
	unsigned half_m = (m/2);

	tVar (float, sum);
	tVar (int, i);
	tVar (int, j);

	tFor (i, half_n, p - half_n - 1) {
		tFor (j, half_m, q - half_m - 1) {
			sum = 0;

			// Loop unrolling
			for ( k = 0; k < n; ++k )
				for ( l = 0; l < m; ++l )
					sum += input[(i + k - half_n)][(j + l - half_m)] * mask[k * m + l];

			output[i][j] = sum;
		}
	}
}

typedef TaskGraph<void, float[IMG_SIZE][IMG_SIZE], float[IMG_SIZE][IMG_SIZE]> imgfilt_TaskGraph;

int main (int argc, char **argv)
{
	float (*im1)[IMG_SIZE] = new float[IMG_SIZE][IMG_SIZE];
	float (*im2)[IMG_SIZE] = new float[IMG_SIZE][IMG_SIZE];
	float (*msk)[MSK_SIZE] = new float[MSK_SIZE][MSK_SIZE];

	// Generate a random image
	srand48 (time (NULL));
	for ( unsigned i = 0; i < IMG_SIZE; i++)
		for ( unsigned j = 0; j < IMG_SIZE; j++)
			im1[i][j] = static_cast<float>(drand48 ());

	// Generate NUM_MASKS masks and filter im1 with them
	Timer timer;
	for ( unsigned n = 0; n < NUM_MASKS; ++n ) {
		imgfilt_TaskGraph T;

        // Generate a mask
		for ( unsigned i = 0; i < MSK_SIZE; i++)
			for ( unsigned j = 0; j < MSK_SIZE; j++)
				msk[i][j] = static_cast<float>(drand48 ());

		// Build a filter
		taskgraph (imgfilt_TaskGraph, T, tuple2(img1, img2)) {
			filter (&msk[0][0], MSK_SIZE, MSK_SIZE, img1, img2, IMG_SIZE, IMG_SIZE);
		}

		timer.stop();
		T.compile ( tg::GCC, true );
		timer.start();

		for ( unsigned m = 0; m < NUM_IMAGES; ++m )
			T.execute (im1, im2);
	}

	timer.stop();

	std::cout << "Filtering took " << timer.getTime() << " seconds.\n";
	theTimingInfo.print ( std::cout );

	delete[] im1;
	delete[] im2;
	delete[] msk;
}

