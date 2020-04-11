/*
 * This program demonstrates the use of the TaskGraph library in constructing
 * (dynamically, at run-time) radix-2 Cooley-Tukey FFT algorithms.
 *
 * We do make some minor assumptions - in particular that "unsigned" is a
 * 32-bit unsigned integer. If it isn't, then you might have to change the
 * program to make it work.

 * Note that this program itself *isn't* intended to be especially useful; it
 * is only suitable for generating small FFTs of power-of-two size (it uses
 * the Cooley-Tukey algorithm and inlines *everything*). If you actually want
 * a practical FFT system, the author would recommend that you take a look
 * at FFTW - the "Fastest Fourier Transform in the West" - which is a rather
 * more sophisticated offering and is probably much better suited to whatever
 * you wanted to do.
 *
 * (Also, the author has no current intention of checking that his FFT
 * algorithm doesn't contain bugs - so please, don't use this in production
 * code *without checking it first*)
 *
 */

#include <TaskGraph>

#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <time.h>
#include <sys/time.h>

using namespace tg;

const double pi = 3.14159265358979323846;
const double eps = 1e-6;

const unsigned data_size = 10000;//1048576;

/*
 * Name    : nearly
 * Function: Test if two numbers are nearly equal.
 *
 */

bool nearly (double a, double b) {
  double t = a - b;

  if (t < 0)
    t = -t;

  return t < eps;
}

/*
 * Name    : W
 * Function: Generate the "twiddle factors"; the twiddle factor is defined
 *         : as:
 *         :                  W(k,N) = exp (2 * pi * k / N)
 *
 */

void W (unsigned k, unsigned N, double &re, double &im, bool inverse) {
  double angle = 2 * pi * k / N;

  re = cos (angle);
  im = inverse ? sin (angle) : -sin (angle);
}

/*
 * Name    : reverse32
 * Function: Reverse a 32-bit number. (I'm quite pleased with the way it
 *         : works, as I came up with it myself - but I'm sure someone else
 *         : has already thought of it... perhaps there's an even *faster*
 *         : way yet [aside from special processor instructions])
 *
 */

inline unsigned reverse32 (unsigned a) {
  // First reverse pairs of bits
  a = ((a & 0x55555555) <<  1) | ((a & 0xaaaaaaaa) >> 1 );

  // Now reverse the pairs in each nibble
  a = ((a & 0x33333333) <<  2) | ((a & 0xcccccccc) >> 2 );

  // Now do the nibbles in each byte
  a = ((a & 0x0f0f0f0f) <<  4) | ((a & 0xf0f0f0f0) >> 4 );

  /* Some CPUs have instructions that could be used instead
     of the next two lines of code. */

  // Now do the bytes in each word
  a = ((a & 0x00ff00ff) <<  8) | ((a & 0xff00ff00) >> 8 );

  // Finally, do the two words
  a = ((a & 0x0000ffff) << 16) | ((a & 0xffff0000) >> 16);

  return a;
}

/*
 * Name    : reverse
 * Function: Reverse an N-bit number
 *
 */

inline unsigned
reverse (unsigned a, unsigned N)
{
  return reverse32 (a) >> (32 - N);
}

/*
 * Name    : bits
 * Function: Count the number of bits in a number (i.e. find the highest
 *         : set bit). This is a cunning algorithm as well; however, I didn't
 *         : come up with it entirely on my own - it's used for a 12-bit case
 *         : in Linux's "random.c" device driver... but *they* don't
 *         : explain it, so I've annotated mine to show how it works.
 *
 */

inline unsigned
bits (unsigned a)
{
  // Smear the MSB out
  a |= a >> 16;
  a |= a >> 8;
  a |= a >> 4;
  a |= a >> 2;
  a |= a >> 1;

  // Now we have a mask; count the bits

  /* Add up pairs of bits. This operation should logically be

       a = ((a & 0xaaaaaaaaa) >> 1) + (a & 0x55555555);

     however, we can take a shortcut by observing that this
     operation just clears the lower bit of each pair (exception:
     it doesn't clear the MSB, if that bit is the lower bit
     of a pair */
  a ^= (a >> 1) & 0x55555555;

  // Add up pairs of pairs to form nybble-sized partial sums
  a = ((a & 0xcccccccc) >> 2) + (a & 0x33333333);

  /* Add up pairs of nybbles to form byte-sized partial sums
     (the mask is to stop carries from being generated in the
     next stage) */
  a = (a + (a >> 4)) & 0x0f0f0f0f;

  /* Now the cool thing about the next part is that since we only care
     about the bottom six bits of the result, carries no longer matter
     (so we don't need any masking) */

  // Add up pairs of bytes to form words
  a += a >> 8;

  // Add up the two words
  a += a >> 16;

  return a & 63;
}

/*
 * Name    : semiMultiply
 * Function: Generate half a complex multiplication. We make some effort
 *         : to optimise the expression at this point.
 *         : The expression
 *         :
 *         :       semiMultiply (var, x, y, xi, yi, plus, re, im)
 *         :
 *         : is the same as
 *         :
 *         :                 var = x[xi] * re + y[yi] * im
 *         :
 *         : if "plus" is true. However, semiMultiply will try to hoist
 *         : '-' signs out of constants; it also attempts to remove
 *         : superfluous multiplies, and can factor the expression if
 *         : "re" and "im" are approximately equal.
 *
 */

// Need some kind of persistant Expression that does a deep copy after use
void
semiMultiply (TaskScalarVariable	&ret,
	      const TaskExpression	&A,
	      const TaskExpression	&B,
	      bool		plus,
	      double		re,
	      double		im)
{
  bool combined = false;
  bool negre = false;

  // Check for the simple case of re = im = 0.
  if (nearly (re, 0.0) && nearly (im, 0.0))
    {
      ret = 0.0;
      return;
    }

  // Propagate '-' signs
  if (im < 0)
    {
      im = -im;
      plus = !plus;
    }

  if (re < 0)
    {
      re = -re;
      negre = true;
    }

  /* See if the real and imaginary components are the same; if so,
     we can combine the two multiplications. */
  if (nearly (re, im))
    combined = true;

  // If we have a real component
  if (!nearly (re, 0.0))
    {
      // And an imaginary component
      if (!nearly (im, 0.0))
	{
	  if (!combined)
	    {
	      if (plus)
		{
		  if (negre)
		    ret = (nearly (re, 1.0) ? -A : (-A * re))
		      + (nearly (im, 1.0) ? B : (B * im));
		  else
		    ret = (nearly (re, 1.0) ? A : (A * re))
		      + (nearly (im, 1.0) ? B : (B * im));
		}
	      else
		{
		  if (negre)
		    ret = (nearly (re, 1.0) ? -A : (-A * re))
		      - (nearly (im, 1.0) ? B : (B * im));
		  else
		    ret = (nearly (re, 1.0) ? A : (A * re))
		      - (nearly (im, 1.0) ? B : (B * im));
		}
	    }
	  else
	    {
	      if (nearly (im, 1.0) && nearly (re, 1.0))
		{
		  if (plus)
		    {
		      ret = (negre
			      ? B - A
			      : A + B);
		    }
		  else
		    {
		      ret = (negre
			      ? - (A + B)
			      : A - B);
		    }
		}
	      else
		{
		  if (plus)
		    {
		      ret = (negre
			      ? (B - A)
			      : (A + B)) * im;
		    }
		  else
		    {
		      ret = (negre
			      ? - (A + B)
			      : (A - B)) * im;
		    }
		}
	    }
	}
      else
	{
	  if (negre)
	    ret = (nearly (re, 1.0) ? -A : (-A * re));
	  else
	    ret = (nearly (re, 1.0) ? A : (A * re));
	}
    }
  else
    {
      if (plus)
	ret = (nearly (im, 1.0) ? B : (B * im));
      else
	ret = (nearly (im, 1.0) ? -B : (-B * im));
    }
}

/*
 * Name    : butterfly
 * Function: Generate a butterfly computation.
 *
 */

// Real case first
template<typename T>
void
butterfly (TaskScalarVariable		&tr,
	   TaskScalarVariable		&ti,
	   TaskArray<1,T>	&x,
	   unsigned i, unsigned j,
	   unsigned k, unsigned N,
	   bool inverse)
{
  double re, im;

  // Find the twiddle factor
  W (k, N, re, im, inverse);

  // Do the complex multiplication
  semiMultiply (tr, x[j * 2], x[j * 2 + 1], false, re, im);
  semiMultiply (ti, x[j * 2], x[j * 2 + 1], true, im, re);

  // Finish the butterfly
  x[j * 2] = x[i * 2] - tr;	    	x[i * 2] += tr;
  x[j * 2 + 1] = x[i * 2 + 1] - ti; 	x[i * 2 + 1] += ti;
}

// Complex case
template<typename T>
void
butterfly (TaskScalarVariable		&tr,
	   TaskScalarVariable		&ti,
	   TaskArray<1,T>	&xr,
	   TaskArray<1,T> &xi,
	   unsigned i, unsigned j,
	   unsigned k, unsigned N,
	   bool inverse)
{
  double re, im;

  // Find the twiddle factor
  W (k, N, re, im, inverse);

  // Do the complex multiplication
  semiMultiply (tr, xr[j], xi[j], false, re, im);
  semiMultiply (ti, xr[j], xi[j], true, im, re);

  // Finish the butterfly
  xr[j] = xr[i] - tr; xr[i] += tr;
  xi[j] = xi[i] - ti; xi[i] += ti;
}

/*
 * Name    : butterfly2
 * Function: Generate a bottom-level butterfly computation. This is separate
 *         : because it is responsible for grabbing data from the input
 *         : array(s).
 *
 */

// Real case first
template<typename T>
void
butterfly2 (TaskScalarVariable	&tr,
	    TaskScalarVariable	&ti,
	    TaskArray<1,T>	&x,
	    TaskArray<1,T>	&y,
	    unsigned i, unsigned j,
	    unsigned l, unsigned m,
	    unsigned k, unsigned N,
	    bool inverse)
{
  double re, im;

  // Find the twiddle factor
  W (k, N, re, im, inverse);

  // Do the complex multiplication
  semiMultiply (tr, y[j * 2], y[j * 2 + 1], false, re, im);
  semiMultiply (ti, y[j * 2], y[j * 2 + 1], true, im, re);

  // Finish the butterfly
  x[m * 2] = y[i * 2] - tr;		x[l * 2] = y[i * 2] + tr;
  x[m * 2 + 1] = y[i * 2 + 1] - ti;	x[l * 2 + 1] = y[i * 2 + 1] + tr;
}

// Complex case
template<typename T>
void
butterfly2 (TaskScalarVariable	&tr,
	    TaskScalarVariable	&ti,
	    TaskArray<1,T>	&xr,
	    TaskArray<1,T>	&xi,
	    TaskArray<1,T>	&yr,
	    TaskArray<1,T>	&yi,
	    unsigned i, unsigned j,
	    unsigned l, unsigned m,
	    unsigned k, unsigned N,
	    bool inverse)
{
  double re, im;

  // Find the twiddle factor
  W (k, N, re, im, inverse);

  // Do the complex multiplication
  semiMultiply (tr, yr[j], yi[j], false, re, im);
  semiMultiply (ti, yr[j], yi[j], true, im, re);

  // Finish the butterfly
  xr[m] = yr[i] - tr; xr[l] = yr[i] + tr;
  xi[m] = yi[i] - ti; xi[l] = yi[i] + ti;
}

/*
 * Name    : genInternalFFT
 * Function: Generate an internal N-point FFT as part of the overall FFT.
 *
 */

// Real case first
template<typename T>
void
genInternalFFT (TaskScalarVariable	&tr,
		TaskScalarVariable	&ti,
		TaskArray<1,T>	&x,
		TaskArray<1,T>	&y,
		unsigned	N,
		unsigned	offset,
		unsigned	bits,
		bool		inverse)
{
  unsigned m = N >> 1;
  unsigned i;

  if (N > 2)
    {
      genInternalFFT (tr, ti, x, y, m, offset, bits, inverse);
      genInternalFFT (tr, ti, x, y, m, m + offset, bits, inverse);
    }

  for (i = 0; i < m; i++)
    {
      unsigned ni, nj;

      ni = i + offset;
      nj = i + m + offset;

      if (N == 2)
	butterfly2 (tr, ti, y, x,
		    reverse (ni, bits), reverse (nj, bits),
		    ni, nj,
		    i, N, inverse);
      else
	butterfly (tr, ti, y, ni, nj, i, N, inverse);
    }
}

// Complex case
template<typename T>
void
genInternalFFT (TaskScalarVariable	&tr,
		TaskScalarVariable	&ti,
		TaskArray<1,T>	&xr,
		TaskArray<1,T>	&xi,
		TaskArray<1,T>	&yr,
		TaskArray<1,T>	&yi,
		unsigned	N,
		unsigned	offset,
		unsigned	bits,
		bool		inverse)
{
  unsigned m = N >> 1;
  unsigned i;

  if (N > 2)
    {
      genInternalFFT (tr, ti, xr, xi, yr, yi, m, offset, bits, inverse);
      genInternalFFT (tr, ti, xr, xi, yr, yi, m, m + offset, bits, inverse);
    }

  for (i = 0; i < m; i++)
    {
      unsigned ni, nj;

      ni = i + offset;
      nj = i + m + offset;

      if (N == 2)
	butterfly2 (tr, ti, yr, yi, xr, xi,
		    reverse (ni, bits), reverse (nj, bits),
		    ni, nj,
		    i, N, inverse);
      else
	butterfly (tr, ti, yr, yi, ni, nj, i, N, inverse);
    }
}

/*
 * Name    : realTweak
 * Function: Generate a tweaker that will allow us to perform a real
 *         : transform using a complex FFT.
 *
 */

template<typename T>
void
realTweak (TaskArray<1,T> &x,
	   unsigned      N,
	   bool		 inverse)
{
  double         re, im;
  unsigned	 i;

  tScope {
  tVar (double, tr);
  tVar (double, ti);
  tVar (double, ar);
  tVar (double, ai);
  tVar (double, br);
  tVar (double, bi);

  // First do the zero element (this holds two real values)
  ar = 2 * (x[0] + x[1]);
  br = 2 * (x[0] - x[1]);
  x[0] = ar;
  x[1] = br;

  // Next, do the rest
  for (i = 1; i < (N >> 1); i++)
    {
      unsigned n = i + 1;
      unsigned m = (N << 1) - i - 1;

      // Compute the twiddle factor
      W (i, (N << 1), re, im, inverse);

      // Compute the two parts
      ar = x[n] + x[m];	ai = x[n + 1] - x[m + 1];
      br = x[m] - x[n]; bi = x[m + 1] - x[n + 1];

      // Perform the complex multiplication
      semiMultiply (tr, bi, br, false, re, im);
      semiMultiply (ti, br, bi, true, im, re);

      // Finish-off
      x[n] = ar + tr; x[n + 1] = ai + ti;
      x[m] = ar - tr; x[m + 1] = ti - ai;
    }
	}
}

typedef TaskGraph<void, double[100], double[100]> realfft_TaskGraph;
typedef TaskGraph<void, double[100], double[100], double[100], double[100]> complexfft_TaskGraph;

/*
 * Name    : genRealFFT
 * Function: Generate a real FFT TaskGraph. Parameters for the
 *         : TaskGraph are "x" and "y".
 *
 */

realfft_TaskGraph *
genRealFFT (unsigned N, bool inverse)
{
  realfft_TaskGraph *pFFT = new realfft_TaskGraph ();

  // Generate our FFT algorithm
  taskgraph (realfft_TaskGraph, *pFFT, tuple2(x, y) )
    {
      tVar (double, tr);
      tVar (double, ti);

      // Generate a real FFT

      N >>= 1;

      // Tweak if necessary
      if (inverse)
        realTweak (x, N, inverse);

      // Do the top level algorithm
      genInternalFFT (tr, ti, x, y, N, 0, bits (N) - 1, inverse);

      // Tweak if necessary
      if (!inverse)
        realTweak (x, N, inverse);
    }

  return pFFT;
}


/*
 * Name    : genComplexFFT
 * Function: Generate a complex FFT TaskGraph. Parameters for the
 *         : TaskGraph are "xr", "xi", "yr" and "yi".
 *
 */

complexfft_TaskGraph *
genComplexFFT (unsigned N, bool inverse)
{
  complexfft_TaskGraph *pFFT = new complexfft_TaskGraph ();

  // Generate our FFT algorithm
  taskgraph (complexfft_TaskGraph, *pFFT, tuple4(xr, xi, yr, yi))
    {
      tVar (double, tr);
      tVar (double, ti);

      // Otherwise generate a complex FFT

      // Generate
      genInternalFFT (tr, ti, xr, xi, yr, yi, N, 0, bits (N) - 1, inverse);
    }

  return pFFT;
}

/* Name    : main
 * Function: Generate an FFT.
 *
 */

int main (int argc, char **argv)
{
  complexfft_TaskGraph  *myFFT;
  unsigned		i;
  double		*xr, *xi;
  double		*yr, *yi;
  unsigned		fft_size;
  Timer timer;

  if (argc < 2)
    {
      fprintf (stderr, "usage: fft <fft size>\n");

      return 1;
    }

  fft_size = static_cast<unsigned>( atoi (argv[1]) );

  printf ("Generating %u-point complex FFT...", fft_size);

  fflush (stdout);

  // Generate a complex FFT
  timer.start();
  myFFT = genComplexFFT (fft_size, false);
  timer.stop();

  printf ("took %g seconds.\n", timer.getTime());


  printf ("Compiling...");

  fflush (stdout);

  // Compile it
  GnuCCompiler gcc;

  timer.start();
  myFFT->compile (&gcc,true);
  timer.stop();

  printf ("took %g seconds.\n", timer.getTime() );

  // Build some data for it to process
  srand48 (time (0));

  // Allocate some memory
  xr = new double[fft_size * 4];
  xi = xr + fft_size;
  yr = xi + fft_size;
  yi = yr + fft_size;

  // Process it
  printf ("Processing...");

  fflush (stdout);

  // First fill it with random data
  for (i = 0; i < fft_size; i++)
    {
      xr[i] = drand48 ();
      xi[i] = drand48 ();
    }
  timer.start();
  for (i = 0; i < data_size; i++)
    myFFT->execute (xr, xi, yr, yi);
  timer.stop();

  printf ("took %g seconds (%g us per %u-point FFT).\n", timer.getTime(),
	  (timer.getTime() / data_size) * 1e6, fft_size);

  // myFFT->print (stdout);

  // Destroy the data
  delete []xr;
  delete myFFT;
}
