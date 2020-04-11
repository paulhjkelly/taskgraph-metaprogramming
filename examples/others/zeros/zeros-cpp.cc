/* ========================================================================== */
/*                                                                            */
/*   Program: TaskGraph Zero Finding Sample Program                           */
/*   Started: 11/05/2000                                                      */
/*   Version: 0.9                                                             */
/*   Author : Alastair J. Houghton                                            */
/*                                                                            */
/* -------------------------------------------------------------------------- */
/*                                                                            */
/*   Module : zeros.cc                                                        */
/*   Started: 11/05/2000                                                      */
/*   Version: 0.9                                                             */
/*   Stamped: <00/06/14 12:35:53 ajh>                                         */
/*                                                                            */
/* ========================================================================== */

#include <cmath>
#include <cfloat>
#include <ctime>
#include <cstdio>
#include <iostream>
#include <limits>

static const unsigned NUM_TO_TIME = 1000;
static const double pi = M_PI;

/*
 * Name    : polyEval
 * Function: Evaluate a polynomial at the specified point.
 *
 */

inline void polyEval (unsigned n, const double *p,
	  double re, double im,
	  double &are, double &aim)
{
  are = 0.0;
  aim = 0.0;

  for(unsigned i=0; i<n; ++i)
  {
    double a, b;

    // Multiply
    a = are * re - aim * im;
    b = aim * re + are * im;

    are = a;
    aim = b;

    // Add
    are += p[i];
  }
}

/*
 * Name    : polyZeros
 * Function: Find the zeros of a polynomial. Uses the Durand-Kerner algorithm,
 *         : but with the modification that revised zero estimates are used
 *         : immediately upon calculation, rather than waiting for the next
 *         : iteration. (This algorithm is also referred to variously as
 *         : the Weierstrass algorithm or Dochev's algorithm).
 *
 */

void polyZeros (unsigned n, const double *p, double eps, unsigned maxit,
	   double *re, double *im, unsigned &m)
{
  double   max = 0.0;
  double   Z;
  double   prodre, prodim, mag2, pvre, pvim;
  bool	   finished = false;

  // First find the maximum coefficient
  for (unsigned i = 0; i < n; ++i)
  {
    if (p[i] > max)
      max = p[i];
  }

  // Compute the radius of the initial approximations
  Z = 3.0 * max / p[n - 1];

  // Generate initial approximations
  for (unsigned i = 0; i < n - 1; ++i)
  {
    re[i] = Z * std::cos ((2 * pi * i) / n);
    im[i] = Z * std::sin ((2 * pi * i) / n);
  }

  // Iterate
  for (unsigned it = 0; !finished && it < maxit; ++it)
  {
    finished = true;
    m = it;

    for (unsigned j = 0; j < n - 1; ++j)
    {
      double a, b;

      prodre = 1.0;
      prodim = 0.0;

      // Compute the product of all zero differences z[j] - z[i]
      for (unsigned i = 0; i < n - 1; ++i)
      {
        double c, d;

        if (j != i)
        {
	  c = re[j] - re[i];
	  d = im[j] - im[i];

	  a = prodre * c - prodim * d;
	  b = prodim * c + prodre * d;

	  prodre = a;
	  prodim = b;
	}
      }

      // Compute the polynomial value at z[j]
      polyEval (n, p, re[j], im[j], pvre, pvim);

      // Check it to see if it's small enough; do this the quick way.
      if (fabs(pvre) > eps || fabs(pvim) > eps)
      {
        finished = false;
      }

      // Compute its magnitude squared
      mag2 = prodre * prodre + prodim * prodim;
      
      // Multiply by the conjugate of the product value, divide by mag2
      a = (pvre * prodre + pvim * prodim) / mag2;
      b = (pvim * prodre - pvre * prodim) / mag2;

      /* This is the new estimate for z[j] (note: technically we
         should use this only on the *NEXT* iteration; however, we
         don't do that because it takes up more space and time, and
         this probably converges faster like this anyway) */
      re[j] -= a;
      im[j] -= b;
    }
  }
}

/*
 * Name    : main
 * Function: Time the C++ zero solver.
 *
 */

int main (int argc, char **argv)
{
  double p[11], zr[10], zi[10];
  unsigned m;
  clock_t clk;

  // This is the polynomial (x - 1)(x - 2)(x - 3)(x - 4) ... (x - 10)
  p[0] = 1;
  p[1] = -55;
  p[2] = 1320;
  p[3] = -18150;
  p[4] = 157773;
  p[5] = -902055;
  p[6] = 3416930;
  p[7] = -8409500;
  p[8] = 12753576;
  p[9] = -10628640;
  p[10] = 3628800;

  clk = -clock ();

  for (unsigned i = 0; i < NUM_TO_TIME; ++i)
    polyZeros (11, p, 1e-7, 22, zr, zi, m);

  clk += clock ();

  for (unsigned i = 0; i < 10; ++i)
  {
    double re, im;

    polyEval (11, p, zr[i], zi[i], re, im);

    printf ("z[%d] = %g + %gj\t (p(z[%d]) = %g + %gj)\n",
            i, zr[i], zi[i],
            i, re, im);
  }

  std::cout << std::endl << "Zero-finding took " << static_cast<double>(clk) / CLOCKS_PER_SEC << " seconds." << std::endl;
  std::cout << m+1 << " iterations." << std::endl;

  return 0;
}
