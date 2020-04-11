/* file "suifmath.h" */

/*  Copyright (c) 1994 Stanford University

    All rights reserved.

    This software is provided under the terms described in
    the "suif_copyright.h" include file. */

#include <suif_copyright.h>

/*  Top-Level Include File for the SUIF Math Library */

#ifndef SUIFMATH_H
#define SUIFMATH_H


#ifdef SUIFMATHLIB
#define MATHINCFILE(F) #F
#else
#define MATHINCFILE(F) <suifmath/F>
#endif

#include MATHINCFILE(coeff.h)
#include MATHINCFILE(int_matrix.h)
#include MATHINCFILE(solver_hook.h)
#include MATHINCFILE(fract.h)
#include MATHINCFILE(fract_vector.h)
#include MATHINCFILE(fract_matrix.h)
#include MATHINCFILE(vector_space.h)
#include MATHINCFILE(matrix.h)
#include MATHINCFILE(nametable.h)
#include MATHINCFILE(linear_ineq.h)
#include MATHINCFILE(fmred.h)
#include MATHINCFILE(bounds.h)
#include MATHINCFILE(named_lin_ineq.h)
#include MATHINCFILE(named_symcoeff_ineq.h)
#include MATHINCFILE(named_sc_fm.h)  
#include MATHINCFILE(named_sc_merge.h)
#include MATHINCFILE(code_context.h)
#endif /* SUIFMATH_H */
