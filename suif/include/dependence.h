/* file "dependence.h" */

/*  Copyright (c) 1994 Stanford University

    All rights reserved.

    This software is provided under the terms described in
    the "suif_copyright.h" include file. */

#include <suif_copyright.h>

/*  General SUIF Include File */

#ifndef DEPENDENCE_H
#define DEPENDENCE_H

#ifndef SUIF_H
#include <suif1.h>
#endif

#ifndef SUIFMATH_H
#include <suifmath.h>
#endif

#ifdef DEPENDENCELIB
#define DEPINCFILE(F) #F
#else
#define DEPINCFILE(F) <dependence/F>
#endif

#include DEPINCFILE(access_vector.h)
#include DEPINCFILE(dep_node.h)
#include DEPINCFILE(deptest_msc.h)
#include DEPINCFILE(dvector.h)
#include DEPINCFILE(exact.h)
#include DEPINCFILE(dodep.h)



#endif /* DEPENDENCE_H */
