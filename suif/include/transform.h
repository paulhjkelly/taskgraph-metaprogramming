/* file "transform.h" */

/*  Copyright (c) 1994 Stanford University

    All rights reserved.

    This software is provided under the terms described in
    the "suif_copyright.h" include file. */

#include <suif_copyright.h>

/*  Loop tranformation library include file */

#ifndef TRANSFORM_H
#define TRANSFORM_H

#ifdef TRANSFORMLIB
#define TRANSINCFILE(F) #F
#else
#define TRANSINCFILE(F) <transform/F>
#endif

#include TRANSINCFILE(bexpr.h)
#include TRANSINCFILE(lptrans.h)

RCS_HEADER(transform_h, "$Id$")

#endif /* TRANSFORM_H */











