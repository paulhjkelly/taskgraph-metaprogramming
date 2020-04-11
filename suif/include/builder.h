/* file "builder.h" */

/*  Copyright (c) 1994 Stanford University

    All rights reserved.

    This software is provided under the terms described in
    the "suif_copyright.h" include file. */

#include <suif_copyright.h>

/*  Top-Level Include File for the SUIF Builder Library */

#ifndef BUILDER_H
#define BUILDER_H


#ifdef BUILDERLIB
#define BUILDERINCFILE(F) #F
#else
#define BUILDERINCFILE(F) <builder/F>
#endif

#include BUILDERINCFILE(builder_enum.h)
#include BUILDERINCFILE(builder_internals.h)
#include BUILDERINCFILE(builder_def.h)

#endif /* BUILDER_H */
