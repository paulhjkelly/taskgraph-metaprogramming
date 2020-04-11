/* file "useful_internal.h" */

/*  Copyright (c) 1994 Stanford University

    All rights reserved.

    This software is provided under the terms described in
    the "suif_copyright.h" include file. */

#include <suif_copyright.h>

/*  Internal header for SUIF library of miscellaneous useful routines */

#ifndef USEFUL_INTERNAL_H
#define USEFUL_INTERNAL_H

// Definition for local compilation. (jsimmons)
#define EXPORTED_BY_USEFUL extern

#include "inumbers.h"
#include "basic.h"
#include "walk.h"
#include "operand_operators.h"

/*----------------------------------------------------------------------*
    Beginning of Internal Functions
 *----------------------------------------------------------------------*/

extern void init_labinfo(void);
extern void init_linkinfo(void);
extern void init_make_instr_ops(void);
extern void init_transform(void);

/*----------------------------------------------------------------------*
    End of Internal Functions
 *----------------------------------------------------------------------*/

#endif /* USEFUL_INTERNAL_H */
