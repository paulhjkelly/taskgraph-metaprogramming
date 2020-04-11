/*  Include file for internal declarations for the SUIF check library */

/*  Copyright (c) 1994 Stanford University

    All rights reserved.

    This software is provided under the terms described in
    the "suif_copyright.h" include file. */

#include <suif_copyright.h>

#ifndef CHECK_INTERNAL_H
#define CHECK_INTERNAL_H

#include "check.h"
#include <cstdarg>

RCS_HEADER(check_internal_h,
     "$Id$")


/*----------------------------------------------------------------------*
    Begin Declarations
 *----------------------------------------------------------------------*/

extern base_symtab *current_scope;
extern tree_node *current_node;

extern void problem(const char *fmt, ...);
extern void vproblem(const char *fmt, va_list ap);
extern void problem_instr(tree_node *the_node, instruction *the_instr,
                          const char *fmt, ...);
extern void vproblem_instr(tree_node *the_node, instruction *the_instr,
                           const char *fmt, va_list ap);

/*----------------------------------------------------------------------*
    End Declarations
 *----------------------------------------------------------------------*/

#endif
