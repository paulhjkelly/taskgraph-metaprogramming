/* file "useful.h" */

/*  Copyright (c) 1994,95 Stanford University

    All rights reserved.

    This software is provided under the terms described in
    the "suif_copyright.h" include file. */

#include <suif_copyright.h>

/*  Header for SUIF library of miscellaneous useful routines */

#ifndef USEFUL_H
#define USEFUL_H

// Mark exported symbols as DLL imports for Win32. (jsimmons)
#if defined(_WIN32)
#define EXPORTED_BY_USEFUL _declspec(dllimport) extern
#else
#define EXPORTED_BY_USEFUL extern
#endif

#include <suif1.h>
#include <useful/inumbers.h>
#include <useful/basic.h>
#include <useful/walk.h>
#include <useful/operand_operators.h>

#endif /* USEFUL_H */
