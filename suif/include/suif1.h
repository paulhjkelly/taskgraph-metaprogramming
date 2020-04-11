/*  Top-level SUIF Verion 1 Include File */

/*  Copyright (c) 1994 Stanford University

    All rights reserved.

    This software is provided under the terms described in
    the "suif_copyright.h" include file. */

#ifndef SUIF1_H
#define SUIF1_H

/*
 *  Use a macro to include files so that they can be treated differently
 *  when compiling the library than when compiling an application.
 */

#ifdef SUIFLIB
#define INCLFILE(F) #F
#else
#define INCLFILE(F) <suif1/F>
#endif

/*
 *  The files are listed below in groups.  Each group generally depends
 *  on the groups included before it.  Within each group, indentation is
 *  used to show the dependences between files.
 */

#include <suif_copyright.h>

#include <machine_dependent.h>

/* system headers */
#include <new>
#include <cerrno>
#include <cstdio>
#include <cstdlib>

/* command line parsing is independent */
#include INCLFILE(cmdparse.h)

/* generic data structures */
#include INCLFILE(misc.h)
#include   INCLFILE(bitset.h)
#include   INCLFILE(xarray.h)
#include   INCLFILE(glist.h)
#include     INCLFILE(dlist.h)
#include     INCLFILE(mtflist.h)
#include       INCLFILE(alist.h)
#include       INCLFILE(hash.h)
#include   INCLFILE(stringtable.h)
#include   INCLFILE(string_index.h)
#include   INCLFILE(ptr_index.h)
#include   INCLFILE(tree_string_index.h)
#include   INCLFILE(ts_ptr_index.h)

/* library initialization and I/O streams */
#include INCLFILE(initsuif.h)
#include INCLFILE(suifstreams.h)

/* annotation manager and suif objects */
#include INCLFILE(aman.h)
#include INCLFILE(suifobj.h)

/* immediates and annotations: depend on suif types */
#include INCLFILE(operand.h)
#include INCLFILE(symaddr.h)
#include   INCLFILE(immed.h)
#include     INCLFILE(annote.h)

/* symbols and types: depend on suifobj */
#include INCLFILE(types.h)
#include INCLFILE(symbols.h)
#include INCLFILE(vardef.h)
#include   INCLFILE(symtab.h)
#include     INCLFILE(fileset.h)

/* instruction: depends on suifobj, immed, and types */
#include INCLFILE(opcodes.h)
#include   INCLFILE(instruction.h)

/* trees: depend on just about everything! */
#include INCLFILE(trees.h)


/* This needs to be after the RCS_HEADER macro is defined in misc.h. */
RCS_HEADER(suif_h,
    "$Id$")

#endif /* SUIF1_H */
