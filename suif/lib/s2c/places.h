/* file "places.h" */

/*  Copyright (c) 1994 Stanford University

    All rights reserved.

    This software is provided under the terms described in
    the "suif_copyright.h" include file. */

/* #include <suif_copyright.h> */

/*
 *  These are all the places that can be chosen as locations to of
 *  annotes to be printed.
 */

place(fses)
place(nodes)
  place(loops)
  place(fors)
  place(ifs)
  place(blocks)
place2(all-instrs, all_instrs)
place(symtabs)
place2(all-syms, all_syms)
  place(vars)
  place2(proc-syms, proc_syms)
  place(labels)
place2(var-defs, var_defs)
place(types)
