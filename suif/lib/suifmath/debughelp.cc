/* file "debughelp.cc" */

/*  Copyright (c) 1994 Stanford University

    All rights reserved.

    This software is provided under the terms described in
    the "suif_copyright.h" include file. */

#include <suif_copyright.h>

#define _MODULE_ "libsuifmath.a"
 
#include <cstdlib>
#include <suif1.h>
#include "suifmath.h"


// these functions can be called from gdb to print the structures

void named_symcoeff_ineq_print(named_symcoeff_ineq * sq) { sq->print(); }
void named_lin_ineq_print(named_lin_ineq * nq)           { nq->print(); }
void lin_ineq_print(lin_ineq * lq)                       { lq->print(); }
void integer_matrix_print(integer_matrix * im)           { im->print(); }
void name_table_print(name_table * nt)                   { nt->print(); }
void integer_row_print(integer_row * ir)                 { ir->print(); }
					      

