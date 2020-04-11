/*  Header for SUIF checking library */

/*  Copyright (c) 1994 Stanford University

    All rights reserved.

    This software is provided under the terms described in
    the "suif_copyright.h" include file. */

#include <suif_copyright.h>

#ifndef CHECK_H
#define CHECK_H

#include <suif1.h>
#include <useful.h>

/*  global function declarations */

extern void init_check(int &argc, char *argv[]);
extern void exit_check(void);
extern void check_type(tree_node *tn, void *);
extern void check_type_instr(instruction *i, void *x);
extern void check_global_symtab(global_symtab *the_global_symtab);
extern void check_file_symtab(file_symtab *the_file_symtab);
extern void check_symtab(base_symtab *the_symtab);
extern void check_proc(tree_proc *the_proc);
extern void check_node(tree_node *the_node);
extern void check_operand(operand the_operand);
extern void check_instruction(instruction *the_instr);

/*  global variable declarations */

extern boolean checkfail;  /* If TRUE, give error messages and abort() when */
                           /* problems are found; if FALSE (the default), */
                           /* only give warnings and continue. */
extern boolean checkabort; /* If TRUE, always abort after printing an error or
                              warning message.  This is useful under a debugger
                              to make execution stop and leave the stack frame
                              and other data for examination.  It defaults to
                              FALSE. */
extern boolean pointer_freedom;
                           /* If TRUE, the restrictions on pointer types are
                              relaxed in the following ways:
                                * ldc's can have any pointer type as long
                                  as the immediate value is an integer or a
                                  symbolic address of some sort.
                                * The result of pointer addition and
                                  subtraction of a pointer and an integer can
                                  be any other pointer type.
                              If the value of this flag is FALSE (the default),
                              then an ldc with pointer type of a symbol address
                              must match the type of the symbol modified by the
                              "fields" annotation and the result of pointer
                              addition must be the operand pointer type,
                              possibly as modified by the "fields" annotation.
                              */

#endif /* CHECK_H */
